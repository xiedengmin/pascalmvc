
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2014 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoDataTypeMapUni;

interface

uses
  SysUtils,
  CLRClasses, CRTypes, CRDataTypeMap, MemData, CRJson;

const
  mongoBase                  = 1600;

{ native JSON types }
  mongoString                = mongoBase + 1;
  mongoNumber                = mongoBase + 2;
  mongoBoolean               = mongoBase + 3;
  mongoObject                = mongoBase + 4;
  mongoArray                 = mongoBase + 5;
  mongoNull                  = mongoBase + 6;

{ additional ExtJSON/BSON types }
  mongoObjectId              = mongoBase + 7;
  mongoInt32                 = mongoBase + 8;
  mongoInt64                 = mongoBase + 9;
  mongoDateTime              = mongoBase + 10;
  mongoJavaCode              = mongoBase + 11;
  mongoUndefined             = mongoBase + 12;
  mongoJavaScopeCode         = mongoBase + 13;
  mongoRegex                 = mongoBase + 14;
  mongoTimestamp             = mongoBase + 15;
  mongoBinary                = mongoBase + 16;
  mongoDouble                = mongoBase + 17;
  mongoBytes                 = mongoBase + 18;
  mongoMinKey                = mongoBase + 19;
  mongoMaxKey                = mongoBase + 20;
  mongoDBPointer             = mongoBase + 21;
  mongoDecimal128            = mongoBase + 22;

{ internal type }
  mongoGeneric               = mongoBase + 22;

type
  TMongoConverterManager = class{$IFNDEF LITE}(TConverterManager){$ENDIF}
{$IFNDEF LITE}
  public
    constructor Create;

    class function GetDBProvider: Word; override;
{$ENDIF}
    class function GetDBType(JSONTag: TJSONTag): Word;
    class function GetDataType(const DBType: Word; out SubDataType: Word): Word;
  end;

{$IFNDEF LITE}
  TMongoMapRules = class(TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TMongoDataConverters = class(TDataConverters)
  public
    class procedure JsonToString(const Source: TJSONValue; out Dest: string);
    class function StringToJson(var Source: string; var Dest: TJSONValue; IgnoreConvertErrors: boolean): TConvertStatus;

    class function InternalStrToDouble(var Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    class function JsonToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function JsonToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;

    class function AStrToJson(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToJson(var AConvertInfo: TConvertInfo): TConvertStatus;
  end;
{$ENDIF}

implementation

uses
  Classes, StrUtils,
{$IFNDEF UNIDACPRO}
  MongoCall, MongoObjects;
{$ELSE}
  MongoCallUni, MongoObjectsUni;
{$ENDIF}

{$IFNDEF LITE}
type
  TInternalDocument = class(TMongoDocument);

var
  MongoConverterManager: TMongoConverterManager;

procedure InitMongoTypes;
begin
  DBTypeInfos.Add(mongoString,        'String',           True,  False);
  DBTypeInfos.Add(mongoNumber,        'Number',           False, False);
  DBTypeInfos.Add(mongoBoolean,       'Boolean',          False, False);
  DBTypeInfos.Add(mongoObject,        'Object',           False, False);
  DBTypeInfos.Add(mongoArray,         'Array',            False, False);
  DBTypeInfos.Add(mongoNull,          'Null',             False, False);

  DBTypeInfos.Add(mongoObjectId,      'ObjectId',         False, False);
  DBTypeInfos.Add(mongoInt32,         'Int32',            False, False);
  DBTypeInfos.Add(mongoInt64,         'Int64',            False, False);
  DBTypeInfos.Add(mongoDouble,        'Double',           False, False);
  DBTypeInfos.Add(mongoDateTime,      'DateTime',         False, False);
  DBTypeInfos.Add(mongoTimestamp,     'TimeStamp',        False, False);
  DBTypeInfos.Add(mongoUndefined,     'Undefined',        False, False);
  DBTypeInfos.Add(mongoBinary,        'Binary',           False, False);
  DBTypeInfos.Add(mongoRegex,         'RegEx',            False, False);
  DBTypeInfos.Add(mongoJavaCode,      'JavaCode',         False, False);
  DBTypeInfos.Add(mongoJavaScopeCode, 'JavaScopeCode',    False, False);
  DBTypeInfos.Add(mongoMinKey,        'MinKey',           False, False);
  DBTypeInfos.Add(mongoMaxKey,        'MaxKey',           False, False);
  DBTypeInfos.Add(mongoDBPointer,     'DBPointer',        False, False);
  DBTypeInfos.Add(mongoDecimal128,    'Decimal128',       False, False);
end;

{ TMongoMapRules }

class function TMongoMapRules.GetConverterManager: TConverterManager;
begin
  Result := MongoConverterManager;
end;

{ TLiteConverterManager }

constructor TMongoConverterManager.Create;
begin
  inherited;

  AddFetchConverter(mongoString, dtString);
  AddFetchConverter(mongoString, dtWideString);
  AddFetchConverter(mongoObjectId, dtString);
  AddFetchConverter(mongoObjectId, dtWideString);
  AddFetchConverter(mongoObject, dtString);
  AddFetchConverter(mongoObject, dtWideString);
  AddFetchConverter(mongoArray, dtString);
  AddFetchConverter(mongoArray, dtWideString);
  AddFetchConverter(mongoNull, dtString);
  AddFetchConverter(mongoNull, dtWideString);
  AddFetchConverter(mongoUndefined, dtString);
  AddFetchConverter(mongoUndefined, dtWideString);
  AddFetchConverter(mongoBinary, dtString);
  AddFetchConverter(mongoBinary, dtWideString);
  AddFetchConverter(mongoRegex, dtString);
  AddFetchConverter(mongoRegex, dtWideString);
  AddFetchConverter(mongoJavaCode, dtString);
  AddFetchConverter(mongoJavaCode, dtWideString);
  AddFetchConverter(mongoJavaScopeCode, dtString);
  AddFetchConverter(mongoJavaScopeCode, dtWideString);
  AddFetchConverter(mongoDBPointer, dtString);
  AddFetchConverter(mongoDBPointer, dtWideString);
  AddFetchConverter(mongoMinKey, dtString);
  AddFetchConverter(mongoMinKey, dtWideString);
  AddFetchConverter(mongoMaxKey, dtString);
  AddFetchConverter(mongoMaxKey, dtWideString);
  AddFetchConverter(mongoDecimal128, dtString);
  AddFetchConverter(mongoDecimal128, dtWideString);
end;

class function TMongoConverterManager.GetDBProvider: Word;
begin
  Result := mongoBase;
end;
{$ENDIF}

class function TMongoConverterManager.GetDBType(JSONTag: TJSONTag): Word;
begin
  Result := mongoString;

  case JSONTag of
    jtObject: Result := mongoObject;
    jtArray: Result := mongoArray;
    jtString: Result := mongoString;
    jtNumber: Result := mongoNumber;
    jtBoolean: Result := mongoBoolean;
    jtNull: Result := mongoNull;
    jtObjectId: Result := mongoObjectId;
    jtInt32: Result := mongoInt32;
    jtInt64: Result := mongoInt64;
    jtDateTime: Result := mongoDateTime;
    jtJavaCode: Result := mongoJavaCode;
    jtUndefined: Result := mongoUndefined;
    jtJavaScopeCode: Result := mongoJavaScopeCode;
    jtRegex: Result := mongoRegex;
    jtTimestamp: Result := mongoTimestamp;
    jtBinary: Result := mongoBinary;
    jtDouble: Result := mongoDouble;
    jtBytes: Result := mongoBytes;
    jtMinKey: Result := mongoMinKey;
    jtMaxKey: Result := mongoMaxKey;
    jtDBPointer: Result := mongoDBPointer;
    jtDecimal128: Result := mongoDecimal128;
  else
    Assert(False);
  end;
end;

class function TMongoConverterManager.GetDataType(const DBType: Word; out SubDataType: Word): Word;
begin
  SubDataType := DBType;

  case DBType of
    mongoString,
    mongoObjectId,
    mongoDecimal128: Result := dtString;
    mongoNumber: Result := dtFloat;
    mongoBoolean: Result := dtBoolean;
    mongoObject,
    mongoArray,
    mongoRegex,
    mongoJavaCode,
    mongoJavaScopeCode,
    mongoBinary,
    mongoDBPointer,
    mongoNull,
    mongoUndefined,
    mongoMinKey,
    mongoMaxKey,
    mongoTimestamp: Result := dtObject;
    mongoInt32: Result := dtInt32;
    mongoInt64: Result := dtInt64;
    mongoDateTime: Result := dtDateTime;
    mongoDouble: Result := dtFloat;
    mongoBytes: Result := dtBytes;
  else
    raise Exception.Create('Unknown data type');
  end;
end;

{$IFNDEF LITE}

{ TMongoDataConverters }

class procedure TMongoDataConverters.JsonToString(const Source: TJSONValue; out Dest: string);
begin
  if Source = nil then
    Exit;

  TInternalDocument(Source.Data).API.Serializer.ToText(Source, Dest);

  if (Source.Tag = jtDouble) or ((Source.Tag = jtNumber) and (TJSONNumber(Source).Subtype = jtDouble)) then
    ChangeDecimalSeparator(Dest);
end;

class function TMongoDataConverters.StringToJson(var Source: string; var Dest: TJSONValue; IgnoreConvertErrors: boolean): TConvertStatus;
var
  TmpValue: TJSONValue;
  i: integer;
begin
  Assert(Dest <> nil);
  Result := csSuccess;

  case Dest.Tag of
    jtObject: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtObject) then
          raise Exception.Create(cInvalidObject)
        else begin
          TJSONObject(Dest).Pairs.Clear;
          for i := 0 to TJSONObject(TmpValue).Pairs.Count - 1 do
            TJSONObject(Dest).AddPair(TJSONObject(TmpValue).Pairs[i].Clone(Dest, Dest.Data) as TJSONPair);
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtArray: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtArray) then
          raise Exception.Create(cInvalidArray)
        else begin
          TJSONArray(Dest).Elements.Clear;
          for i := 0 to TJSONArray(TmpValue).Elements.Count - 1 do
            TJSONArray(Dest).Elements.Add(TJSONArray(TmpValue).Elements[i].Clone(Dest, Dest.Data) as TJSONValue);
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtString: begin
      if IsQuoted(Source) then begin
        if Length(Source) > Dest.Size then
          Result := csStringTruncated
        else
          TJSONString(Dest).Value := UnQuote(Source);
      end
      else
        raise Exception.Create(cInvalidString);
    end;
    jtNumber: begin
      Assert(False);
      {TODO:}
//      if TJSONNumber(Dest).Subtype = jtDouble then begin
//
//
//      end;
    end;
    jtDouble: Result := InternalStrToDouble(Source, TJSONDouble(Dest).ValuePtr, IgnoreConvertErrors);
    jtBoolean: Result := InternalStrToBool(Source, TJSONBoolean(Dest).ValuePtr, IgnoreConvertErrors);
    jtNull: Assert(False);
    jtObjectId:
      {$IFDEF VER12P}
        TJSONObjectId(Dest).Value := StringToOid(Source);
      {$ELSE}
        TJSONObjectId(Dest).SetValue(StringToOid(Source));
      {$ENDIF}
    jtInt32: Result := InternalStrToInt32(Source, TJSONInt32(Dest).ValuePtr, IgnoreConvertErrors);
    jtInt64: Result := InternalStrToInt64(Source, TJSONInt64(Dest).ValuePtr, IgnoreConvertErrors);
    jtDecimal128: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtDecimal128) then
          raise Exception.Create(cInvalidDecimal128)
        else
          TJSONDecimal128(Dest).Value := TJSONDecimal128(TmpValue).Value;
      finally
        TmpValue.Free;
      end;
    end;
    jtDateTime: Result := InternalStrToDateTime(Source, cDateFormat + ' ' + cTimeFormat, TJSONDateTime(Dest).ValuePtr, IgnoreConvertErrors);
    jtJavaCode: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtJavaCode) then
          raise Exception.Create(cInvalidJavaCode)
        else
          TJSONJavaCode(Dest).Code.Value := TJSONJavaCode(TmpValue).Code.Value;
      finally
        TmpValue.Free;
      end;
    end;
    jtUndefined: Assert(False);
    jtJavaScopeCode: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtJavaScopeCode) then
          raise Exception.Create(cInvalidJavaCode)
        else begin
          TJSONJavaScopeCode(Dest).Code.Value := TJSONJavaScopeCode(TmpValue).Code.Value;
          TJSONJavaScopeCode(Dest).Scope.Pairs.Clear;
          for i := 0 to TJSONJavaScopeCode(TmpValue).Scope.Pairs.Count - 1 do
            TJSONJavaScopeCode(Dest).Scope.AddPair(TJSONJavaScopeCode(TmpValue).Scope.Pairs[i].Clone(Dest, Dest.Data) as TJSONPair);
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtRegex: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtRegex) then
          raise Exception.Create(cInvalidRegex)
        else begin
          TJSONRegex(Dest).Pattern.Value := TJSONRegex(TmpValue).Pattern.Value;
          TJSONRegex(Dest).Options.Value := TJSONRegex(TmpValue).Options.Value;
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtTimestamp: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtTimestamp) then
          raise Exception.Create(cInvalidTimestamp)
        else begin
          TJSONTimestamp(Dest).Timestamp.Value := TJSONTimestamp(TmpValue).Timestamp.Value;
          TJSONTimestamp(Dest).Increment.Value := TJSONTimestamp(TmpValue).Increment.Value;
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtBinary: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtBinary) then
          raise Exception.Create(cInvalidBinary)
        else begin
        {$IFDEF VER12P}
          TJSONBinary(Dest).Binary.Value := TJSONBinary(TmpValue).Binary.Value;
        {$ELSE}
          TJSONBinary(Dest).Binary.SetValue(TJSONBinary(TmpValue).Binary.GetValue);
        {$ENDIF}
          TJSONBinary(Dest).Subtype.Value := TJSONBinary(TmpValue).Subtype.Value;
        end;
      finally
        TmpValue.Free;
      end;
    end;
    jtBytes: Assert(False);
    jtMinKey: Assert(False);
    jtMaxKey: Assert(False);
    jtDBPointer: begin
      TmpValue := TInternalDocument(Dest.Data).API.Deserializer.FromText(Source, Dest.Data);
      try
        if (TmpValue = nil) or (TmpValue.Tag <> jtDBPointer) then
          raise Exception.Create(cInvalidDBPointer)
        else begin
          TJSONDBPointer(Dest).Name.Value := TJSONDBPointer(TmpValue).Name.Value;
        {$IFDEF VER12P}
          TJSONDBPointer(Dest).Value := TJSONDBPointer(TmpValue).Value;
        {$ELSE}
          TJSONDBPointer(Dest).SetValue(TJSONDBPointer(TmpValue).GetValue);
        {$ENDIF}  
        end;
      finally
        TmpValue.Free;
      end;
    end;
  end;
end;

class function TMongoDataConverters.InternalStrToDouble(var Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  e: Extended;
begin
  ChangeDecimalSeparator(Str);

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

class function TMongoDataConverters.JsonToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  JsonToString(TJSONValue(AConvertInfo.Source), Str);
  Result := InternalWriteAStr(AnsiString(Str), AConvertInfo.SourceLen,
    AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TMongoDataConverters.JsonToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  JsonToString(TJSONValue(AConvertInfo.Source), Str);
  Result := InternalWriteWStr(WideString(Str), AConvertInfo.SourceLen,
    AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csStringTruncated then
    Result := csValueOverflow;
end;

class function TMongoDataConverters.AStrToJson(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(AConvertInfo.Source, AConvertInfo.SourceLen));
  Result := StringToJson(Str, TJSONValue(AConvertInfo.Dest), AConvertInfo.IgnoreConvertErrors);
end;

class function TMongoDataConverters.WStrToJson(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(AConvertInfo.Source, AConvertInfo.SourceLen));
  Result := StringToJson(Str, TJSONValue(AConvertInfo.Dest), AConvertInfo.IgnoreConvertErrors);
end;

initialization
  InitMongoTypes;
  MongoConverterManager := TMongoConverterManager.Create;

finalization
  MongoConverterManager.Free;
{$ENDIF}

end.
