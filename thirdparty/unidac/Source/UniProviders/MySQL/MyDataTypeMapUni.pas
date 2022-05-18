
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyDataTypeMapUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
  SysUtils;

const
  myBase           = 200;
  myBit            = myBase + 1;
  myTiny           = myBase + 2;
  myTinyUnsigned   = myBase + 3;
  mySmall          = myBase + 4;
  mySmallUnsigned  = myBase + 5;
  myMedium         = myBase + 6;
  myMediumUnsigned = myBase + 7;
  myInt            = myBase + 8;
  myIntUnsigned    = myBase + 9;
  myBigint         = myBase + 10;
  myBigintUnsigned = myBase + 11;
  myFloat          = myBase + 12;
  myDouble         = myBase + 13;
  myDecimal        = myBase + 14;
  myDate           = myBase + 15;
  myTime           = myBase + 16;
  myDatetime       = myBase + 17;
  myTimestamp      = myBase + 18;
  myYear           = myBase + 19;
  myChar           = myBase + 20;
  myVarchar        = myBase + 21;
  myBinary         = myBase + 22;
  myVarbinary      = myBase + 23;
  myTinyBlob       = myBase + 24;
  myBlob           = myBase + 25;
  myTinyText       = myBase + 26;
  myText           = myBase + 27;
  myEnum           = myBase + 28;
  mySet            = myBase + 29;
  myLongBlob       = myBase + 30;
  myMediumBlob     = myBase + 31;
  myLongText       = myBase + 32;
  myMediumText     = myBase + 33;
  myJSON           = myBase + 34;
  myNull           = myBase + 35;

type

{$IFNDEF LITE}
  TMyMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;
{$ENDIF}

  TMyConverterManager = class{$IFNDEF LITE}(TConverterManager){$ENDIF}
  protected
  {$IFNDEF LITE}
    function GetDateFormat: string; override;
    function GetTimeFormat: string; override;
    function GetDateTimeFormat: string; override;
  {$ENDIF}
  public
  {$IFNDEF LITE}
    constructor Create;

    class function GetDBProvider: Word; override;
  {$ENDIF}
    class function GetDBType(SQLType: Word; const LengthInChars, Flags: cardinal; const CharsetNr: integer): Word;
  end;

const
  myUnsignedTypes = [myTinyUnsigned, mySmallUnsigned, myMediumUnsigned, myIntUnsigned, myBigintUnsigned];

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  MyCall;
{$ELSE}
  MyCallUni;
{$ENDIF}

{$IFNDEF LITE}

var
  MyConverterManager: TMyConverterManager;

{ Functions }

procedure InitMyTypes;
begin
  DBTypeInfos.Add(myBit,           'Bit',                True,  False);
  DBTypeInfos.Add(myTiny,          'Tinyint',            True,  False);
  DBTypeInfos.Add(myTinyUnsigned,  'Tinyint Unsigned',   True,  False);
  DBTypeInfos.Add(mySmall,         'Smallint',           False, False);
  DBTypeInfos.Add(mySmallUnsigned, 'Smallint Unsigned',  False, False);
  DBTypeInfos.Add(myMedium,        'Mediumint',          False, False);
  DBTypeInfos.Add(myMediumUnsigned,'Mediumint Unsigned', False, False);
  DBTypeInfos.Add(myInt,           'Integer',            False, False);
  DBTypeInfos.Add(myIntUnsigned,   'Integer Unsigned',   False, False);
  DBTypeInfos.Add(myBigint,        'Bigint',             True,  False);
  DBTypeInfos.Add(myBigintUnsigned,'Bigint Unsigned',    True,  False);
  DBTypeInfos.Add(myFloat,         'Float',              False, False);
  DBTypeInfos.Add(myDouble,        'Double',             False, False);
  DBTypeInfos.Add(myDecimal,       'Decimal',            True,  True);
  DBTypeInfos.Add(myDate,          'Date',               False, False);
  DBTypeInfos.Add(myTime,          'Time',               False, False);
  DBTypeInfos.Add(myDatetime,      'Datetime',           False, False);
  DBTypeInfos.Add(myTimestamp,     'Timestamp',          True,  False);
  DBTypeInfos.Add(myYear,          'Year',               True,  False);
  DBTypeInfos.Add(myChar,          'Char',               True,  False);
  DBTypeInfos.Add(myVarchar,       'Varchar',            True,  False);
  DBTypeInfos.Add(myBinary,        'Binary',             True,  False);
  DBTypeInfos.Add(myVarbinary,     'Varbinary',          True,  False);
  DBTypeInfos.Add(myTinyBlob,      'TinyBlob',           False, False);
  DBTypeInfos.Add(myMediumBlob,    'MediumBlob',         False, False);
  DBTypeInfos.Add(myBlob,          'Blob',               False, False);
  DBTypeInfos.Add(myLongBlob,      'LongBlob',           False, False);
  DBTypeInfos.Add(myTinyText,      'TinyText',           False, False);
  DBTypeInfos.Add(myMediumText,    'MediumText',         False, False);
  DBTypeInfos.Add(myText,          'Text',               False, False);
  DBTypeInfos.Add(myLongText,      'LongText',           False, False);
  DBTypeInfos.Add(myEnum,          'Enum',               False, False);
  DBTypeInfos.Add(mySet,           'Set',                False, False);
  DBTypeInfos.Add(myJSON,          'JSON',               False, False);
end;

{ TMyMapRules }

class function TMyMapRules.GetConverterManager: TConverterManager;
begin
  Result := MyConverterManager;
end;

{ TMyConverterManager }

constructor TMyConverterManager.Create;
begin
  inherited;

  AddFetchConverter(myBit,  1,  8, -1, -1, dtBoolean, dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtInt8,    dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtInt16,   dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtUInt16,  dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtInt32,   dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtUInt32,  dtUInt16);
  AddFetchConverter(myBit,  1,  8, -1, -1, dtInt64,   dtUInt16);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtBoolean, dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtInt8,    dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtInt16,   dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtUInt16,  dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtInt32,   dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtUInt32,  dtUInt32);
  AddFetchConverter(myBit,  9, 32, -1, -1, dtInt64,   dtUInt32);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtBoolean, dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtInt8,    dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtInt16,   dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtUInt16,  dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtInt32,   dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtUInt32,  dtInt64);
  AddFetchConverter(myBit, 33, 64, -1, -1, dtInt64,   dtInt64);

  AddFetchConverter(myTiny, 1,  4, -1, -1, dtBoolean,dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtInt8,   dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtInt16,  dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtUInt16, dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtInt32,  dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtUInt32, dtInt8);
  AddFetchConverter(myTiny, 1,  4, -1, -1, dtInt64,  dtInt8);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtBoolean,dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtInt8,   dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtInt16,  dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtUInt16, dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtInt32,  dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtUInt32, dtInt16);
  AddFetchConverter(myTiny, 5, -1, -1, -1, dtInt64,  dtInt16);

  AddFetchConverter(myMediumUnsigned, dtUInt32);

  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtBoolean,dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtInt8,   dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtInt16,  dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtUInt16, dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtInt32,  dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtUInt32, dtUInt8);
  AddFetchConverter(myTinyUnsigned, 1,  3, -1, -1, dtInt64,  dtUInt8);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtBoolean,dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtInt8,   dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtInt16,  dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtUInt16, dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtInt32,  dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtUInt32, dtUInt16);
  AddFetchConverter(myTinyUnsigned, 4, -1, -1, -1, dtInt64,  dtUInt16);

  AddFetchConverter(myBigint,  1,  9, -1, -1, dtInt8,  dtInt32);
  AddFetchConverter(myBigint,  1,  9, -1, -1, dtInt16, dtInt32);
  AddFetchConverter(myBigint,  1,  9, -1, -1, dtUInt16,dtInt32);
  AddFetchConverter(myBigint,  1,  9, -1, -1, dtInt32, dtInt32);
  AddFetchConverter(myBigint,  1,  9, -1, -1, dtUInt32,dtInt32);
  AddFetchConverter(myBigint,  1,  9, -1, -1, dtInt64, dtInt32);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtInt8,  dtInt64);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtInt16, dtInt64);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtUInt16,dtInt64);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtInt32, dtInt64);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtUInt32,dtInt64);
  AddFetchConverter(myBigint, 10, 20, -1, -1, dtInt64, dtInt64);

  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtInt8,  dtInt32);
  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtInt16, dtInt32);
  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtUInt16,dtInt32);
  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtInt32, dtInt32);
  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtUInt32,dtInt32);
  AddFetchConverter(myBigintUnsigned,  1,  9, -1, -1, dtInt64, dtInt32);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtInt8,  dtInt64);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtInt16, dtInt64);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtUInt16,dtInt64);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtInt32, dtInt64);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtUInt32,dtInt64);
  AddFetchConverter(myBigintUnsigned, 10, 20, -1, -1, dtInt64, dtInt64);
  AddFetchConverter(myBigintUnsigned, dtFmtBCD);
  AddFetchConverter(myBigintUnsigned, dtString);

  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtInt8,   dtInt16);
  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtInt16,  dtInt16);
  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtUInt16, dtInt16);
  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtInt32,  dtInt16);
  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtUInt32, dtInt16);
  AddFetchConverter(myDecimal,  1,  4, 0, 0, dtInt64,  dtInt16);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtInt8,   dtInt32);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtInt16,  dtInt32);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtUInt16, dtInt32);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(myDecimal,  5,  9, 0, 0, dtInt64,  dtInt32);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtInt8,   dtInt64);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtInt16,  dtInt64);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtUInt16, dtInt64);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtInt32,  dtInt64);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtUInt32, dtInt64);
  AddFetchConverter(myDecimal, 10, -1, 0, 0, dtInt64,  dtInt64);

  AddFetchConverter(myDecimal, dtString);
  AddFetchConverter(myDecimal, dtFloat);
  AddFetchConverter(myDecimal, dtBCD);
  AddFetchConverter(myDecimal, dtFmtBCD);

  AddFetchConverter(myTime, dtDateTime);
  AddFetchConverter(myTime, dtDate, dtDateTime);
  AddFetchConverter(myTime, dtBytes, dtDateTime);
  AddFetchConverter(myTime, dtVarBytes, dtDateTime);

  AddFetchConverter(myChar, dtBytes, dtVarBytes);
  AddFetchConverter(myChar, dtVarBytes);
  AddFetchConverter(myChar, dtString);
  AddFetchConverter(myChar, dtFixedChar, dtString);
  AddFetchConverter(myChar, dtWideString);
  AddFetchConverter(myChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(myVarchar, dtBytes, dtVarBytes);
  AddFetchConverter(myVarchar, dtVarBytes);
  AddFetchConverter(myVarchar, dtString);
  AddFetchConverter(myVarchar, dtWideString);

  AddFetchConverter(myBinary, dtBytes);
  AddFetchConverter(myBinary, dtVarBytes);
  AddFetchConverter(myBinary, dtString);
  AddFetchConverter(myBinary, dtWideString);

  AddFetchConverter(myVarbinary, dtBytes);
  AddFetchConverter(myVarbinary, dtVarBytes);
  AddFetchConverter(myVarbinary, dtString);
  AddFetchConverter(myVarbinary, dtWideString);

  AddFetchConverter(myEnum, dtString);
  AddFetchConverter(myEnum, dtWideString);

  AddFetchConverter(mySet, dtString);
  AddFetchConverter(mySet, dtWideString);

  AddFetchConverter(myTinyBlob, dtMemo);
  AddFetchConverter(myTinyBlob, dtWideMemo);
  AddFetchConverter(myTinyBlob, dtString,     dtMemo);
  AddFetchConverter(myTinyBlob, dtWideString, dtWideMemo);

  AddFetchConverter(myMediumBlob, dtMemo);
  AddFetchConverter(myMediumBlob, dtWideMemo);
  AddFetchConverter(myMediumBlob, dtString,     dtMemo);
  AddFetchConverter(myMediumBlob, dtWideString, dtWideMemo);

  AddFetchConverter(myLongBlob, dtMemo);
  AddFetchConverter(myLongBlob, dtWideMemo);
  AddFetchConverter(myLongBlob, dtString,     dtMemo);
  AddFetchConverter(myLongBlob, dtWideString, dtWideMemo);

  AddFetchConverter(myBlob, dtMemo);
  AddFetchConverter(myBlob, dtWideMemo);
  AddFetchConverter(myBlob, dtString,     dtMemo);
  AddFetchConverter(myBlob, dtWideString, dtWideMemo);

  AddFetchConverter(myTinyText, dtMemo);
  AddFetchConverter(myTinyText, dtWideMemo);
  AddFetchConverter(myTinyText, dtString,     dtMemo);
  AddFetchConverter(myTinyText, dtWideString, dtWideMemo);

  AddFetchConverter(myMediumText, dtMemo);
  AddFetchConverter(myMediumText, dtWideMemo);
  AddFetchConverter(myMediumText, dtString,     dtMemo);
  AddFetchConverter(myMediumText, dtWideString, dtWideMemo);

  AddFetchConverter(myLongText, dtMemo);
  AddFetchConverter(myLongText, dtWideMemo);
  AddFetchConverter(myLongText, dtString,     dtMemo);
  AddFetchConverter(myLongText, dtWideString, dtWideMemo);

  AddFetchConverter(myText, dtMemo);
  AddFetchConverter(myText, dtWideMemo);
  AddFetchConverter(myText, dtString,     dtMemo);
  AddFetchConverter(myText, dtWideString, dtWideMemo);

  AddFetchConverter(myJSON, dtMemo);
  AddFetchConverter(myJSON, dtWideMemo);
  AddFetchConverter(myJSON, dtString,     dtMemo);
  AddFetchConverter(myJSON, dtWideString, dtWideMemo);
end;

function TMyConverterManager.GetDateFormat: string;
begin
  Result := 'YYYY-MM-DD';
end;

function TMyConverterManager.GetTimeFormat: string;
begin
  Result := 'HH:NN:SS';
end;

function TMyConverterManager.GetDateTimeFormat: string;
begin
  Result := 'YYYY-MM-DD HH:NN:SS';
end;

class function TMyConverterManager.GetDBProvider: Word;
begin
  Result := myBase;
end;

{$ENDIF}

class function TMyConverterManager.GetDBType(SQLType: Word;
  const LengthInChars, Flags: cardinal; const CharsetNr: integer): Word;

  function IsFlagSetted(const Flag: cardinal): boolean;
  begin
    Result := (Flags and Flag) <> 0;
  end;

begin
  case SQLType of // Must be sync with TCustomMyDataSet.SetNumberRange
    FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL:
      Result := myDecimal;
    FIELD_TYPE_BIT:
      Result := myBit;
    FIELD_TYPE_TINY:
      if IsFlagSetted(UNSIGNED_FLAG) then
        Result := myTinyUnsigned
      else
        Result := myTiny;
    FIELD_TYPE_SHORT:
      if IsFlagSetted(UNSIGNED_FLAG) then
        Result := mySmallUnsigned
      else
        Result := mySmall;
    FIELD_TYPE_INT24:
      if IsFlagSetted(UNSIGNED_FLAG) then
        Result := myMediumUnsigned
      else
        Result := myMedium;
    FIELD_TYPE_LONG:
      if IsFlagSetted(UNSIGNED_FLAG) then
        Result := myIntUnsigned
      else
        Result := myInt;
    FIELD_TYPE_LONGLONG:
      if IsFlagSetted(UNSIGNED_FLAG) then
        Result := myBigintUnsigned
      else
        Result := myBigint;

    FIELD_TYPE_FLOAT:
      Result := myFloat;
    FIELD_TYPE_DOUBLE:
      Result := myDouble;
    FIELD_TYPE_TIMESTAMP:
      Result := myTimestamp;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
      Result := myDate;
    FIELD_TYPE_TIME:
      Result := myTime;
    FIELD_TYPE_DATETIME:
      Result := myDateTime;
    FIELD_TYPE_YEAR:
      Result := myYear;

    FIELD_TYPE_VAR_STRING, FIELD_TYPE_VARCHAR:
      if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
        Result := myVarbinary
      else
        Result := myVarchar;
    FIELD_TYPE_STRING: begin
      if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
        Result := myBinary
      else
      if IsFlagSetted(ENUM_FLAG) then
        Result := myEnum
      else
      if IsFlagSetted(SET_FLAG) then
        Result := mySet
      else
        Result := myChar;
    end;

    FIELD_TYPE_NULL:
      Result := myVarchar;
    FIELD_TYPE_ENUM:
      Result := myEnum;
    FIELD_TYPE_SET:
      Result := mySet;

    {https://dev.mysql.com/doc/refman/8.0/en/c-api-data-structures.html
    To distinguish between binary and nonbinary data for string data types,check whether the charsetnr value is 63.
    If so, the character set is binary, which indicates binary rather than nonbinary data.
    This enables you to distinguish BINARY from CHAR, VARBINARY from VARCHAR, and the BLOB types from the TEXT types.}
    FIELD_TYPE_TINY_BLOB:
      if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
        Result := myTinyBlob
      else
        Result := myTinyText;
    FIELD_TYPE_MEDIUM_BLOB:
      if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
        Result := myMediumBlob
      else
        Result := myMediumText;
    FIELD_TYPE_LONG_BLOB:
      if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
        Result := myLongBlob
      else
        Result := myLongText;
    FIELD_TYPE_GEOMETRY,
    FIELD_TYPE_BLOB: begin
      if LengthInChars <= 255 then begin
        if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
          Result := myTinyBlob
        else
          Result := myTinyText;
      end
      else if LengthInChars <= 65535 then begin
        if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
          Result := myBlob
        else
          Result := myText;
      end
      else if LengthInChars <= 16777215 then begin
        if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
          Result := myMediumBlob
        else
          Result := myMediumText;
      end
      else
        if IsFlagSetted(BINARY_FLAG) and (CharsetNr = 63) then
          Result := myLongBlob
        else
          Result := myLongText;
    end;
    FIELD_TYPE_JSON:
      Result := myJSON;
  else
    Result := 0;
  end;
end;

{$IFNDEF LITE}

initialization
  InitMyTypes;
  MyConverterManager := TMyConverterManager.Create;

finalization
  MyConverterManager.Free;

{$ENDIF}

end.
