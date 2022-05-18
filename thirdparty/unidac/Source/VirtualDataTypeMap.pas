
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualDataTypeMap;

interface

uses
  SysUtils,
  MemData, CRDataTypeMap,
  LiteDataTypeMapVirtual;

const
  vqBase                = 5000;
  vqUnknown             = vqBase;

  vqString              = vqBase + 1;
  vqExtString           = vqBase + 2;
  vqWideString          = vqBase + 3;
  vqExtWideString       = vqBase + 4;
  vqFixedChar           = vqBase + 5;
  vqFixedWideChar       = vqBase + 6;

  vqInt8                = vqBase + 7;
  vqByte                = vqBase + 8;
  vqSmallint            = vqBase + 9;
  vqInteger             = vqBase + 10;
  vqWord                = vqBase + 11;
  vqLongWord            = vqBase + 12;
  vqInt64               = vqBase + 13;
  vqUInt64              = vqBase + 14;

  vqSingle              = vqBase + 15;
  vqFloat               = vqBase + 16;
  vqExtended            = vqBase + 17;
  vqCurrency            = vqBase + 18;

  vqNumeric             = vqBase + 19;
  vqBCD                 = vqBase + 20;
  vqFMTBCD              = vqBase + 21;

  vqDate                = vqBase + 22;
  vqTime                = vqBase + 23;
  vqDateTime            = vqBase + 24;
  vqSQLTimeStamp        = vqBase + 25;
  vqSQLTimeStampOffset  = vqBase + 26;

  vqBoolean             = vqBase + 27;
  vqBytes               = vqBase + 28;
  vqVarBytes            = vqBase + 29;
  vqExtVarBytes         = vqBase + 30;

  vqBlob                = vqBase + 31;
  vqMemo                = vqBase + 32;
  vqWideMemo            = vqBase + 33;

  vqObject              = vqBase + 34;
  vqArray               = vqBase + 35;
  vqTable               = vqBase + 36;
  vqGuid                = vqBase + 37;
  vqCursor              = vqBase + 38;
  vqXML                 = vqBase + 39;

type
  TVirtualMapRules = class(TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TVirtualConverterManager = class(TLiteConverterManager)
  public
    constructor Create;

    class function GetDBProvider: Word; override;
    class function GetDBType(const SQLTypeName: string): Word; overload; override;
    class function GetDBType(SQLType: integer): Word; overload; override;
    class function GetDataType(SQLType: integer): Word;
    class function DataTypeToDBType(DataType: integer): Word;
  end;

procedure InitVirtualTypes;

implementation

uses
  LiteCallVirtual;

var
  VirtualConverterManager: TVirtualConverterManager;

procedure InitVirtualTypes;
begin
  DBTypeInfos.Add(vqString,             'String',           True, False);
  DBTypeInfos.Add(vqWideString,         'WideString',       True, False);
  DBTypeInfos.Add(vqFixedChar,          'Char',             True, False);
  DBTypeInfos.Add(vqFixedWideChar,      'WideChar',         True, False);

  DBTypeInfos.Add(vqInt8,               'TinyInt',          False, False);
  DBTypeInfos.Add(vqByte,               'Byte',             False, False);
  DBTypeInfos.Add(vqSmallint,           'SmallInt',         False, False);
  DBTypeInfos.Add(vqInteger,            'Integer',          False, False);
  DBTypeInfos.Add(vqWord,               'Word',             False, False);
  DBTypeInfos.Add(vqLongWord,           'LongWord',         False, False);
  DBTypeInfos.Add(vqInt64,              'Int64',            False, False);
  DBTypeInfos.Add(vqUInt64,             'UInt64',           False, False);

  DBTypeInfos.Add(vqSingle,             'Single',           False, False);
  DBTypeInfos.Add(vqFloat,              'Float',            False, False);
  DBTypeInfos.Add(vqExtended,           'Extended',         False, False);
  DBTypeInfos.Add(vqCurrency,           'Currency',         False, False);

  DBTypeInfos.Add(vqNumeric,            'Numeric',          False, False);
  DBTypeInfos.Add(vqBCD,                'BCD',              False, False);
  DBTypeInfos.Add(vqFMTBCD,             'FmtBCD',           False, False);

  DBTypeInfos.Add(vqDate,               'Date',             False, False);
  DBTypeInfos.Add(vqTime,               'Time',             False, False);
  DBTypeInfos.Add(vqDateTime,           'DateTime',         False, False);
  DBTypeInfos.Add(vqSQLTimeStamp,       'Timestamp',        False, False);
  DBTypeInfos.Add(vqSQLTimeStampOffset, 'TimestampOffset',  False, False);

  DBTypeInfos.Add(vqBoolean,            'Boolean',          False, False);
  DBTypeInfos.Add(vqBytes,              'Bytes',            True, False);
  DBTypeInfos.Add(vqVarBytes,           'VarBytes',         True, False);
  DBTypeInfos.Add(vqExtVarBytes,        'ExtVarBytes',      True, False);

  DBTypeInfos.Add(vqBlob,               'BLOB',             False, False);
  DBTypeInfos.Add(vqMemo,               'Memo',             False, False);
  DBTypeInfos.Add(vqWideMemo,           'WideMemo',         False, False);

  DBTypeInfos.Add(vqObject,             'Object',           False, False);
  DBTypeInfos.Add(vqArray,              'Array',            True, False);
  DBTypeInfos.Add(vqTable,              'Table',            False, False);
  DBTypeInfos.Add(vqGuid,               'GUID',             False, False);
  DBTypeInfos.Add(vqCursor,             'Cursor',           False, False);
  DBTypeInfos.Add(vqXML,                'XML',              False, False);
end;

{ TVirtualMapRules }

class function TVirtualMapRules.GetConverterManager: TConverterManager;
begin
  Result := VirtualConverterManager;
end;


{ TVirtualConverterManager }

constructor TVirtualConverterManager.Create;
begin
  inherited;

  AddFetchConverter(vqString, dtString);
  AddFetchConverter(vqString, dtWideString);
  AddFetchConverter(vqWideString, dtString);
  AddFetchConverter(vqWideString, dtWideString);
  AddFetchConverter(vqFixedChar, dtString);
  AddFetchConverter(vqFixedChar, dtFixedChar,     dtString);
  AddFetchConverter(vqFixedChar, dtWideString);
  AddFetchConverter(vqFixedChar, dtFixedWideChar, dtWideString);
  AddFetchConverter(vqFixedWideChar, dtString);
  AddFetchConverter(vqFixedWideChar, dtFixedChar,     dtString);
  AddFetchConverter(vqFixedWideChar, dtWideString);
  AddFetchConverter(vqFixedWideChar, dtFixedWideChar, dtWideString);
  AddFetchConverter(vqMemo, dtMemo);
  AddFetchConverter(vqMemo, dtWideMemo);
  AddFetchConverter(vqMemo, dtString,     dtMemo);
  AddFetchConverter(vqMemo, dtWideString, dtMemo);
  AddFetchConverter(vqWideMemo, dtMemo);
  AddFetchConverter(vqWideMemo, dtWideMemo);
  AddFetchConverter(vqWideMemo, dtString,     dtMemo);
  AddFetchConverter(vqWideMemo, dtWideString, dtMemo);
end;

class function TVirtualConverterManager.GetDBProvider: Word;
begin
  Result := vqBase;
end;

class function TVirtualConverterManager.GetDBType(const SQLTypeName: string): Word;
begin
  if SQLTypeName = 'BOOLEAN' then
    Result := vqBoolean
  else if SQLTypeName = 'TINYINT' then
    Result := vqInt8
  else if SQLTypeName = 'BYTE' then
    Result := vqByte
  else if SQLTypeName = 'SMALLINT' then
    Result := vqSmallint
  else if SQLTypeName = 'INTEGER' then
    Result := vqInteger
  else if SQLTypeName = 'WORD' then
    Result := vqWord
  else if SQLTypeName = 'LONGWORD' then
    Result := vqLongWord
  else if SQLTypeName = 'BIGINT' then
    Result := vqInt64
  else if SQLTypeName = 'UBIGINT' then
    Result := vqUInt64
  else if SQLTypeName = 'EXTVARCHAR' then
    Result := vqExtString
  else if SQLTypeName = 'CHAR' then
    Result := vqFixedChar
  else if SQLTypeName = 'WCHAR' then
    Result := vqFixedWideChar
  else if SQLTypeName = 'VARCHAR' then
    Result := vqString
  else if SQLTypeName = 'EXTVARCHAR' then
    Result := vqExtString
  else if SQLTypeName = 'WVARCHAR' then
    Result := vqWideString
  else if SQLTypeName = 'EXTWVARCHAR' then
    Result := vqExtWideString
  else if SQLTypeName = 'TEXT' then
    Result := vqMemo
  else if SQLTypeName = 'WTEXT' then
    Result := vqWideMemo
  else if SQLTypeName = 'REAL' then
    Result := vqSingle
  else if SQLTypeName = 'FLOAT' then
    Result := vqFloat
  else if SQLTypeName = 'DOUBLE' then
    Result := vqFloat
  else if SQLTypeName = 'EXTENDED' then
    Result := vqExtended
  else if SQLTypeName = 'MONEY' then
    Result := vqCurrency
  else if SQLTypeName = 'CURRENCY' then
    Result := vqCurrency
  else if SQLTypeName = 'DATE' then
    Result := vqDate
  else if SQLTypeName = 'TIME' then
    Result := vqTime
  else if SQLTypeName = 'DATETIME' then
    Result := vqDateTime
  else if SQLTypeName = 'TIMESTAMP' then
    Result := vqSQLTimeStamp
  else if SQLTypeName = 'TIMESTAMPOFFSET' then
    Result := vqSQLTimeStampOffset
  else if SQLTypeName = 'BCD' then
    Result := vqBCD
  else if SQLTypeName = 'NUMERIC' then
    Result := vqNumeric
  else if SQLTypeName = 'FMTBCD' then
    Result := vqFMTBCD
  else if SQLTypeName = 'BLOB' then
    Result := vqBlob
  else if SQLTypeName = 'BYTES' then
    Result := vqBytes
  else if SQLTypeName = 'VARBYTES' then
    Result := vqVarBytes
  else if SQLTypeName = 'EXTVARBYTES' then
    Result := vqExtVarBytes
  else if SQLTypeName = 'ARRAY' then
    Result := vqArray
  else if SQLTypeName = 'OBJECT' then
    Result := vqObject
  else if SQLTypeName = 'GUID' then
    Result := vqGuid
  else if SQLTypeName = 'CURSOR' then
    Result := vqCursor
  else if SQLTypeName = 'XML' then
    Result := vqXML
  else if SQLTypeName = 'ATABLE' then
    Result := vqTable
  else
    Result := vqUnknown;
end;

class function TVirtualConverterManager.GetDBType(SQLType: integer): Word;
begin
  Result := vqUnknown;

  case SQLType of
    SQLITE_INTEGER:
      Result := vqInt64;
    SQLITE_FLOAT:
      Result := vqFloat;
    SQLITE_TEXT,
    SQLITE_NULL:
      Result := vqString;
    SQLITE_BLOB:
      Result := vqBlob;
  else
    Assert(False);
  end;
end;

class function TVirtualConverterManager.GetDataType(SQLType: integer): Word;
begin
  case SQLType of
    vqString:
      Result := dtString;
    vqExtString:
      Result := dtExtString;
    vqWideString:
      Result := dtWideString;
    vqExtWideString:
      Result := dtExtWideString;
    vqFixedChar:
      Result := dtString;
    vqFixedWideChar:
      Result := dtWideString;
    vqInt8:
      Result := dtInt8;
    vqByte:
      Result := dtByte;
    vqSmallint:
      Result := dtSmallint;
    vqInteger:
      Result := dtInteger;
    vqWord:
      Result := dtWord;
    vqLongWord:
      Result := dtLongWord;
    vqInt64:
      Result := dtInt64;
    vqUInt64:
      Result := dtUInt64;
    vqSingle:
      Result := dtSingle;
    vqFloat:
      Result := dtFloat;
    vqExtended:
      Result := dtExtended;
    vqCurrency:
      Result := dtCurrency;
    vqBCD:
      Result := dtBCD;
    vqFMTBCD:
      Result := dtFMTBCD;
    vqDate:
      Result := dtDate;
    vqTime:
      Result := dtTime;
    vqDateTime:
      Result := dtDateTime;
    vqSQLTimeStamp:
      Result := dtSQLTimeStamp;
    vqSQLTimeStampOffset:
      Result := dtSQLTimeStampOffset;
    vqBoolean:
      Result := dtBoolean;
    vqBytes:
      Result := dtBytes;
    vqVarBytes:
      Result := dtVarBytes;
    vqExtVarBytes:
      Result := dtExtVarBytes;
    vqBlob:
      Result := dtBlob;
    vqMemo:
      Result := dtMemo;
    vqWideMemo:
      Result := dtWideMemo;
    vqObject:
      Result := dtObject;
    vqArray:
      Result := dtArray;
    vqTable:
      Result := dtTable;
    vqGuid:
      Result := dtGuid;
    vqCursor:
      Result := dtCursor;
    vqXML:
      Result := dtXML;
  else
    Result := dtUnknown;
  end;
end;

class function TVirtualConverterManager.DataTypeToDBType(DataType: integer): Word;
begin
  case DataType of
    dtString:
      Result := vqString;
    dtExtString:
      Result := vqExtString;
    dtWideString:
      Result := vqWideString;
    dtExtWideString:
      Result := vqExtWideString;
    dtFixedChar:
      Result := vqFixedChar;
    dtFixedWideChar:
      Result := vqFixedWideChar;
    dtInt8:
      Result := vqInt8;
    dtByte:
      Result := vqByte;
    dtSmallint:
      Result := vqSmallint;
    dtInteger:
      Result := vqInteger;
    dtWord:
      Result := vqWord;
    dtLongWord:
      Result := vqLongWord;
    dtInt64:
      Result := vqInt64;
    dtUInt64:
      Result := vqUInt64;
    dtSingle:
      Result := vqSingle;
    dtFloat:
      Result := vqFloat;
    dtExtended:
      Result := vqExtended;
    dtCurrency:
      Result := vqCurrency;
    dtBCD:
      Result := vqBCD;
    dtFMTBCD:
      Result := vqFMTBCD;
    dtDate:
      Result := vqDate;
    dtTime:
      Result := vqTime;
    dtDateTime:
      Result := vqDateTime;
    dtSQLTimeStamp:
      Result := vqSQLTimeStamp;
    dtSQLTimeStampOffset:
      Result := vqSQLTimeStampOffset;
    dtBoolean:
      Result := vqBoolean;
    dtBytes:
      Result := vqBytes;
    dtVarBytes:
      Result := vqVarBytes;
    dtExtVarBytes:
      Result := vqExtVarBytes;
    dtBlob:
      Result := vqBlob;
    dtMemo:
      Result := vqMemo;
    dtWideMemo:
      Result := vqWideMemo;
    dtObject:
      Result := vqObject;
    dtArray:
      Result := vqArray;
    dtTable:
      Result := vqTable;
    dtGuid:
      Result := vqGuid;
    dtCursor:
      Result := vqCursor;
    dtXML:
      Result := vqXML;
  else
    Result := vqUnknown;
  end;
end;

initialization
  InitVirtualTypes;
  VirtualConverterManager := TVirtualConverterManager.Create;

finalization
  VirtualConverterManager.Free;

end.
