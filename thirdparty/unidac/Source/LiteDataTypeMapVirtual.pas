
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2014 Devart. All right reserved.
//////////////////////////////////////////////////

{$I VirtualQuery.inc}
{$I LiteDac.inc}
unit LiteDataTypeMapVirtual;

interface

uses
{$IFNDEF LITE}
  SysUtils, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CRTypes, CRDataTypeMap, CRFunctions, CRTimeStamp,
{$ENDIF}
  MemData;

const
  liteBase                  = 600;

  { native SQLite types }

  liteInteger               = liteBase + 1;
  liteReal                  = liteBase + 2;
  liteText                  = liteBase + 3;
  liteBlob                  = liteBase + 4;
  liteNull                  = liteBase + 5;

  { additional types handled by LiteDAC }

  liteTinyInt               = liteBase + 6;
  liteInt2                  = liteBase + 7;
  liteSmallInt              = liteBase + 8;
  liteInt                   = liteBase + 9;
  liteMediumInt             = liteBase + 10;
  liteBigInt                = liteBase + 11;
  liteUBigInt               = liteBase + 12;
  liteInt8                  = liteBase + 13;
  liteInt64                 = liteBase + 14;

  liteChar                  = liteBase + 15;
  liteVarChar               = liteBase + 16;
  liteClob                  = liteBase + 17;

  liteFloat                 = liteBase + 18;
  liteDouble                = liteBase + 19;
  liteNumeric               = liteBase + 20;
  liteDecimal               = liteBase + 21;
  liteNumber                = liteBase + 22;
  liteMoney                 = liteBase + 23;

  liteBool                  = liteBase + 24;

  liteBinary                = liteBase + 25;
  liteVarBinary             = liteBase + 26;

  liteDate                  = liteBase + 27;
  liteTime                  = liteBase + 28;
  liteDateTime              = liteBase + 29;
  liteTimestamp             = liteBase + 30;

  liteBit                   = liteBase + 31;

  liteTimestampTZ           = liteBase + 32;

type

  TLiteConverterManager = class{$IFNDEF LITE}(TConverterManager){$ENDIF}
  public
  {$IFNDEF LITE}
    constructor Create;

    class function GetDBProvider: Word; override;
  {$ENDIF}
    class function GetDBType(const SQLTypeName: string): Word; overload; virtual;
    class function GetDBType(SQLType: integer): Word; overload; virtual;
  end;

{$IFNDEF LITE}
  TLiteMapRules = class(TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TLiteDataConverters = class(TDataConverters)
  public
    class function IntToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function RealToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToInt(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToReal(var AConvertInfo: TConvertInfo): TConvertStatus;

    class function MemoToInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToUInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToUInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToUInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
{$IFDEF USE_UINT64}
    class function MemoToUInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
{$ENDIF}
    class function MemoToSingle(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToFloat(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToExtended(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToDate(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToTime(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToSQLTimeStamp(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function MemoToBoolean(var AConvertInfo: TConvertInfo): TConvertStatus;

    class function Int8ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
{$IFDEF USE_UINT64}
    class function UInt64ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
{$ENDIF}
    class function SingleToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtendedToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function DateToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function TimeToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function DateTimeToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function SQLTimeStampToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function BooleanToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
  end;

  function IsLiteUnsignedType(DBType: Word): boolean;
  procedure InitLiteTypes;
{$ENDIF}

implementation

uses
  StrUtils,
  CLRClasses, MemUtils,
  DateUtils,
{$IFDEF VIRTUAL_QUERY}
  LiteCallVirtual, LiteClassesVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteCall, LiteClasses;
{$ELSE}
  LiteCallUni, LiteClassesUni;
{$ENDIF}
{$ENDIF}

{$IFNDEF LITE}

var
  LiteConverterManager: TLiteConverterManager;

function IsLiteUnsignedType(DBType: Word): boolean;
begin
  case DBType of
    liteUBigInt:
      Result := True;
    else
      Result := False;
  end;
end;

procedure InitLiteTypes;
begin
  DBTypeInfos.Add(liteInteger,     'Integer',                  False, False);
  DBTypeInfos.Add(liteReal,        'Real',                     False, False);
  DBTypeInfos.Add(liteText,        'Text',                     False, False);
  DBTypeInfos.Add(liteBlob,        'BLOB',                     False, False);
  DBTypeInfos.Add(liteNull,        'Unknown',                  False, False);

  DBTypeInfos.Add(liteBit,         'Bit',                      False, False);
  DBTypeInfos.Add(liteTinyInt,     'TinyInt',                  False, False);
  DBTypeInfos.Add(liteSmallInt,    'SmallInt',                 False, False);
  DBTypeInfos.Add(liteInt2,        'Int2',                     False, False);
  DBTypeInfos.Add(liteInt,         'Int',                      False, False);
  DBTypeInfos.Add(liteMediumInt,   'MediumInt',                False, False);
  DBTypeInfos.Add(liteBigInt,      'BigInt',                   False, False);
  DBTypeInfos.Add(liteUBigInt,     'Unsigned BigInt',          False, False);
  DBTypeInfos.Add(liteInt8,        'Int8',                     False, False);
  DBTypeInfos.Add(liteInt64,       'Int64',                    False, False);
  DBTypeInfos.Add(liteChar,        'Char',                     True,  False);
  DBTypeInfos.Add(liteVarChar,     'VarChar',                  True,  False);
  DBTypeInfos.Add(liteClob,        'CLOB',                     False, False);
  DBTypeInfos.Add(liteFloat,       'Float',                    False, False);
  DBTypeInfos.Add(liteDouble,      'Double',                   False, False);
  DBTypeInfos.Add(liteNumeric,     'Numeric',                  True,  True);
  DBTypeInfos.Add(liteDecimal,     'Decimal',                  True,  True);
  DBTypeInfos.Add(liteNumber,      'Number',                   True,  True);
  DBTypeInfos.Add(liteMoney,       'Money',                    False, False);
  DBTypeInfos.Add(liteBool,        'Bool',                     False, False);
  DBTypeInfos.Add(liteBinary,      'Binary',                   True,  False);
  DBTypeInfos.Add(liteVarBinary,   'VarBinary',                True,  False);
  DBTypeInfos.Add(liteDate,        'Date',                     False, False);
  DBTypeInfos.Add(liteTime,        'Time',                     False, False);
  DBTypeInfos.Add(liteDateTime,    'DateTime',                 False, False);
  DBTypeInfos.Add(liteTimestamp,   'Timestamp',                False, False);
  DBTypeInfos.Add(liteTimestampTZ, 'Timestamp with time zone', False, False);
end;

{ TLiteConverterManager }

constructor TLiteConverterManager.Create;
begin
  inherited;

  AddFetchConverter(liteBlob, dtMemo);
  AddFetchConverter(liteBlob, dtWideMemo);
  AddFetchConverter(liteBlob, dtString,     dtMemo);
  AddFetchConverter(liteBlob, dtWideString, dtWideMemo);

  AddFetchConverter(liteClob, dtMemo);
  AddFetchConverter(liteClob, dtWideMemo);
  AddFetchConverter(liteClob, dtString,     dtMemo);
  AddFetchConverter(liteClob, dtWideString, dtWideMemo);

  AddFetchConverter(liteChar, dtString);
  AddFetchConverter(liteChar, dtFixedChar,     dtString);
  AddFetchConverter(liteChar, dtWideString);
  AddFetchConverter(liteChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(liteVarChar, dtString);
  AddFetchConverter(liteVarChar, dtWideString);

  AddFetchConverter(liteText, dtMemo);
  AddFetchConverter(liteText, dtWideMemo);
  AddFetchConverter(liteText, dtString,     dtMemo);
  AddFetchConverter(liteText, dtWideString, dtWideMemo);
  AddFetchConverter(liteText, dtUnknown);
  AddFetchConverter(liteText, dtBoolean,    dtMemo);
  AddFetchConverter(liteText, dtInt8);
  AddFetchConverter(liteText, dtInt16);
  AddFetchConverter(liteText, dtUInt16);
  AddFetchConverter(liteText, dtInt32);
  AddFetchConverter(liteText, dtUInt32);
  AddFetchConverter(liteText, dtInt64);
  AddFetchConverter(liteText, dtBCD);
  AddFetchConverter(liteText, dtFMTBCD);
  AddFetchConverter(liteText, dtFloat);
  AddFetchConverter(liteText, dtCurrency);
  AddFetchConverter(liteText, dtDate);
  AddFetchConverter(liteText, dtTime);
  AddFetchConverter(liteText, dtDateTime);
  AddFetchConverter(liteText, dtBytes);
  AddFetchConverter(liteText, dtVarBytes);

  AddFetchConverter(liteInteger, dtInt8,     dtInt32);
  AddFetchConverter(liteInteger, dtUInt8,    dtInt32);
  AddFetchConverter(liteInteger, dtInt16,    dtInt32);
  AddFetchConverter(liteInteger, dtUInt16,   dtInt32);
  AddFetchConverter(liteInteger, dtInt32,    dtInt32);
  AddFetchConverter(liteInteger, dtUInt32,   dtInt32);
  AddFetchConverter(liteInteger, dtInt64,    dtInt64);
  AddFetchConverter(liteInteger, dtUInt64,   dtInt64);
  AddFetchConverter(liteInteger, dtCurrency, dtFloat);

  AddFetchConverter(liteTinyInt, dtInt8,     dtInt8);
  AddFetchConverter(liteTinyInt, dtUInt8,    dtInt8);
  AddFetchConverter(liteTinyInt, dtInt16,    dtInt16);
  AddFetchConverter(liteTinyInt, dtUInt16,   dtInt16);
  AddFetchConverter(liteTinyInt, dtInt32,    dtInt32);
  AddFetchConverter(liteTinyInt, dtUInt32,   dtInt32);
  AddFetchConverter(liteTinyInt, dtInt64,    dtInt64);
  AddFetchConverter(liteTinyInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteTinyInt, dtCurrency, dtFloat);

  AddFetchConverter(liteSmallInt, dtInt8,     dtInt16);
  AddFetchConverter(liteSmallInt, dtUInt8,    dtInt16);
  AddFetchConverter(liteSmallInt, dtInt16,    dtInt16);
  AddFetchConverter(liteSmallInt, dtUInt16,   dtInt16);
  AddFetchConverter(liteSmallInt, dtInt32,    dtInt32);
  AddFetchConverter(liteSmallInt, dtUInt32,   dtInt32);
  AddFetchConverter(liteSmallInt, dtInt64,    dtInt64);
  AddFetchConverter(liteSmallInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteSmallInt, dtCurrency, dtFloat);

  AddFetchConverter(liteInt2, dtInt8,     dtInt16);
  AddFetchConverter(liteInt2, dtUInt8,    dtInt16);
  AddFetchConverter(liteInt2, dtInt16,    dtInt16);
  AddFetchConverter(liteInt2, dtUInt16,   dtInt16);
  AddFetchConverter(liteInt2, dtInt32,    dtInt32);
  AddFetchConverter(liteInt2, dtUInt32,   dtInt32);
  AddFetchConverter(liteInt2, dtInt64,    dtInt64);
  AddFetchConverter(liteInt2, dtUInt64,   dtInt64);
  AddFetchConverter(liteInt2, dtCurrency, dtFloat);

  AddFetchConverter(liteInt, dtInt8,     dtInt32);
  AddFetchConverter(liteInt, dtUInt8,    dtInt32);
  AddFetchConverter(liteInt, dtInt16,    dtInt32);
  AddFetchConverter(liteInt, dtUInt16,   dtInt32);
  AddFetchConverter(liteInt, dtInt32,    dtInt32);
  AddFetchConverter(liteInt, dtUInt32,   dtInt32);
  AddFetchConverter(liteInt, dtInt64,    dtInt64);
  AddFetchConverter(liteInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteInt, dtCurrency, dtFloat);

  AddFetchConverter(liteMediumInt, dtInt8,     dtInt32);
  AddFetchConverter(liteMediumInt, dtUInt8,    dtInt32);
  AddFetchConverter(liteMediumInt, dtInt16,    dtInt32);
  AddFetchConverter(liteMediumInt, dtUInt16,   dtInt32);
  AddFetchConverter(liteMediumInt, dtInt32,    dtInt32);
  AddFetchConverter(liteMediumInt, dtUInt32,   dtInt32);
  AddFetchConverter(liteMediumInt, dtInt64,    dtInt64);
  AddFetchConverter(liteMediumInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteMediumInt, dtCurrency, dtFloat);

  AddFetchConverter(liteBigInt, dtInt8,     dtInt64);
  AddFetchConverter(liteBigInt, dtUInt8,    dtInt64);
  AddFetchConverter(liteBigInt, dtInt16,    dtInt64);
  AddFetchConverter(liteBigInt, dtUInt16,   dtInt64);
  AddFetchConverter(liteBigInt, dtInt32,    dtInt64);
  AddFetchConverter(liteBigInt, dtUInt32,   dtInt64);
  AddFetchConverter(liteBigInt, dtInt64,    dtInt64);
  AddFetchConverter(liteBigInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteBigInt, dtCurrency, dtFloat);

  AddFetchConverter(liteUBigInt, dtInt8,     dtInt64);
  AddFetchConverter(liteUBigInt, dtUInt8,    dtInt64);
  AddFetchConverter(liteUBigInt, dtInt16,    dtInt64);
  AddFetchConverter(liteUBigInt, dtUInt16,   dtInt64);
  AddFetchConverter(liteUBigInt, dtInt32,    dtInt64);
  AddFetchConverter(liteUBigInt, dtUInt32,   dtInt64);
  AddFetchConverter(liteUBigInt, dtInt64,    dtInt64);
  AddFetchConverter(liteUBigInt, dtUInt64,   dtInt64);
  AddFetchConverter(liteUBigInt, dtCurrency, dtFloat);

  AddFetchConverter(liteInt8, dtInt8,     dtInt64);
  AddFetchConverter(liteInt8, dtUInt8,    dtInt64);
  AddFetchConverter(liteInt8, dtInt16,    dtInt64);
  AddFetchConverter(liteInt8, dtUInt16,   dtInt64);
  AddFetchConverter(liteInt8, dtInt32,    dtInt64);
  AddFetchConverter(liteInt8, dtUInt32,   dtInt64);
  AddFetchConverter(liteInt8, dtInt64,    dtInt64);
  AddFetchConverter(liteInt8, dtUInt64,   dtInt64);
  AddFetchConverter(liteInt8, dtCurrency, dtFloat);

  AddFetchConverter(liteInt64, dtInt8,     dtInt64);
  AddFetchConverter(liteInt64, dtUInt8,    dtInt64);
  AddFetchConverter(liteInt64, dtInt16,    dtInt64);
  AddFetchConverter(liteInt64, dtUInt16,   dtInt64);
  AddFetchConverter(liteInt64, dtInt32,    dtInt64);
  AddFetchConverter(liteInt64, dtUInt32,   dtInt64);
  AddFetchConverter(liteInt64, dtInt64,    dtInt64);
  AddFetchConverter(liteInt64, dtUInt64,   dtInt64);
  AddFetchConverter(liteInt64, dtCurrency, dtFloat);

  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtInt8,   dtInt16);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtInt16,  dtInt16);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtUInt16, dtInt16);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumeric,  1,  4, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtInt8,   dtInt32);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtInt16,  dtInt32);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtUInt16, dtInt32);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumeric,  5,  9, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtInt8,   dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtInt16,  dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtUInt16, dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtInt32,  dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtUInt32, dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumeric, 10, -1, 0, 0, dtUInt64, dtInt64);

  AddFetchConverter(liteNumeric, -1, -1,  1, -1, dtFloat);
  AddFetchConverter(liteNumeric, -1, -1,  1, -1, dtCurrency);
  AddFetchConverter(liteNumeric, -1, -1,  1, -1, dtBCD);
  AddFetchConverter(liteNumeric, -1, -1,  1, -1, dtFmtBCD);

  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtInt8,   dtInt16);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtInt16,  dtInt16);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtUInt16, dtInt16);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteDecimal,  1,  4, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtInt8,   dtInt32);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtInt16,  dtInt32);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtUInt16, dtInt32);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteDecimal,  5,  9, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtInt8,   dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtInt16,  dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtUInt16, dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtInt32,  dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtUInt32, dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteDecimal, 10, -1, 0, 0, dtUInt64, dtInt64);

  AddFetchConverter(liteDecimal, -1, -1,  1, -1, dtFloat);
  AddFetchConverter(liteDecimal, -1, -1,  1, -1, dtCurrency);
  AddFetchConverter(liteDecimal, -1, -1,  1, -1, dtBCD);
  AddFetchConverter(liteDecimal, -1, -1,  1, -1, dtFmtBCD);

  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtInt8,   dtInt16);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtInt16,  dtInt16);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtUInt16, dtInt16);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumber,  1,  4, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtInt8,   dtInt32);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtInt16,  dtInt32);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtUInt16, dtInt32);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtInt32,  dtInt32);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtUInt32, dtInt32);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumber,  5,  9, 0, 0, dtUInt64, dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtInt8,   dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtInt16,  dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtUInt16, dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtInt32,  dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtUInt32, dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtInt64,  dtInt64);
  AddFetchConverter(liteNumber, 10, -1, 0, 0, dtUInt64, dtInt64);

  AddFetchConverter(liteNumber, -1, -1,  1, -1, dtFloat);
  AddFetchConverter(liteNumber, -1, -1,  1, -1, dtCurrency);
  AddFetchConverter(liteNumber, -1, -1,  1, -1, dtBCD);
  AddFetchConverter(liteNumber, -1, -1,  1, -1, dtFmtBCD);

  AddFetchConverter(liteBinary, dtString);
  AddFetchConverter(liteBinary, dtWideString);

  AddFetchConverter(liteVarBinary, dtString);
  AddFetchConverter(liteVarBinary, dtWideString);

  AddFetchConverter(liteDate, dtDateTime);

  AddFetchConverter(liteTime, dtDateTime);

  AddFetchConverter(liteDateTime, dtTime);
  AddFetchConverter(liteDateTime, dtDate);

  AddFetchConverter(liteTimeStamp,   dtTime);
  AddFetchConverter(liteTimeStamp,   dtDate);
  AddFetchConverter(liteTimeStamp,   dtDateTime);
  AddFetchConverter(liteTimeStamp,   dtFloat);
  AddFetchConverter(liteTimeStamp,   dtExtended);
  AddFetchConverter(liteTimeStampTZ, dtTime);
  AddFetchConverter(liteTimeStampTZ, dtDate);
  AddFetchConverter(liteTimeStampTZ, dtDateTime);
  AddFetchConverter(liteTimeStampTZ, dtFloat);
  AddFetchConverter(liteTimeStampTZ, dtExtended);

  // liteNull can be mapped to any of the data type
  // because SQLite can return the NULL field type for any field of a non-standard type
  // for example, for calulated fields (like "select FIELD1 + FIELD2 from ...")
  // so user must have the ability to map such fields to anything he wants

  AddFetchConverter(liteNull, dtUnknown);
  AddFetchConverter(liteNull, dtBoolean,    dtUInt16);
  AddFetchConverter(liteNull, dtMemo);
  AddFetchConverter(liteNull, dtWideMemo);
  AddFetchConverter(liteNull, dtString,     dtMemo);
  AddFetchConverter(liteNull, dtWideString, dtWideMemo);
  AddFetchConverter(liteNull, dtInt8,       dtInt32);
  AddFetchConverter(liteNull, dtInt16,      dtInt32);
  AddFetchConverter(liteNull, dtUInt16,     dtInt32);
  AddFetchConverter(liteNull, dtInt32);
  AddFetchConverter(liteNull, dtUInt32,     dtInt32);
  AddFetchConverter(liteNull, dtInt64);
  AddFetchConverter(liteNull, dtUInt64,     dtInt64);
  AddFetchConverter(liteNull, dtBCD);
  AddFetchConverter(liteNull, dtFMTBCD);
  AddFetchConverter(liteNull, dtFloat);
  AddFetchConverter(liteNull, dtCurrency);
  AddFetchConverter(liteNull, dtDate);
  AddFetchConverter(liteNull, dtTime);
  AddFetchConverter(liteNull, dtDateTime);
  AddFetchConverter(liteNull, dtBytes);
  AddFetchConverter(liteNull, dtVarBytes);
  AddFetchConverter(liteNull, dtExtended);

  AddOnDemandConverter(dtInteger, dtDateTime, TLiteDataConverters.IntToDateTime, TLiteDataConverters.DateTimeToInt);
  AddOnDemandConverter(dtInt64, dtDateTime, TLiteDataConverters.Int64ToDateTime, TLiteDataConverters.DateTimeToInt64);
  AddOnDemandConverter(dtFloat, dtDateTime, TLiteDataConverters.RealToDateTime, TLiteDataConverters.DateTimeToReal);

  AddOnDemandConverter(dtMemo, dtInt8, TLiteDataConverters.MemoToInt8, TLiteDataConverters.Int8ToMemo);
  AddOnDemandConverter(dtMemo, dtUInt8, TLiteDataConverters.MemoToUInt8, TLiteDataConverters.UInt8ToMemo);
  AddOnDemandConverter(dtMemo, dtSmallint, TLiteDataConverters.MemoToInt16, TLiteDataConverters.Int16ToMemo);
  AddOnDemandConverter(dtMemo, dtWord, TLiteDataConverters.MemoToUInt16, TLiteDataConverters.UInt16ToMemo);
  AddOnDemandConverter(dtMemo, dtInteger, TLiteDataConverters.MemoToInt32, TLiteDataConverters.Int32ToMemo);
  AddOnDemandConverter(dtMemo, dtUInt32, TLiteDataConverters.MemoToUInt32, TLiteDataConverters.UInt32ToMemo);
  AddOnDemandConverter(dtMemo, dtInt64, TLiteDataConverters.MemoToInt64, TLiteDataConverters.Int64ToMemo);
{$IFDEF USE_UINT64}
  AddOnDemandConverter(dtMemo, dtUInt64, TLiteDataConverters.MemoToUInt64, TLiteDataConverters.UInt64ToMemo);
{$ENDIF}
  AddOnDemandConverter(dtMemo, dtSingle, TLiteDataConverters.MemoToFloat, TLiteDataConverters.FloatToMemo);
  AddOnDemandConverter(dtMemo, dtFloat, TLiteDataConverters.MemoToFloat, TLiteDataConverters.FloatToMemo);
  AddOnDemandConverter(dtMemo, dtCurrency, TLiteDataConverters.MemoToFloat, TLiteDataConverters.FloatToMemo);
  AddOnDemandConverter(dtMemo, dtExtended, TLiteDataConverters.MemoToExtended, TLiteDataConverters.ExtendedToMemo);
  AddOnDemandConverter(dtMemo, dtDate, TLiteDataConverters.MemoToDate, TLiteDataConverters.DateToMemo);
  AddOnDemandConverter(dtMemo, dtTime, TLiteDataConverters.MemoToTime, TLiteDataConverters.TimeToMemo);
  AddOnDemandConverter(dtMemo, dtDateTime, TLiteDataConverters.MemoToDateTime, TLiteDataConverters.DateTimeToMemo);
  AddOnDemandConverter(dtMemo, dtSQLTimeStamp, TLiteDataConverters.MemoToSQLTimeStamp, TLiteDataConverters.SQLTimeStampToMemo);
  AddOnDemandConverter(dtMemo, dtBoolean, TLiteDataConverters.MemoToBoolean, TLiteDataConverters.BooleanToMemo);
//  AddOnDemandConverter(dtMemo, dtSQLTimeStampOffset, TLiteDataConverters.MemoToSQLTimeStampOffset, TLiteDataConverters.SQLTimeStampOffsetToMemo);
end;

class function TLiteConverterManager.GetDBProvider: Word;
begin
  Result := liteBase;
end;

{$ENDIF}

class function TLiteConverterManager.GetDBType(const SQLTypeName: string): Word;
begin
  //
  if (SQLTypeName = 'NULL')
  then
    Result := liteNull
  else
  if (SQLTypeName = 'INT2') or
     AnsiStartsStr('SMALLINT', SQLTypeName) or
     AnsiStartsStr('SMALL INT', SQLTypeName)
  then
    Result := liteSmallint // Int16
  else
  if AnsiStartsStr('TINYINT', SQLTypeName) or
     AnsiStartsStr('TINY INT', SQLTypeName)
  then
    Result := liteTinyInt // Int8
  else
  if AnsiStartsStr('BIGINT', SQLTypeName) or
     AnsiStartsStr('BIG INT', SQLTypeName)
  then
    Result := liteBigInt // Int64
  else
  if AnsiStartsStr('UNSIGNED BIGINT', SQLTypeName) or
     AnsiStartsStr('UNSIGNED BIG INT', SQLTypeName) or
     (SQLTypeName = 'INT8') or
     (SQLTypeName = 'INT64')
  then
    Result := liteUBigInt // Int64
  else
  if (SQLTypeName = 'INTEGER')
  then
    Result := liteInteger // native INTEGER
  else
  if (SQLTypeName = 'INT')
  then
    Result := liteInt   // Int32
  else
  if AnsiStartsStr('MEDIUMINT', SQLTypeName) or
     AnsiStartsStr('MEDIUM INT', SQLTypeName)
  then
    Result := liteMediumInt // Int32
  else
  // other integers
  if Pos('INT', SQLTypeName) > 0
  then
    Result := liteInt       // Int32
  else
  if (Pos('VARCHAR', SQLTypeName) > 0) then
    Result := liteVarChar
  else
  if (Pos('CHAR', SQLTypeName) > 0) then
    Result := liteChar
  else
  if SQLTypeName = 'TEXT'
  then
    Result := liteText
  else
  if AnsiStartsStr('CLOB', SQLTypeName)
  then
    Result := liteClob
  else
  if SQLTypeName = 'BLOB'
  then
    Result := liteBlob
  else
  if AnsiStartsStr('FLOAT', SQLTypeName)
  then
    Result := liteFloat
  else
  if SQLTypeName = 'REAL'
  then
    Result := liteReal
  else
  if AnsiStartsStr('DOUBLE', SQLTypeName)
  then
    Result := liteDouble
  else
  if SQLTypeName = 'NUMERIC' then
    Result := liteNumeric
  else
  if SQLTypeName = 'DECIMAL' then
    Result := liteDecimal
  else
  if SQLTypeName = 'NUMBER' then
    Result := liteNumber
  else
  if (SQLTypeName = 'MONEY')
  or (SQLTypeName = 'CURRENCY')
  then
    Result := liteMoney
  else
  if AnsiStartsStr('BOOL', SQLTypeName)
  then
    Result := liteBool
  else
  if SQLTypeName = 'BINARY'
  then
    Result := liteBinary
  else
  if SQLTypeName = 'VARBINARY'
  then
    Result := liteVarBinary
  else
  if SQLTypeName = 'DATE'
  then
    Result := liteDate
  else
  if SQLTypeName = 'TIME'
  then
    Result := liteTime
  else
  if (SQLTypeName = 'DATETIME')
  then
    Result := liteDateTime
  else
  if (SQLTypeName = 'TIMESTAMP')
  then
    Result := liteTimestamp
  else
  if (SQLTypeName = 'TIMESTAMPTZ')
  then
    Result := liteTimestampTZ
  else
  if (SQLTypeName = 'BIT')
  then
    Result := liteBit
  else
    Result := liteNull;
end;

class function TLiteConverterManager.GetDBType(SQLType: integer): Word;
begin
  Result := liteNull;

  case SQLType of
    SQLITE_INTEGER:
      Result := liteInteger;
    SQLITE_FLOAT:
      Result := liteFloat;
    SQLITE_TEXT:
      Result := liteText;
    SQLITE_BLOB:
      Result := liteBlob;
    SQLITE_NULL:
      Result := liteNull;
  else
    Assert(False);
  end;
end;

{$IFNDEF LITE}

{ TLiteMapRules }

class function TLiteMapRules.GetConverterManager: TConverterManager;
begin
  Result := LiteConverterManager;
end;

{ TLiteDataConverters }

class function TLiteDataConverters.IntToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
begin
  dt := UnixToDateTime(Integer(Marshal.ReadInt64(AConvertInfo.Source)));
  Marshal.WriteInt64(AConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(dt))));
  Result := csSuccess;
end;

class function TLiteDataConverters.Int64ToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
begin
  dt := UnixToDateTime(Marshal.ReadInt64(AConvertInfo.Source));
  Marshal.WriteInt64(AConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(dt))));
  Result := csSuccess;
end;

class function TLiteDataConverters.RealToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
begin
  dt := JulianDateToDateTime(BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AConvertInfo.Source)));
  Marshal.WriteInt64(AConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TimeStampToMSecs(DateTimeToTimeStamp(dt))));
  Result := csSuccess;
end;

class function TLiteDataConverters.DateTimeToInt(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
{$IFDEF FPC}
  d: Int64;
{$ELSE}
  d: Double;
{$ENDIF}
begin
{$IFDEF FPC}
  d := Marshal.ReadInt64(AConvertInfo.Source);
{$ELSE}
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AConvertInfo.Source));
{$ENDIF}
  dt := TimeStampToDateTime(MSecsToTimeStamp(d));
  Marshal.WriteInt64(AConvertInfo.Dest, DateTimeToUnix(dt));
  Result := csSuccess;
end;

class function TLiteDataConverters.DateTimeToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
{$IFDEF FPC}
  d: Int64;
{$ELSE}
  d: Double;
{$ENDIF}
begin
{$IFDEF FPC}
  d := Marshal.ReadInt64(AConvertInfo.Source);
{$ELSE}
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AConvertInfo.Source));
{$ENDIF}
  dt := TimeStampToDateTime(MSecsToTimeStamp(d));
  Marshal.WriteInt64(AConvertInfo.Dest, DateTimeToUnix(dt));
  Result := csSuccess;
end;

class function TLiteDataConverters.DateTimeToReal(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  dt: TDateTime;
{$IFDEF FPC}
  d: Int64;
{$ELSE}
  d: Double;
{$ENDIF}
begin
{$IFDEF FPC}
  d := Marshal.ReadInt64(AConvertInfo.Source);
{$ELSE}
  d := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AConvertInfo.Source));
{$ENDIF}
  dt := TimeStampToDateTime(MSecsToTimeStamp(d));
  Marshal.WriteInt64(AConvertInfo.Dest, BitConverter.DoubleToInt64Bits(DateTimeToJulianDate(dt)));
  Result := csSuccess;
end;

class function TLiteDataConverters.MemoToInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToInt8(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToUInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToUInt8(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToInt16(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToUInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToUInt16(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToInt32(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToUInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToUInt32(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToInt64(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

{$IFDEF USE_UINT64}
class function TLiteDataConverters.MemoToUInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToUInt64(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;
{$ENDIF}

class function TLiteDataConverters.MemoToSingle(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  Str: string;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  Str := StringReplace(string(AStr), '.', {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
  if Str <> '' then
    Result := InternalStrToSingle(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToFloat(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  Str: string;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  Str := StringReplace(string(AStr), '.', {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
  if Str <> '' then
    Result := InternalStrToFloat(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToExtended(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  Str: string;
  l: integer;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  Str := StringReplace(string(AStr), '.', {$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
  if Str <> '' then
    Result := InternalStrToExtended(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToDate(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
  fmt: string;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then begin
    fmt := AConvertInfo.Format;
    if fmt = '' then
      fmt := 'yyyy-mm-dd';
    Result := InternalStrToDate(string(AStr), fmt, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
  end
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToTime(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
  fmt: string;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
 if AStr <> '' then begin
    fmt := AConvertInfo.Format;
    if fmt = '' then
      fmt := 'hh:nn:ss';
    Result := InternalStrToTime(string(AStr), fmt, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
  end
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToDateTime(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
  fmt: string;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
 if AStr <> '' then begin
    fmt := AConvertInfo.Format;
    if fmt = '' then
      fmt := 'yyyy-mm-dd hh:nn:ss';
    Result := InternalStrToDateTime(string(AStr), fmt, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
  end
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToSQLTimeStamp(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
  fmt: string;
begin
  l := 255;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
 if AStr <> '' then begin
    fmt := AConvertInfo.Format;
    if fmt = '' then
      fmt := 'yyyy-mm-dd hh:nn:ss';
    Result := InternalStrToSQLTimeStamp(string(AStr), fmt, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
  end
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.MemoToBoolean(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  AStr: AnsiString;
  l: integer;
begin
  l := 5;
  InternalMemoToAStr(AConvertInfo.Source, AConvertInfo.SourceOffset, AConvertInfo.SourceLen, 0, l, AStr);
  if AStr <> '' then
    Result := InternalStrToBool(string(AStr), AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors)
  else
    Result := csSuccess;
end;

class function TLiteDataConverters.Int8ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalInt8ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.UInt8ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalUInt8ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.Int16ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalInt16ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.UInt16ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalUInt16ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.Int32ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalInt32ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.UInt32ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalUInt32ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.Int64ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalInt64ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

{$IFDEF USE_UINT64}
class function TLiteDataConverters.UInt64ToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalUInt64ToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;
{$ENDIF}

class function TLiteDataConverters.SingleToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  InternalSingleToStr(AConvertInfo.Source, Str, AConvertInfo.DestLen);
  Result := InternalAStrToMemo(AnsiString(Str), 0, AConvertInfo.DestLen, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.FloatToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  InternalFloatToStr(AConvertInfo.Source, Str, AConvertInfo.DestLen);
  Result := InternalAStrToMemo(AnsiString(Str), 0, AConvertInfo.DestLen, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.ExtendedToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
begin
  InternalExtendedToStr(AConvertInfo.Source, Str, AConvertInfo.DestLen);
  Result := InternalAStrToMemo(AnsiString(Str), 0, AConvertInfo.DestLen, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.DateToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  InternalDateToStr(AConvertInfo.Source, Str, CheckDateTimeFormat(AConvertInfo.Format, AConvertInfo.DestLen));
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.TimeToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  InternalTimeToStr(AConvertInfo.Source, 0, Str, CheckDateTimeFormat(AConvertInfo.Format, AConvertInfo.DestLen));
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.DateTimeToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  InternalDateTimeToStr(AConvertInfo.Source, 0, Str, CheckDateTimeFormat(AConvertInfo.Format, AConvertInfo.DestLen));
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.SQLTimeStampToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  InternalSQLTimeStampToStr(AConvertInfo.Source, 0, Str, CheckDateTimeFormat(AConvertInfo.Format, AConvertInfo.DestLen));
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TLiteDataConverters.BooleanToMemo(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  l: integer;
begin
  Str := InternalBoolToStr(AConvertInfo.Source);
  l := Length(Str);
  Result := InternalAStrToMemo(AnsiString(Str), 0, l, AConvertInfo.Dest, AConvertInfo.DestOffset, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

initialization
  InitLiteTypes;
  LiteConverterManager := TLiteConverterManager.Create;

finalization
  LiteConverterManager.Free;

{$ENDIF}

end.
