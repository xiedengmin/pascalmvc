
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraDataTypeMapUni;

interface

uses
{$IFNDEF LITE}
  SysUtils, FMTBcd,
  CLRClasses, CRDataTypeMap, CRTypes,
{$ENDIF}
  MemUtils, MemData;

const
  oraBase                       = 100;

  oraChar                       = oraBase + 1;
  oraVarchar2                   = oraBase + 2;
  oraNChar                      = oraBase + 3;
  oraNVarchar2                  = oraBase + 4;
  oraNumber                     = oraBase + 5;
  oraInteger                    = oraBase + 6;
  oraFloat                      = oraBase + 7;
  oraBinaryFloat                = oraBase + 8;
  oraBinaryDouble               = oraBase + 9;
  oraDoublePrecision            = oraBase + 10;
  oraDate                       = oraBase + 11;
  oraTimeStamp                  = oraBase + 12;
  oraTimeStampWithTimeZone      = oraBase + 13;
  oraTimeStampWithLocalTimeZone = oraBase + 14;
  oraIntervalYM                 = oraBase + 15;
  oraIntervalDS                 = oraBase + 16;
  oraBlob                       = oraBase + 17;
  oraClob                       = oraBase + 18;
  oraNClob                      = oraBase + 19;
  oraBFile                      = oraBase + 20;
  oraCFile                      = oraBase + 21;
  oraLong                       = oraBase + 22;
  oraRaw                        = oraBase + 23;
  oraLongRaw                    = oraBase + 24;
  oraRowID                      = oraBase + 25;
  oraURowID                     = oraBase + 26;
  oraCursor                     = oraBase + 27;
  oraObject                     = oraBase + 28;
  oraReference                  = oraBase + 29;
  oraXML                        = oraBase + 30;
  oraAnyData                    = oraBase + 31;
  oraLabel                      = oraBase + 32;
  oraUndefined                  = oraBase + 33;

type

  TOraConverterManager = class{$IFNDEF LITE}(TConverterManager){$ENDIF}
  public
  {$IFNDEF LITE}
    constructor Create;

    class function GetDBProvider: Word; override;
  {$ENDIF}
    class function GetDBType(SQLType: Word; ObjectDataType: Word; IsNational: boolean = False): Word;
  end;

{$IFNDEF LITE}

  TOraMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TOraDataConverters = class(TDataConverters)
  protected
    class function InternalNumberToStr(Source: IntPtr;  out Str: String; DestLen: Integer): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function InternalStrToNumber(var Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus; {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    class function NumberToInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToUInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToUInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToUInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToFloat(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToSingle(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToBCD(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToFMTBCD(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToExtAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function NumberToExtWStr(var AConvertInfo: TConvertInfo): TConvertStatus;

    class function Int8ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt8ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int16ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt16ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int32ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function UInt32ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function Int64ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function FloatToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function SingleToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function BCDToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function FMTBCDToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtAStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function ExtWStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function XmlToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function XmlToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToXml(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToXml(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function IntervalToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function IntervalToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function AStrToInterval(var AConvertInfo: TConvertInfo): TConvertStatus;
    class function WStrToInterval(var AConvertInfo: TConvertInfo): TConvertStatus;
  end;

  procedure InitOraTypes;

{$ENDIF}

implementation

uses
  Classes, CRFunctions,
{$IFNDEF UNIDACPRO}
  {$IFNDEF LITE}OraObjects,{$ENDIF} OraCall, OraClasses;
{$ELSE}
  {$IFNDEF LITE}OraObjectsUni,{$ENDIF} OraCallUni, OraClassesUni;
{$ENDIF}

const
  oraVarchar                    = oraBase + 51;
  oraDecimal                    = oraBase + 53;

{$IFNDEF LITE}

var
  OraConverterManager: TOraConverterManager;

{ Functions }

procedure InitOraTypes;
begin
  DBTypeInfos.Add(oraChar,                       'Char',                            True,  False);
  DBTypeInfos.Add(oraVarchar2,                   'Varchar2',                        True,  False);
  DBTypeInfos.Add(oraNChar,                      'NChar',                           True,  False);
  DBTypeInfos.Add(oraNVarchar2,                  'NVarchar2',                       True,  False);
  DBTypeInfos.Add(oraNumber,                     'Number',                          True,  True);
  DBTypeInfos.Add(oraInteger,                    'Integer',                         False, False);
  DBTypeInfos.Add(oraFloat,                      'Float',                           False, False);
  DBTypeInfos.Add(oraBinaryFloat,                'BinaryFloat',                     False, False);
  DBTypeInfos.Add(oraBinaryDouble,               'BinaryDouble',                    False, False);
  DBTypeInfos.Add(oraDate,                       'Date',                            False, False);
  DBTypeInfos.Add(oraTimeStamp,                  'TimeStamp',                       False, False);
  DBTypeInfos.Add(oraTimeStampWithTimeZone,      'Time Stamp With Time Zone',       False, False);
  DBTypeInfos.Add(oraTimeStampWithLocalTimeZone, 'Time Stamp With Local Time Zone', False, False);
  DBTypeInfos.Add(oraBlob,                       'Blob',                            False, False);
  DBTypeInfos.Add(oraClob,                       'Clob',                            False, False);
  DBTypeInfos.Add(oraNClob,                      'NClob',                           False, False);
  DBTypeInfos.Add(oraBFile,                      'BFile',                           False, False);
  DBTypeInfos.Add(oraCFile,                      'CFile',                           False, False);
  DBTypeInfos.Add(oraLong,                       'Long',                            False, False);
  DBTypeInfos.Add(oraRaw,                        'Raw',                             True,  False);
  DBTypeInfos.Add(oraLongRaw,                    'LongRaw',                         False, False);
  DBTypeInfos.Add(oraRowID,                      'RowID',                           False, False);
  DBTypeInfos.Add(oraURowID,                     'URowID',                          False, False);
  DBTypeInfos.Add(oraXML,                        'XML',                             False, False);
  DBTypeInfos.Add(oraIntervalYM,                 'IntervalYM',                      False, False);
  DBTypeInfos.Add(oraIntervalDS,                 'IntervalDS',                      False, False);
  DBTypeInfos.Add(oraCursor,                     'Cursor',                          False, False);
  DBTypeInfos.Add(oraObject,                     'Object',                          False, False);
  DBTypeInfos.Add(oraReference,                  'Reference',                       False, False);
  DBTypeInfos.Add(oraLabel,                      'Label',                           False, False);
  DBTypeInfos.Add(oraUndefined,                  'Undefined',                       False, False);
  DBTypeInfos.Add(oraDecimal,                    'Decimal',                         False, False);
  DBTypeInfos.Add(oraAnyData,                    'AnyData',                         False, False);
end;

{ TOraDataMapRules }

class function TOraMapRules.GetConverterManager: TConverterManager;
begin
  Result := OraConverterManager;
end;

{ TOraConverterManager }

constructor TOraConverterManager.Create;
begin
  inherited;

  AddFetchConverter(oraChar, dtString);
  AddFetchConverter(oraChar, dtFixedChar, dtString);
  AddFetchConverter(oraChar, dtWideString);
  AddFetchConverter(oraChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(oraVarchar2, dtString);
  AddFetchConverter(oraVarchar2, dtWideString);

  AddFetchConverter(oraNChar, dtString);
  AddFetchConverter(oraNChar, dtFixedChar, dtString);
  AddFetchConverter(oraNChar, dtFixedNChar, dtString);
  AddFetchConverter(oraNChar, dtWideString);
  AddFetchConverter(oraNChar, dtFixedWideChar, dtWideString);
  AddFetchConverter(oraNChar, dtFixedNWideChar, dtWideString);

  AddFetchConverter(oraNVarchar2, dtString);
  AddFetchConverter(oraNVarchar2, dtWideString);

  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtBoolean,  dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtInt8,     dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtUInt8,    dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtSmallint, dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtWord,     dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtInteger,  dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtLongWord, dtSmallint);
  AddFetchConverter(oraNumber,  1,  4, 0, 0, dtLargeint, dtSmallint);

  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtBoolean,  dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtInt8,     dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtUInt8,    dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtSmallint, dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtWord,     dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtInteger,  dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtLongWord, dtInteger);
  AddFetchConverter(oraNumber,  5,  9, 0, 0, dtLargeint, dtInteger);

  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtBoolean,  dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtInt8,     dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtUInt8,    dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtSmallint, dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtWord,     dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtInteger,  dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtLongWord, dtLargeint);
  AddFetchConverter(oraNumber, 10, -1, 0, 0, dtLargeint, dtLargeint);

  AddFetchConverter(oraNumber, dtFloat);
  AddFetchConverter(oraNumber, dtCurrency, dtFloat);
  AddFetchConverter(oraNumber, dtSingle, dtFloat);
  AddFetchConverter(oraNumber, -1, 14, -1, 4, dtBCD);
  AddFetchConverter(oraNumber, dtBCD, dtFMTBCD);
  AddFetchConverter(oraNumber, dtFMTBCD);
  AddFetchConverter(oraNumber, dtNumber);

  AddFetchConverter(oraBinaryFloat, dtFloat);
  AddFetchConverter(oraBinaryFloat, dtSingle);

  AddFetchConverter(oraFloat, dtNumber);
  AddFetchConverter(oraBinaryFloat, dtNumber);
  AddFetchConverter(oraBinaryDouble, dtNumber);


  AddFetchConverter(oraTimeStamp, dtTime, dtDateTime);
  AddFetchConverter(oraTimeStamp, dtDate, dtDateTime);
  AddFetchConverter(oraTimeStamp, dtDateTime);
  AddFetchConverter(oraTimeStamp, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtTimeStamp, dtTimeStamp);
  AddFetchConverter(oraTimeStamp, dtTimeStampTZ, dtTimeStamp);
  AddFetchConverter(oraTimeStamp, dtTimeStampLTZ, dtTimeStamp);

  // for convert oraTimeStamp to string or bytes store data as SQLTimeStamp
  AddFetchConverter(oraTimeStamp, dtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtExtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtExtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtVarBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStamp, dtExtVarBytes, dtSQLTimeStamp);

  AddFetchConverter(oraTimeStampWithTimeZone, dtTime, dtDateTime);
  AddFetchConverter(oraTimeStampWithTimeZone, dtDate, dtDateTime);
  AddFetchConverter(oraTimeStampWithTimeZone, dtDateTime);
  AddFetchConverter(oraTimeStampWithTimeZone, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtTimeStamp, dtTimeStampTZ);
  AddFetchConverter(oraTimeStampWithTimeZone, dtTimeStampTZ, dtTimeStampTZ);
  AddFetchConverter(oraTimeStampWithTimeZone, dtTimeStampLTZ, dtTimeStampTZ);

  // for convert oraTimeStampWithTimeZone to string or bytes store data as SQLTimeStamp
  AddFetchConverter(oraTimeStampWithTimeZone, dtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtExtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtExtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtVarBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithTimeZone, dtExtVarBytes, dtSQLTimeStamp);

  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtTime, dtDateTime);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtDate, dtDateTime);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtDateTime);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtTimeStamp, dtTimeStampLTZ);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtTimeStampTZ, dtTimeStampLTZ);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtTimeStampLTZ, dtTimeStampLTZ);

  // for convert oraTimeStampWithLocalTimeZone to string or bytes store data as SQLTimeStamp
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtExtString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtExtWideString, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtVarBytes, dtSQLTimeStamp);
  AddFetchConverter(oraTimeStampWithLocalTimeZone, dtExtVarBytes, dtSQLTimeStamp);

  // ODBC need both Fetch + Deman convertors for Interval types
  AddFetchConverter(oraIntervalYM, dtString);
  AddFetchConverter(oraIntervalYM, dtExtString);
  AddFetchConverter(oraIntervalYM, dtWideString);
  AddFetchConverter(oraIntervalYM, dtExtWideString);

  // ODBC need both Fetch + Deman convertors for Interval types
  AddFetchConverter(oraIntervalDS, dtString);
  AddFetchConverter(oraIntervalDS, dtExtString);
  AddFetchConverter(oraIntervalDS, dtWideString);
  AddFetchConverter(oraIntervalDS, dtExtWideString);

  AddFetchConverter(oraBlob, dtBlob, dtOraBlob);

  AddFetchConverter(oraClob, dtOraClob);
  AddFetchConverter(oraClob, dtMemo, dtOraClob);
  AddFetchConverter(oraClob, dtWideOraClob);
  AddFetchConverter(oraClob, dtWideMemo, dtWideOraClob);

  AddFetchConverter(oraNClob, dtOraClob);
  AddFetchConverter(oraNClob, dtMemo, dtOraClob);
  AddFetchConverter(oraNClob, dtWideOraClob);
  AddFetchConverter(oraNClob, dtWideMemo, dtWideOraClob);

  AddFetchConverter(oraLong, dtMemo);
  AddFetchConverter(oraLong, dtWideMemo);

  CloneOnDemandConverters(dtBlob, dtOraBlob);
  AddOnDemandConverter(dtOraBlob, dtBlob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtBlob, dtOraBlob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  CloneOnDemandConverters(dtBlob, dtBFILE);
  AddOnDemandConverter(dtBFILE, dtBlob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtBlob, dtBFILE, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  CloneOnDemandConverters(dtMemo, dtOraClob);
  AddOnDemandConverter(dtOraClob, dtMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtMemo, dtOraClob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  CloneOnDemandConverters(dtWideMemo, dtWideOraClob);
  AddOnDemandConverter(dtWideOraClob, dtWideMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtWideMemo, dtWideOraClob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  CloneOnDemandConverters(dtMemo, dtNClob);
  AddOnDemandConverter(dtNClob, dtMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtMemo, dtNClob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  AddOnDemandConverter(dtNumber, dtInt8, TOraDataConverters.NumberToInt8, TOraDataConverters.Int8ToNumber);
  AddOnDemandConverter(dtNumber, dtUInt8, TOraDataConverters.NumberToUInt8, TOraDataConverters.UInt8ToNumber);
  AddOnDemandConverter(dtNumber, dtSmallint, TOraDataConverters.NumberToInt16, TOraDataConverters.Int16ToNumber);
  AddOnDemandConverter(dtNumber, dtWord, TOraDataConverters.NumberToUInt16, TOraDataConverters.UInt16ToNumber);
  AddOnDemandConverter(dtNumber, dtInteger, TOraDataConverters.NumberToInt32, TOraDataConverters.Int32ToNumber);
  AddOnDemandConverter(dtNumber, dtUInt32, TOraDataConverters.NumberToUInt32, TOraDataConverters.UInt32ToNumber);
  AddOnDemandConverter(dtNumber, dtInt64, TOraDataConverters.NumberToInt64, TOraDataConverters.Int64ToNumber);
  AddOnDemandConverter(dtNumber, dtFloat, TOraDataConverters.NumberToFloat, TOraDataConverters.FloatToNumber);
  AddOnDemandConverter(dtNumber, dtSingle, TOraDataConverters.NumberToSingle, TOraDataConverters.SingleToNumber);
  AddOnDemandConverter(dtNumber, dtCurrency, TOraDataConverters.NumberToFloat, TOraDataConverters.FloatToNumber);
  AddOnDemandConverter(dtNumber, dtBCD, TOraDataConverters.NumberToBCD, TOraDataConverters.BCDToNumber);
  AddOnDemandConverter(dtNumber, dtFMTBCD, TOraDataConverters.NumberToFMTBCD, TOraDataConverters.FMTBCDToNumber);
  AddOnDemandConverter(dtNumber, dtString, TOraDataConverters.NumberToAStr, TOraDataConverters.AStrToNumber);
  AddOnDemandConverter(dtNumber, dtExtString, TOraDataConverters.NumberToExtAStr, TOraDataConverters.ExtAStrToNumber);
  AddOnDemandConverter(dtNumber, dtWideString, TOraDataConverters.NumberToWStr, TOraDataConverters.WStrToNumber);
  AddOnDemandConverter(dtNumber, dtExtWideString, TOraDataConverters.NumberToExtWStr, TOraDataConverters.ExtWStrToNumber);

  AddOnDemandConverter(dtXML,    dtString, TOraDataConverters.XmlToAStr, TOraDataConverters.AStrToXml);
  AddOnDemandConverter(dtXML,    dtWideString, TOraDataConverters.XmlToWStr, TOraDataConverters.WStrToXml);

  // ODBC need both Fetch + Deman convertors for Interval types
  AddOnDemandConverter(dtIntervalYM, dtString, TOraDataConverters.IntervalToAStr, TOraDataConverters.AStrToInterval);
  AddOnDemandConverter(dtIntervalYM, dtWideString, TOraDataConverters.IntervalToWStr, TOraDataConverters.WStrToInterval);
  AddOnDemandConverter(dtIntervalDS, dtString, TOraDataConverters.IntervalToAStr, TOraDataConverters.AStrToInterval);
  AddOnDemandConverter(dtIntervalDS, dtWideString, TOraDataConverters.IntervalToWStr, TOraDataConverters.WStrToInterval);
end;

class function TOraConverterManager.GetDBProvider: Word;
begin
  Result := oraBase;
end;

{$ENDIF}

class function TOraConverterManager.GetDBType(SQLType: Word; ObjectDataType: Word; IsNational: boolean = False): Word;
begin
  case SQLType of
    SQLT_AFC:
      if IsNational then
        Result := oraNChar
      else
        Result := oraChar;
    SQLT_AVC, SQLT_CHR:
      if IsNational then
        Result := oraNVarchar2
      else
        Result := oraVarchar2;
    SQLT_INT:
      Result := oraInteger;
    SQLT_FLT:
      Result := oraFloat;
    SQLT_NUM:
      Result := oraNumber;
    SQLT_PDN:
      Result := oraDecimal;
    SQLT_IBFLOAT:
      Result := oraBinaryFloat;
    SQLT_IBDOUBLE:
      Result := oraBinaryDouble;
    SQLT_DAT:
      Result := oraDate;
    SQLT_TIMESTAMP:
      Result := oraTimeStamp;
    SQLT_TIMESTAMP_TZ:
      Result := oraTimeStampWithTimeZone;
    SQLT_TIMESTAMP_LTZ:
      Result := oraTimeStampWithLocalTimeZone;
    SQLT_INTERVAL_YM:
      Result := oraIntervalYM;
    SQLT_INTERVAL_DS:
      Result := oraIntervalDS;
    SQLT_BLOB:
      Result := oraBlob;
    SQLT_CLOB:
      if IsNational then
        Result := oraNClob
      else
        Result := oraClob;
    SQLT_BFILEE:
      Result := oraBFile;
    SQLT_CFILEE:
      Result := oraCFile;
    SQLT_LNG:
      Result := oraLong;
    SQLT_BIN:
      Result := oraRaw;
    SQLT_LBI:
      Result := oraLongRaw;
    SQLT_RID:
      Result := oraRowID;
    SQLT_RDD:
      Result := oraURowID;
    SQLT_CUR, SQLT_RSET:
      Result := oraCursor;
    SQLT_NTY:
      if ObjectDataType = dtXML then
        Result := oraXML
      else if ObjectDataType = dtAnyData then
        Result := oraAnyData
      else
        Result := oraObject;
    SQLT_REF:
      Result := oraReference;
    SQLT_OSL:
      Result := oraLabel;
    SQLT_UND:
      Result := oraUndefined;
    else
      Result := 0;
  end;
end;

{$IFNDEF LITE}

{ TOraDataConverters }

class function TOraDataConverters.InternalNumberToStr(Source: IntPtr;  out Str: String; DestLen: Integer): TConvertStatus;
begin
  Str := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(Source))).AsString;
    Result := CheckNumericStr(Str, DestLen);
end;

class function TOraDataConverters.InternalStrToNumber(var Str: String; Dest: IntPtr; IgnoreConvertErrors: Boolean): TConvertStatus;
var
  Number: TOraNumber;
begin
  Number := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(Dest)));
  try

    Number.AsString := Str;
    Result := csSuccess;
  except
    Result := csInvalidNumericValue;
    if IgnoreConvertErrors then
      Number.AsInteger := 0;
  end;
end;

class function TOraDataConverters.NumberToInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsInteger;
  Result := InternalInt32ToInt8(i, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToUInt8(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsInteger;
  Result := InternalInt32ToUInt8(i, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsInteger;
  Result := InternalInt32ToInt16(i, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToUInt16(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsInteger;
  Result := InternalInt32ToUInt16(i, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i: integer;
begin
  i := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsInteger;
  Marshal.WriteInt32(AConvertInfo.Dest, i);
  Result := csSuccess;
end;

class function TOraDataConverters.NumberToUInt32(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsLargeInt;
  Result := InternalInt64ToUInt32(i64, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToInt64(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  i64: Int64;
begin
  i64 := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsLargeInt;
  Marshal.WriteInt64(AConvertInfo.Dest, i64);
  Result := csSuccess;
end;

class function TOraDataConverters.NumberToFloat(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(AConvertInfo.Dest, BitConverter.DoubleToInt64Bits(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsFloat));
  Result := csSuccess;
end;

class function TOraDataConverters.NumberToSingle(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  Marshal.WriteInt64(AConvertInfo.Dest, CRBitConverter.SingleToInt32Bits(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsFloat));
  Result := csSuccess;
end;

class function TOraDataConverters.NumberToBCD(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  d: Double;
  r: TConvertStatus;
begin
  d := TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsFloat * 10000;

  // Cast check
  if System.Frac(d) <> 0 then begin
    Result := csFractionTruncated;
    if not AConvertInfo.IgnoreConvertErrors then
      Exit
  end
  else
    Result := csSuccess;

  r := WriteInt64AsBCD(Trunc(d), AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.DestScale, AConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TOraDataConverters.NumberToFMTBCD(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  Result := InternalBCDToFMTBCD(TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsBCD, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.DestScale, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.NumberToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalNumberToStr(AConvertInfo.Source,  Str, AConvertInfo.DestLen);
  r := InternalWriteAStr(AnsiString(Str), AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TOraDataConverters.NumberToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalNumberToStr(AConvertInfo.Source,  Str, AConvertInfo.DestLen);
  r := InternalWriteWStr(WideString(Str), AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TOraDataConverters.NumberToExtAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalNumberToStr(AConvertInfo.Source,  Str, AConvertInfo.DestLen);
  r := InternalWriteExtAStr(AConvertInfo.StringHeap, AnsiString(Str), AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TOraDataConverters.NumberToExtWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: String;
  r: TConvertStatus;
begin
  Result := InternalNumberToStr(AConvertInfo.Source,  Str, AConvertInfo.DestLen);
  r := InternalWriteExtWStr(AConvertInfo.StringHeap, WideString(Str), AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
  if Result = csSuccess then
    Result := r;
end;

class function TOraDataConverters.Int8ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsInteger := ShortInt(Marshal.ReadByte(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.UInt8ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsInteger := Marshal.ReadByte(AConvertInfo.Source);
  Result := csSuccess;
end;

class function TOraDataConverters.Int16ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsInteger := Marshal.ReadInt16(AConvertInfo.Source);
  Result := csSuccess;
end;

class function TOraDataConverters.UInt16ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsInteger := Word(Marshal.ReadInt16(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.Int32ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsInteger := Marshal.ReadInt32(AConvertInfo.Source);
  Result := csSuccess;
end;

class function TOraDataConverters.UInt32ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsLargeInt := Cardinal(Marshal.ReadInt32(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.Int64ToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsLargeInt := Marshal.ReadInt64(AConvertInfo.Source);
  Result := csSuccess;
end;

class function TOraDataConverters.FloatToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsFloat := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.SingleToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsFloat := CRBitConverter.Int32BitsToSingle(Marshal.ReadInt32(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.BCDToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
begin
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsFloat := Marshal.ReadInt64(AConvertInfo.Source) / 10000;
  Result := csSuccess;
end;

class function TOraDataConverters.FMTBCDToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  bcd: TBcd;
begin
  bcd := PBcd(AConvertInfo.Source)^;
  TOraNumber(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest))).AsBCD := bcd;
  Result := csSuccess;
end;

class function TOraDataConverters.AStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(AConvertInfo.Source, AConvertInfo.SourceLen));
  Result := InternalStrToNumber(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.WStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(AConvertInfo.Source, AConvertInfo.SourceLen));
  Result := InternalStrToNumber(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.ExtAStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringAnsi(Marshal.ReadIntPtr(AConvertInfo.Source), AConvertInfo.SourceLen));
  Result := InternalStrToNumber(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.ExtWStrToNumber(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: string;
begin
  Str := string(Marshal.PtrToStringUni(Marshal.ReadIntPtr(AConvertInfo.Source), AConvertInfo.SourceLen));
  Result := InternalStrToNumber(Str, AConvertInfo.Dest, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.XmlToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: AnsiString;
begin
  Str := AnsiString(TOraXML(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsString);
  Result := InternalWriteAStr(Str, AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.XmlToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: WideString;
begin
  Str := WideString(TOraXML(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsString);
  Result := InternalWriteWStr(Str, AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.AStrToXml(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  XML: TOraXML;
begin
  XML := TOraXML(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest)));
  XML.AsString := string(Marshal.PtrToStringAnsi(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.WStrToXml(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  XML: TOraXML;
begin
  XML := TOraXML(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest)));
  XML.AsString := string(Marshal.PtrToStringUni(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.IntervalToAStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: AnsiString;
begin
  Str := AnsiString(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsString);
  Result := InternalWriteAStr(Str, AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.IntervalToWStr(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Str: WideString;
begin
  Str := WideString(TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Source))).AsString);
  Result := InternalWriteWStr(Str, AConvertInfo.SourceLen, AConvertInfo.Dest, AConvertInfo.DestLen, AConvertInfo.IgnoreConvertErrors);
end;

class function TOraDataConverters.AStrToInterval(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Interval: TOraInterval;
begin
  Interval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest)));
  Interval.AsString := string(Marshal.PtrToStringAnsi(AConvertInfo.Source));
  Result := csSuccess;
end;

class function TOraDataConverters.WStrToInterval(var AConvertInfo: TConvertInfo): TConvertStatus;
var
  Interval: TOraInterval;
begin
  Interval := TOraInterval(GetGCHandleTarget(Marshal.ReadIntPtr(AConvertInfo.Dest)));
  Interval.AsString := string(Marshal.PtrToStringUni(AConvertInfo.Source));
  Result := csSuccess;
end;

initialization
  InitOraTypes;
  OraConverterManager := TOraConverterManager.Create;

finalization
  OraConverterManager.Free;

{$ENDIF}

end.
