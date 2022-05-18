
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I Sdac.inc}
unit MSDataTypeMapUni;

interface

uses
  CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  OLEDBC;
{$ELSE}
  OLEDBCUni;
{$ENDIF}

const
  msBase            = 300;

  msBit             = msBase + 1;
  msTinyint         = msBase + 2;
  msSmallint        = msBase + 3;
  msInt             = msBase + 4;
  msBigint          = msBase + 5;
  msDecimal         = msBase + 6;
  msNumeric         = msDecimal;
  msSmallmoney      = msBase + 7;
  msMoney           = msBase + 8;
  msFloat           = msBase + 9;
  msReal            = msBase + 10;
  msDatetime        = msBase + 11;
  msSmalldatetime   = msBase + 12;
  msDate            = msBase + 13;
  msTime            = msBase + 14;
  msDatetime2       = msBase + 15;
  msDatetimeoffset  = msBase + 16;
  msChar            = msBase + 17;
  msVarchar         = msBase + 18;
  msText            = msBase + 19;
  msNChar           = msBase + 20;
  msNVarchar        = msBase + 21;
  msNText           = msBase + 22;
  msBinary          = msBase + 23;
  msVarbinary       = msBase + 24;
  msImage           = msBase + 25;
  msTimestamp       = msBase + 26;
  msUniqueIdentifier= msBase + 27;
  msSqlVariant      = msBase + 28;
  msXml             = msBase + 29;

type
  TMSMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TMSConverterManager = class(TConverterManager)
  protected
    function GetDateFormat: string; override;
    function GetTimeFormat: string; override;
    function GetDateTimeFormat: string; override;

  public
    constructor Create;

    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: Word;
      const Flags: DBCOLUMNFLAGS; const Length: integer): Word;
  end;

implementation

uses
  SysUtils, MemData;

var
  MSConverterManager: TMSConverterManager;

{ Functions }

procedure InitMSTypes;
begin
  DBTypeInfos.Add(msBit,             'Bit',              False, False);
  DBTypeInfos.Add(msTinyint,         'Tinyint',          False, False);
  DBTypeInfos.Add(msSmallint,        'Smallint',         False, False);
  DBTypeInfos.Add(msInt,             'Int',              False, False);
  DBTypeInfos.Add(msBigint,          'Bigint',           False, False);
  DBTypeInfos.Add(msDecimal,         'Decimal',          True,  True);
  DBTypeInfos.Add(msNumeric,         'Numeric',          True,  True);
  DBTypeInfos.Add(msSmallmoney,      'Smallmoney',       False, False);
  DBTypeInfos.Add(msMoney,           'Money',            False, False);
  DBTypeInfos.Add(msFloat,           'Float',            False, False);
  DBTypeInfos.Add(msReal,            'Real',             False, False);
  DBTypeInfos.Add(msDatetime,        'Datetime',         False, False);
  DBTypeInfos.Add(msSmalldatetime,   'Smalldatetime',    False, False);
  DBTypeInfos.Add(msDate,            'Date',             False, False);
  DBTypeInfos.Add(msTime,            'Time',             False, False);
  DBTypeInfos.Add(msDatetime2,       'Datetime2',        False, False);
  DBTypeInfos.Add(msDatetimeoffset,  'Datetimeoffset',   False, False);
  DBTypeInfos.Add(msChar,            'Char',             True,  False);
  DBTypeInfos.Add(msVarchar,         'Varchar',          True,  False);
  DBTypeInfos.Add(msText,            'Text',             False, False);
  DBTypeInfos.Add(msNChar,           'NChar',            True,  False);
  DBTypeInfos.Add(msNVarchar,        'NVarchar',         True,  False);
  DBTypeInfos.Add(msNText,           'NText',            False, False);
  DBTypeInfos.Add(msBinary,          'Binary',           True,  False);
  DBTypeInfos.Add(msVarbinary,       'Varbinary',        True,  False);
  DBTypeInfos.Add(msImage,           'Image',            False, False);
  DBTypeInfos.Add(msTimestamp,       'Timestamp',        False, False);
  DBTypeInfos.Add(msUniqueIdentifier,'UniqueIdentifier', False, False);
  DBTypeInfos.Add(msSqlVariant,      'SqlVariant',       False, False);
  DBTypeInfos.Add(msXml,             'Xml',              False, False);
end;

{ TMSMapRules }

class function TMSMapRules.GetConverterManager: TConverterManager;
begin
  Result := MSConverterManager;
end;

{ TMSConverterManager }

constructor TMSConverterManager.Create;
begin
  inherited;

  AddFetchConverter(msDecimal, dtFloat);
  AddFetchConverter(msDecimal, dtBCD, dtFloat);
  AddFetchConverter(msDecimal, dtFmtBCD);

  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtInt16,      dtFmtBCD);
  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtUInt16,     dtFmtBCD);
  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtInt32,      dtFmtBCD);
  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtUInt32,     dtFmtBCD);
  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtInt64,      dtFmtBCD);
  AddFetchConverter(msDecimal, -1, -1, 0, 0, dtUInt64,     dtFmtBCD);
  AddFetchConverter(msDecimal, dtString,     dtFmtBCD);
  AddFetchConverter(msDecimal, dtWideString, dtFmtBCD);

  AddFetchConverter(msTinyint, dtInt16,      dtUInt8);
  AddFetchConverter(msTinyint, dtUInt16,     dtUInt8);
  AddFetchConverter(msTinyint, dtInt32,      dtUInt8);
  AddFetchConverter(msTinyint, dtUInt32,     dtUInt8);
  AddFetchConverter(msTinyint, dtInt64,      dtUInt8);
  AddFetchConverter(msTinyint, dtUInt64,     dtUInt8);
  AddFetchConverter(msTinyint, dtString,     dtUInt8);
  AddFetchConverter(msTinyint, dtWideString, dtUInt8);

  AddFetchConverter(msDatetime2, dtSQLTimeStamp);
  AddFetchConverter(msDatetime2, dtString, dtSQLTimeStamp);
  AddFetchConverter(msDatetime2, dtWideString, dtSQLTimeStamp);

  AddFetchConverter(msChar, dtString);
  AddFetchConverter(msChar, dtFixedChar, dtString);
  AddFetchConverter(msChar, dtWideString);
  AddFetchConverter(msChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(msVarchar, dtString);
  AddFetchConverter(msVarchar, dtWideString);

  AddFetchConverter(msNChar, dtString);
  AddFetchConverter(msNChar, dtFixedChar, dtString);
  AddFetchConverter(msNChar, dtWideString);
  AddFetchConverter(msNChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(msNVarchar, dtString);
  AddFetchConverter(msNVarchar, dtWideString);

//  AddFetchConverter(msBinary, dtString);
//  AddFetchConverter(msBinary, dtWideString);

//  AddFetchConverter(msVarbinary, dtString);
//  AddFetchConverter(msVarbinary, dtWideString);

  AddFetchConverter(msText, dtMemo);
  AddFetchConverter(msText, dtWideMemo);
  AddFetchConverter(msText, dtString,     dtMemo);
  AddFetchConverter(msText, dtWideString, dtMemo);

  AddFetchConverter(msNText, dtMemo);
  AddFetchConverter(msNText, dtWideMemo);
  AddFetchConverter(msNText, dtString,     dtWideMemo);
  AddFetchConverter(msNText, dtWideString, dtWideMemo);

//  AddFetchConverter(msImage, dtMemo);
//  AddFetchConverter(msImage, dtWideMemo);
//  AddFetchConverter(msImage, dtString,     dtMemo);
//  AddFetchConverter(msImage, dtWideString, dtWideMemo);

  AddFetchConverter(msXml, dtMemo);
  AddFetchConverter(msXml, dtWideMemo);
  AddFetchConverter(msXml, dtString,     dtWideMemo);
  AddFetchConverter(msXml, dtWideString, dtWideMemo);

  AddOnDemandConverter(dtString, dtXML, TDataConverters.AStrToMemo, TDataConverters.MemoToAStr);
  AddOnDemandConverter(dtWideString, dtXML, TDataConverters.WStrToWideMemo, TDataConverters.WideMemoToWStr);
  AddOnDemandConverter(dtExtString, dtXML, TDataConverters.ExtAStrToWideMemo, TDataConverters.WideMemoToExtAStr);
  AddOnDemandConverter(dtExtWideString, dtXML, TDataConverters.ExtWStrToWideMemo, TDataConverters.WideMemoToExtWStr);
  AddOnDemandConverter(dtBytes, dtXML, TDataConverters.BytesToBlob, TDataConverters.BlobToBytes);
  AddOnDemandConverter(dtMemo, dtXML, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtWideMemo, dtXML, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtBlob, dtXML, TDataConverters.CopyPtr, TDataConverters.CopyPtr);

  AddOnDemandConverter(dtXML, dtString, TDataConverters.WideMemoToAStr, TDataConverters.AStrToWideMemo);
  AddOnDemandConverter(dtXML, dtWideString, TDataConverters.WideMemoToWStr, TDataConverters.WStrToWideMemo);
  AddOnDemandConverter(dtXML, dtExtString, TDataConverters.WideMemoToExtAStr, TDataConverters.ExtAStrToWideMemo);
  AddOnDemandConverter(dtXML, dtExtWideString, TDataConverters.WideMemoToExtWStr, TDataConverters.ExtWStrToWideMemo);
  AddOnDemandConverter(dtXML, dtBytes, TDataConverters.BlobToBytes, TDataConverters.BytesToBlob);
  AddOnDemandConverter(dtXML, dtMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtXML, dtWideMemo, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
  AddOnDemandConverter(dtXML, dtBlob, TDataConverters.CopyPtr, TDataConverters.CopyPtr);
end;

class function TMSConverterManager.GetDBProvider: Word;
begin
  Result := msBase;
end;

class function TMSConverterManager.GetDBType(SQLType: Word;
  const Flags: DBCOLUMNFLAGS; const Length: integer): Word;

  function IsFlagSetted(const Flag: cardinal): boolean;
  begin
    Result := (Flags and Flag) <> 0;
  end;

begin
  case SQLType of
    DBTYPE_BOOL:
      Result := msBit;
    DBTYPE_UI1:
      Result := msTinyint;
    DBTYPE_I2, DBTYPE_UI2:
      Result := msSmallint;
    DBTYPE_I4, DBTYPE_UI4:
      Result := msInt;
    DBTYPE_I8, DBTYPE_UI8:
      Result := msBigint;

    DBTYPE_NUMERIC:
      Result := msDecimal;
    DBTYPE_R4:
      Result := msReal;
    DBTYPE_R8:
      Result := msFloat;
    DBTYPE_CY:
      if Length <= 10 then
        Result := msSmallmoney
      else
        Result := msMoney;

    DBTYPE_DBTIMESTAMP, DBTYPE_DATE:
      if Length <= 16 then
        Result := msSmalldatetime
      else
      if Length <= 23 then
        Result := msDatetime
      else
        Result := msDatetime2;
    DBTYPE_DBDATE:
      Result := msDate;
    DBTYPE_DBTIME:
      Result := msTime;
    DBTYPE_DBDATETIMEOFFSET:
      Result := msDatetimeoffset;

    DBTYPE_STR:
      if IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then
        Result := msText
      else
      if IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH) then
        Result := msChar
      else
        Result := msVarchar;
    DBTYPE_WSTR:
      if IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then
        Result := msNText
      else
      if IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH) then
        Result := msNChar
      else
        Result := msNVarchar;
    DBTYPE_BYTES:
      if IsFlagSetted(DBCOLUMNFLAGS_ISROWVER) then
        Result := msTimestamp
      else
      if IsFlagSetted(DBCOLUMNFLAGS_ISLONG) then
        Result := msImage
      else
      if IsFlagSetted(DBCOLUMNFLAGS_ISFIXEDLENGTH) then
        Result := msBinary
      else
        Result := msVarBinary;

    DBTYPE_GUID:
      Result := msUniqueIdentifier;
    DBTYPE_VARIANT:
      Result := msSqlVariant;
    DBTYPE_XML:
      Result := msXml;
  else
    Result := 0;
  end;
end;

function TMSConverterManager.GetDateFormat: string;
begin
  Result := 'yyyy-mm-dd';
end;

function TMSConverterManager.GetTimeFormat: string;
begin
  Result := 'hh:mm:ss.zzz';
end;

function TMSConverterManager.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss.zzz';
end;

initialization
  InitMSTypes;
  MSConverterManager := TMSConverterManager.Create;

finalization
  MSConverterManager.Free;

end.
