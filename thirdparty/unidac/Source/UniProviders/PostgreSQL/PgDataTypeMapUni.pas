
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I PgDac.inc}
unit PgDataTypeMapUni;
{$ENDIF}

interface

uses
  CRDataTypeMap;

const
  pgBase                  = 500;

  pgBigInt                = pgBase + 1;
  pgBigSerial             = pgBase + 2;
  pgBoolean               = pgBase + 5;
  pgBytea                 = pgBase + 6;
  pgCharacter             = pgBase + 7;
  pgCharacterVarying      = pgBase + 8;
  pgDate                  = pgBase + 9;
  pgDoublePrecision       = pgBase + 10;
  pgInteger               = pgBase + 11;
  pgMoney                 = pgBase + 12;
  pgNumeric               = pgBase + 13;
  pgReal                  = pgBase + 14;
  pgSmallint              = pgBase + 15;
  pgSerial                = pgBase + 16;
  pgText                  = pgBase + 17;
  pgTime                  = pgBase + 18;
  pgTimeWithTimeZone      = pgBase + 19;
  pgTimeStamp             = pgBase + 20;
  pgTimeStampWithTimeZone = pgBase + 21;
  pgUUID                  = pgBase + 23;

  pgBit                   = pgBase + 51;
  pgBitVarying            = pgBase + 52;

type

  TPgMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TPgConverterManager = class(TConverterManager)
  public
    constructor Create;

    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: integer): Word;
  end;

  procedure InitPgTypes;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  PgCall, PgClasses;
{$ELSE}
  PgCallUni, PgClassesUni;
{$ENDIF}

const
  pgBox                   = pgBase + 53;
  pgCidr                  = pgBase + 54;
  pgCircle                = pgBase + 55;
  pgInet                  = pgBase + 56;
  pgInterval              = pgBase + 57;
  pgLine                  = pgBase + 58;
  pgLseg                  = pgBase + 59;
  pgMacaddr               = pgBase + 60;
  pgPath                  = pgBase + 61;
  pgPoint                 = pgBase + 62;
  pgPolygon               = pgBase + 63;
  pgTSQuery               = pgBase + 64;
  pgTSVector              = pgBase + 65;
  pgTxIDSnapshot          = pgBase + 66;
  pgXML                   = pgBase + 67;
  pgJSONB                 = pgBase + 68;

var
  PgConverterManager: TPgConverterManager;

{ Functions }

procedure InitPgTypes;
begin
  DBTypeInfos.Add(pgCharacter,             'Character',                True,  False);
  DBTypeInfos.Add(pgCharacterVarying,      'Character Varying',        True,  False);
  DBTypeInfos.Add(pgSmallint,              'Smallint',                 False, False);
  DBTypeInfos.Add(pgInteger,               'Integer',                  False, False);
  DBTypeInfos.Add(pgBigInt,                'BigInt',                   False, False);
  DBTypeInfos.Add(pgSerial,                'Serial',                   False, False);
  DBTypeInfos.Add(pgBigSerial,             'BigSerial',                False, False);
  DBTypeInfos.Add(pgDoublePrecision,       'Double Precision',         False, False);
  DBTypeInfos.Add(pgReal,                  'Real',                     False, False);
  DBTypeInfos.Add(pgMoney,                 'Money',                    False, False);
  DBTypeInfos.Add(pgNumeric,               'Numeric',                  True,  True);
  DBTypeInfos.Add(pgDate,                  'Date',                     False, False);
  DBTypeInfos.Add(pgTime,                  'Time',                     False, False);
  DBTypeInfos.Add(pgTimeWithTimeZone,      'Time With Time Zone',      False, False);
  DBTypeInfos.Add(pgTimeStamp,             'TimeStamp',                False, False);
  DBTypeInfos.Add(pgTimeStampWithTimeZone, 'TimeStamp With Time Zone', False, False);
  DBTypeInfos.Add(pgBytea,                 'Bytea',                    False, False);
  DBTypeInfos.Add(pgText,                  'Text',                     False, False);
  DBTypeInfos.Add(pgUUID,                  'UUID',                     False, False);
end;

{ TPgDataMapRules }

class function TPgMapRules.GetConverterManager: TConverterManager;
begin
  Result := PgConverterManager;
end;

{ TPgConverterManager }

constructor TPgConverterManager.Create;
begin
  inherited;

  AddFetchConverter(pgCharacter, dtString);
  AddFetchConverter(pgCharacter, dtFixedChar, dtString);
  AddFetchConverter(pgCharacter, dtWideString);
  AddFetchConverter(pgCharacter, dtFixedWideChar, dtWideString);

  AddFetchConverter(pgCharacterVarying, 0, 0, -1, -1, dtString,     dtMemo);
  AddFetchConverter(pgCharacterVarying, 0, 0, -1, -1, dtWideString, dtWideMemo);
  AddFetchConverter(pgCharacterVarying, dtString);
  AddFetchConverter(pgCharacterVarying, dtWideString);

  AddFetchConverter(pgText, dtMemo);
  AddFetchConverter(pgText, dtWideMemo);
  AddFetchConverter(pgText, dtString,        dtMemo);
  AddFetchConverter(pgText, dtWideString,    dtWideMemo);

  AddFetchConverter(pgBytea, dtMemo);
  AddFetchConverter(pgBytea, dtWideMemo);
  AddFetchConverter(pgBytea, dtString,        dtMemo);
  AddFetchConverter(pgBytea, dtWideString,    dtWideMemo);

  AddFetchConverter(pgReal, dtSingle);

  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtInt8,     dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtUInt8,    dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtSmallint, dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtWord,     dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtInteger,  dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtLongWord,  dtSmallint);
  AddFetchConverter(pgNumeric,  1,  4, 0, 0, dtLargeint, dtSmallint);

  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtInt8,     dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtUInt8,    dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtSmallint, dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtWord,     dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtInteger,  dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtLongWord, dtInteger);
  AddFetchConverter(pgNumeric,  5,  9, 0, 0, dtLargeint, dtInteger);

  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtInt8,     dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtUInt8,    dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtSmallint, dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtWord,     dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtInteger,  dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtLongWord, dtLargeint);
  AddFetchConverter(pgNumeric, 10, -1, 0, 0, dtLargeint, dtLargeint);

  AddFetchConverter(pgNumeric, dtFloat);
{$IFDEF VER14P}
  AddFetchConverter(pgNumeric, dtSingle, dtFloat);
{$ENDIF}
  AddFetchConverter(pgNumeric, dtBCD);
  AddFetchConverter(pgNumeric, dtFMTBCD);

  AddFetchConverter(pgDate, dtDate);
  AddFetchConverter(pgDate, dtDateTime, dtDate);
  AddFetchConverter(pgDate, dtSQLTimeStamp, dtDate);
  AddFetchConverter(pgDate, dtPgDate);

  AddFetchConverter(pgTime, dtTime);
  AddFetchConverter(pgTime, dtDateTime, dtTime);
  AddFetchConverter(pgTime, dtSQLTimeStamp, dtTime);
  AddFetchConverter(pgTime, dtPgTime);
  AddFetchConverter(pgTime, dtPgTimeTZ, dtPgTime);

  AddFetchConverter(pgTimeWithTimeZone, dtTime);
  AddFetchConverter(pgTimeWithTimeZone, dtDateTime, dtTime);
  AddFetchConverter(pgTimeWithTimeZone, dtSQLTimeStamp, dtTime);
  AddFetchConverter(pgTimeWithTimeZone, dtPgTimeTZ);
  AddFetchConverter(pgTimeWithTimeZone, dtPgTime, dtPgTimeTZ);

  AddFetchConverter(pgTimeStamp, dtTime, dtDateTime);
  AddFetchConverter(pgTimeStamp, dtDate, dtDateTime);
  AddFetchConverter(pgTimeStamp, dtDateTime);
  AddFetchConverter(pgTimeStamp, dtSQLTimeStamp, dtDateTime);
  AddFetchConverter(pgTimeStamp, dtPgTimeStamp);
  AddFetchConverter(pgTimeStamp, dtPgTimeStampTZ, dtPgTimeStamp);

  AddFetchConverter(pgTimeStampWithTimeZone, dtTime, dtDateTime);
  AddFetchConverter(pgTimeStampWithTimeZone, dtDate, dtDateTime);
  AddFetchConverter(pgTimeStampWithTimeZone, dtDateTime);
  AddFetchConverter(pgTimeStampWithTimeZone, dtSQLTimeStamp, dtDateTime);
  AddFetchConverter(pgTimeStampWithTimeZone, dtPgTimeStampTZ);
  AddFetchConverter(pgTimeStampWithTimeZone, dtPgTimeStamp, dtPgTimeStampTZ);
end;

class function TPgConverterManager.GetDBProvider: Word;
begin
  Result := pgBase;
end;

class function TPgConverterManager.GetDBType(SQLType: integer): Word;
begin
  case SQLType of
    SQL_PG_BIGINT:
      Result := pgBigInt;
    SQL_PG_BIT:
      Result := pgBit;
    SQL_PG_VARBIT:
      Result := pgBitVarying;
    SQL_PG_BOOLEAN:
      Result := pgBoolean;
    SQL_PG_BOX:
      Result := pgBox;
    SQL_PG_BYTEA:
      Result := pgBytea;
    SQL_PG_CHAR:
      Result := pgCharacter;
    SQL_PG_VARCHAR,
    SQL_PG_NAME:
      Result := pgCharacterVarying;
    SQL_PG_CIDR:
      Result := pgCidr;
    SQL_PG_CIRCLE:
      Result := pgCircle;
    SQL_PG_DATE:
      Result := pgDate;
    SQL_PG_DOUBLE:
      Result := pgDoublePrecision;
    SQL_PG_INT,
    SQL_PG_OID:
      Result := pgInteger;
    SQL_PG_INTERVAL:
      Result := pgInterval;
    SQL_PG_LINE:
      Result := pgLine;
    SQL_PG_LSEG:
      Result := pgLseg;
    SQL_PG_MONEY:
      Result := pgMoney;
    SQL_PG_NUMERIC:
      Result := pgNumeric;
    SQL_PG_PATH:
      Result := pgPath;
    SQL_PG_POINT:
      Result := pgPoint;
    SQL_PG_POLYGON:
      Result := pgPolygon;
    SQL_PG_REAL:
      Result := pgReal;
    SQL_PG_SMALLINT:
      Result := pgSmallint;
    SQL_PG_TEXT:
      Result := pgText;
    SQL_PG_TIME:
      Result := pgTime;
    SQL_PG_TIMETZ:
      Result := pgTimeWithTimeZone;
    SQL_PG_TIMESTAMP:
      Result := pgTimeStamp;
    SQL_PG_TIMESTAMPTZ:
      Result := pgTimeStampWithTimeZone;
    SQL_PG_UUID:
      Result := pgUUID;
    SQL_PG_JSONB:
      Result := pgJSONB;
    else
      Result := 0;
  end;
end;

initialization
  InitPgTypes;
  PgConverterManager := TPgConverterManager.Create;

finalization
  PgConverterManager.Free;

end.
