
//////////////////////////////////////////////////
//  OBDBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCDataTypeMapUni;

interface

uses
  CRDataTypeMap;

const
  odbcBase                  = 2000;

  odbcChar                  = odbcBase + 1;
  odbcWideChar              = odbcBase + 2;
  odbcVarChar               = odbcBase + 3;
  odbcWideVarChar           = odbcBase + 4;
  odbcLongVarChar           = odbcBase + 5;
  odbcWideLongVarChar       = odbcBase + 6;
  odbcSmallInt              = odbcBase + 7;
  odbcTinyInt               = odbcBase + 8;
  odbcInteger               = odbcBase + 9;
  odbcBigInt                = odbcBase + 10;
  odbcDecimal               = odbcBase + 11;
  odbcFloat                 = odbcBase + 12;
  odbcDouble                = odbcBase + 13;
  odbcReal                  = odbcBase + 14;
  odbcNumeric               = odbcBase + 15;
//  odbcDateTime              = odbcBase + 16;
  odbcDate                  = odbcBase + 17;
  odbcTime                  = odbcBase + 18;
  odbcTimeStamp             = odbcBase + 19;
  odbcBit                   = odbcBase + 20;
  odbcBinary                = odbcBase + 21;
  odbcVarBinary             = odbcBase + 22;
  odbcLongVarBinary         = odbcBase + 23;
  odbcBlob                  = odbcBase + 24; // DB2 data type
  odbcClob                  = odbcBase + 25; // DB2 data type
  odbcXml                   = odbcBase + 26;
  odbcVariant               = odbcBase + 27;
  odbcUTinyInt              = odbcBase + 28;
  odbcUSmallInt             = odbcBase + 29;
  odbcUInteger              = odbcBase + 30;
  odbcUBigInt               = odbcBase + 31;

type

  TODBCMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TODBCConverterManager = class(TConverterManager)
  public
    constructor Create;

    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; virtual;
  end;

  procedure InitODBCTypes;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCClasses;
{$ELSE}
  ODBCCallUni, ODBCClassesUni;
{$ENDIF}

var
  ODBCConverterManager: TODBCConverterManager;

{ Functions }

procedure InitODBCTypes;
begin
  DBTypeInfos.Add(odbcChar,              'Char',              True,  False);
  DBTypeInfos.Add(odbcWideChar,          'WideChar',          True,  False);
  DBTypeInfos.Add(odbcVarChar,           'VarChar',           True,  False);
  DBTypeInfos.Add(odbcWideVarChar,       'WideVarChar',       True,  False);
  DBTypeInfos.Add(odbcLongVarChar,       'LongVarChar',       False, False);
  DBTypeInfos.Add(odbcWideLongVarChar,   'WideLongVarChar',   False, False);
  DBTypeInfos.Add(odbcBit,               'Bit',               False, False);
  DBTypeInfos.Add(odbcTinyInt,           'TinyInt',           False, False);
  DBTypeInfos.Add(odbcUTinyInt,          'Unsigned TinyInt',  False, False);
  DBTypeInfos.Add(odbcSmallInt,          'SmallInt',          False, False);
  DBTypeInfos.Add(odbcUSmallInt,         'Unsigned SmallInt', False, False);
  DBTypeInfos.Add(odbcInteger,           'Integer',           False, False);
  DBTypeInfos.Add(odbcUInteger,          'Unsigned Integer',  False, False);
  DBTypeInfos.Add(odbcBigInt,            'BigInt',            False, False);
  DBTypeInfos.Add(odbcUBigInt,           'Unsigned BigInt',   False, False);
  DBTypeInfos.Add(odbcReal,              'Real',              False, False);
  DBTypeInfos.Add(odbcFloat,             'Float',             False, False);
  DBTypeInfos.Add(odbcDouble,            'Double',            False, False);
  DBTypeInfos.Add(odbcDecimal,           'Decimal',           False, False);
  DBTypeInfos.Add(odbcNumeric,           'Numeric',           True,  True);
//  DBTypeInfos.Add(odbcDateTime,          'DateTime',          False, False);
  DBTypeInfos.Add(odbcDate,              'Date',              False, False);
  DBTypeInfos.Add(odbcTime,              'Time',              False, False);
  DBTypeInfos.Add(odbcTimeStamp,         'TimeStamp',         False, False);
  DBTypeInfos.Add(odbcBinary,            'Binary',            True,  False);
  DBTypeInfos.Add(odbcVarBinary,         'VarBinary',         True,  False);
  DBTypeInfos.Add(odbcLongVarBinary,     'LongVarBinary',     False, False);
  DBTypeInfos.Add(odbcBlob,              'Blob',              False, False);
  DBTypeInfos.Add(odbcClob,              'Clob',              False, False);
  DBTypeInfos.Add(odbcXml,               'Xml',               False, False);
  DBTypeInfos.Add(odbcVariant,           'Variant',           False, False);
end;

{ TPgDataMapRules }

class function TODBCMapRules.GetConverterManager: TConverterManager;
begin
  Result := ODBCConverterManager;
end;

{ TPgConverterManager }

constructor TODBCConverterManager.Create;
begin
  inherited;
end;

class function TODBCConverterManager.GetDBProvider: Word;
begin
  Result := odbcBase;
end;

class function TODBCConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  case SQLType of
    SQL_CHAR:
      Result := odbcChar;
    SQL_WCHAR:
      Result := odbcWideChar;
    SQL_VARCHAR:
      Result := odbcVarChar;
    SQL_WVARCHAR:
      Result := odbcWideVarChar;
    SQL_LONGVARCHAR:
      Result := odbcLongVarChar;
    SQL_WLONGVARCHAR:
      Result := odbcWideLongVarChar;
    SQL_SMALLINT:
      if Unsigned then
        Result := odbcUSmallInt
      else
        Result := odbcSmallInt;
    SQL_TINYINT:
      if Unsigned then
        Result := odbcUTinyInt
      else
        Result := odbcTinyInt;
    SQL_INTEGER:
      if Unsigned then
        Result := odbcUInteger
      else
        Result := odbcInteger;
    SQL_BIGINT:
      if Unsigned then
        Result := odbcUBigInt
      else
        Result := odbcBigInt;
    SQL_DECIMAL:
      Result := odbcDecimal;
    SQL_FLOAT:
      Result := odbcFloat;
    SQL_DOUBLE:
      Result := odbcDouble;
    SQL_REAL:
      Result := odbcReal;
    SQL_NUMERIC:
      Result := odbcNumeric;
//    SQL_DATETIME:
//      Result := odbcDateTime;
    SQL_DATE, SQL_TYPE_DATE:
      Result := odbcDate;
    SQL_TIME, SQL_TYPE_TIME:
      Result := odbcTime;
    SQL_TIMESTAMP, SQL_TYPE_TIMESTAMP:
      Result := odbcTimeStamp;
    SQL_BIT:
      Result := odbcBit;
    SQL_BINARY:
      Result := odbcBinary;
    SQL_VARBINARY:
      Result := odbcVarBinary;
    SQL_LONGVARBINARY:
      Result := odbcLongVarBinary;
    SQL_BLOB:
      Result := odbcBlob;
    SQL_CLOB, SQL_DBCLOB:
      Result := odbcClob;
    SQL_XML, SQL_XML_DB2:
      Result := odbcXml;
    SQL_VARIANT:
      Result := odbcVariant;
    else
      Result := 0;
  end;
end;

initialization
  InitODBCTypes;
  ODBCConverterManager := TODBCConverterManager.Create;

finalization
  ODBCConverterManager.Free;

end.
