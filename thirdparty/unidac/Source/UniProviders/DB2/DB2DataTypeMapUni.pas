
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DB2Dac.inc}
unit DB2DataTypeMapUni;
{$ENDIF}

interface

uses
  CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCDataTypeMap;
{$ELSE}
  ODBCCallUni, ODBCDataTypeMapUni;
{$ENDIF}

const
  db2Offset                = 400;
  db2Base                  = odbcBase + db2Offset;

  db2Char                  = odbcChar          + db2Offset;
  db2VarChar               = odbcVarChar       + db2Offset;
  db2SmallInt              = odbcSmallInt      + db2Offset;
  db2Integer               = odbcInteger       + db2Offset;
  db2BigInt                = odbcBigInt        + db2Offset;
  db2Decimal               = odbcDecimal       + db2Offset;
  db2Float                 = odbcFloat         + db2Offset;
  db2Double                = odbcDouble        + db2Offset;
  db2Real                  = odbcReal          + db2Offset;
  db2Numeric               = odbcNumeric       + db2Offset;
  db2Date                  = odbcDate          + db2Offset;
  db2Time                  = odbcTime          + db2Offset;
  db2TimeStamp             = odbcTimeStamp     + db2Offset;
  db2Binary                = odbcBinary        + db2Offset;
  db2VarBinary             = odbcVarBinary     + db2Offset;
  db2Blob                  = odbcLongVarBinary + db2Offset;
  db2Clob                  = odbcLongVarChar   + db2Offset;

type

  TDB2MapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TDB2ConverterManager = class(TODBCConverterManager)
  public
    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; override;
  end;

  procedure InitDB2Types;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  DB2Classes;
{$ELSE}
  DB2ClassesUni;
{$ENDIF}

var
  DB2ConverterManager: TDB2ConverterManager;

{ Function }

procedure InitDB2Types;
begin
  DBTypeInfos.Add(db2Char,             'Char',             True,  False);
  DBTypeInfos.Add(db2VarChar,          'VarChar',          True,  False);
  DBTypeInfos.Add(db2SmallInt,         'SmallInt',         False, False);
  DBTypeInfos.Add(db2Integer,          'Integer',          False, False);
  DBTypeInfos.Add(db2BigInt,           'BigInt',           False, False);
  DBTypeInfos.Add(db2Decimal,          'Decimal',          False, False);
  DBTypeInfos.Add(db2Float,            'Float',            False, False);
  DBTypeInfos.Add(db2Double,           'Double',           False, False);
  DBTypeInfos.Add(db2Real,             'Real',             False, False);
  DBTypeInfos.Add(db2Numeric,          'Numeric',          True,  True);
  DBTypeInfos.Add(db2Date,             'Date',             False, False);
  DBTypeInfos.Add(db2Time,             'Time',             False, False);
  DBTypeInfos.Add(db2TimeStamp,        'TimeStamp',        False, False);
  DBTypeInfos.Add(db2Binary,           'Binary',           True,  False);
  DBTypeInfos.Add(db2VarBinary,        'VarBinary',        True,  False);
  DBTypeInfos.Add(db2Blob,             'Blob',             False, False);
  DBTypeInfos.Add(db2Clob,             'Clob',             False, False);
end;

{ TDB2DataMapRules }

class function TDB2MapRules.GetConverterManager: TConverterManager;
begin
  Result := DB2ConverterManager;
end;

{ TDB2ConverterManager }

class function TDB2ConverterManager.GetDBProvider: Word;
begin
  Result := db2Base;
end;

class function TDB2ConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  Result := inherited GetDBType(SQLType, Unsigned) + db2Offset;
end;

initialization
  InitDB2Types;
  DB2ConverterManager := TDB2ConverterManager.Create;

finalization
  DB2ConverterManager.Free;

end.
