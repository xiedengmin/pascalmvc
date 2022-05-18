
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DBFDac.inc}
unit DBFDataTypeMapUni;
{$ENDIF}

interface

uses
{$IFDEF ODBC_PROVIDER}
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCDataTypeMap,
{$ELSE}
  ODBCCallUni, ODBCDataTypeMapUni,
{$ENDIF}
{$ENDIF}
  CRDataTypeMap;

const
  dbfOffset              = 500;
  dbfBase                = {$IFNDEF ODBC_PROVIDER}2000{$ELSE}odbcBase{$ENDIF} + dbfOffset;

  dbfChar                = {$IFNDEF ODBC_PROVIDER}1{$ELSE}odbcChar{$ENDIF}           + dbfOffset;
  dbfMemo                = {$IFNDEF ODBC_PROVIDER}2{$ELSE}odbcLongVarChar{$ENDIF}    + dbfOffset;
  dbfNumeric             = {$IFNDEF ODBC_PROVIDER}3{$ELSE}odbcNumeric{$ENDIF}        + dbfOffset;
  dbfDate                = {$IFNDEF ODBC_PROVIDER}4{$ELSE}odbcDate{$ENDIF}           + dbfOffset;
  dbfTime                = {$IFNDEF ODBC_PROVIDER}5{$ELSE}odbcTime{$ENDIF}           + dbfOffset;
  dbfTimeStamp           = {$IFNDEF ODBC_PROVIDER}6{$ELSE}odbcTimeStamp{$ENDIF}      + dbfOffset;
  dbfLogical             = {$IFNDEF ODBC_PROVIDER}7{$ELSE}odbcBit{$ENDIF}            + dbfOffset;
  dbfFloat               = {$IFNDEF ODBC_PROVIDER}8{$ELSE}odbcFloat{$ENDIF}          + dbfOffset;
  dbfDouble              = {$IFNDEF ODBC_PROVIDER}9{$ELSE}odbcDouble{$ENDIF}         + dbfOffset;
  dbfInteger             = {$IFNDEF ODBC_PROVIDER}10{$ELSE}odbcInteger{$ENDIF}       + dbfOffset;
  dbfBlob                = {$IFNDEF ODBC_PROVIDER}11{$ELSE}odbcLongVarBinary{$ENDIF} + dbfOffset;
  dbfVarBinary           = {$IFNDEF ODBC_PROVIDER}12{$ELSE}odbcVarBinary{$ENDIF}     + dbfOffset;
  dbfVarChar             = {$IFNDEF ODBC_PROVIDER}13{$ELSE}odbcVarChar{$ENDIF}       + dbfOffset;
{$IFDEF DBFENGINE}
  dbfCurrency            = 51 + dbfOffset; // more than highest ODBC
  dbfAutoincrement       = 52 + dbfOffset;
{$ENDIF}

type
  TDBFMapRules = class(TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TDBFConverterManager = class({$IFDEF ODBC_PROVIDER}TODBCConverterManager{$ELSE}TConverterManager{$ENDIF})
  public
    class function GetDBProvider: Word; override;
  {$IFDEF ODBC_PROVIDER}
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; override;
  {$ENDIF}
  end;

  procedure InitDBFTypes;

implementation

uses
{$IFDEF ODBC_PROVIDER}
{$IFNDEF UNIDACPRO}
  DBFClasses,
{$ELSE}
  DBFClassesUni,
{$ENDIF}
{$ENDIF}
  MemData;

var
  DBFConverterManager: TDBFConverterManager;

{ Function }

procedure InitDBFTypes;
begin
  DBTypeInfos.Add(dbfChar,              'Char',              True,  False);
  DBTypeInfos.Add(dbfVarChar,           'VarChar',           True,  False);
  DBTypeInfos.Add(dbfMemo,              'Memo',              False, False);
{$IFDEF DBFENGINE}
  DBTypeInfos.Add(dbfAutoincrement,     'Autoincrement',     False, False);
{$ENDIF}
  DBTypeInfos.Add(dbfLogical,           'Logical',           False, False);
  DBTypeInfos.Add(dbfInteger,           'Integer',           False, False);
  DBTypeInfos.Add(dbfFloat,             'Float',             False, False);
  DBTypeInfos.Add(dbfDouble,            'Double',            False, False);
{$IFDEF DBFENGINE}
  DBTypeInfos.Add(dbfCurrency,          'Currency',          False, False);
{$ENDIF}
  DBTypeInfos.Add(dbfNumeric,           'Numeric',           True,  True);
  DBTypeInfos.Add(dbfDate,              'Date',              False, False);
  DBTypeInfos.Add(dbfTime,              'Time',              False, False);
  DBTypeInfos.Add(dbfVarBinary,         'VarBinary',         True,  False);
  DBTypeInfos.Add(dbfBlob,              'Blob',              False, False);
end;

{ TDBFDataMapRules }

class function TDBFMapRules.GetConverterManager: TConverterManager;
begin
  Result := DBFConverterManager;
end;

{ TDBFConverterManager }

class function TDBFConverterManager.GetDBProvider: Word;
begin
  Result := dbfBase;
end;

{$IFDEF ODBC_PROVIDER}
class function TDBFConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  Result := inherited GetDBType(SQLType, Unsigned) + dbfOffset;
end;
{$ENDIF}

initialization
  InitDBFTypes;
  DBFConverterManager := TDBFConverterManager.Create;

finalization
  DBFConverterManager.Free;

end.
