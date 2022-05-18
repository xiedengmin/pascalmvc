
//////////////////////////////////////////////////
//  MS Access Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I AccessDac.inc}
unit AccessDataTypeMapUni;
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
  accOffset                = 100;
  accBase                  = odbcBase + accOffset;

  accBinary                = odbcBinary        + accOffset;
  accBit                   = odbcBit           + accOffset;
  accCurrency              = odbcNumeric       + accOffset;
  accDateTime              = odbcTimeStamp     + accOffset;
  accLongBinary            = odbcLongVarBinary + accOffset;
  accLongText              = odbcLongVarChar   + accOffset;
  accSingle                = odbcReal          + accOffset;
  accDouble                = odbcDouble        + accOffset;
  accByte                  = odbcTinyInt       + accOffset;
  accInteger               = odbcSmallInt      + accOffset;
  accLong                  = odbcInteger       + accOffset;
  accNumeric               = odbcNumeric       + accOffset;
  {$IFDEF VER12P}
  accText                  = odbcWideVarChar   + accOffset;
  {$ELSE}
  accText                  = odbcVarChar       + accOffset;
  {$ENDIF}
  accVarBinary             = odbcVarBinary     + accOffset;

type

  TAccessMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TAccessConverterManager = class(TODBCConverterManager)
  public
    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; override;
  end;

  procedure InitAccessTypes;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  AccessClasses;
{$ELSE}
  AccessClassesUni;
{$ENDIF}

var
  AccessConverterManager: TAccessConverterManager;

{ Functions }

procedure InitAccessTypes;
begin
  DBTypeInfos.Add(accText,              'Text',              True,  False);
  DBTypeInfos.Add(accLongText,          'LongText',          False, False);
  DBTypeInfos.Add(accByte,              'Byte',              False, False);
  DBTypeInfos.Add(accInteger,           'Integer',           False, False);
  DBTypeInfos.Add(accLong,              'Long',              False, False);
  DBTypeInfos.Add(accSingle,            'Single',            False, False);
  DBTypeInfos.Add(accDouble,            'Double',            False, False);
  DBTypeInfos.Add(accNumeric,           'Numeric',           True,  True);
  DBTypeInfos.Add(accDateTime,          'DateTime',          False, False);
  DBTypeInfos.Add(accBit,               'Bit',               False, False);
  DBTypeInfos.Add(accBinary,            'Binary',            True,  False);
  DBTypeInfos.Add(accVarBinary,         'VarBinary',         True,  False);
  DBTypeInfos.Add(accLongBinary,        'LongBinary',        False, False);
end;

{ TAccessDataMapRules }

class function TAccessMapRules.GetConverterManager: TConverterManager;
begin
  Result := AccessConverterManager;
end;

{ TAccessConverterManager }

class function TAccessConverterManager.GetDBProvider: Word;
begin
  Result := accBase;
end;

class function TAccessConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  Result := inherited GetDBType(SQLType, Unsigned) + accOffset;
end;

initialization
  InitAccessTypes;
  AccessConverterManager := TAccessConverterManager.Create;

finalization
  AccessConverterManager.Free;

end.
