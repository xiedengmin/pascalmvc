
//////////////////////////////////////////////////
//  Advantage Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ADSDac.inc}
unit ADSDataTypeMapUni;
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
  adsOffset          = 200;
  adsBase            = odbcBase + adsOffset;

  adsCharacter       = odbcChar            + adsOffset;
  adsNChar           = odbcWideChar        + adsOffset;
  adsVarChar         = odbcVarChar         + adsOffset;
  adsNVarChar        = odbcWideVarChar     + adsOffset;
  adsShortInteger    = odbcSmallInt        + adsOffset;
  adsInteger         = odbcInteger         + adsOffset;
  adsNumeric         = odbcDecimal         + adsOffset;
  adsDouble          = odbcDouble          + adsOffset;
  adsMoney           = odbcNumeric         + adsOffset;
  adsDate            = odbcDate            + adsOffset;
  adsTime            = odbcTime            + adsOffset;
  adsTimeStamp       = odbcTimeStamp       + adsOffset;
  adsBinary          = odbcLongVarBinary   + adsOffset;
  adsLogical         = odbcBit             + adsOffset;
  adsVarBinary       = odbcVarBinary       + adsOffset;
  adsImage           = odbcLongVarBinary   + adsOffset;
  adsMemo            = odbcLongVarChar     + adsOffset;
  adsNMemo           = odbcWideLongVarChar + adsOffset;

type

  TADSMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TADSConverterManager = class(TODBCConverterManager)
  public
    class function GetDBProvider: Word; override;
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; override;
  end;

  procedure InitADSTypes;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  ADSClasses;
{$ELSE}
  ADSClassesUni;
{$ENDIF}

var
  ADSConverterManager: TADSConverterManager;

{ Functions }

procedure InitADSTypes;
begin
  DBTypeInfos.Add(adsCharacter,          'Character',      True,  False);
  DBTypeInfos.Add(adsNChar,              'NChar',          True,  False);
  DBTypeInfos.Add(adsVarChar,            'VarChar',        True,  False);
  DBTypeInfos.Add(adsNVarChar,           'NVarChar',       True,  False);
  DBTypeInfos.Add(adsShortInteger,       'ShortInteger',   False, False);
  DBTypeInfos.Add(adsInteger,            'Integer',        False, False);
  DBTypeInfos.Add(adsNumeric,            'Numeric',        False, False);
  DBTypeInfos.Add(adsDouble,             'Double',         False, False);
  DBTypeInfos.Add(adsMoney,              'Money',          False, False);
  DBTypeInfos.Add(adsDate,               'Date',           False, False);
  DBTypeInfos.Add(adsTime,               'Time',           False, False);
  DBTypeInfos.Add(adsTimeStamp,          'TimeStamp',      False, False);
  DBTypeInfos.Add(adsLogical,            'Logical',        False, False);
  DBTypeInfos.Add(adsBinary,             'Binary',         True,  False);
  DBTypeInfos.Add(adsVarBinary,          'VarBinary',      True,  False);
  DBTypeInfos.Add(adsImage,              'Image',          False, False);
  DBTypeInfos.Add(adsMemo,               'Memo',           False, False);
  DBTypeInfos.Add(adsNMemo,              'NMemo',          False, False);
end;

{ TADSDataMapRules }

class function TADSMapRules.GetConverterManager: TConverterManager;
begin
  Result := ADSConverterManager;
end;

{ TADSConverterManager }

class function TADSConverterManager.GetDBProvider: Word;
begin
  Result := adsBase;
end;

class function TADSConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  Result := inherited GetDBType(SQLType, Unsigned) + adsOffset;
end;

initialization
  InitADSTypes;
  ADSConverterManager := TADSConverterManager.Create;

finalization
  ADSConverterManager.Free;

end.
