
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEDataTypeMapUni;

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
  aseOffset       = 700;
  aseBase         = aseOffset;

  aseChar           = {$IFNDEF ODBC_PROVIDER}1{$ELSE}odbcChar{$ENDIF}             + aseOffset;
  aseUniChar        = {$IFNDEF ODBC_PROVIDER}2{$ELSE}odbcWideChar{$ENDIF}         + aseOffset;
  aseVarchar        = {$IFNDEF ODBC_PROVIDER}3{$ELSE}odbcVarChar{$ENDIF}          + aseOffset;
  aseUniVarChar     = {$IFNDEF ODBC_PROVIDER}4{$ELSE}odbcWideVarChar{$ENDIF}      + aseOffset;
  aseSmallint       = {$IFNDEF ODBC_PROVIDER}5{$ELSE}odbcSmallInt{$ENDIF}         + aseOffset;
  aseTinyInt        = {$IFNDEF ODBC_PROVIDER}6{$ELSE}odbcUTinyInt{$ENDIF}         + aseOffset;
  aseInteger        = {$IFNDEF ODBC_PROVIDER}7{$ELSE}odbcInteger{$ENDIF}          + aseOffset;
  aseBigint         = {$IFNDEF ODBC_PROVIDER}8{$ELSE}odbcBigint{$ENDIF}           + aseOffset;
  aseDecimal        = {$IFNDEF ODBC_PROVIDER}9{$ELSE}odbcDecimal{$ENDIF}          + aseOffset;
  aseFloat          = {$IFNDEF ODBC_PROVIDER}10{$ELSE}odbcFloat{$ENDIF}           + aseOffset;
  aseDouble         = {$IFNDEF ODBC_PROVIDER}11{$ELSE}odbcDouble{$ENDIF}          + aseOffset;
  aseReal           = {$IFNDEF ODBC_PROVIDER}12{$ELSE}odbcReal{$ENDIF}            + aseOffset;
  aseNumeric        = {$IFNDEF ODBC_PROVIDER}13{$ELSE}odbcNumeric{$ENDIF}         + aseOffset;
  aseDate           = {$IFNDEF ODBC_PROVIDER}14{$ELSE}odbcDate{$ENDIF}            + aseOffset;
  aseTime           = {$IFNDEF ODBC_PROVIDER}15{$ELSE}odbcTime{$ENDIF}            + aseOffset;
  aseDateTime       = {$IFNDEF ODBC_PROVIDER}16{$ELSE}odbcTimeStamp{$ENDIF}       + aseOffset;
  aseBit            = {$IFNDEF ODBC_PROVIDER}17{$ELSE}odbcBit{$ENDIF}             + aseOffset;
  aseBinary         = {$IFNDEF ODBC_PROVIDER}18{$ELSE}odbcBinary{$ENDIF}          + aseOffset;
  aseVarBinary      = {$IFNDEF ODBC_PROVIDER}19{$ELSE}odbcVarBinary{$ENDIF}       + aseOffset;
  aseImage          = {$IFNDEF ODBC_PROVIDER}20{$ELSE}odbcLongVarBinary{$ENDIF}   + aseOffset;
  aseText           = {$IFNDEF ODBC_PROVIDER}21{$ELSE}odbcLongVarChar{$ENDIF}     + aseOffset;
  aseUniText        = {$IFNDEF ODBC_PROVIDER}22{$ELSE}odbcWideLongVarChar{$ENDIF} + aseOffset;
  aseXML            = {$IFNDEF ODBC_PROVIDER}23{$ELSE}odbcXml{$ENDIF}             + aseOffset;
  aseUSmallint      = {$IFNDEF ODBC_PROVIDER}24{$ELSE}odbcUSmallInt{$ENDIF}        + aseOffset;
  aseUInteger       = {$IFNDEF ODBC_PROVIDER}25{$ELSE}odbcUInteger{$ENDIF}        + aseOffset;
  aseUBigint        = {$IFNDEF ODBC_PROVIDER}26{$ELSE}odbcUBigint{$ENDIF}         + aseOffset;

  aseNChar          = 51 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;
  aseNVarChar       = 52 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;
  aseMoney          = 53 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;
  aseSmallmoney     = 54 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;
  aseSmalldatetime  = 55 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;
  aseTimestamp      = 56 {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF}                 + aseOffset;

type

  TASEMapRules = class (TCRMapRules)
  public
    class function GetConverterManager: TConverterManager; override;
  end;

  TASEConverterManager = class({$IFDEF ODBC_PROVIDER}TODBCConverterManager{$ELSE}TConverterManager{$ENDIF})
  protected
    function GetDateFormat: string; override;
    function GetTimeFormat: string; override;
    function GetDateTimeFormat: string; override;

  public
    constructor Create;

    class function GetDBProvider: Word; override;
  {$IFDEF ODBC_PROVIDER}
    class function GetDBType(SQLType: Integer; Unsigned: Boolean): Word; override;
  {$ENDIF}
  end;

  procedure InitASETypes;

implementation

uses
{$IFDEF ODBC_PROVIDER}
  {$IFNDEF UNIDACPRO}
  ASEClasses,
  {$ELSE}
  ASEClassesUni,
  {$ENDIF}
{$ENDIF}
  MemData;

var
  ASEConverterManager: TASEConverterManager;

{ Function }

procedure InitASETypes;
begin
  DBTypeInfos.Add(aseChar,           'Char',             True,  False);
  DBTypeInfos.Add(aseNChar,          'NChar',            True,  False);
  DBTypeInfos.Add(aseUniChar,        'UniChar',          True,  False);
  DBTypeInfos.Add(aseNVarChar,       'NVarChar',         True,  False);
  DBTypeInfos.Add(aseVarchar,        'VarChar',          True,  False);
  DBTypeInfos.Add(aseUniVarChar,     'UniVarChar',       True,  False);
  DBTypeInfos.Add(aseTinyInt,        'TinyInt',          False, False);
  DBTypeInfos.Add(aseSmallint,       'SmallInt',         False, False);
  DBTypeInfos.Add(aseUSmallint,      'USmallInt',        False, False);
  DBTypeInfos.Add(aseInteger,        'Integer',          False, False);
  DBTypeInfos.Add(aseUInteger,       'UInteger',         False, False);
  DBTypeInfos.Add(aseBigint,         'Bigint',           False, False);
  DBTypeInfos.Add(aseUBigint,        'UBigint',          False, False);
  DBTypeInfos.Add(aseDecimal,        'Decimal',          False, False);
  DBTypeInfos.Add(aseFloat,          'Float',            False, False);
  DBTypeInfos.Add(aseDouble,         'Double',           False, False);
  DBTypeInfos.Add(aseReal,           'Real',             True,  False);
  DBTypeInfos.Add(aseNumeric,        'Numeric',          True,  True);
  DBTypeInfos.Add(aseDate,           'Date',             False, False);
  DBTypeInfos.Add(aseTime,           'Time',             False, False);
  DBTypeInfos.Add(aseDateTime,       'DateTime',         False, False);
  DBTypeInfos.Add(aseBit,            'Bit',              False, False);
  DBTypeInfos.Add(aseBinary,         'Binary',           True,  False);
  DBTypeInfos.Add(aseVarBinary,      'VarBinary',        True,  False);
  DBTypeInfos.Add(aseImage,          'Image',            False, False);
  DBTypeInfos.Add(aseText,           'Text',             False, False);
  DBTypeInfos.Add(aseUniText,        'UniText',          False, False);
  DBTypeInfos.Add(aseXml,            'Xml',              False, False);
  DBTypeInfos.Add(aseSmallmoney,     'Smallmoney',       False, False);
  DBTypeInfos.Add(aseMoney,          'Money',            False, False);
  DBTypeInfos.Add(aseSmalldatetime,  'Smalldatetime',    False, False);
  DBTypeInfos.Add(aseTimestamp,      'Timestamp',        False, False);
end;

{ TASEDataMapRules }

class function TASEMapRules.GetConverterManager: TConverterManager;
begin
  Result := ASEConverterManager;
end;

{ TASEConverterManager }

constructor TASEConverterManager.Create;
begin
  inherited;

  AddFetchConverter(aseDecimal, dtFloat);
  AddFetchConverter(aseDecimal, dtBCD, dtFloat);
  AddFetchConverter(aseDecimal, dtFmtBCD);

  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtInt16,      dtFmtBCD);
  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtUInt16,     dtFmtBCD);
  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtInt32,      dtFmtBCD);
  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtUInt32,     dtFmtBCD);
  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtInt64,      dtFmtBCD);
  AddFetchConverter(aseDecimal, -1, -1, 0, 0, dtUInt64,     dtFmtBCD);
  AddFetchConverter(aseDecimal, dtString,     dtFmtBCD);
  AddFetchConverter(aseDecimal, dtWideString, dtFmtBCD);

  AddFetchConverter(aseTinyint, dtInt16,      dtUInt8);
  AddFetchConverter(aseTinyint, dtUInt16,     dtUInt8);
  AddFetchConverter(aseTinyint, dtInt32,      dtUInt8);
  AddFetchConverter(aseTinyint, dtUInt32,     dtUInt8);
  AddFetchConverter(aseTinyint, dtInt64,      dtUInt8);
  AddFetchConverter(aseTinyint, dtUInt64,     dtUInt8);
  AddFetchConverter(aseTinyint, dtString,     dtUInt8);
  AddFetchConverter(aseTinyint, dtWideString, dtUInt8);


  AddFetchConverter(aseChar, dtString);
  AddFetchConverter(aseChar, dtFixedChar, dtString);
  AddFetchConverter(aseChar, dtWideString);
  AddFetchConverter(aseChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(aseVarchar, dtString);
  AddFetchConverter(aseVarchar, dtWideString);

  AddFetchConverter(aseUniChar, dtString);
  AddFetchConverter(aseUniChar, dtFixedChar, dtString);
  AddFetchConverter(aseUniChar, dtWideString);
  AddFetchConverter(aseUniChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(aseUniVarchar, dtString);
  AddFetchConverter(aseUniVarchar, dtWideString);

//  AddFetchConverter(aseBinary, dtString);
//  AddFetchConverter(aseBinary, dtWideString);

//  AddFetchConverter(msVarbinary, dtString);
//  AddFetchConverter(msVarbinary, dtWideString);

  AddFetchConverter(aseText, dtMemo);
  AddFetchConverter(aseText, dtWideMemo);
  AddFetchConverter(aseText, dtString,     dtMemo);
  AddFetchConverter(aseText, dtWideString, dtMemo);

  AddFetchConverter(aseUniText, dtMemo);
  AddFetchConverter(aseUniText, dtWideMemo);
  AddFetchConverter(aseUniText, dtString,     dtWideMemo);
  AddFetchConverter(aseUniText, dtWideString, dtWideMemo);

//  AddFetchConverter(aseImage, dtMemo);
//  AddFetchConverter(aseImage, dtWideMemo);
//  AddFetchConverter(aseImage, dtString,     dtMemo);
//  AddFetchConverter(aseImage, dtWideString, dtWideMemo);

  AddFetchConverter(aseXml, dtMemo);
  AddFetchConverter(aseXml, dtWideMemo);
  AddFetchConverter(aseXml, dtString,     dtWideMemo);
  AddFetchConverter(aseXml, dtWideString, dtWideMemo);
end;

function TASEConverterManager.GetDateFormat: string;
begin
  Result := 'yyyy-mm-dd';
end;

function TASEConverterManager.GetTimeFormat: string;
begin
  Result := 'hh:mm:ss.zzz';
end;

function TASEConverterManager.GetDateTimeFormat: string;
begin
  Result := 'yyyy-mm-dd hh:mm:ss.zzz';
end;

class function TASEConverterManager.GetDBProvider: Word;
begin
  Result := aseBase {$IFDEF ODBC_PROVIDER}+ odbcBase{$ENDIF};
end;

{$IFDEF ODBC_PROVIDER}

class function TASEConverterManager.GetDBType(SQLType: Integer; Unsigned: Boolean): Word;
begin
  Result := inherited GetDBType(SQLType, Unsigned) + aseOffset;
end;

{$ENDIF}

initialization
  InitASETypes;
  ASEConverterManager := TASEConverterManager.Create;

finalization
  ASEConverterManager.Free;

end.
