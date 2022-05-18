
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 2006-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I IbDac.inc}
unit IBCDataTypeMapUni;

interface

uses
  Classes,
  DB,
{$IFNDEF LITE}
  CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  IBCCall;
{$ELSE}
  IBCCallUni;
{$ENDIF}

const
  ibcBase       = 400;

  ibcBoolean    = ibcBase + 1;
  ibcSmallint   = ibcBase + 2;
  ibcInteger    = ibcBase + 3;
  ibcBigint     = ibcBase + 4;
  ibcFloat      = ibcBase + 5;
  ibcDouble     = ibcBase + 6;
  ibcDecimal    = ibcBase + 7;
  ibcNumeric    = ibcBase + 8;
  ibcDate       = ibcBase + 9;
  ibcTime       = ibcBase + 10;
  ibcTimestamp  = ibcBase + 11;
  ibcChar       = ibcBase + 12;
  ibcVarchar    = ibcBase + 13;
  ibcCharBin    = ibcBase + 14;
  ibcVarcharBin = ibcBase + 15;
  ibcBlob       = ibcBase + 16;
  ibcText       = ibcBase + 17;
  ibcArray      = ibcBase + 18;

type

{$IFNDEF LITE}
  TIBCMapRule = class(TCRMapRule)
  private
    FCharsetID: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToRule(Dest: TIBCMapRule);
  public
    constructor Create(Owner: TCollection); override;

    property CharsetID: integer read FCharsetID write FCharsetID;
  end;

  TIBCMapRules = class(TCRMapRules)
  protected
    class function GetMapRuleClass: TCRMapRuleClass; override;

    function CheckSQLType(Rule: TMapRule; DBType, DBLength, DBScale, CharsetID: integer): boolean;
  public
    class function GetConverterManager: TConverterManager; override;

    function AddRule(const FieldName: string;
      DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, CharsetID: integer;
      DataType: Word; FieldLength, FieldScale: integer;
      IgnoreErrors: boolean; const Format: string): TCRMapRule;
    function DetectDBTypeMapRule(DBType, DBLength, DBScale, CharsetID: integer): TCRMapRule;
  end;
{$ENDIF}

  TIBCConverterManager = class{$IFNDEF LITE}(TConverterManager){$ENDIF}
  public
  {$IFNDEF LITE}
    constructor Create;

    class function GetDBProvider: Word; override;
  {$ENDIF}
    class function GetDBType(SQLType: Word; const SQLSubType: integer): Word;
  end;

implementation

uses
  MemData,
{$IFNDEF UNIDACPRO}
  IBCClasses;
{$ELSE}
  IBCClassesUni;
{$ENDIF}

{$IFNDEF LITE}

var
  IBCConverterManager: TIBCConverterManager;

{ Functions }

procedure InitIBCTypes;
begin
  DBTypeInfos.Add(ibcBoolean,   'Boolean',    False, False);
  DBTypeInfos.Add(ibcSmallint,  'Smallint',   False, False);
  DBTypeInfos.Add(ibcInteger,   'Integer',    False, False);
  DBTypeInfos.Add(ibcBigint,    'Bigint',     False, False);
  DBTypeInfos.Add(ibcFloat,     'Float',      False, False);
  DBTypeInfos.Add(ibcDouble,    'Double',     False, False);
  DBTypeInfos.Add(ibcDecimal,   'Decimal',    True,  True);
  DBTypeInfos.Add(ibcNumeric,   'Numeric',    True,  True);
  DBTypeInfos.Add(ibcDate,      'Date',       False, False);
  DBTypeInfos.Add(ibcTime,      'Time',       False, False);
  DBTypeInfos.Add(ibcTimestamp, 'Timestamp',  False, False);
  DBTypeInfos.Add(ibcChar,      'Char',       True,  False);
  DBTypeInfos.Add(ibcVarchar,   'Varchar',    True,  False);
  DBTypeInfos.Add(ibcCharBin,   'Binary',    True,  False);
  DBTypeInfos.Add(ibcVarcharBin,'Varbinary', True,  False);
  DBTypeInfos.Add(ibcBlob,      'Blob',       False, False);
  DBTypeInfos.Add(ibcText,      'Text',       False, False);
end;

{ TIBCMapRule }

constructor TIBCMapRule.Create(Owner: TCollection);
begin
  inherited;

  FCharsetID := -1;
end;

procedure TIBCMapRule.AssignTo(Dest: TPersistent);
begin
  if Dest is TIBCMapRule then
    AssignToRule(TIBCMapRule(Dest))
  else
    inherited;
end;

procedure TIBCMapRule.AssignToRule(Dest: TIBCMapRule);
begin
  inherited AssignToRule(Dest);

  Dest.FCharsetID := FCharsetID;
end;

{ TIBCMapRules }

class function TIBCMapRules.GetMapRuleClass: TCRMapRuleClass;
begin
  Result := TIBCMapRule;
end;

class function TIBCMapRules.GetConverterManager: TConverterManager;
begin
  Result := IBCConverterManager;
end;

function TIBCMapRules.CheckSQLType(Rule: TMapRule; DBType, DBLength, DBScale, CharsetID: integer): boolean;
begin
  Result := (Rule.DBType = DBType) and
   ((DBLength = -1) or ((Rule.DBLengthMin = -1) or (DBLength >= Rule.DBLengthMin)) and
                       ((Rule.DBLengthMax = -1) or (DBLength <= Rule.DBLengthMax))) and
   ((DBScale  = -1) or ((Rule.DBScaleMin  = -1) or (DBScale  >= Rule.DBScaleMin)) and
                       ((Rule.DBScaleMax  = -1) or (DBScale  <= Rule.DBScaleMax))) and
   ((TIBCMapRule(Rule).CharsetID = -1) or (CharsetID = TIBCMapRule(Rule).CharsetID));
end;

function TIBCMapRules.AddRule(const FieldName: string;
  DBType: Word; DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax, CharsetID: integer;
  DataType: Word; FieldLength, FieldScale: integer;
  IgnoreErrors: boolean; const Format: string): TCRMapRule;
begin
  Result := inherited AddRule(FieldName, DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
                              DataType, FieldLength, FieldScale, IgnoreErrors, Format);

  TIBCMapRule(Result).CharsetID := CharsetID;
end;

function TIBCMapRules.DetectDBTypeMapRule(DBType, DBLength, DBScale, CharsetID: integer): TCRMapRule;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if (Result.DBType <> 0) and CheckSQLType(Result, DBType, DBLength, DBScale, CharsetID) then
      exit;
  end;

  Result := nil;
end;

{ TIBCConverterManager }

constructor TIBCConverterManager.Create;
begin
  inherited;

  AddFetchConverter(ibcSmallint, dtSingle);
  AddFetchConverter(ibcSmallint, dtFloat);
  AddFetchConverter(ibcSmallint, dtExtended);

  AddFetchConverter(ibcFloat, dtSingle);
  AddFetchConverter(ibcFloat, dtExtended);

  AddFetchConverter(ibcDecimal, -1, -1, -1, -1, dtFloat);
  AddFetchConverter(ibcDecimal, -1, -1, -1, -1, dtBCD, dtFloat);
  AddFetchConverter(ibcDecimal, -1, -1, -1, -1, dtFmtBCD);

  AddFetchConverter(ibcNumeric, -1, -1, -1, -1, dtFloat);
  AddFetchConverter(ibcNumeric, -1, -1, -1, -1, dtBCD, dtFloat);
  AddFetchConverter(ibcNumeric, -1, -1, -1, -1, dtFmtBCD);

  AddFetchConverter(ibcChar, dtString);
  AddFetchConverter(ibcChar, dtFixedChar, dtString);
  AddFetchConverter(ibcChar, dtWideString);
  AddFetchConverter(ibcChar, dtFixedWideChar, dtWideString);

  AddFetchConverter(ibcVarchar, dtString);
  AddFetchConverter(ibcVarchar, dtWideString);

  AddFetchConverter(ibcBlob, dtMemo);
  AddFetchConverter(ibcBlob, dtWideMemo);
  AddFetchConverter(ibcBlob, dtString,     dtMemo);
  AddFetchConverter(ibcBlob, dtWideString, dtWideMemo);

  AddFetchConverter(ibcText, dtMemo);
  AddFetchConverter(ibcText, dtWideMemo);
  AddFetchConverter(ibcText, dtString,     dtMemo);
  AddFetchConverter(ibcText, dtWideString, dtWideMemo);
end;

class function TIBCConverterManager.GetDBProvider: Word;
begin
  Result := ibcBase;
end;

{$ENDIF}

class function TIBCConverterManager.GetDBType(SQLType: Word;
  const SQLSubType: integer): Word;
begin
  case SQLType of
    SQL_IB_BOOLEAN,
    SQL_FB_BOOLEAN:
      Result := ibcBoolean;
    SQL_IB_SHORT:
      case SQLSubType of
        0: Result := ibcSmallint;
        1: Result := ibcNumeric;
        2: Result := ibcDecimal;
      else
        Result := 0;
      end;
    SQL_IB_LONG:
      case SQLSubType of
        0: Result := ibcInteger;
        1: Result := ibcNumeric;
        2: Result := ibcDecimal;
      else
        Result := 0;
      end;
    SQL_IB_INT64:
      case SQLSubType of
        0: Result := ibcBigint;
        1: Result := ibcNumeric;
        2: Result := ibcDecimal;
      else
        Result := 0;
      end;
    SQL_IB_FLOAT:
      Result := ibcFloat;
    SQL_IB_DOUBLE:
      Result := ibcDouble;
    SQL_IB_TYPE_DATE:
      Result := ibcDate;
    SQL_IB_TYPE_TIME:
      Result := ibcTime;
    SQL_IB_TIMESTAMP:
      Result := ibcTimestamp;
    SQL_IB_TEXT:
      if SQLSubType = CH_OCTETS then
        Result := ibcCharBin
      else
        Result := ibcChar;
    SQL_IB_VARYING:
      if SQLSubType = CH_OCTETS then
        Result := ibcVarcharBin
      else
        Result := ibcVarchar;
    SQL_IB_QUAD, SQL_IB_BLOB:
      if SQLSubType = isc_blob_text then
        Result := ibcText
      else
        Result := ibcBlob;
    SQL_IB_ARRAY:
      Result := ibcArray;
  else
    Result := 0;
  end;
end;

{$IFNDEF LITE}

initialization
  InitIBCTypes;
  IBCConverterManager := TIBCConverterManager.Create;

finalization
  IBCConverterManager.Free;

{$ENDIF}

end.
