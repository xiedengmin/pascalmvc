//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I NexusDac.inc}
unit NexusServicesUni;
{$ENDIF}

interface

{$IFNDEF DUMMY}
uses
{$IFDEF CLR}
  System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  CRTypes, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, CRServerEnumerator, DBAccess, DAScript, DADump, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  NexusClasses, NexusParser, NexusSQLGenerator;
{$ELSE}
  NexusClassesUni, NexusParserUni, NexusSQLGeneratorUni;
{$ENDIF}

type
{ TCustomNexusDataSetUpdater }

  TCustomNexusDataSetUpdater = class(TDADataSetUpdater)
  protected
    function PerformRefreshRecord: boolean; override;
  public
    function GetDefaultExpressionValue(DefExpr: string; out Value: variant): boolean; override;
  end;

{ TCustomNexusDataSetService }

  TCustomNexusDataSetService = class(TDADataSetService)
  protected
    FUpdater: TCustomNexusDataSetUpdater;

    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    procedure SetDataSetUpdater(Value: TDataSetUpdater); override;

    function DetectCanModify: boolean; override;
    procedure FillFieldsDefaultValues; override;
    function GetRecCount: integer; override;
    procedure InitCursor; override;
  public
    function GetDBKeyList(const TableName, IndexName: string): string; override;
    function NeedParamValuesOnPrepare: boolean; override;
  end;

  TCustomNexusFieldTypeMap = class(TDAFieldTypeMap)
  public
    class function GetFieldType(DataType: Word): TFieldType; override;
    class function GetDataType(FieldType: TFieldType; SubDataType: Word = 0): integer; override;
  end;

  TNexusScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

  TCustomNexusDumpProcessor = class(TDADumpProcessor)
  protected
    function CreateQuery: TCustomDADataSet; override;
    function GetFieldValueForDump(Field: TField): string; override;
  end;

  TNexusServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: TStrings); override;
  end;

{$ENDIF} // DUMMY

implementation

{$IFNDEF DUMMY}
uses
{$IFNDEF NEXUS_EMBEDDED}
  nxtwWinsockTransport,
{$ENDIF}
  CRProps,
{$IFNDEF UNIDACPRO}
  NexusProps;
{$ELSE}
  NexusPropsUni;
{$ENDIF}

{ TCustomNexusDataSetUpdater }

function TCustomNexusDataSetUpdater.PerformRefreshRecord: boolean;
var
  Bookmark: TBookmark;
  ServerCursor: variant;
begin
  Result := True;

  GetIRecordSet.GetProp(prServerCursor, ServerCursor);

  if ServerCursor then begin
    Bookmark := FDataSet.GetBookmark;
    try
      GetIRecordSet.SetToBookmark(IntPtr(Bookmark)); // ReFetch
    finally
      FDataSet.FreeBookmark(Bookmark);
    end;
    SetRowsAffected(1); // Must be always OK
  end
  else
    Result := inherited PerformRefreshRecord;
end;

function TCustomNexusDataSetUpdater.GetDefaultExpressionValue(DefExpr: string; out Value: variant): boolean;
begin
  Result := True;
  DefExpr := Trim(DefExpr);
  if DefExpr = CURRENT_TIME then
    Value := Now
  else
  if DefExpr = EMPTY_STRING then
    Value := ''
  else
    Result := inherited GetDefaultExpressionValue(DefExpr, Value);
end;

{ TCustomNexusDataSetService }

procedure TCustomNexusDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomNexusDataSetUpdater.Create(Self));
end;

procedure TCustomNexusDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomNexusSQLGenerator.Create(TDASQLGeneratorService));
end;

procedure TCustomNexusDataSetService.SetDataSetUpdater(Value: TDataSetUpdater);
begin
  inherited;

  FUpdater := TCustomNexusDataSetUpdater(Value);
end;

function TCustomNexusDataSetService.NeedParamValuesOnPrepare: boolean;
begin
  Result := True;
end;

function TCustomNexusDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

procedure TCustomNexusDataSetService.FillFieldsDefaultValues;
var
  MetaData: TDAMetaData;
  FieldDesc: TFieldDesc;
  NameField, DefField: TField;
  Field: TField;
begin
  if GetIRecordSet.UpdatingTableInfo = nil then
    Exit;

  MetaData := UsedConnection.CreateMetaData;
  try
    MetaData.MetaDataKind := 'Columns';
    MetaData.Restrictions.Text := 'table_name=' + GetIRecordSet.UpdatingTableInfo.TableName;
    MetaData.Open;
    NameField := MetaData.FieldByName('COLUMN_NAME');
    DefField := MetaData.FieldByName('DEFAULT_VALUE');
    while not MetaData.Eof do begin
      FieldDesc := GetIRecordSet.FindField(NameField.AsString);
      if FieldDesc <> nil then begin
        Field := FDataSet.GetField(FieldDesc);
        if (Field <> nil) and not DefField.IsNull then
          Field.DefaultExpression := DefField.AsString;
      end;

      MetaData.Next;
    end;
  finally
    MetaData.Free;
  end;
end;

function TCustomNexusDataSetService.GetRecCount: integer;
var
  St: string;
  UQ: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
  SelectPos, FromPos: integer;
  Parser: TNexusParser;
begin
  St := FDataSet.FinalSQL;
  St := _SetOrderBy(St, '', TNexusParser);
  Parser := TNexusParser.Create(St);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      if Parser.ToLexem(lxFROM) <> lcEnd then begin
        FromPos := Parser.CurrPos;
        St := Copy(St, 1, SelectPos) + ' COUNT(*)' + Copy(St, FromPos - 4 {length('FROM')}, MaxInt);
      end;
    end;
  finally
    Parser.Free;
  end;

  if St = '' then begin
    Result := 0;
    Exit;
  end;

  FUpdater.CheckUpdateQuery(stCustom);
  UQ := FUpdater.UpdateQuery as TCustomDADataSet;
  UQ.SQL.Text := St;
  UQ.Macros.Assign(FDataSet.Macros);
  UQ.Params.Assign(FDataSet.Params);

  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
  if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
    MonitorClass.SQLExecute(FDataSet, St, UQ.Params, 'Get RecCount', MessageID, True);

  UQ.Execute;
  Result := UQ.Fields[0].AsInteger;

  if not TDBAccessUtils.GetLockDebug(FDataSet) and (MonitorClass.HasMonitor or FDataSet.Debug) then
    MonitorClass.SQLExecute(FDataSet, St, UQ.Params, 'Get RecCount', MessageID, False);
end;

procedure TCustomNexusDataSetService.InitCursor;
var
  ServerCursor: Variant;
begin
  inherited;

  GetIRecordSet.GetProp(prServerCursor, ServerCursor);
  SetNeedAddRef(ServerCursor);
end;

function TCustomNexusDataSetService.GetDBKeyList(const TableName, IndexName: string): string;
var
  Info: TSQLObjectInfo;
  Database: string;
  v: Variant;
begin
  Result := '';

  if (GetICommand <> nil) and (GetICommand.GetConnection <> nil) then begin
    GetICommand.GetConnection.GetProp(prDatabase, v);
    Database := VarToStr(v);
    TDBAccessUtils.GetSQLInfo(FDataSet).SplitObjectName(TableName, Info);
    if (Info.Schema <> '') and not AnsiSameText(Info.Schema, Database) then
      Exit;
  end;

  try
    Result := inherited GetDBKeyList(TableName, IndexName);
  except
  end;
end;

{ TNexusScriptProcessor }

function TNexusScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TNexusParser;
end;

{ TCustomNexusFieldTypeMap }

class function TCustomNexusFieldTypeMap.GetFieldType(DataType: Word): TFieldType;
begin
  case DataType of
    dtInt8:
      Result := {$IFDEF VER14P}ftShortint{$ELSE}ftSmallint{$ENDIF};
  else
    Result := inherited GetFieldType(DataType);
  end;
end;

class function TCustomNexusFieldTypeMap.GetDataType(FieldType: TFieldType; SubDataType: Word = 0): integer;
begin
  case FieldType of
    ftBCD:
      Result := dtFMTBCD;
    ftGraphic:
      Result := dtBlob;
  else
    Result := inherited GetDataType(FieldType, SubDataType);
  end;
end;

{ TCustomNexusDumpProcessor }

function TCustomNexusDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  Result.Options.QueryRecCount := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

function TCustomNexusDumpProcessor.GetFieldValueForDump(Field: TField): string;
var
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ENDIF}
  sb: AnsiStringBuilder;
  s: AnsiString;
  i: integer;
  dt: TDateTime;
  v: Variant;
begin
  if Field.IsNull then
    Result := 'NULL'
  else begin
    case Field.DataType of
      ftBoolean, ftCurrency {$IFDEF VER14P}, ftLongWord, ftShortint, ftByte{$ENDIF}:
        Result := Field.AsString;
      ftDate: begin
        dt := Field.AsDateTime;
      {$IFDEF VER7P}
        FmtSet.DateSeparator := '-';
        Result := 'DATE''' + FormatDateTime('yyyy-mm-dd', dt, FmtSet) + '''';
      {$ELSE}
        Result := 'DATE''' + FormatDateTime('yyyy-mm-dd', dt) + '''';
        if DateSeparator <> '-' then
          Result := StringReplace(Result, DateSeparator, '-', [rfReplaceAll]);
      {$ENDIF}
      end;
      ftTime: begin
        dt := Field.AsDateTime;
      {$IFDEF VER7P}
        FmtSet.TimeSeparator := ':';
        Result := 'TIME''' + FormatDateTime('hh:nn:ss', dt, FmtSet) + '''';
      {$ELSE}
        Result := 'TIME''' + FormatDateTime('hh:nn:ss', dt) + '''';
        if TimeSeparator <> ':' then
          Result := StringReplace(Result, TimeSeparator, ':', [rfReplaceAll]);
      {$ENDIF}
      end;
      ftDateTime: begin
        dt := Field.AsDateTime;
        if dt < 32874 {01.01.1990} then
          dt := System.Int(dt) - Frac(dt) - 2.0;
      {$IFDEF VER7P}
        FmtSet.DateSeparator := '-';
        FmtSet.TimeSeparator := ':';
        Result := 'TIMESTAMP''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dt, FmtSet) + '''';
      {$ELSE}
        Result := FormatDateTime('yyyy-mm-dd', dt);
        if DateSeparator <> '-' then
          Result := StringReplace(Result, DateSeparator, '-', [rfReplaceAll]);

        s := FormatDateTime('hh:nn:ss', dt);
        if TimeSeparator <> ':' then
          s := StringReplace(s, TimeSeparator, ':', [rfReplaceAll]);

        Result := 'TIMESTAMP''' + Result + ' ' + s + '''';
      {$ENDIF}
      end;
      ftGuid:
        Result := 'GUID''' + Field.AsString + '''';
      ftBlob: begin
        s := AnsiString(Field.AsString);
        sb := AnsiStringBuilder.Create(Length(s) * 2 + 3);
        try
          sb.Append('X''');
          for i := 1 to Length(s) do
            sb.Append(AnsiString(IntToHex(byte(s[i]), 2)));
          sb.Append('''');
          Result := string(sb.ToString);
        finally
          sb.Free;
        end;
      end;
      ftBytes, ftVarBytes: begin
        v := Field.Value;
        sb := AnsiStringBuilder.Create(Length(v) * 2 + 3);
        try
          sb.Append('X''');
          for i := VarArrayLowBound(v, 1) to VarArrayHighBound(v, 1) do
            sb.Append(AnsiString(IntToHex(byte(v[i]), 2)));
          sb.Append('''');
          Result := string(sb.ToString);
        finally
          sb.Free;
        end;
      end;
    else
      Result := inherited GetFieldValueForDump(Field);
    end;
  end;
end;

{ TNexusServerEnumerator }

procedure TNexusServerEnumerator.GetServerList(List: TStrings);
{$IFNDEF NEXUS_EMBEDDED}
var
  WinsockTransport: TnxWinsockTransport;
{$ENDIF}
begin
  List.Clear;
{$IFNDEF NEXUS_EMBEDDED}
  WinsockTransport := TnxWinsockTransport.Create(nil);
  try
    WinsockTransport.GetServerNames(List, 1000);
  finally
    WinsockTransport.Free;
  end;
{$ENDIF}  
end;

{$ENDIF} // DUMMY

end.
