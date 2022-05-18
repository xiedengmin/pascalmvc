
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright � 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I LiteDac.inc}
unit LiteServicesUni;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB,
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
  CRTypes, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, CRServerEnumerator, DBAccess, DAScript, DADump, DASQLGenerator,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteClasses, LiteParser, LiteSQLGenerator;
{$ELSE}
  LiteCallUni, LiteClassesUni, LiteParserUni, LiteSQLGeneratorUni;
{$ENDIF}

type
{ TCustomLiteFieldTypeMap }

  TCustomLiteFieldTypeMap = class(TDAFieldTypeMap)
  end;

{ TCustomLiteDataSetUpdater }

  TCustomLiteDataSetUpdater = class(TDADataSetUpdater)
  protected
    // CLR cross-assembly
    procedure CheckUpdateQuery(const StatementType: TStatementType); override;

    function BatchUpdate: boolean; override;
    procedure SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean); override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomLiteDataSetService }

  TCustomLiteDataSetService = class(TDADataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;

    function DetectCanModify: boolean; override;
    procedure InitFieldsOptions; override;
    function GetRecCount: integer; override;

  public
    constructor Create(AOwner: TMemDataSet); override;

    function GetDBKeyList(const TableName, IndexName: string): string; override;
  end;

  TLiteScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
  end;

  TCustomLiteDumpProcessor = class(TDADumpProcessor)
  private
  {$IFDEF VER12P}
    FUseUnicode: boolean;
    FIConnection: TSQLiteConnection;
    FEncoding: Encoding;
  {$ENDIF}
  protected
    procedure Add(const Line: string); overload; override;

    procedure Backup(Query: string); override;
    function CreateQuery: TCustomDADataSet; override;
    function CreateDataQuery: TCustomDADataSet; override;
    function GetFieldValueForDump(Field: TField): string; override;
  end;

  TLiteServerEnumerator = class (TCRServerEnumerator)
  end;

implementation

uses
  CRProps, CRFunctions, DAConsts, DASQLMonitor,
{$IFNDEF UNIDACPRO}
  LiteProps;
{$ELSE}
  LitePropsUni;
{$ENDIF}

{ TCustomLiteDataSetUpdater }

procedure TCustomLiteDataSetUpdater.CheckUpdateQuery(const StatementType: TStatementType);
begin
  inherited;
end;

function TCustomLiteDataSetUpdater.BatchUpdate: boolean;
begin
  Result := False;
end;

procedure TCustomLiteDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean);
var
  DestRecordSet, SourceRecordSet: TCRRecordSet;

  procedure CopyPropR(Prop: integer);
  var
    v: variant;
  begin
    SourceRecordSet.GetProp(Prop, v);
    DestRecordSet.SetProp(Prop, v);
  end;

begin
  CheckIRecordSet; // can be inactive
  SourceRecordSet := GetIRecordSet;
  DestRecordSet := TDBAccessUtils.GetIRecordSet(UpdateQuery as TCustomDADataSet);

//  CopyPropR(prCommandTimeout);

  DestRecordSet.SetProp(prExtendedFieldsInfo, False);
end;

function TCustomLiteDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := TDBAccessUtils.GetLastInsertId(UpdateQuery as TCustomDADataSet);
  Result := True;
end;

{ TCustomLiteDataSetService }

constructor TCustomLiteDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;
end;

procedure TCustomLiteDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomLiteDataSetUpdater.Create(Self));
end;

procedure TCustomLiteDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomLiteSQLGenerator.Create(TDASQLGeneratorService));
end;

function TCustomLiteDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

procedure TCustomLiteDataSetService.InitFieldsOptions;
var
  TableName: string;
  MetaData: TDAMetadata;
  Field: TField;
  FieldDesc: TFieldDesc;
  FieldName: string;
  DefValue: string;
  ExtFieldsInfo: boolean;
  Info: TSQLObjectInfo;
  v: variant;
  IsMetaDataAPIAvailable,
  IdentityFieldExists: Boolean;
  KeyCount: integer;
begin
  inherited;

  GetIRecordSet.GetProp(prExtendedFieldsInfo, v);
  ExtFieldsInfo := v;
  IsMetaDataAPIAvailable := TSQLiteConnection(GetIRecordSet.GetConnection).API.IsMetaDataAPIAvailable;

  if (not FDataSet.Options.DefaultValues and (not ExtFieldsInfo or IsMetaDataAPIAvailable)) or
     (GetIRecordSet.UpdatingTableInfo = nil)
  then
    Exit;

  SQLiteInfo.SplitObjectName(GetIRecordSet.UpdatingTableInfo.TableName, Info);
  TableName := Info.Name;
  IdentityFieldExists := False;
  KeyCount := 0;
  MetaData := TDAMetadata.Create(nil);
  try
    MetaData.Connection := UsedConnection;
    MetaData.MetaDataKind := 'Columns';
    MetaData.Restrictions.Text := 'table_name=' + TableName;
    MetaData.Open;
    while not MetaData.EOF do begin
      FieldName := MetaData.FieldByName('COLUMN_NAME').AsString;
      FieldDesc := GetIRecordSet.FindField(FieldName);
      if FieldDesc = nil then
        FieldDesc := GetIRecordSet.FindField(FieldName);
      if FieldDesc <> nil then
        Field := FDataSet.GetField(FieldDesc)
      else
        Field := nil;
      if (Field <> nil) then begin
        if not IsMetaDataAPIAvailable then begin
        {$IFNDEF DBX_METADATA}
          FieldDesc.IsAutoIncrement := (MetaData.FieldByName('PRIMARY_KEY').AsInteger > 0) and (FieldDesc.DataType = dtInteger);
        {$ELSE}
          FieldDesc.IsAutoIncrement := MetaData.FieldByName('NULLABLE').AsInteger = 0;
        {$ENDIF}
          if FieldDesc.IsAutoIncrement then begin
            if not IdentityFieldExists then begin
            {$IFNDEF FPC}
              Field.AutoGenerateValue := arAutoInc;
            {$ENDIF}
              GetIRecordSet.SetIdentityField(TCRFieldDesc(FieldDesc));
              IdentityFieldExists := True;
            end
            else
              FieldDesc.IsAutoIncrement := False;
            Inc(KeyCount);
          end;
        end;

        if FDataSet.Options.DefaultValues and
          not MetaData.FieldByName('DEFAULT_VALUE').IsNull
        then begin
          DefValue := MetaData.FieldByName('DEFAULT_VALUE').AsString;
          Field.DefaultExpression := DefValue;
        end;
      end;
      MetaData.Next;
    end;

    if not IsMetaDataAPIAvailable and (KeyCount > 1) then begin
      GetIdentityField.IsAutoIncrement := False;
    {$IFNDEF FPC}
      FDataSet.GetField(GetIdentityField).AutoGenerateValue := arNone;
    {$ENDIF}
    end;
  finally
    MetaData.Free;
  end;
end;

function TCustomLiteDataSetService.GetRecCount: integer;
var
  St: string;
  UpdateQuery: TCustomDADataSet;
  MonitorClass: TDASQLMonitorClass;
  MessageID: cardinal;
begin
  Result := 0;
  St := Trim(FDataSet.SQLRecCount.Text);
  if St = '' then
    St := FSQLGenerator.GenerateRecCountSQL;

  TCustomLiteDataSetUpdater(Updater).CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(TCustomLiteDataSetUpdater(Updater).UpdateQuery);
  UpdateQuery.SQL.Text := St;
  UpdateQuery.Params.Assign(FDataSet.Params);

  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

  UpdateQuery.Open;
  if not UpdateQuery.EOF then
    Result := UpdateQuery.Fields[0].AsInteger;

  if MonitorClass.HasMonitor or FDataSet.Debug then
    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
end;

function TCustomLiteDataSetService.GetDBKeyList(const TableName, IndexName: string): string;
var
  SQLInfo: TSQLInfo;
  MetaData: TDAMetaData;
  ConstraintName, ConstraintType, s: string;
  Info: TSQLObjectInfo;
begin
  Result := '';
  BeginConnection;
  try
    SQLInfo := TDBAccessUtils.GetSQLInfo(FDataSet);
    SQLInfo.SplitObjectName(TableName, Info);

    ConstraintName := '';
    MetaData := TDAMetaData.Create(nil);
    try
      MetaData.Connection := UsedConnection;
      TDBAccessUtils.SetTransaction(MetaData, TDBAccessUtils.UsedTransaction(FDataSet)); //TODO:
      MetaData.MetaDataKind := 'constraints';
      MetaData.Restrictions.Text := 'table_name=' + Info.Name;
      MetaData.Open;
      while not MetaData.Eof do begin
        s := VarToStr(MetaData.FieldByName('CONSTRAINT_NAME').Value);
        ConstraintType := VarToStr(MetaData.FieldByName('CONSTRAINT_TYPE').Value);
        if (ConstraintType = 'UNIQUE') then
          ConstraintName := s
        else if ConstraintType = 'PRIMARY KEY' then begin
          ConstraintName := s;
          Break;
        end;
        MetaData.Next;
      end;
      MetaData.Close;

      if ConstraintName <> '' then begin
        MetaData.MetaDataKind := 'constraintcolumns';
        MetaData.Restrictions.Text := 'table_name=' + Info.Name +
          #13#10'constraint_name=' + ConstraintName;
        MetaData.Open;
        while not MetaData.Eof do begin
          if Result <> '' then
            Result := Result + ';';
          Result := Result + VarToStr(MetaData.FieldByName('COLUMN_NAME').Value);
          MetaData.Next;
        end;
        MetaData.Close;
      end;
    finally
      MetaData.Free;
    end;
  finally
    EndConnection;
  end;
end;

{ TLiteScriptProcessor }

function TLiteScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

procedure TLiteScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if Code = lxTRIGGER then
    StatementType := ST_SPECIFIC_SQL;
end;

{ TCustomLiteDumpProcessor }

procedure TCustomLiteDumpProcessor.Add(const Line: string);
{$IFDEF VER12P}
var
  buf: TBytes;
{$ENDIF}
begin
{$IFDEF VER12P}
  if GetStream = nil then
    inherited
  else begin
    buf := FEncoding.GetBytes(Line + #$D#$A);
    GetStream.WriteBuffer(buf[0], Length(buf));
  end;
{$ELSE}
  inherited;
{$ENDIF}
end;

procedure TCustomLiteDumpProcessor.Backup(Query: string);
{$IFDEF VER12P}
const
  Utf8BOM: array[1..3] of byte = ($EF, $BB, $BF);
var
  v: variant;
{$ENDIF}
begin
{$IFDEF VER12P}
  FIConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(GetConnection));
  FIConnection.GetProp(prUseUnicode, v);
  FUseUnicode := v;

  if GetStream <> nil then begin
    if FUseUnicode then begin
      FEncoding := Encoding.UTF8;
      GetStream.WriteBuffer(Utf8BOM, Length(Utf8BOM));
    end
    else
      FEncoding := Encoding.Default;
  end;
{$ENDIF}
  inherited;
end;

function TCustomLiteDumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  Result.ReadOnly := True;
  Result.UniDirectional := True;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

function TCustomLiteDumpProcessor.CreateDataQuery: TCustomDADataSet;
begin
  Result := CreateQuery;
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prDumpData, True);
end;

function TCustomLiteDumpProcessor.GetFieldValueForDump(Field: TField): string;
begin
  Result := Field.AsString;
end;

end.