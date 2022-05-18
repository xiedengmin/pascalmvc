{$IFNDEF CLR}
{$I Odac.inc}
unit OraSQLGeneratorUni;
{$ENDIF}

interface

uses
  Classes,
  CLRClasses, CRTypes, CRAccess, MemData, DASQLGenerator;

type
  // must be sync with types in Ora
  _TSequenceMode = (_smInsert, _smPost);

  TCustomOraSQLGenerator = class(TDASQLGenerator)
  private
    FSequenceMode: _TSequenceMode;

    function GetTemporaryLobUpdate: boolean;
  protected
    FSeqReturning: boolean;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FSeqFieldDesc: TCRFieldDesc;
    FReturnSB,
    FIntoSB: StringBuilder;

    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;

    function GenerateIndexName(const Name: string): string; override;
    function MaxIdentLength: integer; override;

    function IsSubstituteParamName: boolean; override;
    procedure AddParam(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder;
      FieldDesc: TCRFieldDesc;
      const StatementType: _TStatementType;
      ParamType: TParamDirection;
      Index: integer = -1;
      Old: boolean = False;
      AllowDuplicates: boolean = True); override;

    procedure AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
      FieldDesc: TCRFieldDesc;
      const Index: integer = -1); override;
    procedure AddFieldToUpdateSQL(ParamsInfo: TDAParamsInfo;
      FieldDesc: TCRFieldDesc;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;

    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

  public
    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
    function GenerateSelectValues(const ValuesList: string): string; override;
    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateEmptyTableSQL(const TableName: string): string; override;
    function GenerateSmartFetchMetaInfoSQL: string; override;

    property SequenceMode: _TSequenceMode read FSequenceMode write FSequenceMode;
  end;

implementation

uses
  SysUtils, DAConsts, CRProps, CRParser,
{$IFNDEF UNIDACPRO}
  OraProps, OraCall, OraClasses, OraParser;
{$ELSE}
  OraPropsUni, OraCallUni, OraClassesUni, OraParserUni;
{$ENDIF}

{ TCustomOraSQLGenerator }

function TCustomOraSQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean;
  Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
begin
  if FieldDesc = FSeqFieldDesc then
    Result := False
  else
    Result := inherited FieldIsNull(FieldDesc, OldValue, Data, OldRecBuf, NewRecBuf);
end;

function TCustomOraSQLGenerator.GenerateIndexName(const Name: string): string;
begin
  Result := '"' + '_' + Name + '"';
end;

function TCustomOraSQLGenerator.MaxIdentLength: integer;
begin
  Result := 30;
end;

function TCustomOraSQLGenerator.IsSubstituteParamName: boolean;
begin
  Result := False;
end;

procedure TCustomOraSQLGenerator.AddParam(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder;
  FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType;
  ParamType: TParamDirection;
  Index: integer = -1; // only for CachedUpdates
  Old: boolean = False;
  AllowDuplicates: boolean = True);
begin
  if FieldDesc = FIRecordSet.IdentityField then
    Old := True;

  if FieldDesc.DataType in [dtOraBlob, dtOraClob, dtWideOraClob] then
    ParamType := pdInput;

  inherited AddParam(ParamsInfo, SB, FieldDesc, StatementType, ParamType, Index, Old, AllowDuplicates);
end;

function TCustomOraSQLGenerator.GetTemporaryLobUpdate: boolean;
var
  Value: Variant;
begin
  Assert(FICommand <> nil);
  if FIUpdateRecordSet <> nil then
    FIUpdateRecordSet.GetCommand.GetProp(prTemporaryLobUpdate, Value)
  else
    FICommand.GetProp(prTemporaryLobUpdate, Value);
  Result := Value;
end;

procedure TCustomOraSQLGenerator.AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
  FieldDesc: TCRFieldDesc;
  const Index: integer = -1);

  procedure AddComma;
  begin
    if FFldSB.Length > 0 then begin
      FFldSB.Append(', ');
      FFldParamSB.Append(', ');
    end;
  end;

begin
  if (FieldDesc.DataType = dtOraBlob) and not GetTemporaryLobUpdate then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FFldParamSB.Append('EMPTY_BLOB()');
  end
  else
  if (FieldDesc.DataType in [dtOraClob, dtWideOraClob]) and not GetTemporaryLobUpdate then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FFldParamSB.Append('EMPTY_CLOB()');
  end
  else
  if FSeqReturning and (FieldDesc = FSeqFieldDesc) then begin
    AddComma;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FFldParamSB.Append(FKeySequence + '.NEXTVAL');
  end
  else
    inherited;
end;

procedure TCustomOraSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  Sequenced: boolean;
  FieldDesc: TCRFieldDesc;
  LobReturning: boolean;
  i: integer;

  ReturnSB: StringBuilder;
  IntoSB: StringBuilder;
begin
  // we should not override user's value by the sequenced value
  Sequenced := (FSequenceMode = _smPost) and
    (FIRecordSet.KeyGeneratorField <> nil) and
    FieldIsNull(FIRecordSet.KeyGeneratorField, False);

  if Sequenced then begin
    with TOCIConnection(GetIConnection) do
      FSeqReturning := (GetOCICallStyle = OCI80) and (GetOracleVersion >= 8000);

    FSeqFieldDesc := KeyAndDataFields.KeyFieldDescs[0];
    if not FSeqReturning then begin
      FHeaderSB.Append('begin'#$D#$A'  SELECT ');
      FHeaderSB.Append(FKeySequence);
      FHeaderSB.Append('.NEXTVAL INTO ');
      AddParam(ParamsInfo, FHeaderSB, FSeqFieldDesc, _stInsert, pdInputOutput, Index);
      FHeaderSB.Append(' FROM SYS.Dual;' + #$D#$A#$D#$A + '  ');
    end;
  end;

  ReturnSB := StringBuilder.Create(100);
  IntoSB := StringBuilder.Create(100);
  try
    LobReturning := False;
    if not GetTemporaryLobUpdate then
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
        if KeyAndDataFields.DataFieldDescs[i].DataType in [dtOraBlob, dtOraClob, dtWideOraClob] then begin
          LobReturning := True;
          Break;
        end;

    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

    if FFldSB.Length = 0 then begin
      Clear;
      inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, False, Index);
    end;

    if FDMLRefresh or LobReturning or Sequenced then begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob])) or
          (LobReturning and (FieldDesc.DataType in [dtOraBlob, dtOraClob, dtWideOraClob]) and
          (not ModifiedFieldsOnly or not FieldIsNull(FieldDesc, False))) or
          ((FSeqFieldDesc = FieldDesc) and FSeqReturning)
        then begin
          if ReturnSB.Length > 0 then begin
            ReturnSB.Append(', ');
            IntoSB.Append(', ');
          end;
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
          AddParam(ParamsInfo, IntoSB, FieldDesc, _stInsert, pdInputOutput, Index, False, False);
        end;
      end;
    end;

    if ReturnSB.Length > 0 then begin
      FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
      FFooterSB.Append(ReturnSB);
    end;

    if IntoSB.Length > 0 then begin
      FFooterSB.Append(#$D#$A'INTO'#$D#$A'  ');
      FFooterSB.Append(IntoSB);
    end;

    if Sequenced and not FSeqReturning then
      FFooterSB.Append(';'#$D#$A'end;');
  finally
    FSeqFieldDesc := nil;
    FSeqReturning := False;
    ReturnSB.Free;
    IntoSB.Free;
  end;
end;

procedure TCustomOraSQLGenerator.AddFieldToUpdateSQL(ParamsInfo: TDAParamsInfo;
  FieldDesc: TCRFieldDesc;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
begin
  if (FieldDesc.DataType in [dtOraBlob, dtOraClob, dtWideOraClob]) and not GetTemporaryLobUpdate then begin
    if FFldSB.Length > 0 then
      FFldSB.Append(', ');

    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FFldSB.Append('=');

    if ModifiedFieldsOnly and FieldIsNull(FieldDesc, False) then
      FFldSB.Append('NULL')
    else begin
      if FieldDesc.DataType = dtOraBlob then
        FFldSB.Append('EMPTY_BLOB()')
      else
        FFldSB.Append('EMPTY_CLOB()');
    end;
  end
  else
    inherited;
end;

procedure TCustomOraSQLGenerator.GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  LobReturning: boolean;
  FieldDesc: TCRFieldDesc;

  ReturnSB: StringBuilder;
  IntoSB: StringBuilder;
begin
  inherited GenerateUpdateSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then
    Exit;

  LobReturning := False;
  if not GetTemporaryLobUpdate then
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      if KeyAndDataFields.DataFieldDescs[i].DataType in [dtOraBlob, dtOraClob, dtWideOraClob] then begin
        LobReturning := True;
        Break;
      end;

  ReturnSB := StringBuilder.Create(100);
  IntoSB := StringBuilder.Create(100);
  try
    if FDMLRefresh or LobReturning then begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtOraBlob, dtOraClob, dtWideOraClob])) or
          (LobReturning and (FieldDesc.DataType in [dtOraBlob,dtOraClob,dtWideOraClob]) and
          (not ModifiedFieldsOnly or FieldModified(FieldDesc)))
        then begin
          if ReturnSB.Length > 0 then begin
            ReturnSB.Append(', ');
            IntoSB.Append(', ');
          end;
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
          AddParam(ParamsInfo, IntoSB, FieldDesc, _stUpdate, pdInputOutput, Index, False, False);
        end;
      end;

      if ReturnSB.Length <> 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;

      if IntoSB.Length > 0 then
        FFooterSB.Append(#$D#$A'INTO'#$D#$A'  ');
        FFooterSB.Append(IntoSB);
      end;
  finally
    ReturnSB.Free;
    IntoSB.Free;
  end;
end;

procedure TCustomOraSQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  GenerateRefreshSQLSelectPart(KeyAndDataFields);
  FMiddleSB.Append(' FROM ');
  FMiddleSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
  FMiddleSB.Append(#$D#$A'WHERE'#$D#$A'  ');
  GenerateConditions(ParamsInfo, FCondSB, _stLock, KeyAndDataFields, Index);
  FFooterSB.Append(#$D#$A'FOR UPDATE NOWAIT');
end;

function TCustomOraSQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  QuotedTableName, St1: string;
  i: integer;
  v: variant;
  KeyFields: string;
  ReadOnly: boolean;
begin
  QuotedTableName := SQLInfo.NormalizeName(TableName, QuoteNames);

  FIRecordSet.GetProp(prKeyFields, v);
  KeyFields := v;
  FIRecordSet.GetProp(prReadOnly, v);
  ReadOnly := v;

  if (KeyFields = '') and not ReadOnly then begin
    KeyFields := FService.GetDBKeyList(TableName, '');
    if KeyFields <> '' then
      FIRecordSet.SetProp(prKeyFields, KeyFields);
  end;

  if not ReadOnly and ((KeyFields = '') or (UpperCase(KeyFields) = 'ROWID'))  then
    Result := 'SELECT T.RowId, T.*'#13#10'FROM ' + QuotedTableName + ' T'#13#10
  else
    Result := 'SELECT T.*'#13#10'FROM ' + QuotedTableName + ' T'#13#10;

  if OrderFields <> '' then begin
    St1 := OrderFields;

    for i := 1 to Length(St1) do
      if St1[i] = ';' then
        St1[i] := ',';

    Result := Result + 'ORDER BY ' + St1 + #13#10;
  end;
end;

function TCustomOraSQLGenerator.GenerateSelectValues(const ValuesList: string): string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM SYS.Dual';
end;

function TCustomOraSQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean): string;
var
  Parser: TOraParser;
begin
  Clear;
  if UseBaseSQL then
    Result := FService.BaseSQL
  else
    Result := FService.FinalSQL;
  Result := SQLSetOrderBy(Result, '');
  Parser := TOraParser.Create(Result);
  try
    if Parser.ToLexem(lxFOR) <> lcEnd then begin
      Result := copy(Result, 1, Parser.CurrPos - 3);
    end;
  finally
    Parser.Free;
  end;
  Result := 'SELECT Count(*) FROM (' + DALineSeparator + Result + DALineSeparator + ')';
  FHeaderSB.Append(Result);
end;

function TCustomOraSQLGenerator.GenerateEmptyTableSQL(const TableName: string): string;
begin
  Result := 'TRUNCATE TABLE ' + TableName;
end;

function TCustomOraSQLGenerator.GenerateSmartFetchMetaInfoSQL: string;
begin
  Result := FIRecordSet.OriginalSQL;
end;

end.
