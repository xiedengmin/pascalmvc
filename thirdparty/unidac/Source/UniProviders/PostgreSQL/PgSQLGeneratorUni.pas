{$IFNDEF CLR}
{$I PgDac.inc}
unit PgSQLGeneratorUni;
{$ENDIF}

interface

uses
  Classes, Variants,
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, DASQLGenerator, MemData, CRAccess;

type
  // must be sync with types in PgAccess
  _TSequenceMode = (_smInsert, _smPost);

  TCustomPgSQLGenerator = class(TDASQLGenerator)
  private
    FLimit: integer;
    FOffset: integer;
    FSequenceMode: _TSequenceMode;
    FKeyGeneratorField: TFieldDesc;
    FSequenceRequestUseReturning: boolean;

  protected
    procedure Clear; override;

    function SubstitutedParamName(ParamsInfo: TDAParamsInfo): string; override;
    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;
    function FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; override;

    procedure AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
      FieldDesc: TCRFieldDesc; const Index: integer = -1); override;

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
    constructor Create(ServiceClass: TSQLGeneratorServiceClass); override;

    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
    function GenerateEmptyTableSQL(const TableName: string): string; override;

    property SequenceMode: _TSequenceMode read FSequenceMode write FSequenceMode;
    property SequenceRequestUseReturning: boolean read FSequenceRequestUseReturning write FSequenceRequestUseReturning;
    property Limit: integer write FLimit;
    property Offset: integer write FOffset;
  end;

implementation

uses
  SysUtils, DAConsts, CRParser,
{$IFNDEF UNIDACPRO}
  PgConsts, PgParser, PgObjects, PgClasses;
{$ELSE}
  PgConstsUni, PgParserUni, PgObjectsUni, PgClassesUni;
{$ENDIF}

{ TCustomPgSQLGenerator }

constructor TCustomPgSQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited;

  FLimit := -1;
  FOffset := 0;
end;

procedure TCustomPgSQLGenerator.Clear;
begin
  inherited;
  FKeyGeneratorField := nil;
end;

function TCustomPgSQLGenerator.SubstitutedParamName(ParamsInfo: TDAParamsInfo): string;
begin
  Result := '$' + IntToStr(ParamsInfo.Count + 1);
end;

function TCustomPgSQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
var
  Blob: TBlob;
begin
  if FieldDesc = FKeyGeneratorField then
    Result := False
  else begin
    Result := inherited FieldIsNull(FieldDesc, OldValue, Data, OldRecBuf, NewRecBuf);

    if Result and (FieldDesc.DataType = dtPgLargeObject) then begin
      if OldValue then
        Blob := Data.GetBlob(FieldDesc, OldRecBuf)
      else
        Blob := Data.GetBlob(FieldDesc, NewRecBuf);

      Result := TPgSQLLargeObject(Blob).OID = 0;
    end;
  end;
end;

function TCustomPgSQLGenerator.FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
var
  Blob: TBlob;
begin
  if (FieldDesc.DataType = dtPgLargeObject) and not InCacheProcessing then begin
    Blob := Data.GetBlob(FieldDesc, NewRecBuf);
    Result := TPgObjectsUtils.GetLargeObjectOIDChanged(TPgSQLLargeObject(Blob));
  end
  else
    Result := False;

  Result := Result or inherited FieldModified(FieldDesc, Data, OldRecBuf, NewRecBuf);
end;

procedure TCustomPgSQLGenerator.AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
  FieldDesc: TCRFieldDesc; const Index: integer = -1);
begin
  if FieldDesc = FKeyGeneratorField then begin
    if FFldSB.Length > 0 then begin
      FFldSB.Append(', ');
      FFldParamSB.Append(', ');
    end;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));

    FFldParamSB.Append(Format('NEXTVAL(%s)', [PgSQLInfo.ToStringConst(FKeySequence)]));
  end
  else
    inherited;
end;

procedure TCustomPgSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  ReturnSB: StringBuilder;
  FieldDesc: TCRFieldDesc;
begin
  if FSequenceRequestUseReturning then
    FKeyGeneratorField := FIRecordSet.KeyGeneratorField
  else
    FKeyGeneratorField := nil;

  inherited;

  if FFldSB.Length = 0 then begin
    Clear;
    FHeaderSB.Append('INSERT INTO ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
    FHeaderSB.Append(' DEFAULT VALUES');
  end;

  if FDMLRefresh or (FKeyGeneratorField <> nil) then begin
    ReturnSB := StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDMLRefresh and
          not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtPgLargeObject])) or
          (FKeyGeneratorField = FieldDesc)
        then begin
          if ReturnSB.Length > 0 then
            ReturnSB.Append(', ');
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
        end;
      end;

      if ReturnSB.Length > 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;
    finally
      ReturnSB.Free;
    end;
  end;
end;

procedure TCustomPgSQLGenerator.GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer = -1);
var
  i: integer;
  ReturnSB: StringBuilder;
  FieldDesc: TCRFieldDesc;
begin
  inherited GenerateUpdateSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then
    Exit;

  if FDMLRefresh then begin
    ReturnSB := StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob, dtPgLargeObject])
        then begin
          if ReturnSB.Length > 0 then
            ReturnSB.Append(', ');
          ReturnSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
        end;
      end;

      if ReturnSB.Length <> 0 then begin
        FFooterSB.Append(#$D#$A'RETURNING'#$D#$A'  ');
        FFooterSB.Append(ReturnSB);
      end;
    finally
      ReturnSB.Free;
    end;
  end;
end;

procedure TCustomPgSQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
  FMiddleSB.Append(#$D#$A'WHERE'#$D#$A'  ');
  GenerateConditions(ParamsInfo, FCondSB, _stLock, KeyAndDataFields, Index);
  FFooterSB.Append(#$D#$A'FOR UPDATE NOWAIT');
end;

function TCustomPgSQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean): string;
var
  Parser: TPgParser;
begin
  Clear;
  if UseBaseSQL then
    Result := FService.BaseSQL
  else
    Result := FService.FinalSQL;
  Result := SQLSetOrderBy(Result, '');
  Parser := TPgParser.Create(Result);
  try
    if Parser.ToLexem(lxFOR) <> lcEnd then begin
      Result := copy(Result, 1, Parser.CurrPos - 3);
    end;
  finally
    Parser.Free;
  end;
  Result := 'SELECT count(*) FROM (' + DALineSeparator + Result + DALineSeparator + ') t';
  FHeaderSB.Append(Result);
end;

function TCustomPgSQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
begin
  Result := inherited GenerateTableSQL(TableName, OrderFields);

  if FLimit >= 0 then
    Result := Result + ' LIMIT ' + IntToStr(FLimit);

  if FOffset > 0 then
    Result := Result + ' OFFSET ' + IntToStr(FOffset);
end;

function TCustomPgSQLGenerator.GenerateEmptyTableSQL(const TableName: string): string;
begin
  Result := 'TRUNCATE TABLE ' + TableName;
end;

end.
