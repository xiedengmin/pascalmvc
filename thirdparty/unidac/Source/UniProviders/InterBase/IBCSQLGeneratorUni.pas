{$IFNDEF CLR}
{$I IbDac.inc}
unit IBCSQLGeneratorUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, Variants,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRAccess, DASQLGenerator;

type
  _TGeneratorMode = (_gmInsert, _gmPost);

  TCustomIBCSQLGenerator = class(TDASQLGenerator)
  private
    FGeneratorMode: _TGeneratorMode;
    FGeneratorStep: integer;

  protected
    function FieldModified(FieldDesc: TCRFieldDesc): boolean; override;
    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    function GenerateIndexName(const Name: string): string; override;

  public
    constructor Create(ServiceClass: TSQLGeneratorServiceClass); override;

    function DecodeFieldIndex(const FieldName: string): integer; override;
    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
    function GenerateSelectValues(const ValuesList: string): string; override;
    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateSmartFetchMetaInfoSQL: string; override;

    function NormalizeStrValue(const Value: string): string;
    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean; override;

    property GeneratorMode: _TGeneratorMode read FGeneratorMode write FGeneratorMode;
    property GeneratorStep: integer read FGeneratorStep write FGeneratorStep;
  end;

implementation

uses
  SysUtils, Classes, DAConsts, MemData, CRParser,
{$IFNDEF UNIDACPRO}
  IBCProps, IBCCall, IBCClasses, IBCArray, IBCParser;
{$ELSE}
  IBCPropsUni, IBCCallUni, IBCClassesUni, IBCArrayUni, IBCParserUni;
{$ENDIF}

{ TCustomIBCSQLGenerator }

constructor TCustomIBCSQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited;

  FGeneratorMode := _gmPost;
  FGeneratorStep := 1;
end;

function TCustomIBCSQLGenerator.FieldModified(FieldDesc: TCRFieldDesc): boolean;
begin
  if (FieldDesc.DataType = dtArray) and (FService.GetFieldObject(FieldDesc) <> nil) then
    Result := TCustomIBCArray(FService.GetFieldObject(FieldDesc)).Modified
  else
    Result := inherited FieldModified(FieldDesc);
end;

function TCustomIBCSQLGenerator.NormalizeStrValue(const Value: string): string;
var
  Connection: TGDSConnection;
  SQLDialect: variant;
begin
  Connection := TGDSConnection(GetIConnection);
  if Connection <> nil then
    Connection.GetProp(prSQLDialect, SQLDialect)
  else
    SQLDialect := 1;

  Result := Trim(Value);
  if SQLDialect = 3 then
    Result := IBCSQLInfo.NormalizeName(Value, QuoteNames)
  else
    Result := UpperCase(Result);
end;

function TCustomIBCSQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean;
begin
  Result := FieldIsNull(FieldDesc, OldValue, FIRecordSet, FService.GetOldRecBuf, FService.GetNewRecBuf);
end;

function TCustomIBCSQLGenerator.GenerateIndexName(const Name: string): string;
begin
  Result := 'IBC$' + Name;
end;

function TCustomIBCSQLGenerator.DecodeFieldIndex(const FieldName: string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 5) and (FieldName[1] = 'I') and
    (FieldName[2] = 'B') and (FieldName[3] = 'C') and (FieldName[4] = '$') then begin
    Val(Copy(FieldName, 5, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

procedure TCustomIBCSQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);

var
  FieldDesc: TCRFieldDesc;
  GDSConnection: TGDSConnection;

  procedure GenerateLockCondition;
  var
    AllFields: TKeyAndDataFields;
    i: integer;
  begin
    //LockSQL where clause should contains all fields to check that there are no changes in locked record
    if FDesignMode then  // Design-Time generation we should include key fields only (IS NULL issue)
      AllFields.DataFieldDescs := KeyAndDataFields.KeyFieldDescs
    else
      AllFields.DataFieldDescs := KeyAndDataFields.DataFieldDescs; //in Run-Time include all field (IS NULL issue)
    //Include ReadOnly Key Fields
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      if KeyAndDataFields.KeyFieldDescs[i].ReadOnly then begin//This field is not included in DataFields
        SetLength(AllFields.DataFieldDescs, Length(AllFields.DataFieldDescs) + 1);
        AllFields.DataFieldDescs[High(AllFields.DataFieldDescs)] := KeyAndDataFields.KeyFieldDescs[i];
      end;
    SetLength(AllFields.KeyFieldDescs, 0);   //we should use DataFields to perform IsLargeDataTypeUsed check
    GenerateConditions(ParamsInfo, FCondSB, _stUpdate, AllFields);
  end;

begin
  GDSConnection := TGDSConnection(GetIConnection);
  if (GDSConnection <> nil) and
    GDSConnection.IsFBServer and ((GDSConnection.GetMajorServerVersion > 1) or
    ((GDSConnection.GetMajorServerVersion = 1) and (GDSConnection.GetMinorServerVersion >= 5))) then begin // FB1.5 or higher
    FHeaderSB.Append('SELECT NULL FROM ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
    FHeaderSB.Append(DALineSeparator + 'WHERE'  + DALineSeparator);
    GenerateLockCondition; // FCondSB
    FFooterSB.Append(DALineSeparator + 'FOR UPDATE WITH LOCK');
  end
  else
    if High(KeyAndDataFields.DataFieldDescs) > 0 then begin
      FHeaderSB.Append('UPDATE ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
      FHeaderSB.Append(DALineSeparator + 'SET' + DALineSeparator + '  ');

      FieldDesc := TCRFieldDesc(KeyAndDataFields.DataFieldDescs[0]);

      FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
      FFldSB.Append(' = ');
      AddParam(ParamsInfo, FFldSB, FieldDesc, _stLock, pdUnknown, Index);

      FMiddleSB.Append(DALineSeparator + 'WHERE' + DALineSeparator + '  ');
      GenerateLockCondition; // FCondSB
    end;
end;

procedure TCustomIBCSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  GDSConnection: TGDSConnection;
  FieldDesc: TCRFieldDesc;
  IsGenerated: boolean;
  UpdateCommand: TCRCommand;

  ParamInfo: TDAParamInfo;
  ReturnSB: StringBuilder;
  FieldName: string;

  procedure AddFieldToReturning(FieldDesc: TCRFieldDesc);
  begin
    if ReturnSB.Length > 0 then
      ReturnSB.Append(', ');
    FieldName := FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames);
    ReturnSB.Append(FieldName);

    ParamInfo := TDAParamInfo(ParamsInfo.Add); //Create Returning params
    ParamInfo.FieldDesc := FieldDesc;
    ParamInfo.Old := False;
    ParamInfo.ParamName := QuoteName('RET_' + FieldDesc.Name);
    ParamInfo.ParamType := pdOutput;
  end;

begin
  UpdateCommand := FService.GetUpdateCommand;
  if UpdateCommand <> nil then
    UpdateCommand.SetProp(prParsedSQLType, Variant(SQL_UNKNOWN));

  GDSConnection := TGDSConnection(GetIConnection);
  if (GDSConnection <> nil) and
    GDSConnection.IsFBServer and      //if FB2.0 or higher and
    (GDSConnection.GetMajorServerVersion >= 2) and
    ((FIRecordSet.IdentityField <> nil) or
    //(DMLRefresh or generator value insertion at post time)
    FDMLRefresh or
    (FIRecordSet.KeyGeneratorField <> nil) and (FGeneratorMode = _gmPost))
  then begin

    //generator value insertion at post time will be used
    IsGenerated := (FIRecordSet.KeyGeneratorField <> nil) and (FGeneratorMode = _gmPost);

    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
      FieldDesc := KeyAndDataFields.DataFieldDescs[i];

      if IsGenerated and (FieldDesc = KeyAndDataFields.KeyFieldDescs[0]) and
        FieldIsNull(FieldDesc, False)
      then begin //Generator field
        if FFldSB.Length > 0 then begin
          FFldSB.Append(', ');
          FFldParamSB.Append(', ');
        end;

        FFldParamSB.Append('NEXT VALUE FOR ' + NormalizeStrValue(FKeySequence));
        FieldName := FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames);
        FFldSB.Append(FieldName); // Do not call AddFieldToInsertSQL cause of FFldParamSB modification
      end
      else
        if not ModifiedFieldsOnly or not FieldIsNull(FieldDesc, False) then
          AddFieldToInsertSQL(ParamsInfo, FieldDesc, Index);
    end;

    if FFldSB.Length > 0 then begin
      FHeaderSB.Append('INSERT INTO ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
      FHeaderSB.Append(DALineSeparator + '  (');
      // Append FFldSB
      FMiddleSB.Append(')' + DALineSeparator + 'VALUES' + DALineSeparator + '  (');
      // Append FFldParamSB
      FFooterSB.Append(')');
    end
    else
      FHeaderSB.Append(Format('INSERT INTO %s () VALUES ()', [
        SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames)]));

    ReturnSB := StringBuilder.Create(100);
    try
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if (FDMLRefresh and not (FieldDesc.DataType in [dtMemo, dtWideMemo, dtBlob])) or
          (IsGenerated and (FieldDesc = KeyAndDataFields.KeyFieldDescs[0]))
        then
          AddFieldToReturning(FieldDesc);
      end;

      if FIRecordSet.IdentityField <> nil then
        AddFieldToReturning(FIRecordSet.IdentityField);

      if ReturnSB.Length > 0 then begin
        FFooterSB.Append(DALineSeparator +  'RETURNING ' + DALineSeparator + '  ');
        FFooterSB.Append(ReturnSB);
        if UpdateCommand <> nil then
          UpdateCommand.SetProp(prParsedSQLType, Variant(SQL_INSERT));
      end;
    finally
      ReturnSB.Free;
    end;
  end
  else
    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, False, Index);
  end;
end;

function TCustomIBCSQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  AOrderFields: string;
  StartPos, i: integer;
begin
  Result := 'SELECT * FROM ' + NormalizeStrValue(TableName) + DALineSeparator;

  AOrderFields := Trim (OrderFields);
  if AOrderFields <> '' then begin
    StartPos := 1;
    Result := Result + 'ORDER BY ';

    for i := 1 to Length(AOrderFields) do
      if (AOrderFields[i] = ';') or (AOrderFields[i] = ',') then begin
          Result := Result + Copy(AOrderFields, StartPos, i - StartPos);
          Result := Result + ',';
          StartPos := i + 1;
      end
      else
      if (i = Length(AOrderFields)) then
          Result := Result + Copy(AOrderFields, StartPos, i - StartPos + 1);
  end;
  Result := Result + DALineSeparator;
end;

function TCustomIBCSQLGenerator.GenerateSelectValues(const ValuesList: string): string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM RDB$DATABASE';
end;

function TCustomIBCSQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean): string;
var
  Con: TGDSConnection;
  Parser: TIBCParser;
  SelectPos, FromPos, DelimiterPos, Code: integer;
  s, Lexem: string;
  CanOptimize: boolean;

  function PlanWithOrderExists(const SQL: string): boolean;
  var
    Parser: TIBCParser;
    Code, BracketsCount: integer;
    Lexem: string;
  begin
    Result := False;
    Parser := TIBCParser.Create(SQL);
    Parser.OmitBlank := True;
    Parser.OmitComment := True;
    try
      repeat
        Code := Parser.GetNext(Lexem);
        if Code = lxPLAN then begin
          Parser.GetNext(Lexem); // "("
          BracketsCount := 1;
          repeat
            Code := Parser.GetNext(Lexem);
            Result := Code = lxORDER;
            if Result then
              Break;
            if Code = lxLeftBracket then
              Inc(BracketsCount)
            else if Code = lxRightBracket then
              Dec(BracketsCount);
          until (Code = lxSemicolon) or (Code = lcEnd) or (BracketsCount = 0);
        end;
        if Result then
          Break;
      until (Code = lxSemicolon) or (Code = lcEnd);
    finally
      Parser.Free;
    end;
  end;

begin
  Result := '';
  Clear;

  if UseBaseSQL then
    s := FService.BaseSQL
  else
    s := FService.FinalSQL;
  if not PlanWithOrderExists(s) then
    s := SQLSetOrderBy(s, '');

  Con := TGDSConnection(GetIConnection);
  Parser := TIBCParser.Create(s);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      Code := Parser.ToLexem(lxFROM, True);

      if Code <> lcEnd then begin
        FromPos := Parser.CurrPos;

        if Con.IsFBServer and (Con.GetMajorServerVersion >= 2) then begin
          CanOptimize := True;
          repeat
            Code := Parser.GetNext(Lexem);
            if (Code = lxUNION) or (Code = lxORDER) or (Code = lxGROUP) then
              CanOptimize := False;
          until (Code = lxSemicolon) or (Code = lcEnd);

          if Code = lxSemicolon then // ';'
            DelimiterPos := Parser.CurrPos
          else
            DelimiterPos := MaxInt;

          if CanOptimize then
            s := 'SELECT COUNT(*) FROM (' + DALineSeparator +
              Copy(s, 1, SelectPos) + ' 1 AS C ' +
              Copy(s, FromPos - 4 {length('FROM')}, DelimiterPos - FromPos + 4) + DALineSeparator + ') q' +
              Copy(s, DelimiterPos, MaxInt)
          else
            s := Copy(s, 1, SelectPos) + ' COUNT(*) FROM (SELECT ' +
              Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ') q' +
              Copy(s, DelimiterPos, MaxInt);
        end
        else
          s := Format('%s COUNT(*)%s', [Copy(s, 1, SelectPos), Copy(s, FromPos - 4 {length('FROM')}, MaxInt)]);

        FHeaderSB.Append(s);
        Result := s;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TCustomIBCSQLGenerator.GenerateSmartFetchMetaInfoSQL: string;
var
  Parser: TSQLParser;
  SelectPos: integer;
  FirstPos: integer;
  LastPos: integer;
  Code: integer;
  StLex: string;
  BracketCount: integer;
begin
  Result := FIRecordSet.OriginalSQL;
  Parser := TIBCParser.Create(Result);
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      Code := Parser.GetNext(StLex);
      repeat
        if Code = lxFIRST then begin
          FirstPos := Parser.CurrPos + 1;
          Code := Parser.GetNext(StLex);
          LastPos := Parser.CurrPos;
          if Code = lcEnd then
            Exit;

          if Code = lxLeftBracket then begin
            BracketCount := 1;
            repeat
              Code := Parser.GetNext(StLex);
              if Code = lcEnd then
                Exit;

              LastPos := Parser.CurrPos;
              if Code = lxLeftBracket then
                inc(BracketCount)
              else if Code = lxRightBracket then
                dec(BracketCount);

              if BracketCount = 0 then
                Break;
            until Code = lcEnd;
          end;
          Result := Copy(Result, 1, FirstPos) + '0' + Copy(Result, LastPos + 1, Length(Result));
          Break;
        end
        else if Code = lxFROM then begin
          if TGDSConnection(FICommand.GetConnection).IsFBServer then
            Insert(' FIRST 0', Result, SelectPos + 1)
          else
            Result := Result + ' ROWS 0';
          Break;
        end;
        Code := Parser.GetNext(StLex);
      until Code = lcEnd;
    end;
  finally
    Parser.Free;
  end;
end;

end.
