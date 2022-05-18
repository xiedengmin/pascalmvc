{$IFNDEF CLR}
{$I MyDac.inc}
unit MySQLGeneratorUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, Variants,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, DASQLGenerator, MemData, CRAccess;

type
  TLockRecordTypeI = (ilrImmediately, ilrDelayed);
  TLockTypeI = (iltRead, iltReadLocal, iltWrite, iltWriteLowPriority);

  TCustomMySQLGenerator = class(TDASQLGenerator)
  private
    FLimit: integer;
    FOffset: integer;
    FUseHandler: boolean;
    FHandlerIndex: string;
    FCheckRowVersion: boolean;
    FLockRecordType: TLockRecordTypeI; // for GenerateLockSQL

  protected
    procedure AddFieldToCondition(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder; FieldDesc: TCRFieldDesc;
      const StatementType: _TStatementType; const Index: integer = -1); override;
    procedure GenerateConditions(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder; const StatementType: _TStatementType;
      const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1); override;

    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;

  public
    constructor Create(ServiceClass: TSQLGeneratorServiceClass); override;

    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateEmptyTableSQL(const TableName: string): string; override;
    function GenerateSmartFetchMetaInfoSQL: string; override;

    property Limit: integer write FLimit;
    property Offset: integer write FOffset;
    property UseHandler: boolean write FUseHandler;
    property HandlerIndex: string write FHandlerIndex;
    property CheckRowVersion: boolean write FCheckRowVersion;
    property LockRecordType: TLockRecordTypeI write FLockRecordType;
  end;

implementation

uses
  SysUtils, Classes, CRProps, DAConsts, CRParser,
{$IFNDEF UNIDACPRO}
  MyProps, MyConsts, MyCall, MyParser, MyClasses;
{$ELSE}
  MyPropsUni, MyConstsUni, MyCallUni, MyParserUni, MyClassesUni;
{$ENDIF}                                                          

{ TCustomMySQLGenerator }

constructor TCustomMySQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited;

  FLimit := -1;
  FOffset := 0;
end;

procedure TCustomMySQLGenerator.AddFieldToCondition(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder; FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType; const Index: integer = -1);
begin
  Assert(FieldDesc is TMySQLFieldDesc);
  if not Assigned(FieldDesc.TableInfo) then
    Exit;

  if (StatementType = _stRefresh) and
    (TMySQLFieldDesc(FieldDesc).MySQLType = FIELD_TYPE_TIMESTAMP) and
    FieldIsNull(FieldDesc, True) then begin // Not 'IS NULL'. Must be compared with '00000000'
    if SB.Length > 0 then
      SB.Append(' AND ');
    SB.Append(GetActualFieldName(FieldDesc, (StatementType = _stRefresh) and FullRefresh));
    SB.Append(' = ''00000000''');
  end
  else
    inherited;
end;

procedure TCustomMySQLGenerator.GenerateConditions(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder; const StatementType: _TStatementType;
  const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1);

  function GetTimestampField: TCRFieldDesc;
  var
    i: integer;
  begin
    Result := nil;
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      if TMySQLFieldDesc(KeyAndDataFields.DataFieldDescs[i]).IsCurrentTimestamp then
        Result := KeyAndDataFields.DataFieldDescs[i];
  end;

var
  SQL: AnsiStringBuilder;
  TimestampFieldDesc: TCRFieldDesc;
  FldUsed: set of byte;
  TsValue: string;
  i: integer;
begin
  if StatementType = _stRefreshQuick then begin
    TimestampFieldDesc := GetTimestampField;
    if TimestampFieldDesc = nil then
      FService.RaiseError(STimestampFieldRequired);

    SQL := AnsiStringBuilder.Create;
    try
      TMySQLConnection(GetIConnection).AppendValueToSQL(SQL, dtDateTime, TMyTableInfo(TimestampFieldDesc.TableInfo).MaxTimestamp,
        False, False{$IFDEF HAVE_COMPRESS}, False{$ENDIF});
      TsValue := string(SQL.ToString);
    finally
      SQL.Free;
    end;

    FCondSB.Append(GetActualFieldName(TimestampFieldDesc, True) + ' >= ' + TsValue);
  end
  else
    if FCheckRowVersion and (StatementType in [_stUpdate, _stDelete]) then begin
      FldUsed := [];
      if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
        for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do begin
          AddFieldToCondition(ParamsInfo, SB, KeyAndDataFields.KeyFieldDescs[i], StatementType, Index);
          FldUsed := FldUsed + [KeyAndDataFields.KeyFieldDescs[i].FieldNo];
        end;

      TimestampFieldDesc := GetTimestampField;
      // TimestampField may be nil and TimestampField.Value may be unassigned too
      if (TimestampFieldDesc <> nil) and not FieldIsNull(TimestampFieldDesc, False) then
        AddFieldToCondition(ParamsInfo, SB, TimestampFieldDesc, StatementType, Index)
      else begin
        Assert(Length(KeyAndDataFields.DataFieldDescs) > 0);
        for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
          if not KeyAndDataFields.DataFieldDescs[i].IsBlob // not "text", or "blob"
            and not (KeyAndDataFields.DataFieldDescs[i].FieldNo in FldUsed) then
            AddFieldToCondition(ParamsInfo, SB, KeyAndDataFields.DataFieldDescs[i], StatementType, Index);
      end;
    end
    else
      inherited;
end;

procedure TCustomMySQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(FTableInfo.TableName);
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('WHERE');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  ');
  GenerateConditions(ParamsInfo, FCondSB, _stLock, KeyAndDataFields, Index);
  FFooterSB.Append(SLLineSeparator);
  case FLockRecordType of
    ilrImmediately:
      FFooterSB.Append('FOR UPDATE');
    ilrDelayed:
      FFooterSB.Append('LOCK IN SHARE MODE');
  end;
end;

function TCustomMySQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  Limit: integer;
  BatchedHandler: boolean;
  Parser: TMyParser;
  Lexem: string;
  QTableName: string;
begin
  QTableName := MySQLInfo.NormalizeName(TableName, QuoteNames);

  if not FUseHandler then begin
    Result := 'SELECT * FROM ' + QTableName;

    if OrderFields <> '' then
      Result := Result + ' ORDER BY ' + OrderFields;

    if (FLimit <> -1) or (FOffset <> 0) then
      Result := Result + ' LIMIT ' + IntToStr(FOffset) + ', ' + IntToStr(FLimit);
  end
  else begin
    BatchedHandler := TMySQLConnection(GetIConnection).IsClient41 and TMySQLConnection(GetIConnection).IsServer41;
    Result := '';
    if BatchedHandler then
      Result := 'HANDLER ' + QTableName + ' OPEN; ';

    Result := Result + 'HANDLER ' + QTableName + ' READ';

    FHandlerIndex := Trim(FHandlerIndex);
    if FHandlerIndex <> '' then begin
      Result := Result + ' ' + FHandlerIndex;
      Parser := TMyParser.Create(FHandlerIndex);
      try
        Parser.OmitBlank := True;
        Parser.OmitComment := True;
        Parser.GetNextToken;
        Parser.GetNext(Lexem); //+++ char instead of string
        if (Lexem <> '=') and (Lexem <> '<') and (Lexem <> '>') then
          Result := Result + ' FIRST';
      finally
        Parser.Free;
      end;
    end
    else
      Result := Result + ' FIRST';

    if FService.FilterSQL <> '' then
      Result := Result + ' WHERE ' + FService.FilterSQL;

    if FLimit = -1 then
      Limit := MaxInt
    else
      Limit := FLimit;
    Result := Result + ' LIMIT ' + IntToStr(FOffset) + ', ' + IntToStr(Limit);

    if BatchedHandler then begin
      Result := Result + ';';
      Result := Result + ' HANDLER ' + QTableName + ' CLOSE';
    end;
  end;
end;

function TCustomMySQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean = False): string;
var
  Parser: TMyParser;
  SelectPos: integer;
  FromPos: integer;
  s: string;
  Lexem: integer;
  DelimiterPos: integer;
  HaveDistinct, UseNestedSubquery: boolean;
begin
  Result := '';
  Clear;

  if UseBaseSQL then
    s := FService.BaseSQL
  else
    s := FService.FinalSQL;
  s := SQLSetOrderBy(s, '');
  Parser := TMyParser.Create(s);
  Parser.OmitBlank := True;
  Parser.OmitComment := True;
  try
    if Parser.ToLexem(lxSELECT) <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      Lexem := Parser.GetNextToken;
      HaveDistinct := (Lexem = lxDISTINCT) or (Lexem = lxDISTINCTROW);
      UseNestedSubquery := False;
      if HaveDistinct then begin
        repeat
          Lexem := Parser.GetNextToken;
        until (Lexem = lxAS) or (Lexem = lxFROM) or (Lexem = lcEnd);
        if Lexem = lxAS then
          UseNestedSubquery := True;
      end;

      if (Lexem = lxFROM) or (Parser.ToLexem(lxFROM, True) <> lcEnd) then begin
        FromPos := Parser.CurrPos;

        if UseNestedSubquery or (Parser.ToLexem(lxLIMIT) <> lcEnd) then begin
          if Parser.ToLexem(lxSemicolon) <> lcEnd then
            DelimiterPos := Parser.CurrPos
          else
            DelimiterPos := MaxInt;
          s := Copy(s, 1, SelectPos) + ' COUNT(*) FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ') q' + Copy(s, DelimiterPos, MaxInt);
        end
        else
        if HaveDistinct then
          s := Copy(s, 1, SelectPos) + ' COUNT(' + Copy(s, SelectPos + 1, FromPos - 4 - SelectPos) + ')' + Copy(s, FromPos - 4 {length('FROM')}, MaxInt)
        else
          s := Copy(s, 1, SelectPos) + ' COUNT(*)' + Copy(s, FromPos - 4 {length('FROM')}, MaxInt);

        FHeaderSB.Append(s);
        Result := AssembleSB();
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function TCustomMySQLGenerator.GenerateEmptyTableSQL(const TableName: string): string;
begin
  Result := 'TRUNCATE TABLE ' + TableName;
end;

function TCustomMySQLGenerator.GenerateSmartFetchMetaInfoSQL: string;
begin
  Result := FIRecordSet.OriginalSQL;
end;

end.
