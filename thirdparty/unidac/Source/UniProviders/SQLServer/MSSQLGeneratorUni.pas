{$IFNDEF CLR}
{$I Sdac.inc}
unit MSSQLGeneratorUni;
{$ENDIF}

interface

uses
{$IFDEF CLR}
  System.Runtime.InteropServices, System.Text, Variants,
{$ELSE}
  CLRClasses,
{$ENDIF}
  SysUtils, Classes, CRTypes, DASQLGenerator, MemData, CRAccess,
{$IFNDEF UNIDACPRO}
  SqlClasses, MSClasses;
{$ELSE}
  SqlClassesUni, MSClassesUni;
{$ENDIF}

type
  TMSLockTypeI = (iltUpdate, iltExclusive);
  TMSLockObjectI = (iloRow, iloTable);

  TCustomMSSQLGenerator = class(TDASQLGenerator)
  private
    FQueryIdentity: boolean;
    FLastIdentityValueFunction: TMSLastIdentityValueFunction;
    FCheckRowVersion: boolean;
    FLockType: TMSLockTypeI;
    FLockObject: TMSLockObjectI;

    FCountIsParam: boolean;
    FUseParamType: boolean;
    function GetCursorType: TMSCursorType;
    function GetMSProvider: TMSProvider;
    function GetDatabase: string;

  protected
    procedure Clear; override;

    function GetFullTableName(FieldDesc: TCRFieldDesc): string; override;

    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
    procedure GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields); override;
    procedure GenerateRefreshSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields); override;

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

    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateSelectFilestreamContextSQL(const FieldName: string; ParamsInfo: TDAParamsInfo): string;
    function GenerateEmptyTableSQL(const TableName: string): string; override;

    class function GenerateQuotedIdentifierSQL(const Value: Boolean): string;
    class function GenerateLockTimeoutSQL(const Value: Integer): string;
    class function GenerateIsolationLevelSQL(const Value: TCRIsolationLevel): string;

    property CountIsParam: boolean read FCountIsParam;
    property UseParamType: boolean read FUseParamType;
    property QueryIdentity: boolean read FQueryIdentity write FQueryIdentity;
    property LastIdentityValueFunction: TMSLastIdentityValueFunction write FLastIdentityValueFunction;

    property CheckRowVersion: boolean write FCheckRowVersion;
    property LockType: TMSLockTypeI write FLockType;
    property LockObject: TMSLockObjectI write FLockObject;
  end;

implementation

uses
  DAConsts, CRProps, CRParser,
{$IFNDEF UNIDACPRO}
  MSConsts, MSProps, MSParser;
{$ELSE}
  MSConstsUni, MSPropsUni, MSParserUni;
{$ENDIF}

{ TCustomMSSQLGenerator }

constructor TCustomMSSQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited;

  FQueryIdentity := True;
  FLastIdentityValueFunction := vfScopeIdentity;
end;

procedure TCustomMSSQLGenerator.Clear;
begin
  inherited;

  FUseParamType := False;
end;

function TCustomMSSQLGenerator.GetCursorType: TMSCursorType;
var
  Value: Variant;
begin
  Assert(FIRecordSet <> nil);
  FIRecordSet.GetProp(prCursorType, Value);
  Result := TMSCursorType(Value);
end;

function TCustomMSSQLGenerator.GetMSProvider: TMSProvider;
var
  Value: Variant;
begin
  GetIConnection.GetProp(prProvider, Value);
  Result := TMSProvider(Value);
end;

function TCustomMSSQLGenerator.GetDatabase: string;
var
  Value: Variant;
begin
  GetIConnection.GetProp(prDatabase, Value);
  Result := Value;
end;

function TCustomMSSQLGenerator.GetFullTableName(FieldDesc: TCRFieldDesc): string;
begin
  Result := TSqlRecordSet(FIRecordSet).GenerateTableName(TMSFieldDesc(FieldDesc).BaseCatalogName,
    TMSFieldDesc(FieldDesc).BaseSchemaName, TMSFieldDesc(FieldDesc).BaseTableName, GetDatabase);
end;

procedure TCustomMSSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  FieldDesc, IdentityField: TCRFieldDesc;
  i: integer;
  IsFirstParam: boolean;
begin
  inherited;

  if FFldSB.Length = 0 then begin
    if not IsCompactEdition(TMSSQLConnection(GetIConnection).ServerMajorVer) then begin
      Clear;
      FHeaderSB.Append('INSERT INTO ');
      FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
      FHeaderSB.Append(' DEFAULT VALUES');
    end
    else begin
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if not FieldDesc.ReadOnly then begin
          if FFldSB.Length > 0 then begin
            FFldSB.Append(', ');
            FFldParamSB.Append(', ');
          end;
          FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
          FFldParamSB.Append('DEFAULT');
        end;
      end;
    end;
  end;

  if GetCursorType in [ctDefaultResultSet, ctBaseTable] then begin
    { Getting Identity value }
    if FQueryIdentity and
      not IsCompactEdition(TMSSQLConnection(GetIConnection).ServerMajorVer) then begin
      IdentityField := FIRecordSet.IdentityField;
      if IdentityField <> nil then begin
        // Warning - Identity param must be last in param list, see SetIdentityParam
        // Warning - If in 'INSERT ...' statement present sql_variant value then adding 'SET ...' fails statement
        // Warning - TMSFieldDesc(GetFieldDescByField(FIdentityField)).BaseColumnName cannot be used (for example see gettting identity on INSERT into View)

        FFooterSB.Append(#$D#$A'SET ');
        FUseParamType := True;
        AddParam(ParamsInfo, FFooterSB, IdentityField, _stInsert, pdInputOutput);

        if TMSSQLConnection(GetIConnection).ServerMajorVer > 7 then
          case FLastIdentityValueFunction of
            vfScopeIdentity:
              FFooterSB.Append(' = SCOPE_IDENTITY()');
            vfIdentCurrent: begin
              FFooterSB.Append(' = IDENT_CURRENT(''');
              FFooterSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, False));
              FFooterSB.Append(''')');
            end;
            vfIdentity:
              FFooterSB.Append(' = @@Identity');
          end
        else
          FFooterSB.Append(' = @@Identity');
      end;
    end;

    { DMLRefresh }
    if FDMLRefresh then begin
      IsFirstParam := True;
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if not (FieldDesc.ReadOnly or FieldDesc.IsBlob) then begin
          if not IsFirstParam then
            FFooterSB.Append(', ')
          else
            FFooterSB.Append(SLLineSeparator + 'SELECT ');
          IsFirstParam := False;
          FUseParamType := True;
          AddParam(ParamsInfo, FFooterSB, FieldDesc, _stInsert, pdInputOutput);
          FFooterSB.Append(' = ' + FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
        end;
      end;
      if not IsFirstParam then begin
        FFooterSB.Append(' FROM ' + SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames) +
          SLLineSeparator + 'WHERE' + SLLineSeparator + '  ');
        GenerateConditions(ParamsInfo, FCondSB, _stInsert, KeyAndDataFields);
        FFooterSB.Append(FCondSB.ToString);
        FCondSB.Length := 0;
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  FieldDesc: TCRFieldDesc;
  i: integer;
  IsFirstParam: boolean;
  OldCondSB: StringBuilder;
begin
  inherited;

  if GetCursorType in [ctDefaultResultSet, ctBaseTable] then begin
    { DMLRefresh }
    if (FFldSB.Length > 0) and FDMLRefresh then begin
      FFooterSB.Append(SLLineSeparator);
      FFooterSB.Append('SELECT ');
      IsFirstParam := True;
      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
        FieldDesc := KeyAndDataFields.DataFieldDescs[i];
        if not (FieldDesc.ReadOnly or FieldDesc.IsBlob) then begin
          if not IsFirstParam then
            FFooterSB.Append(', ');
          IsFirstParam := False;
          FUseParamType := True;
          AddParam(ParamsInfo, FFooterSB, FieldDesc, _stUpdate, pdInputOutput);
          FFooterSB.Append(' = ' + FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
        end;
      end;
      FFooterSB.Append(' FROM ');
      FFooterSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
      FFooterSB.Append(SLLineSeparator);
      FFooterSB.Append('WHERE');
      FFooterSB.Append(SLLineSeparator);
      FFooterSB.Append('  ');
      OldCondSB := FCondSB;
      try
        FCondSB := StringBuilder.Create;
        try
          GenerateConditions(ParamsInfo, FCondSB, _stUpdate, KeyAndDataFields);
          FFooterSB.Append(FCondSB.ToString);
        finally
          FCondSB.Free;
        end;
      finally
        FCondSB := OldCondSB;
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields);
var
  FieldDesc: TCRFieldDesc;
  UseDataFields: boolean;
  FieldArrHigh: integer;
begin
  inherited;

  if TSqlRecordSet(FIRecordSet).TimestampField <> nil then begin
    UseDataFields := Length(KeyAndDataFields.DataFieldDescs) + Length(KeyAndDataFields.DataFieldDescs) > 0;
    if UseDataFields then
      FieldArrHigh := Length(KeyAndDataFields.DataFieldDescs) + Length(KeyAndDataFields.DataFieldDescs) - 1
    else
      FieldArrHigh := High(KeyAndDataFields.KeyFieldDescs);

    if FieldArrHigh >= 0 then
      FFldSB.Append(', ');
    FieldDesc := TSqlRecordSet(FIRecordSet).TimestampField;
    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));

    if not FDesignMode then
      FFldSB.Append(' AS ' + GenerateIndexName(IntToStr(FIRecordSet.Fields.IndexOf(FieldDesc))));
  end;
end;

procedure TCustomMSSQLGenerator.GenerateRefreshSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields);
var
  IsSPProc, FullRefresh: variant;
begin
  FIRecordSet.GetCommand.GetProp(prIsStoredProc, IsSPProc);
  FIRecordSet.GetProp(prFullRefresh, FullRefresh);
  if IsSPProc and FullRefresh then
    GenerateFullRefreshSQL
  else
    inherited;
end;

procedure TCustomMSSQLGenerator.AddFieldToCondition(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder; FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType; const Index: integer = -1);
begin
  Assert(FieldDesc <> nil);

  if (StatementType = _stInsert) and (FieldDesc = FIRecordSet.IdentityField) then begin
    FCondSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FCondSB.Append(' = ');
    if TMSSQLConnection(GetIConnection).ServerMajorVer > 7 then
      case FLastIdentityValueFunction of
        vfScopeIdentity:
          FCondSB.Append('SCOPE_IDENTITY()');
        vfIdentCurrent: begin
          FCondSB.Append('IDENT_CURRENT(''');
          FCondSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, False));
          FCondSB.Append(''')');
        end;
        vfIdentity:
          FCondSB.Append('@@Identity');
      end
    else
      FCondSB.Append('@@Identity');
  end
  else
    inherited;
end;

procedure TCustomMSSQLGenerator.GenerateConditions(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder; const StatementType: _TStatementType;
  const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1);

  procedure GenerateCondForRQ;
  {SQL Server Books Online -> Accessing and Changing Relational Data ->
   Transact-SQL Syntax Elements -> Using Data Types -> Using Special Data:

   In SQL Server version 7.0 and SQL Server 2000, @@DBTS is only incremented for use
   in timestamp columns. If a table contains a timestamp column, every time a row is
   modified by an INSERT, UPDATE, or DELETE statement, the timestamp value in the row
   is set to the current @@DBTS value, and then @@DBTS is incremented by one...}
  var
    MaxTimestamp: Int64;
    FieldDesc: TCRFieldDesc;
  begin
    FieldDesc := TSqlRecordSet(FIRecordSet).TimestampField;
    if FieldDesc = nil then
      FService.RaiseError(STimestampFieldRequired);

    MaxTimestamp := TSqlTableInfo(FieldDesc.TableInfo).MaxTimestamp;
    FCondSB.Append(GetActualFieldName(FieldDesc, True) + ' > ' + '0x' + IntToHex(MaxTimestamp, SizeOf(MaxTimestamp) * 2));
  end;

var
  i: integer;
  FldUsed: set of byte;
  TestChanges: boolean;
begin
  FCondSB.Length := 0;

  if StatementType = _stRefreshQuick then
    GenerateCondForRQ
  else begin
    TestChanges := (StatementType in [_stInsert, _stUpdate]) and FDMLRefresh and
      (GetCursorType in [ctDefaultResultSet, ctBaseTable]) and FCheckRowVersion;

    if not TestChanges then
      inherited
    else begin
      if (TSqlRecordSet(FIRecordSet).TimestampField <> nil) and not FieldIsNull(TSqlRecordSet(FIRecordSet).TimestampField, False) then
        AddFieldToCondition(ParamsInfo, FCondSB, TSqlRecordSet(FIRecordSet).TimestampField, StatementType, Index)
      else begin
        FldUsed := [];
        if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
          for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do begin
            AddFieldToCondition(ParamsInfo, FCondSB, KeyAndDataFields.KeyFieldDescs[i], StatementType, Index);
            FldUsed := FldUsed + [KeyAndDataFields.KeyFieldDescs[i].FieldNo];
          end;

        if Length(KeyAndDataFields.DataFieldDescs) = 0 then
          FService.RaiseError(SNoKeyFields);
        for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
          if not KeyAndDataFields.DataFieldDescs[i].IsBlob // not "text", "ntext", "image"
            and not (KeyAndDataFields.DataFieldDescs[i].FieldNo in FldUsed) then
            AddFieldToCondition(ParamsInfo, FCondSB, KeyAndDataFields.DataFieldDescs[i], StatementType, Index);
      end;
    end;
  end;
end;

procedure TCustomMSSQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1);
begin
  FHeaderSB.Append('SELECT * FROM ');
  FHeaderSB.Append(FTableInfo.TableName);
  FHeaderSB.Append(SLLineSeparator);

  (* MSDN: Table Hint (Transact-SQL)
    HOLDLOCK     Makes shared locks more restrictive by holding them until a transaction is completed
    ROWLOCK      Specifies that row locks are taken when page or table locks are ordinarily taken.
    ROWLOCK      Specifies that row locks are taken when page or table locks are ordinarily taken.
    TABLOCK      Specifies that a shared lock is taken on the table held until the end-of-statement. If HOLDLOCK is also specified, the shared table lock is held until the end of the transaction.
    TABLOCKX     Specifies that an exclusive lock is taken on the table. If HOLDOCK is also specified, the lock is held until the transaction completes.
    UPDLOCK      Specifies that update locks are to be taken and held until the transaction completes.
    XLOCK        Specifies that exclusive locks are to be taken and held until the transaction completes.

    ltUpdate:    ROWLOCK, UPDLOCK, HOLDLOCK
    ltExclusive: ROWLOCK, XLOCK
  *)
  case FLockType of
    iltUpdate:
      case FLockObject of
        iloRow:
          FMiddleSB.Append('WITH (UPDLOCK, ROWLOCK, HOLDLOCK)');
        iloTable:
          FMiddleSB.Append('WITH (UPDLOCK, TABLOCK, HOLDLOCK)');
      end;
    iltExclusive:
      case FLockObject of
        iloRow:
          FMiddleSB.Append('WITH (ROWLOCK, XLOCK)');
        iloTable:
          FMiddleSB.Append('WITH (TABLOCKX, HOLDLOCK)');
      end;
  end;
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('WHERE');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  ');
  case FLockObject of
    iloRow:
      GenerateConditions(ParamsInfo, FCondSB, _stLock, KeyAndDataFields, Index);
    iloTable:
      FCondSB.Append('1 = 0');
  end;
end;

function TCustomMSSQLGenerator.GenerateSelectFilestreamContextSQL(
  const FieldName: string; ParamsInfo: TDAParamsInfo): string;
var
  KeyAndDataFields: TKeyAndDataFields;
begin
  Assert(FIRecordSet <> nil);

  FIRecordSet.GetKeyAndDataFields(KeyAndDataFields, False);
  if (FIRecordSet.UpdatingTableInfo = nil) or (FIRecordSet.UpdatingTableInfo.TableName = '') then
    FService.RaiseError(SBadTableInfoName);
  if FieldName = '' then
    FService.RaiseError(SFieldNameNotDefined);

  Clear;
  ParamsInfo.Clear;

  FHeaderSB.Append('SELECT ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FieldName, QuoteNames));
  FHeaderSB.Append('.PathName(), GET_FILESTREAM_TRANSACTION_CONTEXT()');
  FHeaderSB.Append(SLLineSeparator);
  FMiddleSB.Append('FROM ');
  FMiddleSB.Append(SQLInfo.NormalizeName(FIRecordSet.UpdatingTableInfo.TableNameFull, QuoteNames));
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('WHERE ');
  // Append FParamSB
  GenerateConditions(ParamsInfo, FCondSB, _stCustom, KeyAndDataFields);

  Result := AssembleSB();
end;

function TCustomMSSQLGenerator.GenerateEmptyTableSQL(const TableName: string): string;
begin
  Result := 'TRUNCATE TABLE ' + TableName;
end;

function TCustomMSSQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean): string;
var
  Parser: TMSParser;
  Code, SelectPos, FromPos, DelimiterPos: integer;
  s: string;
  CountParamName: string;
  WithLexemUsed, IsSelectModifier: boolean;
begin
  Result := '';
  Clear;
  FCountIsParam := False;

  if UseBaseSQL then
    s := FService.BaseSQL
  else
    s := FService.FinalSQL;
  s := SQLSetOrderBy(s, '');
  CountParamName := '';

  Parser := TMSParser.Create(s);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    IsSelectModifier := False;

    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    WithLexemUsed := Code = lxWITH;
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      repeat
        Code := Parser.ToLexem([lxALL, lxDISTINCT, lxTOP, lxFROM, lxSELECT]);
        case Code of
          lxSELECT:
            SelectPos := Parser.CurrPos;
          lxALL, lxDISTINCT, lxTOP:
            IsSelectModifier := True;
        end;
      until (Code = lcEnd) or (Code = lxFROM);

      if Code = lxFROM then
        FromPos := Parser.CurrPos
      else
        FromPos := 0;

      repeat
        Code := Parser.ToLexem([lxUNION, lxGROUP, lxSemicolon{';'}]);
        if (Code = lxUNION) or (Code = lxGROUP) then
          IsSelectModifier := True;
      until (Code = lxSemicolon{';'}) or (Code = lcEnd);

      if Code = lxSemicolon then
        DelimiterPos := Parser.CurrPos
      else
        DelimiterPos := MaxInt;

      if (FromPos > 0) and not IsSelectModifier then begin
        if (GetMSProvider = prCompact) or WithLexemUsed then begin
          s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT ' + Copy(s, FromPos - 4{length('FROM')}, MaxInt);
        end
        else begin
          CountParamName := ':PCOUNT';
          s := Copy(s, 1, SelectPos - 6{length('SELECT')}) +
            'SET ' + CountParamName + ' = (SELECT COUNT(*)' + Copy(s, FromPos - 4{length('FROM')}, MaxInt) + ')';
        end;
      end
      else
        if IsSelectModifier then
          s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ') q'
        else
          s := Copy(s, 1, SelectPos) + ' COUNT(*) AS FCOUNT FROM (SELECT ' + Copy(s, SelectPos + 1, DelimiterPos - 1 - SelectPos) + ' f) q';
    end;
  finally
    Parser.Free;
  end;

  if s <> '' then begin
    Result := s;
    FHeaderSB.Append(Result);
    FCountIsParam := CountParamName <> '';
  end;
end;

class function TCustomMSSQLGenerator.GenerateQuotedIdentifierSQL(const Value: Boolean): string;
begin
  if Value then
    Result := 'SET QUOTED_IDENTIFIER ON'
  else
    Result := 'SET QUOTED_IDENTIFIER OFF';
end;

class function TCustomMSSQLGenerator.GenerateLockTimeoutSQL(const Value: Integer): string;
begin
  Result := 'SET LOCK_TIMEOUT ' + IntToStr(Value);
end;

class function TCustomMSSQLGenerator.GenerateIsolationLevelSQL(const Value: TCRIsolationLevel): string;
begin
  case Value of
    ilReadCommitted:
      Result := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
    ilReadUnCommitted:
      Result := 'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
    ilRepeatableRead:
      Result := 'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ';
    ilIsolated:
      Result := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    ilSnapshot:
      Result := 'SET TRANSACTION ISOLATION LEVEL SNAPSHOT';
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
  end;
end;

end.
