{$IFNDEF CLR}

{$I Dac.inc}

unit DASQLGenerator;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
  CRTypes, CRFunctions, CRParser, CLRClasses, MemData, CRAccess, DAConsts;

type
  _TStatementType = (_stQuery, _stInsert, _stUpdate, _stDelete, _stLock, _stRefresh,
    _stCustom, _stRefreshQuick, _stRefreshCheckDeleted, _stBatchUpdate, _stRecCount);

{ TSQLGeneratorService }

  TSQLGeneratorServiceClass = class of TSQLGeneratorService;
  TDASQLGeneratorClass = class of TDASQLGenerator;

  TSQLGeneratorService = class
  public
    constructor Create; virtual;

    procedure RaiseError(const Message: string); virtual;

    function GetOldRecBuf: IntPtr; virtual; abstract;
    function GetNewRecBuf: IntPtr; virtual; abstract;
    function BlobFieldModified(FieldDesc: TCRFieldDesc): boolean; virtual;
    function GetFieldObject(FieldDesc: TFieldDesc): TSharedObject; virtual;

    function GetUpdateCommand: TCRCommand; virtual;
    function GetDBKeyList(const TableName, IndexName: string): string; virtual;

    function ParamExists(const ParamName: string): boolean; virtual;

    function BaseSQL: string; virtual; abstract;
    function FinalSQL: string; virtual; abstract;
    function FilterSQL: string; virtual; abstract;
  end;

{ TDASQLGenerator }

  TDASQLGenerator = class(TSQLGenerator)
  protected
    FService: TSQLGeneratorService;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIRecordSet: TCRRecordSet;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FICommand: TCRCommand;  // for performance
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FIUpdateRecordSet: TCRRecordSet;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FTableInfo: TCRTableInfo;
    FKeySequence: string;

                                    // stInsert                 stUpdate                 stDelete                 stRefresh        stRecCount
    FHeaderSB,                      // INSERT INTO Tbl(         UPDATE Tbl SET           DELETE FROM Tbl WHERE    SELECT           SELECT COUNT(*) FROM (
    FFldSB,                         // f1, f2, f3, ...          f1 = :p1, f2 = :p2, ...                           f1, f2, f3, ...
    FMiddleSB,                      // ) VALUES (               WHERE                                             FROM Tbl WHERE   query
    FFldParamSB,                    // :p1, :p2, :p3, ...
    FCondSB,                        //                          f0 = :p0                 f0 = :p0                 f0 = :p0
    FFooterSB: StringBuilder;       // )                                                                                           )

    FOldRecBuf, FNewRecBuf: IntPtr;
    FDesignMode: boolean;
    FDMLRefresh: boolean;
    FSubstituteParamName: boolean;
    FRefreshSQLFields: string;
    FIsUsedIndexNameForFields: boolean;

    function GetIConnection: TCRConnection;
    procedure SetIRecordSet(Value: TCRRecordSet);
    function SQLInfo: TSQLInfo;
    function GetParserClass: TSQLParserClass;
    function GetFullRefresh: boolean;
    function GetQuoteNames: boolean;

    function QuoteName(const AName: string): string;
    function CachedUpdates: boolean;
    function InCacheProcessing: boolean;

    function GetOldRecBuf: IntPtr;
    function GetNewRecBuf: IntPtr;

    procedure Clear; virtual;
    function AssembleSB: string;

    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; overload; virtual;
    function FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean; overload; virtual;
    function FieldIsChanged(FieldDesc: TCRFieldDesc): boolean;

    function BlobModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
    function FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean; overload; virtual;
    function FieldModified(FieldDesc: TCRFieldDesc): boolean; overload; virtual;

    function GetActualFieldName(FieldDesc: TCRFieldDesc; FullRefresh: boolean): string;
    function GetFullTableName(FieldDesc: TCRFieldDesc): string; virtual;
    function GenerateIndexName(const Name: string): string; virtual;
    function MaxIdentLength: integer; virtual;

    function ParamPrefix: Char; virtual;
    function IsSubstituteParamName: boolean; virtual;
    function SubstitutedParamName(ParamsInfo: TDAParamsInfo): string; virtual;

    function SQLGetFrom(const SQLText: string): string;
    function SQLGetWhere(const SQLText: string): string;
    function SQLAddWhere(const SQLText, Condition: string): string;
    function SQLSetOrderBy(const SQLText, Fields: string): string;

    procedure AddParam(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder;
      FieldDesc: TCRFieldDesc;
      const StatementType: _TStatementType;
      ParamType: TParamDirection;
      Index: integer = -1;
      Old: boolean = False;
      AllowDuplicates: boolean = True); virtual;

    function AddParamDesc(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder; FieldDesc: TCRFieldDesc;
      const StatementType: _TStatementType; const ParamType: TParamDirection;
      Index: integer = -1): TParamDesc;

    procedure AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
      FieldDesc: TCRFieldDesc;
      const Index: integer = -1); virtual;
    procedure AddFieldToUpdateSQL(ParamsInfo: TDAParamsInfo;
      FieldDesc: TCRFieldDesc;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure AddFieldToRefreshSQL(FieldDesc: TCRFieldDesc); virtual;
    procedure AddFieldToCondition(ParamsInfo: TDAParamsInfo;
      SB: StringBuilder; FieldDesc: TCRFieldDesc;
      const StatementType: _TStatementType; const Index: integer = -1); virtual;

    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); virtual;
    procedure GenerateDeleteSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); virtual;
    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); virtual;
    procedure GenerateFullRefreshSQL;
    procedure GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields); virtual;
    procedure GenerateRefreshSQLFromPart; virtual;
    procedure GenerateRefreshSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields); virtual;
    procedure GenerateRefreshQuickSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields); virtual;
    procedure GenerateRefreshCheckDeletedSQL(const KeyAndDataFields: TKeyAndDataFields); virtual;

    procedure GenerateConditions( // Generate WHERE part for UPDATE, DELETE, REFRESH SQLs
      ParamsInfo: TDAParamsInfo; SB: StringBuilder; const StatementType: _TStatementType;
      const KeyAndDataFields: TKeyAndDataFields; const Index: integer = -1); virtual;

    property FullRefresh: boolean read GetFullRefresh;
    property QuoteNames: boolean read GetQuoteNames;

  public
    constructor Create(ServiceClass: TSQLGeneratorServiceClass); virtual;
    destructor Destroy; override;

    function CloneGenerator: TSQLGenerator; override;

    function IndexedPrefix: string;
    function DecodeFieldIndex(const FieldName: string): integer; virtual;
    function GetParamOffset(ParamInfo: TDAParamInfo): integer;

    // Generate insert, update, delete or refresh SQL statements
    function GenerateSQLforUpdTable(ParamsInfo: TDAParamsInfo;
      UpdatingTableInfo: TCRTableInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const StatementType: _TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1): string;

    function GenerateSQL(ParamsInfo: TDAParamsInfo;
      const StatementType: _TStatementType;
      const ModifiedFieldsOnly: boolean;
      const Index: Integer = -1): string; virtual;

    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;

    function GenerateSelectValues(const ValuesList: string): string; override;

    function GenerateEmptyTableSQL(const TableName: String): string; override;

  {$IFNDEF LITE}
    function GenerateSmartFetchMetaInfoSQL: string; override;
    function GenerateSmartFetchKeyOnlySQL(const PrefetchedFields: TFieldDescArray): string; override;
    procedure PrepareSmartFetchDataByKeySQL(const Sql: string; SmartFetchInfo: TSmartFetchInfo); override;
    function GenerateSmartFetchDataByKeySQL(SmartFetchInfo: TSmartFetchInfo;
      ParamsInfo: TDAParamsInfo; FirstItem: PItemHeader; RowCount: integer): string; override;
  {$ENDIF}

    property IRecordSet: TCRRecordSet write SetIRecordSet;
    property IUpdateRecordSet: TCRRecordSet write FIUpdateRecordSet;
    property DesignMode: boolean write FDesignMode;
    property DMLRefresh: boolean write FDMLRefresh;
    property SubstituteParamName: boolean write FSubstituteParamName;
    property KeySequence: string read FKeySequence write FKeySequence;
    property IsUsedIndexNameForFields: boolean read FIsUsedIndexNameForFields;
    property RefreshSQLFields: string read FRefreshSQLFields;
    property Service: TSQLGeneratorService read FService;
  end;

implementation

uses
  CRProps, MemUtils;

{ TSQLGeneratorService }

constructor TSQLGeneratorService.Create;
begin
  inherited Create;
end;

procedure TSQLGeneratorService.RaiseError(const Message: string);
begin
  raise Exception.Create(Message);
end;

function TSQLGeneratorService.BlobFieldModified(FieldDesc: TCRFieldDesc): boolean;
begin
  Result := False;
end;

function TSQLGeneratorService.GetFieldObject(FieldDesc: TFieldDesc): TSharedObject;
begin
  Result := nil;
end;

function TSQLGeneratorService.GetUpdateCommand: TCRCommand;
begin
  Result := nil;
end;

function TSQLGeneratorService.GetDBKeyList(const TableName, IndexName: string): string;
begin
  Result := '';
end;

function TSQLGeneratorService.ParamExists(const ParamName: string): boolean;
begin
  Result := False;
end;

{ TDASQLGenerator }

constructor TDASQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited Create;

  FService := ServiceClass.Create;

  FHeaderSB := StringBuilder.Create(100);
  FFldSB := StringBuilder.Create(100);
  FMiddleSB := StringBuilder.Create(100);
  FFldParamSB := StringBuilder.Create(100);
  FCondSB := StringBuilder.Create(100);
  FFooterSB := StringBuilder.Create(100);

  FSubstituteParamName := True;
end;

destructor TDASQLGenerator.Destroy;
begin
  FHeaderSB.Free;
  FFldSB.Free;
  FMiddleSB.Free;
  FFldParamSB.Free;
  FCondSB.Free;
  FFooterSB.Free;

  FService.Free;

  inherited;
end;

procedure TDASQLGenerator.SetIRecordSet(Value: TCRRecordSet);
begin
  FIRecordSet := Value;
  if FIRecordSet <> nil then
    FICommand := FIRecordSet.GetCommand;
end;

function TDASQLGenerator.GetIConnection: TCRConnection;
begin
  if FICommand <> nil then
    Result := FICommand.GetConnection
  else
    Result := nil;
end;

function TDASQLGenerator.SQLInfo: TSQLInfo;
begin
  if FICommand <> nil then
    Result := FICommand.SQLInfo
  else
    Result := DefaultSQLInfo;
end;

function TDASQLGenerator.GetParserClass: TSQLParserClass;
begin
  if FICommand <> nil then
    Result := FICommand.GetParserClass
  else
    Result := TSQLParser;
end;

function TDASQLGenerator.GetFullRefresh: boolean;
var
  Value: Variant;
begin
  if FIRecordSet <> nil then begin
    FIRecordSet.GetProp(prFullRefresh, Value);
    Result := Value;
    FIRecordSet.GetProp(prReadOnly, Value);
    Result := Result or boolean(Value);
  end
  else
    Result := False;
end;

function TDASQLGenerator.GetQuoteNames: boolean;
var
  Value: Variant;
begin
  if FICommand <> nil then begin
    FICommand.GetProp(prQuoteNames, Value);
    Result := Value;
  end
  else
    Result := False;
end;

function TDASQLGenerator.QuoteName(const AName: string): string;
begin
  if QuoteNames or SQLInfo.QuotesNeeded(AName) then
    Result := SQLInfo.Quote(AName)
  else
    Result := AName;
end;

function TDASQLGenerator.CachedUpdates: boolean;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.CachedUpdates
  else
    Result := False;
end;

function TDASQLGenerator.InCacheProcessing: boolean;
begin
  if FIRecordSet <> nil then
    Result := FIRecordSet.InCacheProcessing
  else
    Result := False;
end;

function TDASQLGenerator.GetOldRecBuf: IntPtr;
begin
  if FOldRecBuf = nil then
    FOldRecBuf := FService.GetOldRecBuf;

  Result := FOldRecBuf;
end;

function TDASQLGenerator.GetNewRecBuf: IntPtr;
begin
  if FNewRecBuf = nil then
    FNewRecBuf := FService.GetNewRecBuf;

  Result := FNewRecBuf;
end;

procedure TDASQLGenerator.Clear;
begin
  FHeaderSB.Length := 0;
  FFldSB.Length := 0;
  FMiddleSB.Length := 0;
  FFldParamSB.Length := 0;
  FCondSB.Length := 0;
  FFooterSB.Length := 0;
  FRefreshSQLFields := '';

  FOldRecBuf := nil;
  FNewRecBuf := nil;
end;

function TDASQLGenerator.AssembleSB: string;
begin
  Result :=
    FHeaderSB.ToString +
    FFldSB.ToString +
    FMiddleSB.ToString +
    FFldParamSB.ToString +
    FCondSB.ToString +
    FFooterSB.ToString;
end;

function TDASQLGenerator.GetParamOffset(ParamInfo: TDAParamInfo): integer;
begin
  if ParamInfo.SB = FFldSB then
    Result := FHeaderSB.Length
  else
  if ParamInfo.SB = FFldParamSB then
    Result := FHeaderSB.Length + FFldSB.Length + FMiddleSB.Length
  else
  if ParamInfo.SB = FCondSB then
    Result := FHeaderSB.Length + FFldSB.Length + FMiddleSB.Length + FFldParamSB.Length
  else
  if ParamInfo.SB = FFooterSB then
    Result := FHeaderSB.Length + FFldSB.Length + FMiddleSB.Length + FFldParamSB.Length + FCondSB.Length
  else begin
    Result := 0;
    Assert(False);
  end;
end;

function TDASQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean;
  Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
//This function added for Expand fields support
var
  i: integer;
  ChildFieldDesc: TFieldDesc;
begin
  if FieldDesc.IsObject then begin
    i := FieldDesc.FieldNo;
    Result := False;
    while i < Data.Fields.Count do begin
      ChildFieldDesc := Data.Fields[i];
      Result := (ChildFieldDesc.ParentField = FieldDesc) and FieldIsNull(TCRFieldDesc(ChildFieldDesc), OldValue);
      inc(i);
      if not Result then
        Break;
    end;
  end
  else
    if OldValue then
      Result := Data.GetNull(FieldDesc, OldRecBuf)
    else
      Result := Data.GetNull(FieldDesc, NewRecBuf);
end;

function TDASQLGenerator.FieldIsNull(FieldDesc: TCRFieldDesc; OldValue: boolean): boolean;
begin
  Result := FieldIsNull(FieldDesc, OldValue, FIRecordSet, GetOldRecBuf, GetNewRecBuf);
end;

function TDASQLGenerator.FieldIsChanged(FieldDesc: TCRFieldDesc): boolean;
begin
  Result := FIRecordSet.GetChanged(FieldDesc, GetNewRecBuf);
end;

function TDASQLGenerator.BlobModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
begin
  if Data.InCacheProcessing then
    Result := Data.GetBlob(FieldDesc, NewRecBuf) <> Data.GetBlob(FieldDesc, OldRecBuf)
  else
    Result := Data.GetBlob(FieldDesc, NewRecBuf).CanRollback or
              FService.BlobFieldModified(FieldDesc);
end;

function TDASQLGenerator.FieldModified(FieldDesc: TCRFieldDesc; Data: TData; OldRecBuf, NewRecBuf: IntPtr): boolean;
//This function added for Expand fields support
var
  i: integer;
  ChildFieldDesc: TFieldDesc;
begin
  if FieldDesc.IsBlob then
    Result := BlobModified(FieldDesc, Data, OldRecBuf, NewRecBuf)
  else
  if FieldDesc.IsObject then begin
    Result := False;
    i := FieldDesc.FieldNo;
    while i < Data.Fields.Count do begin
      // Child FieldDescs always next to parent FielDescs
      // But child fields can be mixed with their own childs in case of nested objects
      ChildFieldDesc := Data.Fields[i];
      Result := (ChildFieldDesc.ParentField = FieldDesc) and FieldModified(TCRFieldDesc(ChildFieldDesc));
      inc(i);
      if Result then
        break;
    end;
  end
  else
    Result := TMemData(Data).CompareFields(OldRecBuf, NewRecBuf, FieldDesc, [coOrdinalCompare], False) <> 0;
end;

function TDASQLGenerator.FieldModified(FieldDesc: TCRFieldDesc): boolean;
begin
  Result := FieldModified(FieldDesc, FIRecordSet, GetOldRecBuf, GetNewRecBuf);
end;

function TDASQLGenerator.GetActualFieldName(FieldDesc: TCRFieldDesc; FullRefresh: boolean): string;
begin
  Result := FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames);
  if FullRefresh and (FIRecordSet.TablesInfo.Count <> 1) // and not FIRecordSet.UpdTableIsArtificial
  then begin // Use original Select part, so we could use Field aliasess
    if FieldDesc.TableInfo <> nil then
      if FieldDesc.TableInfo.TableAlias <> '' then
        Result := SQLInfo.NormalizeName(FieldDesc.TableInfo.TableAlias, QuoteNames) +
          '.' + Result
      else
        Result := SQLInfo.NormalizeName(FieldDesc.TableInfo.TableName, QuoteNames) +
          '.' + Result;
  end;
end;

function TDASQLGenerator.GetFullTableName(FieldDesc: TCRFieldDesc): string;
begin
  Result := FieldDesc.TableInfo.TableNameFull;
end;

function TDASQLGenerator.GenerateIndexName(const Name: string): string;
begin
  Result := '_' + Name;
end;

function TDASQLGenerator.CloneGenerator: TSQLGenerator;
var
  SQLGeneratorClass: TDASQLGeneratorClass;
  SQLGeneratorServiceClass: TSQLGeneratorServiceClass;
begin
  SQLGeneratorClass := TDASQLGeneratorClass(Self.ClassType);
  SQLGeneratorServiceClass := TSQLGeneratorServiceClass(FService.ClassType);
  Result := SQLGeneratorClass.Create(SQLGeneratorServiceClass);
  TDASQLGenerator(Result).IRecordSet := FIRecordSet;
end;

function TDASQLGenerator.DecodeFieldIndex(const FieldName: string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 2) and (FieldName[1] = '_') then begin
    Val(Copy(FieldName, 2, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

function TDASQLGenerator.IndexedPrefix: string;
begin
  Result := 'P_';
end;

function TDASQLGenerator.SubstitutedParamName(ParamsInfo: TDAParamsInfo): string;
begin
  Result := '?';
end;

function TDASQLGenerator.MaxIdentLength: integer;
begin
  Result := MaxInt;
end;

function TDASQLGenerator.ParamPrefix: Char;
begin
  Result := ':';
end;

function TDASQLGenerator.IsSubstituteParamName: boolean;
begin
  Result := FSubstituteParamName;
end;

function TDASQLGenerator.SQLGetFrom(const SQLText: string): string;
begin
  Result := _GetFrom(SQLText, GetParserClass, False, MacroChar);
end;

function TDASQLGenerator.SQLGetWhere(const SQLText: string): string;
begin
  Result := _GetWhere(SQLText, GetParserClass, False, MacroChar);
end;

function TDASQLGenerator.SQLAddWhere(const SQLText, Condition: string): string;
begin
  Result := _AddWhere(SQLText, Condition, GetParserClass, False, MacroChar);
end;

function TDASQLGenerator.SQLSetOrderBy(const SQLText, Fields: string): string;
begin
  Result := _SetOrderBy(SQLText, Fields, GetParserClass);
end;

procedure TDASQLGenerator.AddParam(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder;
  FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType;
  ParamType: TParamDirection;
  Index: integer = -1; // only for CachedUpdates
  Old: boolean = False;
  AllowDuplicates: boolean = True);
var
  i, p: integer;
  ParamName, Prefix: string;
  CheckQuoteNeeded: boolean;
  ParamInfo: TDAParamInfo;
begin
  ParamName := FieldDesc.Name;
  if not SQLInfo.ParamQuoteAllowed and not IsSubstituteParamName then
    ParamName := StringReplace(ParamName, ' ', '_', [rfReplaceAll]);

  CheckQuoteNeeded := True;

  if Old then
    Prefix := 'Old_';

  if _stRefresh = StatementType then
    if FService.ParamExists(ParamName) then
      Index := 1;

  if Index > -1 then
    Prefix := IndexedPrefix + IntToStr(Index) + '_' + Prefix;

  if Length(Prefix + ParamName) <= MaxIdentLength then
    ParamName := Prefix + ParamName
  else begin
    ParamName := Prefix + IntToStr(FieldDesc.FieldNo);
    CheckQuoteNeeded := False;
  end;

  if SQLInfo.ParamQuoteAllowed then
    if QuoteNames or (CheckQuoteNeeded and SQLInfo.QuotesNeeded(FieldDesc.Name)) then
      ParamName := SQLInfo.Quote(ParamName);

  p := SB.Length + 1;
  if not IsSubstituteParamName then begin
    SB.Append(ParamPrefix);
    SB.Append(ParamName);
  end
  else
    SB.Append(SubstitutedParamName(ParamsInfo));

  if not AllowDuplicates then
    for i := 0 to ParamsInfo.Count - 1 do
      // check FieldDesc before ParamName to improve performance
      if (ParamsInfo[i].FieldDesc = FieldDesc) and
         (ParamsInfo[i].ParamName = ParamName)
      then begin
        if ParamType = pdInputOutput then
          ParamsInfo[i].ParamType := pdInputOutput;
        Exit;
      end;

  ParamInfo := TDAParamInfo(ParamsInfo.Add);
  ParamInfo.FieldDesc := FieldDesc;
  ParamInfo.Old := Old;
  ParamInfo.ParamName := ParamName;
  ParamInfo.ParamType := ParamType;
  ParamInfo.StartPosition := p;
  ParamInfo.EndPosition := SB.Length + 1;
  ParamInfo.SB := SB;
end;

procedure TDASQLGenerator.AddFieldToCondition(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder;
  FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType;
  const Index: integer = -1);
var
  ActualName: string;
begin
  if SB.Length > 0 then
    SB.Append(' AND ');

  Assert(FieldDesc <> nil);
  ActualName := GetActualFieldName(FieldDesc, (StatementType = _stRefresh) and FullRefresh);
  SB.Append(ActualName);

  if not FDesignMode and FieldIsNull(FieldDesc, StatementType <> _stRefresh) then // Refresh generated with current field values
    SB.Append(' IS NULL')
  else begin
    SB.Append(' = ');
    AddParam(ParamsInfo, SB, FieldDesc, StatementType, pdInput, Index,
      (StatementType <> _stRefresh) or
      (CachedUpdates and not InCacheProcessing)); // no need to use old value on refresh after update, insert
  end;
end;

procedure TDASQLGenerator.GenerateConditions(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder;
  const StatementType: _TStatementType;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
var
  i: integer;
begin
  if Length(KeyAndDataFields.KeyFieldDescs) > 0 then
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      AddFieldToCondition(ParamsInfo, SB, KeyAndDataFields.KeyFieldDescs[i], StatementType, Index)
  else begin
    if FIRecordSet.IdentityField <> nil then
      AddFieldToCondition(ParamsInfo, SB, FIRecordSet.IdentityField, StatementType, Index)
    else begin
      if Length(KeyAndDataFields.DataFieldDescs) = 0 then
        FService.RaiseError(SNoKeyFields);

      for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
        if not KeyAndDataFields.DataFieldDescs[i].IsBlob then
          AddFieldToCondition(ParamsInfo, SB, KeyAndDataFields.DataFieldDescs[i], StatementType, Index);

      if SB.Length = 0 then
        FService.RaiseError(SNoKeyFields);
    end;
  end;
end;

procedure TDASQLGenerator.AddFieldToInsertSQL(ParamsInfo: TDAParamsInfo;
  FieldDesc: TCRFieldDesc;
  const Index: integer = -1);
begin
  if FFldSB.Length > 0 then begin
    FFldSB.Append(', ');
    FFldParamSB.Append(', ');
  end;

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
  AddParam(ParamsInfo, FFldParamSB, FieldDesc, _stInsert, pdInput, Index);
end;

procedure TDASQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
begin
  for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
    FieldDesc := KeyAndDataFields.DataFieldDescs[i];

    // Insert all(!) field values
    if not ModifiedFieldsOnly  then
      AddFieldToInsertSQL(ParamsInfo, FieldDesc, Index)
    else if not FieldIsNull(FieldDesc, False) or FieldIsChanged(FieldDesc) then
      AddFieldToInsertSQL(ParamsInfo, FieldDesc, Index);
  end;

  FHeaderSB.Append('INSERT INTO ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('  (');
  // Append FFldSB
  FMiddleSB.Append(')');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('VALUES');
  FMiddleSB.Append(SLLineSeparator);
  FMiddleSB.Append('  (');
  // Append FFldParamSB
  FFooterSB.Append(')');
end;

procedure TDASQLGenerator.AddFieldToUpdateSQL(ParamsInfo: TDAParamsInfo;
  FieldDesc: TCRFieldDesc;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
begin
  if FFldSB.Length > 0 then
    FFldSB.Append(', ');

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
  FFldSB.Append(' = ');
  AddParam(ParamsInfo, FFldSB, FieldDesc, _stUpdate, pdInput, Index);
end;

procedure TDASQLGenerator.GenerateUpdateSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
begin
  for i := 0 to High(KeyAndDataFields.DataFieldDescs) do begin
    FieldDesc := KeyAndDataFields.DataFieldDescs[i];

    if not ModifiedFieldsOnly  then
      AddFieldToUpdateSQL(ParamsInfo, FieldDesc, ModifiedFieldsOnly, Index)
    else if FieldModified(FieldDesc) then
      AddFieldToUpdateSQL(ParamsInfo, FieldDesc, ModifiedFieldsOnly, Index);
  end;

  if FFldSB.Length > 0 then begin
    FHeaderSB.Append('UPDATE ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
    FHeaderSB.Append(SLLineSeparator);
    FHeaderSB.Append('SET');
    FHeaderSB.Append(SLLineSeparator);
    FHeaderSB.Append('  ');
    // Append FFldSB
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('WHERE');
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('  ');
    // Append FFldParamSB
    GenerateConditions(ParamsInfo, FCondSB, _stUpdate, KeyAndDataFields, Index);
  end;
end;

procedure TDASQLGenerator.GenerateDeleteSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
  FHeaderSB.Append('DELETE FROM ');
  FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('WHERE');
  FHeaderSB.Append(SLLineSeparator);
  FHeaderSB.Append('  ');
  GenerateConditions(ParamsInfo, FCondSB, _stDelete, KeyAndDataFields, Index);
end;

procedure TDASQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin

end;

procedure TDASQLGenerator.AddFieldToRefreshSQL(FieldDesc: TCRFieldDesc);
var
  IndexName: string;
begin
  if FFldSB.Length > 0 then
    FFldSB.Append(', ');

  FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));

  if not FDesignMode then begin
    IndexName := GenerateIndexName(IntToStr(FIRecordSet.Fields.IndexOf(FieldDesc)));
    FFldSB.Append(' AS ' + IndexName);
  {$IFNDEF LITE}
    if Assigned(FieldDesc.Encryptor) then
      FRefreshSQLFields := FRefreshSQLFields + ';' + IndexName;
  {$ENDIF}
  end;
end;

procedure TDASQLGenerator.GenerateFullRefreshSQL;
var
  i: integer;
  FieldDescs: TFieldDescs;
  FieldDesc: TCRFieldDesc;
  TableName: string;
  TableNames: TStringList;
  IndexName: string;
begin
  TableNames := TStringList.Create;
  try
    // SELECT ... FROM ... WHERE ... {WITH NOLOCK}
    // Add SELECT section
    FHeaderSB.Append('SELECT ');
    FieldDescs := FIRecordSet.Fields;
    for i := 0 to FieldDescs.Count - 1 do begin
      FieldDesc := TCRFieldDesc(FieldDescs[i]);

      if FieldDesc.FieldDescKind = fdkData then begin
        if FieldDesc.TableInfo <> nil then begin
          TableName := GetFullTableName(FieldDesc);

          if TableNames.IndexOf(TableName) = - 1 then
            TableNames.Add(TableName);

          FHeaderSB.Append(GetActualFieldName(FieldDesc, True));
          if not FDesignMode then begin
            IndexName := GenerateIndexName(IntToStr(FIRecordSet.Fields.IndexOf(FieldDesc)));
            FHeaderSB.Append(' AS ' + IndexName);
          {$IFNDEF LITE}
            if Assigned(FieldDesc.Encryptor) then
              FRefreshSQLFields := FRefreshSQLFields + ';' + IndexName;
          {$ENDIF}
          end;

          FHeaderSB.Append(', ');
        end;
      end;
    end;
    FHeaderSB.Length := FHeaderSB.Length - 2;

    // Add FROM section
    FHeaderSB.Append(' FROM ');
    for i := 0 to TableNames.Count - 1 do
      FHeaderSB.Append(SQLInfo.NormalizeName(TableName, True) + ', ');
    FHeaderSB.Length := FHeaderSB.Length - 2;

    // Add WHERE section
    if FCondSB.Length > 0 then
      FMiddleSB.Append(' WHERE ');

    FIsUsedIndexNameForFields := True;
  finally
    TableNames.Free;
  end;
end;

procedure TDASQLGenerator.GenerateRefreshSQLSelectPart(const KeyAndDataFields: TKeyAndDataFields);
var
  i: integer;
begin
  FHeaderSB.Append('SELECT ');

  // SELECT ... FROM .... {WITH NOLOCK}
  // Add SELECT section
  if Length(KeyAndDataFields.DataFieldDescs) > 0 then begin
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      AddFieldToRefreshSQL(KeyAndDataFields.DataFieldDescs[i]);
  end
  else
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      AddFieldToRefreshSQL(KeyAndDataFields.KeyFieldDescs[i]);

  FIsUsedIndexNameForFields := True;
end;

procedure TDASQLGenerator.GenerateRefreshSQLFromPart;
begin
  FMiddleSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
  if FTableInfo.TableAlias <> '' then begin
    FMiddleSB.Append(' ');
    FMiddleSB.Append(SQLInfo.NormalizeName(FTableInfo.TableAlias, QuoteNames));
  end;
end;

procedure TDASQLGenerator.GenerateRefreshSQL(ParamsInfo: TDAParamsInfo; const KeyAndDataFields: TKeyAndDataFields);
{
  function HasEncryptor(const KeyAndDataFields: TKeyAndDataFields): boolean;
  var
    i: integer;
  begin
    for i := 0 to High(KeyAndDataFields.DataFieldDescs) do
      if Assigned(KeyAndDataFields.DataFieldDescs[i].Encryptor) then begin
        Result := True;
        Exit;
      end;
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      if Assigned(KeyAndDataFields.KeyFieldDescs[i].Encryptor) then begin
        Result := True;
        Exit;
      end;

    Result := False;
  end;
}
var
  SelectSQL: string;
begin
  GenerateConditions(ParamsInfo, FCondSB, _stRefresh, KeyAndDataFields);
  if FullRefresh then begin
    if FCondSB.Length = 0 then begin
      SelectSQL := FService.FinalSQL;
      FHeaderSB.Append(SelectSQL);
    end
    else begin
      SelectSQL := FService.BaseSQL;
      FHeaderSB.Append(SQLAddWhere(SelectSQL, SLLineSeparator + '  ' + FCondSB.ToString));
      FCondSB.Length := 0; // WHERE clause already added to FHeaderSB
    end;
  end
  else begin
    GenerateRefreshSQLSelectPart(KeyAndDataFields);
    FMiddleSB.Append(' FROM ');
    GenerateRefreshSQLFromPart;
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('WHERE');
    FMiddleSB.Append(SLLineSeparator);
    FMiddleSB.Append('  ');
  end;
end;

procedure TDASQLGenerator.GenerateRefreshQuickSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields);
begin
  GenerateConditions(ParamsInfo, FCondSB, _stRefreshQuick, KeyAndDataFields);
  FHeaderSB.Append(SQLAddWhere(FService.FinalSQL, FCondSB.ToString));
  FCondSB.Length := 0; // WHERE clause already added to FHeaderSB
end;

procedure TDASQLGenerator.GenerateRefreshCheckDeletedSQL(const KeyAndDataFields: TKeyAndDataFields);
var
  i: integer;
  FieldDesc: TCRFieldDesc;
  ActualFieldName: string;
  Condition, FromClause: string;
  FinalSQL: string;
begin
  FHeaderSB.Append('SELECT ');

  for i := 0 to Length(KeyAndDataFields.KeyFieldDescs) - 1 do begin
    FieldDesc := KeyAndDataFields.KeyFieldDescs[i];
    if i > 0 then begin
      FFldSB.Append(', ');
      FFooterSB.Append(', ');
    end;
    ActualFieldName := GetActualFieldName(FieldDesc, True);
    FFldSB.Append(ActualFieldName);
    FFooterSB.Append(ActualFieldName);
    if not SameText(FieldDesc.ActualName, FieldDesc.Name) then
      FFldSB.Append(' AS ' + QuoteName(FieldDesc.Name));
  end;

  FinalSQL := FService.FinalSQL;
  FromClause := SQLGetFrom(FinalSQL);
  FMiddleSB.Append(' FROM ' + FromClause);
  Condition := SQLGetWhere(FinalSQL);
  if Condition <> '' then
    FMiddleSB.Append(' WHERE ' + Condition);
  FMiddleSB.Append(' ORDER BY ');
end;

function TDASQLGenerator.GenerateSQLforUpdTable(ParamsInfo: TDAParamsInfo;
  UpdatingTableInfo: TCRTableInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const StatementType: _TStatementType;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1): string;
begin
  if UpdatingTableInfo.TableName = '' then
    FService.RaiseError(SBadTableInfoName);

  Clear;
  ParamsInfo.Clear;
  FTableInfo := UpdatingTableInfo;

  case StatementType of
    _stInsert:
      GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);
    _stUpdate:
      GenerateUpdateSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);
    _stDelete:
      GenerateDeleteSQL(ParamsInfo, KeyAndDataFields, Index);
    _stLock:
      GenerateLockSQL(ParamsInfo, KeyAndDataFields, Index);
    _stRefresh:
      GenerateRefreshSQL(ParamsInfo, KeyAndDataFields);
    _stRefreshQuick:
      GenerateRefreshQuickSQL(ParamsInfo, KeyAndDataFields);
    _stRefreshCheckDeleted:
      GenerateRefreshCheckDeletedSQL(KeyAndDataFields);
    _stRecCount:
      GenerateRecCountSQL(True);
  else
    FService.RaiseError(SBadStatementType);
  end;

  Result := AssembleSB();
end;

function TDASQLGenerator.GenerateSQL(ParamsInfo: TDAParamsInfo;
  const StatementType: _TStatementType;
  const ModifiedFieldsOnly: boolean;
  const Index: Integer = -1): string;
var
  KeyAndDataFields: TKeyAndDataFields;
  ForceUseAllFields: boolean;
begin
  FIsUsedIndexNameForFields := False;

  Assert(FIRecordSet <> nil);
  if FIRecordSet.UpdatingTableInfoIdx = -1 then
    Result := ''
  else begin
    ForceUseAllFields := (StatementType = _stRefresh) and FullRefresh and
      (FIRecordSet.TablesInfo.Count <> 1);
    FIRecordSet.GetKeyAndDataFields(KeyAndDataFields, ForceUseAllFields);
    Result := GenerateSQLforUpdTable(ParamsInfo, FIRecordSet.UpdatingTableInfo,
      KeyAndDataFields, StatementType, ModifiedFieldsOnly, Index);
  end;
end;

function TDASQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  AOrderFields: string;
begin
  Result := 'SELECT * FROM ' + SQLInfo.NormalizeName(TableName, QuoteNames) + SLLineSeparator;

  AOrderFields := Trim(OrderFields);
  if AOrderFields <> '' then
    Result := Result + 'ORDER BY ' + AOrderFields + SLLineSeparator;
end;

function TDASQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean = False): string;
var
  SelectSQL: string;
begin
  if UseBaseSQL then
    SelectSQL := FService.BaseSQL // for SQLGeneratorFrame
  else
    SelectSQL := FService.FinalSQL;
  if SelectSQL = '' then
    FService.RaiseError(SEmptySQLStatement);

  Clear;

  FHeaderSB.Append('SELECT COUNT(*) FROM (');
  FMiddleSB.Append(SelectSQL);
  FFooterSB.Append(')');

  Result := AssembleSB();
end;

function TDASQLGenerator.GenerateSelectValues(const ValuesList: string): string;
begin
  Result := 'SELECT ' + ValuesList;
end;

function TDASQLGenerator.GenerateEmptyTableSQL(const TableName: String): string;
begin
  Result := 'DELETE FROM ' + SQLInfo.NormalizeName(TableName, QuoteNames);
end;

{$IFNDEF LITE}
function TDASQLGenerator.GenerateSmartFetchMetaInfoSQL: string;
begin
  Result := SQLAddWhere(FIRecordSet.OriginalSQL, '1=0');
end;

function TDASQLGenerator.GenerateSmartFetchKeyOnlySQL(const PrefetchedFields: TFieldDescArray): string;

  procedure AddFieldToCondition(FieldDesc: TCRFieldDesc);
  begin
    if FFldSB.Length > 0 then
      FFldSB.Append(', ');
    FFldSB.Append(GetActualFieldName(FieldDesc, True));
    if not SameText(FieldDesc.ActualName, FieldDesc.Name) then
      FFldSB.Append(' AS ' + QuoteName(FieldDesc.Name));
  end;

var
  KeyFieldDescs: TFieldDescArray;
  NeedAdd: boolean;
  FinalSQL: string;
  Parser: TSQLParser;
  SelectPos, FromPos: integer;
  Code: integer;
  i, j: integer;
begin
  Result := '';
  Clear;

  FIRecordSet.GetKeyFieldDescs(KeyFieldDescs);
  if Length(KeyFieldDescs) = 0 then
    raise ESmartFetchError.Create(SNoKeyFields);
  for i := 0 to Length(KeyFieldDescs) - 1 do
    AddFieldToCondition(KeyFieldDescs[i]);

  if Length(PrefetchedFields) > 0 then
    for i := 0 to Length(PrefetchedFields) - 1 do begin
      NeedAdd := True;
      for j := 0 to Length(KeyFieldDescs) - 1 do
        if PrefetchedFields[i] = KeyFieldDescs[j] then begin
          NeedAdd := False;
          break;
        end;
      if NeedAdd then
        AddFieldToCondition(PrefetchedFields[i]);
    end;

  FinalSQL := FIRecordSet.OriginalSQL;
  Parser := GetParserClass.Create(FinalSQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      SelectPos := Parser.CurrPos;
      if Parser.ToLexem(lxFROM, True) <> lcEnd then begin
        FromPos := Parser.CurrPos;

        FHeaderSB.Append(Copy(FinalSQL, 1, SelectPos));
        FHeaderSB.Append(' ');
        FMiddleSB.Append(' ');
        FMiddleSB.Append(Copy(FinalSQL, FromPos - 3, MaxInt));
        Result := AssembleSB();
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDASQLGenerator.PrepareSmartFetchDataByKeySQL(const Sql: string; SmartFetchInfo: TSmartFetchInfo);
var
  ParamInfo: TDAParamInfo;
  Condition: string;
  StartPos, EndPos: integer;
  i: integer;
begin
  Condition := '';
  _FindWherePosition(Sql, Condition, GetParserClass, False, MacroChar, StartPos, EndPos);
  if StartPos = -1 then
    raise ESmartFetchError.Create(SOnlySelectAllowed);

  for i := 0 to SmartFetchInfo.ParamsInfo.Count - 1 do begin
    ParamInfo := SmartFetchInfo.ParamsInfo.Items[i];
    if ParamInfo.StartPosition > EndPos then begin
      ParamInfo.SB := FFooterSB;
      ParamInfo.StartPosition := ParamInfo.StartPosition - EndPos;
      ParamInfo.EndPosition := ParamInfo.EndPosition - EndPos;
    end
    else
    if (StartPos <> EndPos) and (ParamInfo.StartPosition >= StartPos) then begin
      ParamInfo.StartPosition := ParamInfo.StartPosition + 1;
      ParamInfo.EndPosition := ParamInfo.EndPosition + 1;
    end;
  end;

  SmartFetchInfo.SqlBeforeWhere := Copy(Sql, 1, EndPos) + Condition;
  if StartPos <= EndPos then
    Insert('(', SmartFetchInfo.SqlBeforeWhere, StartPos);
  SmartFetchInfo.SqlAfterWhere := Copy(Sql, EndPos + 1, MaxInt);
end;

function TDASQLGenerator.GenerateSmartFetchDataByKeySQL(SmartFetchInfo: TSmartFetchInfo;
  ParamsInfo: TDAParamsInfo; FirstItem: PItemHeader; RowCount: integer): string;
var
  FieldDesc: TCRFieldDesc;
  ParamDesc: TParamDesc;
  ParamInfo: TDAParamInfo;
  RecBuf: IntPtr;
  Value: variant;
  Offset: integer;
  i, k: integer;
begin
  Clear;
  SubstituteParamName := True;

  ParamsInfo.Assign(SmartFetchInfo.ParamsInfo);

  FCondSB.Append('(');

  for i := 0 to RowCount - 1 do begin
    if FirstItem = nil then
      break;

    if i > 0 then
      FCondSB.Append(' OR ');

    if Length(SmartFetchInfo.KeyFieldDescs) > 1 then
      FCondSB.Append('(');

    RecBuf := PtrOffset(FirstItem, SizeOf(TItemHeader));
    for k := 0 to Length(SmartFetchInfo.KeyFieldDescs) - 1 do begin
      if k > 0 then
        FCondSB.Append(' AND ');
      FCondSB.Append('(');
      FieldDesc := SmartFetchInfo.KeyFieldDescs[k];
      FCondSB.Append(GetActualFieldName(FieldDesc, True));

      if FIRecordSet.GetNull(FieldDesc, RecBuf) then
        FCondSB.Append(' IS NULL')
      else begin
        FCondSB.Append('=');

        ParamDesc := AddParamDesc(ParamsInfo, FCondSB, FieldDesc, _stCustom, pdInput, i);
        if FieldDesc.IsSharedObject then begin
          ParamDesc.SetObject(TSharedObject(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, FieldDesc.Offset))));
          ParamDesc.SetNull(False);
        end
        else begin
          FIRecordSet.GetFieldAsVariant(FieldDesc, RecBuf, Value);
          ParamDesc.SetValue(Value);
        end;
      end;

      if Length(SmartFetchInfo.KeyFieldDescs) > 1 then
        FCondSB.Append(')');
    end;

    FCondSB.Append(')');
    FirstItem := FirstItem.Next;
  end;

  FCondSB.Append(') ');

  FHeaderSB.Append(SmartFetchInfo.SqlBeforeWhere);
  FFooterSB.Append(SmartFetchInfo.SqlAfterWhere);
  Result := AssembleSB();

  for i := 0 to ParamsInfo.Count - 1 do begin
    ParamInfo := ParamsInfo.Items[i];
    if ParamInfo.SB <> nil then begin
      Offset := GetParamOffset(ParamInfo);
      ParamInfo.StartPosition := ParamInfo.StartPosition + Offset;
      ParamInfo.EndPosition := ParamInfo.EndPosition + Offset;
    end;
  end;

  for i := 0 to ParamsInfo.Count - 1 do begin
    ParamInfo := ParamsInfo.Items[i];
    if ParamInfo.SB = FFooterSB then begin
      /// move to end
      FICommand.Params.Move(i, FICommand.Params.Count - 1);
      ParamInfo.Index := ParamsInfo.Count - 1;
    end
    else
    if ParamInfo.SB = FCondSB then
      Break;
  end;
end;
{$ENDIF}

function TDASQLGenerator.AddParamDesc(ParamsInfo: TDAParamsInfo;
  SB: StringBuilder; FieldDesc: TCRFieldDesc;
  const StatementType: _TStatementType; const ParamType: TParamDirection;
  Index: integer = -1): TParamDesc;
var
  ParamInfo: TDAParamInfo;
  ParamDesc: TParamDesc;
begin
  AddParam(ParamsInfo, SB, FieldDesc, StatementType, ParamType, Index);
  ParamInfo := ParamsInfo[ParamsInfo.Count - 1];

  ParamDesc := FICommand.AddParam;
  ParamDesc.SetName(ParamInfo.ParamName);
  ParamDesc.SetParamType(ParamInfo.ParamType);

  case ParamInfo.FieldDesc.DataType of
    dtExtString:
      ParamDesc.SetDataType(dtString);
    dtExtWideString:
      ParamDesc.SetDataType(dtWideString);
    dtExtVarBytes:
      ParamDesc.SetDataType(dtVarBytes);
  else
    ParamDesc.SetDataType(ParamInfo.FieldDesc.DataType);
  end;
  ParamDesc.SetSubDataType(ParamInfo.FieldDesc.SubDataType);
  ParamDesc.SetNational(ParamInfo.FieldDesc.IsNational);
  ParamDesc.SetSize(ParamInfo.FieldDesc.Length);
  ParamDesc.SetConvertEOL(FIRecordSet.NeedConvertEOL);
{$IFNDEF LITE}
  ParamDesc.SetEncryptor(ParamInfo.FieldDesc.Encryptor);
{$ENDIF}
  ParamInfo.ParamRef := ParamDesc;
  Result := ParamDesc;
end;

end.
