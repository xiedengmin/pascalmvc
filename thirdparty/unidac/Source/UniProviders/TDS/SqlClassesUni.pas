
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQL Server Base level
//////////////////////////////////////////////////

{$I Dac.inc}
unit SqlClassesUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Variants, FMTBcd, SyncObjs,
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
  DAConsts, CLRClasses, CRTypes, CRFunctions, CRParser, CRProps,
  MemUtils, MemData, CRAccess, CRThread;

const
  sdtNumeric = 202;
{ internal sub data types }
  sdtWide = $8000;
  sdtMSUDT = 101;
  EmptyString = '';
  EmptyWString = WideString('');
{$IFDEF IS_UTF8}
  MaxSizeClientCharset = 4;
{$ENDIF}

type
  TRunMethod = procedure of object;

  TSqlThreadWrapper = class(TCRThreadWrapper)
  protected
    FCommand: TCRCommand;
    FRunMethod: TRunMethod;
    procedure InternalExecute; override;
    function CloneException(E: Exception): Exception; override;
  public
    constructor Create(Command: TCRCommand; RunMethod: TRunMethod);
  end;

  TSqlTableInfo = class(TCRTableInfo)
  protected
    FBaseTableName: string;
    FMaxTimestamp: Int64;
  public
    property BaseTableName: string read FBaseTableName write FBaseTableName;
    property MaxTimestamp: Int64 read FMaxTimestamp write FMaxTimestamp;
  end;

  TSqlFieldDesc = class(TCRFieldDesc)
  protected
    FBaseCatalogName: string;
    FBaseSchemaName: string;
    FBaseTableName: string;
    FIsTimestamp: boolean;
    FIsLong: boolean;
    FXMLSchemaCollectionCatalogName: string;
    FXMLSchemaCollectionSchemaName: string;
    FXMLSchemaCollectionName: string;
    FXMLTyped: boolean;

  public
    property BaseCatalogName: string read FBaseCatalogName write FBaseCatalogName;
    property BaseSchemaName: string read FBaseSchemaName write FBaseSchemaName;
    property BaseTableName: string read FBaseTableName write FBaseTableName;
    property IsTimestamp: boolean read FIsTimestamp write FIsTimestamp;
    property IsLong: boolean read FIsLong write FIsLong;

    property XMLSchemaCollectionCatalogName: string read FXMLSchemaCollectionCatalogName write FXMLSchemaCollectionCatalogName;
    property XMLSchemaCollectionSchemaName: string read FXMLSchemaCollectionSchemaName write FXMLSchemaCollectionSchemaName;
    property XMLSchemaCollectionName: string read FXMLSchemaCollectionName write FXMLSchemaCollectionName;
    property XMLTyped: boolean read FXMLTyped write FXMLTyped;
  end;

  TSqlCommand = class(TCRCommand)
  protected
    FIsSProc: boolean;
    FOpenNext: boolean;
    FUseDescribeParams: boolean;
    FCanReadParams: boolean;
    FSensibleBCDMapping: boolean;
    FCursorState: TCursorState;
    FCommandTimeout: integer;
    FRequestResultSet: boolean; // Indicate current command owner - Command(False) or DataSet(True)

    { NonBlocking }
    FNonBlocking: boolean;
    FExecutor: TSqlThreadWrapper;
    FLock: TCriticalSection;
    FBreakExecCS: TCriticalSection;
    FWaitForBreak: boolean;

    procedure Busy;
    procedure Release;
    procedure CreateExecutor;
    procedure DoExecuteTerminate(Sender: TObject);
    procedure DoExecuteException(Sender: TObject; E: Exception; var Fail: boolean); virtual; abstract;
    procedure WaitAsynchCompletion; virtual; abstract;
    procedure CancelCommand; virtual; abstract;

    procedure EndExecute(E: Exception); virtual;

    function GetRowsAffected: NativeInt; virtual; abstract;
    procedure SetRowsAffected(Value: NativeInt); virtual; abstract;
    class function InternalGetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
      ParamsInfo: TDAParamsInfo; Iters: integer): string; virtual;
    function NeedPrepareOnBatch: boolean; virtual;
    function GetBatchIters(Iters: integer): integer; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

    class procedure ParseProcName(var ProcName: string; out OverloadNum: string);
    procedure CheckSQLParamType(ParsedSQL: StringBuilder; Parser: TSQLParser; Param: TParamDesc); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BreakExec; override;

    function CalcEnableBCD: boolean;
    function CalcEnableFMTBCD: boolean;
    class function GetTableInfoClass: TTableInfoClass; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property RowsAffected: NativeInt read GetRowsAffected write SetRowsAffected;
    property CommandTimeout: integer read FCommandTimeout write FCommandTimeout;
  end;

  TSqlCommandClass = class of TSqlCommand;

  TSqlRecordSet = class(TCRRecordSet)
  private
    FTableInfoWasModified: boolean;
    FFetchExecutor: TSqlThreadWrapper;

    procedure DoFetchTerminate(Sender: TObject);
    procedure DoFetchException(Sender: TObject; E: Exception; var Fail: boolean);
    procedure DoFetchSendEvent(Sender: TObject; Event: Pointer);

    function GetUidWithBraces: Boolean;
  protected
    FCommand: TSqlCommand;

    FDisconnectedMode: boolean;
    FTimestampField: TCRFieldDesc;
    FLastFetchBack: boolean; // True, if previous Fetch was called with True parameter
    FCursorUpdate: boolean;

    // Bookmarks
    FFetchFromBookmark: boolean;
    FBookmarkSize: integer; // If FBookmarkSize = 4 then bookmark is ordinal otherwise bookmark is special (DBBMK_LAST, DBBMK_FIRST)
    FBookmarkValue: NativeInt; // If FBookmarkValue = - 1 then last fetch is not OK for KeySet cursor

    class function ExceptionIsCanceled(E: Exception): boolean; virtual;
    function GetDisconnectedMode: boolean;

    { Fetch }
    function InternalFetch(FetchBack: boolean = False): boolean; override;
    function CanFetchBack: boolean; override;
    procedure DoAfterFetch; override;
    procedure DoFetchAll;
    procedure EndFetchAll;

    function GetCommandClass: TSqlCommandClass; virtual; abstract;
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    function NeedInitFieldsOnFetch: Boolean; override;
    function IsServerCursor: boolean; virtual;
    function IsStaticCursor: boolean; virtual;
    function IsDynamicCursor: boolean; virtual;

    { Fields }
  {$IFNDEF LITE}
    procedure ApplyColumnsInfo(Columns: TCRColumnsInfo; ReadFieldsFromServer: boolean; DefaultTable: integer; AsteriskCount: integer); override;
  {$ENDIF}
    function FindTableInfoBySimpleName(const Name: string): integer; override;
    function CanUseAllKeyFields: boolean; override;
    function IdentityFieldIsData: boolean; override;
    function LoadKeyFieldsFromDB: boolean; virtual;
    procedure FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;
    procedure FillTablesAliases;
    function ExtFieldsInfoIsInternal: boolean; override;

    function IsWideField(Field: TFieldDesc): boolean; virtual; abstract;
    function CreateBlob(Field: TFieldDesc): TBlob;

    property DisconnectedMode: boolean read GetDisconnectedMode;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Disconnect; override;

    { Fields }
    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;
    function GenerateTableName(const CatalogName, SchemaName, TableName, DefaultCatalogName: string): string;
    function GetFieldDescType: TFieldDescClass; override;
    procedure DetectIdentityField; override;
    procedure ExplicitInitFields; override;
    function FieldListDependsOnParams: boolean; override;

    class function IsBlobDataType(DataType: word): boolean; override;
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); override;

    function GetFieldStrValue(RecBuf: IntPtr; Field: TFieldDesc): string;

    { Fetch }
    procedure FetchAll; override;
    procedure BreakFetch; override;

    { Filter/Find/Locate }
    function IsCaseSensitive: boolean; override;

    { Sorting }
    procedure SetIndexFieldNames(const Value: string); override;
    procedure SortItems; override;

    { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;
    function FetchToBookmarkValue: boolean;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; override;

    { Records }
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

    function HasNextResult: boolean; virtual; abstract;
    function CheckNextResult: boolean; virtual; abstract;
    procedure AssignToNextResult(Dest: TSqlRecordSet); virtual; abstract;
    function IsMetaInfo: boolean; virtual;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property TimestampField: TCRFieldDesc read FTimestampField;
    property FetchExecutor: TSqlThreadWrapper read FFetchExecutor;
    property UidWithBraces: boolean read GetUidWithBraces;
  end;

  TSqlLoaderColumn = class(TCRLoaderColumn)
  private
    FSkiped: Boolean;
    FParamDesc: TParamDesc;
    FDataArrPtr: PVariantArray;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;
    procedure Init(ArraySize: Integer);

    property Skiped: Boolean read FSkiped write FSkiped;
    property ParamDesc: TParamDesc read FParamDesc write FParamDesc;
    property DataArrPtr: PVariantArray read FDataArrPtr;
  end;

const // from OLEDBC
  DBBMK_INVALID = $00000000;
  {$EXTERNALSYM DBBMK_INVALID}
  DBBMK_FIRST = $00000001;
  {$EXTERNALSYM DBBMK_FIRST}
  DBBMK_LAST = $00000002;
  {$EXTERNALSYM DBBMK_LAST}
  MaxBatchParams = 2000;

  function GetParamNameWODog(const ParamName: string): string;
  function StrToGUID(const Value: string): TGUID;

{$IFDEF NODBACCESS}
  procedure DatabaseError(const Message: string; Component: TComponent = nil);
  procedure DatabaseErrorFmt(const Message: string; const Args: array of const; Component: TComponent = nil);
{$ENDIF}

implementation

function GetParamNameWODog(const ParamName: string): string;
begin
  if (ParamName <> '') and (ParamName[1] = '@') then
    Result := Copy(ParamName, 2, 1000)
  else
    Result := ParamName;
end;

function StrToGUID(const Value: string): TGUID;
var
  GuidStr: string;
begin
  GuidStr := Trim(Value);
  if Length(GuidStr) > 1 then begin
    if GuidStr[1] <> '{' then
      GuidStr := '{' + GuidStr;
    if string(GuidStr[Length(GuidStr)]) <> '}' then // Range check error
      GuidStr := GuidStr + '}';
  end;

  Result := StringToGUID(GuidStr);
end;

{$IFDEF NODBACCESS}
procedure DatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
    raise Exception.Create(Format('%s: %s', [Component.Name, Message])) else
    raise Exception.Create(Message);
end;

procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  DatabaseError(Format(Message, Args), Component);
end;
{$ENDIF}

{ TSqlThreadWrapper }

constructor TSqlThreadWrapper.Create(Command: TCRCommand; RunMethod: TRunMethod);
begin
  inherited Create;

  FCommand := Command;
  FRunMethod := RunMethod;
end;

procedure TSqlThreadWrapper.InternalExecute;
begin
  if Assigned(FRunMethod) then
    FRunMethod;
end;

function TSqlThreadWrapper.CloneException(E: Exception): Exception;
var
  Connector: TCRConnector;
begin
  Connector := FCommand.GetConnection.GetConnector;
  if Connector <> nil then
    Result := Connector.CloneException(E)
  else
    Result := Exception.Create(E.Message);
end;

{ TSqlCommand }

constructor TSqlCommand.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;
  FBreakExecCS := TCriticalSection.Create;

  FRequestResultSet := False;
  FCursorState := csInactive;
{$IFDEF LITE}
  FSensibleBCDMapping := True;
{$ENDIF}
end;

destructor TSqlCommand.Destroy;
begin
  FreeAndNil(FExecutor);

  inherited;

  FLock.Free;
  FBreakExecCS.Free;
end;

procedure TSqlCommand.Busy;
begin
  if FNonBlocking then
    FLock.Acquire;
end;

procedure TSqlCommand.Release;
begin
  if FNonBlocking then
    FLock.Release;
end;

procedure TSqlCommand.CreateExecutor;
begin
  FreeAndNil(FExecutor);

  FExecutor := TSqlThreadWrapper.Create(Self, WaitAsynchCompletion);
  FExecutor.OnException := DoExecuteException;
  FExecutor.OnTerminate := DoExecuteTerminate;
  FExecutor.Resume;
end;

procedure TSqlCommand.DoExecuteTerminate(Sender: TObject); // MainThread context
begin
  EndExecute(FExecutor.LastException);
end;

procedure TSqlCommand.EndExecute(E: Exception);
begin
  try
    if Assigned(FAfterExecute) then
      FAfterExecute(E = nil);
  finally
    FExecuting := False;
    FWaitForBreak := False;
  end;
end;

procedure TSqlCommand.BreakExec;
begin
  if FNonBlocking and (FExecutor <> nil) then begin
    FExecutor.Free;
    FExecutor := nil;
  end
  else begin
    FBreakExecCS.Acquire;
    try
      FWaitForBreak := True;
      if FExecuting then
        CancelCommand;
    finally
      FBreakExecCS.Release;
    end;
  end;
end;

function TSqlCommand.CalcEnableBCD: boolean;
begin
  Result := EnableBCD or ((FConnection <> nil) and FConnection.EnableBCD);
end;

function TSqlCommand.CalcEnableFMTBCD: boolean;
begin
  Result := EnableFMTBCD or ((FConnection <> nil) and FConnection.EnableFMTBCD);
  if not FSensibleBCDMapping then
    Result := Result and not EnableBCD;
end;

class function TSqlCommand.GetTableInfoClass: TTableInfoClass;
begin
  Result := TSqlTableInfo;
end;

class procedure TSqlCommand.ParseProcName(var ProcName: string; out OverloadNum: string);
var
  i: integer;
begin
  OverloadNum := '';

  i := Pos(';', ProcName);
  if i > 0 then begin
    i := Length(ProcName);
    while CharInSet(ProcName[i], ['0'..'9']) do
      Dec(i);

    if ProcName[i] = ';' then begin
      OverloadNum := Copy(ProcName, i + 1, Length(ProcName) - i);
      ProcName := Copy(ProcName, 1, i - 1);
    end;
  end;
end;

procedure TSqlCommand.CheckSQLParamType(ParsedSQL: StringBuilder; Parser: TSQLParser; Param: TParamDesc);
var
  St: string;
begin
  if Param <> nil then begin
    while Parser.GetNext(St) = lcBlank do
      ParsedSQL.Append(St);

    if LowerCase(St) = 'out' then
      Param.SetParamType(pdInputOutput);
    Parser.Back;
  end;
end;

class function TSqlCommand.InternalGetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
  ParamsInfo: TDAParamsInfo; Iters: integer): string;
begin
  Result := '';
  Assert(False);
end;

function TSqlCommand.NeedPrepareOnBatch: boolean;
begin
  Result := True;
end;

function TSqlCommand.GetBatchIters(Iters: integer): integer;
begin
  if (FParamsInfo.Count * Iters) > MaxBatchParams then
    Result := MaxBatchParams div FParamsInfo.Count
  else
    Result := MaxBatchParams;
  if Result > Iters then
    Result := Iters;
end;

procedure TSqlCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  BatchSize, LastBatchSize: integer;
  i, n: integer;
  BatchSQL: string;
  BatchCommand: TSqlCommand;
  NeedPrepare: boolean;
begin
  BatchCommand := TSqlCommand(GetBatchCommand);
  Assert(BatchCommand <> nil);


  BatchSize := GetBatchIters(Iters);
  RowsAffected := 0;

  if BatchSize <= 1 then begin
    for i := 0 to Iters - 1 do begin
      inherited InternalExecuteBatch(1, Offset + i);
      RowsAffected := RowsAffected + 1;
    end;
  end
  else begin
    n := 0;
    LastBatchSize := 0;
    FParamsProcessed := 0;
    NeedPrepare := False;

    try
      while n < Iters do begin
        if n + BatchSize > Iters then
          BatchSize := Iters - n;

        BatchCommand.FBatchIters := BatchSize;
        BatchCommand.FBatchOffset := Offset + n;

        NeedPrepare := LastBatchSize <> BatchSize;
        if NeedPrepare then begin
          if BatchCommand.GetPrepared then
            BatchCommand.Unprepare;

          BatchSQL := InternalGetBatchSQL(FSQL, ParsedSQLType, ParamsInfo, BatchSize);
          BatchCommand.SetSQL(BatchSQL);
          BatchCommand.FBatchIters := BatchSize;
          BatchCommand.FBatchOffset := Offset + n;

          if NeedPrepareOnBatch then
            BatchCommand.Prepare;
        end;

        BatchCommand.Execute;

        RowsAffected := RowsAffected + BatchCommand.RowsAffected;
        Inc(FParamsProcessed, BatchSize);
        Inc(n, BatchSize);
        LastBatchSize := BatchSize;
      end;
    finally
      if NeedPrepare and (BatchCommand.GetCursorState >= csPrepared) then
        BatchCommand.Unprepare;
    end;
  end;
end;

function TSqlCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TSqlCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

function TSqlCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prCanReadParams:
      Value := FCanReadParams;
    prCommandTimeout:
      Value := FCommandTimeout;
    prIsStoredProc:
      Value := FIsSProc;
    prNonBlocking:
      Value := FNonBlocking;
    prUseDescribeParams:
      Value := FUseDescribeParams;

    prRowsProcessed: begin
      if GetRowsAffected = -1 then
        Value := 0
      else
        Value := GetRowsAffected;
    end
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TSqlCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCanReadParams:
      FCanReadParams := Value;
    prCommandTimeout:
      FCommandTimeout := Value;
    prIsStoredProc:
      FIsSProc := Boolean(Value);
    prNonBlocking:
      FNonBlocking := Value;
    prUseDescribeParams:
      FUseDescribeParams := Value;
    prSensibleBCDMapping:
      FSensibleBCDMapping := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TSqlRecordSet }

constructor TSqlRecordSet.Create;
begin
  inherited;

  FFetchRows := 25;
  FCursorUpdate := True;
end;

destructor TSqlRecordSet.Destroy;
begin
  FreeAndNil(FFetchExecutor);

  inherited;
end;

procedure TSqlRecordSet.Disconnect;
begin
  if FCommand.FNonBlocking then
    BreakFetch;

  GetDisconnectedMode;

  inherited;
end;

procedure TSqlRecordSet.CreateCommand;
var
  Cmd: TSqlCommand;
begin
  Cmd := GetCommandClass.Create;
  Cmd.FRequestResultSet := True;
  SetCommand(Cmd);
end;

function TSqlRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TSqlFieldDesc;
end;

class function TSqlRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  case DataType of
    dtInt8, dtUInt8:
      Result := sizeof(byte);
  else
    Result := inherited GetBufferSize(DataType, LengthInChars);
  end;
end;

function TSqlRecordSet.GetDisconnectedMode: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    FDisconnectedMode := FCommand.FConnection.DisconnectedMode;
  Result := FDisconnectedMode;
end;

function TSqlRecordSet.IsCaseSensitive: boolean;
begin
  Result := False;
end;

procedure TSqlRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TSqlCommand(Value);
end;

function TSqlRecordSet.CanUseAllKeyFields: boolean;
begin
  Result := True;
end;

function TSqlRecordSet.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

function TSqlRecordSet.NeedInitFieldsOnFetch: boolean;
begin
  Result := FCommand.FIsSProc or FCommand.FOpenNext or
            inherited NeedInitFieldsOnFetch;
end;

function TSqlRecordSet.GenerateTableName(const CatalogName, SchemaName, TableName, DefaultCatalogName: string): string;
begin
  if (CatalogName <> '') and not SameText(CatalogName, DefaultCatalogName) then
    Result := Format('%s.%s.%s',
      [FCommand.SQLInfo.QuoteIfNeed(CatalogName),
       FCommand.SQLInfo.QuoteIfNeed(SchemaName),
       FCommand.SQLInfo.QuoteIfNeed(TableName)])
  else
  if SchemaName <> '' then
    Result := Format('%s.%s',
      [FCommand.SQLInfo.QuoteIfNeed(SchemaName),
       FCommand.SQLInfo.QuoteIfNeed(TableName)])
  else
    Result := Format('%s',
      [FCommand.SQLInfo.QuoteIfNeed(TableName)]);
end;

function TSqlRecordSet.IsMetaInfo: boolean;
begin
  Result := False;
end;

procedure TSqlRecordSet.SetIndexFieldNames(const Value: string);
begin
  if IsServerCursor and not CachedUpdates then // for TableTypeRecordSet
    DatabaseError(SLocalSortingServerCursor);

  inherited SetIndexFieldNames(Value);
end;

function TSqlRecordSet.CanFetchBack: boolean;
begin
  Result := IsServerCursor;
end;

function TSqlRecordSet.InternalFetch(FetchBack: boolean = False): boolean;
begin
  FCommand.Busy;
  try
    Result := inherited InternalFetch(FetchBack);
  finally
    FCommand.Release;
  end;
end;

procedure TSqlRecordSet.FetchAll;
begin
  if (FCommand.GetCursorState < csFetchingAll) or IsDynamicCursor then begin
    FCommand.SetCursorState(csFetchingAll);
    if FCommand.FNonBlocking then begin
      if FFetchExecutor <> nil then
        FFetchExecutor.Free;

      FFetchExecutor := TSqlThreadWrapper.Create(FCommand, DoFetchAll);
      FFetchExecutor.OnException := DoFetchException;
      FFetchExecutor.OnTerminate := DoFetchTerminate;
      FFetchExecutor.OnSendEvent := DoFetchSendEvent;
      FStringHeap.ThreadSafety := True;
      // FFetchExecutor.Resume; // in TCustomMSDataSet.SetActive
    end
    else
      DoFetchAll;
  end;
end;

procedure TSqlRecordSet.DoFetchAll; // FFetchExecutor.FThread context
begin
  while Fetch do
    if (FFetchExecutor <> nil) and FWaitForFetchBreak then
      Break;
end;

procedure TSqlRecordSet.EndFetchAll; // MainThread context
begin
  FStringHeap.ThreadSafety := False;

  if FCommand.FNonBlocking and (IndexFieldCount > 0) then
    SortItems;
end;

const
  FE_AFTERFETCH = 1;

procedure TSqlRecordSet.DoAfterFetch;
begin
  if Assigned(FOnAfterFetch) then begin
    if FCommand.FNonBlocking and (FFetchExecutor <> nil) and FFetchExecutor.InThread then
      FFetchExecutor.SendEvent(Pointer(FE_AFTERFETCH))
    else
      FOnAfterFetch();
  end;
end;

procedure TSqlRecordSet.DoFetchTerminate(Sender: TObject); // MainThread context
begin
  EndFetchAll;
end;

class function TSqlRecordSet.ExceptionIsCanceled(E: Exception): boolean;
begin
  Result := False;
end;

procedure TSqlRecordSet.DoFetchException(Sender: TObject; E: Exception; var Fail: boolean); // MainThread context
begin
  if ExceptionIsCanceled(E) then
    Fail := False
  else
  if E.Message = SOpeningWasCanceled then
    Fail := False
  else
    if (FCommand <> nil) and (FCommand.FConnection <> nil) and (E is {$IFDEF NODBACCESS}ECRError{$ELSE}EDAError{$ENDIF}) then
      FCommand.FConnection.DoError(E, Fail);
end;

procedure TSqlRecordSet.DoFetchSendEvent(Sender: TObject; Event: Pointer);
begin
  if NativeUInt(Event) = FE_AFTERFETCH then
    if Assigned(FOnAfterFetch) then
      FOnAfterFetch();
end;

function TSqlRecordSet.GetUidWithBraces: Boolean;
begin
  if (FCommand <> nil) and (FCommand.GetConnection <> nil) then
    Result := FCommand.GetConnection.UuidWithBraces
  else
    Result := True;
end;

procedure TSqlRecordSet.BreakFetch;
begin
  inherited;

  if FFetchExecutor <> nil then begin
    FFetchExecutor.Free;
    FFetchExecutor := nil;
  end;
end;

procedure TSqlRecordSet.SortItems;
begin
  // SortItems can be called during the command is executing or fetching
  if not FCommand.FNonBlocking or (FCommand.GetCursorState = csFetched) then
    inherited SortItems
  else begin
    FetchAll;
    if FFetchExecutor <> nil then
      FFetchExecutor.Resume;
  end;
end;

{$IFNDEF LITE}
procedure TSqlRecordSet.ApplyColumnsInfo(Columns: TCRColumnsInfo; ReadFieldsFromServer: boolean; DefaultTable: integer; AsteriskCount: integer);
var
  FieldDesc: TCRFieldDesc;
  IdentCase: TIdentCase;
  FieldName: string;
  ColumnInfo: TCRColumnInfo;
  i, j: integer;
begin
  if AsteriskCount > 0 then
    Exit;

  IdentCase := FCommand.SQLInfo.IdentCase;
  for i := 0 to Fields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(Fields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      continue;

    FieldName := FieldDesc.ActualName;
    for j := 0 to Columns.Count - 1 do begin
      ColumnInfo := Columns[j];
      if not ColumnInfo.Used and
        ((IdentCase <> icMixed) and (FieldName = ColumnInfo.Name) or
        (IdentCase = icMixed) and SameText(FieldName, ColumnInfo.Name)) and
        (ColumnInfo.TableIndex <> -1)
      then begin
        if ColumnInfo.Name <> '' then begin
          ColumnInfo.Used := True;

          if ColumnInfo.TableIndex <> - 1 then
            FieldDesc.TableInfo := TablesInfo[ColumnInfo.TableIndex];

          if ColumnInfo.Alias <> '' then
            FieldDesc.ActualName := ColumnInfo.Name;
          FieldDesc.DefaultExpr := ColumnInfo.Expr;
        end;
        break;
      end;
    end;
  end;
end;
{$ENDIF}

function TSqlRecordSet.FindTableInfoBySimpleName(const Name: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FTablesInfo.Count - 1 do
    if FTablesInfo.CaseSensitive and (TSqlTableInfo(FTablesInfo[i]).BaseTableName = Name) or
      not FTablesInfo.CaseSensitive and SameText(TSqlTableInfo(FTablesInfo[i]).BaseTableName, Name)
    then begin
      Result := i;
      Break;
    end;
end;

procedure TSqlRecordSet.FillTablesAliases;

  function AllBracketsClosed(const Text: String): Boolean;
  var
    i, Count: Integer;
  begin
    Count := 0;
    for i := 1 to Length(Text) do
      if Text[i] = '(' then
        Inc(Count)
      else
        if Text[i] = ')' then
          Dec(Count);
    Result := (Count = 0);
  end;

// For
//   SELECT FieldName FieldAlias FROM TableName TableAlias
// must be
//   Field.Name = 'FieldAlias'
//   Field.ActualName = 'FieldAlias'
//   Field.TableName = '' ???
//   Field.BaseColumnName = 'c_int'
//   Field.BaseTableName = 'ALL_TYPES'
var
  Parser: TSQLParser;
  TableName: string; // Table or view name
  StLex, Alias: string;
  Info: TSQLObjectInfo;
  Code, PrevCode: integer;
  TableInfo: TCRTableInfo;
  TableCount: integer;
  InNestedQuery, PrevWasNestedQuery: boolean;
  NestedTables: TList;
  i: Integer;
begin
  FTableInfoWasModified := False;
  TableCount := 0;
  InNestedQuery := False;
  PrevWasNestedQuery := False;
  Parser := FCommand.GetParserClass.Create(FCommand.SQL);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;

    NestedTables := TList.Create;
    try
      Code := Parser.ToLexem([lxWITH, lxSELECT]);
      if Code = lxWITH then
        Code := Parser.ToLexem(lxSELECT, True);

      if Code = lcEnd then
        Exit;

      if Parser.ToLexem(lxFROM, True) = lcEnd then
        Exit;

      //Clear aliases
      for i := 0 to FTablesInfo.Count - 1 do
        FTablesInfo[i].TableAlias := '';

      repeat
        repeat
          Code := Parser.GetNext(StLex); // Omit blank
        until Code <> lcBlank;

        if not PrevWasNestedQuery then begin
          if InNestedQuery then begin
            if Code = lxLeftBracket then begin
              Code := Parser.ToLexem([], True); // goto lxRightBracket

              if Code <> lcEnd then begin
                repeat
                  Code := Parser.GetNext(StLex); // Omit blank
                until Code <> lcBlank;
                TableName := StLex;
              end
              else
                TableName := '';
            end
            else
              TableName := StLex;
          end
          else begin
            if Code = lxLeftBracket then begin
              Code := Parser.ToLexem([lxFROM], True);

              if Code = lxFROM then begin
                InNestedQuery := True;

                repeat
                  Code := Parser.GetNext(StLex); // Omit blank
                until Code <> lcBlank;
                TableName := StLex;
              end
              else
                TableName := '';
            end
            else
              TableName := StLex;
          end;

          // TableName
          PrevCode := 0;
          while True do begin
            Code := Parser.GetNext(StLex);

            if Code = lcBlank then begin
              repeat
                Code := Parser.GetNext(StLex); // Omit blank
              until Code <> lcBlank;

              if (Code <> lxPoint) and (PrevCode <> lxPoint) and AllBracketsClosed(TableName) then
                Break;
            end;

            if (Code = lcEnd) or ((Code in [lxComma, lxSemicolon, lxRightBracket]) or
              ((Code >= lxSQLFirst) and (PrevCode <> lxPoint))) and AllBracketsClosed(TableName)
            then
              Break;

            PrevCode := Code;
            TableName := TableName + StLex;
          end;
        end
        else
          TableName := '';

        Inc(TableCount);

        if not (Code in [lcEnd, lxComma, lxSemicolon]) then begin
          if Code = lxAS then begin
            repeat
              Code := Parser.GetNext(Alias); // Omit blank
            until Code <> lcBlank;

            Code := Parser.GetNextToken;
          end
          else
          if Code = lcIdent then begin
            Alias := StLex;
            Code := Parser.GetNextToken;
          end
          else begin
            Alias := '';
            Parser.Back;
          end;

          if PrevWasNestedQuery then begin
  //          TableName := Alias;
            PrevWasNestedQuery := False;

            for i := 0 to NestedTables.Count - 1 do
              TCRTableInfo(NestedTables[i]).TableAlias := Alias;

            NestedTables.Clear;
          end;

          if Code <> lxComma then begin
            Code := Parser.ToLexem([lxComma, lxJOIN, lxWHERE, lxHAVING, lxGROUP, lxORDER, lxUNION], InNestedQuery);

            if Code in [lxWHERE, lxHAVING, lxGROUP, lxORDER] then
              Code := Parser.ToLexem([lxUNION], InNestedQuery);

            if Code = lxUNION then
              Code := Parser.ToLexem(lxFROM, True);

            if InNestedQuery and (Code = lxRightBracket) then begin
              InNestedQuery := False;
              PrevWasNestedQuery := True;
            end
            else
            if Code <> lcEnd then
              Code := lxComma;
          end;
        end
        else
          Alias := '';

        if TableName <> '' then begin
          TableName := FCommand.SQLInfo.NormalizeName(TableName, True, True);
          if TableName = '' then
            DatabaseError('TableName cannot be empty');

          FCommand.SQLInfo.SplitObjectName(TableName, Info);
          TableName := GenerateTableName(FCommand.SQLInfo.UnQuote(Info.Catalog), Info.Schema, Info.Name,
            FCommand.SQLInfo.NormalizeName(FCommand.FConnection.Database, False, True));

          if InNestedQuery or PrevWasNestedQuery then
            Alias := '';

          if Alias <> '' then
            Alias := FCommand.SQLInfo.NormalizeName(Alias, True, True);

          if (TableCount = 1) and (Code <> lxComma) and (FTablesInfo.Count = 1) then begin
            TableInfo := FTablesInfo[0];
            if not SameText(TableInfo.TableName, TableName) then begin
              Alias := TableName;
              TableInfo.IsView := True;
            end;
          end
          else begin
            TableInfo := FTablesInfo.FindByNameAndAlias(TableName, Alias);
            if TableInfo = nil then begin
              TableInfo := FTablesInfo.FindByName(TableName);
              if (TableInfo = nil) or (TableInfo.TableAlias <> '') then begin
                TableInfo := FTablesInfo.Add;
                TableInfo.TableName := TableName;
                TableInfo.TableAlias := '';
                TableInfo.IsView := True;
                FTableInfoWasModified := True;
              end
            end;
          end;

          if (TableInfo <> nil) and (InNestedQuery or PrevWasNestedQuery) then
            NestedTables.Add(TableInfo);

          if Alias <> '' then
            TableInfo.TableAlias := Alias;
        end;

      until (Code <> lxComma) and (Code <> lxRightBracket);
    finally
      NestedTables.Free;
    end;
  finally
    Parser.Free;
  end;
end;

function TSqlRecordSet.LoadKeyFieldsFromDB: boolean;
begin
  Result := False;
end;

procedure TSqlRecordSet.FillKeyFieldDescs(out KeyFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
var
  i: integer;
  FieldDesc: TSqlFieldDesc;
begin
  SetLength(KeyFieldDescs, 0);

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TSqlFieldDesc(FFields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      Continue;

    if ForceUseAllKeyFields or (FieldDesc.TableInfo = UpdatingTableInfo) then
      if FieldDesc.IsKey or FieldDesc.IsAutoIncrement then begin
        if Length(KeyFieldDescs) > 0 then
          if (KeyFieldDescs[High(KeyFieldDescs)].ActualName = FieldDesc.ActualName) and
             (TSqlFieldDesc(KeyFieldDescs[High(KeyFieldDescs)]).BaseTableName = FieldDesc.BaseTableName)
          then
            Continue;

        SetLength(KeyFieldDescs, Length(KeyFieldDescs) + 1);
        KeyFieldDescs[High(KeyFieldDescs)] := FieldDesc;
      end;
  end;

  if (Length(KeyFieldDescs) = 0) and LoadKeyFieldsFromDB then
    inherited FillKeyFieldDescs(KeyFieldDescs, ForceUseAllKeyFields);
end;

function TSqlRecordSet.ExtFieldsInfoIsInternal: boolean;
begin
  Result := True;
  FTableInfoWasModified := False;
end;

procedure TSqlRecordSet.DetectIdentityField;
var
  FieldDesc: TSqlFieldDesc;
  i: integer;
begin
  FIdentityField := nil;
  FTimeStampField := nil;

  for i := FFields.Count - 1 downto 0 do begin
    FieldDesc := TSqlFieldDesc(FFields[i]);
    if FieldDesc.FieldDescKind <> fdkData then
      continue;

    if FieldDesc.IsAutoIncrement then begin
      if FSetFieldsReadOnly then
        FieldDesc.ReadOnly := True;

      if (FieldDesc.TableInfo <> nil) and (FieldDesc.TableInfo = UpdatingTableInfo) then
        FIdentityField := FieldDesc;
    end;

    if FieldDesc.IsTimestamp and not FieldDesc.Hidden then
      if (FieldDesc.TableInfo <> nil) and (FieldDesc.TableInfo = UpdatingTableInfo) then
        FTimestampField := FieldDesc;
  end;
end;

procedure TSqlRecordSet.ExplicitInitFields;
var
  NeedReset: boolean;
begin
  NeedReset := GetCommand.GetCursorState <= csPrepared;
  try
    inherited;
  finally
    if NeedReset then begin
      GetCommand.SetCursorState(csInactive);
      GetCommand.CommandType := ctUnknown;
    end;
  end;
end;

function TSqlRecordSet.FieldListDependsOnParams: boolean;
begin
  Result := (FCommand <> nil) and FCommand.FIsSProc;
end;

function TSqlRecordSet.CreateBlob(Field: TFieldDesc): MemData.TBlob;
var
  IsUnicode: Boolean;
begin
  IsUnicode := (Field.DataType in [dtWideMemo, dtXML]);

{$IFDEF HAVE_COMPRESS}
  if FCommand.FCompressBlob <> cbNone then
    Result := TCompressedBlob.Create(IsUnicode)
  else
{$ENDIF}
    Result := MemData.TBlob.Create(IsUnicode);

  Result.EnableRollback;
end;

class function TSqlRecordSet.IsBlobDataType(DataType: word): boolean;
begin
  Result := (DataType = dtXML) or inherited IsBlobDataType(DataType);
end;

procedure TSqlRecordSet.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Blob: MemData.TBlob;
begin
  case Field.DataType of
    dtBlob, dtMemo, dtWideMemo, dtXML: begin
      Blob := CreateBlob(Field);
      SetBlob(Field, RecBuf, Blob);
  end
  else
    inherited;
  end;
end;

procedure TSqlRecordSet.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Handle: IntPtr;
  so: TSharedObject;
  b: boolean;
begin
  inherited;

  if Field.DataType = dtXML then begin
    Handle := Marshal.ReadIntPtr(RecBuf, Field.DataOffset);
    if Handle <> nil then begin
      so := TSharedObject(GetGCHandleTarget(Handle));
      b := (so <> nil) and (so.RefCount = 1);
      so.Free;
      if b then
        Marshal.WriteIntPtr(RecBuf, Field.DataOffset, nil);
    end;
  end;
end;

procedure TSqlRecordSet.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean);
var
  Blob: TBlob;
begin
  if DataType = dtXML then begin
    Blob := MemData.TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));
    if UseRollback and not Blob.UseRollback and Blob.CanRollback then begin
      Blob.UseRollback := True;
      try
        Value := Blob.AsString;
      finally
        Blob.UseRollback := False;
      end;
    end
    else
      Value := Blob.AsString;
  end
  else
    inherited;
end;

function TSqlRecordSet.GetFieldStrValue(RecBuf: IntPtr; Field: TFieldDesc): string;
var
  DataLen: Word;
begin
  if GetNull(Field, RecBuf) then
    Result := ''
  else begin
    DataLen := Marshal.ReadUInt16(RecBuf, Field.Offset);
    if Field.DataType in [dtWideString, dtWideMemo] then
      Result := string(Marshal.PtrToStringUni(PtrOffset(RecBuf, Field.DataOffset), DataLen))
    else
      Result := string(Marshal.PtrToStringAnsi(PtrOffset(RecBuf, Field.DataOffset), DataLen));
  end;
end;

function TSqlRecordSet.IsServerCursor: boolean;
begin
  Result := False;
end;

function TSqlRecordSet.IsStaticCursor: boolean;
begin
  Result := False;
end;

function TSqlRecordSet.IsDynamicCursor: boolean;
begin
  Result := False;
end;

procedure TSqlRecordSet.SetToBegin;
begin
  if IsStaticCursor then begin
    try
      FFetchFromBookmark := True;
      FBookmarkSize := sizeof(byte);
      FBookmarkValue := DBBMK_FIRST;
      Fetch;
    finally
      FFetchFromBookmark := False;
      FBookmarkSize := sizeof(FBookmarkValue);
    end;
  end
  else
  if IsDynamicCursor then
    while Fetch(True) do;

  inherited;
end;

procedure TSqlRecordSet.SetToEnd;
begin
  if IsStaticCursor then
    try
      FFetchFromBookmark := True;
      FBookmarkSize := sizeof(byte);
      FBookmarkValue := DBBMK_LAST;
      Fetch;
    finally
      FFetchFromBookmark := False;
      FBookmarkSize := sizeof(FBookmarkValue);
    end
  else
    FetchAll;

  inherited;
end;

function TSqlRecordSet.FetchToBookmarkValue: boolean; // Fetch to Bookmark. Bookmark value is stored in FBookmarkValue. Bookmark value used only for ctStatic and ctKeyset. For ctDynamic method refetched current record in specified direction
begin
  Assert(IsServerCursor);
  try
    FFetchFromBookmark := True;
    Result := Fetch;
  finally
    FFetchFromBookmark := False;
  end;
end;

procedure TSqlRecordSet.SetToBookmark(Bookmark: PRecBookmark);
begin
  if IsStaticCursor then begin
    // Cannot optimize - used to RefreshRecord
    // if (FBookmarkValue <> Bookmark.Order) or (CurrentItem = nil) then begin
    if IntPtr(Bookmark) <> nil then begin
      FBookmarkValue := Bookmark.Order;
      FetchToBookmarkValue;
    end;
  end;

  inherited;
end;

function TSqlRecordSet.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1), (1,0));
begin
  if IsStaticCursor then begin
    // Copied from TData.CompareBookmarks
    Result := RetCodes[IntPtr(Bookmark1) = nil, IntPtr(Bookmark2) = nil];
    if Result = 2 then begin
      if Bookmark1.Order >= Bookmark2.Order then
        if Bookmark1.Order = Bookmark2.Order then
          Result := 0
        else
          Result := 1
      else
        Result := -1
    end;
  end
  else
    Result := inherited CompareBookmarks(Bookmark1, Bookmark2);
end;

procedure TSqlRecordSet.InsertRecord(RecBuf: IntPtr);
var
  Field: TFieldDesc;
  i: integer;
begin
  if IsServerCursor then begin
    InternalAppend(RecBuf);

    if FCursorUpdate then begin
      if IntPtr(CurrentItem) = nil then begin
        if IntPtr(FirstItem) <> nil then
          CurrentItem := FirstItem
        else
        if IntPtr(LastItem) <> nil then
          CurrentItem := LastItem;
      end;
      if IntPtr(CurrentItem) = nil then
        CurrentItem := AppendItem
      else
        FreeComplexFields(PtrOffset(CurrentItem, sizeof(TItemHeader)), True);

      PutRecord(RecBuf);
      ReorderItems(CurrentItem, roInsert);

      if IsStaticCursor then begin
        FirstItem := nil;
        CurrentItem := nil;
        LastItem := nil;

        SetToEnd;
      end
      else begin
        Assert(IsDynamicCursor);
        if HasBlobFields then
          for i := 0 to FFields.Count - 1 do begin
            Field := FFields[i];
            if Field.IsBlob then
              GetBlob(Field, RecBuf).Commit;
          end;

        FirstItem := nil;
        CurrentItem := nil;

        FLastFetchBack := False;
        SetToEnd;
      end;
    end
    else begin
      if FRecordCount = 0 then begin
        CurrentItem := AppendItem;
        PutRecord(RecBuf);
        ReorderItems(CurrentItem, roInsert);

        CurrentItem := nil;
        FBof := False;
      end
      else begin
        FreeComplexFields(RecBuf, True);
        SetToEnd;
        CurrentItem := LastItem;

        FBof := IntPtr(FirstItem) = nil;
        FEof := IntPtr(LastItem) = nil;
      end;
    end
  end
  else
    inherited;
end;

procedure TSqlRecordSet.UpdateRecord(RecBuf: IntPtr);
begin
  inherited;
end;

procedure TSqlRecordSet.DeleteRecord;
begin
  if IsServerCursor then begin
    InternalDelete;
    RemoveRecord;
    if IsDynamicCursor then begin
      FRecordCount := - 1;
      FLastFetchBack := False;
    end;

    Fetch;
    CurrentItem := FirstItem;

    if IntPtr(CurrentItem) = nil then begin
      Fetch(True);
      CurrentItem := FirstItem;
    end;
  end
  else
    inherited;
end;

function TSqlRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := FCommand.FCommandTimeout;
    prCursorUpdate:
      Value := FCursorUpdate;
    prIsStoredProc:
      FCommand.GetProp(prIsStoredProc, Value);
    prNonBlocking:
      FCommand.GetProp(prNonBlocking, Value);
    prOpenNext:
      Value := FCommand.FOpenNext;
    prUseDescribeParams:
      Value := FCommand.FUseDescribeParams;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TSqlRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommand.FCommandTimeout := Value;
    prCursorUpdate:
      FCursorUpdate := Value;
    prIsStoredProc:
      FCommand.SetProp(prIsStoredProc, Value);
    prNonBlocking:
      FCommand.SetProp(prNonBlocking, Value);
    prOpenNext:
      FCommand.FOpenNext := Value;
    prUseDescribeParams:
      FCommand.FUseDescribeParams := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TSqlLoaderColumn }

constructor TSqlLoaderColumn.Create;
begin
  inherited;

  FSkiped := False;
  FParamDesc := nil;
  FDataArrPtr := nil;
end;

destructor TSqlLoaderColumn.Destroy;
begin
  Clear;

  inherited;
end;

procedure TSqlLoaderColumn.Clear;
begin
  if FDataArrPtr <> nil then begin
     Dispose(FDataArrPtr);
     FDataArrPtr := nil;
  end;
end;

procedure TSqlLoaderColumn.Init(ArraySize: Integer);
begin
  if ArraySize > 0 then begin
    if FDataArrPtr = nil then begin
      New(FDataArrPtr);
      SetLength(FDataArrPtr^, ArraySize);
    end
    else
      SetLength(FDataArrPtr^, ArraySize);
  end
  else
    Clear;
end;

end.
