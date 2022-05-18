
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualQuery;

interface

uses
{$IFDEF FPC}
  variants,
{$ENDIF}
  SysUtils, Classes, DB,
  CRTypes, MemData, CRAccess, CLRClasses,
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  DAConsts, DBAccess, DASQLGenerator,
  LiteCallVirtual, 
  CRVirtualData, CRVirtualQuery, CRVirtualConsts;


type
  TCustomVirtualQuery = class;
  TDataSetLink = class;
  TVirtualFunctionManager = class;
  TVirtualCollationManager = class;

  TVirtualFunction = function(InValues: array of Variant): Variant;
  TVirtualMethod = procedure(InValues: array of Variant; var ResultValue: Variant) of object;
  TRegisterFunctionsEvent = procedure(Sender: TObject; const FunctionManager: TVirtualFunctionManager) of object;

  TVirtualCollation = function(const Str1, Str2: string): Integer;
  TVirtualCollationMethod = function(const Str1, Str2: string): Integer of object;
  TVirtualAnsiCollation = function(const Str1, Str2: AnsiString): Integer;
  TVirtualAnsiCollationMethod = function(const Str1, Str2: AnsiString): Integer of object;
  TVirtualWideCollation = function(const Str1, Str2: WideString): Integer;
  TVirtualWideCollationMethod = function(const Str1, Str2: WideString): Integer of object;
  TRegisterCollationsEvent = procedure(Sender: TObject; const CollationManager: TVirtualCollationManager) of object;

  TMemDataSetData = class(TVirtualMemData)
  private
    FMemDataSet: TMemDataSet;
  public
    constructor Create(const MemDataSet: TMemDataSet);

    function Open(const Filter: PVirtualConstraint): TVirtualBookmark; override;

    procedure EditRecord(const Values: TVirtualValues); override;
    procedure InsertRecord(const Values: TVirtualValues); override;
  end;

  TDataSetFieldAccessor = class(TFieldAccessor)
    class function AsInteger(const Field, Buffer: IntPtr): Int64; override;
    class function AsFloat(const Field, Buffer: IntPtr): double; override;
    class function AsAnsiString(const Field, Buffer: IntPtr): AnsiString; override;
    class function AsWideString(const Field, Buffer: IntPtr): WideString; override;
  end;

  TVirtualDataSetIndex = class(TVirtualLocalIndex)
  protected
    function GetFieldPtr(const FieldIndex: integer): IntPtr; override;
    function GetBuffer(const Item: pointer): IntPtr; override;
    function InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer; override;
    function IsIntegerField(const FieldPtr: IntPtr): boolean; override;
    function IsDateTimeField(const FieldPtr: IntPtr): boolean; override;
    function IsFloatField(const FieldPtr: IntPtr): boolean; override;
    function IsAnsiStringField(const FieldPtr: IntPtr): boolean; override;
    function IsWideStringField(const FieldPtr: IntPtr): boolean; override;
  end;

  TDataSetData = class(TVirtualData)
  private
    FDataSet: TDataSet;
    FBookmarks: array of TBookmark;
    FSavedBookmark: TBookmark;
    FBookmark,
    FSavedRecNo: integer;
    FOpening,
    FNeedDisableControls: boolean;

    function FindBookmark(const Bookmark: TBookmark): integer;
  protected
    class function GetLocalIndexClass: TVirtualLocalIndexClass; override;
    function GetFieldAccessorClass: TFieldAccessorClass; override;

    function GetNextRecord(const Filter: PVirtualConstraint): boolean; override;

    procedure InternalGetCurrentRecord; override;
    procedure InternalNext; override;
    function InternalEof(const Filter: PVirtualConstraint): boolean; override;
    procedure InternalDescribeFields(const SpecificTypes: TSpecificTypes); override;

    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: variant): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean; overload; override;
  public
    constructor Create(const DataSet: TDataSet);
    destructor Destroy; override;

    function Active: boolean; override;

    function Open(const Filter: PVirtualConstraint): TVirtualBookmark; override;
    procedure Close; override;
    function Next(const Bookmark: TVirtualBookmark; const Filter: PVirtualConstraint): TVirtualBookmark; override;

    function GetBookmark: TVirtualBookmark; override;
    procedure GotoBookmark(const Bookmark: TVirtualBookmark); override;

    function GetRecordCount: integer; override;
    function GetFieldNull(FieldIndex: integer): boolean; override;
    function GetFieldValue(FieldIndex: integer): variant; override;

    procedure DisableControls(SaveRecNo: boolean); override;
    procedure EnableControls; override;

    procedure EditRecord(const Values: TVirtualValues); override;
    procedure InsertRecord(const Values: TVirtualValues); override;
    procedure DeleteRecord; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    function IsSimpleFieldType(const FieldIndex: Integer): boolean; override;
    function CompareInteger(const Field, Buffer: IntPtr; const Value: Int64): integer; override;
    function CompareFloat(const Field, Buffer: IntPtr; const Value: double): integer; override;
    function CompareAnsiString(const Field, Buffer: IntPtr; const Value: AnsiString): integer; override;
    function CompareWideString(const Field, Buffer: IntPtr; const Value: WideString): integer; override;
    function CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer; override;
    function GetLocalIndex(const Constraint: PVirtualConstraint): integer; override;
  end;

  TSourceDataLink = class(TDataLink)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TDataSetLink;
  protected
    procedure ActiveChanged; override;
  public
    constructor Create(Owner: TDataSetLink);
  end;

  TDataSetLink = class(TCollectionItem)
  private
    FSchemaName,
    FTableName: string;
    FDataSource: TDataSource;
    FDataLink: TSourceDataLink;

    function GetDataSet: TDataSet;
    procedure SetDataSet(const Value: TDataSet);
    function GetSchemaName: string;
    procedure SetSchemaName(const Value: string);
    function GetTableName: string;
    procedure SetTableName(const Value: string);

    function GetTableNameStored: boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property SchemaName: string read GetSchemaName write SetSchemaName;
    property TableName: string read GetTableName write SetTableName stored GetTableNameStored;
    property DataSet: TDataSet read GetDataSet write SetDataSet;
  end;

  TDataSetLinks = class(TCollection)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomVirtualQuery;

    function GetItem(Index: Integer): TDataSetLink;
    procedure SetItem(Index: Integer; Value: TDataSetLink);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomVirtualQuery);
    destructor Destroy; override;

    function Add: TDataSetLink; overload;
    function Add(DataSet: TDataSet): TDataSetLink; overload;
    function Add(DataSet: TDataSet; const TableName: string): TDataSetLink; overload;
    function Add(DataSet: TDataSet; const SchemaName: string; const TableName: string): TDataSetLink; overload;

    property Items[Index: Integer]: TDataSetLink read GetItem write SetItem; default;
  end;

  TVirtualSQLGenerator = class(TDASQLGenerator)
  end;

  TVirtualDataSetUpdater = class(TDADataSetUpdater)
  protected
    procedure SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean); override;
  public
    function PerformSQL(const SQL: string; const StatementTypes: TStatementTypes): boolean; override;
  end;

  TVirtualDataSetService = class(TDADataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    function DetectCanModify: boolean; override;
    function GetRecCount: integer; override;
  end;

  TVirtualTransaction = class(TDATransaction)
  protected
    function GetITransactionClass: TCRTransactionClass; override;
  end;

  TVirtualConnection = class(TCustomDAConnection)
  private
    FVirtualQuery: TCustomVirtualQuery;
  protected
    function GetIConnectionClass: TCRConnectionClass; override;
    function GetIRecordSetClass: TCRRecordSetClass; override;
    function GetITransactionClass: TCRTransactionClass; override;
    function GetIMetaDataClass: TCRMetaDataClass; override;

    procedure CreateIConnection; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
  public
    constructor Create(Owner: TComponent); override;

    function CreateTransaction: TDATransaction; override;
    function CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet; override;

    procedure RegisterDataSetLinks;
  end;

  TVirtualQueryOptions = class(TDADataSetOptions)
  private
    FTableCreationMode: TTableCreationMode;
    FAutoOpenSources: boolean;
    FUseUnicode: boolean;

    procedure SetTableCreationMode(const Value: TTableCreationMode);
    procedure SetAutoOpenSources(const Value: boolean);
    procedure SetUseUnicode(const Value: boolean);
  protected
    property TableCreationMode: TTableCreationMode read FTableCreationMode write SetTableCreationMode default cmAll;
  public
    constructor Create(Owner: TCustomDADataSet);
  published
    property AutoOpenSources: boolean read FAutoOpenSources write SetAutoOpenSources default False;
    property UseUnicode: boolean read FUseUnicode write SetUseUnicode default DefValUseUnicode;

    property FullRefresh;
    property TrimVarChar;
    property SetEmptyStrToNull;
    property ExtendedFieldsInfo default True;
    property SetFieldsReadOnly default True;
    property RequiredFields;
    property StrictUpdate;
    property PrepareUpdateSQL;
    property NumberRange;
    property QueryRecCount;
    property AutoPrepare;
    property ReturnParams;
    property TrimFixedChar;
    property LongStrings;
    property RemoveOnRefresh;
    property QuoteNames;
    property DetailDelay;
    property CacheCalcFields;
    property FieldOrigins;
    property UpdateBatchSize;
    property UpdateAllFields;
    property MasterFieldsNullable;
  end;

  TCustomVirtualQuery = class(TCustomDADataSet)
  private
    FInternalConnection: TVirtualConnection;
    FSourceDataSets: TDataSetLinks;
    FFunctionManager: TVirtualFunctionManager;
    FCollationManager: TVirtualCollationManager;
    FStreamedActive,
    FLockRegisterLinks,
    FOwnConnection: boolean;
    FOnRegisterFunctions: TRegisterFunctionsEvent;
    FOnRegisterCollations: TRegisterCollationsEvent;

    function GetKeepDesignConnected: boolean;
    procedure SetKeepDesignConnected(Value: boolean);
    procedure SetSourceDataSets(const Value: TDataSetLinks);
    function GetOptions: TVirtualQueryOptions;
    procedure SetOptions(const Value: TVirtualQueryOptions);
    procedure SetInternalConnection(const Value: TVirtualConnection);
    procedure SetLockRegisterLinks(Value: boolean);

    procedure LinkActiveChanged(Value: boolean);
    procedure InternalRegisterLinks;
  protected
    function GetDataSetServiceClass: TDataSetServiceClass; override;

    function CreateOptions: TDADataSetOptions; override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure Loaded; override;
    function UsedConnection: TCustomDAConnection; override;

    procedure SetActive(Value: boolean); override;
    procedure Disconnect(NeedClose: boolean = True); override;

    procedure InternalExecute(Iters: integer; Offset: integer); override;
    procedure InternalClose; override;
    procedure BeforeOpenCursor(InfoQuery: boolean); override;

    procedure InitFieldDefs; override;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; override;

    property KeepDesignConnected: boolean read GetKeepDesignConnected write SetKeepDesignConnected default False;
    property SourceDataSets: TDataSetLinks read FSourceDataSets write SetSourceDataSets;
    property Options: TVirtualQueryOptions read GetOptions write SetOptions;

    property OnRegisterFunctions: TRegisterFunctionsEvent read FOnRegisterFunctions write FOnRegisterFunctions;
    property OnRegisterCollations: TRegisterCollationsEvent read FOnRegisterCollations write FOnRegisterCollations;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TVirtualQuery = class(TCustomVirtualQuery)
  published
    property Active;
    property AutoCalcFields;
    property CachedUpdates;
    property Constraints stored IsConstraintsStored;
    property DataTypeMap;
    property Options;
    property Debug;
    property DetailFields;
    property FetchAll;
    property FetchRows;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FilterSQL;
    property IndexFieldNames;
    property KeyFields;
    property LocalUpdate;
    property Macros;
    property MasterFields;
    property MasterSource;
    property ParamCheck;
    property Params;
    property ReadOnly;
    property RefreshOptions;
    property SourceDataSets;
    property SQL;
    property SQLDelete;
    property SQLInsert;
    property SQLLock;
    property SQLRefresh;
    property SQLUpdate;
    property SQLRecCount;
    property UniDirectional;
    property UpdatingTable;

    property BeforeExecute;
    property AfterExecute;
    property BeforeUpdateExecute;
    property AfterUpdateExecute;
    property OnUpdateError;
    property OnUpdateRecord;
    property BeforeOpen;
    property AfterOpen;
    property BeforeFetch;
    property AfterFetch;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property AfterRefresh;
    property BeforeRefresh;

    property OnRegisterFunctions;
    property OnRegisterCollations;
  end;

  TVirtualFunctionManager = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TVirtualConnection;

    procedure UnregisterFunctions;
  public
    constructor Create(const Connection: TVirtualConnection);

    procedure RegisterFunction(const Name: string; ParamCount: Integer; VirtualFunction: TVirtualFunction); overload;
    procedure RegisterFunction(const Name: string; ParamCount: Integer; VirtualMethod: TVirtualMethod); overload;
  end;

  TVirtualCollationManager = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TVirtualConnection;

    procedure UnregisterCollations;
  public
    constructor Create(const Connection: TVirtualConnection);

    procedure RegisterCollation(const Name: string; VirtualCollation: TVirtualCollation); overload;
    procedure RegisterCollation(const Name: string; VirtualCollation: TVirtualCollationMethod); overload;
    procedure UnRegisterCollation(const Name: string);

    procedure RegisterAnsiCollation(const Name: string; VirtualAnsiCollation: TVirtualAnsiCollation); overload;
    procedure RegisterAnsiCollation(const Name: string; VirtualAnsiCollation: TVirtualAnsiCollationMethod); overload;
    procedure UnRegisterAnsiCollation(const Name: string);

    procedure RegisterWideCollation(const Name: string; VirtualWideCollation: TVirtualWideCollation); overload;
    procedure RegisterWideCollation(const Name: string; VirtualWideCollation: TVirtualWideCollationMethod); overload;
    procedure UnRegisterWideCollation(const Name: string);

    procedure RegisterDefaultCollations;
    procedure UnRegisterDefaultCollations;
  end;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CRProps, CRDataTypeMap,
  LiteClassesVirtual, LiteErrorVirtual, LitePropsVirtual;

const
  DataTypeMap: array [TFieldType] of word = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord
    dtUnknown, dtString, dtInt16, dtInteger, dtUInt16,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    dtBoolean, dtFloat, dtCurrency, dtBCD, dtDate, dtTime, dtDateTime,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, dtBlob, dtMemo,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    0, 0, 0, dtCursor, dtString, dtWideString,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    dtInt64, dtObject, dtArray, dtReference, dtTable, 0, 0,
    // ftVariant, ftInterface, ftIDispatch, ftGuid
    dtVariant, 0, 0, dtGuid
    // ftTimeStamp, ftFMTBcd
    {$IFNDEF FPC}, dtSQLTimeStamp{$ELSE}, 0{$ENDIF}, dtFmtBCD
  {$IFDEF FPC}
    // ftFixedWideChar, ftWideMemo
    , dtWideString, dtWideMemo
  {$ENDIF}
  {$IFDEF VER10P}
    // ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval
    , dtWideString, dtWideMemo, 0, 0
  {$IFDEF VER12P}
    // ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream
    , dtUInt32, dtInt8, dtUInt8, dtExtended, 0, 0, 0
  {$IFDEF VER14P}
    // ftTimeStampOffset, ftObject, ftSingle
    , dtSQLTimeStampOffset, 0, dtSingle
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
    );

type
  TMemDataSetHelper = class(TMemDataSet);

{ TMemDataSetData }

constructor TMemDataSetData.Create(const MemDataSet: TMemDataSet);
begin
  inherited Create(TMemDataSetHelper(MemDataSet).Data as TMemData);

  FMemDataSet := MemDataSet;
end;

function TMemDataSetData.Open(const Filter: PVirtualConstraint): TVirtualBookmark;
begin
  if not FMemDataSet.Active then
    raise Exception.Create(SDataSetNotOpened);

  Result := inherited Open(Filter);
end;

procedure TMemDataSetData.EditRecord(const Values: TVirtualValues);
var
  TmpBuf: TRecordBuffer;
begin
  TmpBuf := TMemDataSetHelper(FMemDataSet).GetNewRecBuf;
  FMemData.InitRecord(TmpBuf);
  if FMemData.HasComplexFields then
    FMemData.CreateComplexFields(TmpBuf, True);
  InternalPutRecord(Values, TmpBuf);

  FMemData.UpdateRecord(TmpBuf);
end;

procedure TMemDataSetData.InsertRecord(const Values: TVirtualValues);
var
  TmpBuf: TRecordBuffer;
begin
  TmpBuf := TMemDataSetHelper(FMemDataSet).GetNewRecBuf;
  FMemData.InitRecord(TmpBuf);
  if FMemData.HasComplexFields then
    FMemData.CreateComplexFields(TmpBuf, True);
  InternalPutRecord(Values, TmpBuf);

  FMemData.InsertRecord(TmpBuf);
end;

{ TDataSetFieldAccessor }

class function TDataSetFieldAccessor.AsInteger(const Field, Buffer: IntPtr): Int64;
begin
  Result := TField(Field).AsInteger;
end;

class function TDataSetFieldAccessor.AsFloat(const Field, Buffer: IntPtr): double;
begin
  Result := TField(Field).AsFloat;
end;

class function TDataSetFieldAccessor.AsAnsiString(const Field, Buffer: IntPtr): AnsiString;
begin
{$IFNDEF NEXTGEN}
  Result := AnsiString(TField(Field).{$IFDEF VER12P}AsAnsiString{$ELSE}AsString{$ENDIF});
{$ELSE}
  Result := AnsiString(TField(Field).AsString);
{$ENDIF}  
end;

class function TDataSetFieldAccessor.AsWideString(const Field, Buffer: IntPtr): WideString;
begin
  Result := WideString(TField(Field).{$IFDEF VER12P}AsWideString{$ELSE}AsString{$ENDIF});
end;

function TVirtualDataSetIndex.GetFieldPtr(const FieldIndex: integer): IntPtr;
begin
  Result := TDataSetData(FData).FDataSet.Fields[FieldIndex];
end;

function TVirtualDataSetIndex.GetBuffer(const Item: pointer): IntPtr;
begin
  Result := Item;
end;

function TVirtualDataSetIndex.InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer;
var
  Bookmark1, Bookmark2: integer;
  IntValue: Int64;
  FloatValue: Double;
  AStrValue: AnsiString;
  WStrValue: WideString;
begin
  Bookmark1 := integer({$IFNDEF FPC}Item1{$ELSE}(@Item1)^{$ENDIF});
  Bookmark2 := integer({$IFNDEF FPC}Item2{$ELSE}(@Item2)^{$ENDIF});

  TDataSetData(FData).GotoBookmark(Bookmark1);
  FNull1 := TField(FieldPtr).IsNull;
  TDataSetData(FData).GotoBookmark(Bookmark2);
  FNull2 := TField(FieldPtr).IsNull;

  if FNull1 then begin
    if FNull2 then begin
      Result := 0;
      Exit;
    end
    else begin
      Result := -1;
      Exit;
    end;
  end
  else if FNull2 then begin
    Result := 1;
    Exit;
  end;

  if IsIntegerField(FieldPtr) then begin
    IntValue := TDataSetFieldAccessor.AsInteger(FieldPtr, nil);
    Result := FData.CompareInteger(FieldPtr, Item1, IntValue);
  end
  else if IsFloatField(FieldPtr) then begin
    FloatValue := TDataSetFieldAccessor.AsFloat(FieldPtr, nil);
    Result := FData.CompareFloat(FieldPtr, Item1, FloatValue);
  end
  else if IsAnsiStringField(FieldPtr) then begin
    AStrValue := TDataSetFieldAccessor.AsAnsiString(FieldPtr, nil);
    Result := FData.CompareAnsiString(FieldPtr, Item1, AStrValue);
  end
  else if IsWideStringField(FieldPtr) then begin
    WStrValue := TDataSetFieldAccessor.AsWideString(FieldPtr, nil);
    Result := FData.CompareWideString(FieldPtr, Item1, WStrValue);
  end
  else
    Result := 0;
end;

function TVirtualDataSetIndex.IsIntegerField(const FieldPtr: IntPtr): boolean;
begin
  Result := (FieldPtr <> nil) and (TField(FieldPtr).DataType in [ftSmallint, ftInteger, ftWord, ftLargeint{$IFDEF VER12P}, ftLongWord, ftShortint, ftByte{$ENDIF}]);
end;

function TVirtualDataSetIndex.IsDateTimeField(const FieldPtr: IntPtr): boolean;
begin
  Result := (FieldPtr <> nil) and (TField(FieldPtr).DataType in [ftDate, ftTime, ftDateTime]);
end;

function TVirtualDataSetIndex.IsFloatField(const FieldPtr: IntPtr): boolean;
begin
  Result := IsDateTimeField(FieldPtr) or ((FieldPtr <> nil) and (TField(FieldPtr).DataType in [ftFloat, ftCurrency{$IFDEF VER14P}, ftExtended, ftSingle{$ENDIF}]));
end;

function TVirtualDataSetIndex.IsAnsiStringField(const FieldPtr: IntPtr): boolean;
begin
  Result := (FieldPtr <> nil) and (TField(FieldPtr).DataType in [ftString, ftFixedChar]);
end;

function TVirtualDataSetIndex.IsWideStringField(const FieldPtr: IntPtr): boolean;
begin
{$IFDEF VER12P}
  Result := (FieldPtr <> nil) and (TField(FieldPtr).DataType in [ftWideString, ftFixedWideChar]);
{$ELSE}
  Result := False;
{$ENDIF}
end;

{ TDataSetData }

constructor TDataSetData.Create(const DataSet: TDataSet);
begin
  inherited Create;

  FDataSet := DataSet;
  FBookmark := -1;
  FSavedRecNo := -1;
  FCanUseLocalindex := True;
end;

destructor TDataSetData.Destroy;
begin
  FDataSet := nil;

  inherited;
end;

function TDataSetData.FindBookmark(const Bookmark: TBookmark): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to High(FBookmarks) do
  {$IFNDEF VER12P}
  {$IFNDEF FPC}
    if Bookmark = FBookmarks[i] then begin
  {$ELSE}
    if CompareMem(@Bookmark[0], @FBookmarks[i][0], Length(Bookmark)) then begin
  {$ENDIF}
  {$ELSE}
    if CompareMem(@Bookmark[0], @FBookmarks[i][0], Length(Bookmark)) then begin
  {$ENDIF}
      Result := i;
      Break;
    end;
end;

class function TDataSetData.GetLocalIndexClass: TVirtualLocalIndexClass;
begin
  Result := TVirtualDataSetIndex;
end;

function TDataSetData.GetFieldAccessorClass: TFieldAccessorClass;
begin
  Result := TDataSetFieldAccessor;
end;

function TDataSetData.GetNextRecord(const Filter: PVirtualConstraint): boolean;
var
  Index, Bookmark: integer;
  Item: IntPtr;
begin
  if Filter^.LocalIndex < 0 then
    Result := inherited GetNextRecord(Filter)
  else begin
    Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter);

    if (Index >= 0) and (Length(Filter^.Items) > 1) then
      repeat
        Item := TVirtualDataSetIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        Bookmark := integer({$IFNDEF FPC}Item{$ELSE}(@Item)^{$ENDIF});
        GotoBookmark(Bookmark);

        if OmitRecord(Filter, Filter^.SortItemIndex) then
          Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
        else
          Break;
      until Index = -1;

    if Index >= 0 then begin
      if Length(Filter^.Items) <= 1 then begin
        Item := TVirtualDataSetIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
        Bookmark := integer({$IFNDEF FPC}Item{$ELSE}(@Item)^{$ENDIF});
        GotoBookmark(Bookmark);
      end;

      InternalGetCurrentRecord;
      Result := True;
    end
    else
      Result := False;
  end;
end;

procedure TDataSetData.InternalGetCurrentRecord;
begin
  // do nothing
end;

procedure TDataSetData.InternalNext;
begin
  if not FOpening then
    FDataSet.Next
  else
    FOpening := False;
end;

function TDataSetData.InternalEof(const Filter: PVirtualConstraint): boolean;
begin
  Result := FDataSet.Eof;
end;

procedure TDataSetData.InternalDescribeFields(const SpecificTypes: TSpecificTypes);
var
  i: integer;
begin
  inherited;

  if not Active then
    raise EVirtualQueryError.Create(SDataSetNotOpened);

  SetLength(FFields, FDataSet.Fields.Count);

  for i := 0 to Length(FFields) - 1 do begin
    FFields[i].Name := FDataSet.Fields[i].FieldName;
    if FDataSet.Fields[i].DataType > High(TFieldType) then
      FFields[i].DataType := dtUnknown
    else
      FFields[i].DataType := DataTypeMap[FDataSet.Fields[i].DataType];
    FFields[i].Length := FDataSet.Fields[i].Size;
//    FFields[i].Scale := FDataSet.Fields[i].Scale;
    FFields[i].IsKey := pfInKey in FDataSet.Fields[i].ProviderFlags;
    FFields[i].IsAutoIncrement := FFields[i].IsKey;
    FFields[i].Required := FDataSet.Fields[i].Required;
    FFields[i].ReadOnly := FDataSet.Fields[i].ReadOnly;
    FFields[i].ActualIndex := i;
  end;
end;

function TDataSetData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: variant): boolean;
var
  Field: TField;
  FieldValue: variant;

  function MatchesMask(St: string; Mask: string): boolean;
  const
    WildcardAst = '*';
    WildcardPct = '%';
    WildcardOne = '_';
  type
    TMatchesResult = (mrFalse, mrTrue, mrEnd);

    function SubMatchesMask(StIndex, MaskIndex: integer): TMatchesResult;
    begin
      while (MaskIndex <= Length(Mask)) and
        ((StIndex <= Length(St)) or
        ((Mask[MaskIndex] = WildcardAst) or (Mask[MaskIndex] = WildcardPct))) do begin
        if ((Mask[MaskIndex] = WildcardAst) or (Mask[MaskIndex] = WildcardPct)) then begin
          if MaskIndex = Length(Mask) then begin
            Result := mrTrue;
            Exit;
          end
          else
            case SubMatchesMask(StIndex, MaskIndex + 1) of
              mrTrue: begin
                Result := mrTrue;
                Exit;
              end;
              mrFalse:
                if StIndex > Length(St) then begin
                  Result := mrEnd;
                  Exit;
                end
                else
                  Inc(StIndex);
              mrEnd: begin
                Result := mrEnd;
                Exit;
              end;
            end;
        end
        else begin
          if (Mask[MaskIndex] = '\') and (MaskIndex < Length(Mask)) then
            Inc(MaskIndex);
          if (St[StIndex] = Mask[MaskIndex]) or ((Mask[MaskIndex] = WildcardOne) and (Mask[MaskIndex - 1] <> '\'))
          then begin
            Inc(StIndex);
            Inc(MaskIndex);
          end
          else begin
            Result := mrFalse;
            Exit;
          end;
        end;
      end;

      if StIndex > Length(St) then
        if MaskIndex > Length(Mask) then
          Result := mrTrue
        else
          Result := mrEnd
      else
        Result := mrFalse;
    end;
  begin
    Result := SubMatchesMask(1, 1) = mrTrue;
  end;

begin
  Field := FDataSet.Fields[FFields[FieldIndex].ActualIndex];
  case Field.DataType of
    ftBCD,
    ftFMTBcd:
      FieldValue := Field.AsFloat;
  else
    FieldValue := Field.Value;
  end;

  case Operation of
    ntEqual: Result := FieldValue = Value;
    ntMore: Result := FieldValue > Value;
    ntLess: Result := FieldValue < Value;
    ntMoreEqual: Result := FieldValue >= Value;
    ntLessEqual: Result := FieldValue <= Value;
    ntNoEqual: Result := FieldValue <> Value;
    ntLike: Result := MatchesMask(FieldValue, Value);
  else
    Result := False;
  end;
end;

function TDataSetData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean;
var
  FieldValue: Int64;
begin
  FieldValue := FDataSet.Fields[FFields[FieldIndex].ActualIndex].AsInteger;

  case Operation of
    ntEqual: Result := FieldValue = Value;
    ntMore: Result := FieldValue > Value;
    ntLess: Result := FieldValue < Value;
    ntMoreEqual: Result := FieldValue >= Value;
    ntLessEqual: Result := FieldValue <= Value;
    ntNoEqual: Result := FieldValue <> Value;
  else
    Result := False;
  end;
end;

function TDataSetData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean;
var
  FieldValue: double;
begin
  FieldValue := FDataSet.Fields[FFields[FieldIndex].ActualIndex].AsFloat;

  case Operation of
    ntEqual: Result := FieldValue = Value;
    ntMore: Result := FieldValue > Value;
    ntLess: Result := FieldValue < Value;
    ntMoreEqual: Result := FieldValue >= Value;
    ntLessEqual: Result := FieldValue <= Value;
    ntNoEqual: Result := FieldValue <> Value;
  else
    Result := False;
  end;
end;

function TDataSetData.Active: boolean;
begin
  Result := FDataSet.Active;
end;

function TDataSetData.Open(const Filter: PVirtualConstraint): TVirtualBookmark;
var
  Index, Bookmark: integer;
  Item: IntPtr;
begin
  FOpening := True;
  try
    Result := -1;
    if not Active then
      raise EVirtualQueryError.Create(SDataSetNotOpened);

    if Filter^.LocalIndex >= 0 then begin
      Index := TVirtualDataSetIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetItem(Filter);

      Bookmark := -1;
      if (Index >= 0) and (Length(Filter^.Items) > 1) then
        repeat
          Item := TVirtualDataSetIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
          Bookmark := integer({$IFNDEF FPC}Item{$ELSE}(@Item)^{$ENDIF});
          GotoBookmark(Bookmark);

          if OmitRecord(Filter, Filter^.SortItemIndex) then
            Index := TVirtualMemDataIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
          else
            Break;
        until Index = -1;

      if Index >= 0 then begin
        if Length(Filter^.Items) <= 1 then begin
          Item := TVirtualDataSetIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index];
          Bookmark := integer({$IFNDEF FPC}Item{$ELSE}(@Item)^{$ENDIF});
          GotoBookmark(Bookmark);
        end;

        InternalGetCurrentRecord;
        Result := Bookmark;
      end
      else
        Exit;
    end
    else begin
      FDataSet.First;
      Result := inherited Open(Filter);
    end;
  finally
    FOpening := False;
  end;
end;

procedure TDataSetData.Close;
begin
  SetLength(FBookmarks, 0);
end;

function TDataSetData.Next(const Bookmark: TVirtualBookmark; const Filter: PVirtualConstraint): TVirtualBookmark;
begin
  if Bookmark <> -1 then begin
    GotoBookmark(Bookmark);

    if GetNextRecord(Filter) then
      Result := GetBookmark
    else
      Result := -1;
  end
  else
    Result := -1;
end;

function TDataSetData.GetBookmark: TVirtualBookmark;
var
  Bookmark: TBookmark;
begin
  Bookmark := FDataSet.GetBookmark;
  FBookmark := FindBookmark(Bookmark);

  if FBookmark = -1 then begin
    FBookmark := length(FBookmarks);
    SetLength(FBookmarks, FBookmark + 1);
    FBookmarks[FBookmark] := Bookmark;
  end;

  Result := FBookmark;
end;

procedure TDataSetData.GotoBookmark(const Bookmark: TVirtualBookmark);
begin
  Assert(Bookmark < Length(FBookmarks));
  FBookmark := Bookmark;
  FDataSet.GotoBookmark(FBookmarks[FBookmark]);
end;

function TDataSetData.GetRecordCount: integer;
begin
  Result := FDataSet.RecordCount;
end;

function TDataSetData.GetFieldNull(FieldIndex: integer): boolean;
begin
  Result := FDataSet.Fields[FieldIndex].IsNull;
end;

function TDataSetData.GetFieldValue(FieldIndex: integer): variant;
begin
  Result := FDataSet.Fields[FieldIndex].Value;
end;

procedure TDataSetData.DisableControls(SaveRecNo: boolean);
begin
  FNeedDisableControls := not FDataSet.ControlsDisabled;
  if FNeedDisableControls then
    FDataSet.DisableControls;

  if SaveRecNo then
    FSavedRecNo := FDataSet.RecNo
  else if FSavedRecNo = -1 then
    FSavedBookmark := FDataSet.GetBookmark;
end;

procedure TDataSetData.EnableControls;
begin
  if FSavedRecNo <> -1 then
    FDataSet.RecNo := FSavedRecNo
  else
    FDataSet.GotoBookmark(FSavedBookmark);

  FSavedRecNo := -1;

  if FNeedDisableControls then
    FDataSet.EnableControls;
end;

procedure TDataSetData.EditRecord(const Values: TVirtualValues);
var
  i: integer;
  Field: TField;
begin
  FDataSet.Edit;
  try
    for i := 0 to Length(Values) - 1 do begin
      Field := FDataSet.Fields[i];
      if not Field.ReadOnly then
        Field.Value := Values[i].Value;
    end;
    FDataSet.Post;
  except
    FDataSet.Cancel;
    raise;
  end;
end;

procedure TDataSetData.InsertRecord(const Values: TVirtualValues);
var
  i: integer;
  Field: TField;
begin
  FDataSet.Insert;
  try
    for i := 0 to Length(Values) - 1 do begin
      Field := FDataSet.Fields[i];
      if not Field.ReadOnly and (Values[i].ValueType <> vrNull) then
        FDataSet.Fields[i].Value := Values[i].Value;
    end;
    FDataSet.Post;
  except
    FDataSet.Cancel;
    raise;
  end;
end;

procedure TDataSetData.DeleteRecord;
begin
  FDataSet.Delete;
end;

function TDataSetData.InTransaction: boolean;
begin
  Result := False;
end;

procedure TDataSetData.StartTransaction;
begin

end;

procedure TDataSetData.Commit;
begin

end;

procedure TDataSetData.Rollback;
begin
  // do nothing
end;

function TDataSetData.IsSimpleFieldType(const FieldIndex: Integer): boolean;
begin
  Result := FDataSet.Fields[FieldIndex].DataType in [ftSmallint, ftInteger, ftWord, ftLargeint, {$IFDEF VER12P}ftLongWord, ftShortint, ftByte,{$ENDIF}
                                                     ftFloat, ftCurrency, ftDate, ftTime, ftDateTime, {$IFDEF VER14P}ftExtended, ftSingle,{$ENDIF}
                                                     ftString, ftFixedChar{$IFDEF VER12P}, ftWideString, ftFixedWideChar{$ENDIF}];
end;

function TDataSetData.CompareInteger(const Field, Buffer: IntPtr; const Value: Int64): integer;
var
  Bookmark: integer;
begin
  Bookmark := integer({$IFNDEF FPC}Buffer{$ELSE}(@Buffer)^{$ENDIF});
  GotoBookmark(Bookmark);

  Result := inherited CompareInteger(Field, Buffer, Value);
end;

function TDataSetData.CompareFloat(const Field, Buffer: IntPtr; const Value: double): integer;
var
  Bookmark: integer;
begin
  Bookmark := integer({$IFNDEF FPC}Buffer{$ELSE}(@Buffer)^{$ENDIF});
  GotoBookmark(Bookmark);

  Result := inherited CompareFloat(Field, Buffer, Value);
end;

function TDataSetData.CompareAnsiString(const Field, Buffer: IntPtr; const Value: AnsiString): integer;
var
  Bookmark: integer;
begin
  Bookmark := integer({$IFNDEF FPC}Buffer{$ELSE}(@Buffer)^{$ENDIF});
  GotoBookmark(Bookmark);

  Result := inherited CompareAnsiString(Field, Buffer, Value);
end;

function TDataSetData.CompareWideString(const Field, Buffer: IntPtr; const Value: WideString): integer;
var
  Bookmark: integer;
begin
  Bookmark := integer({$IFNDEF FPC}Buffer{$ELSE}(@Buffer)^{$ENDIF});
  GotoBookmark(Bookmark);

  Result := inherited CompareWideString(Field, Buffer, Value);
end;

function TDataSetData.CompareVariant(const Field, Buffer: IntPtr; const Value: Variant): integer;
var
  Bookmark: integer;
  FieldValue: Variant;
begin
  Bookmark := integer({$IFNDEF FPC}Buffer{$ELSE}(@Buffer)^{$ENDIF});
  GotoBookmark(Bookmark);

  FieldValue := TField(Field).AsVariant;

  if FieldValue = Value then
    Result := 0
  else if FieldValue > Value then
    Result := 1
  else
    Result := -1;
end;

function TDataSetData.GetLocalIndex(const Constraint: PVirtualConstraint): integer;
var
  i: integer;
  IndexName: string;
  Index: TVirtualDataSetIndex;
begin
  IndexName := '';
  for i := 0 to Length(Constraint^.Items) - 1 do
    if Constraint^.Items[i].FieldIndex <> -100 then
      IndexName := IndexName + FFields[Constraint^.Items[i].FieldIndex].Name + ';';
  Result := FLocalIndexes.IndexOf(IndexName);

  if Result = -1 then begin
    Index := GetLocalIndexClass.Create(Self, FDataSet.RecordCount) as TVirtualDataSetIndex;
    Result := FLocalIndexes.AddObject(IndexName, Index);

    FDataSet.First;
    while not FDataSet.Eof do begin
      FBookmark := -1;
    {$IFNDEF FPC}
      Index.Add(pointer(GetBookmark));
    {$ELSE}
      i := GetBookmark;
      Index.Add(pointer((@i)^));
    {$ENDIF}
      FDataSet.Next;
    end;

    Index.Sort(FFields[Constraint^.Items[Constraint^.SortItemIndex].FieldIndex].ActualIndex);
  end;
end;

{ TSourceDataLink }

constructor TSourceDataLink.Create(Owner: TDataSetLink);
begin
  inherited Create;

  FOwner := Owner;
end;

procedure TSourceDataLink.ActiveChanged;
begin
  (FOwner.Collection as TDataSetLinks).FOwner.LinkActiveChanged(Active);
end;


{ TDataSetLink }

constructor TDataSetLink.Create(Collection: TCollection);
begin
  inherited;

  FDataLink := TSourceDataLink.Create(Self);
  FDataSource := TDataSource.Create(nil);
  FDataLink.DataSource := FDataSource;
end;

destructor TDataSetLink.Destroy;
begin
  FDataSource.DataSet := nil;
  FDataLink.DataSource := nil;
  FDataSource.Free;
  FDataLink.Free;

  inherited;
end;

function TDataSetLink.GetDataSet: TDataSet;
begin
  if FDataSource <> nil then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

procedure TDataSetLink.SetDataSet(const Value: TDataSet);
begin
  if DataSet <> Value then begin
    FDataSource.DataSet := Value;
  {$IFDEF FPC}
    Value.UpdateCursorPos;
  {$ENDIF}
  end;
end;

function TDataSetLink.GetSchemaName: string;
begin
  Result := FSchemaName;
end;

procedure TDataSetLink.SetSchemaName(const Value: string);
begin
  FSchemaName := Value;
end;

function TDataSetLink.GetTableName: string;
begin
  if FTableName <> '' then
    Result := FTableName
  else if DataSet <> nil then
    Result := DataSet.Name
  else
    Result := '';
end;

procedure TDataSetLink.SetTableName(const Value: string);
begin
  if FTableName <> Value then
    FTableName := Value;
end;

function TDataSetLink.GetTableNameStored: boolean;
begin
  Result := FTableName <> '';
end;

procedure TDataSetLink.AssignTo(Dest: TPersistent);
begin
  if Dest is TDataSetLink then begin
    TDataSetLink(Dest).DataSet := DataSet;
    TDataSetLink(Dest).SchemaName := FSchemaName;
    TDataSetLink(Dest).TableName := FTableName;
  end
  else
    inherited;
end;

function TDataSetLink.GetDisplayName: string;
begin
  Result := GetSchemaName;
  if Result <> '' then
    Result := Result + '.';
  Result := Result + GetTableName;

  if Result = '' then
    Result := ClassName;
end;

{ TDataSetLinks }

constructor TDataSetLinks.Create(AOwner: TCustomVirtualQuery);
begin
  inherited Create(TDataSetLink);

  FOwner := AOwner;
end;

destructor TDataSetLinks.Destroy;
begin
  inherited;
end;


function TDataSetLinks.GetItem(Index: Integer): TDataSetLink;
begin
  Result := TDataSetLink(inherited Items[Index]);
end;

procedure TDataSetLinks.SetItem(Index: Integer; Value: TDataSetLink);
begin
  inherited Items[Index] := Value;
end;

function TDataSetLinks.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TDataSetLinks.Update(Item: TCollectionItem);
begin
  if (not (csLoading in FOwner.ComponentState)) and FOwner.Active then
    FOwner.Close;
end;

function TDataSetLinks.Add: TDataSetLink;
begin
  Result := TDataSetLink(inherited Add);
end;

function TDataSetLinks.Add(DataSet: TDataSet): TDataSetLink;
begin
  Result := Add(DataSet, '', DataSet.Name);
end;

function TDataSetLinks.Add(DataSet: TDataSet; const TableName: string): TDataSetLink;
begin
  Result := Add(DataSet, '', TableName);
end;

function TDataSetLinks.Add(DataSet: TDataSet; const SchemaName: string; const TableName: string): TDataSetLink;
begin
  Result := Add;

  Result.DataSet := DataSet;
  if TableName <> '' then
    Result.TableName := TableName;
  Result.FSchemaName := SchemaName;
end;

{ TVirtualDataSetUpdater }

procedure TVirtualDataSetUpdater.SetUpdateQueryOptions(const StatementType: TStatementType; const IsAutoGeneratedSQL: Boolean);
var
  DestRecordSet: TCRRecordSet;
begin
  CheckIRecordSet; // can be inactive
  DestRecordSet := TDBAccessUtils.GetIRecordSet(UpdateQuery as TCustomDADataSet);

  DestRecordSet.SetProp(prExtendedFieldsInfo, False);
end;

function TVirtualDataSetUpdater.PerformSQL(const SQL: string; const StatementTypes: TStatementTypes): boolean;
begin
  try
    Result := inherited PerformSQL(SQL, StatementTypes);
  except
    on E: Exception do begin
      if (E is ESQLiteError) and (ESQLiteError(E).ErrorCode = SQLITE_EMPTY) then
        raise EVirtualQueryError.Create(SDataSetNotOpened)
      else
        raise;
    end;
  end;
end;

{ TVirtualDataSetService }

procedure TVirtualDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TVirtualDataSetUpdater.Create(Self));
end;

procedure TVirtualDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TVirtualSQLGenerator.Create(TDASQLGeneratorService));
end;

function TVirtualDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TVirtualDataSetService.GetRecCount: integer;
var
  St: string;
  UpdateQuery: TCustomDADataSet;
//  MonitorClass: TDASQLMonitorClass;
//  MessageID: cardinal;
begin
//  if Options.FTableCreationMode = cmOnDemand then
//    InternalRegisterLinks;

  Result := 0;
  St := Trim(FDataSet.SQLRecCount.Text);
  if St = '' then
    St := FSQLGenerator.GenerateRecCountSQL;

  TVirtualDataSetUpdater(Updater).CheckUpdateQuery(stCustom);
  UpdateQuery := TCustomDADataSet(TVirtualDataSetUpdater(Updater).UpdateQuery);
  UpdateQuery.SQL.Text := St;
  UpdateQuery.Params.Assign(FDataSet.Params);

//  MonitorClass := TDASQLMonitorClass(TDBAccessUtils.SQLMonitorClass(UsedConnection));
//  if MonitorClass.HasMonitor or FDataSet.Debug then
//    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, True);

  UpdateQuery.Open;
  if not UpdateQuery.EOF then
    Result := UpdateQuery.Fields[0].AsInteger;

//  if MonitorClass.HasMonitor or FDataSet.Debug then
//    MonitorClass.SQLExecute(FDataSet, St, UpdateQuery.Params, 'Get RecordCount', MessageID, False);
end;

{ TVirtualTransaction }

function TVirtualTransaction.GetITransactionClass: TCRTransactionClass;
begin
  Result := TCRVirtualTransaction;
end;

{ TVirtualConnection }

constructor TVirtualConnection.Create(Owner: TComponent);
begin
  inherited;

  LoginPrompt := False;
end;

function TVirtualConnection.GetIConnectionClass: TCRConnectionClass;
begin
  Result := TCRVirtualConnection;
end;

function TVirtualConnection.GetIRecordSetClass: TCRRecordSetClass;
begin
  Result := TCRVirtualRecordSet;
end;

function TVirtualConnection.GetITransactionClass: TCRTransactionClass;
begin
  Result := TCRVirtualTransaction;
end;

function TVirtualConnection.GetIMetaDataClass: TCRMetaDataClass;
begin
  Result := TCRVirtualMetaData;
end;

procedure TVirtualConnection.CreateIConnection;
begin
  if FIConnection = nil then begin
    SetIConnection(GetIConnection);
    FIConnection.SetProp(prTableCreationMode, TVirtualQueryOptions(FVirtualQuery.Options).FTableCreationMode);
    FIConnection.SetProp(prUseUnicode, TVirtualQueryOptions(FVirtualQuery.Options).FUseUnicode);
  end;
end;

procedure TVirtualConnection.DoConnect;
begin

  inherited;

  if TVirtualQueryOptions(FVirtualQuery.Options).FTableCreationMode = cmAll then
    FVirtualQuery.InternalRegisterLinks;

  if Assigned(FVirtualQuery.FOnRegisterFunctions) then
    FVirtualQuery.FOnRegisterFunctions(FVirtualQuery, FVirtualQuery.FFunctionManager);
  if Assigned(FVirtualQuery.FOnRegisterCollations) then
    FVirtualQuery.FOnRegisterCollations(FVirtualQuery, FVirtualQuery.FCollationManager);
end;

procedure TVirtualConnection.DoDisconnect;
begin
  FVirtualQuery.FFunctionManager.UnregisterFunctions;
  FVirtualQuery.FCollationManager.UnregisterCollations;

  inherited;
end;

function TVirtualConnection.CreateTransaction: TDATransaction;
begin
  Result := TVirtualTransaction.Create(nil);
  Result.DefaultConnection := Self;
end;

function TVirtualConnection.CreateDataSet(AOwner: TComponent = nil): TCustomDADataSet;
begin
  Result := TCustomVirtualQuery.Create(AOwner);
  TCustomVirtualQuery(Result).SetInternalConnection(Self);
  TCustomVirtualQuery(Result).SetLockRegisterLinks(True);
end;

procedure TVirtualConnection.RegisterDataSetLinks;
var
  i: integer;
  SourceDataSet: TDataSetLink;
begin
  for i := 0 to FVirtualQuery.FSourceDataSets.Count - 1 do begin
    SourceDataSet := FVirtualQuery.FSourceDataSets[i];

    if (SourceDataSet.DataSet is TMemDataSet) and (TMemDataSetHelper(SourceDataSet.DataSet).Data is TMemData) then
      TCRVirtualConnection(FIConnection).RegisterVirtualData(SourceDataSet.SchemaName, SourceDataSet.TableName,
        TMemDataSetData.Create(TMemDataSet(SourceDataSet.DataSet)))
    else if SourceDataSet.DataSet is TDataSet then
      TCRVirtualConnection(FIConnection).RegisterVirtualData(SourceDataSet.SchemaName, SourceDataSet.TableName,
        TDataSetData.Create(SourceDataSet.DataSet));
  end;
end;

{ TVirtualQueryOptions }

constructor TVirtualQueryOptions.Create(Owner: TCustomDADataSet);
begin
  inherited;

  FTableCreationMode := cmAll;
  FUseUnicode := DefValUseUnicode;
  ExtendedFieldsInfo := True;
  SetFieldsReadOnly := True;
end;

procedure TVirtualQueryOptions.SetTableCreationMode(const Value: TTableCreationMode);
begin
  if Value <> FTableCreationMode then begin
    TCustomVirtualQuery(FOwner).CheckInactive;
    FTableCreationMode := Value;
    if (TCustomVirtualQuery(FOwner).FIRecordSet <> nil) and (TCustomVirtualQuery(FOwner).FIRecordSet.GetConnection <> nil) then
      TCustomVirtualQuery(FOwner).FIRecordSet.GetConnection.SetProp(prTableCreationMode, Value);
  end;
end;

procedure TVirtualQueryOptions.SetAutoOpenSources(const Value: boolean);
begin
  FAutoOpenSources := Value;
end;

procedure TVirtualQueryOptions.SetUseUnicode(const Value: boolean);
begin
  if Value <> FUseUnicode then begin
    FUseUnicode := Value;
    if TCustomVirtualQuery(FOwner).FInternalConnection.FIConnection <> nil then begin
      TCustomVirtualQuery(FOwner).FInternalConnection.Disconnect;
      if TCustomVirtualQuery(FOwner).FInternalConnection.FIConnection <> nil then
        TCustomVirtualQuery(FOwner).FInternalConnection.FIConnection.SetProp(prUseUnicode, Value);
    end;
  end;
end;

{ TCustomVirtualQuery }

constructor TCustomVirtualQuery.Create(Owner: TComponent);
begin
  inherited;

  FSourceDataSets := TDataSetLinks.Create(Self);

  FOwnConnection := not (Owner is TVirtualQuery);
  if FOwnConnection then begin
    FInternalConnection := TVirtualConnection.Create(nil);
    FInternalConnection.FVirtualQuery := Self;
    KeepDesignConnected := False;
  end;

  FFunctionManager := TVirtualFunctionManager.Create(FInternalConnection);
  FCollationManager := TVirtualCollationManager.Create(FInternalConnection);
end;

destructor TCustomVirtualQuery.Destroy;
begin
  inherited;

  if FOwnConnection then
    FreeAndNil(FInternalConnection);
  FFunctionManager.Free;
  FCollationManager.Free;
  FSourceDataSets.Free;
end;

procedure TCustomVirtualQuery.Prepare;
begin
  if not Prepared and not Active then
    BeginConnection;

  if Options.FTableCreationMode = cmOnDemand then
    InternalRegisterLinks;

  inherited;
end;

function TCustomVirtualQuery.GetKeepDesignConnected: boolean;
begin
  Result := FInternalConnection.Options.KeepDesignConnected;
end;

procedure TCustomVirtualQuery.SetKeepDesignConnected(Value: boolean);
begin
  FInternalConnection.Options.KeepDesignConnected := Value;
end;

procedure TCustomVirtualQuery.SetSourceDataSets(const Value: TDataSetLinks);
begin
  FSourceDataSets.Assign(Value);
end;

function TCustomVirtualQuery.GetOptions: TVirtualQueryOptions;
begin
  Result := TVirtualQueryOptions(inherited Options);
end;

procedure TCustomVirtualQuery.SetOptions(const Value: TVirtualQueryOptions);
begin
  Options.Assign(Value);
end;

procedure TCustomVirtualQuery.SetInternalConnection(const Value: TVirtualConnection);
begin
  FInternalConnection := Value;
end;

procedure TCustomVirtualQuery.SetLockRegisterLinks(Value: boolean);
begin
  FLockRegisterLinks := Value;
end;

procedure TCustomVirtualQuery.LinkActiveChanged(Value: boolean);
var
  i: integer;
begin
  if Value and FStreamedActive then begin
    for i := 0 to FSourceDataSets.Count - 1 do
      if (FSourceDataSets[i].DataSet = nil) or not FSourceDataSets[i].DataSet.Active then
        Exit;

    if (FIRecordSet = nil) or (FIRecordSet.GetCommand.CommandType = ctCursor) then
      SetActive(True)
    else
      FInternalConnection.DoConnect;
  end
  else
  if not Value and
     not (csDestroying in ComponentState) and
     (FInternalConnection <> nil) and FInternalConnection.GetConnected
  then begin
    if not LocalUpdate then begin
      if (FIRecordSet = nil) or (FIRecordSet.GetCommand.CommandType = ctCursor) then
        SetActive(False)
      else
        FInternalConnection.DoDisconnect;
      FStreamedActive := True;
    end;

    FLockRegisterLinks := False;
  end;
end;

procedure TCustomVirtualQuery.InternalRegisterLinks;
begin
  if not FLockRegisterLinks then begin
    FLockRegisterLinks := True;
    FInternalConnection.RegisterDataSetLinks;
  end;
end;

function TCustomVirtualQuery.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := TVirtualDataSetService;
end;

function TCustomVirtualQuery.CreateOptions: TDADataSetOptions;
begin
  Result := TVirtualQueryOptions.Create(Self);
end;


procedure TCustomVirtualQuery.AssignTo(Dest: TPersistent);
var
  stIdx: TStatementType;
begin
  if Dest is TCustomVirtualQuery then begin
    TCustomVirtualQuery(Dest).CachedUpdates := CachedUpdates;
    TCustomVirtualQuery(Dest).LocalConstraints := LocalConstraints;
    TCustomVirtualQuery(Dest).LocalUpdate := LocalUpdate;
    TCustomVirtualQuery(Dest).MasterSource := MasterSource;
    TCustomVirtualQuery(Dest).MasterFields := MasterFields;
    TCustomVirtualQuery(Dest).DetailFields := DetailFields;
    TCustomVirtualQuery(Dest).DetailDelay := DetailDelay;

    TCustomVirtualQuery(Dest).ParamCheck := ParamCheck;  // before SQL
    TCustomVirtualQuery(Dest).SQL.Text := SQL.Text;
    for stIdx := Low(FUpdateSQL) to High(FUpdateSQL) do
      if Assigned(TCustomVirtualQuery(Dest).FUpdateSQL[stIdx]) and Assigned(FUpdateSQL[stIdx]) then
        TCustomVirtualQuery(Dest).FUpdateSQL[stIdx].Text := FUpdateSQL[stIdx].Text;

    TCustomVirtualQuery(Dest).FilterSQL := FilterSQL;
    TCustomVirtualQuery(Dest).Macros.Assign(Macros);
    TCustomVirtualQuery(Dest).Params.Assign(Params);
    TCustomVirtualQuery(Dest).Debug := Debug;

    TCustomVirtualQuery(Dest).FetchRows := FetchRows;
    TCustomVirtualQuery(Dest).UniDirectional := UniDirectional;
    TCustomVirtualQuery(Dest).AutoCommit := AutoCommit;
    TCustomVirtualQuery(Dest).RefreshOptions := RefreshOptions;
    TCustomVirtualQuery(Dest).UpdatingTable := UpdatingTable;
    TCustomVirtualQuery(Dest).KeyFields := KeyFields;
    TCustomVirtualQuery(Dest).LockMode := LockMode;
    TCustomVirtualQuery(Dest).DMLRefresh := DMLRefresh;
    TCustomVirtualQuery(Dest).Options.Assign(Options);
    TCustomVirtualQuery(Dest).DataTypeMap.Assign(DataTypeMap);
    TCustomVirtualQuery(Dest).Encryption.Assign(Encryption);
    TCustomVirtualQuery(Dest).SmartFetch.Assign(SmartFetch);
    TCustomVirtualQuery(Dest).Conditions.Assign(Conditions);

    TCustomVirtualQuery(Dest).FSourceDataSets.Assign(FSourceDataSets);
  end;
end;

procedure TCustomVirtualQuery.Loaded;
begin
  inherited;

  SetActive(FStreamedActive);
end;

function TCustomVirtualQuery.UsedConnection: TCustomDAConnection;
begin
  Result := FInternalConnection;
end;

procedure TCustomVirtualQuery.SetActive(Value: boolean);
var
  i: integer;
begin
  if Value <> Active then begin
    if (csLoading in ComponentState) then
      FStreamedActive := Value
    else if FStreamedActive then begin
      for i := 0 to FSourceDataSets.Count - 1 do
        if not FSourceDataSets[i].FDataLink.Active then begin
          inherited SetActive(False);
          Exit;
        end;

      FStreamedActive := False;

      inherited SetActive(True);
    end
    else begin
      FStreamedActive := False;

      if Value and Options.FAutoOpenSources then
        for i := 0 to FSourceDataSets.Count - 1 do
          FSourceDataSets[i].FDataLink.DataSet.Active := True;

      inherited SetActive(Value);
    end;
  end;
end;

procedure TCustomVirtualQuery.Disconnect(NeedClose: boolean = True);
begin
  if NeedClose then begin
    Close;
    UnPrepare;
    if FieldDefs <> nil then
      FieldDefs.Updated := False;
  end
  else if FIRecordSet <> nil then
    FIRecordSet.Disconnect;
end;

procedure TCustomVirtualQuery.InternalExecute(Iters: integer; Offset: integer);
begin
  if Options.FTableCreationMode = cmOnDemand then
    InternalRegisterLinks;

  inherited;
end;

procedure TCustomVirtualQuery.InternalClose;
begin
  inherited;

  if FOwnConnection then begin
    FInternalConnection.Disconnect;
    FLockRegisterLinks := False;
  end;
end;

procedure TCustomVirtualQuery.BeforeOpenCursor(InfoQuery: boolean);
begin
  if Options.FTableCreationMode = cmOnDemand then
    InternalRegisterLinks;

  inherited;
end;

procedure TCustomVirtualQuery.InitFieldDefs;
begin
  FInternalConnection.Connect;

  inherited;
end;

{ TVirtualFunctionManager }

constructor TVirtualFunctionManager.Create(const Connection: TVirtualConnection);
begin
  inherited Create;

  FConnection := Connection;
end;

procedure TVirtualFunctionManager.UnregisterFunctions;
var
  CRConnection: TSQLiteConnection;
begin
  if FConnection <> nil then begin
    CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
    CRConnection.GetFunctionManager.UnRegistrAllFunctions;
  end;
end;

procedure TVirtualFunctionManager.RegisterFunction(const Name: string; ParamCount: Integer; VirtualFunction: TVirtualFunction);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetFunctionManager.RegisterFunction(Name, ParamCount, VirtualFunction);
end;

procedure TVirtualFunctionManager.RegisterFunction(const Name: string; ParamCount: Integer; VirtualMethod: TVirtualMethod);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetFunctionManager.RegisterFunction(Name, ParamCount, VirtualMethod);
end;

{ TVirtualCollationManager }

constructor TVirtualCollationManager.Create(const Connection: TVirtualConnection);
begin
  inherited Create;

  FConnection := Connection;
end;

procedure TVirtualCollationManager.UnregisterCollations;
var
  CRConnection: TSQLiteConnection;
begin
  if FConnection <> nil then begin
    CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
    CRConnection.GetCollationManager.UnRegistrAllCollations;
  end;
end;

procedure TVirtualCollationManager.RegisterCollation(const Name: string; VirtualCollation: TVirtualCollation);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterCollation(Name, VirtualCollation);
end;

procedure TVirtualCollationManager.RegisterCollation(const Name: string; VirtualCollation: TVirtualCollationMethod);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterCollation(Name, VirtualCollation);
end;

procedure TVirtualCollationManager.UnRegisterCollation(const Name: string);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.UnRegisterCollation(Name);
end;

procedure TVirtualCollationManager.RegisterAnsiCollation(const Name: string; VirtualAnsiCollation: TVirtualAnsiCollation);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterAnsiCollation(Name, VirtualAnsiCollation);
end;

procedure TVirtualCollationManager.RegisterAnsiCollation(const Name: string; VirtualAnsiCollation: TVirtualAnsiCollationMethod);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterAnsiCollation(Name, VirtualAnsiCollation);
end;

procedure TVirtualCollationManager.UnRegisterAnsiCollation(const Name: string);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.UnRegisterAnsiCollation(Name);
end;

procedure TVirtualCollationManager.RegisterWideCollation(const Name: string; VirtualWideCollation: TVirtualWideCollation);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterWideCollation(Name, VirtualWideCollation);
end;

procedure TVirtualCollationManager.RegisterWideCollation(const Name: string; VirtualWideCollation: TVirtualWideCollationMethod);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterWideCollation(Name, VirtualWideCollation);
end;

procedure TVirtualCollationManager.UnRegisterWideCollation(const Name: string);
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.UnRegisterWideCollation(Name);
end;

procedure TVirtualCollationManager.RegisterDefaultCollations;
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.RegisterDefaultCollations;
end;

procedure TVirtualCollationManager.UnRegisterDefaultCollations;
var
  CRConnection: TSQLiteConnection;
begin
  CRConnection := TSQLiteConnection(TDBAccessUtils.GetIConnection(FConnection));
  CRConnection.GetCollationManager.UnRegisterDefaultCollations;
end;

end.
