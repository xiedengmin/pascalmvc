
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFEngineUni;

interface

{$IFDEF DBFENGINE}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Variants, DateUtils, Types, TypInfo,
{$IFDEF LOG_PACKETS}
  LogHandler,
{$ENDIF}
  FMTBcd,
  CRTypes, CLRClasses, CRProps, CRFunctions, CRAccess, CRParser, MemData, DAConsts,
  CRVirtualData, CRVirtualQuery,
  LiteClassesVirtual, LiteErrorVirtual, LiteCallVirtual, LiteFunctionVirtual,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFProps, DBFStructs, DBFDBase, DBFFoxPro, DBFHiPerSix,
  DBFConnection, DBFDataTypeMap, DBFIndexes, DBFParser, DBFFunction;
{$ELSE}
  DBFConstsUni, DBFPropsUni, DBFStructsUni, DBFDBaseUni, DBFFoxProUni, DBFHiPerSixUni,
  DBFConnectionUni, DBFDataTypeMapUni, DBFIndexesUni, DBFParserUni, DBFFunctionUni;
{$ENDIF}

type
  TSearchRecList = record
    List: TList;
    Valid: boolean;
  end;

  TDBFFieldAccessor = class(TFieldAccessor)
    class function AsInteger(const Field, Buffer: IntPtr): Int64; override;
    class function AsFloat(const Field, Buffer: IntPtr): double; override;
    class function AsAnsiString(const Field, Buffer: IntPtr): AnsiString; override;
    class function AsWideString(const Field, Buffer: IntPtr): WideString; override;
  end;

  TVirtualDBFIndexItem = class
    FRecNo: integer;
    FEmpty,
    FNull: boolean;
    FIntValue: Int64;
    FFloatValue: double;
    FStringValue: WideString;
  public
    constructor Create(const RecNo: integer);
  end;

  TVirtualDBFIndex = class(TVirtualLocalIndex)
  private
   FSortStream: TMemoryStream;
  protected
    function GetFieldPtr(const FieldIndex: integer): IntPtr; override;
    function GetBuffer(const Item: pointer): IntPtr; override;
    function InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer; override;
    function IsIntegerField(const FieldPtr: IntPtr): boolean; override;
    function IsDateTimeField(const FieldPtr: IntPtr): boolean; override;
    function IsFloatField(const FieldPtr: IntPtr): boolean; override;
    function IsAnsiStringField(const FieldPtr: IntPtr): boolean; override;
    function IsWideStringField(const FieldPtr: IntPtr): boolean; override;
  public
    destructor Destroy; override;

    procedure Sort(const FieldIndex: integer); override;
  end;

  TDBFData = class(TVirtualData)
  private
    FDatabase: string;
    FTableName: string;
    FFile: TDBFDBase;
    FIndexedSearchInitialized: boolean;
    FIndexedSearchRecNoLists: array of TSearchRecList;
    FIndexedSearchIndex: integer;
    FFirstValidKey: integer;
    FValidKeysFound: boolean;
    FUseIndexedSearch: boolean;
    FCodePage: TDBFCodePage;
    FConnectMode: TDBFConnectMode;
    FIndexOnReading: TDBFIndexKind;
    FIgnoreDataErrors,
    FIgnoreMetadataErrors,
    FIgnoreBrokenTables: boolean;
    FIdentifierCase: TDBFIdentifierCase;
    FAllFieldsAsNullable: boolean;

    procedure ClearSearchLists;
    function InternalGetNextRecord(const Filter: PVirtualConstraint): boolean;
  protected
    class function GetLocalIndexClass: TVirtualLocalIndexClass; override;
    function GetFieldAccessorClass: TFieldAccessorClass; override;
    procedure CreateSQLInfo; override;

    procedure InternalNext; override;
    function GetNextRecord(const Filter: PVirtualConstraint): boolean; override;
    function InternalEof(const Filter: PVirtualConstraint): boolean; override;
    function InternalCompareFieldValue(FieldNo: integer; Operation: TExpressionType; const Value: variant): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean; overload; override;
    function InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean; overload; override;
    procedure InternalDescribeFields(const SpecificTypes: TSpecificTypes); override;

    function GetFieldDataTypeName(const Field: PVirtualFieldDesc): string; override;
    function GetFieldDefinition(const Field: PVirtualFieldDesc): string; override;
  public
    constructor Create(const Database, TableName: string; DBFFormat: TDBFFormat = dfAuto);
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean;

    function GetDBFFormat(DBFFormat: TDBFFormat; var HeaderFormat: TDBFFormat): TDBFFormat;
    function Active: boolean; override;

    procedure Prepare; override;
    procedure PrepareFile;
    procedure PrepareConstraints(const Filter: PVirtualConstraint; var Cost: integer); override;

    function Open(const Filter: PVirtualConstraint): TVirtualBookmark; override;
    procedure Close; override;

    function GetBookmark: TVirtualBookmark; override;
    procedure GotoBookmark(const Bookmark: TVirtualBookmark); override;

    function GetRecordCount: integer; override;
    function GetFieldNull(FieldNo: integer): boolean; override;
    function GetFieldValue(FieldNo: integer): variant; overload; override;
    function GetFieldValue(FieldIndex: integer; var FieldNull: boolean): variant; overload; override;
    procedure FreeFieldValue(Value: variant); override;

    procedure EditRecord(const Values: TVirtualValues); override;
    procedure InsertRecord(const Values: TVirtualValues); override;
    procedure DeleteRecord; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure StartTransactionNoSQLite;
    procedure CommitNoSQLite;
    procedure RollbackNoSQLite;

    function IsSimpleFieldType(const FieldIndex: Integer): boolean; override;
    function GetLocalIndex(const Constraint: PVirtualConstraint): integer; override;

    property Database: string read FDatabase;
    property DBFFile: TDBFDBase read FFile;
  end;

  TDBFVirtualConnection = class(TCRVirtualConnection)
  protected
    function CreateSQLInfo: TSQLInfo; override;
  public
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetFunctionManagerClass: TSQLiteFunctionManagerClass; override;
    procedure Connect(const ConnectString: string); override;
  end;

  TDBFTable = class(TCollectionItem)
  public
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    VirtualTable: TSQLiteVirtualTable;
    SchemaName,
    TableName,
    NormalizedTableName,
    FileName: string;
    Created: boolean;
  end;

  TDBFTables = class(TCollection)
  private
    function GetTable(Index: integer): TDBFTable;
    procedure SetTable(Index: integer; Value: TDBFTable);
  public
    function Find(const TableName: string): TDBFTable;

    property Tables[Index: integer]: TDBFTable read GetTable write SetTable; default;
  end;

  TDBFDirectConnector = class;

  TDBFDatabase = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnector: TDBFDirectConnector;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRVirtualConnection;
    FPath: string;
    FTables: TDBFTables;
  public
    constructor Create(Owner: TDBFDirectConnector; const Connection: TCRVirtualConnection);
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean;

    procedure InitTables;
    function AcquireTable(TableName: string): TDBFTable;
    procedure InitTable(const Table: TDBFTable);
    procedure DeleteTable(TableName: string);

    property Path: string read FPath;
    property Tables: TDBFTables read FTables;
  end;

  TDBFDirectConnector = class(TCustomDBFDirectConnector)
  private
    FDatabase: TDBFDatabase;
    FDatabaseName: string;

    procedure InternalCreateTable(const Table: TDBFTable);
  public
    constructor Create(Owner: TDBFConnection; const UseUnicode: boolean); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;

    property Database: TDBFDatabase read FDatabase;
  end;

  TDBFTableAction = (taNone, taCreate, taDrop, taPack, taZap, taReindex, taAlter, taCreateIndex, taDropIndex);

  TDBFDirectCommand = class(TCRVirtualCommand)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDBFConnection: TDBFConnection;
    FTableName: string;
    FTableAction: TDBFTableAction;
    FMembers: TTableMembers;
    FPackMemo,
    FPackDbf: boolean;
    FAlterOperation: TAlterOperation;
  protected
    procedure CheckPrepareError(ErrorCode: integer); override;

    procedure InternalPrepare; override;
    procedure InternalExecute; override;
    procedure DetectTableAction;
  public
    procedure SetConnection(Value: TCRConnection); override;

    procedure Unprepare; override;
    procedure Execute; override;
    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; override;
  end;

  TDBFDirectRecordSet = class(TCRVirtualRecordSet)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDBFConnection: TDBFConnection;
  protected
    procedure CreateCommand; override;
  public
    procedure Reopen; override;
    procedure SetConnection(Value: TCRConnection); override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    property DBFConnection: TDBFConnection read FDBFConnection;
  end;

  TDBFMetaData = class(TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;

    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); override;
    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDBFDirectTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TDBFDirectLoader = class (TCRSimpleLoader)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TDBFConnection;
  protected
    procedure SetConnection(Value: TCRConnection); override;
    procedure CreateCommand; override;
  end;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

uses Math;

const
  FuzzFactor = 1000;
  DoubleResolution   = 1E-15 * FuzzFactor;

{ TDBFFieldAccessor }

class function TDBFFieldAccessor.AsInteger(const Field, Buffer: IntPtr): Int64;
begin
  Result := TVirtualDBFIndexItem(Buffer).FIntValue;
end;

class function TDBFFieldAccessor.AsFloat(const Field, Buffer: IntPtr): double;
begin
  Result := TVirtualDBFIndexItem(Buffer).FFloatValue;
end;

class function TDBFFieldAccessor.AsAnsiString(const Field, Buffer: IntPtr): AnsiString;
begin
  Result := AnsiString(TVirtualDBFIndexItem(Buffer).FStringValue);
end;

class function TDBFFieldAccessor.AsWideString(const Field, Buffer: IntPtr): WideString;
begin
  Result := TVirtualDBFIndexItem(Buffer).FStringValue;
end;

{ TVirtualDBFIndexItem }

constructor TVirtualDBFIndexItem.Create(const RecNo: integer);
begin
  inherited Create;

  FRecNo := RecNo;
  FEmpty := True;
end;

{ TVirtualDBFIndex }

destructor TVirtualDBFIndex.Destroy;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TVirtualDBFIndexItem(FItems[i]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};

  inherited;
end;

procedure TVirtualDBFIndex.Sort(const FieldIndex: integer);
var
  SavedStream: TStream;
  Cnt: integer;
begin
  SavedStream := TDBFData(FData).FFile.GetStream;
  FSortStream := TMemoryStream.Create;

  try
    FSortStream.SetSize(SavedStream.Size);
    SavedStream.Seek(LongInt(0), {$IFNDEF NEXTGEN}soFromBeginning{$ELSE}soBeginning{$ENDIF});
    FSortStream.CopyFrom(SavedStream, FSortStream.Size);

    TDBFData(FData).FFile.SetStream(FSortStream);

    inherited;
  finally
    TDBFData(FData).FFile.SetStream(SavedStream);
    FSortStream.Free;
  end;

  Cnt := FItems.Count;
  while (FStartIndex < Cnt) and (TVirtualDBFIndexItem(FItems[FStartIndex]).FNull) do
    Inc(FStartIndex);
end;

function TVirtualDBFIndex.GetFieldPtr(const FieldIndex: integer): IntPtr;
begin
  Result := IntPtr({$IFNDEF FPC}FieldIndex{$ELSE}(@FieldIndex)^{$ENDIF});
end;

function TVirtualDBFIndex.GetBuffer(const Item: pointer): IntPtr;
begin
  Result := Item;
end;

function TVirtualDBFIndex.InternalCompare(const FieldPtr: IntPtr; Item1, Item2: pointer): integer;
var
  IndexItem1, IndexItem2: TVirtualDBFIndexItem;
  ValuePtr1, ValuePtr2: IntPtr;
  FieldType: Word;
  Epsilon: double;

  function InternalGetIndexValue(const IndexItem: TVirtualDBFIndexItem; out ValuePtr: IntPtr): boolean;
  begin
    Result := False;

    if IndexItem.FEmpty then begin
      IndexItem.FEmpty := False;
      TDBFData(FData).GotoBookmark(IndexItem.FRecNo);
      TDBFData(FData).FFile.ReadRecord;
      Result := TDBFData(FData).FFile.GetFieldNull(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));

      if not Result then begin
        IndexItem.FNull := False;

        case FieldType of
          dbfNumeric,
          dbfFloat,
          dbfDouble: begin
            IndexItem.FFloatValue := TDBFData(FData).FFile.GetFieldAsFloat(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}), True);
            ValuePtr := @IndexItem.FFloatValue;
          end;
          dbfInteger: begin
            IndexItem.FIntValue := TDBFData(FData).FFile.GetFieldValue(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
            ValuePtr := @IndexItem.FIntValue;
          end;
          dbfChar,
          dbfVarChar: begin
            IndexItem.FStringValue := WideString(TDBFData(FData).FFile.GetFieldAsString(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF})));
            ValuePtr := @IndexItem.FStringValue;
          end;
          dbfDate: begin
            IndexItem.FStringValue := WideString(TDBFData(FData).FFile.GetFieldAsString(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF})));
            IndexItem.FFloatValue := EncodeDate(StrToInt(Copy(string(IndexItem.FStringValue), 1, 4)), StrToInt(Copy(string(IndexItem.FStringValue), 5, 2)), StrToInt(Copy(string(IndexItem.FStringValue), 7, 2)));
            ValuePtr := @IndexItem.FFloatValue;
          end;
        else
          IndexItem.FNull := True;
          Result := True;
        end;
      end
      else
        IndexItem.FNull := True;
    end
    else
    if not IndexItem.FNull then
        case FieldType of
          dbfNumeric,
          dbfFloat,
          dbfDouble,
          dbfDate:
            ValuePtr := @IndexItem.FFloatValue;
          dbfInteger:
            ValuePtr := @IndexItem.FIntValue;
          dbfChar,
          dbfVarChar:
            ValuePtr := @IndexItem.FStringValue
        else
          Result := True;
        end
    else
      Result := True;
  end;

begin
  ValuePtr1 := nil;
  ValuePtr2 := nil;

  IndexItem1 := TVirtualDBFIndexItem({$IFNDEF FPC}Item1{$ELSE}(@Item1)^{$ENDIF});
  IndexItem2 := TVirtualDBFIndexItem({$IFNDEF FPC}Item2{$ELSE}(@Item2)^{$ENDIF});

  FieldType := TDBFData(FData).FFile.GetFieldDbfType(Integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
  FNull1 := InternalGetIndexValue(IndexItem1, ValuePtr1);
  FNull2 := InternalGetIndexValue(IndexItem2, ValuePtr2);

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
  end
  else if (ValuePtr1 <> nil) and (ValuePtr2 <> nil) then begin
    case FieldType of
      dbfNumeric,
      dbfFloat,
      dbfDouble,
      dbfDate: begin
        if DoubleValueDelta = 0 then
          Epsilon := Math.Max(Min(Abs(double(ValuePtr1^)), Abs(double(ValuePtr2^))) * DoubleResolution, DoubleResolution)
        else
          Epsilon := DoubleValueDelta;

        if double(ValuePtr1^) > double(ValuePtr2^) then begin
          if (double(ValuePtr1^) - double(ValuePtr2^)) <= Epsilon then
            Result := 0
          else
            Result := 1;
        end
        else begin
          if (double(ValuePtr2^) - double(ValuePtr1^)) <= Epsilon then
            Result := 0
          else
            Result := -1;
        end;
      end;
      dbfInteger: begin
        if Int64(ValuePtr1^) = Int64(ValuePtr2^) then
          Result := 0
        else if Int64(ValuePtr1^) > Int64(ValuePtr2^) then
          Result := 1
        else
          Result := -1;
      end;
      dbfChar,
      dbfVarChar:
        Result := CompareStr({$IFNDEF FPC}WideString{$ELSE}AnsiString{$ENDIF}(ValuePtr1^), {$IFNDEF FPC}WideString{$ELSE}AnsiString{$ENDIF}(ValuePtr2^));
    else
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

function TVirtualDBFIndex.IsIntegerField(const FieldPtr: IntPtr): boolean;
var
  FieldType: Word;
begin
  FieldType := TDBFData(FData).FFile.GetFieldDbfType(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
  Result := FieldType = dbfInteger;
end;

function TVirtualDBFIndex.IsDateTimeField(const FieldPtr: IntPtr): boolean;
var
  FieldType: Word;
begin
  FieldType := TDBFData(FData).FFile.GetFieldDbfType(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
  Result := (FieldType = dbfDate) or (FieldType = dbfTime) or (FieldType = dbfTimeStamp);
end;

function TVirtualDBFIndex.IsFloatField(const FieldPtr: IntPtr): boolean;
var
  FieldType: Word;
begin
  FieldType := TDBFData(FData).FFile.GetFieldDbfType(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
  Result := (FieldType = dbfNumeric) or (FieldType = dbfFloat) or (FieldType = dbfDouble);
end;

function TVirtualDBFIndex.IsAnsiStringField(const FieldPtr: IntPtr): boolean;
begin
  Result := False;
end;

function TVirtualDBFIndex.IsWideStringField(const FieldPtr: IntPtr): boolean;
var
  FieldType: Word;
begin
  FieldType := TDBFData(FData).FFile.GetFieldDbfType(integer({$IFNDEF FPC}FieldPtr{$ELSE}(@FieldPtr)^{$ENDIF}));
  Result := (FieldType = dbfChar) or (FieldType = dbfVarChar);
end;

{ TDBFData }

constructor TDBFData.Create(const Database, TableName: string; DBFFormat: TDBFFormat = dfAuto);
var
  HeaderFormat: TDBFFormat;
begin
  inherited Create;

  FDatabase := Database;
  FTableName := TableName;
  FUseIndexedSearch := True;
  FCanUseLocalindex := True;
  FAutoClose := False;
  FCodePage := dpDefault;
  FConnectMode := cmShared;
  FIndexOnReading := ikNative;
  FIgnoreDataErrors := False;
  FIgnoreMetadataErrors := False;
  FIgnoreBrokenTables := False;
  FIdentifierCase := icOriginal;
  FAllFieldsAsNullable := False;

  DBFFormat := GetDBFFormat(DBFFormat, HeaderFormat);
  FFile := TDBFDBase.GetFileClass(DBFFormat).Create(Database, SQLInfo.UnQuote(TableName), HeaderFormat);
end;

destructor TDBFData.Destroy;
begin
  FFile.Free;

  inherited;
end;

function TDBFData.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCodePage:
      FCodePage := Value;
    prConnectMode: begin
      FConnectMode := Value;
      FAutoUnwind := FConnectMode = cmUnsafe;
      FAutoClose := FAutoUnwind or (FConnectMode in [cmShared, cmUnsafe]);
    end;
    prIndexOnReading:
      FIndexOnReading := Value;
    prIgnoreDataErrors:
      FIgnoreDataErrors := Value;
    prIgnoreMetadataErrors:
      FIgnoreMetadataErrors := Value;
    prIdentifierCase:
      FIdentifierCase := Value;
    prAllFieldsAsNullable:
      FAllFieldsAsNullable := Value;
    prIgnoreBrokenTables:
      FIgnoreBrokenTables := Value;
  end;
end;

function TDBFData.GetDBFFormat(DBFFormat: TDBFFormat; var HeaderFormat: TDBFFormat): TDBFFormat;
var
  fName: string;
  fs: TFileStream;
  hdr: TDBFDBase7Header;
  readed: integer;
  Mode: Word;
  FieldBuf: PDBFField;
  FieldSize: integer;
  FieldType: AnsiChar;
  CompatibleFormat: TDBFFormat;
begin
  Result := DBFFormat;

  if (FFile <> nil) and (Result = dfAuto) then begin
    Result := FFile.DBFFormat;
    HeaderFormat := FFile.HeaderFormat;
  end
  else begin
    fName := IncludeTrailingPathDelimiter(FDatabase) + FTableName;
    if ExtractFileExt(fName) = '' then
      fName := fName + DBF_EXT;
    if not FileExists(fName) then begin
      HeaderFormat := Result;
      Exit;
    end;

    Mode := fmOpenRead or fmShareDenyNone;
    fs := TFileStream.Create(fName, Mode);
    if fs <> nil then
    try
      readed := fs.Read(hdr, SizeOf(TDBFHeader));
      if readed < SizeOf(TDBFHeader) then
        raise Exception.CreateFmt('Invalid header size (%d): %s', [readed, fName]);

      if Result <> dfAuto then begin
        HeaderFormat := Result;
        Exit;
      end;
    {$IFDEF LOG_PACKETS}
      AddToLog(Format('%s Ver %X (%d, 4-6: %X, 3: %d, 7: %d), YYMMDD %s, NumRec %d, DataOffset %.4X, NumBytesRec %X, Flags: %.2X, LandDrv %d', [
        FTableName,
        hdr.DB3.version, hdr.DB3.version and 3, (hdr.DB3.version and $70) shr 4, (hdr.DB3.version and 8) shr 3, (hdr.DB3.version and $80) shr 7,
        DateTimeToStr(EncodeDate(hdr.DB3.year + 1900, hdr.DB3.month, hdr.DB3.day)),
        hdr.DB3.numRecs,
        hdr.DB3.dataOffset,
        hdr.DB3.recLen,
        hdr.DB3.flags,
        hdr.DB3.langDrv
        ]));
    {$ENDIF}

      if hdr.DB3.version = $E5 then
        Result := dfHiPerSix
      else if (hdr.DB3.version and $7F) = $75 then
        Result := dfFoxPro2
      else if (hdr.DB3.version and $F0) = $30 then
        Result := dfVisualFoxPro
      else if (hdr.DB3.version and $07) = 4 then
        Result := dfdBaseVII
      else if (hdr.DB3.version and DBF_DBASE4_DBT) <> 0 then
        Result := dfdBaseV // dfdBaseIV; - version same for both but V supports 3 more types
      else if (hdr.DB3.version and $03) = 3 then begin
        if (hdr.DB3.version and $08) = 8 then
          Result := dfClipper
        else
          Result := dfdBaseIII;
      end
      else
        raise Exception.Create(SInvalidTableHeader);

      HeaderFormat := Result;
      FieldSize := DBaseFieldSize;
      if HeaderFormat = dfdBaseVII then begin
        FieldSize := DBase7FieldSize;
        fs.Position := SizeOf(TDBFDBase7Header);
      end;

      FieldBuf := Marshal.AllocHGlobal(FieldSize);
      try
        while (fs.Position + FieldSize) <= hdr.DB3.dataOffset do begin
          readed := fs.Read(FieldBuf^, FieldSize);
          if ((readed > 0) and (PByte(FieldBuf)^ = DBF_SECTION_END)) or (readed < FieldSize) then
            Break;

          if HeaderFormat = dfdBaseVII then
            FieldType := FieldBuf^.DB7FType
          else
            FieldType := FieldBuf^.DB3FType;

          if FieldType in [DBF_TYPE_NUMERIC, DBF_TYPE_CHAR, DBF_TYPE_DATE, DBF_TYPE_LOGICAL, DBF_TYPE_MEMO] then
            CompatibleFormat := dfdBaseIII
          else if FieldType in [DBF_TYPE_AUTOINC, DBF_TYPE_TIMESTAMP, DBF_DB7_TYPE_DOUBLE, DBF_TYPE_BINARY] then
            CompatibleFormat := dfdBaseVII
          else if FieldType in [DBF_TYPE_INTEGER, DBF_TYPE_FLOAT, DBF_TYPE_GENERAL] then begin
            if Result <> dfdBaseVII then
              CompatibleFormat := dfVisualFoxPro
            else
              CompatibleFormat := dfdBaseVII;
          end
          else
            CompatibleFormat := dfVisualFoxPro;

          if (CompatibleFormat > Result) or ((Result = dfClipper) and (CompatibleFormat = dfVisualFoxPro)) then
            Result := CompatibleFormat;
        end;
      finally
        Marshal.FreeHGlobal(FieldBuf);
      end;
    finally
      fs.Free;
    end;
  end;
end;

class function TDBFData.GetLocalIndexClass: TVirtualLocalIndexClass;
begin
  Result := TVirtualDBFIndex;
end;

function TDBFData.GetFieldAccessorClass: TFieldAccessorClass;
begin
  Result := TDBFFieldAccessor;
end;

procedure TDBFData.CreateSQLInfo;
begin
  FSQLInfo := TVirtualSQLInfo.Create(TDBFParser, DBFKeywordLexems);
end;

procedure TDBFData.InternalNext;
begin
  // do nothing
end;

function TDBFData.GetNextRecord(const Filter: PVirtualConstraint): boolean;

  function ExistsInSecondaryLists(RecNo: integer): boolean;
  var
    i: integer;
  begin
    Result := True;
    // acts like "AND" for several filters
    for i := 1 to Length(FIndexedSearchRecNoLists) - 1 do
      if FIndexedSearchRecNoLists[i].Valid and (FIndexedSearchRecNoLists[i].List.IndexOf(pointer(NativeUInt(RecNo))) < 0) then begin
        Result := False;
        Break;
      end;
  end;

var
  idxNo: integer;
  keyBuffer: TBytes;
  idx: TDBFIndexFile;

  procedure FillEqual(ListIndex: integer);
  var
    FirstEntry, LastEntry: PDBFLinkedListEntry;
    isFound: boolean;
  begin
    FirstEntry := idx.FindFirstEntry(idxNo, @keyBuffer[0], isFound);
    try
      if isFound then begin
        LastEntry := idx.GetEqualRightEntry(FirstEntry, idxNo, @keyBuffer[0], isFound);
        try
          idx.FillListByEntries(FirstEntry, LastEntry, idxNo, FIndexedSearchRecNoLists[ListIndex].List);
        finally
          idx.FreeLinkedList(LastEntry);
        end;
      end;
    finally
      idx.FreeLinkedList(FirstEntry);
    end;
  end;

  procedure FillMoreAsc(ListIndex: integer);
  var
    FirstEntry, LastEntry: PDBFLinkedListEntry;
    isFound: boolean;
  begin
    FirstEntry := idx.FindLastEntry(idxNo, @keyBuffer[0], isFound);
    try
      if isFound then
        idx.IncEntryNo(FirstEntry, idxNo);
      LastEntry := idx.GetRightmostEntry(idxNo);
      try
        idx.FillListByEntries(FirstEntry, LastEntry, idxNo, FIndexedSearchRecNoLists[ListIndex].List);
      finally
        idx.FreeLinkedList(LastEntry);
      end;
    finally
      idx.FreeLinkedList(FirstEntry);
    end;
  end;

  procedure FillMoreDesc(ListIndex: integer);
  var
    FirstEntry, LastEntry: PDBFLinkedListEntry;
    isFound: boolean;
  begin
    FirstEntry := idx.GetLeftmostEntry(idxNo);
    try
      LastEntry := idx.FindFirstEntry(idxNo, @keyBuffer[0], isFound);
      try
        if isFound then
          idx.DecEntryNo(LastEntry, idxNo);
        idx.FillListByEntries(FirstEntry, LastEntry, idxNo, FIndexedSearchRecNoLists[ListIndex].List);
      finally
        idx.FreeLinkedList(LastEntry);
      end;
    finally
      idx.FreeLinkedList(FirstEntry);
    end;
  end;

  procedure FillLessAsc(ListIndex: integer);
  var
    FirstEntry, LastEntry: PDBFLinkedListEntry;
    isFound: boolean;
  begin
    FirstEntry := idx.GetLeftmostEntry(idxNo);
    try
      LastEntry := idx.FindFirstEntry(idxNo, @keyBuffer[0], isFound);
      try
        if isFound then
          idx.DecEntryNo(LastEntry, idxNo);
        idx.FillListByEntries(FirstEntry, LastEntry, idxNo, FIndexedSearchRecNoLists[ListIndex].List);
      finally
        idx.FreeLinkedList(LastEntry);
      end;
    finally
      idx.FreeLinkedList(FirstEntry);
    end;
  end;

  procedure FillLessDesc(ListIndex: integer);
  var
    FirstEntry, LastEntry: PDBFLinkedListEntry;
    isFound: boolean;
  begin
    FirstEntry := idx.FindLastEntry(idxNo, @keyBuffer[0], isFound);
    try
      if isFound then
        idx.IncEntryNo(FirstEntry, idxNo);
      LastEntry := idx.GetRightmostEntry(idxNo);
      try
        idx.FillListByEntries(FirstEntry, LastEntry, idxNo, FIndexedSearchRecNoLists[ListIndex].List);
      finally
        idx.FreeLinkedList(LastEntry);
      end;
    finally
      idx.FreeLinkedList(FirstEntry);
    end;
  end;

var
  i, recNo: integer;
begin
  Result := True;


  if (FIndexOnReading = ikLocal) and (Filter^.LocalIndex >= 0) then begin
    Result := InternalGetNextRecord(Filter);
    Exit;
  end;

  keyBuffer := nil;

  if FUseIndexedSearch and (FFile.IndexFileType in [itMDX, itCDX]) then begin
    if not FIndexedSearchInitialized then begin
      FValidKeysFound := False;
      FFirstValidKey := -1;
    end;

    if (Length(Filter^.Items) > 0) and (FFile.IndexFile <> nil) then begin
      if not FIndexedSearchInitialized then begin
        FIndexedSearchIndex := 0;
        SetLength(FIndexedSearchRecNoLists, Length(Filter^.Items));
        for i := 0 to High(Filter^.Items) do begin
          FIndexedSearchRecNoLists[i].List := TList.Create;
          if FFile.VirtualContainsValue(Filter^.Items[i].Value) then begin
            // todo: get TIntegerList of RecNo by expression
            case FFile.IndexFileType of
              itMDX: begin
                idx := FFile.IndexFile;
                idxNo := idx.GetIndexNo(FFields[Filter^.Items[i].FieldIndex].Name);
                FIndexedSearchRecNoLists[i].Valid := (idxNo >= 0) and idx.GetIndexIsValid(IdxNo);
                FValidKeysFound := FValidKeysFound or FIndexedSearchRecNoLists[i].Valid;
                if FIndexedSearchRecNoLists[i].Valid then begin
                  if FFirstValidKey < 0 then
                    FFirstValidKey := i;
                  keyBuffer := FFile.GetIndexKeyData(Filter^.Items[i].Value, Filter^.Items[i].FieldIndex, idxNo);
                  if Length(keyBuffer) > 0 then begin
                    // below on the left, equal from while key equal, above after that
                    case Filter^.Items[i].Operation of
                      ntEqual:
                        FillEqual(i);
                      ntMore:
                        if not idx.GetIndexIsDescending(idxNo) then
                          FillMoreAsc(i)
                        else
                          FillMoreDesc(i);
                      ntLess:
                        if not idx.GetIndexIsDescending(idxNo) then
                          FillLessAsc(i)
                        else
                          FillLessDesc(i);
                      ntMoreEqual:
                        if not idx.GetIndexIsDescending(idxNo) then begin
                          FillEqual(i);
                          FillMoreAsc(i);
                        end
                        else begin
                          FillMoreDesc(i);
                          FillEqual(i);
                        end;
                      ntLessEqual:
                        if not idx.GetIndexIsDescending(idxNo) then begin
                          FillLessAsc(i);
                          FillEqual(i);
                        end
                        else begin
                          FillEqual(i);
                          FillLessDesc(i);
                        end;
                      ntNoEqual:
                        if not idx.GetIndexIsDescending(idxNo) then begin
                          FillLessAsc(i);
                          FillMoreAsc(i);
                        end
                        else begin
                          FillMoreDesc(i);
                          FillLessDesc(i);
                        end;
                    else
                      raise Exception.CreateFmt('Unknown operation %d', [Ord(Filter^.Items[i].Operation)]);
                    end;
                  end;
                end;
              end;
              itCDX: begin
                idx := FFile.IndexFile;
                idxNo := idx.GetIndexNo(FFields[Filter^.Items[i].FieldIndex].Name);
                FIndexedSearchRecNoLists[i].Valid := (idxNo >= 0) and idx.GetIndexIsValid(IdxNo);
                FValidKeysFound := FValidKeysFound or FIndexedSearchRecNoLists[i].Valid;
                if FIndexedSearchRecNoLists[i].Valid then begin
                  if FFirstValidKey < 0 then
                    FFirstValidKey := i;
                  keyBuffer := FFile.GetIndexKeyData(Filter^.Items[i].Value, Filter^.Items[i].FieldIndex, idxNo);
                  if Length(keyBuffer) > 0 then begin
                    // below on the left, equal from while key equal, above after that
                    case Filter^.Items[i].Operation of
                      ntEqual:
                        FillEqual(i);
                      ntMore:
                        FillMoreAsc(i);
                      ntLess:
                        FillLessAsc(i);
                      ntMoreEqual: begin
                        FillEqual(i);
                        FillMoreAsc(i);
                      end;
                      ntLessEqual: begin
                        FillLessAsc(i);
                        FillEqual(i);
                      end;
                      ntNoEqual: begin
                        FillLessAsc(i);
                        FillMoreAsc(i);
                      end;
                    else
                      raise Exception.CreateFmt('Unknown operation %d', [Ord(Filter^.Items[i].Operation)]);
                    end;
                  end;
                end;
              end;
            else
              FIndexedSearchRecNoLists[i].Valid := False;
            end;
          end;
        end;

        FIndexedSearchInitialized := True;
      end;

      if FIndexedSearchInitialized and FValidKeysFound and (FFirstValidKey >= 0) then begin
        while True do begin
          // if there is no applicable indexes then "repeat" below
          // else ReadRecord that meets all requiremets and exit
          if FIndexedSearchIndex >= FIndexedSearchRecNoLists[FFirstValidKey].List.Count then begin
            Result := False;
            ClearSearchLists;
            Break;
          end
          else begin
            // index file RecNo 1 based
            recNo := integer(NativeUInt(FIndexedSearchRecNoLists[FFirstValidKey].List[FIndexedSearchIndex]));
            FFile.SetRecno(recNo - 1);
            Inc(FIndexedSearchIndex);
            FFile.ReadRecord;

            if FFile.RecordDeleted or (Length(FIndexedSearchRecNoLists) > 1) and not ExistsInSecondaryLists(recNo) or OmitRecord(Filter) then
              Continue
            else
              Break;
          end;
        end;

        Exit;
      end;
    end;
  end;

  repeat
    FFile.Next;

    if FFile.EOF then begin
      Result := False;
      Break;
    end;

    if not FFile.ReadRecord then
      Break;

    if FFile.RecordDeleted or OmitRecord(Filter) then
      Continue
    else
      Break;
  until False;

end;

function TDBFData.InternalEof(const Filter: PVirtualConstraint): boolean;
begin
  Result := True;
end;

function TDBFData.InternalCompareFieldValue(FieldNo: integer; Operation: TExpressionType; const Value: variant): boolean;
var
  temp, v: variant;

  function VarCompare: boolean;
  var
    rl: TVariantRelationship;
  begin
    case Operation of
      ntEqual:
        Result := VarCompareValue(temp, v) = vrEqual;
      ntMore:
        Result := VarCompareValue(temp, v) = vrGreaterThan;
      ntLess:
        Result := VarCompareValue(temp, v) = vrLessThan;
      ntMoreEqual: begin
        rl := VarCompareValue(temp, v);
        Result := (rl = vrGreaterThan) or (rl = vrEqual);
      end;
      ntLessEqual: begin
        rl := VarCompareValue(temp, v);
        Result := (rl = vrLessThan) or (rl = vrEqual);
      end;
      ntNoEqual:
        Result := VarCompareValue(temp, v) = vrNotEqual;
    else
      Result := False;
    end;
  end;

  function ResultByOperation(Operation: TExpressionType; CompareResult: integer): boolean;
  begin
    case Operation of
      ntEqual: Result := CompareResult = 0;
      ntMore: Result := CompareResult = 1;
      ntLess: Result := CompareResult = -1;
      ntMoreEqual: Result := CompareResult >= 0;
      ntLessEqual: Result := CompareResult <= 0;
      ntNoEqual: Result := CompareResult <> 0;
    else
      Result := False;
    end;
  end;

var
  SingleField, SingleValue: single;
  FloatField, FloatValue: double;
{$IFDEF FPC}
  vv: variant;
{$ENDIF}
  CompareResult: integer;
begin
  Result := False;

  if FFile.GetFieldNull(FieldNo) then
    temp := Null
  else
    temp := FFile.GetFieldValue(FieldNo);

  if VarIsEmpty(Value) then
    v := Null
  else
    v := Value;

  if VarIsNull(temp) or VarIsNull(v) then
    Exit;

  try
    if FFields[FieldNo].DataType = dtString then
      temp := TrimRight(temp);

    case FFields[FieldNo].DataType of
      dtSingle: begin
        SingleField := temp;
        SingleValue := v;
        CompareResult := Math.CompareValue(SingleField, SingleValue);
        Result := ResultByOperation(Operation, CompareResult);
      end;
      dtFloat,
      dtExtended: begin
        FloatField := temp;
        FloatValue := v;
        CompareResult := Math.CompareValue(FloatField, FloatValue);
        Result := ResultByOperation(Operation, CompareResult);
      end;
      dtFixedChar: begin
        if (FFile.DBFFormat <> dfVisualFoxPro) and ((VarIsNull(temp) or (AnsiString(temp) = '')) and (VarIsNull(v) or (AnsiString(v) = ''))) then begin
          CompareResult := 0;
          Result := ResultByOperation(Operation, CompareResult);
        end
        else
          Result := VarCompare;
      end;
    {$IFDEF FPC}
      dtDate: begin
        FloatField := temp;
        if CastValue(v, varDate, vv) then begin
          FloatValue := vv;
          CompareResult := Math.CompareValue(FloatField, FloatValue);
          Result := ResultByOperation(Operation, CompareResult);
        end
        else
          Result := False;
      end;
    {$ENDIF}
    else
      Result := VarCompare;
    end;
  finally
    if (TVarData(temp).VType and varByRef) <> 0 then
      TBlob(TVarData(temp).VPointer).Free;
  end;
end;

function TDBFData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: Int64): boolean;
var
  V: Variant;
begin
  V := Value;
  Result := InternalCompareFieldValue(FieldIndex, Operation, V);
end;

function TDBFData.InternalCompareFieldValue(FieldIndex: integer; Operation: TExpressionType; const Value: double): boolean;
var
  V: Variant;
begin
  V := Value;
  Result := InternalCompareFieldValue(FieldIndex, Operation, V);
end;

procedure TDBFData.InternalDescribeFields(const SpecificTypes: TSpecificTypes);
var
  i, idx: integer;
begin
  inherited;

  SetLength(FFields, FFile.FieldCount);
  idx := 1;

  for i := 0 to FFile.GetFieldCount - 1 do
    FFile.DescribeField(i, idx, FFields[i]);
end;

function TDBFData.GetFieldDataTypeName(const Field: PVirtualFieldDesc): string;
begin
  case Field^.DataType of
    dtFloat:
      if Field^.DBType = dbfDouble then
         Result := 'DOUBLE'
      else if Field^.DBType = dbfFloat then
         Result := 'FLOAT'
      else if Field.Scale = 0 then
        Result := 'FLOAT'
      else
        Result := 'NUMERIC';
  else
    Result := inherited GetFieldDataTypeName(Field);
  end;
end;

function TDBFData.GetFieldDefinition(const Field: PVirtualFieldDesc): string;
begin
  case Field^.DataType of
    dtFloat: begin
      Result := GetFieldDataTypeName(Field);
      if Field^.DBType = dbfNumeric then
        Result := Result + '(' + IntToStr(Field^.Length) + ', ' + IntToStr(Field^.Scale) + ')';
    end;
  else
    Result := inherited GetFieldDefinition(Field);
  end;
end;


function TDBFData.Active: boolean;
begin
  Result := FFile.Active;
end;

procedure TDBFData.Prepare;
begin
  if Prepared then
    Exit;

  PrepareFile;

  inherited;
end;

procedure TDBFData.PrepareFile;
begin
  FFile.CodePage := FCodePage;
  FFile.ConnectMode := FConnectMode;
  FFile.IndexOnReading := FIndexOnReading;
  FFile.IgnoreDataErrors := FIgnoreDataErrors;
  FFile.IgnoreMetadataErrors := FIgnoreMetadataErrors;
  FFile.IgnoreBrokenTables := FIgnoreBrokenTables;
  FFile.IdentifierCase := FIdentifierCase;
  FFile.AllFieldsAsNullable := FAllFieldsAsNullable;

  FFile.Prepare;
end;

procedure TDBFData.PrepareConstraints(const Filter: PVirtualConstraint; var Cost: integer);
begin
  if not FFile.Active then
    FFile.Open(True, False);

  inherited;
end;

procedure TDBFData.ClearSearchLists;
var
  i: integer;
begin
  for i := 0 to Length(FIndexedSearchRecNoLists) - 1 do
    FIndexedSearchRecNoLists[i].List.Free;
  FIndexedSearchRecNoLists := nil;
end;

function TDBFData.InternalGetNextRecord(const Filter: PVirtualConstraint): boolean;
var
  Index: integer;
begin
  Index := TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter);

  if Index >= 0 then
    repeat
      GotoBookmark(TVirtualDBFIndexItem(TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index]).FRecNo);
      FFile.ReadRecord;

      if FFile.RecordDeleted or OmitRecord(Filter, Filter^.SortItemIndex) then
        Index := TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
      else
        Break;
    until Index = -1;

  Result := Index >= 0;
end;

function TDBFData.Open(const Filter: PVirtualConstraint): TVirtualBookmark;
var
  Index: integer;
begin
  if not FFile.Active then
    FFile.Open(True, False);
  if Prepared then
    FFile.ReadHeader;

  FFile.Recno := -1;
  FIndexedSearchInitialized := False;
  // free lists if left, else memory leak sometime
  ClearSearchLists;

  if (FIndexOnReading = ikLocal) and (Filter <> nil) and (Filter^.LocalIndex >= 0) then begin
    Index := TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetItem(Filter);

    if Index >= 0 then
      repeat
        GotoBookmark(TVirtualDBFIndexItem(TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex])[Index]).FRecNo);
        FFile.ReadRecord;

        if FFile.RecordDeleted or OmitRecord(Filter, Filter^.SortItemIndex) then
          Index := TVirtualDBFIndex(FLocalIndexes.Objects[Filter^.LocalIndex]).GetNextItem(Filter^.CurrentItem + 1, Filter)
        else
          Break;
      until Index = -1;

    if Index >= 0 then
      Result := GetBookmark
    else
      Result := -1;
  end
  else
    Result := inherited Open(Filter);
end;

procedure TDBFData.Close;
begin
  FFile.Close;
  ClearSearchLists;

  inherited;
end;

function TDBFData.GetBookmark: TVirtualBookmark;
begin
  Result := TVirtualBookmark(FFile.RecNo + 1);
end;

procedure TDBFData.GotoBookmark(const Bookmark: TVirtualBookmark);
begin
  if Bookmark <> -1 then
    FFile.RecNo := Cardinal(NativeUInt(Bookmark)) - 1;
end;

function TDBFData.GetRecordCount: Integer;
begin
  Result := FFile.GetRecordCount;
end;

function TDBFData.GetFieldNull(FieldNo: integer): boolean;
begin
  Result := FFile.GetFieldNull(FieldNo);
end;

function TDBFData.GetFieldValue(FieldNo: integer): variant;
begin
  Result := FFile.GetFieldValue(FieldNo);
end;

function TDBFData.GetFieldValue(FieldIndex: integer; var FieldNull: boolean): variant;
begin
  Result := Unassigned;
  FieldNull := GetFieldNull(FieldIndex);
  if not FieldNull then begin
    Result := GetFieldValue(FieldIndex);
    if VarIsNull(Result) then begin
      if not (FIgnoreDataErrors or FAllFieldsAsNullable) then
        FFile.RaiseDataError(FieldIndex)
      else
        FieldNull := True;
    end;
  end;
end;

procedure TDBFData.FreeFieldValue(Value: variant);
begin
  if TVarData(Value).VType = varSharedObject then
    TBlob(TVarData(Value).VPointer).Free;
end;

procedure TDBFData.EditRecord(const Values: TVirtualValues);
begin
{$IFDEF LOG_PACKETS}
  FFile.ReadRecord;
  AddToLog(Format('EditRecord: %s', [string(GetFieldValue(0))]));
{$ENDIF}
  FFile.EditRecord(Values);
end;

procedure TDBFData.InsertRecord(const Values: TVirtualValues);
begin
{$IFDEF LOG_PACKETS}
  AddToLog(Format('InsertRecord: %s', [string(Values[0].Value)]));
{$ENDIF}
  FFile.InsertRecord(Values);
end;

procedure TDBFData.DeleteRecord;
begin
{$IFDEF LOG_PACKETS}
  AddToLog(Format('DeleteRecord: %s', [string(GetFieldValue(0))]));
{$ENDIF}
  FFile.RecordDeleted := True;
end;

function TDBFData.InTransaction: boolean;
begin
  Result := FFile.InTransaction;
end;

procedure TDBFData.StartTransaction;
begin
end;

procedure TDBFData.Commit;
begin
  // sqlite commits through sqlite3ModuleCommit after every ExecSQL operation
end;

procedure TDBFData.Rollback;
begin
  // since sqlite commits on every operation rollback messed
end;

procedure TDBFData.StartTransactionNoSQLite;
begin
  FFile.ClearTransactionData;
  FFile.InTransaction := True;
end;

procedure TDBFData.CommitNoSQLite;
begin
  FFile.ClearTransactionData;
  FFile.InTransaction := False;
end;

procedure TDBFData.RollbackNoSQLite;
begin
  if FFile.InTransaction then
    FFile.RollbackTransaction;
  FFile.ClearTransactionData;
  FFile.InTransaction := False;
end;

function TDBFData.IsSimpleFieldType(const FieldIndex: Integer): boolean;
begin
  Result := True;
end;

function TDBFData.GetLocalIndex(const Constraint: PVirtualConstraint): integer;
var
  i: integer;
  IndexName: string;
  Index: TVirtualDBFIndex;
begin
  if FIndexOnReading = ikNative then begin
    Result := -1;
    Exit;
  end;

  try
    IndexName := '';
    for i := 0 to Length(Constraint^.Items) - 1 do
      if Constraint^.Items[i].FieldIndex <> -100 then
        IndexName := IndexName + FFields[Constraint^.Items[i].FieldIndex].Name + ';';
    Result := FLocalIndexes.IndexOf(IndexName);

    if Result = -1 then begin
      Index := GetLocalIndexClass.Create(Self, FFile.GetRecordCount) as TVirtualDBFIndex;
      Result := FLocalIndexes.AddObject(IndexName, Index);

      for i := 1 to FFile.GetRecordCount do
        Index.Add(TVirtualDBFIndexItem.Create(i));

      Index.Sort(FFields[Constraint^.Items[Constraint^.SortItemIndex].FieldIndex].ActualIndex);
    end;
  except
    if SuppressIndexOpenErrors then
      Result := -1
    else
      raise;
  end;
end;

{ TDBFVirtualConnection }

function TDBFVirtualConnection.CreateSQLInfo: TSQLInfo;
begin
  Result := TDBFSQLInfo.Create(TSQLParser);
end;


function TDBFVirtualConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDBFDirectRecordset;
end;

function TDBFVirtualConnection.GetFunctionManagerClass: TSQLiteFunctionManagerClass;
begin
  Result := TDBFFunctionManager;
end;

procedure TDBFVirtualConnection.Connect(const ConnectString: string);
begin
  inherited;

  TDBFFunctionManager(GetFunctionManager).RegisterDBFFunctions;
end;

{ TDBFTables }

function TDBFTables.GetTable(Index: integer): TDBFTable;
begin
  Result := TDBFTable(Items[Index]);
end;

procedure TDBFTables.SetTable(Index: integer; Value: TDBFTable);
begin
  Items[Index] := Value;
end;

function TDBFTables.Find(const TableName: string): TDBFTable;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := GetTable(i);
    if SameText(Result.NormalizedTableName, TableName) then
      Exit;
  end;

  Result := nil;
end;

{ TDBFDatabase }

constructor TDBFDatabase.Create(Owner: TDBFDirectConnector; const Connection: TCRVirtualConnection);
begin
  inherited Create;

  FConnector := Owner;
  FConnection := Connection;

  FTables := TDBFTables.Create(TDBFTable);
end;

destructor TDBFDatabase.Destroy;
begin
  FTables.Free;

  inherited;
end;

function TDBFDatabase.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FPath := ExcludeTrailingPathDelimiter(VarToStrDef(Value, ''));
  end;
end;

procedure TDBFDatabase.InitTables;
var
  Rec: TSearchRec;
  Path, FileName: string;
  Table: TDBFTable;
begin
  Path := IncludeTrailingPathDelimiter(FPath);

  if FindFirst(path + '*.*', faAnyFile - faDirectory, rec) = 0 then begin
    try
      repeat
        FileName := string(Rec.Name);
        if LowerCase(ExtractFileExt(FileName)) = '.dbf' then begin
          Table := TDBFTable(FTables.Add);
          Table.FileName := FileName;
          Table.SchemaName := TCRVirtualConnection(FConnection).NormalizeTableName(FPath);
          Table.TableName := UpperCase(Copy(FileName, 1, Length(FileName) - 4));
          Table.NormalizedTableName := TCRVirtualConnection(FConnection).NormalizeTableName(Table.TableName);
        end;
      until FindNext(rec) <> 0;
    finally
      FindClose(rec);
    end;
  end;
end;

function TDBFDatabase.AcquireTable(TableName: string): TDBFTable;
var
  FileName: string;
  n: integer;
begin
  if FConnection.SQLInfo.IsQuoted(TableName) then
    TableName := FConnection.SQLInfo.UnQuote(TableName);
  n := Pos(UpperCase(FConnection.SQLInfo.UnQuote(FPath)), UpperCase(TableName));
  if n > 0 then begin
    Delete(TableName, 1, Length(FPath));
    while (Length(TableName) > 0) and CharInSet(TableName[1], ['.', '''', '"', '[']) do
      Delete(TableName, 1, 1);
  end;

  Result := FTables.Find(FConnection.NormalizeTableName(UpperCase(TableName)));

  if Result = nil then begin
    FileName := IncludeTrailingPathDelimiter(FPath) + TableName + DBF_EXT;
    if FileExists(FileName) then begin
      Result := TDBFTable(FTables.Add);
      Result.FileName := FileName;
      Result.SchemaName := TCRVirtualConnection(FConnection).NormalizeTableName(FPath);
      Result.TableName := TableName;
      Result.NormalizedTableName := TCRVirtualConnection(FConnection).NormalizeTableName(Result.TableName);
    end;
  end;

  if Result <> nil then
    try
      InitTable(Result);
    except
      on e: Exception do begin
        FTables.Delete(Result.Index);
        raise;
      end;
    end;
end;

procedure TDBFDatabase.InitTable(const Table: TDBFTable);
begin
  if not Table.Created then
    try
      FConnector.InternalCreateTable(Table);
      FConnection.DoCreateTable(Table.SchemaName, Table.TableName);
    except
      on e: Exception do begin
        FConnection.DoDropTable(Table.SchemaName, Table.TableName);
        raise;
      end;
    end;
end;

procedure TDBFDatabase.DeleteTable(TableName: string);
var
  Table: TDBFTable;
begin
  if FConnection.SQLInfo.IsQuoted(TableName) then
    TableName := FConnection.SQLInfo.UnQuote(TableName);
  Table := FTables.Find(FConnection.NormalizeTableName(UpperCase(TableName)));
  if Table <> nil then begin
    FConnection.DoDropTable(Table.SchemaName, Table.TableName);
    FTables.Delete(Table.Index);
  end;
end;

{ TDBFDirectConnector }

constructor TDBFDirectConnector.Create(Owner: TDBFConnection; const UseUnicode: boolean);
begin
  inherited;

  FDatabase := nil;
  FDatabaseName := '';

  FInternalConnection := TDBFVirtualConnection.Create(Self);
  FInternalConnection.SetProp(prUseUnicode, FUseUnicode);
end;

destructor TDBFDirectConnector.Destroy;
begin
  FDatabase.Free;

  inherited;
end;

procedure TDBFDirectConnector.InternalCreateTable(const Table: TDBFTable);
var
  Data: TDBFData;
begin
  Data := TDBFData.Create(FDatabase.Path, ExtractFileName(Table.FileName), FOwner.DBFFormat);

  Data.SetProp(prConnectMode, FOwner.ConnectMode);
  Data.SetProp(prCodePage, FOwner.CodePage);
  Data.SetProp(prIndexOnReading, FOwner.IndexOnReading);
  Data.SetProp(prIgnoreDataErrors, FOwner.IgnoreDataErrors);
  Data.SetProp(prIgnoreMetadataErrors, FOwner.IgnoreMetadataErrors);
  Data.SetProp(prIgnoreBrokenTables, FOwner.IgnoreBrokenTables);
  Data.SetProp(prIdentifierCase, FOwner.IdentifierCase);
  Data.SetProp(prAllFieldsAsNullable, FOwner.AllFieldsAsNullable);

  TCRVirtualConnection(FInternalConnection).RegisterVirtualData(FDatabase.Path, Table.TableName, Data);
  Table.VirtualTable := TCRVirtualConnection(FInternalConnection).Tables.Find(TCRVirtualConnection(FInternalConnection).NormalizeTableName(FDatabase.Path), Table.NormalizedTableName);
  Table.Created := True;
end;

procedure TDBFDirectConnector.Connect;
var
  Database: string;
begin
  if not DirectoryExists(FDatabase.Path) then
    raise Exception.CreateFmt(SInvalidPath, [FDatabase.Path]);

  FDatabase.InitTables;
  FInternalConnection.Connect('');
  Database := TCRVirtualConnection(FInternalConnection).NormalizeTableName(FDatabase.Path);
  TCRVirtualConnection(FInternalConnection).CheckSchema(Database, Database);
end;

procedure TDBFDirectConnector.Disconnect;
begin
  FInternalConnection.Disconnect;
end;

function TDBFDirectConnector.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase: begin
      FDatabaseName := Value;
      if FDatabase <> nil then
        FDatabase.Free;
      FDatabase := TDBFDatabase.Create(Self, TCRVirtualConnection(FInternalConnection));
      FDatabase.SetProp(prDatabase, FDatabaseName);
    end;
  end;
end;

{ TDBFDirectCommand }

procedure TDBFDirectCommand.InternalPrepare;
begin
  if FTableAction = taNone then
    DetectTableAction;

  if FTableAction = taNone then
    repeat
      inherited;
      Break;
    until False;
end;

procedure TDBFDirectCommand.InternalExecute;
var
  Connector: TDBFDirectConnector;
  Table: TSQLiteVirtualTable;
  DBFData: TDBFData;
  Database,
  DBFFormat,
  ConnectMode: variant;
  HeaderFormat: TDBFFormat;
  AutoUnwind: boolean;
  nullFlagsMembers: TTableMembers;
  nullableLenBefore, nullableLenAfter: integer;

  function GetTable: TSQLiteVirtualTable;
  var
    DBFTable: TDBFTable;
  begin
    DBFTable := Connector.FDatabase.AcquireTable(FTableName);
    if DBFTable <> nil then
      Result := DBFTable.VirtualTable
    else
      Result := nil;
  end;

  procedure DetachTable;
  begin
    Connector.FDatabase.DeleteTable(FTableName);
  end;

  procedure AttachTable;
  begin
    Connector.FDatabase.AcquireTable(FTableName);
  end;

  procedure CreateData;
  begin
    DBFData := TDBFData.Create(Database, SQLInfo.UnQuote(FTableName), DBFFormat);
    DBFData.SetProp(prConnectMode, cmExclusive);
    DBFData.SetProp(prCodePage, FDBFConnection.CodePage);
    DBFData.SetProp(prIndexOnReading, ikNative);
    DBFData.SetProp(prIgnoreDataErrors, False);
    DBFData.SetProp(prIgnoreMetadataErrors, False);
    DBFData.SetProp(prIgnoreBrokenTables, False);
    DBFData.SetProp(prIdentifierCase, FDBFConnection.IdentifierCase);
    DBFData.SetProp(prAllFieldsAsNullable, FDBFConnection.AllFieldsAsNullable);
  end;

begin
  if FTableAction <> taNone then begin
    Connector := TDBFDirectConnector(FDBFConnection.Connector);
    FDBFConnection.GetProp(prDBFFormat, DBFFormat);
    FDBFConnection.GetProp(prDatabase, Database);
    FDBFConnection.GetProp(prConnectMode, ConnectMode);
    AutoUnwind := TDBFConnectMode(ConnectMode) = cmUnsafe;

    case FTableAction of
      taCreate: begin
        Table := GetTable;
        if Table <> nil then
          raise Exception.CreateFmt(STableExists, [FTableName]);

        CreateData;
        try
          DBFData.FFile.CreateTable(Database, SQLInfo.UnQuote(FTableName), FMembers);
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taDrop: begin
        Table := GetTable;
        if Table <> nil then begin
          DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
          DetachTable;

          CreateData;
          try
            if not DBFData.FFile.Prepared then
              DBFData.FFile.Prepare;
            DBFData.FFile.DropTable;
          finally
            DBFData.Free;
          end;
        end
        else
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
      end;
      taPack: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        try
          if not DBFData.FFile.Prepared then
            DBFData.FFile.Prepare;
          DBFData.FFile.PackTable(FPackMemo, FPackDbf);
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taZap: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        try
          if not DBFData.FFile.Prepared then
            DBFData.FFile.Prepare;
          DBFData.FFile.ZapTable;
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taReindex: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        try
          if not DBFData.FFile.Prepared then
            DBFData.FFile.Prepare;
          DBFData.FFile.ReindexTable;
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taAlter: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        if not DBFData.FFile.Prepared then
          DBFData.FFile.Prepare;
        DBFData.FFile.Open;
        nullableLenBefore := DBFData.FFile.GetNullFlagsLength(DBFData.FFile.GetNullableFieldCount);
        try
          // todo: FMembers requires Expression for indexes
          case FAlterOperation of
            aoAddField: begin
              DBFData.FFile.AlterTableAddField(FMembers);
              nullableLenAfter := DBFData.FFile.GetNullFlagsLength(DBFData.FFile.GetNullableFieldCount);
              if nullableLenAfter > 0 then
                if DBFData.FFile.NullMaskFieldIndex <= 0 then begin
                  // add nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableAddField(nullFlagsMembers);
                end
                else
                if nullableLenBefore <> nullableLenAfter then begin
                  // resize nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableAlterField(nullFlagsMembers);
                end;
            end;
            aoDropField: begin
              DBFData.FFile.AlterTableDropField(FMembers);
              nullableLenAfter := DBFData.FFile.GetNullFlagsLength(DBFData.FFile.GetNullableFieldCount);
              if nullableLenBefore <> nullableLenAfter then
                if nullableLenAfter <= 0 then begin
                  // drop nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableDropField(nullFlagsMembers);
                end
                else begin
                  // resize nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableAlterField(nullFlagsMembers);
                end;
            end;
            aoAlterField: begin
              DBFData.FFile.AlterTableAlterField(FMembers);
              nullableLenAfter := DBFData.FFile.GetNullFlagsLength(DBFData.FFile.GetNullableFieldCount);
              if nullableLenAfter > 0 then begin
                if DBFData.FFile.NullMaskFieldIndex <= 0 then begin
                  // add nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableAddField(nullFlagsMembers);
                end
                else
                if nullableLenBefore <> nullableLenAfter then begin
                  // resize nullflags
                  SetLength(nullFlagsMembers.Columns, 1);
                  nullFlagsMembers.Columns[0].Name := VFP_NULLFLAGS_NAME;
                  nullFlagsMembers.Columns[0].DataType := VFP_NULLFLAGS_TYPE;
                  DBFData.FFile.AlterTableAlterField(nullFlagsMembers);
                end;
              end
              else begin
                  // todo drop field
              end;
            end;
          end;
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taCreateIndex: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        try
          if not DBFData.FFile.Prepared then
            DBFData.FFile.Prepare;
          if not DBFData.FFile.Active then
            DBFData.FFile.Open;
          DBFData.FFile.AlterTableCreateIndex(FMembers);
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
      taDropIndex: begin
        Table := GetTable;
        if Table = nil then
          raise Exception.CreateFmt(STableNotFound, [FTableName]);
        DBFFormat := TDBFData(Table.Data).GetDBFFormat(DBFFormat, HeaderFormat);
        DetachTable;

        CreateData;
        try
          if not DBFData.FFile.Prepared then
            DBFData.FFile.Prepare;
          DBFData.FFile.Open;
          DBFData.FFile.AlterTableDropIndex(FMembers);
          if AutoUnwind then
            DBFData.Close;
        finally
          DBFData.Free;
        end;

        if not AutoUnwind then
          AttachTable;
      end;
    end;
  end
  else
    inherited;
end;

type
  TExpecting = (exTable, exAction, exFieldName, exFieldType, exFieldDefs, exFieldWidth, exPrecision, exConstraints, exIndexName, exOn);

procedure TDBFDirectCommand.DetectTableAction;
var
  Conn: TCRVirtualConnection;
  Parser: TDBFParser;
  idx, Code, NextCode, brackets: integer;
  St, Stmt: string;
  exp: TExpecting;

  procedure SetCloumnIndex(Idx: integer; const FieldName: string);
  var
    OldActive: boolean;
    Table: TSQLiteVirtualTable;
  begin
    SetLength(FMembers.Constraints[Idx].ColumnInfo, 1);
    Table := Conn.Tables.Find('', FTableName);
    if Table = nil then
      raise Exception.CreateFmt('Table %s not found', [FTableName]);

    OldActive := TDBFData(Table.Data).FFile.Active;
    try
      if not TDBFData(Table.Data).FFile.Prepared then
        TDBFData(Table.Data).FFile.Prepare;
      FMembers.Constraints[Idx].ColumnInfo[0].ColumnIndex := TDBFData(Table.Data).FFile.GetFieldNo(FieldName);
    finally
      if OldActive and not TDBFData(Table.Data).FFile.Active then
        TDBFData(Table.Data).FFile.Open;
    end;
  end;

  function GetFieldCount: integer;
  var
    wasOpen: boolean;
    Table: TSQLiteVirtualTable;
  begin
    Table := Conn.Tables.Find('', FTableName);
    if Table = nil then
      raise Exception.CreateFmt('Table %s not found', [FTableName]);
    wasOpen := TDBFData(Table.Data).FFile.Active;
    if not wasOpen then
      TDBFData(Table.Data).FFile.Open;
    try
      Result := TDBFData(Table.Data).FFile.GetFieldCount;
    finally
      if not wasOpen then
        TDBFData(Table.Data).FFile.Close;
    end;
  end;

  function NormalizeIdent(const IdentCode: integer; const Ident: string): string;
  begin
    Result := Ident;
    if IdentCode = lcString then
      Result := SQLInfo.Quote(SQLInfo.UnQuote(Result));
    Result := Conn.NormalizeTableName(Result);
  end;

begin
  Conn := TCRVirtualConnection(FDBFConnection.Connector.GetInternalConnection);
  FTableAction := taNone;
  FAlterOperation := aoNone;
  FMembers.Columns := nil;
  FMembers.Constraints := nil;

  Parser := TDBFParser.Create(SQL);
  try
    Parser.DecSeparator := '.';
    Parser.OmitBlank := True;
    Parser.OmitComment := True;

    Parser.ToBegin;
    Code := Parser.GetNext(St);
    case Code of
      lxCREATE, lxDROP: begin
        // CREATE TABLE TableName ...
        // DROP TABLE TableName
        // CREATE INDEX IndexName ON TableName FieldName
        // DROP INDEX IndexName ON TableName
        NextCode := Parser.GetNext(St);

        case NextCode of
          lxTABLE: begin
            NextCode := Parser.GetNext(St);

            if NextCode = lcEnd then
              Exit;

            FTableName := NormalizeIdent(NextCode, St);
            case Code of
              lxCREATE: begin
                FTableAction := taCreate;
                TSQLiteMetaData.ParseTableSQL(SQL, FTableName, FMembers);
              end;
              lxDROP:
                FTableAction := taDrop;
            end;
          end;
          lxINDEX: begin
            case Code of
              lxCREATE:
                FTableAction := taCreateIndex;
              lxDROP:
                FTableAction := taDropIndex;
            end;

            exp := exIndexName;
            repeat
              NextCode := Parser.GetNext(St);
              case exp of
                exIndexName:
                  if (NextCode = lcIdent) or (NextCode = lcString) then begin
                    idx := Length(FMembers.Constraints);
                    SetLength(FMembers.Constraints, idx + 1);
                    FMembers.Constraints[idx].ConstraintType := ctPrimaryKey;
                    FMembers.Constraints[idx].Name := NormalizeIdent(NextCode, St);
                    exp := exOn;
                  end
                  else
                    raise Exception.Create('IndexName expected');
                exOn: begin
                  NextCode := Parser.GetNext(St);
                  if (NextCode <> lcIdent) and (NextCode <> lcString) then
                    raise Exception.Create('TableName expected');
                  FTableName := NormalizeIdent(NextCode, St);

                  if Code = lxCREATE then begin
                    NextCode := Parser.GetNext(St);

                    if NextCode <> lxLeftBracket then
                      raise Exception.Create('Invalid index expression');

                    brackets := 1;
                    Stmt := '';
                    NextCode := Parser.GetNext(St);
                    while NextCode <> lxEnd do begin
                      if NextCode = lxRightBracket then begin
                        Dec(brackets);
                        if (brackets = 0) and (Stmt <> '') then begin
                          idx := Length(FMembers.Columns);
                          SetLength(FMembers.Columns, idx + 1);
                          FMembers.Columns[idx].Name := Stmt;

                          idx := Length(FMembers.Constraints) - 1;
                          Assert(idx >= 0, 'FMembers.Constraints are empty');
                          SetCloumnIndex(idx, Stmt);

                          Break;
                        end
                        else
                          Stmt := Stmt + St;
                      end
                      else if NextCode = lxLeftBracket then begin
                        Inc(brackets);
                        Stmt := Stmt + St;
                      end
                      else if (NextCode = lxComma) and (brackets = 1) then begin
                        idx := Length(FMembers.Columns);
                        SetLength(FMembers.Columns, idx + 1);
                        FMembers.Columns[idx].Name := Stmt;

                        idx := Length(FMembers.Constraints) - 1;
                        Assert(idx >= 0, 'FMembers.Constraints are empty');
                        SetCloumnIndex(idx, Stmt);

                        Stmt := '';
                      end
                      else
                        Stmt := Stmt + St;

                      NextCode := Parser.GetNext(St);
                    end;
                  end;

                  Break;
                end;
              end;
            until NextCode = lcEnd;
          end;
        else
          raise Exception.CreateFmt('Unexpected token %s', [St]);
        end;
      end;
      lxPACK: begin
        FPackMemo := False;
        FPackDbf := False;

        Code := Parser.GetNext(St);

        if Code = lxMEMO then begin
          FPackMemo := True;
          Code := Parser.GetNext(St);
        end
        else if Code = lxDBF then begin
          FPackDbf := True;
          Code := Parser.GetNext(St);
        end;

        if Code = lcEnd then
          Exit;

        FTableName := NormalizeIdent(Code, St);
        FTableAction := taPack;
      end;
      lxZAP, lxREINDEX: begin
        NextCode := Parser.GetNext(St);

        if NextCode = lcEnd then
          Exit;

        FTableName := NormalizeIdent(NextCode, St);
        case Code of
          lxZAP:
            FTableAction := taZap;
          lxREINDEX:
            FTableAction := taReindex;
        end;
      end;
      lxALTER: begin
        // VFP8 https://msdn.microsoft.com/en-us/library/aa977261(v=vs.71).aspx
        // there is no real PRIMARY KEY in dbf, only in dbc
        //
        // ALTER TABLE TableName1 ADD | ALTER [COLUMN] FieldName1
        //    FieldType [( nFieldWidth [, nPrecision])] [NULL | NOT NULL] [CHECK lExpression1 [ERROR cMessageText1]]
        //    [AUTOINC [NEXTVALUE NextValue [STEP StepValue]]] [DEFAULT eExpression1]
        //    [PRIMARY KEY | UNIQUE [COLLATE cCollateSequence]]
        //    [REFERENCES TableName2 [TAG TagName1]] [NOCPTRANS] [NOVALIDATE]
        //
        // -or-
        //
        // ALTER TABLE TableName1 ALTER [COLUMN] FieldName2 [NULL | NOT NULL] [SET DEFAULT eExpression2]
        //    [SET CHECK lExpression2 [ERROR cMessageText2]] [ DROP DEFAULT ] [ DROP CHECK ] [ NOVALIDATE ]
        //
        // -or-
        //
        // ALTER TABLE TableName1 [DROP [COLUMN] FieldName3]
        //    [SET CHECK lExpression3 [ERRORcMessageText3]] [DROP CHECK]
        //    [ADD PRIMARY KEY eExpression3 [FOR lExpression4] TAG TagName2
        //    [COLLATE cCollateSequence]] [DROP PRIMARY KEY]
        //    [ADD UNIQUE eExpression4 [[FOR lExpression5] TAG TagName3
        //    [COLLATE cCollateSequence]]] [DROP UNIQUE TAG TagName4]
        //    [ADD FOREIGN KEY [eExpression5] [FOR lExpression6] TAG TagName4
        //    [COLLATE cCollateSequence] REFERENCES TableName2 [TAG TagName5]]
        //    [DROP FOREIGN KEY TAG TagName6 [SAVE]] [RENAME COLUMN FieldName4 TO FieldName5] [NOVALIDATE]

        FTableAction := taAlter;
        FMembers.Columns := nil;
        FMembers.Constraints := nil;
        exp := exTable;
        repeat
          NextCode := Parser.GetNext(St);
          case exp of
            exTable:
              case NextCode of
                lxTABLE:
                  Continue;
                lcIdent,
                lcString: begin
                  FTableName := NormalizeIdent(NextCode, St);
                  exp := exAction;
                end;
              else
                Break;
              end;
            exAction: begin
              case NextCode of
                lxADD:
                  FAlterOperation := aoAddField;
                lxDROP:
                  FAlterOperation := aoDropField;
                lxALTER:
                  FAlterOperation := aoAlterField;
              else
                Break;
              end;
              exp := exFieldName;
            end;
            exFieldName:
              case NextCode of
                lcIdent,
                lcString: begin
                  SetLength(FMembers.Columns, 1);
                  FMembers.Columns[0].Name := NormalizeIdent(NextCode, St);
                  case FAlterOperation of
                    aoAddField, aoAlterField:
                      exp := exFieldType;
                  else
                    Break;
                  end;
                end;
                lxCOLUMN:
                  Continue;
                // todo: PRIMARY, UNIQUE
              else
                Break;
              end;
            exFieldType: begin
              FMembers.Columns[0].DataType := St;
              exp := exFieldDefs;
            end;
            exFieldDefs:
              if NextCode = lxLeftBracket then
                exp := exFieldWidth
              else
                exp := exConstraints;
            exFieldWidth:
              if NextCode = lcNumber then begin
                FMembers.Columns[0].Length := StrToInt(St);
                exp := exPrecision;
              end
              else
                Break;
            exPrecision:
              case NextCode of
                lcNumber: begin
                  FMembers.Columns[0].Scale := StrToInt(St);
                  NextCode := Parser.GetNext(St);
                  if not NextCode = lxRightBracket then
                    Break;
                  exp := exConstraints;
                end;
                lxComma:
                  Continue;
                lxRightBracket:
                  exp := exConstraints;
              end;
            exConstraints: begin
              case NextCode of
                lxNOT: begin
                  NextCode := Parser.GetNext(St);
                  if NextCode = lxNULL then
                    FMembers.Columns[0].NotNull := True
                  else
                    raise Exception.CreateFmt('"NULL" expected after "NOT" instead of "%s"', [St]);
                end;
                lxNULL:
                  FMembers.Columns[0].Default := 'NULL';
                lxNEXTVALUE: begin
                  NextCode := Parser.GetNext(St);
                  if NextCode = lcNumber then
                    {todo: nextvalue in constraints};
                end;
                lxSTEP: begin
                  NextCode := Parser.GetNext(St);
                  if NextCode = lcNumber then
                    {todo: step in constraints};
                end;
                lxDEFAULT: begin
                  NextCode := Parser.GetNext(St);
                  FMembers.Columns[0].Default := St;
                end;
                lxPRIMARY: begin
                  NextCode := Parser.GetNext(St); //lxKEY: ;
                  if (UpperCase(FMembers.Columns[0].DataType) = 'INT')
                    or (UpperCase(FMembers.Columns[0].DataType) = 'INTEGER') then
                    FMembers.Columns[0].DataType := 'AUTOINC';
                  idx := Length(FMembers.Constraints);
                  SetLength(FMembers.Constraints, idx + 1);
                  FMembers.Constraints[idx].ConstraintType := ctPrimaryKey;
                  SetLength(FMembers.Constraints[idx].ColumnInfo, 1);
                  FMembers.Constraints[Idx].ColumnInfo[0].ColumnIndex := GetFieldCount;
                end;
                lxUNIQUE: begin
                  idx := Length(FMembers.Constraints);
                  SetLength(FMembers.Constraints, idx + 1);
                  FMembers.Constraints[idx].ConstraintType := ctUnique;
                  SetLength(FMembers.Constraints[idx].ColumnInfo, 1);
                  FMembers.Constraints[Idx].ColumnInfo[0].ColumnIndex := GetFieldCount;
                end;
              else
                Break;
              end;
            end;
          end;
        until NextCode = lcEnd;
      end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDBFDirectCommand.SetConnection(Value: TCRConnection);
begin
  if (Value <> nil) and IsClass(Value, TDBFConnection) then begin
    FDBFConnection := TDBFConnection(Value);
    inherited SetConnection(TDBFConnection(Value).Connector.GetInternalConnection)
  end
  else begin
    FDBFConnection := nil;
    inherited;
  end;
end;

procedure TDBFDirectCommand.CheckPrepareError(ErrorCode: integer);
const
  ErrMsg = 'no such table:';
var
  Connection: TSQLiteConnection;
  Connector: TDBFDirectConnector;
  n: integer;
  ErrorMsg, TableName: string;
begin
  if ErrorCode = SQLITE_ERROR then begin
    Connection := TSQLiteConnection(GetConnection);
    Connection.DetectError(ErrorCode, ErrorMsg);
    n := Pos(ErrMsg, ErrorMsg);
    if n >= 1 then begin
      Unprepare;
      Connector := TDBFDirectConnector(FDBFConnection.Connector);
      TableName := Trim(Copy(ErrorMsg, n + Length(ErrMsg), Length(ErrorMsg)));
      if Connector.FDatabase.AcquireTable(TableName) <> nil then
        InternalPrepare
      else
        inherited;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TDBFDirectCommand.Unprepare;
begin
  if FTableAction = taNone then
    inherited
  else
    SetCursorState(csInactive);

  FTableAction := taNone;
end;

procedure TDBFDirectCommand.Execute;
begin
  if not GetPrepared and (FTableAction = taNone) then
    DetectTableAction;

  inherited;

  FTableAction := taNone;
end;

function TDBFDirectCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
begin
  FTableAction := taNone;

  Result := inherited ParseSQL(SQL, Params, RenamePrefix);
end;

{ TDBFDirectRecordSet }

procedure TDBFDirectRecordSet.CreateCommand;
begin
  SetCommand(TDBFDirectCommand.Create);
end;

procedure TDBFDirectRecordSet.Reopen;
var
  i: integer;
  Conn: TCRVirtualConnection;
begin
  if FDBFConnection.ConnectMode in [cmShared, cmUnsafe] then begin
    Conn := TCRVirtualConnection(GetConnection);
    for i := 0 to Conn.Tables.Count - 1 do begin
      TDBFData(TSQLiteVirtualTable(Conn.Tables[i]).Data).FFile.Close;
      TDBFData(TSQLiteVirtualTable(Conn.Tables[i]).Data).FFile.Prepare;
      TDBFData(TSQLiteVirtualTable(Conn.Tables[i]).Data).FFile.Open(True, False);
    end;
  end;

  inherited;
end;

procedure TDBFDirectRecordSet.SetConnection(Value: TCRConnection);
begin
  if (Value <> nil) and IsClass(Value, TDBFConnection) then begin
    FDBFConnection := TDBFConnection(Value);
    FCommand.SetConnection(FDBFConnection);
  end
  else begin
    FDBFConnection := nil;
    inherited;
  end;
end;

function TDBFDirectRecordSet.SetProp(Prop: integer;
  const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout: ;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TDBFDirectRecordSet.GetProp(Prop: integer;
  out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := 0;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TDBFMetaData }

constructor TDBFMetaData.Create;
begin
  inherited;
end;

destructor TDBFMetaData.Destroy;
begin
  inherited;
end;

function TDBFMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TDBFDirectRecordSet.Create;
end;

procedure TDBFMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  List.Clear;
  List.Sorted := False;
  List.Add('MetaDataKinds');
  List.Add('Restrictions');
  List.Add('Tables');
  List.Add('Columns');
  List.Add('Indexes');
  List.Add('IndexColumns');
  List.Add('Constraints');
  List.Add('ConstraintColumns');
  List.Sorted := True;
end;

procedure TDBFMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'restrictions' then begin
    List.Add('METADATA_NAME');
  end
  else
  if MetaDataKind = 'tables' then begin
    List.Add('TABLE_NAME');
  end
  else
  if MetaDataKind = 'columns' then begin
    List.Add('TABLE_NAME');
    List.Add('COLUMN_NAME');
  end
  else
  if MetaDataKind = 'indexes' then begin
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'indexcolumns' then begin
    List.Add('TABLE_NAME');
    List.Add('INDEX_NAME');
  end
  else
  if MetaDataKind = 'constraints' then begin
    List.Add('TABLE_NAME');
    List.Add('CONSTRAINT_NAME');
    List.Add('CONSTRAINT_TYPE');
  end
  else
  if MetaDataKind = 'constraintcolumns' then begin
    List.Add('TABLE_NAME');
    List.Add('CONSTRAINT_NAME');
    List.Add('COLUMN_NAME');
  end;
end;

function TDBFMetaData.GetTables(Restrictions: TStrings): TData;
const
  dnCATALOG_NAME  = 1;
  dnSCHEMA_NAME   = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
var
  TableRestriction,
  TableName: string;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  i: integer;
begin
  TableRestriction := Trim(Restrictions.Values['TABLE_NAME']);

  CreateTablesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  Connector := TDBFDirectConnector(TDBFDirectRecordSet(FRecordSet).FDBFConnection.Connector);

  for i := 0 to Connector.FDatabase.Tables.Count - 1 do begin
    Table := TDBFTable(Connector.FDatabase.Tables[i]);
    TableName := Table.TableName;
    case TDBFDirectRecordSet(FRecordSet).FDBFConnection.IdentifierCase of
      icLower: TableName := LowerCase(TableName);
      icUpper: TableName := UpperCase(TableName);
    end;

    if (TableRestriction = '') or SameText(TableName, TableRestriction) then begin
      FMemDataHelper.InitRecord;
      FMemDataHelper.FieldValues[dnCATALOG_NAME] := Table.SchemaName;
      FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
      FMemDataHelper.FieldValues[dnTABLE_TYPE] := 'TABLE';
      FMemDataHelper.AppendRecord;
    end;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TDBFMetaData.GetColumns(Restrictions: TStrings): TData;
const
  dnCATALOG_NAME  = 1;
  dnSCHEMA_NAME   = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;
var
  TableRestriction,
  TableName,
  ColumnRestriction,
  ColumnName,
  DataTypeName: string;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
  Field: TVirtualFieldDesc;
  FileActive: boolean;
  i, j: integer;
begin
  TableRestriction := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnRestriction := Trim(Restrictions.Values['COLUMN_NAME']);

  CreateColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  Connection := TDBFDirectRecordSet(FRecordSet).FDBFConnection;
  Connector := TDBFDirectConnector(Connection.Connector);

  for i := 0 to Connector.FDatabase.Tables.Count - 1 do begin
    Table := TDBFTable(Connector.FDatabase.Tables[i]);
    TableName := Table.TableName;
    case Connection.IdentifierCase of
      icLower: TableName := LowerCase(TableName);
      icUpper: TableName := UpperCase(TableName);
    end;

    if (TableRestriction = '') or SameText(TableName, TableRestriction) then begin
      Connector.FDatabase.InitTable(Table);
      Data := TDBFData(Table.VirtualTable.Data);
      FileActive := Data.FFile.Active;
      try
        if not FileActive then
          Data.DescribeFields(nil);
        for j := 0 to High(Data.Fields) do begin
          Field := Data.Fields[j];

          if not Field.Hidden then begin
            ColumnName := Field.Name;
            case Connection.IdentifierCase of
              icLower: ColumnName := LowerCase(ColumnName);
              icUpper: ColumnName := UpperCase(ColumnName);
            end;

            if (ColumnRestriction = '') or SameText(ColumnName, ColumnRestriction) then begin
              FMemDataHelper.InitRecord;
              FMemDataHelper.FieldValues[dnCATALOG_NAME] := Table.SchemaName;
              FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
              FMemDataHelper.FieldValues[dnCOLUMN_NAME] := ColumnName;
              FMemDataHelper.FieldValues[dnPOSITION] := j + 1;
              DataTypeName := Data.GetFieldDataTypeName(@Field);
              case Connection.IdentifierCase of
                icLower: DataTypeName := LowerCase(DataTypeName);
                icUpper: DataTypeName := UpperCase(DataTypeName);
              end;
              FMemDataHelper.FieldValues[dnDATA_TYPE] := DataTypeName;
              if Field.Scale <> 0 then begin
                FMemDataHelper.FieldValues[dnPRECISION] := Field.Length;
                FMemDataHelper.FieldValues[dnSCALE] := Field.Scale;
              end
              else if Field.Length <> 0 then
                FMemDataHelper.FieldValues[dnLENGTH] := Field.Length;
              FMemDataHelper.FieldValues[dnNULLABLE] := 1 - integer(Field.Required);
              FMemDataHelper.FieldValues[dnDEFAULT] := Field.Default;
              FMemDataHelper.AppendRecord;
            end;
          end;
        end;
      finally
        if not FileActive then
          Data.FFile.Close;
      end;
    end;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TDBFMetaData.GetProcedures(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TDBFMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TDBFMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  TableRestriction,
  TableName,
  IndexRestriction,
  IndexName: string;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
  FileActive: boolean;
  idx: TDBFIndexFile;
  i, idxNo: integer;
begin
  TableRestriction := Trim(Restrictions.Values['TABLE_NAME']);
  IndexRestriction := Trim(Restrictions.Values['INDEX_NAME']);

  CreateIndexesFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  Connection := TDBFDirectRecordSet(FRecordSet).FDBFConnection;
  Connector := TDBFDirectConnector(Connection.Connector);

  for i := 0 to Connector.FDatabase.Tables.Count - 1 do begin
    Table := TDBFTable(Connector.FDatabase.Tables[i]);
    TableName := Table.TableName;
    case Connection.IdentifierCase of
      icLower: TableName := LowerCase(TableName);
      icUpper: TableName := UpperCase(TableName);
    end;

    if (TableRestriction = '') or SameText(TableName, TableRestriction) then begin
      Connector.FDatabase.InitTable(Table);
      Data := TDBFData(Table.VirtualTable.Data);
      FileActive := Data.FFile.Active;
      try
        if not FileActive then
          Data.FFile.Open;
        if Data.FFile.IndexFile <> nil then begin
          idx := Data.FFile.IndexFile;
          for idxNo := 0 to idx.GetIndexCount - 1 do begin
            // Since DBF has no explicit primary key,
            // index name has to be the same for all fields.
            // Otherwise, TDADataSetService.GetDBKeyList does not return all index fields.
            IndexName := ''; // idx.GetIndexName(idxNo);
            if (IndexRestriction = '') or (LowerCase(IndexName) = IndexRestriction) then begin
              FMemDataHelper.InitRecord;
              FMemDataHelper.FieldValues[dnTABLE_CATALOG] := Table.SchemaName;
              FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
              FMemDataHelper.FieldValues[dnINDEX_CATALOG] := Table.SchemaName;
              FMemDataHelper.FieldValues[dnINDEX_NAME] := IndexName;
              FMemDataHelper.FieldValues[dnUNIQUE] := idx.GetIndexIsUnique(idxNo);
              FMemDataHelper.AppendRecord;
            end;
          end;
        end;
      finally
        if not FileActive then
          Data.FFile.Close;
      end;
    end;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TDBFMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnPOSITION        = 8;
  dnSORT_ORDER      = 9;
var
  TableRestriction,
  TableName,
  IndexRestriction,
  IndexName,
  FieldName: string;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
  FileActive: boolean;
  idx: TDBFIndexFile;
  i, idxNo, FieldNo: integer;
begin
  TableRestriction := LowerCase(Trim(Restrictions.Values['TABLE_NAME']));
  IndexRestriction := LowerCase(Trim(Restrictions.Values['INDEX_NAME']));

  CreateIndexColumnsFields;
  FMemData.Open;
  FMemDataHelper.AllocBuffer;

  Connection := TDBFDirectRecordSet(FRecordSet).FDBFConnection;
  Connector := TDBFDirectConnector(Connection.Connector);

  for i := 0 to Connector.FDatabase.Tables.Count - 1 do begin
    Table := TDBFTable(Connector.FDatabase.Tables[i]);
    TableName := Table.TableName;
    case Connection.IdentifierCase of
      icLower: TableName := LowerCase(TableName);
      icUpper: TableName := UpperCase(TableName);
    end;

    if (TableRestriction = '') or SameText(TableName, TableRestriction) then begin
      Connector.FDatabase.InitTable(Table);
      Data := TDBFData(Table.VirtualTable.Data);
      FileActive := Data.FFile.Active;
      try
        if not FileActive then
          Data.FFile.Open(True, False);
        if Data.FFile.IndexFile <> nil then begin
          idx := Data.FFile.IndexFile;
          for idxNo := 0 to idx.GetIndexCount - 1 do begin
            // Since DBF has no explicit primary key,
            // index name has to be the same for all fields.
            // Otherwise, TDADataSetService.GetDBKeyList does not return all index fields.
            IndexName := ''; // idx.GetIndexName(idxNo);
            if (IndexRestriction = '') or (LowerCase(IndexName) = IndexRestriction) then begin
              FMemDataHelper.InitRecord;
              FMemDataHelper.FieldValues[dnTABLE_CATALOG] := Table.SchemaName;
              FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
              FMemDataHelper.FieldValues[dnINDEX_CATALOG] := Table.SchemaName;
              FMemDataHelper.FieldValues[dnINDEX_NAME] := IndexName;
              FieldName := idx.GetIndexExpressionFieldName(idxNo);
              case Connection.IdentifierCase of
                icLower: FieldName := LowerCase(FieldName);
                icUpper: FieldName := UpperCase(FieldName);
              end;
              FMemDataHelper.FieldValues[dnCOLUMN_NAME] := FieldName;
              FieldNo := Data.FFile.GetFieldNo(FieldName);
              if FieldNo >= 0 then // else ? it is expression, field name or something
                FMemDataHelper.FieldValues[dnPOSITION] := FieldNo;
              if idx.GetIndexIsDescending(idxNo) then
                FMemDataHelper.FieldValues[dnSORT_ORDER] := 'DESC'
              else
                FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC';
              FMemDataHelper.AppendRecord;
            end;
          end;
        end;
      finally
        if not FileActive then
          Data.FFile.Close;
      end;
    end;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TDBFMetaData.GetConstraints(Restrictions: TStrings): TData;
var
  TableName: string;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateConstraintsFields;
  FMemData.Open;
  Result := FMemData;
end;

{ TDBFDirectTransaction }

procedure TDBFDirectTransaction.StartTransaction;
var
  i, j: integer;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
begin
  for i := 0 to FConnections.Count - 1 do
    if FConnections[i] is TDBFConnection then begin
      Connection := TDBFConnection(FConnections[i]);
      Connector := TDBFDirectConnector(Connection.Connector);

      for j := 0 to Connector.FDatabase.Tables.Count - 1 do begin
        Table := Connector.FDatabase.Tables[j];
        Connector.FDatabase.InitTable(Table);
        Data := TDBFData(Table.VirtualTable.Data);
        Data.StartTransactionNoSQLite;
      end;
    end;

  FActive := True;
end;

procedure TDBFDirectTransaction.Commit;
var
  i, j: integer;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
begin
  for i := 0 to FConnections.Count - 1 do
    if FConnections[i] is TDBFConnection then begin
      Connection := TDBFConnection(FConnections[i]);
      Connector := TDBFDirectConnector(Connection.Connector);

      for j := 0 to Connector.FDatabase.Tables.Count - 1 do begin
        Table := Connector.FDatabase.Tables[j];
        Connector.FDatabase.InitTable(Table);
        Data := TDBFData(Table.VirtualTable.Data);
        Data.CommitNoSQLite;
      end;
    end;

  FActive := False;
end;

procedure TDBFDirectTransaction.Rollback;
var
  i, j: integer;
  Connection: TDBFConnection;
  Connector: TDBFDirectConnector;
  Table: TDBFTable;
  Data: TDBFData;
begin
  for i := 0 to FConnections.Count - 1 do
    if FConnections[i] is TDBFConnection then begin
      Connection := TDBFConnection(FConnections[i]);
      Connector := TDBFDirectConnector(Connection.Connector);

      for j := 0 to Connector.FDatabase.Tables.Count - 1 do begin
        Table := Connector.FDatabase.Tables[j];
        Connector.FDatabase.InitTable(Table);
        Data := TDBFData(Table.VirtualTable.Data);
        Data.RollbackNoSQLite;
      end;
    end;

  FActive := False;
end;

{ TDBFDirectLoader }

procedure TDBFDirectLoader.SetConnection(Value: TCRConnection);
begin
  inherited;
  FConnection := TDBFConnection(Value);
end;

procedure TDBFDirectLoader.CreateCommand;
begin
  FCommand := TDBFDirectCommand.Create;
end;

{$ENDIF}

end.
