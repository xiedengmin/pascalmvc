
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoClassesUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils,
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  CRAccess, CRJson, CRTypes, MemData, CRParser, CRDataTypeMap,
{$IFNDEF NOSQL}
  CRVirtualData, CRVirtualQuery,
  LiteClassesVirtual,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MongoCall, MongoObjects;
{$ELSE}
  MongoCallUni, MongoObjectsUni;
{$ENDIF}

const
  dtMongoObjectId = 100;

type
  TDescribeMethod = (dmObject, dmGrid);
  TQueryMethod = (qmCollection, qmDatabase);
  TIdGenerationMode = (igClient, igDatabase);

  TMongoCommand = class;

  TMongoWriteConcern = class
  private
    FAPI: TMongoAPI;
    FHandle: pmongoc_write_concern_t;

    procedure Initialize;
    procedure Finalize;
  public
    constructor Create(const API: TMongoAPI);
    destructor Destroy; override;
  end;

  TMongoCursor = class
  private
    FAPI: TMongoAPI;
    FHandle: pmongoc_cursor_t;

    function GetIsAlive: boolean;
    function GetMore: boolean;
  public
    constructor Create(const API: TMongoAPI; const Handle: pmongoc_cursor_t);
    destructor Destroy; override;

    function Next: pbson_t;

    property IsAlive: boolean read GetIsAlive;
    property More: boolean read GetMore;
  end;

  TMongoCollection = class
  private
    FAPI: TMongoAPI;
    FWriteConcern: TMongoWriteConcern;
    FHandle: pmongoc_collection_t;
  public
    constructor Create(const API: TMongoAPI; const WriteConcern: TMongoWriteConcern; const Handle: pmongoc_collection_t);
    destructor Destroy; override;

    function Find(const Query, Projection: TMongoDocument; const Skip, Limit: integer): TMongoCursor;
    function Insert(const Document: TMongoDocument; const Error: pbson_error_t): boolean;
    function Update(const Document: TMongoDocument; const Error: pbson_error_t): boolean;
    function Delete(const Document: TMongoDocument; const Error: pbson_error_t): boolean;

    function GetLastError: TMongoDocument;

    function GetIndexes: TMongoCursor;
  end;

  TMongoConnection = class(TCRConnection)
  private
    FAPI: TMongoAPI;
    FClient: pmongoc_client_t;
    FWriteConcern: TMongoWriteConcern;
    FDB: pmongoc_database_t;
    FError: bson_error_t;
    FCollections: TStringList;

    FAdditionalServers: string;
    FPort: Integer;
    FClientLibrary,
    FBSONLibrary: string;
    FDatabase: string;
    FUseUnicode: boolean;
    FConnectionOptions: string;
    FLowerCaseObjectId: boolean;
    FDescribeAmount: integer;

    FServerVersion: string;
    FServerVersionFull: string;
    FClientVersion: string;
    FLibVersionMajor,
    FLibVersionMinor,
    FLibVersionRelease: integer;

    function BuildURI: string;
    procedure CheckClient;
    procedure ClearCollections;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
  {$ENDIF}

    procedure CheckError;

    function ExecuteJSON(const Database, JSON: string): TMongoDocument;
    function GetCollection(const Name: string): TMongoCollection;
    procedure DropCollection(const Name: string);

    function CheckIsValid: boolean; override;

    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    function SetProp(Prop: Integer; const Value: variant): boolean; override;
    function GetProp(Prop: Integer; out Value: variant): boolean; override;

    procedure ProcessError(ErrorCode: integer; ErrorMsg: string; Component: TObject);

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    procedure Ping; override;

    property API: TMongoAPI read FAPI;
    property Database: string read FDatabase;
    property Client: pmongoc_client_t read FClient;
    property DB: pmongoc_database_t read FDB;
    property Error: bson_error_t read FError;
  end;

  TMongoTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TMongoCommand = class(TCRCommand)
  private
    FInternalCommand,
    FProjection: TMongoDocument;
    FCollectionName: string;
    FCollection: TMongoCollection;
    FCursorState: TCursorState;
    FSkip,
    FLimit,
    FRowsAffected: integer;

    function InternalExecute: TMongoCursor;
    procedure InternalInsert(const Document: TMongoDocument);
    procedure InternalUpdate(const Document: TMongoDocument);
    procedure InternalDelete(const Document: TMongoDocument);

    procedure SetProjection(const Projection: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetParserClass: TSQLParserClass; override;
  {$IFNDEF LITE}
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute; override;
    function ExecuteCursor: TMongoCursor;
    function ExecuteSimple(const Database: string): TMongoDocument;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    function GetProp(Prop: integer; out Value: Variant): boolean; override;
    procedure SetConnection(Value: TCRConnection); override;

    procedure SetSQL(const Value: string); override;
    function ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string; overload; override;
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;

    property CollectionName: string read FCollectionName;
  end;

  TMongoFieldDesc = class(TCRFieldDesc)
  private
    FData: TJSONValue;
    FForceUseString: boolean;
    FFullName,
    FMaskedName: string;
  public
    constructor Create(RecordSetClass: TRecordSetClass); override;

    property FullName: string read FFullName write FFullName;
    property MaskedName: string read FMaskedName write FMaskedName;
  end;

  TMongoRecordset = class(TCRRecordset)
  private
    FPrefetchedDocs: TList;
    FCursor: TMongoCursor;
    FFetchAmount,
    FFetched: integer;
    FDescribeMethod: TDescribeMethod;
    FIdGenerationMode: TIdGenerationMode;
    FDocumentType: TMongoObjectType;
    FImplicitAddFields,
    FImplicitChangeType,
    FComplexAsString: boolean;
    FLowercaseObjectId: boolean;
    FPutBufferSize: integer;
    FPutBuffer: TBytes;
    FCachedFields: TStringList;

    procedure CheckDocumentType;
    procedure ClearDocumentType;
    procedure Prefetch;

    function DescribeFieldDesc(const ParentField, RootField: TMongoFieldDesc; const Name: string; const Value: TJSONValue; const CreateField: boolean = True): TMongoFieldDesc;
  protected
    procedure CreateFieldDescs; override;
    procedure CreateCommand; override;

    procedure FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean); override;

    function NeedInitFieldsOnPrepare: boolean; override;
    procedure InternalClose; override;

    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;

    function InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer; override;

    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;

    function GetChildFieldIsNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    procedure ClearFields; override;
    procedure UnPrepare; override;
    function IsFullReopen: boolean; override;

    class function IsComplexDataType(DataType: Word): boolean; override;

    function GetFieldDescType: TFieldDescClass; override;
    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;
    procedure CopyComplexField(Source: IntPtr; Dest: IntPtr; Field: TFieldDesc); override;
    procedure FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;

    procedure GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean); override;
    procedure PutField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); override;
    procedure PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False); override;
    procedure GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean); override;
    procedure PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: Variant; IsDatabaseValue: boolean); override;
    procedure GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False); override;

  {$IFNDEF LITE}
    procedure InternalInitFieldDescs; override;
  {$ENDIF}

    class function IsObjectDataType(DataType: word): boolean; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;
  end;

  TMongoMetaData = class(TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;

    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;
    function GetDatabases(Restrictions: TStrings): TData; override;
  end;

  TMongoSQLInfo = class(TSQLInfo);

{$IFNDEF NOSQL}
  TMongoVirtualConnection = class;

  TMongoFieldAccessor = class(TFieldAccessor)
    class function AsInteger(const Field, Buffer: IntPtr): Int64; override;
    class function AsFloat(const Field, Buffer: IntPtr): double; override;
    class function AsAnsiString(const Field, Buffer: IntPtr): AnsiString; override;
    class function AsWideString(const Field, Buffer: IntPtr): WideString; override;
  end;

  TVirtualMongoIndex = class(TVirtualMemDataIndex);

  TMongoVirtualData = class(TVirtualMemData)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TMongoVirtualConnection;
    FTableName: string;

//    function GetFilterString(const Filter: PVirtualConstraint): string;
  protected
    class function GetLocalIndexClass: TVirtualLocalIndexClass; override;
    function GetFieldAccessorClass: TFieldAccessorClass; override;

    function InternalOpenNoIndex(const Filter: PVirtualConstraint): TVirtualBookmark; override;
    procedure InternalAllocBuffer; override;
    procedure InternalDescribeFields(const SpecificTypes: TSpecificTypes); override;
  public
    constructor Create(const Connection: TMongoVirtualConnection; const TableName: string);
    destructor Destroy; override;

    procedure Prepare; override;
    procedure InsertRecord(const Values: TVirtualValues); override;

    function GetLocalIndex(const Constraint: PVirtualConstraint): integer; override;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
  end;

  TMongoVirtualTable = class(TSQLiteVirtualTable)
  end;

  TMongoVirtualConnection = class(TCRVirtualConnection)
  private
    FInternalConnection: TMongoConnection;
    FInternalMetaData: TMongoMetaData;
    FPort: integer;
    FDatabase: string;
    FClientLibrary,
    FBSONLibrary: string;
    FImplicitAddFields,
    FImplicitChangeType,
    FComplexAsString,
    FUseMaskedFields: boolean;
    FAdditionalServers,
    FConnectionOptions: string;
    FLowercaseObjectId: boolean;

    procedure InternalRegisterTables;
  protected
    class function GetVirtualTableClass: TVirtualTableClass; override;
  public
    constructor Create; overload; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    property InternalConnection: TMongoConnection read FInternalConnection;

    property ClientLibrary: string read FClientLibrary write FClientLibrary;
    property BSONLibrary: string read FBSONLibrary write FBSONLibrary;
    property ImplicitAddFields: boolean read FImplicitAddFields write FImplicitAddFields;
    property ImplicitChangeType: boolean read FImplicitChangeType write FImplicitChangeType;
    property ComplexAsString: boolean read FComplexAsString write FComplexAsString;
    property LowercaseObjectId: boolean read FLowercaseObjectId write FLowercaseObjectId;
    property AdditionalServers: string read FAdditionalServers write FAdditionalServers;
    property ConnectionOptions: string read FConnectionOptions write FConnectionOptions;
  end;

  TMongoVirtualCommand = class(TCRVirtualCommand)
  protected
    procedure InternalCreateTablesInfo; override;
  public
    procedure SetConnection(Value: TCRConnection); override;
  end;

  TMongoVirtualRecordset = class(TCRVirtualRecordSet)
  private
    function GetCreateSQL(const TableName: string; const Members: TTableMembers): string;
  protected
    procedure CreateCommand; override;
    procedure InternalPrepare; override;
  public
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
  end;
{$ENDIF}

  TMongoConnector = class(TCRConnection)
  private
    FInternalConnection: TCRConnection;

    FPort: Integer;
    FDatabase: string;
    FClientLibrary,
    FBSONLibrary: string;
    FUseUnicode: boolean;
    FConnectionOptions: string;
    FAdditionalServers: string;
    FSQLEngine,
    FLowerCaseObjectId: boolean;
    FDescribeAmount: integer;

    procedure SetSQLEngine(const Value: boolean);
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetTransactionClass: TCRTransactionClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetCommandClass: TCRCommandClass; override;
  {$IFNDEF LITE}
    function GetMetaDataClass: TCRMetaDataClass; override;
  {$ENDIF}

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;
  end;

var
  MongoSQLInfo: TMongoSQLInfo;

implementation

uses
{$IFDEF VER24P}
  NetEncoding,
{$ENDIF}
  MemUtils, CRProps, CRFunctions, CLRClasses,
  DAConsts,
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoError, MongoProps, MongoDataTypeMap;
{$ELSE}
  MongoConstsUni, MongoErrorUni, MongoPropsUni, MongoDataTypeMapUni;
{$ENDIF}

type
  TInternalDocument = class(TMongoDocument);
  TInternalJSONString = class(TJSONString);

{$IFNDEF VER24P}
  UnsafeChar = Byte;
  TUnsafeChars = set of UnsafeChar;
  TEncodeOption = (SpacesAsPlus, EncodePercent);
  TEncodeOptions = set of TEncodeOption;

  TURLEncoding = class;

  TNetEncoding = class
  private
    FURLEncoding: TURLEncoding;

    function GetURLEncoding: TURLEncoding;
  public
    destructor Destroy; override;

    property URL: TURLEncoding read GetURLEncoding;
  end;

  TURLEncoding = class(TNetEncoding)
  public
    function EncodeAuth(const Auth: string; const AExtraUnsafeChars: TUnsafeChars = []): string;
    function Encode(const AInput: string; const ASet: TUnsafeChars; const AOptions: TEncodeOptions; AEncoding: Encoding = nil): string;
  end;

const
  AuthUnsafeChars: TUnsafeChars = [Ord('"'), Ord(''''), Ord(':'), Ord(';'), Ord('<'), Ord('='), Ord('>'),
    Ord('@'), Ord('['), Ord(']'), Ord('^'), Ord('`'), Ord('{'), Ord('}'), Ord('|'), Ord('/'), Ord('\'), Ord('?'), Ord('#')];

var
  NetEncoding: TNetEncoding;

{ TNetEncoding }

destructor TNetEncoding.Destroy;
begin
  FURLEncoding.Free;

  inherited;
end;

function TNetEncoding.GetURLEncoding: TURLEncoding;
begin
  if FURLEncoding = nil then
    FURLEncoding := TURLEncoding.Create;
  Result := FURLEncoding;
end;

{ TURLEncoding }

function TURLEncoding.EncodeAuth(const Auth: string; const AExtraUnsafeChars: TUnsafeChars): string;
begin
  Result := Encode(Auth, AuthUnsafeChars + AExtraUnsafeChars, []);
end;

function TURLEncoding.Encode(const AInput: string; const ASet: TUnsafeChars; const AOptions: TEncodeOptions; AEncoding: Encoding = nil): string;

  function IsHexChar(C: Byte): Boolean;
  begin
    Result := C in [Ord('0')..Ord('9'), Ord('A')..Ord('F'), Ord('a')..Ord('f')];
  end;

const
  XD: array[0..15] of char = ('0', '1', '2', '3', '4', '5', '6', '7',
                              '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  Buff: TBytes;
  I: Integer;
  Len: Integer;
  LSet: TUnsafeChars;
begin
  Result := '';

  if AInput = '' then
    Exit;

{$IFNDEF VER9P}
  SetLength(Buff, 0); // anti-warning
{$ENDIF}

  if AEncoding = nil then
    AEncoding := Encoding.UTF8;
  Buff := AEncoding.GetBytes(AInput);
  Len := Length(Buff);
  I := 0;
  if (SpacesAsPlus in AOptions) then
    LSet := ASet + [Ord('+')]
  else
    LSet := ASet;
  if (EncodePercent in AOptions) then
    LSet := LSet + [Ord('%')];

  while I < Len do begin
    if not(EncodePercent in AOptions) and (I + 2 < Len) and (Buff[I] = Ord('%')) and IsHexChar(Buff[I + 1]) and IsHexChar(Buff[I + 2]) then begin
      Result := Result + '%' + Char(Buff[I + 1]) + Char(Buff[I + 2]);
      Inc(I, 3);
    end
    else begin
      if (Buff[I] >= $21) and (Buff[I] <= $7E) then begin
        if Buff[I] in LSet then
          Result := Result + '%' + XD[(Buff[I] shr 4) and $0F] + XD[Buff[I] and $0F]
        else
          Result := Result + Char(Buff[I]);
      end
      else if (SpacesAsPlus in AOptions) and (Buff[I] = Ord(' ')) then
        Result := Result + '+'
      else
        Result := Result + '%' + XD[(Buff[I] shr 4) and $0F] + XD[Buff[I] and $0F];

      Inc(I);
    end;
  end;
end;
{$ENDIF}

{ TMongoWriteConcern }

constructor TMongoWriteConcern.Create(const API: TMongoAPI);
begin
  inherited Create;

  FAPI := API;
  Initialize;
end;

destructor TMongoWriteConcern.Destroy;
begin
  Finalize;

  inherited;
end;

procedure TMongoWriteConcern.Initialize;
begin
  Finalize;

  if FAPI.Initialized then begin
    FHandle := FAPI.mongoc_write_concern_new;
    if FHandle <> nil then
      FAPI.mongoc_write_concern_set_w(FHandle, MONGOC_WRITE_CONCERN_W_DEFAULT);
  end;
end;

procedure TMongoWriteConcern.Finalize;
begin
  if (FHandle <> nil) and FAPI.Initialized then begin
    FAPI.mongoc_write_concern_destroy(FHandle);
    FHandle := nil;
  end;
end;

{ TMongoCursor }

constructor TMongoCursor.Create(const API: TMongoAPI; const Handle: pmongoc_cursor_t);
begin
  inherited Create;

  FAPI := API;
  FHandle := Handle;
end;

destructor TMongoCursor.Destroy;
begin
  if FHandle <> nil then
    FAPI.mongoc_cursor_destroy(FHandle);

  inherited;
end;

function TMongoCursor.GetIsAlive: boolean;
begin
  Result := FAPI.mongoc_cursor_is_alive(FHandle);
end;

function TMongoCursor.GetMore: boolean;
begin
  Result := FAPI.mongoc_cursor_more(FHandle);
end;

function TMongoCursor.Next: pbson_t;
var
  p: pbson_t;
begin
  if FAPI.mongoc_cursor_next(FHandle, @p) then
    Result := p
  else
    Result := nil;
end;

{ TMongoCollection }

constructor TMongoCollection.Create(const API: TMongoAPI; const WriteConcern: TMongoWriteConcern; const Handle: pmongoc_collection_t);
begin
  inherited Create;

  FHandle := Handle;
  FAPI := API;
  FWriteConcern := WriteConcern;
end;

destructor TMongoCollection.Destroy;
begin
  if FHandle <> nil then
    FAPI.mongoc_collection_destroy(FHandle);

  inherited;
end;

function TMongoCollection.Find(const Query, Projection: TMongoDocument; const Skip, Limit: integer): TMongoCursor;
var
  p: pmongoc_cursor_t;
begin
  if Projection <> nil then
    p := FAPI.mongoc_collection_find(FHandle, MONGOC_QUERY_NONE, Skip, Limit, 0, TInternalDocument(Query).Handle, TInternalDocument(Projection).Handle, nil)
  else
    p := FAPI.mongoc_collection_find(FHandle, MONGOC_QUERY_NONE, Skip, Limit, 0, TInternalDocument(Query).Handle, nil, nil);
  if p <> nil then
    Result := TMongoCursor.Create(FAPI, p)
  else
    raise Exception.Create('Invalid cursor');
end;

function TMongoCollection.Insert(const Document: TMongoDocument; const Error: pbson_error_t): boolean;
begin
  TInternalDocument(Document).Prepare;
  Result := FAPI.mongoc_collection_insert(FHandle, MONGOC_INSERT_NONE, TInternalDocument(Document).Handle, FWriteConcern.FHandle, Error);
end;

function TMongoCollection.Update(const Document: TMongoDocument; const Error: pbson_error_t): boolean;
begin
  TInternalDocument(Document).Prepare;
  Result := FAPI.mongoc_collection_save(FHandle, TInternalDocument(Document).Handle, FWriteConcern.FHandle, Error);
end;

function TMongoCollection.Delete(const Document: TMongoDocument; const Error: pbson_error_t): boolean;
begin
  TInternalDocument(Document).Prepare;
  Result := FAPI.mongoc_collection_remove(FHandle, MONGOC_REMOVE_SINGLE_REMOVE, TInternalDocument(Document).Handle, FWriteConcern.FHandle, Error);
end;

function TMongoCollection.GetLastError: TMongoDocument;
var
  bson: pbson_t;
begin
  Result := nil;

  bson := FAPI.mongoc_collection_get_last_error(FHandle);
  if bson = nil then
    Exit;
  Result := TMongoDocument.Create;
  TInternalDocument(Result).AllocHandle(htOuter, bson);
end;

function TMongoCollection.GetIndexes: TMongoCursor;
var
  p: pmongoc_cursor_t;
begin
  p := FAPI.mongoc_collection_find_indexes(FHandle, nil);
  if p <> nil then
    Result := TMongoCursor.Create(FAPI, p)
  else
    raise Exception.Create('Invalid cursor');
end;

{ TMongoConnection }

constructor TMongoConnection.Create;
begin
  inherited;

  FAdditionalServers := '';
  FPort := MnDefValPort;
  FServer := MnDefValServer;
  FClientLibrary := '';
  FBSONLibrary := '';
  FDatabase := '';
  FConnectionOptions := '';
  FDescribeAmount := 25;

  FServerVersion := '';
  FServerVersionFull := '';
  FClientVersion := '';
  FLibVersionMajor := 0;
  FLibVersionMinor := 0;
  FLibVersionRelease := 0;

  FAPI := TMongoAPI.Create;
  FDB := nil;
  FCollections := TStringList.Create;
  FWriteConcern := TMongoWriteConcern.Create(FAPI);
end;

destructor TMongoConnection.Destroy;
begin
  inherited;

  FWriteConcern.Free;
  FCollections.Free;
  FAPI.Free;
end;

function TMongoConnection.BuildURI: string;
var
  Builder: StringBuilder;
begin
  Builder := StringBuilder.Create(1024);
  try
    Builder.Append('mongodb://');
    if FUsername <> '' then begin
      Builder.Append({$IFDEF VER24P}TNetEncoding{$ELSE}NetEncoding{$ENDIF}.URL.EncodeAuth(FUserName));
      Builder.Append(':');
      Builder.Append({$IFDEF VER24P}TNetEncoding{$ELSE}NetEncoding{$ENDIF}.URL.EncodeAuth(FPassword));
      Builder.Append('@');
    end;
    if FServer <> '' then
      Builder.Append(FServer)
    else
      Builder.Append(MnDefValServer);
    Builder.Append(':');
    if FPort <> 0 then
      Builder.Append(IntToStr(FPort))
    else
      Builder.Append(IntToStr(MnDefValPort));

    if FAdditionalServers <> '' then begin
      Builder.Append(',');
      Builder.Append(FAdditionalServers);
    end;

    if FConnectionOptions <> '' then begin
      Builder.Append('/?');
      Builder.Append(FConnectionOptions);
    end;

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure TMongoConnection.CheckClient;
begin
  if FClient = nil then
    raise Exception.Create('Invalid client');
end;

procedure TMongoConnection.ClearCollections;
var
  i: integer;
begin
  for i := 0 to FCollections.Count - 1 do
    TMongoCollection(FCollections.Objects[i]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};

  FCollections.Clear;
end;

function TMongoConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TMongoCommand;
end;

function TMongoConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TMongoRecordset;
end;

function TMongoConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TMongoTransaction;
end;

{$IFNDEF LITE}

class function TMongoConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMongoCommand.GetMapRulesClass;
end;

function TMongoConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TMongoMetadata;
end;

function TMongoConnection.GetLoaderClass: TCRLoaderClass;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise Exception.Create(SFeatureNotSupported);
end;

{$ENDIF}

procedure TMongoConnection.CheckError;
var
  Error: EMongoDBError;
  Fail, NeedFreeError: boolean;
  Msg: PAnsiChar;
begin
  if FError.code = 0 then
    Exit;

  NeedFreeError := True;
  Msg := PAnsiChar(@FError.message[0]);
  Error := EMongoDBError.Create(FError.code, string(Msg));
  try
  {$IFNDEF NODBACCESS}
    Error.Component := Component;
  {$ENDIF}
    Fail := True;
    DoError(Error, Fail);
    if Fail then
      NeedFreeError := False;
  finally
    if NeedFreeError then
      Error.Free;
  end;

  if Fail then
    raise Error
  else
    Abort;
end;

function TMongoConnection.ExecuteJSON(const Database, JSON: string): TMongoDocument;
var
  Command: TCRCommand;
begin
  Command := GetCommand;
  try
    Command.SetCursorState(csInactive); // To prevent blocking execute after previous exec
    Command.SetSQL(JSON);
    Result := TMongoCommand(Command).ExecuteSimple(Database);
  finally
    ReleaseCommand(Command);
  end;
end;

function TMongoConnection.GetCollection(const Name: string): TMongoCollection;
var
  Buf: TBytes;
  n: integer;
  Handle: pmongoc_collection_t;
begin
  n := FCollections.IndexOf(Name);
  if n >= 0 then
    Result := TMongoCollection(FCollections.Objects[n])
  else begin
    Buf := Encoding.UTF8.GetBytes(WideString(Name));
    n := Length(Buf);
    SetLength(Buf, n + 1);
    Buf[n] := 0;
    Handle := FAPI.mongoc_database_get_collection(FDB, @Buf[0]);

    if Handle <> nil then begin
      Result := TMongoCollection.Create(FAPI, FWriteConcern, Handle);
      FCollections.AddObject(Name, Result);
    end
    else
      raise Exception.Create(SInvalidCollection);
  end;
end;

procedure TMongoConnection.DropCollection(const Name: string);
var
  Collection: TMongoCollection;
begin
  Collection := GetCollection(Name);
  if FAPI.mongoc_collection_drop(Collection.FHandle, @FError) then begin
    FCollections.Delete(FCollections.IndexOfObject(Collection));
    Collection.Free;
  end
  else
    CheckError;
end;

function TMongoConnection.CheckIsValid: boolean;
begin
  if not FIsValid then
    Result := False
  else begin
    try
      Ping;
    except
      FIsValid := False;
    end;
    Result := FIsValid;
  end;
end;

procedure TMongoConnection.AssignConnect(Source: TCRConnection);
var
  Src: TMongoConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TMongoConnection(Source);
      Assign(Src);
      FUseUnicode := Src.FUseUnicode;
      FAPI.UnInitialize;
      FAPI.Assign(Src.FAPI);
      FClient := Src.FClient;
      FDB := Src.FDB;
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := FDB <> nil;
      FNativeConnection := False;
    end;
  end;
end;

function TMongoConnection.GetServerVersion: string;
begin
  if FServerVersion = '' then begin
    if FServerVersionFull = '' then
      GetServerVersionFull;

    FServerVersion := FServerVersionFull;
  end;

  Result := FServerVersion;
end;

function TMongoConnection.GetServerVersionFull: string;
var
  Handle: pbson_t;
  Document: TInternalDocument;
  Field: IDocumentExpression;
begin
  if FServerVersionFull = '' then begin
    Handle := FAPI.bson_new;
    if FAPI.mongoc_client_get_server_status(FClient, nil, Handle, @FError) then begin
      Document := TInternalDocument.Create(FAPI, Handle, FUseUnicode);
      try
        Field := Document.FindField('version');
        if Field <> nil then
          FServerVersionFull := Field.Value;
      finally
        Document.Release;
      end;
    end
    else begin
      FAPI.bson_destroy(Handle);
      CheckError;
    end;
  end;

  Result := FServerVersionFull;
end;

function TMongoConnection.GetClientVersion: string;
var
  s: string;
  i, n: integer;
begin
  if FClientVersion = '' then begin
    if @FAPI.mongoc_get_version <> @NotLink then
      FClientVersion := string(FAPI.mongoc_get_version);

    s := FClientVersion;
    i := 1;
    n := Length(s);
    while (i <= n) and CharInSet(s[i], ['0'..'9']) do
      inc(i);
    FLibVersionMajor := StrToIntDef(Copy(s, 1, i - 1), 0);
    Delete(s, 1, i);

    i := 1;
    n := Length(s);
    while (i <= n) and CharInSet(s[i], ['0'..'9']) do
      inc(i);
    FLibVersionMinor := StrToIntDef(Copy(s, 1, i - 1), 0);
    Delete(s, 1, i);
    FLibVersionRelease := StrToIntDef(s, 0);
  end;

  Result := FClientVersion;
end;

function TMongoConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prPort:
      FPort := Value;
    prDatabase:
      FDatabase := Value;
    prUseUnicode:
      FUseUnicode := Value;
    prClientLibrary:
      FClientLibrary := Value;
    prBSONLibrary:
      FBSONLibrary := Value;
    prAdditionalServers:
      FAdditionalServers := Value;
    prConnectionOptions:
      FConnectionOptions := Value;
    prLowercaseObjectId:
      FLowerCaseObjectId := Value;
    prDescribeAmount: begin
      FDescribeAmount := Value;
      if FDescribeAmount <= 0 then
        FDescribeAmount := 1;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMongoConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prPort:
      Value := FPort;
    prDatabase:
      Value := FDatabase;
    prUseUnicode:
      Value := FUseUnicode;
    prClientLibrary:
      Value := FClientLibrary;
    prBSONLibrary:
      Value := FBSONLibrary;
    prAdditionalServers:
      Value := FAdditionalServers;
    prConnectionOptions:
      Value := FConnectionOptions;
    prLowercaseObjectId:
      Value := FLowerCaseObjectId;
    prDescribeAmount:
      Value := FDescribeAmount;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TMongoConnection.ProcessError(ErrorCode: integer; ErrorMsg: string; Component: TObject);
var
  Error: EMongoDBError;
  Fail, NeedFreeError: boolean;
begin
  NeedFreeError := True;
  Error := EMongoDBError.Create(ErrorCode, ErrorMsg);
  try
  {$IFNDEF NODBACCESS}
    Error.Component := Component;
  {$ENDIF}
    Fail := True;
    DoError(Error, Fail);
    if Fail then
      NeedFreeError := False;
  finally
    if NeedFreeError then
      Error.Free;
  end;

  if Fail then
    raise Error
  else
    Abort;
end;

procedure TMongoConnection.Connect(const ConnectString: string);
var
  Buf: TBytes;
  n: integer;
begin
  if FConnected then
    Exit;

  if not FAPI.Initialized then begin
    FAPI.ClientLibrary := FClientLibrary;
    FAPI.BSONLibrary := FBSONLibrary;
    FAPI.Initialize;
  end;

  if ConnectString <> '' then
    Buf := Encoding.UTF8.GetBytes(ConnectString)
  else
    Buf := Encoding.UTF8.GetBytes(BuildURI);

  FClient := nil;
  try
    n := Length(Buf);
    SetLength(Buf, n + 1);
    Buf[n] := 0;
    FClient := FAPI.mongoc_client_new(@Buf[0]);
    if FClient = nil then
      raise Exception.Create(SInvalidURI);

    FWriteConcern.Initialize;

    Ping;

    Buf := Encoding.UTF8.GetBytes(WideString(FDatabase));
    n := Length(Buf);
    SetLength(Buf, n + 1);
    Buf[n] := 0;
    FDB := FAPI.mongoc_client_get_database(FClient, @Buf[0]);
    if FDB = nil then
      raise Exception.Create(SInvalidDatabase);

    FConnected := True;
    FNativeConnection := True;

    inherited;
  except
    on EFailOver do ;
    else begin
      if FClient <> nil then begin
        if FDB <> nil then begin
          FAPI.mongoc_database_destroy(FDB);
          FDB := nil;
        end;

        FAPI.mongoc_client_destroy(FClient);
        FClient := nil;
      end;

      FConnected := False;
      raise;
    end;
  end;
end;

procedure TMongoConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FConnected := False;
  try
    ClearCollections;

    if FNativeConnection then begin
      if FDB <> nil then
        FAPI.mongoc_database_destroy(FDB);
      if FClient <> nil then
        FAPI.mongoc_client_destroy(FClient);
    end;
  finally
    FClient := nil;
    FDB := nil;
    FNativeConnection := True;
  end;

  FServerVersion := '';
  FServerVersionFull := '';
  FClientVersion := '';
  FLibVersionMajor := 0;
  FLibVersionMinor := 0;
  FLibVersionRelease := 0;

  inherited;
end;

procedure TMongoConnection.Ping;
var
  Document: TMongoDocument;
  Reply: IDocumentExpression;
begin
  Document := ExecuteJSON('admin', '{ping:1}');
  try
    if Document <> nil then begin
      Reply := Document.FindField('ok');
      if (Reply = nil) or (Reply.Value <> 1) then
        raise Exception.Create('Ping error');
    end
    else
      raise Exception.Create('Ping error');
  finally
    if Document <> nil then
      Document.Release;
  end;
end;

{ TMongoTransaction }

procedure TMongoTransaction.Commit;
begin
  // do nothing
end;

procedure TMongoTransaction.Rollback;
begin
  // do nothing
end;

procedure TMongoTransaction.StartTransaction;
begin
  // do nothing
end;

{ TMongoCommand }

constructor TMongoCommand.Create;
begin
  inherited;

  FInternalCommand := nil;
  FProjection := nil;
  FCollectionName := '';
  FCollection := nil;

  FRowsAffected := 0;
end;

destructor TMongoCommand.Destroy;
begin
  FInternalCommand.Release;
  FProjection.Release;

  inherited;
end;

class function TMongoCommand.GetParserClass: TSQLParserClass;
begin
  Result := TSQLParser;
end;

{$IFNDEF LITE}
class function TMongoCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMongoMapRules;
end;
{$ENDIF}

function TMongoCommand.InternalExecute: TMongoCursor;
var
  NeedPrepare: boolean;
begin
  Result := nil;
  NeedPrepare := not GetPrepared;
  if NeedPrepare then
    Prepare;

  try
    if FParsedSQLType = qtSelect then begin
      FCollection := TMongoConnection(GetConnection).GetCollection(FCollectionName);
      Result := FCollection.Find(FInternalCommand, FProjection, FSkip, FLimit);
    end
    else
      Assert(False);
//    else
//      Result.FHandle := TMongoConnection(GetConnection).FAPI.mongoc_database_command(
//        TMongoConnection(GetConnection).FDB,
//        MONGOC_QUERY_NONE, 0, 0, 0,
//        FInternalCommand.FHandle,
//        nil, nil);

    SetCursorState(csExecuted);
  finally
    if NeedPrepare and (FCommandType <> ctCursor) then
      UnPrepare;
  end;
end;

procedure TMongoCommand.InternalInsert(const Document: TMongoDocument);
begin
  if not FCollection.Insert(Document, @TMongoConnection(GetConnection).FError) then
    TMongoConnection(GetConnection).CheckError;
end;

procedure TMongoCommand.InternalUpdate(const Document: TMongoDocument);
begin
  if not FCollection.Update(Document, @TMongoConnection(GetConnection).FError) then
    TMongoConnection(GetConnection).CheckError;
end;

procedure TMongoCommand.InternalDelete(const Document: TMongoDocument);
begin
  if not FCollection.Delete(Document, @TMongoConnection(GetConnection).FError) then
    TMongoConnection(GetConnection).CheckError;
end;

procedure TMongoCommand.SetProjection(const Projection: string);
var
  Connection: TMongoConnection;
begin
  if FProjection <> nil then begin
    FProjection.Release;
    FProjection := nil;
  end;

  if Projection <> '' then begin
    Connection := TMongoConnection(GetConnection);
    FProjection := TInternalDocument.Create(Connection.FAPI, Connection.FUseUnicode);
    TInternalDocument(FProjection).SetData(Connection.FAPI.CommandDeserializer.FromText(Projection, nil, Connection.FUseUnicode));
  end;
end;

procedure TMongoCommand.Prepare;
begin
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);
  if FParams.Count > 0 then
    raise Exception.Create(SInvalidParams);

  TInternalDocument(FInternalCommand).Prepare;
  if FProjection <> nil then
    TInternalDocument(FProjection).Prepare;
  SetCursorState(csPrepared);
end;

procedure TMongoCommand.Unprepare;
begin
  if GetPrepared then begin
    TInternalDocument(FInternalCommand).Unprepare;
    if FProjection <> nil then
      TInternalDocument(FProjection).Unprepare;
  end;
  SetCursorState(csInactive);
end;

function TMongoCommand.GetPrepared: boolean;
begin
  Result := (FInternalCommand <> nil) and TInternalDocument(FInternalCommand).Prepared;
end;

procedure TMongoCommand.Execute;
var
  Document: TMongoDocument;
  Cursor: TMongoCursor;
  NeedCheckReply: boolean;
  Reply: IDocumentExpression;
begin
  if CommandType = ctCursor then begin
    Cursor := ExecuteCursor;
    Cursor.Free;
  end
  else begin
    Document := ExecuteSimple(TMongoConnection(GetConnection).FDatabase);
    try
      NeedCheckReply := FParsedSQLType in [qtInsert, qtUpdate, qtDelete];
      if NeedCheckReply then begin
        Reply := Document.FindField('ok');
        if (Reply = nil) or (Reply.Value <> 1) then
          raise Exception.Create('Command error');
      end;
    finally
      Document.Release;
    end;
  end;
end;

function TMongoCommand.ExecuteCursor: TMongoCursor;
begin
  TMongoConnection(GetConnection).CheckClient;

  try
    Result := InternalExecute;
  except
    if Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;

  if Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

function TMongoCommand.ExecuteSimple(const Database: string): TMongoDocument;
var
  NeedPrepare: boolean;
  Handle: pbson_t;
  Field: IDocumentExpression;
begin
  Result := nil;
  FRowsAffected := 0;
  TMongoConnection(GetConnection).CheckClient;

  NeedPrepare := not GetPrepared;
  if NeedPrepare then
    Prepare;

  Handle := TMongoConnection(GetConnection).FAPI.bson_new;
  try
    if TMongoConnection(GetConnection).FAPI.mongoc_client_command_simple(
      TMongoConnection(GetConnection).FClient,
      PAnsiChar(AnsiString(Database)),
      TInternalDocument(FInternalCommand).Handle,
      nil,
      Handle,
      @TMongoConnection(GetConnection).FError)
    then begin
      Result := TInternalDocument.Create(TMongoConnection(GetConnection).FAPI, Handle, TMongoConnection(GetConnection).FUseUnicode);
      TJSONTextWriter.CalcSize(TInternalDocument(Result).Data, TInternalDocument(Result).FieldMap);
      Field := Result.FindField('n');
      if Field <> nil then
        FRowsAffected := Field.Value;
    end
    else begin
      TMongoConnection(GetConnection).FAPI.bson_destroy(Handle);
      TMongoConnection(GetConnection).CheckError;
    end;
  finally
    if NeedPrepare then
      UnPrepare;
  end;
end;

function TMongoCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TMongoCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

function TMongoCommand.GetProp(Prop: integer; out Value: Variant): boolean;
begin
  case Prop of
    prRowsProcessed: begin
      Value := FRowsAffected;
      Result := True;
    end;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TMongoCommand.SetConnection(Value: TCRConnection);
begin
  if Value is TMongoConnector then begin
    if TMongoConnector(Value).FInternalConnection = nil then
      inherited SetConnection(nil)
  {$IFNDEF NOSQL}
    else if TMongoConnector(Value).FInternalConnection is TMongoVirtualConnection then
      inherited SetConnection(TMongoVirtualConnection(TMongoConnector(Value).FInternalConnection).FInternalConnection)
  {$ENDIF}
    else
      inherited SetConnection(TMongoConnector(Value).FInternalConnection);
  end
  else
    inherited;
end;

procedure TMongoCommand.SetSQL(const Value: string);
begin
  FParsedSQLType := qtUnparsed;
  FUserSQL := Value;
  FSQL := ParseSQL(Value, FParams);
end;

function TMongoCommand.ParseSQL(const SQL: string; Params: TParamDescs; const RenamePrefix: string = ''): string;
var
  Connection: TMongoConnection;
  Field: IDocumentExpression;
  Builder: StringBuilder;
  n: integer;
begin
  if SQL <> '' then begin
    FParsedSQLType := qtUnparsed;
    Connection := TMongoConnection(GetConnection);

    if FInternalCommand <> nil then
      FInternalCommand.Release;
    FInternalCommand := TInternalDocument.Create(Connection.FAPI, Connection.FUseUnicode);

    TInternalDocument(FInternalCommand).SetData(Connection.FAPI.CommandDeserializer.FromText(SQL, nil, Connection.FUseUnicode));
     if (TInternalDocument(FInternalCommand).Data = nil) or not (TInternalDocument(FInternalCommand).Data is TJSONObject) then
      raise Exception.Create('Invalid command text');
    TJSONTextWriter.CalcSize(TInternalDocument(FInternalCommand).Data, TInternalDocument(FInternalCommand).FieldMap);

    Result := SQL;
    FCommandType := ctStatement;
    FParsedSQLType := qtUnknown;
    FCollectionName := '';
    FSkip := 0;
    FLimit := 0;

    if FInternalCommand.FieldCount > 0 then begin
      Field := FInternalCommand.Fields[0];
      if SameText(Field.Name, 'find') then begin
        FCommandType := ctCursor;
        FParsedSQLType := qtSelect;
        FCollectionName := Field.Value;

        if (Connection.FLibVersionMajor <> 1) or (Connection.FLibVersionMinor > 3) then begin
          Builder := StringBuilder.Create(Length(SQL));
          try
            n := 0;
            Builder.Append('{');
            Field := FInternalCommand.FindField('filter');
            if Field <> nil then begin
              Builder.Append('"$query":' + Field.Value);
              Inc(n);
            end;
            Field := FInternalCommand.FindField('sort');
            if Field <> nil then begin
              if n > 0 then
                Builder.Append(',');
              Builder.Append('"$orderby":' + Field.Value);
            end;
            Builder.Append('}');

            Field := FInternalCommand.FindField('skip');
            if Field <> nil then
              FSkip := Field.Value;
            Field := FInternalCommand.FindField('limit');
            if Field <> nil then
              FLimit := Field.Value;

            Field := FInternalCommand.FindField('projection');
            if Field <> nil then
              SetProjection(Field.Value);

            TInternalDocument(FInternalCommand).SetData(Connection.FAPI.CommandDeserializer.FromText(Builder.ToString, nil, Connection.FUseUnicode));
          finally
            Builder.Free;
          end;
        end;
      end
      else if SameText(Field.Name, 'insert') then begin
        FCommandType := ctStatement;
        FParsedSQLType := qtInsert;
        FCollectionName := Field.Value;
      end
      else if SameText(Field.Name, 'update') then begin
        FCommandType := ctStatement;
        FParsedSQLType := qtUpdate;
        FCollectionName := Field.Value;
      end
      else if SameText(Field.Name, 'delete') then begin
        FCommandType := ctStatement;
        FParsedSQLType := qtDelete;
        FCollectionName := Field.Value;
      end;
    end;
  end;
end;

function TMongoCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
begin
  Result := '';
end;

{ TMongoFieldDesc }

constructor TMongoFieldDesc.Create(RecordSetClass: TRecordSetClass);
begin
  inherited;

  FFullName := '';
  FMaskedName := '';
end;

{ TMongoRecordset }

constructor TMongoRecordset.Create;
begin
  inherited;

  FCursor := nil;
  FFetched := 0;
  FFetchAll := False;
  FDescribeMethod := dmGrid;
  FIdGenerationMode := igClient;
  FFetchAll := False;
  FFetchRows := 25;

  FPrefetchedDocs := TList.Create;
  FCachedFields := TStringList.Create;
  FCachedFields.Sorted := True;
end;

destructor TMongoRecordset.Destroy;
begin
  inherited;

  FCursor.Free;
  FPrefetchedDocs.Free;
  FCachedFields.Free;
end;

procedure TMongoRecordset.ExecCommand(Iters: integer = 1; Offset: integer = 0);
var
  Document: TMongoDocument;
  NeedCheckReply: boolean;
  Reply: IDocumentExpression;
begin
  if (Iters = 1) and (Offset = 0) then begin
    if FCommand.CommandType = ctCursor then begin
      if FCursor = nil then
        FCursor := TMongoCommand(FCommand).ExecuteCursor;
    end
    else begin
      Document := TMongoCommand(FCommand).ExecuteSimple(TMongoConnection(GetConnection).FDatabase);
      try
        NeedCheckReply := FCommand.ParsedSQLType in [qtInsert, qtUpdate, qtDelete];
        if NeedCheckReply then begin
          Reply := Document.FindField('ok');
          if (Reply = nil) or (Reply.Value <> 1) then
            raise Exception.Create('Command error');
        end;
      finally
        Document.Release;
      end;
    end;
  end
  else
    raise Exception.Create(SFeatureNotSupported);
  FWaitForFetchBreak := False;
end;

procedure TMongoRecordset.ClearFields;
begin
  inherited;

  ClearDocumentType;
end;

procedure TMongoRecordset.UnPrepare;
var
  i, n: integer;
  Document: TMongoDocument;
begin
  inherited;

  FreeAndNil(FCursor);
  n := FPrefetchedDocs.Count;
  if n > 0 then
    for i := 0 to n - 1 do begin
      Document := TMongoDocument(FPrefetchedDocs[i]);
      if (Document <> nil) and (TInternalDocument(Document).State <> dsStored) then
        Document.Release;
    end;
  FPrefetchedDocs.Clear;
  FCommand.Unprepare;
end;

function TMongoRecordset.IsFullReopen: boolean;
begin
  Result := True;
end;

class function TMongoRecordset.IsComplexDataType(DataType: word): boolean;
begin
  Result := (DataType = dtObject) or inherited IsComplexDataType(DataType);
end;

procedure TMongoRecordset.CheckDocumentType;
begin
  ClearDocumentType;

  FDocumentType := TMongoObjectType.Create('');
end;

procedure TMongoRecordset.ClearDocumentType;
begin
  if FDocumentType <> nil then begin
    Assert(FDocumentType.RefCount = 1);

    FDocumentType.Release;
    FDocumentType := nil;
  end;
end;

procedure TMongoRecordset.Prefetch;
var
  Handle: pbson_t;
  Document: TInternalDocument;
  Fetched: integer;
begin
  Fetched := 0;
  if (FDescribeMethod = dmObject) then
    FFetchAmount := 1
  else
    FFetchAmount := TMongoConnection(GetConnection).FDescribeAmount;

  repeat
    if not FCursor.More then
      Break;

    Handle := FCursor.Next;
    if Handle <> nil then begin
      Document := TInternalDocument.Create(TMongoConnection(GetConnection).FAPI, Handle, TMongoConnection(GetConnection).FUseUnicode, FImplicitAddFields, FImplicitChangeType);
      TJSONTextWriter.CalcSize(Document.Data, Document.FieldMap);
      Document.FieldMap.Sorted := True;

      FPrefetchedDocs.Add(Document);
      Inc(Fetched);
    end
    else
      Break;

    if Fetched >= FFetchAmount then
      Break;
  until EOF;
end;

function TMongoRecordset.DescribeFieldDesc(const ParentField, RootField: TMongoFieldDesc; const Name: string; const Value: TJSONValue; const CreateField: boolean = True): TMongoFieldDesc;
var
  TableName,
  DatabaseName,
  FieldName,
  FieldFullName,
  FieldActualName,
  FieldMaskedName: string;
  TableInfo: TCRTableInfo;
  DBType: Word;
  DBLength,
  DBScale,
  FieldIndex: integer;
  DataType,
  SubDataType: Word;
  Attribute: TMongoAttribute;
  Chain: TAttributeChain;
  NeedCreateField,
  ForceUseString: boolean;
{$IFNDEF LITE}
  FetchConverter: TFetchConverter;
{$ENDIF}

  function IsCompatible(const Field: TMongoFieldDesc; const DataType: Word; const Value: TJSONValue): boolean;
  begin
    if Field.FData = nil then
      Result := Field.DataType = DataType
    else
      Result := Field.FData.IsEqual(Value);
  end;

begin
  ForceUseString := False;

  TableInfo := nil;
  TableName := TMongoCommand(FCommand).FCollectionName;
  DatabaseName := TMongoConnection(GetConnection).FDatabase;
  if TableName <> '' then begin
    TableInfo := FTablesInfo.FindByName(TableName);
    if TableInfo = nil then begin
      TableInfo := FTablesInfo.Add;
      TableInfo.TableName := TableName;
      TableInfo.TableAlias := '';
      if DatabaseName <> '' then
        TableName := DatabaseName + '.' + TableName;
      TableInfo.TableNameFull := TableName;
    end;
  end;

  FieldName := Name;
  FieldFullName := '';
  FieldMaskedName := '';
  FieldActualName := Name;
  if ParentField = nil then begin
    if TableInfo <> nil then begin
      FieldName := TableInfo.TableName;
      FieldActualName := TableInfo.TableName;
    end;
  end
  else begin
    FieldActualName := ParentField.ActualName + '.' + FieldName;
    if ParentField.FullName <> '' then begin
      FieldFullName := ParentField.FullName + '.' + FieldName;
      FieldMaskedName := ParentField.MaskedName + '_' + FieldName;
    end
    else begin
      FieldFullName := FieldName;
      FieldMaskedName := FieldName;
    end;
  end;

  if ParentField <> nil then begin
    DBType := TMongoConverterManager.GetDBType(Value.Tag);
    DBLength := Value.Size;
  end
  else begin
    DBType := mongoObject;
    DBLength := 0;
  end;
  DBScale := 0;

  DataType := TMongoConverterManager.GetDataType(DBType, SubDataType);
  if (DataType = dtString) and TMongoConnection(GetConnection).FUseUnicode then
    DataType := dtWideString;

  FieldIndex := FCachedFields.IndexOf(FieldActualName);
  NeedCreateField := FieldIndex < 0;
  if not NeedCreateField then
    Result := FCachedFields.Objects[FieldIndex] as TMongoFieldDesc;

  if ParentField <> nil then begin
    if (Value.Tag in JSONComplexTags) and FComplexAsString then
      ForceUseString := True
    else if Value.Tag in JSONConstantTags then
      ForceUseString := True
    else if not NeedCreateField and not IsCompatible(Result, DataType, Value) then
      ForceUseString := True;
  end;

  if ForceUseString then
    if TMongoConnection(GetConnection).FUseUnicode then
      DataType := dtWideString
    else
      DataType := dtString;

{$IFNDEF LITE}
  if ParentField <> nil then begin
    FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale);

    if FetchConverter <> nil then begin
        DataType := FetchConverter.InternalDataType;

      if DataType in [dtString, dtWideString, dtExtString, dtExtWideString] then
        ForceUseString := True;
    end;
  end;
{$ENDIF}

  if NeedCreateField then begin
    Result := TMongoFieldDesc(CreateFieldDesc);
    FCachedFields.AddObject(FieldActualName, Result);

    Result.ParentField := ParentField;
    Result.Name := FieldName;
    Result.FullName := FieldFullName;
    Result.MaskedName := FieldMaskedName;
    Result.ActualName := FieldActualName;
    Result.TableInfo := TableInfo;

    if SameText(FieldName, '_id') then
      Result.IsKey := True;

    if DataType = dtObject then begin
      Result.FData := Value;
      Result.ObjectType := TMongoObjectType.Create(FieldFullName);
      Result.ObjectType.Release;
    end;
  end
  else if DBLength < Result.Length then
    DBLength := Result.Length;

  if not TMongoFieldDesc(Result).FForceUseString then begin
    Result.DataType := DataType;
    TMongoFieldDesc(Result).FForceUseString := ForceUseString;
    if ForceUseString then
      SubDataType := mongoGeneric;
    Result.SubDataType := SubDataType;
    Result.DBType := DBType;
  end;

  if (DataType = dtBytes) and (DBLength = 0) then
    DBLength := 1;
  Result.DBLength := DBLength;
  Result.DBScale := DBScale;
  Result.Length := DBLength;
  Result.Size := GetBufferSize(Result.DataType, Result.Length);
  if FPutBufferSize < Result.Size then
    FPutBufferSize := Result.Size;

  if Result.ParentField <> nil then begin
    Chain := TMongoObjectType(RootField.ObjectType).FindAttributeChain(FieldFullName);
    try
      if Chain = nil then begin
        Attribute := TMongoObjectType(Result.ParentField.ObjectType).AddAttribute(Result);
        TMongoObjectType(Result.ParentField.ObjectType).AttributeList.AddObject(Attribute.Name, Attribute);
      end
      else begin
        Attribute := Chain.Attribute as TMongoAttribute;
        Attribute.DataType := DataType;
        Attribute.SubDataType := SubDataType;
        Attribute.Length := Result.Length;
      end;
    finally
      Chain.Free;
    end;
  end;

  if not CreateField then
    FreeAndNil(Result)
  else if not NeedCreateField then
    Result := nil;
end;

procedure TMongoRecordset.CreateFieldDescs;
var
  i, j, k, StartIndex: integer;
  Document: TInternalDocument;
  RootField,
  ParentFieldDesc,
  FieldDesc: TMongoFieldDesc;
  Field: IDocumentExpression;
  Found: boolean;
begin
  if FCommand.CommandType <> ctCursor then
    raise Exception.Create(SNotRows);

  if TMongoCommand(GetCommand).GetCursorState < csExecuted then
    FCursor := TMongoCommand(FCommand).ExecuteCursor;

  FCachedFields.Clear;

  CheckDocumentType;
  if FPrefetchedDocs.Count = 0 then
    Prefetch;

  FPutBufferSize := 0;
  FTablesInfo.BeginUpdate;
  try
    RootField := DescribeFieldDesc(nil, nil, '', nil);
    RootField.FieldNo := 1;
    RootField.ActualFieldNo := 0;
    FFields.Add(RootField);

    if FDescribeMethod = dmGrid then begin
      for i := 0 to FPrefetchedDocs.Count - 1 do begin
        Document := TInternalDocument(FPrefetchedDocs[i]);
        Document.SetObjectType(RootField.ObjectType);

        for j := 0 to Document.FieldCount - 1 do begin
          Field := Document.Fields[j];

          FieldDesc := DescribeFieldDesc(RootField, RootField, Field.Name, Field.GetData);
          if FieldDesc <> nil then begin
            FieldDesc.FieldNo := FFields.Count + 1;
            FieldDesc.ActualFieldNo := FFields.Count;
            FFields.Add(FieldDesc);
          end;
        end;
      end;

      StartIndex := 1;
      repeat
        Found := False;

        for i := StartIndex to FFields.Count - 1 do begin
          ParentFieldDesc := FFields[i] as TMongoFieldDesc;

          if (ParentFieldDesc.DataType = dtObject) and (ParentFieldDesc.FData <> nil) then begin
            if not Found then
              StartIndex := i + 1;
            Found := True;
          end
          else
            Continue;

          case ParentFieldDesc.SubDataType of
            mongoObject:
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  for k := 0 to TJSONObject(Field.GetData).Pairs.Count - 1 do begin
                    FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, TJSONObject(Field.GetData).Pairs[k].Name.Value, TJSONObject(Field.GetData).Pairs[k].Value, ParentFieldDesc.DataType = dtObject);

                    if FieldDesc <> nil then
                      FFields.Insert(ParentFieldDesc.FieldNo + k, FieldDesc);
                  end;
                end;
              end;
            mongoArray:
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  for k := 0 to TJSONArray(Field.GetData).Elements.Count - 1 do begin
                    FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, IntToStr(k), TJSONValue(TJSONArray(Field.GetData).Elements[k]), ParentFieldDesc.DataType = dtObject);

                    if FieldDesc <> nil then
                      FFields.Insert(ParentFieldDesc.FieldNo + k, FieldDesc);
                  end;
                end;
              end;
            mongoRegex: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrPattern, TJSONRegex(Field.GetData).Pattern);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrOptions, TJSONRegex(Field.GetData).Options);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo + 1, FieldDesc);
                end;
              end;
            end;
            mongoJavaCode: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrCode, TJSONJavaCode(Field.GetData).Code);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                end;
              end;
            end;
            mongoJavaScopeCode: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrCode, TJSONJavaScopeCode(Field.GetData).Code);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrScope, TJSONJavaScopeCode(Field.GetData).Scope);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo + 1, FieldDesc);
                end;
              end;
            end;
            mongoBinary: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrBinary, TJSONBinary(Field.GetData).Binary);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrSubtype, TJSONBinary(Field.GetData).Subtype);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo + 1, FieldDesc);
                end;
              end;
            end;
            mongoDBPointer: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrName, TJSONDBPointer(Field.GetData).Name);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrValue, TJSONDBPointer(Field.GetData){$IFDEF VER12P}.Value{$ELSE}.GetValue{$ENDIF});
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo + 1, FieldDesc);
                end;
              end;
            end;
            mongoTimestamp: begin
              for j := 0 to FPrefetchedDocs.Count - 1 do begin
                Document := TInternalDocument(FPrefetchedDocs[j]);
                Field := Document.FindField(ParentFieldDesc.FullName);

                if Field <> nil then begin
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrTime, TJSONTimestamp(Field.GetData).Timestamp);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo, FieldDesc);
                  FieldDesc := DescribeFieldDesc(ParentFieldDesc, RootField, cAttrIncrement, TJSONTimestamp(Field.GetData).Increment);
                  if FieldDesc <> nil then
                    FFields.Insert(ParentFieldDesc.FieldNo + 1, FieldDesc);
                end;
              end;
            end;
          end;

          for j := ParentFieldDesc.FieldNo to FFields.Count - 1 do begin
            FieldDesc := FFields[j] as TMongoFieldDesc;

            FieldDesc.FieldNo := j + 1;
            FieldDesc.ActualFieldNo := j;
          end;
        end;
      until not Found;
    end;

    SetLength(FPutBuffer, FPutBufferSize);
  finally
    FTablesInfo.EndUpdate;
  end;
end;

procedure TMongoRecordset.CreateCommand;
begin
  SetCommand(TMongoCommand.Create);
end;

procedure TMongoRecordset.FillDataFieldDescs(out DataFieldDescs: TFieldDescArray; ForceUseAllKeyFields: boolean);
var
  RootFieldDesc: TCRFieldDesc;

  function CheckField(FieldDesc: TCRFieldDesc): boolean;
  begin
    Result := False;

    if (FieldDesc.HasParent and (FieldDesc.ParentField <> RootFieldDesc)) or
      ((FieldDesc = FIdentityField) and not IdentityFieldIsData)
    then
      Exit;

    if not ForceUseAllKeyFields and
      ((FieldDesc.TableInfo = nil) or (FieldDesc.TableInfo <> UpdatingTableInfo))
    then
      Exit;

    Result := not FieldDesc.ReadOnly;
  end;

  procedure AddFieldDesc(FieldDesc: TCRFieldDesc);
  begin
    SetLength(DataFieldDescs, Length(DataFieldDescs) + 1);
    DataFieldDescs[High(DataFieldDescs)] := FieldDesc;
  end;

var
  i: integer;
  FieldDesc: TCRFieldDesc;
begin
  SetLength(DataFieldDescs, 0);
  if FFields.Count = 0 then
    Exit;

  RootFieldDesc := TCRFieldDesc(FFields[0]);

  for i := 1 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if FieldDesc.Updateable and CheckField(FieldDesc) then
      AddFieldDesc(FieldDesc);
  end;
end;

function TMongoRecordset.NeedInitFieldsOnPrepare: boolean;
begin
  Result := False;
end;

procedure TMongoRecordset.InternalClose;
begin
  UnPrepare;

  inherited;
end;

procedure TMongoRecordset.InternalAppend(RecBuf: IntPtr);
var
  Document,
  OidDocument: TInternalDocument;
  Oid: bson_oid_t;
  NeedOid: boolean;
begin
  if Fields.Count = 0 then
    Exit;

  Document := TInternalDocument(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Fields[0].Offset)));

  NeedOid := Document.FindField('_id') = nil;

  if NeedOid and (FIdGenerationMode = igClient) then begin
    TMongoConnection(GetConnection).FAPI.bson_oid_init(@Oid, nil);
    Document.SetOid('_id', Oid.bytes);
  end;

  Document.State := dsStored;
  TMongoCommand(GetCommand).InternalInsert(Document);

  if NeedOid and (FIdGenerationMode = igDatabase) then begin
    OidDocument := TInternalDocument(TMongoCommand(GetCommand).FCollection.GetLastError);
    if OidDocument <> nil then begin
      {TODO: }
    end;
  end;
end;

procedure TMongoRecordset.InternalDelete;
var
  Document: TInternalDocument;
begin
  if Fields.Count = 0 then
    Exit;

  Document := TInternalDocument(GetGCHandleTarget(Marshal.ReadIntPtr(PtrOffset(CurrentItem, sizeof(TItemHeader)), Fields[0].Offset)));
  TMongoCommand(GetCommand).InternalDelete(Document);
end;

procedure TMongoRecordset.InternalUpdate(RecBuf: IntPtr);
var
  Document: TInternalDocument;
begin
  if Fields.Count = 0 then
    Exit;

  Document := TInternalDocument(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Fields[0].Offset)));
  TMongoCommand(GetCommand).InternalUpdate(Document);
end;

function TMongoRecordset.InternalCompareFieldValue(ValuePtr: IntPtr; ValueLen: Word; ValueType: Word; DataBuf: IntPtr; DataLen: Word; FieldType: Word; HasParent: boolean; IsFixed: boolean; const Options: TCompareOptions): integer;
var
  Value: TJSONValue;
  str: string;
begin
  try
    Value := TJSONValue(ValuePtr);
  except
    Value := nil;
  end;

  if Value <> nil then
    case Value.Tag of
      jtString,
      jtNumber,
      jtBoolean,
      jtInt32,
      jtInt64,
      jtDateTime,
      jtDouble: ValuePtr := Value.ValuePtr;
      jtObjectId:
        if ValueType in [dtString, dtWideString] then begin
          str := TJSONObjectId(Value).AsString;
          if ValueType = dtString then
            ValuePtr := Marshal.StringToHGlobalAnsi(AnsiString(str))
          else
            ValuePtr := Marshal.StringToHGlobalUni(WideString(str));
        end
        else
          ValuePtr := Value.ValuePtr;
      jtObject,
      jtArray,
      jtNull,
      jtJavaCode,
      jtUndefined,
      jtJavaScopeCode,
      jtRegex,
      jtTimestamp,
      jtBinary,
      jtMinKey,
      jtMaxKey,
      jtDBPointer:
        raise Exception.Create('Field can not be used for comparison');
    end;

  Value := TJSONValue(DataBuf);

  case Value.Tag of
    jtString,
    jtNumber,
    jtBoolean,
    jtInt32,
    jtInt64,
    jtDateTime,
    jtDouble: DataBuf := Value.ValuePtr;
    jtObjectId:
      if ValueType in [dtString, dtWideString] then begin
        str := TJSONObjectId(Value).AsString;
        if ValueType = dtString then
          DataBuf := Marshal.StringToHGlobalAnsi(AnsiString(str))
        else
          DataBuf := Marshal.StringToHGlobalUni(WideString(str));
      end
      else
        DataBuf := Value.ValuePtr;
    jtObject,
    jtArray,
    jtNull,
    jtJavaCode,
    jtUndefined,
    jtJavaScopeCode,
    jtRegex,
    jtTimestamp,
    jtBinary,
    jtMinKey,
    jtMaxKey,
    jtDBPointer:
      raise Exception.Create('Field can not be used for comparison');
  end;

  Result := inherited InternalCompareFieldValue(ValuePtr, ValueType, ValueLen, DataBuf, DataLen, FieldType, HasParent, IsFixed, Options);
end;

procedure TMongoRecordset.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  Item, RecBuf: IntPtr;
  Field: TFieldDesc;
  Handle: pbson_t;
  Document: TInternalDocument;
  IndPtr: IntPtr;
  FieldPtr: IntPtr;
begin
  RowsObtained := 0;
  if Fields.Count = 0 then
    Exit;

  try
    Item := PtrOffset(Block, SizeOf(TBlockHeader));

    while True do begin
      if RowsObtained = FFetchRows then
        Break;

      RecBuf := PtrOffset(Item, SizeOf(TItemHeader));

      Field := Fields[0];
      FieldPtr := PtrOffset(RecBuf, Field.Offset);
      IndPtr := PtrOffset(RecBuf, DataSize);

      if FFetched < FPrefetchedDocs.Count then
        Document := TInternalDocument(FPrefetchedDocs[FFetched])
      else begin
        Handle := FCursor.Next;

        if Handle = nil then begin
          FCommand.SetCursorState(csFetched);
          Exit;
        end;

        Document := TInternalDocument.Create(TMongoConnection(GetConnection).FAPI, Handle, TMongoConnection(GetConnection).FUseUnicode, FImplicitAddFields, FImplicitChangeType);
        Document.SetObjectType(Field.ObjectType);
        TJSONTextWriter.CalcSize(Document.Data, Document.FieldMap);
        Document.FieldMap.Sorted := True;
      end;

      Document.State := dsStored;
      Marshal.WriteIntPtr(FieldPtr, Document.GCHandle);

      Marshal.WriteByte(IndPtr, 0);
      Item := PtrOffset(Item, RecordSize + SizeOf(TItemHeader));
      Inc(RowsObtained);
      Inc(FFetched);
    end;
  finally
    if (FFetched > 0) and (FFetched >= FPrefetchedDocs.Count) then begin
      FPrefetchedDocs.Clear;
      FFetched := 0;
    end;
  end;
end;

function TMongoRecordset.GetChildFieldIsNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
var
  DBObject: IntPtr;
  RootField: TFieldDesc;
  AttrName: string;
begin
  GetChildFieldInfo(Field, RootField, AttrName);
  DBObject := Marshal.ReadIntPtr(RecBuf, RootField.Offset);
  if DBObject <> nil then
    Result := TInternalDocument(GetGCHandleTarget(DBObject)).GetAttrIsNull(Field)
  else
    Result := True;
end;

function TMongoRecordset.GetFieldDescType: TFieldDescClass;
begin
  Result := TMongoFieldDesc;
end;

procedure TMongoRecordset.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Document: TInternalDocument;
begin
  Document := nil;

  case Field.DataType of
    dtObject: begin
      Document := TInternalDocument.Create(TMongoConnection(GetConnection).FAPI, TMongoConnection(GetConnection).FUseUnicode, FImplicitAddFields, FImplicitChangeType);
      Document.SetObjectType(Field.ObjectType);
    end;
    dtArray:;
  else
    inherited;
  end;

  if Document <> nil then
    Marshal.WriteIntPtr(PtrOffset(RecBuf, Field.Offset), Document.GCHandle);
end;

procedure TMongoRecordset.CopyComplexField(Source: IntPtr; Dest: IntPtr; Field: TFieldDesc);
var
  DocumentSrc,
  DocumentDest: TMongoDocument;
begin
  case Field.DataType of
    dtObject: begin
      DocumentSrc := TMongoDocument(GetGCHandleTarget(Marshal.ReadIntPtr(Source, Field.Offset)));
      DocumentDest := TMongoDocument(GetGCHandleTarget(Marshal.ReadIntPtr(Dest, Field.Offset)));
      DocumentDest.Assign(DocumentSrc);
    end;
  else
    inherited;
  end;
end;

procedure TMongoRecordset.FreeComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Document: TMongoDocument;
begin
  case Field.DataType of
    dtObject: begin
      Document := TMongoDocument(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.Offset)));
      if (Document <> nil) {and (TInternalDocument(Document).State = dsStored)} then
        Document.Release;
    end;
    dtArray:;
  else
    inherited;
  end;
end;

procedure TMongoRecordset.GetFieldData(Field: TFieldDesc; DataBuf: IntPtr; var DataLen: Word; Dest: IntPtr; NeedConvert: boolean);

  function ToAnsiString(const StrValue: string): IntPtr;
  var
    aStr: AnsiString;
  begin
    aStr := AnsiString(Unquote(StrValue));
    DataLen := Word(LengthA(astr));
    Move(PAnsiChar(aStr)^, FPutBuffer[0], DataLen);
    Result := FPutBuffer;
  end;

  function ToWideString(const StrValue: string): IntPtr;
  var
    wStr: WideString;
  begin
    wStr := WideString(Unquote(StrValue));
    DataLen := Word(Length(wStr));
    Move(PWideChar(wStr)^, FPutBuffer[0], DataLen * SizeOf(WideChar));
    Result := FPutBuffer;
  end;

var
  Value: TJSONValue;
  ValueBuf: IntPtr;
  str: string;
begin
  if Field.FieldNo = 1 then
    Marshal.WriteIntPtr(Dest, Marshal.ReadIntPtr(DataBuf))
  else if (Field.FieldDescKind = fdkData) then begin
    Value := TJSONValue(DataBuf);

    if TMongoFieldDesc(Field).FForceUseString then begin
      TMongoDataConverters.JsonToString(Value, str);
      if Field.DataType = dtString then
        ValueBuf := ToAnsiString(str)
      else
        ValueBuf := ToWideString(str);
    end
    else
    case Value.Tag of
      jtNumber,
      jtBoolean,
      jtInt32,
      jtInt64,
      jtDateTime,
      jtDouble:
        ValueBuf := Value.ValuePtr;
      jtBytes: begin
        ValueBuf := Value.ValuePtr;
        DataLen := Value.Size;
      end;
      jtString: begin
        ValueBuf := Value.ValuePtr;
        DataLen := Value.Size - 2;
      end;
      jtNull,
      jtObjectId,
      jtDecimal128: begin
        str := Value.AsString;
        if (Value.Tag = jtObjectId) and TMongoConnection(GetConnection).FLowerCaseObjectId then
          str := LowerCase(str);
        if Field.DataType = dtString then
          ValueBuf := ToAnsiString(str)
        else
          ValueBuf := ToWideString(str);
      end;
    else
      if FImplicitChangeType then begin
        TMongoDataConverters.JsonToString(Value, str);
        if not TMongoConnection(GetConnection).FUseUnicode then
          ValueBuf := ToAnsiString(str)
        else
          ValueBuf := ToWideString(str);
      end
      else
        raise Exception.CreateFmt(SIncompatibleData, [Field.Name]);
    end;

    inherited GetFieldData(Field, ValueBuf, DataLen, Dest, NeedConvert);
  end
  else
    inherited GetFieldData(Field, DataBuf, DataLen, Dest, NeedConvert);
end;

procedure TMongoRecordset.PutField(Field: TFieldDesc; RecBuf: IntPtr; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
var
  DataBuf: IntPtr;
  IsNullValue,
  NativeBuffer: boolean;
  DataLen: Word;
  DataLenPtr: PWord;
begin
  if ValuePtr = nil then begin
    SetNull(Field, RecBuf, True);
    SetChanged(Field, RecBuf, True);
    Exit;
  end;


  if Field.ParentField = nil then
    DataBuf := GetDataBuf(RecBuf, Field, DataLenPtr)
  else begin
    GetChildField(Field, RecBuf, DataBuf, DataLen, IsNullValue, NativeBuffer);
    DataLenPtr := @DataLen;
  end;
  PutFieldData(Field, DataBuf, @DataLen, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);

  IsNullValue := False;

  // Set empty string to Null
  if FSetEmptyStrToNull or FRequireEmptyStrToNull then
    case Field.DataType of
      dtString:
        if PByte(DataBuf)^ = 0 then
          IsNullValue := True;
      dtWideString:
        if PWord(DataBuf)^ = 0 then
          IsNullValue := True;
      dtExtString: begin
        DataBuf := PIntPtr(DataBuf)^;
        if (DataBuf = nil) or (PByte(DataBuf)^ = 0) then
          IsNullValue := True;
      end;
      dtExtWideString: begin
        DataBuf := PIntPtr(DataBuf)^;
        if (DataBuf = nil) or (PWord(DataBuf)^ = 0) then
          IsNullValue := True;
      end;
    end;

  SetNull(Field, RecBuf, IsNullValue);
  if not Field.HasParent then
    SetChanged(Field, RecBuf, True)
  else
    SetChanged(Field.ParentField, RecBuf, True);
end;

procedure TMongoRecordset.PutFieldData(Field: TFieldDesc; DataBuf: IntPtr; DataLenPtr: PWord; ValuePtr: IntPtr; ValueLen: Word; NeedConvert: boolean; IsDatabaseValue: boolean = False);
var
  Value: TJSONValue;
  SrcDocument,
  DestDocument: TMongoDocument;
  str: string;
begin
  if Field.FieldNo > 1 then begin
    if (Field.FieldDescKind = fdkData) then begin
      Value := TJSONValue(DataBuf);

      if Value = nil then
        raise Exception.CreateFmt(SFieldNotFound, [Field.Name])
      else if Value.Tag in JSONConstantTags then
        Exit;

      inherited PutFieldData(Field, FPutBuffer, DataLenPtr, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);

      if TMongoFieldDesc(Field).FForceUseString then begin
        if Field.DataType = dtWideString then
          str := string(Marshal.PtrToStringUni(FPutBuffer))
        else if Field.DataType = dtString then
          str := string(Marshal.PtrToStringAnsi(FPutBuffer))
        else
          raise Exception.CreateFmt(SIncompatibleType, [Field.Name]);
        TMongoDataConverters.StringToJson(str, Value, True);
      end
      else
      case Value.Tag of
        jtObjectId: begin
          if Field.DataType = dtWideString then
            str := string(Marshal.PtrToStringUni(FPutBuffer))
          else if Field.DataType = dtString then
            str := string(Marshal.PtrToStringAnsi(FPutBuffer))
          else
            raise Exception.CreateFmt(SIncompatibleType, [Field.Name]);
          TMongoDataConverters.StringToJson(str, Value, False)
        end;
        jtNumber,
        jtString,
        jtDecimal128:
          if Field.DataType = dtWideString then
            TJSONString(Value).AsWideString := {$IFNDEF FPC}string{$ENDIF}(Marshal.PtrToStringUni(FPutBuffer))
          else if Field.DataType = dtString then
            TJSONString(Value).AsAnsiString := {$IFDEF NEXTGEN}string{$ENDIF}(Marshal.PtrToStringAnsi(FPutBuffer))
          else
            raise Exception.CreateFmt(SIncompatibleType, [Field.Name]);
        jtBoolean: TJSONBoolean(Value).Value := boolean(Marshal.ReadByte(FPutBuffer));
        jtInt32: TJSONInt32(Value).Value := Marshal.ReadInt32(FPutBuffer);
        jtInt64: TJSONInt64(Value).Value := Marshal.ReadInt64(FPutBuffer);
        jtDateTime: TJSONDateTime(Value).Value := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FPutBuffer));
        jtDouble: TJSONDouble(Value).Value := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(FPutBuffer));
        jtBytes: begin
          TJSONBytes(Value).Size := Field.Length;
          CopyBuffer(FPutBuffer, TJSONBytes(Value).ValuePtr, Field.Length);
        end;
      end;
    end
    else
      inherited PutFieldData(Field, DataBuf, DataLenPtr, ValuePtr, ValueLen, NeedConvert, IsDatabaseValue);
  end
  else begin // RefreshRecord
    SrcDocument := TMongoDocument(GetGCHandleTarget(PIntPtr(ValuePtr)^));
    DestDocument := TMongoDocument(GetGCHandleTarget(PIntPtr(DataBuf)^));
    try
      DestDocument.Copy(SrcDocument);
    finally
      SrcDocument.Release;
    end;
  end;
end;

procedure TMongoRecordset.GetDataAsVariant(DataBuf: IntPtr; DataLen: Word; DataType, SubDataType: Word; HasParent: boolean; IsFixed: boolean; var Value: variant; UseRollback: boolean);

  function ToAnsiString(const StrValue: string): IntPtr;
  var
    aStr: AnsiString;
  begin
    aStr := AnsiString(Unquote(StrValue));
    DataLen := Word(LengthA(astr));
    Move(PAnsiChar(aStr)^, FPutBuffer[0], DataLen);
    Result := FPutBuffer;
  end;

  function ToWideString(const StrValue: string): IntPtr;
  var
    wStr: WideString;
  begin
    wStr := WideString(Unquote(StrValue));
    DataLen := Word(Length(wStr));
    Move(PWideChar(wStr)^, FPutBuffer[0], DataLen * SizeOf(WideChar));
    Result := FPutBuffer;
  end;

var
  FieldValue: TJSONValue;
  str: string;
begin
  {TODO: handling field #0}
  if not HasParent then
    Exit;

  FieldValue := TJSONValue(DataBuf);

  case SubDataType of
    mongoString,
    mongoNumber,
    mongoBoolean,
    mongoInt32,
    mongoInt64,
    mongoDateTime,
    mongoDouble,
    mongoBytes: begin
      DataBuf := FieldValue.ValuePtr;
      if DataBuf = nil then begin
        TVarData(Value).VType := varEmpty;
        Exit;
      end;
      if SubDataType = mongoString then
        DataLen := DataLen - 2;
    end;
    mongoObjectId:
      if DataType in [dtString, dtWideString] then begin
        str := TJSONObjectId(DataBuf).AsString;
        if FLowercaseObjectId then
          str := AnsiLowerCase(str);
        if DataType = dtString then
          DataBuf := ToAnsiString(str)
        else
          DataBuf := ToWideString(str);
      end
      else
        DataBuf := FieldValue.ValuePtr;
    mongoGeneric: begin
      TMongoDataConverters.JsonToString(FieldValue, str);
      if DataType = dtString then
        DataBuf := ToAnsiString(str)
      else
        DataBuf := ToWideString(str);
    end;
  else
    raise Exception.Create(SCannotGetData);
  end;

  inherited;
end;

procedure TMongoRecordset.PutDataAsVariant(DataBuf: IntPtr; DataLenPtr: PWord; DataType: Word; Len, Scale: Word; HasParent: boolean; const Value: Variant; IsDatabaseValue: boolean);
var
  FieldValue: TJSONValue;
  str: string;
begin
  {TODO: handling field #0}
  if not HasParent then
    Exit;

  FieldValue := TJSONValue(DataBuf);

  case FieldValue.Tag of
    jtNumber,
    jtBoolean,
    jtInt32,
    jtInt64,
    jtDateTime,
    jtDouble,
    jtBytes: DataBuf := FieldValue.ValuePtr;
    jtString,
    jtObject,
    jtArray,
    jtTimestamp,
    jtRegex,
    jtJavaCode,
    jtJavaScopeCode,
    jtBinary,
    jtDBPointer,
    jtObjectId:
      if DataType in [dtString, dtWideString] then
        DataBuf := FPutBuffer
      else
        raise Exception.Create(SCannotPutData);
    jtNull,
    jtMinKey,
    jtMaxKey,
    jtUndefined: Exit;
  else
    raise Exception.Create(SCannotPutData);
  end;

  inherited;

  case FieldValue.Tag of
    jtString: begin
      if DataType = dtString then
        str := string(Marshal.PtrToStringAnsi(DataBuf))
      else if DataType = dtWideString then
        str := string(Marshal.PtrToStringUni(DataBuf))
      else
        raise Exception.Create(SCannotPutData);
      TJSONString(FieldValue).Value := str;
    end;
    jtObject,
    jtArray,
    jtTimestamp,
    jtRegex,
    jtJavaCode,
    jtJavaScopeCode,
    jtBinary,
    jtDBPointer,
    jtObjectId: begin
      if DataType = dtString then
        str := string(Marshal.PtrToStringAnsi(DataBuf))
      else if DataType = dtWideString then
        str := string(Marshal.PtrToStringUni(DataBuf))
      else
        raise Exception.Create(SCannotPutData);
      TMongoDataConverters.StringToJson(str, FieldValue, False);
    end;
  end;
end;

procedure TMongoRecordset.GetFieldAsVariant(Field: TFieldDesc; RecBuf: IntPtr; out Value: variant; UseRollback: boolean = False);
begin
  if Field.FieldNo = 1 then begin
    TVarData(Value).VType := varByRef;
    TVarData(Value).VPointer := Marshal.ReadIntPtr(RecBuf);
  end
  else
    inherited;
end;

procedure TMongoRecordset.InternalInitFieldDescs;
var
  FieldDesc: TCRFieldDesc;
begin
  inherited;

  if Fields.Count > 0 then begin
    FieldDesc := Fields[0] as TCRFieldDesc;
    if (FieldDesc.DataType = dtObject) and (FieldDesc.MapRule <> nil) then begin
      FieldDesc.MapRule := nil;
      FieldDesc.OnDemandConverter := nil;
    end;
  end;
end;

class function TMongoRecordset.IsObjectDataType(DataType: word): boolean;
begin
  Result := DataType in [dtObject];
end;

function TMongoRecordset.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prImplicitAddFields:
      FImplicitAddFields := Value;
    prImplicitChangeType:
      FImplicitChangeType := Value;
    prComplexAsString:
      FComplexAsString := Value;
    prDescribeMethod:
      FDescribeMethod := TDescribeMethod(Value);
    prDescribeAmount:;
    prLowercaseObjectId:
      FLowercaseObjectId := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMongoRecordset.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prImplicitAddFields:
      Value := FImplicitAddFields;
    prImplicitChangeType:
      Value := FImplicitChangeType;
    prComplexAsString:
      Value := FComplexAsString;
    prDescribeMethod:
      Value := Variant(FDescribeMethod);
    prDescribeAmount:
      Value := TMongoConnection(GetConnection).FDescribeAmount;
    prLowercaseObjectId:
      Value := FLowercaseObjectId;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

{ TMongoMetaData }

function TMongoMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TMongoRecordSet.Create;
end;

procedure TMongoMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  List.Clear;
  List.Sorted := False;
  List.Add('MetaDataKinds');
  List.Add('Restrictions');
  List.Add('Tables');
  List.Add('Columns');
  List.Add('Indexes');
  List.Add('IndexColumns');
  List.Sorted := True;
end;

function TMongoMetaData.GetTables(Restrictions: TStrings): TData;
const
  dnCATALOG_NAME  = 1;
  dnSCHEMA_NAME   = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
var
  Connection: TMongoConnection;
  TableName, SQL: string;
  Handle: pbson_t;
  Cursor: TMongoCursor;
  p: pmongoc_cursor_t;
  astr: AnsiString;
  Document: TInternalDocument;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  Connection := TMongoConnection(FRecordSet.GetConnection);

  if TableName <> '' then begin
    SQL := '{"name":"' + TableName + '"}';
    astr := AnsiString(SQL);
    Handle := Connection.FAPI.bson_new_from_json(PAnsiChar(astr), LengthA(astr), nil);
  end
  else
    Handle := nil;

  try
    p := Connection.FAPI.mongoc_database_find_collections(Connection.FDB, Handle, @Connection.FError);
    if p = nil then
      Connection.CheckError;

    Cursor := TMongoCursor.Create(Connection.FAPI, p);
    try
      CreateTablesFields;
      FMemData.Open;
      FMemDataHelper.AllocBuffer;

      repeat
        if not Cursor.More then
          Break;

        Handle := Cursor.Next;
        if Handle <> nil then begin
          Document := TInternalDocument.Create(Connection.FAPI, Handle, Connection.FUseUnicode, False, False);
          try
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[dnCATALOG_NAME] := Connection.Database;
            FMemDataHelper.FieldValues[dnTABLE_NAME] := Document['name'].Value;
            FMemDataHelper.FieldValues[dnTABLE_TYPE] := 1;
            FMemDataHelper.AppendRecord;
          finally
            Document.Release;
          end;
        end
        else
          Break;
      until False;

      FMemData.SetToBegin;
      Result := FMemData;
    finally
      Cursor.Free;
    end;
  finally
    if Handle <> nil then
      Connection.FAPI.bson_destroy(Handle);
  end;
end;

function TMongoMetaData.GetColumns(Restrictions: TStrings): TData;
const
  SQL = '{"find":"%s", "filter":{}}';

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
var
  Field: TMongoFieldDesc;
  Info: TDBTypeInfo;
  TableName, ColumnName: string;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  CreateColumnsFields;
  FMemData.Open;

  FRecordSet.SetSQL(Format(SQL, [LowerCase(MongoSQLInfo.NormalizeName(TableName, False, True))]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    for i := 1 to FRecordSet.Fields.Count - 1 do begin
      Field := TMongoFieldDesc(FRecordSet.Fields[i]);
      FMemDataHelper.AllocBuffer;
      if (ColumnName = '') or (LowerCase(Field.Name) = LowerCase(ColumnName)) then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnCATALOG_NAME] := TMongoConnection(FRecordSet.GetConnection).Database;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnCOLUMN_NAME] := Field.ActualName;
        FMemDataHelper.FieldValues[dnPOSITION] := i;
        Info := DBTypeInfos.FindTypeInfo(Field.DBType);
        if Info <> nil then
          FMemDataHelper.FieldValues[dnDATA_TYPE] := Info.Name
        else if Field.DBType = mongoBytes then
          FMemDataHelper.FieldValues[dnDATA_TYPE] := 'Bytes';
        if Field.Scale <> 0 then begin
          FMemDataHelper.FieldValues[dnPRECISION] := Field.Length;
          FMemDataHelper.FieldValues[dnSCALE] := Field.Scale;
        end
        else if Field.Length <> 0 then
          FMemDataHelper.FieldValues[dnLENGTH] := Field.Length;
        FMemDataHelper.FieldValues[dnNULLABLE] := 1 - integer(Field.IsKey);
        FMemDataHelper.AppendRecord;
      end;
    end;
    FRecordSet.Close;
  end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TMongoMetaData.GetProcedures(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TMongoMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TMongoMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  SQL = '{"find":"system.indexes", "filter":{%s}}';

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  Database, TableName, IndexName, Filter: string;
  Document: TMongoDocument;
  n: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  CreateIndexesFields;
  FMemData.Open;

  if TableName <> '' then
    Filter := '"ns":"' + TMongoConnection(FRecordSet.GetConnection).Database + '.' + LowerCase(MongoSQLInfo.NormalizeName(TableName, False, True)) + '"'
  else
    Filter := '';
  if IndexName <> '' then begin
    if Filter <> '' then
      Filter := Filter + ', ';
    Filter := Filter + '"name":"' + IndexName + '"'
  end;
  FRecordSet.SetSQL(Format(SQL, [Filter]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      FMemDataHelper.AllocBuffer;
      FMemDataHelper.InitRecord;
      Document := TMongoDocument(TVarData(FRecordSetHelper.FieldValues[1]).VPointer);
      Database := TMongoConnection(FRecordSet.GetConnection).Database;
      TableName := Document['ns'].Value;
      n := pos(Database + '.', TableName);
      if n > 0 then
        delete(TableName, 1, n + Length(Database));

      FMemDataHelper.FieldValues[dnTABLE_CATALOG] := Database;
      FMemDataHelper.FieldValues[dnINDEX_CATALOG] := Database;
      FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
      FMemDataHelper.FieldValues[dnINDEX_NAME] := Document['name'].Value;
      FMemDataHelper.FieldValues[dnUNIQUE] := integer(Document['unique'] <> nil);
      FMemDataHelper.AppendRecord;
    end;
    FRecordSet.Close;
    end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TMongoMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  SQL = '{"find":"system.indexes", "filter":{%s}}';

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
  Database, TableName, IndexName, Filter: string;
  Document: TMongoDocument;
  Fields: TJSONObject;
  n: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  CreateIndexColumnsFields;
  FMemData.Open;

  if TableName <> '' then
    Filter := '"ns":"' + TMongoConnection(FRecordSet.GetConnection).Database + '.' + LowerCase(MongoSQLInfo.NormalizeName(TableName, False, True)) + '"'
  else
    Filter := '';
  if IndexName <> '' then begin
    if Filter <> '' then
      Filter := Filter + ', ';
    Filter := Filter + '"name":"' + IndexName + '"'
  end;
  FRecordSet.SetSQL(Format(SQL, [Filter]));
  FRecordSet.Prepare;
  if FRecordSet.GetCommand.CommandType = ctCursor then begin
    FRecordSet.Open;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      FMemDataHelper.AllocBuffer;
      Document := TMongoDocument(TVarData(FRecordSetHelper.FieldValues[1]).VPointer);
      Database := TMongoConnection(FRecordSet.GetConnection).Database;
      TableName := Document['ns'].Value;
      n := pos(Database + '.', TableName);
      if n > 0 then
        delete(TableName, 1, n + Length(Database));

      Fields := Document['key'].GetData as TJSONObject;
      if (Fields.Pairs.Count > 0) and SameText(Fields.Pairs[0].Value.AsString, 'text') then
        Fields := Document['weights'].GetData as TJSONObject;

      for n := 0 to Fields.Pairs.Count - 1 do begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_CATALOG] := Database;
        FMemDataHelper.FieldValues[dnINDEX_CATALOG] := Database;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnINDEX_NAME] := Document['name'].Value;
        FMemDataHelper.FieldValues[dnCOLUMN_NAME] := Fields.Pairs[n].Name.AsString;
        if Fields.Pairs[n].Value.AsString = '1' then
          FMemDataHelper.FieldValues[dnSORT_ORDER] := 1
        else
          FMemDataHelper.FieldValues[dnSORT_ORDER] := 0;
        FMemDataHelper.FieldValues[dnPOSITION] := n + 1;
        FMemDataHelper.AppendRecord;
      end;
    end;
    FRecordSet.Close;
    end;
  FRecordSet.UnPrepare;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TMongoMetaData.GetConstraints(Restrictions: TStrings): TData;
begin
  raise Exception.Create(SUnsupportedMetaDataKind);
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
end;

function TMongoMetaData.GetDatabases(Restrictions: TStrings): TData;
const
  dnCATALOG_NAME = 1;
var
  Connection: TMongoConnection;
  Cursor: TMongoCursor;
  p: pmongoc_cursor_t;
  Handle: pbson_t;
  Document: TInternalDocument;
begin
  Connection := FRecordSet.GetConnection as TMongoConnection;
  p := Connection.FAPI.mongoc_client_find_databases(Connection.FClient, @Connection.FError);
  if p = nil then
    Connection.CheckError;

  Cursor := TMongoCursor.Create(Connection.FAPI, p);
  try
    CreateDatabasesFields;
    FMemData.Open;
    FMemDataHelper.AllocBuffer;

    repeat
      if not Cursor.More then
        Break;

      Handle := Cursor.Next;
      if Handle <> nil then begin
        Document := TInternalDocument.Create(Connection.FAPI, Handle, Connection.FUseUnicode, False, False);
        try
          FMemDataHelper.InitRecord;
          FMemDataHelper.FieldValues[dnCATALOG_NAME] := Document['name'].Value;
          FMemDataHelper.AppendRecord;
        finally
          Document.Free;
        end;
      end
      else
        Break;
    until False;

    FMemData.SetToBegin;
    Result := FMemData;
  finally
    Cursor.Free;
  end;
end;

{$IFNDEF NOSQL}

{ TMongoFieldAccessor }

class function TMongoFieldAccessor.AsInteger(const Field, Buffer: IntPtr): Int64;
var
  DBObject: IntPtr;
  DataBuf: IntPtr;
  DataLen: Word;
  Blank, Native: boolean;
begin
  Result := 0;

  DBObject := Marshal.ReadIntPtr(Buffer, 0);
  if DBObject <> nil then begin
    TInternalDocument(GetGCHandleTarget(DBObject)).GetAttributeValue(TMongoFieldDesc(Field).FullName, DataBuf, DataLen, Blank, Native);
    if not Blank then
      if TJSONValue(DataBuf).Tag = jtInt32 then
        Result := Int64(TJSONInt32(DataBuf).Value)
      else if TJSONValue(DataBuf).Tag = jtInt32 then
        Result := TJSONInt64(DataBuf).Value
      else if (TJSONValue(DataBuf).Tag = jtNumber) and (TJSONNumber(DataBuf).Subtype in [jtInt32, jtInt64]) then
        Result := StrToInt(TJSONNumber(DataBuf).Value);
  end;
end;

class function TMongoFieldAccessor.AsFloat(const Field, Buffer: IntPtr): double;
var
  DBObject: IntPtr;
  DataBuf: IntPtr;
  DataLen: Word;
  Blank, Native: boolean;
begin
  Result := 0;

  DBObject := Marshal.ReadIntPtr(Buffer, 0);
  if DBObject <> nil then begin
    TInternalDocument(GetGCHandleTarget(DBObject)).GetAttributeValue(TMongoFieldDesc(Field).FullName, DataBuf, DataLen, Blank, Native);
    if not Blank then
      if TJSONValue(DataBuf).Tag = jtDouble then
        Result := TJSONDouble(DataBuf).Value
      else if (TJSONValue(DataBuf).Tag = jtNumber) and (TJSONNumber(DataBuf).Subtype = jtDouble) then
        Result := StrToFloat(TJSONNumber(DataBuf).Value);
  end;
end;

class function TMongoFieldAccessor.AsAnsiString(const Field, Buffer: IntPtr): AnsiString;
var
  DBObject: IntPtr;
  DataBuf: IntPtr;
  DataLen: Word;
  Blank, Native: boolean;
begin
  Result := '';

  DBObject := Marshal.ReadIntPtr(Buffer, 0);
  if DBObject <> nil then begin
    TInternalDocument(GetGCHandleTarget(DBObject)).GetAttributeValue(TMongoFieldDesc(Field).FullName, DataBuf, DataLen, Blank, Native);
    if not Blank then
      if TJSONValue(DataBuf).Tag = jtString then
        Result := TJSONString(DataBuf).AsAnsiString
      else
        Result := AnsiString(TJSONValue(DataBuf).AsString);
  end;
end;

class function TMongoFieldAccessor.AsWideString(const Field, Buffer: IntPtr): WideString;
var
  DBObject: IntPtr;
  DataBuf: IntPtr;
  DataLen: Word;
  Blank, Native: boolean;
begin
  Result := '';

  DBObject := Marshal.ReadIntPtr(Buffer, 0);
  if DBObject <> nil then begin
    TInternalDocument(GetGCHandleTarget(DBObject)).GetAttributeValue(TMongoFieldDesc(Field).FullName, DataBuf, DataLen, Blank, Native);
    if not Blank then
      if TJSONValue(DataBuf).Tag = jtString then
        Result := TJSONString(DataBuf).AsWideString
      else
        Result := WideString(TJSONValue(DataBuf).AsString);
  end;
end;

{ TMongoVirtualData }

constructor TMongoVirtualData.Create(const Connection: TMongoVirtualConnection; const TableName: string);
begin
  FConnection := Connection;
  FTableName := TableName;
  FAutoClose := True;

  FMemData := TMongoRecordset.Create;

  inherited Create(FMemData);

  TMongoRecordset(FMemData).SetConnection(Connection.FInternalConnection);

  TMongoRecordset(FMemData).SetProp(prImplicitAddFields, Connection.FImplicitAddFields);
  TMongoRecordset(FMemData).SetProp(prImplicitChangeType, Connection.FImplicitChangeType);
  TMongoRecordset(FMemData).SetProp(prComplexAsString, Connection.FComplexAsString);
  TMongoRecordset(FMemData).SetProp(prLowercaseObjectId, Connection.FLowercaseObjectId);

  TMongoRecordset(FMemData).SetSQL('{"find":"' + TableName + '","filter":{}}');
end;

destructor TMongoVirtualData.Destroy;
begin
  FMemData.Free;

  inherited;
end;

//function TMongoVirtualData.GetFilterString(const Filter: PVirtualConstraint): string;
//var
//  Builder: StringBuilder;
//  i: integer;
//  Constraint: TVirtualConstraintItem;
//  Field: TVirtualFieldDesc;
//begin
//  Result := '';
//  if Length(Filter^.Items) = 0 then
//    Exit;
//
//  Builder := StringBuilder.Create(1024);
//  try
//    for i := 0 to Length(Filter^.Items) - 1 do begin
//      Constraint := Filter^.Items[i];
//      Field := Fields[Constraint.FieldIndex];
//
//      if i > 0 then
//        Builder.Append(',');
//
//      Builder.Append(Field.Name);
//      Builder.Append(':{');
//
//      case Constraint.Operation of
//        ntEqual: Builder.Append('$eq:');
//        ntMore: Builder.Append('$gt:');
//        ntMoreEqual: Builder.Append('$gte:');
//        ntLess: Builder.Append('$lt:');
//        ntLessEqual: Builder.Append('$lte:');
//        ntNoEqual: Builder.Append('$eq:');
//      end;
//
//      case Constraint.Value.ValueType of
//        vrNull: Builder.Append('null');
//        vrInteger: Builder.Append(IntToStr(Constraint.Value.IntValue));
//        vrFloat: Builder.Append(FloatToStr(Constraint.Value.FloatValue));
//        vrString,
//        vrAnsiString,
//        vrWideString: begin
//          case Field.DataType of
//            dtTime,
//            dtDate,
//            dtDateTime: begin
//              Builder.Append('{"$date":"');
//              Builder.Append(string(Constraint.Value.Value));
//              Builder.Append('"}');
//            end;
//          else
//            Builder.Append('"');
//            Builder.Append(string(Constraint.Value.Value));
//            Builder.Append('"');
//          end;
//        end;
//      end;
//
//      Builder.Append('}');
//    end;
//
//    Result := Builder.ToString;
//  finally
//    Builder.Free;
//  end;
//end;

class function TMongoVirtualData.GetLocalIndexClass: TVirtualLocalIndexClass;
begin
  Result := TVirtualMongoIndex;
end;

function TMongoVirtualData.GetFieldAccessorClass: TFieldAccessorClass;
begin
  Result := TMongoFieldAccessor;
end;

function TMongoVirtualData.InternalOpenNoIndex(const Filter: PVirtualConstraint): TVirtualBookmark;
//var
//  ProjectionString: string;
//  MemDataHelper: TDataHelper;
//  Document, Projection: TInternalDocument;
//
//  function CreateDataProjection(const DocumentData, Parent: TJSONValue): TJSONValue;
//  var
//    i: integer;
//  begin
//    case DocumentData.Tag of
//      jtObject: begin
//        Result := TJSONObject.Create(Parent);
//        for i := 0 to TJSONObject(DocumentData).Pairs.Count - 1 do
//          TJSONObject(Result).Pairs.Add(CreateDataProjection(TJSONObject(DocumentData).Pairs[i], Result));
//      end;
//      jtPair: begin
//        Result := TJSONPair.Create(Parent);
//        TJSONPair(Result).Name := TJSONWideString.Create(Result);
//        TJSONString(TJSONPair(Result).Name).Value := TJSONString(TJSONPair(DocumentData).Name).AsString;
//        TJSONPair(Result).Value := CreateDataProjection(TJSONPair(DocumentData).Value, Result);
//      end;
//      jtArray: begin
//        Result := TJSONArray.Create(Parent);
//        for i := 0 to TJSONArray(DocumentData).Elements.Count - 1 do
//          TJSONArray(Result).Elements.Add(CreateDataProjection(TJSONArray(DocumentData).Elements[i], Result));
//      end;
//    else
//      Result := TJSONInt32.Create(Parent);
//      TJSONInt32(Result).Value := 1;
//    end;
//  end;
//
begin
//  if Length(Filter^.Items) > 0 then begin
//    ProjectionString := '';
//    if TMongoRecordset(FMemData).Active then begin
//      MemDataHelper := TDataHelper.Create(FMemData);
//      try
//        if TMongoRecordset(FMemData).RecordCount > 0 then begin
//          MemDataHelper.AllocBuffer;
//          MemDataHelper.NextRecord;
//          Document := TInternalDocument(GetGCHandleTarget(TVarData(MemDataHelper.FieldValues[1]).VPointer));
//          Projection := TInternalDocument.Create;
//          try
//            Projection.SetData(CreateDataProjection(Document.Data, nil));
//            FConnection.FInternalConnection.FAPI.Serializer.ToText(Projection.Data, ProjectionString);
//          finally
//            Projection.Free;
//          end;
//        end;
//      finally
//        MemDataHelper.Free;
//      end;
//      TMongoRecordset(FMemData).Close;
//    end;
//    TMongoRecordset(FMemData).SetSQL('{"find":"' + FTableName + '","filter":{' + GetFilterString(Filter) + '}}');
//    TMongoCommand(TMongoRecordset(FMemData).GetCommand).SetProjection(ProjectionString);
//    TMongoRecordset(FMemData).Open;
//  end;

  Result := inherited InternalOpenNoIndex(Filter);
end;

procedure TMongoVirtualData.InternalAllocBuffer;
begin
  inherited;

  FMemData.InitRecord(FRecordBuffer);
end;

procedure TMongoVirtualData.InternalDescribeFields(const SpecificTypes: TSpecificTypes);
var
  FieldCount, i, j, n: integer;
  FieldDesc: TMongoFieldDesc;
begin
  n := 0;
  FieldCount := FMemData.Fields.Count;
  if FieldCount > MaxFieldCount then
    FieldCount := MaxFieldCount;
  for i := 0 to FieldCount - 1 do
    if not (FMemData.Fields[i].DataType in [dtObject, dtArray]) then
      Inc(n);

  SetLength(FFields, n);

  j := 0;
  for i := 0 to FieldCount - 1 do begin
    FieldDesc := FMemData.Fields[i] as TMongoFieldDesc;
    if not (FieldDesc.DataType in [dtObject, dtArray]) then begin
      if not FConnection.FUseMaskedFields then
        FFields[j].Name := FieldDesc.FullName
      else
        FFields[j].Name := FieldDesc.MaskedName;
      FFields[j].DataType := FieldDesc.DataType;
      FFields[j].Length := FieldDesc.Length;
      FFields[j].Scale := FieldDesc.Scale;
      FFields[j].IsKey := FieldDesc.IsKey;
      FFields[j].IsAutoIncrement := FieldDesc.IsAutoIncrement;
      FFields[j].Required := FieldDesc.Required;
      FFields[j].ReadOnly := FieldDesc.ReadOnly;
      FFields[j].ActualIndex := i;
      Inc(j);
    end;
  end;
end;

procedure TMongoVirtualData.Prepare;
begin
  if not FMemData.Active then
    FMemData.Open;

  inherited;
end;

procedure TMongoVirtualData.InsertRecord(const Values: TVirtualValues);
begin
  if not FBufferAllocated then
    InternalAllocBuffer;
  FMemData.CreateComplexFields(FRecordBuffer, True);

  inherited;
end;

function TMongoVirtualData.GetLocalIndex(const Constraint: PVirtualConstraint): integer;
begin
  Result := -1;
end;

function TMongoVirtualData.InTransaction: boolean;
begin
  Result := False;
end;

procedure TMongoVirtualData.StartTransaction;
begin
end;

procedure TMongoVirtualData.Commit;
begin
end;

procedure TMongoVirtualData.Rollback;
begin
end;

{ TMongoVirtualConnection }

constructor TMongoVirtualConnection.Create;
begin
  inherited;

//  SetProp(prTableCreationMode, cmOnDemand);
  FInternalConnection := TMongoConnection.Create;
  FInternalMetaData := TMongoMetaData.Create;

  FDatabase := '';
  FClientLibrary := '';
  FBSONLibrary := '';
  FImplicitAddFields := True;
  FImplicitChangeType := True;
  FComplexAsString := False;
  FAdditionalServers := '';
  FConnectionOptions := '';
end;

destructor TMongoVirtualConnection.Destroy;
begin
  inherited;

  FInternalMetaData.Free;
  FInternalConnection.Free;
end;

procedure TMongoVirtualConnection.InternalRegisterTables;
var
  Restrictions: TStrings;
  MetaData: TMemData;
  MetaDataHelper: TDataHelper;
  TableName: Variant;
begin
  Restrictions := TStringList.Create;
  try
    MetaData := FInternalMetaData.GetMetaData(FInternalConnection, nil, 'tables', Restrictions) as TMemData;
    MetaDataHelper := TDataHelper.Create(MetaData);
    try
      MetaDataHelper.AllocBuffer;
      MetaDataHelper.NextRecord;
      while not MetaData.Eof do begin
        TableName := MetaDataHelper.FieldValues[3];
        RegisterVirtualData(FInternalConnection.Database, TableName, TMongoVirtualData.Create(Self, TableName));
        MetaDataHelper.NextRecord;
      end;
    finally
      MetaDataHelper.Free;
    end;
  finally
    Restrictions.Free;
  end;
end;

class function TMongoVirtualConnection.GetVirtualTableClass: TVirtualTableClass;
begin
  Result := TMongoVirtualTable;
end;

function TMongoVirtualConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TMongoVirtualCommand;
end;

class function TMongoVirtualConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMongoVirtualCommand.GetMapRulesClass;
end;

function TMongoVirtualConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prPort:
      FPort := Value;
    prDatabase:
      FDatabase := Value;
    prClientLibrary:
      FClientLibrary := Value;
    prBSONLibrary:
      FBSONLibrary := Value;
    prAdditionalServers:
      FAdditionalServers := Value;
    prConnectionOptions:
      FConnectionOptions := Value;
    prLowercaseObjectId:
      FLowerCaseObjectId := Value;
    prUseMaskedFields:
      FUseMaskedFields := Value;
    prDescribeAmount:
      FInternalConnection.SetProp(prDescribeAmount, Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMongoVirtualConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prPort:
      Value := FPort;
    prDatabase:
      Value := FDatabase;
    prClientLibrary:
      Value := FClientLibrary;
    prBSONLibrary:
      Value := FBSONLibrary;
    prAdditionalServers:
      Value := FAdditionalServers;
    prConnectionOptions:
      Value := FConnectionOptions;
    prLowercaseObjectId:
      Value := FLowerCaseObjectId;
    prUseMaskedFields:
      Value := FUseMaskedFields;
    prDescribeAmount:
      Value := FInternalConnection.FDescribeAmount;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TMongoVirtualConnection.Connect(const ConnectString: string);
var
  Command: TCRCommand;
begin
  if FInternalConnection <> nil then begin
    FInternalConnection.SetProp(prServer, FServer);
    FInternalConnection.SetProp(prPort, FPort);
    FInternalConnection.SetProp(prDatabase, FDatabase);
    FInternalConnection.SetProp(prUsername, FUsername);
    FInternalConnection.SetProp(prPassword, FPassword);

    FInternalConnection.SetProp(prAdditionalServers, FAdditionalServers);
    FInternalConnection.SetProp(prConnectionOptions, FConnectionOptions);
    FInternalConnection.SetProp(prClientLibrary, FClientLibrary);
    FInternalConnection.SetProp(prBSONLibrary, FBSONLibrary);
    FInternalConnection.SetProp(prLowercaseObjectId, FLowerCaseObjectId);
    FInternalConnection.Connect('');
  end;

  inherited;

  if FInternalConnection <> nil then begin
    InternalRegisterTables;
    Command := GetCommand;
    try
      TMongoVirtualCommand(Command).InternalCreateTables;
    finally
      ReleaseCommand(Command);
    end;
  end;
end;

procedure TMongoVirtualConnection.Disconnect;
begin
  inherited;

  if FInternalConnection <> nil then
    FInternalConnection.Disconnect;
end;

function TMongoVirtualConnection.GetServerVersion: string;
begin
  Result := FInternalConnection.GetServerVersion;
end;

function TMongoVirtualConnection.GetServerVersionFull: string;
begin
  Result := FInternalConnection.GetServerVersionFull;
end;

function TMongoVirtualConnection.GetClientVersion: string;
begin
  Result := FInternalConnection.GetClientVersion;
end;

{ TMongoVirtualCommand }

procedure TMongoVirtualCommand.InternalCreateTablesInfo;
var
  i: integer;
  ObjectInfo: TSQLObjectInfo;
  Conn: TMongoVirtualConnection;
begin
  inherited;

  Conn := TMongoVirtualConnection(GetConnection);
  for i := 0 to FTablesInfo.Count - 1 do begin
    SQLInfo.SplitObjectName(FTablesInfo[i].TableNameFull, ObjectInfo);
    if ObjectInfo.Catalog = '' then
      FTablesInfo[i].TableNameFull := Conn.FInternalConnection.Database + '.' + FTablesInfo[i].TableNameFull;
  end;
end;

procedure TMongoVirtualCommand.SetConnection(Value: TCRConnection);
begin
  if (Value is TMongoConnector) and (TMongoConnector(Value).FInternalConnection is TMongoVirtualConnection) then
    inherited SetConnection(TMongoConnector(Value).FInternalConnection)
  else
    inherited;
end;

{ TMongoVirtualRecordset }

function TMongoVirtualRecordset.GetCreateSQL(const TableName: string; const Members: TTableMembers): string;
var
  Builder: StringBuilder;
  ColumnInfo: TColumnInfo;
  DBType, DataType: Word;
  Len, Scale, i: integer;
begin
  Builder := StringBuilder.Create(1024);
  try
    Builder.Append('{insert: "');
    Builder.Append(TableName);
    Builder.Append('",documents:[{"_id":{"$oid":"000000000000000000000000"}');

    for i := 0 to High(Members.Columns) do begin
      ColumnInfo := Members.Columns[i];

      Builder.Append(',"');
      Builder.Append(ColumnInfo.Name);
      Builder.Append('":');

      DBType := GetDBType(UpperCase(ColumnInfo.DataType), Len, Scale);
      DataType := GetDataType(DBType);
      case DataType of
        dtUnknown,
        dtString,
        dtExtString,
        dtWideString,
        dtExtWideString,
        dtFixedChar,
        dtFixedWideChar: begin
          Builder.Append('"');
          Builder.Append(StringOfChar('.', ColumnInfo.Length));
          Builder.Append('"');
        end;
        dtInt8,
        dtInt16,
        dtInt32,
        dtUInt8,
        dtUInt16,
        dtUInt32:
          Builder.Append('1111111111');
        dtInt64,
        dtUInt64:
          Builder.Append('{"$numberLong":"1111111111"}');
        dtSingle,
        dtFloat,
        dtExtended,
        dtCurrency,
        dtBCD,
        dtFMTBCD:
          Builder.Append('111111111.1');
        dtDate,
        dtTime,
        dtDateTime:
          Builder.Append('{"$date":"1900-01-01T01:01:01.000"}');
        dtSQLTimeStamp,
        dtSQLTimeStampOffset:
          Builder.Append('{"$timestamp":{"t":1,"i":1}}');
        dtBoolean:
          Builder.Append('true');
        dtBytes,
        dtVarBytes,
        dtExtVarBytes,
        dtBlob,
        dtMemo,
        dtWideMemo:
          Builder.Append('{"$binary":"","$type":"0"}');
      end;
    end;

    Builder.Append('}]}');

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure TMongoVirtualRecordset.CreateCommand;
begin
  SetCommand(TMongoVirtualCommand.Create);
end;

procedure TMongoVirtualRecordset.InternalPrepare;
var
  Command: TMongoVirtualCommand;
begin
  if FCommand.ParsedSQLType in [qtCreate, qtDrop] then begin
    Command := TMongoVirtualCommand(GetCommand);
    Command.SetCursorState(csPrepared);
    Command.CommandType := ctStatement;
  end
  else
    inherited;
end;

procedure TMongoVirtualRecordset.ExecCommand(Iters: integer = 1; Offset: integer = 0);
var
  TableName: string;
  Members: TTableMembers;
  Connection: TMongoVirtualConnection;
  Command: TMongoVirtualCommand;
  SQL: string;
begin
  Command := TMongoVirtualCommand(GetCommand);

  if Command.ParsedSQLType = qtCreate then begin
    Connection := TMongoVirtualConnection(GetConnection);
    TableName := '';
    TSQLiteMetaData.ParseTableSQL(Command.SQL, TableName, Members);

    SQL := GetCreateSQL(TableName, Members);
    Connection.FInternalConnection.ExecuteJSON(Connection.FInternalConnection.Database, SQL);
    Connection.RegisterVirtualData(Connection.FInternalConnection.Database, TableName, TMongoVirtualData.Create(Connection, TableName));
    try
      Connection.DoCreateTable(Connection.FInternalConnection.Database, TableName);

      Command.SetCursorState(csPrepared);
      Command.CommandType := ctStatement;
    except
      on E: Exception do begin
        InternalDropTables;
        raise;
      end;
    end;
  end
  else if Command.ParsedSQLType = qtDrop then begin
    Connection := TMongoVirtualConnection(GetConnection);
    TableName := '';
    TSQLiteMetaData.ParseTableSQL(Command.SQL, TableName, Members);

    Connection.FInternalConnection.DropCollection(TableName);
    Connection.DoDropTable(Connection.FInternalConnection.Database, TableName);

    Command.SetCursorState(csPrepared);
    Command.CommandType := ctStatement;
  end
  else
    inherited;
end;

{$ENDIF}

{ TMongoConnector }

constructor TMongoConnector.Create;
begin
  inherited;

  FInternalConnection := nil;

  FAdditionalServers := '';
  FPort := MnDefValPort;
  FServer := MnDefValServer;
  FClientLibrary := '';
  FBSONLibrary := '';
  FDatabase := '';
  FConnectionOptions := '';
  FDescribeAmount := 25;
end;

destructor TMongoConnector.Destroy;
begin
  inherited;

  FInternalConnection.Free;
end;

function TMongoConnector.GetTransactionClass: TCRTransactionClass;
begin
  Result := TMongoTransaction;
end;

function TMongoConnector.GetRecordSetClass: TCRRecordSetClass;
begin
  if FSQLEngine then
  {$IFNDEF NOSQL}
    Result := TMongoVirtualRecordset
  {$ELSE}
    raise Exception.Create(SSQLEngineNotSupported)
  {$ENDIF}
  else
    Result := TMongoRecordset;
end;

function TMongoConnector.GetCommandClass: TCRCommandClass;
begin
  if FSQLEngine then
  {$IFNDEF NOSQL}
    Result := TMongoVirtualCommand
  {$ELSE}
    raise Exception.Create(SSQLEngineNotSupported)
  {$ENDIF}
  else
    Result := TMongoCommand;
end;

{$IFNDEF LITE}

function TMongoConnector.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TMongoMetadata;
end;

{$ENDIF}

procedure TMongoConnector.SetSQLEngine(const Value: boolean);
begin
  if Value <> FSQLEngine then begin
    Disconnect;
    FreeAndNil(FInternalConnection);
    FSQLEngine := Value;
  end;
end;

function TMongoConnector.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prPort:
      Value := FPort;
    prDatabase:
      Value := FDatabase;
    prUseUnicode:
      Value := FUseUnicode;
    prClientLibrary:
      Value := FClientLibrary;
    prBSONLibrary:
      Value := FBSONLibrary;
    prAdditionalServers:
      Value := FAdditionalServers;
    prConnectionOptions:
      Value := FConnectionOptions;
    prSQLEngine:
      Value := FSQLEngine;
    prLowercaseObjectId:
      Value := FLowerCaseObjectId;
    prDescribeAmount:
      Value := FDescribeAmount;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMongoConnector.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prPort:
      FPort := Value;
    prDatabase:
      FDatabase := Value;
    prUseUnicode:
      FUseUnicode := Value;
    prClientLibrary:
      FClientLibrary := Value;
    prBSONLibrary:
      FBSONLibrary := Value;
    prAdditionalServers:
      FAdditionalServers := Value;
    prConnectionOptions:
      FConnectionOptions := Value;
    prSQLEngine:
      SetSQLEngine(Value);
    prLowercaseObjectId:
      FLowerCaseObjectId := Value;
    prDescribeAmount: begin
      FDescribeAmount := Value;
      if FDescribeAmount <= 0 then
        FDescribeAmount := 25;
    end
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TMongoConnector.Connect(const ConnectString: string);
begin
{$IFDEF NOSQL}
  if FSQLEngine then
    raise Exception.Create(SSQLEngineNotSupported);
{$ENDIF}

  if FInternalConnection = nil then
  {$IFNDEF NOSQL}
    if FSQLEngine then
      FInternalConnection := TMongoVirtualConnection.Create
    else
  {$ENDIF}
      FInternalConnection := TMongoConnection.Create;
  FInternalConnection.SetProp(prServer, FServer);
  FInternalConnection.SetProp(prPort, FPort);
  FInternalConnection.SetProp(prDatabase, FDatabase);
  FInternalConnection.SetProp(prUsername, FUsername);
  FInternalConnection.SetProp(prPassword, FPassword);

  FInternalConnection.SetProp(prAdditionalServers, FAdditionalServers);
  FInternalConnection.SetProp(prConnectionOptions, FConnectionOptions);
  FInternalConnection.SetProp(prClientLibrary, FClientLibrary);
  FInternalConnection.SetProp(prBSONLibrary, FBSONLibrary);
  FInternalConnection.SetProp(prLowercaseObjectId, FLowerCaseObjectId);
  FInternalConnection.SetProp(prDescribeAmount, FDescribeAmount);

  if not FInternalConnection.GetConnected then
    FInternalConnection.Connect('');

  inherited;

  FConnected := FInternalConnection.GetConnected;
end;

procedure TMongoConnector.Disconnect;
begin
  if FInternalConnection <> nil then
    FInternalConnection.Disconnect;

  FConnected := False;
end;

function TMongoConnector.GetServerVersion: string;
begin
  Result := '';

  if FInternalConnection <> nil then begin
  {$IFNDEF NOSQL}
    if FSQLEngine then
      Result := TMongoVirtualConnection(FInternalConnection).GetServerVersion
    else
  {$ENDIF}
      Result := TMongoConnection(FInternalConnection).GetServerVersion;
  end;
end;

function TMongoConnector.GetServerVersionFull: string;
begin
  Result := '';

  if FInternalConnection <> nil then begin
  {$IFNDEF NOSQL}
    if FSQLEngine then
      Result := TMongoVirtualConnection(FInternalConnection).GetServerVersionFull
    else
  {$ENDIF}
      Result := TMongoConnection(FInternalConnection).GetServerVersionFull;
  end;
end;

function TMongoConnector.GetClientVersion: string;
begin
  Result := '';

  if FInternalConnection <> nil then begin
  {$IFNDEF NOSQL}
    if FSQLEngine then
      Result := TMongoVirtualConnection(FInternalConnection).GetClientVersion
    else
  {$ENDIF}
      Result := TMongoConnection(FInternalConnection).GetClientVersion;
  end;
end;

initialization
  MongoSQLInfo := TMongoSQLInfo.Create(nil);
{$IFNDEF VER24P}
  NetEncoding := TNetEncoding.Create;
{$ENDIF}

finalization
  MongoSQLInfo.Free;
{$IFNDEF VER24P}
  NetEncoding.Free;
{$ENDIF}

end.
