
//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I NexusDac.inc}
unit NexusClassesUni;

interface

{$IFNDEF DUMMY}
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Types, Classes, DB, SysUtils, Variants, SyncObjs, FMTBcd, TypInfo, Contnrs,
{$IFDEF VER12P}
  AnsiStrings,
{$ENDIF}
  nxsqlProxies, nxllTypes, nxsdTypes, nxllComponent,
  nxsrSqlEngineBase, nxsqlEngine, nxsdServerEngine, nxseAllEngines,
{$IFNDEF NEXUS_EMBEDDED}
  nxreRemoteServerEngine, nxtwWinsockTransport, nxtsBaseSecuredTransport, nxtnNamedPipeTransport, nxptBasePooledTransport,  nxtsBlowfishRC4SecuredTransport,
{$ENDIF}
  nxsrServerEngine, nxllBde, nxdbBase, nxsdDataDictionary, nxllStreams,
  CLRClasses, CRParser, CRTypes, MemData, CRAccess, CRDataTypeMap, DASQLGenerator,
{$IFNDEF UNIDACPRO}
  NexusError, NexusParser;
{$ELSE}
  NexusErrorUni, NexusParserUni;
{$ENDIF}
{$ENDIF} // DUMMY

{$IFNDEF DUMMY}
type
  TNexusProtocol = (npTCP, npSTCP, npPipe, npSPipe);

const
  CURRENT_TIME = 'CURRENT_TIME';
  EMPTY_STRING = 'EMPTY_STRING';

  dtAutoIncrement = 101;
  dtShortStirng = 102;
  dtSingleChar = 103;
  dtSingleWideChar = 104;
  dtGraphicBlob = 105;
  dtRecRev = 106;

  csNexusLoaderBlockSize = 4096;

  NexusDefValProtocol = npTCP;

type
  TNexusConnection = class;
  TNexusCommand = class;

  TnxEngine = class(TnxStateComponent)
  protected
    FOwner: TNexusConnection;
  {$IFNDEF NEXUS_EMBEDDED}
    FTransport: TnxBasePooledTransport;
    FSecureTransport: TnxBlowfishRC4SecuredTransport;
  {$ENDIF}
    FSqlEngine: TnxBaseSqlEngine;
    FServerEngine: TnxBaseServerEngine;
    FSession: TnxAbstractSession;
    FDataBase: TnxAbstractDatabase;
    FTransContext: TnxAbstractTransContext;
    FCommands: TList;

    procedure ClearRefs;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure nxcNotification(aSender: TnxComponent;
      aOperation: TnxNotifyOp; aParam: Pointer); override;
  public
    constructor Create(Owner: TNexusConnection); reintroduce;
    destructor Destroy; override;
    procedure Connect(const Server, Database, Username, Password: string; Port: integer;
      ConnectionTimeout, HeartbeatInterval, LostConnectionTimeout, WatchdogInterval, CommandTimeout: integer;
      ReadOnly: boolean; Protocol: TNexusProtocol; const SecretKey: AnsiString);
    procedure Disconnect;

    procedure RegisterClient(Command: TNexusCommand);
    procedure UnRegisterClient(Command: TNexusCommand);
    property Session: TnxAbstractSession read FSession;
  end;

  TNexusConnection = class(TCRConnection)
  private
    FDatabase: string;
    FPort: integer;
    FConnectionTimeout: integer;
    FHeartbeatInterval: integer;
    FLostConnectionTimeout: integer;
    FWatchdogInterval: integer;
    FCommandTimeout: integer;
    FReadOnly: boolean;
    FnxEngine: TnxEngine;
    FDetectFieldsOnPrepare: boolean;
    FProtocol: TNexusProtocol;
    FSecretKey: AnsiString;

    function GetCommandTimeoutMSec: integer;
  protected
    procedure ChekTransContext(Component: TObject);
    procedure ChekDataBase(Component: TObject = nil);
    function nxTransContext: TnxAbstractTransContext;
    function nxDataBase: TnxAbstractDatabase;
    procedure Check(ErrorCode: cardinal; Component: TObject = nil);
    procedure ProcessError(ErrorCode: cardinal; Component: TObject; Stream: TStream = nil; const Msg: string = '');

    property EnableBCD;
    property EnableFMTBCD;

    property CommandTimeoutMSec: integer read GetCommandTimeoutMSec;
    property nxEngine: TnxEngine read FnxEngine;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;
    function CheckIsValid: boolean; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;

    function CanChangeDatabase: boolean; override;
  end;

  TNexusTransaction = class(TCRTransaction)
  private
    FSavepointList: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetInTransaction: boolean; override;

    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;

    procedure Savepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

  TNexusParamDesc = class(TParamDesc)
  private
  protected
    procedure GetAnsiString(Buffer: pointer);
    procedure GetWideString(Buffer: pointer);
    procedure GetBytes(Buffer: pointer);
  public
    procedure GetData(Buffer: pointer);
  end;

  TNexusIndexDef = class
  private
    FName: string;
    FFields: string;
  public
    property Name: string read FName write FName;
    property Fields: string read FFields write FFields;
  end;

  TNexusIndexDefs = class(TCRObjectList)
  private
    FUpdated: Boolean;

    function GetItem(Index: integer): TNexusIndexDef;
  public
    function FindIndexForFields(const Fields: string): TNexusIndexDef;

    property Items[Index: Integer]: TNexusIndexDef read GetItem; default;
    property Updated: Boolean read FUpdated write FUpdated;
  end;

  TNexusCommand = class(TCRCommand)
  private
    FConnection: TNexusConnection;
    FCommandTimeout: integer;
    FReadOnly: boolean;
    FRowsAffected: integer;
    FCursorState: TCursorState;
    FRecordSetExec: boolean;
    FnxEngine: TnxEngine;
    FPreparedStatement: TnxAbstractStatement;
    FCursor: TnxAbstractCursor;
    FFieldsDescriptor: TnxFieldsDescriptor;
    FTableDescriptor: TnxBaseTableDescriptor;
    FBindedParams: TnxSqlParamList;
    FIsStoredProc: boolean;
    FIndexDefs: TNexusIndexDefs;

    procedure HandleError(Result: TnxResult; const Msg: string; Stream: TStream);
    procedure BindParams(Offset: Integer; out NeedsGet: Boolean);
    procedure ReadOutParams;
    procedure InitProcParams(const Name: string);
    function GetStatementType: TnxStatementType;
    procedure FreeCursor;
    procedure UpdateIndexDefs;
    function GetDefaultIndexName: string;
  protected
    function GetNexusType(DataType: Integer; SubDataType: integer): TnxFieldType;
    function GetInternalType(DBType: Word; DBLength: Integer; DBScale: SmallInt): Word;
    procedure DetectDataType(DBType: Word; DBLength: Integer; DBScale: SmallInt;
      out DataType, SubDataType: Word; out Length, Scale: Integer);

    procedure CreateBatchCommand; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;
    function InternalExecute(OpenMode: TnxOpenMode; NeedsGet: Boolean): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure SetConnection(Value: TCRConnection); override;

    class function GetParserClass: TSQLParserClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute; override;
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    property EnableBCD;
    property EnableFMTBCD;
  end;

  TNexusRecordSet = class(TCRRecordSet)
  private
    FCommand: TNexusCommand;
    FMapperFieldOffset: array of integer;
    FMapperList: TObjectList;
    FCursorRecBuf: PnxByteArray;
    FServerCursorUsed: boolean;
    FFetchFromBookmark: boolean;
    FCursorUpdate: boolean;
    FroAfterUpdate: boolean; // DataSet.RefreshOptions.roAfterUpdate
    FFetchStatus: TnxResult;
    FIndexFieldNames: string;

    procedure Check(ErrorCode: cardinal);

    function GetDisconnectedMode: boolean;
    procedure CheckActive;
    procedure FreeMappers;

    procedure ReadFieldValue(Field: TCRFieldDesc;  ValueBuf, DataBuf: IntPtr; DataLenPtr: PWord);
    procedure ReadFieldBlob(Field: TCRFieldDesc; BlobNr: TnxInt64; DataBuf: IntPtr);
    procedure ReadFieldValues(RecBuf: IntPtr; Source: PnxByteArray; Row: Integer);

    procedure WriteFieldValue(Field: TFieldDesc; CursorBuf, ValueBuf: IntPtr; ValueLen: Word);
  protected
    function GetDatabaseCursor: TnxAbstractCursor;

    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;

  { Open / Close }
    function NeedInitFieldsOnPrepare: boolean; override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure InternalClose; override;

  { Fields }
    procedure CreateFieldDescs; override;
    procedure DescribeFieldDesc(Field: TCRFieldDesc; nxFieldDesc: TnxFieldDescriptor);
    function IdentityFieldIsData: boolean; override;

  { Fetch }
    procedure InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean); override;
    function CanFetchBack: boolean; override; // Return True, if BlockMan is store only one block of records
    function GetCursorRecord(NewRecord: boolean): PnxByteArray;
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    function FetchingAccessible(FetchBack: boolean): boolean; override;
    function ProcessFetchedException(E: Exception): boolean; override;

  { Modify }
    procedure InternalAppend(RecBuf: IntPtr); override;
    procedure InternalDelete; override;
    procedure InternalUpdate(RecBuf: IntPtr); override;
    procedure InternalAppendOrUpdate(RecBuf: IntPtr; const IsAppend: boolean);

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

  { Fields }
    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;
    procedure DetectIdentityField; override;
    procedure ClearFields; override;
    procedure DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
      out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean; out IsAutoIncrement: Boolean);


  { Sorting }
    procedure SetIndexFieldNames(const Value: string); override;

  { Open / Close }
    procedure Prepare; override;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    function IsFullReopen: boolean; override;
    procedure Reopen; override;

  { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; override;
    function NeedGetRecordAfterGotoBookmark: boolean; override;

  { Records }
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

    procedure CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc); override;

  { Sorting }
    procedure SortItems; override;
  end;

  TNexusBlobData = class(TCRBlobData)
  private
    FRecordSet: TNexusRecordSet;
    FField: TFieldDesc;
    FBlobNr: TnxInt64;
  public
    destructor Destroy; override;

    function GetSize: Cardinal; override;
    procedure SetSize(Value: cardinal); override;

    function Read(Position: Cardinal; Count: Cardinal; Dest: IntPtr): Cardinal; override;
    procedure Write(Position: Cardinal; Count: Cardinal; Source: IntPtr); override;
    procedure Truncate(NewSize: Cardinal); override;
  end;

  TNexusBlob = class(TBlob)
  private
    function GetBlobNr: TnxInt64;
  protected
    function CreateBlobData: TCRBlobData; override;

    procedure SetIsUnicode(Value: boolean); override;
    function GetSizeAnsi: Cardinal; override;
  public
    constructor Create(RecordSet: TNexusRecordSet; Field: TFieldDesc); overload;
    procedure Init(const BlobNr: TnxInt64);

    function Read(Position: cardinal; Count: cardinal; Dest: IntPtr): cardinal; override;
    procedure Write(Position: cardinal; Count: cardinal; Source: IntPtr); override;
    procedure Clear; override;
    procedure Truncate(NewSize: cardinal); override;

    property BlobNr: TnxInt64 read GetBlobNr;
  end;

  TNexusMetaData = class (TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;
    function GetConnection: TNexusConnection;

    function GetTables(Restrictions: TStrings): TData; override;
    function GetColumns(Restrictions: TStrings): TData; override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetIndexColumns(Restrictions: TStrings): TData; override;
    function GetConstraints(Restrictions: TStrings): TData; override;

    function GetDatabases(Restrictions: TStrings): TData; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$IFNDEF LITE}
  TNexusLoader = class (TCRSimpleLoader)
  private
    FDirectLoad: boolean;
    FStartOffset: integer;
    FStream: TnxMemoryStream;
    FBlobStreams: array of TStream;
    FCursor: TnxAbstractCursor;
    FFieldDescriptor: TnxFieldsDescriptor;
    FBuffer: pointer;
    FRecordSize: Cardinal;

    function IsBlobField(FieldType: TnxFieldType): boolean;
    procedure LoadStream;
    procedure ResetBuffers;
    procedure ResetLoader;
    procedure InitLoader;
  protected
    procedure CreateCommand; override;

    procedure DoLoadRow; override;
    procedure DoPrepare; override;
    procedure DoPutColumnData(Col: integer; Row: integer; const Value: variant); override;
    function IsPutColumnDataAllowed: boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Finish; override;
    procedure DoLoad; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;
{$ENDIF}

  TNexusUtils = class
    class function GetServerEngine(Obj: TNexusConnection): TnxBaseServerEngine;
  end;

{$ENDIF} // DUMMY

implementation

{$IFNDEF DUMMY}
uses
  Math, StrUtils, CRProps, CRFunctions, MemUtils, DAConsts, MemDS,
  nxdbStrings, nxsdNativeVariantConverter, nxsdConst, nxsdFmtBcd,
  nxllConst, nxllUtils, nxllWideString,
{$IFNDEF UNIDACPRO}
  NexusProps, NexusDataTypeMap, NexusConsts;
{$ELSE}
  NexusPropsUni, NexusDataTypeMapUni, NexusConstsUni;
{$ENDIF}

const
  ParamTypeToNexusDB: array[TParamDirection] of TnxParamType = (
    nxptUnknown, nxptInput, nxptOutput, nxptInputOutput, nxptResult);

  ParamTypeFromNexusDB: array[TnxParamType] of TParamDirection = (
    pdUnknown, pdInput, pdOutput, pdInputOutput, pdResult);

{ TnxEngine }

constructor TnxEngine.Create(Owner: TNexusConnection);
begin
  inherited Create(nil);
  FOwner := Owner;
  FCommands := TList.Create;
end;

destructor TnxEngine.Destroy;
begin
  Disconnect;
  FCommands.Free;
  inherited;
end;

procedure TnxEngine.AssignTo(Dest: TPersistent);
begin
  if Dest is TnxEngine then begin
  {$IFNDEF NEXUS_EMBEDDED}
    TnxEngine(Dest).FTransport := FTransport;
    TnxEngine(Dest).FSecureTransport := FSecureTransport;
  {$ENDIF}
    TnxEngine(Dest).FSqlEngine := FSqlEngine;
    TnxEngine(Dest).FServerEngine := FServerEngine;
    TnxEngine(Dest).FSession := FSession;
    TnxEngine(Dest).FDataBase := FDataBase;
    TnxEngine(Dest).FTransContext := FTransContext;
  end;
  inherited;
end;

procedure TnxEngine.nxcNotification(aSender: TnxComponent;
  aOperation: TnxNotifyOp; aParam: Pointer);
begin
  inherited;
  if aSender = FServerEngine then
    case aOperation of
      nxn_StateChanging:
        if FServerEngine.State = nxsStarted then begin
          FDataBase.Free;
          FTransContext.Free;
          FDataBase := nil;
          FTransContext := nil;
          FSession := nil;
          ClearRefs;
        end;
      nxn_ConnectionLost:
      ;
    end;
end;

procedure TnxEngine.Connect(const Server, Database, Username, Password: string; Port: integer;
  ConnectionTimeout, HeartbeatInterval, LostConnectionTimeout, WatchdogInterval, CommandTimeout: integer;
  ReadOnly: boolean; Protocol: TNexusProtocol; const SecretKey: AnsiString);
var
  AliasType: string;
begin
{$IFNDEF NEXUS_EMBEDDED}
  Assert(FTransport = nil);
  Assert(FSecureTransport = nil);
{$ENDIF}
  Assert(FSqlEngine = nil);
  Assert(FServerEngine = nil);
  Assert(FSession = nil);
  Assert(FDataBase = nil);

{$IFNDEF NEXUS_EMBEDDED}
  if Server <> '' then begin
    FServerEngine := TnxRemoteServerEngine.Create(nil);

    if Protocol in [npTCP, npSTCP] then
      FTransport := TnxWinsockTransport.Create(nil)
    else
      FTransport := TnxNamedPipeTransport.Create(nil);

    FTransport.Timeout := ConnectionTimeout;
    FTransport.HeartbeatInterval := HeartbeatInterval;
    FTransport.LostConnectionTimeout := LostConnectionTimeout;
    FTransport.WatchdogInterval := WatchdogInterval;
    FTransport.ServerName := Server;
    if Port = 0 then
      Port := NexusDefValPort;
    FTransport.Port := Port;

    if Protocol in [npSTCP, npSPipe] then begin
      FSecureTransport := TnxBlowfishRC4SecuredTransport.Create(nil);
      FSecureTransport.Key := SecretKey;
      FSecureTransport.Transport := FTransport;

      TnxRemoteServerEngine(FServerEngine).Transport := FSecureTransport;
    end
    else
      TnxRemoteServerEngine(FServerEngine).Transport := FTransport;

    AliasType := 'Name';
  end
  else
{$ENDIF}
  begin
    FSqlEngine := TnxSqlEngine.Create(nil);
    // TnxSqlEngine(FSqlEngine).DictionaryMode := dmDisabled;
    FServerEngine := TnxServerEngine.Create(nil);
    TnxServerEngine(FServerEngine).SqlEngine := FSqlEngine;
    if ReadOnly then
      TnxServerEngine(FServerEngine).Options := [seoReadOnly];
    AliasType := 'Path';
  end;

  nxcAddDependingOn(FServerEngine);
  FServerEngine.Active := True;
  FOwner.Check(FServerEngine.SessionOpen(FSession, UserName, Password, 'Direct', ConnectionTimeout));
  FOwner.Check(FSession.TransContextCreate(FTransContext, ConnectionTimeout));
{$IFNDEF NEXUS_EMBEDDED}
  if Database = '#INMEM' then
    FSession.AliasAdd('#INMEM', '#INMEM', False);
{$ENDIF}
  FOwner.Check(FSession.DatabaseOpen(FDataBase, AliasType, Database, FTransContext, omReadWrite, smShared, CommandTimeout));
end;

procedure TnxEngine.Disconnect;
begin
  try
    FServerEngine.Free;
    FSqlEngine.Free;
  {$IFNDEF NEXUS_EMBEDDED}
    FTransport.Free;
    FSecureTransport.Free;
  {$ENDIF}
    ClearRefs;
  finally
    FDataBase := nil;
    FTransContext := nil;
    FSession := nil;
    FServerEngine := nil;
    FSqlEngine := nil;
  {$IFNDEF NEXUS_EMBEDDED}
    FTransport := nil;
    FSecureTransport := nil;
  {$ENDIF}
  end;
end;

procedure TnxEngine.RegisterClient(Command: TNexusCommand);
begin
  if FCommands.IndexOf(Command) = -1 then
    FCommands.Add(Command);
end;

procedure TnxEngine.UnRegisterClient(Command: TNexusCommand);
begin
  FCommands.Remove(Command);
end;

procedure TnxEngine.ClearRefs;
var
  Command: TNexusCommand;
  i: Integer;
begin
  for i := 0 to FCommands.Count - 1 do begin
    Command := TNexusCommand(FCommands[i]);
    Command.FnxEngine := nil;
    Command.FPreparedStatement := nil;
    Command.FCursor := nil;
  end;
  FCommands.Clear;
end;

{ TNexusConnection }

constructor TNexusConnection.Create;
begin
  inherited;

  FProtocol := NexusDefValProtocol;
  FSecretKey := '';
  FConnectionTimeout := DefValConnectionTimeout;
  FHeartbeatInterval := 10;
  FLostConnectionTimeout := 10;
  FWatchdogInterval := 10;
  FCommandTimeout := 15;
  FnxEngine := TnxEngine.Create(Self);
  FReadOnly := False;
  FDetectFieldsOnPrepare := False;
end;

destructor TNexusConnection.Destroy;
begin
  Disconnect;
  FnxEngine.Free;

  inherited;
end;

function TNexusConnection.GetCommandTimeoutMSec: integer;
begin
 Result := FCommandTimeout * 1000;
end;

function TNexusConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prPort:
      FPort := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prHeartbeatInterval:
      FHeartbeatInterval := Value;
    prLostConnectionTimeout:
      FLostConnectionTimeout := Value;
    prWatchdogInterval:
      FWatchdogInterval := Value;
    prCommandTimeout:
      FCommandTimeout := Value;
    prDatabaseReadOnly:
      FReadOnly := Value;
    prDetectFieldsOnPrepare:
      FDetectFieldsOnPrepare := Value;
    prProtocol:
      FProtocol := TNexusProtocol(Integer(Value));
    prSecretKey:
      FSecretKey := AnsiString(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TNexusConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prHeartbeatInterval:
      Value := FHeartbeatInterval;
    prLostConnectionTimeout:
      Value := FLostConnectionTimeout;
    prWatchdogInterval:
      Value := FWatchdogInterval;
    prCommandTimeout:
      Value := FCommandTimeout;
    prDatabaseReadOnly:
      Value := FReadOnly;
    prDetectFieldsOnPrepare:
      Value := FDetectFieldsOnPrepare;
    prProtocol:
      Value := integer(FProtocol);
    prSecretKey:
      Value := FSecretKey;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TNexusConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TNexusCommand;
end;

function TNexusConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TNexusRecordSet;
end;

function TNexusConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TNexusTransaction;
end;

function TNexusConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TNexusLoader;
end;

function TNexusConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TNexusMetaData;
end;

class function TNexusConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TNexusCommand.GetMapRulesClass;
end;

procedure TNexusConnection.ChekTransContext(Component: TObject);
begin
  if FnxEngine.FTransContext = nil then
    Check(DBIERR_SERVERCOMMLOST, Component);
end;

procedure TNexusConnection.ChekDataBase(Component: TObject = nil);
begin
  if FnxEngine.FDataBase = nil then
    Check(DBIERR_SERVERCOMMLOST, Component); // 'Lost connection to server during query'
end;

function TNexusConnection.nxTransContext: TnxAbstractTransContext;
begin
  Result := FnxEngine.FTransContext;
end;

function TNexusConnection.nxDataBase: TnxAbstractDatabase;
begin
  Result := FnxEngine.FDataBase;
end;

procedure TNexusConnection.Check(ErrorCode: cardinal; Component: TObject = nil);
begin
  if Component = nil then
    Component := Self.Component;
  if ErrorCode <> DBIERR_NONE then
    ProcessError(ErrorCode, Component);
end;

procedure TNexusConnection.ProcessError(ErrorCode: cardinal; Component: TObject; Stream: TStream = nil; const Msg: string = '');
var
  nxError: EnxDatabaseError;
  Error: ENexusError;
  EMessage: string;
  ErrorMessage: AnsiString;
  ErrorMessageLength: integer;
  Fail, NeedFreeError: boolean;
begin
  if Stream <> nil then begin
    Stream.Position := 0;
    ErrorMessageLength := 0;
    Stream.Read(ErrorMessageLength, SizeOf(ErrorMessageLength));
    if ErrorMessageLength > 0 then begin
      SetLength(ErrorMessage, ErrorMessageLength);
      Stream.Read(PAnsiChar(ErrorMessage)^, ErrorMessageLength);
      nxError := EnxDatabaseError.nxcCreate(nil{Component}, ErrorCode, Msg, [#13#10, ErrorMessage]);
    end
    else
      nxError := EnxDatabaseError.nxCreate(ErrorCode);
  end
  else
    nxError := EnxDatabaseError.nxCreate(ErrorCode);

  try
    EMessage := nxError.ProcessedErrorString;
  finally
    nxError.Free;
  end;

  NeedFreeError := True;
  Error := ENexusError.Create(Integer(ErrorCode), EMessage);
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

procedure TNexusConnection.Connect(const ConnectString: string);
begin
  if FConnected then
    Exit;

  try
    FnxEngine.Connect(FServer, FDatabase, FUserName, FPassword, FPort,
      FConnectionTimeout * 1000, FHeartbeatInterval * 1000, FLostConnectionTimeout * 1000,
      FWatchdogInterval * 1000, FCommandTimeout * 1000, FReadOnly, FProtocol, FSecretKey);

    FConnected := True;
    FNativeConnection := True;
    inherited;
  except
    on EFailOver do;
    else begin
      FnxEngine.Disconnect;
      FConnected := False;
      raise;
    end;
  end;
end;

procedure TNexusConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FConnected := False;
  try
    if FNativeConnection then
      FnxEngine.Disconnect;
  finally
    FNativeConnection := True;
  end;

  inherited;
end;

procedure TNexusConnection.Ping;
var
  Command: TNexusCommand;
begin
  Command := TNexusCommand.Create;
  try
    Command.SetConnection(Self);
    Command.SetSQL('SELECT 1');
    Command.Execute;
  finally
    Command.Free;
  end;
end;

function TNexusConnection.CheckIsValid: boolean;
begin
  FIsValid := (FnxEngine.FServerEngine <> nil) and (FnxEngine.FServerEngine.State = nxsStarted);
  Result := FIsValid;
end;

procedure TNexusConnection.Assign(Source: TCRConnection);
var
  Src: TNexusConnection;
begin
  inherited;

  Src := TNexusConnection(Source);
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FConnectionTimeout := Src.FConnectionTimeout;
  FHeartbeatInterval := Src.FHeartbeatInterval;
  FLostConnectionTimeout := Src.FLostConnectionTimeout;
  FWatchdogInterval := Src.FWatchdogInterval;
  FCommandTimeout := Src.FCommandTimeout;
  FReadOnly := Src.FReadOnly;
  FProtocol := Src.FProtocol;
  FSecretKey := Src.FSecretKey;
end;

procedure TNexusConnection.AssignConnect(Source: TCRConnection);
var
  Src: TNexusConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TNexusConnection(Source);
      Assign(Src);

      FnxEngine.Assign(Src.FnxEngine);
      FInternalTransaction.AssignConnect(Src.FInternalTransaction);
      FConnected := True;
      FNativeConnection := False;
    end;
  end;
end;

function TNexusConnection.GetServerVersion: string;
begin
  Result := IntToStr(FnxEngine.Session.ServerVersion);
end;

function TNexusConnection.GetServerVersionFull: string;
begin
  Result := IntToStr(FnxEngine.Session.ServerVersion);
end;

function TNexusConnection.GetClientVersion: string;
begin
  Result := IntToStr(nxVersionNumber div 10000) + '.' +
    Copy(Format('%0.4f %s', [Frac(nxVersionNumber / 10000.0), nxSpecialString]), 3, 255);
end;

function TNexusConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

{ TNexusTransaction }

constructor TNexusTransaction.Create;
begin
  inherited;
  FSavepointList := TStringList.Create;
end;

destructor TNexusTransaction.Destroy;
begin
  FSavepointList.Free;
  inherited;
end;

function TNexusTransaction.GetInTransaction: boolean;
var
  Connection: TNexusConnection;
  TransLevel: integer;
  Error: TnxResult;
begin
  Result := FActive;

  if Result then begin
    Connection := TNexusConnection(FConnections[0]);
    Connection.ChekTransContext(Component);
    Error := Connection.nxTransContext.TransactionGetLevel(TransLevel);
    if Error = DBIERR_NOACTIVETRAN then
      Result := False
    else begin
      Connection.Check(Error, Component);
      Result := TransLevel >= 0;
    end;
  end;
end;

procedure TNexusTransaction.StartTransaction;
var
  Connection: TNexusConnection;
  SnapShot: Boolean;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TNexusConnection(FConnections[0]);
  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  case FIsolationLevel of
    ilReadUncommitted, ilReadCommitted, ilRepeatableRead:
      SnapShot := False;
    ilIsolated, ilSnapshot:
      SnapShot := True;
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  Connection.ChekTransContext(Component);
  Connection.Check(Connection.nxTransContext.TransactionStart(False, SnapShot), Component);

  FActive := True;
  FNativeTransaction := True;
end;

procedure TNexusTransaction.Commit;
var
  Connection: TNexusConnection;
begin
  CheckActive;

  Connection := TNexusConnection(FConnections[0]);
  if FNativeTransaction then begin
    while FSavepointList.Count > 0 do begin
      Connection.ChekTransContext(Component);
      Connection.Check(Connection.nxTransContext.TransactionCommit, Component);
      FSavepointList.Delete(FSavepointList.Count - 1);
    end;
    Connection.Check(Connection.nxTransContext.TransactionCommit, Component);
  end;

  FSavepointList.Clear;
  FActive := False;
end;

procedure TNexusTransaction.Rollback;
var
  Connection: TNexusConnection;
begin
  CheckActive;

  Connection := TNexusConnection(FConnections[0]);
  if FNativeTransaction then begin
    while FSavepointList.Count > 0 do begin
      Connection.ChekTransContext(Component);
      Connection.Check(Connection.nxTransContext.TransactionRollback, Component);
      FSavepointList.Delete(FSavepointList.Count - 1);
    end;
    Connection.Check(Connection.nxTransContext.TransactionRollback, Component);
  end;

  FSavepointList.Clear;
  FActive := False;
end;

procedure TNexusTransaction.Savepoint(const Name: string);
var
  Connection: TNexusConnection;
  SnapShot: Boolean;
begin
  CheckActive;

  Connection := TNexusConnection(FConnections[0]);
  if FNativeTransaction then begin
    case FIsolationLevel of
      ilReadUncommitted, ilReadCommitted, ilRepeatableRead:
        SnapShot := False;
      ilIsolated, ilSnapshot:
        SnapShot := True;
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
    end;
    Connection.ChekTransContext(Component);
    Connection.Check(Connection.nxTransContext.TransactionStart(False, SnapShot), Component);
    FSavepointList.Add(Name);
  end;
end;

procedure TNexusTransaction.RollbackToSavepoint(const Name: string);
var
  Connection: TNexusConnection;
  ind, i: integer;
begin
  CheckActive;

  ind := FSavepointList.IndexOf(Name);
  if ind = -1 then
    raise ENexusError.Create(-1, SSavepointNotExist);

  Connection := TNexusConnection(FConnections[0]);
  if FNativeTransaction then begin
    i := FSavepointList.Count - 1;
    while i >= ind do begin
      Connection.ChekTransContext(Component);
      Connection.Check(Connection.nxTransContext.TransactionRollback, Component);
      FSavepointList.Delete(i);
      Dec(i);
    end;
  end;
end;

{ TNexusParamDesc }

procedure TNexusParamDesc.GetAnsiString(Buffer: pointer);
var
  AStr: AnsiString;
  Len: integer;
  SPtr: IntPtr;
begin
  AStr := AnsiString(Value);
  Len := Length(AStr);

  SPtr := Marshal.StringToHGlobalAnsi(AStr);
  try
    if FConvertEOL then
      RemoveCRString(SPtr, Length(AStr), Buffer, Len)
    else begin
      CopyBuffer(SPtr, Buffer, Len);
    end;
  finally
    FreeString(SPtr);
  end;
end;

procedure TNexusParamDesc.GetWideString(Buffer: pointer);
var
  WStr: WideString;
  Len: integer;
  SPtr: IntPtr;
begin
  WStr := WideString(Value);
  Len := Length(WStr);

  SPtr := Marshal.StringToHGlobalUni(WStr);
  try
    if FConvertEOL then
      RemoveCRUnicode(SPtr, Length(WStr), Buffer, Len)
    else
      CopyBuffer(SPtr, Buffer, Len * 2);
  finally
    FreeString(SPtr);
  end;
end;

procedure TNexusParamDesc.GetBytes(Buffer: pointer);
var
  AStr: AnsiString;
  Bytes: TBytes;
  Len: integer;
begin
  Bytes := nil;
  if VarIsStr(Value) then begin
    AStr := AnsiString(Value);
    Len := Length(AStr);

    Marshal.Copy(TBytes(AStr), 0, Buffer, Len);
  end
  else begin
    Bytes := Value;
    Len := Length(Bytes);

    Marshal.Copy(Bytes, 0, Buffer, Len);
  end;
end;

procedure TNexusParamDesc.GetData(Buffer: pointer);
var
  i64: Int64;
  Cur: Currency;
  Str: string;
{$IFNDEF FPC}
  Bcd: TBcd;
{$ENDIF}
begin
  case FDataType of
    dtString:
      GetAnsiString(Buffer);
    dtWideString:
      GetWideString(Buffer);
    dtInt8, dtUInt8:
      Marshal.WriteByte(Buffer, Byte(Value));
    dtSmallInt, dtWord:
      Marshal.WriteInt16(Buffer, Smallint(Value));
    dtInteger:
      Marshal.WriteInt32(Buffer, Integer(Value));
    dtLargeInt: begin
      i64 := Value;
      Marshal.WriteInt64(Buffer, i64);
    end;
    MemData.dtDate:
      Marshal.WriteInt32(Buffer, DateTimeToTimeStamp(TDateTime(Value)).Date);
    MemData.dtTime:
      Marshal.WriteInt32(Buffer, DateTimeToTimeStamp(TDateTime(Value)).Time);
    MemData.dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}:
      Marshal.WriteInt64(Buffer, BitConverter.DoubleToInt64Bits(TDateTime(Value)));
    dtBCD: begin
      Cur := Currency(Value);
      Str := CurrToStr(Cur);
      if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
        Str := StringReplace(Str, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);

      CopyBuffer(Marshal.StringToHGlobalAnsi(AnsiString(Str)), Buffer, Length(Str));
    end;
  {$IFNDEF FPC}
    dtFMTBCD: begin
      if VarType(Value) = VarFMTBcd then
        Bcd := VarToBcd(Value)
      else
        Bcd := StrToBcd(Value);

      Str := BcdToStr(Bcd);
      if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
        Str := StringReplace(Str, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);

      CopyBuffer(Marshal.StringToHGlobalAnsi(AnsiString(Str)), Buffer, Length(Str));
    end;
  {$ENDIF}
    dtFloat, dtCurrency:
      Marshal.WriteInt64(Buffer, BitConverter.DoubleToInt64Bits(Value));
    dtBoolean:
      Marshal.WriteInt16(Buffer, Smallint(Boolean(Value)));
    dtBytes, dtVarBytes:
      GetBytes(Buffer);
    dtBlob, dtMemo, dtWideMemo:
      GetAnsiString(Buffer);
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;
end;

{ TNexusIndexDefs }

function TNexusIndexDefs.GetItem(Index: integer): TNexusIndexDef;
begin
  Result := TNexusIndexDef(inherited Items[Index]);
end;

function TNexusIndexDefs.FindIndexForFields(const Fields: string): TNexusIndexDef;
var
  I: Integer;
  Exact: Boolean;
begin
  Exact := True;
  while True do
  begin
    for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if Exact and (AnsiCompareText(Fields, Result.Fields) = 0) then
        Exit
    end;
    if not Exact then
      Break;
    Exact := False;
  end;
  Result := nil;
end;

{ TNexusCommand }

constructor TNexusCommand.Create;
begin
  inherited;

  FIndexDefs := TNexusIndexDefs.Create;
  FCommandTimeout := 15;
  FReadOnly := False;
  FRowsAffected := -1;
end;

destructor TNexusCommand.Destroy;
begin
  Unprepare;
  if FnxEngine <> nil then
    FnxEngine.UnRegisterClient(Self);

  FIndexDefs.Free;

  inherited;
end;

function TNexusCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
    prIsStoredProc:
      FIsStoredProc := Value;
    prCommandReadOnly:
      FReadOnly := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TNexusCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := FCommandTimeout;
    prRowsProcessed:
      Value := FRowsAffected;
    prCommandReadOnly:
      Value := FReadOnly;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TNexusCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TNexusConnection(Value);
  end;
end;

class function TNexusCommand.GetParserClass: TSQLParserClass;
begin
  Result := TNexusParser;
end;

class function TNexusCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TNexusMapRules;
end;

procedure TNexusCommand.HandleError(Result: TnxResult; const Msg: string; Stream: TStream);
begin
  if Result <> DBIERR_NONE then
    FConnection.ProcessError(Result, Component, Stream, Msg);
end;

function TNexusCommand.GetStatementType: TnxStatementType;
begin
  if FIsStoredProc then
    Result := TnxStatementType(stStoredProcedure)
  else
    Result := TnxStatementType(stQuery);
end;

procedure TNexusCommand.Prepare;
var
  Result: TnxResult;
  Stream: TnxMemoryStream;
begin
  if FPreparedStatement <> nil then
    Exit;

  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  FRowsAffected := -1;

  FConnection.ChekDataBase(Component);
  FConnection.Check(FConnection.nxDataBase.StatementAlloc(FPreparedStatement, FCommandTimeout * 1000), Component);
  try
    Stream := TnxMemoryStream.Create;
    try
      Result := FPreparedStatement.Prepare(GetStatementType, FSQL, Stream);
      HandleError(Result, rsQueryPrepareFail, Stream);
    finally
      Stream.Free;
    end;
  except
    FreeAndNil(FPreparedStatement);
    raise;
  end;

  FnxEngine := FConnection.nxEngine;
  FnxEngine.RegisterClient(Self);

  FCursorState := csPrepared;
end;

procedure TNexusCommand.Unprepare;
begin
  FreeAndNil(FPreparedStatement);

  if FnxEngine <> nil then begin
    FnxEngine.UnRegisterClient(Self);
    FnxEngine := nil;
  end;

  inherited;
end;

function TNexusCommand.GetPrepared: boolean;
begin
  Result := FPreparedStatement <> nil;
end;

procedure TNexusCommand.BindParams(Offset: Integer; out NeedsGet: Boolean);

  procedure nxParamToSqlParamDesc(Param: TNexusParamDesc; var SqlParam: TnxSqlParamDesc);
  var
    Len: Integer;
    ParamVarType: TVarType;
    ParamSize: Integer;
    ParamScale: Integer;
    ParamRequired: Boolean;
    ParamNull: Boolean;
    ParamValuePtr: PVariant;
    SafeArray: PVarArray;
    Blob: TBlob;
    Bcd: TBcd;
  begin
    ParamSize := Param.GetSize;
    ParamScale := Param.GetScale;
    ParamValuePtr := Param.GetItemPtr(Offset);
    ParamNull := VarIsEmpty(ParamValuePtr^) or VarIsNull(ParamValuePtr^);

    case Param.GetDataType of
      dtUnknown: begin
        SqlParam.piDataType := nxtChar;
        ParamSize := 0;
      end;
      dtWideString: begin
        SqlParam.piDataType := GetNexusType(Param.GetDataType, Param.GetSubDataType);
        if ParamNull then
          ParamSize := 0
        else
          ParamSize := Length(UnicodeString(ParamValuePtr^));
      end;
      else
        SqlParam.piDataType := GetNexusType(Param.GetDataType, Param.GetSubDataType);
    end;

    TnxFieldDescriptor.SetupField(SqlParam.piDataType, ParamSize, ParamScale, ParamRequired, Len);

    if ParamNull then
      Len := 0
    else
    if (SqlParam.piDataType in [nxtBlob, nxtBlobGraphic]) then begin
      ParamVarType := VarType(ParamValuePtr^);
      if ParamVarType = varSharedObject then begin
        Assert(TVarData(ParamValuePtr^).VPointer <> nil);
        Blob := TVarData(ParamValuePtr^).VPointer;
        Blob.Defrag;
        Len := Blob.Size;
        SetLength(SqlParam.piData, Len);
        if Len > 0 then
          if Blob.FirstPiece = nil then // ServerCursor
            Blob.Read(0, Len, SqlParam.piData)
          else
            Move(PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader))^, SqlParam.piData[0], Len);
      end
      else
      if ParamVarType = varArray + varByte then begin
        SafeArray := VarArrayAsPSafeArray(ParamValuePtr^);
        Len := SafeArray.Bounds[0].ElementCount;
        SetLength(SqlParam.piData, Len);
        if Len > 0 then
          Move(SafeArray.Data^, SqlParam.piData[0], Len);
      end
      else begin
        Len := Param.GetSize;
        SetLength(SqlParam.piData, Len);
        if Len > 0 then
          Param.GetData(@SqlParam.piData[0]);
      end;
    end
    else
      if SqlParam.piDataType = nxtBcd then
        VariantToNative(SqlParam.piDataType, CP_UTF8, Max(Param.GetSize, 20), 4, ParamValuePtr^, SqlParam.piData)
      else
      if SqlParam.piDataType = nxtFmtBCD then begin
        if VarType(ParamValuePtr^) = VarFMTBcd then
          Bcd := VarToBcd(ParamValuePtr^)
        else
          Bcd := StrToBcd(ParamValuePtr^);
        VariantToNative(SqlParam.piDataType, CP_UTF8, Bcd.Precision, Bcd.SignSpecialPlaces and 63, ParamValuePtr^, SqlParam.piData);
      end
      else
      if (SqlParam.piDataType in [nxtBlobMemo, nxtBlobWideMemo]) and
        (VarType(ParamValuePtr^) = varSharedObject) then begin
        Blob := TVarData(ParamValuePtr^).VPointer;
        Blob.Defrag;
        if SqlParam.piDataType = nxtBlobWideMemo then
          VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, Blob.AsWideString, SqlParam.piData)
        else
          VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, Blob.AsAnsiString, SqlParam.piData);
      end
      else
      if (SqlParam.piDataType = nxtChar) or
         (Param.GetSubDataType in [dtSingleChar, dtSingleWideChar])
      then
        if VarToStr(ParamValuePtr^) = '' then
          VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, ' ', SqlParam.piData)
        else if VarIsNull(ParamValuePtr^) or VarIsEmpty(ParamValuePtr^) then
          VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, ' ', SqlParam.piData)
        else begin
          SqlParam.piDataType := nxtWideChar;
          VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, WideString(ParamValuePtr^), SqlParam.piData);
        end
      else
      if SqlParam.piDataType = nxtShortString then
        VariantToNative(SqlParam.piDataType, 0, 0, 0, ParamValuePtr^, SqlParam.piData)
      else
        VariantToNative(SqlParam.piDataType, CP_UTF8, 0, 0, ParamValuePtr^, SqlParam.piData);
  end;

var
  i: integer;
  Param: TNexusParamDesc;
begin
  NeedsGet := False;
  SetLength(FBindedParams, Params.Count);

  for i := 0 to Params.Count - 1 do begin
    Param := TNexusParamDesc(Params[i]);
    FBindedParams[i].piParamName := Param.GetName;
    FBindedParams[i].piParamType := ParamTypeToNexusDB[Param.GetParamType];
    nxParamToSqlParamDesc(Param, FBindedParams[i]);
    NeedsGet := NeedsGet or (FBindedParams[i].piParamType in [nxptOutput, nxptInputOutput, nxptResult]);
  end;
end;

procedure TNexusCommand.ReadOutParams;
var
  Param: TNexusParamDesc;
  i: integer;
begin
  if Length(FBindedParams) <> Params.Count then
    FConnection.Check(DBIERR_INVALIDPARAM, Component);

  for i := 0 to Params.Count - 1do begin
    Param := TNexusParamDesc(Params.Items[i]);
    with FBindedParams[i] do begin
      if piParamType <> ParamTypeToNexusDB[Param.GetParamType] then
        FConnection.Check(DBIERR_INVALIDPARAM, Component);

      if piParamType in [nxptOutput, nxptInputOutput, nxptResult] then
        if Length(piData) < 1 then
          Param.Clear
        else
          Param.Value := VariantFromNative(piDataType, CP_UTF8, 0, 0, @piData[0], Length(piData));
    end;
  end;
end;

procedure TNexusCommand.Execute;
var
  NeedsGet: Boolean;
  OpenMode: TnxOpenMode;
  Iter, Affected: integer;
begin
  if GetCursorState > csPrepared then
    Exit;

  // FCursor <> nil on Reopen
  if FCursor <> nil then
    Exit;

  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  try
    try
      FRowsAffected := -1;
      FIndexDefs.Updated := False;
      Iter := 0;

      while Iter < FBatchIters do begin
        if FReadOnly then
          OpenMode := omReadOnly
        else
          OpenMode := omReadWrite;

        BindParams(FBatchOffset + Iter, NeedsGet);

        Affected := InternalExecute(OpenMode, NeedsGet);

        if FRowsAffected < 0 then
          FRowsAffected := 0;
        FRowsAffected := FRowsAffected + Affected;

        if NeedsGet then
          ReadOutParams;

        //FConnection.FLastInsertId := ?;
        Inc(Iter);
      end;
    finally
      if not FRecordSetExec then begin
        FTableDescriptor := nil;
        FFieldsDescriptor := nil;
        FreeAndNil(FCursor);
      end;
    end;

    if FCursor <> nil then begin
      FnxEngine := FConnection.nxEngine;
      FnxEngine.RegisterClient(Self);
    end;

  except
    if not FRecordSetExec and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;

  if not FRecordSetExec and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TNexusCommand.FreeCursor;
begin
  if FCursor = nil then
    Exit;

  if FPreparedStatement = nil then begin
    FnxEngine.UnRegisterClient(Self);
    FnxEngine := nil;
  end;

  FIndexDefs.Updated := False;
  FTableDescriptor := nil;
  FFieldsDescriptor := nil;
  FreeAndNil(FCursor);
end;

procedure TNexusCommand.UpdateIndexDefs;
var
  IndexDef: TNexusIndexDef;
  Fields: string;
  i, j: integer;
begin
  if not FIndexDefs.Updated then
  try
    Assert(FTableDescriptor <> nil);
    FIndexDefs.Clear;

    if Assigned(FTableDescriptor.IndicesDescriptor) then
      for i := 0 to FTableDescriptor.IndicesDescriptor.IndexCount - 1 do
        with FTableDescriptor.IndicesDescriptor.IndexDescriptor[i] do begin
          Fields := '';
          if KeyDescriptor is TnxCompKeyDescriptor then
            for j := 0 to TnxCompKeyDescriptor(KeyDescriptor).KeyFieldCount - 1 do begin
              if Fields <> '' then
                Fields := Fields + ';';
              Fields := Fields + TnxCompKeyDescriptor(KeyDescriptor).KeyFields[j].Field.Name;
            end;

          IndexDef := TNexusIndexDef.Create;
          IndexDef.Name := Name;
          IndexDef.Fields := Fields;
          FIndexDefs.Add(IndexDef);
        end;
  finally
    FIndexDefs.Updated := True;
  end;
end;

function TNexusCommand.GetDefaultIndexName: string;
begin
  Assert(FTableDescriptor <> nil);

  if FTableDescriptor.IndicesDescriptor <> nil then begin
    with FTableDescriptor.IndicesDescriptor do
      if DefaultIndex >= 0 then
        Result := IndexDescriptor[DefaultIndex].Name
      else
        Result := '';
  end
  else
    Result := '';
end;

function TNexusCommand.GetNexusType(DataType: integer; SubDataType: integer): TnxFieldType;
begin
  case DataType of
    dtBoolean:
      Result := nxtBoolean;

    dtByte:
      Result := nxtByte;
    dtWord:
      Result := nxtWord16;
    dtLongWord:
      Result := nxtWord32;
    dtInt8:
      Result := nxtInt8;
    dtSmallint:
      Result := nxtInt16;
    dtInteger:
      Result := nxtInt32;
    dtInt64:
      Result := nxtInt64;
    dtSingle:
      Result := nxtSingle;
    dtFloat:
      if SubDataType = dtExtended then
        Result := nxtExtended
      else
        Result := nxtDouble;
    dtExtended:
      Result := nxtExtended;
    dtCurrency:
      Result := nxtCurrency;
    dtBCD:
      Result := nxtBCD;
    dtFmtBCD:
      Result := nxtFMTBCD;

    dtDate:
      Result := nxtDate;
    dtTime:
      Result := nxtTime;
    dtDateTime, dtSQLTimeStamp:
      Result := nxtDateTime;

    dtGuid:
      Result := nxtGuid;
    dtString:
      if SubDataType in [dtFixedChar, dtSingleChar] then
        Result := nxtChar
      else if SubDataType = dtShortStirng then
        Result := nxtShortString
      else
        Result := nxtNullString;
    dtWideString:
      if SubDataType in [dtFixedWideChar, dtSingleWideChar] then
        Result := nxtWideChar
      else
        Result := nxtWideString;
    dtMemo:
      Result := nxtBlobMemo;
    dtWideMemo:
      Result := nxtBlobWideMemo;

    dtBytes, dtVarBytes:
      Result :=  nxtByteArray;
    dtBlob:
      Result := nxtBlob;

    else
      raise Exception.Create(SDataTypeNotSupported);
  end;
end;

function TNexusCommand.GetInternalType(DBType: Word; DBLength: Integer; DBScale: SmallInt): Word;
begin
  case DBType of
    nxBoolean:
      Result := dtBoolean;

    nxAutoInc:
      Result := dtAutoIncrement;
    nxRecRev:
      Result := dtRecRev;

    nxByte:
      Result := dtByte;
    nxWord16:
      Result := dtWord;
    nxWord32:
      Result := dtLongWord;
    nxInt8:
      Result := dtInt8;
    nxInt16:
      Result := dtSmallint;
    nxInt32:
      Result := dtInteger;
    nxInt64:
      Result := dtInt64;
    nxSingle:
      Result := dtSingle;
    nxDouble:
      Result := dtFloat;
    nxCurrency:
      Result := dtCurrency;
    nxExtended:
      Result := dtExtended;
    nxBCD:
      Result := dtBCD;
    nxFmtBCD:
      Result := dtFMTBCD;

    nxDate:
      Result := dtDate;
    nxTime:
      Result := dtTime;
    nxDateTime:
      Result := dtDateTime;
    nxInterval:
      Result := dtUnknown;

    nxGuid:
      Result := dtGuid;
    nxChar:
      if (DBLength = 1) and (DBScale = 0) then
        Result := dtSingleChar
      else
        Result := dtFixedChar;
    nxWideChar:
      if (DBLength = 1) and (DBScale = 0) then
        Result := dtSingleWideChar
      else
        Result := dtFixedWideChar;
    nxShortString:
      Result := dtShortStirng;
    nxNullString:
      Result := dtString;
    nxWideString:
      Result := dtWideString;
    nxBlobMemo:
      Result := dtMemo;
    nxBlobWideMemo:
      Result := dtWideMemo;

    nxByteArray:
      Result := dtBytes;
    nxBlob:
      Result := dtBlob;
    nxBlobGraphic:
      Result := dtGraphicBlob;

    else
      raise Exception.Create(SDataTypeNotSupported);
  end;
end;

procedure TNexusCommand.DetectDataType(DBType: Word; DBLength: Integer; DBScale: SmallInt;
  out DataType, SubDataType: Word; out Length, Scale: Integer);
begin
  SubDataType := dtUnknown;
  Length := DBLength;
  Scale := DBScale;

  case DBType of
    nxBoolean:
      DataType := dtBoolean;

    nxAutoInc: begin
      DataType := dtInteger;
      SubDataType := dtAutoIncrement;
    end;

    nxByte:
      DataType := dtByte;
    nxWord16:
      DataType := dtWord;
    nxWord32:
      DataType := dtLongWord;
    nxInt8:
      DataType := dtInt8;
    nxInt16:
      DataType := dtSmallint;
    nxInt32:
      DataType := dtInteger;
    nxInt64:
      DataType := dtInt64;
    nxSingle:
      DataType := dtSingle;
    nxDouble:
      DataType := dtFloat;
    nxCurrency:
      DataType := dtCurrency;
    nxExtended: begin
      DataType := dtFloat;
      SubDataType := dtExtended;
    end;
    nxBCD:
      DataType := dtBCD;
    nxFmtBCD:
      DataType := dtFMTBCD;

    nxDate:
      DataType := dtDate;
    nxTime:
      DataType := dtTime;
    nxDateTime:
      DataType := dtDateTime;
    nxInterval:
      DataType := dtUnknown;

    nxGuid: begin
      DataType := dtGuid;
      Length := 38;
    end;
    nxChar: begin
      DataType := dtString;
      if (DBLength = 1) and (DBScale = 0) then
        SubDataType := dtSingleChar
      else
        SubDataType := dtFixedChar;
    end;
    nxWideChar: begin
      DataType := dtWideString;
      if (DBLength = 1) and (DBScale = 0) then
        SubDataType := dtSingleWideChar
      else
        SubDataType := dtFixedWideChar;
    end;
    nxShortString: begin
      DataType := dtString;
      SubDataType := dtShortStirng;
    end;
    nxNullString:
      DataType := dtString;
    nxWideString:
      DataType := dtWideString;
    nxBlobMemo:
      DataType := dtMemo;
    nxBlobWideMemo:
      DataType := dtWideMemo;

    nxByteArray:
      DataType := dtBytes;
    nxBlob:
      DataType := dtBlob;
    nxBlobGraphic: begin
      DataType := dtBlob;
      SubDataType := dtGraphicBlob;
    end;

    nxRecRev: begin
      DataType := dtLongWord;
      SubDataType := dtRecRev;
    end
    else
      raise Exception.Create(SDataTypeNotSupported);
  end;

  if SubDataType = dtUnknown then
    SubDataType := DataType;
end;

procedure TNexusCommand.CreateBatchCommand;
begin
  // do nothing
end;

procedure TNexusCommand.InternalExecuteBatch(Iters, Offset: integer);
var
  NeedPrepare: boolean;
begin
  FBatchIters := Iters;
  FBatchOffset := Offset;

  NeedPrepare := not GetPrepared;
  if NeedPrepare then
    Prepare;
  try
    Execute;
  finally
    if NeedPrepare then
      UnPrepare;
  end;
end;

function TNexusCommand.InternalExecute(OpenMode: TnxOpenMode; NeedsGet: Boolean): Integer;
var
  Res: TnxResult;
  ExecStream: TnxMemoryStream;
  SetParamStream: TnxMemoryStream;
  GetParamStream: TnxMemoryStream;
  PrepareStream: TnxMemoryStream;
  Phase: TnxStatementExecDirectPhase;
begin
  ExecStream := TnxMemoryStream.Create;
  SetParamStream := TnxMemoryStream.Create;
  if NeedsGet then
    GetParamStream := TnxMemoryStream.Create
  else
    GetParamStream := nil;
  try
    if Assigned(FPreparedStatement) then begin
      Res := FPreparedStatement.SetParams(FBindedParams, SetParamStream);
      HandleError(Res, rsQuerySetParamsFail, SetParamStream);
      Res := FPreparedStatement.Exec(FCursor, OpenMode, ExecStream);
      HandleError(Res, rsQueryExecFail, ExecStream);

      if NeedsGet then begin
        Res := FPreparedStatement.GetParams(FBindedParams, GetParamStream);
        HandleError(Res, rsQueryGetParamsFail, GetParamStream);
      end;
    end
    else begin
      PrepareStream := TnxMemoryStream.Create;
      try
        FConnection.ChekDataBase(Component);
        Res := FConnection.nxDataBase.StatementExecDirect(FCommandTimeout * 1000,
          GetStatementType, FSQL, PrepareStream, FBindedParams, SetParamStream,
          FCursor, OpenMode, ExecStream, GetParamStream, Phase);
        case Phase of
          sedAlloc:
            FConnection.Check(Res, Component);
          sedPrepare:
            HandleError(Res, rsQueryPrepareFail, PrepareStream);
          sedSetParams:
            HandleError(Res, rsQuerySetParamsFail, SetParamStream);
          sedGetParams:
            HandleError(Res, rsQueryGetParamsFail, GetParamStream);
          sedFree:
            FConnection.Check(Res, Component);
        end;
      finally
        PrepareStream.Free;
      end;

      HandleError(Res, rsQueryExecFail, ExecStream);
    end;

    if FCursor <> nil then begin
      FCursor.FilterSetTimeout(FCommandTimeout * 1000);
      FTableDescriptor := FCursor.TableDescriptor;
      FFieldsDescriptor := FTableDescriptor.FieldsDescriptor;
      Result := 0;
    end
    else begin
      FTableDescriptor := nil;
      FFieldsDescriptor := nil;
      if GetStatementType = stStoredProcedure then
        ExecStream.Seek(SizeOf(Integer), soFromCurrent);
      ExecStream.Read(Result, SizeOf(Integer));
    end;
  finally
    GetParamStream.Free;
    SetParamStream.Free;
    ExecStream.Free;
  end;
end;

procedure TNexusCommand.InitProcParams(const Name: string);
var
  v: variant;
  s: string;
  MetaData: TNexusMetaData;
  Data: TData;
  Restrictions: TStrings;
  RecBuf: IntPtr;
  Param: TParamDesc;
  NexusType: TnxFieldType;
  DBType: Word;
  DBLength: Integer;
  DBScale: SmallInt;
  DataType: Word;
  SubDataType: Word;
  Len: Integer;
  Scale: Integer;
begin
  if FConnection = nil then
    raise Exception.Create('SConnectionNotDefined');

  Restrictions := TStringList.Create;
  MetaData := TNexusMetaData.Create;
  try
    Restrictions.Add('PROCEDURE_NAME=' + Name);
    Data := MetaData.GetMetaData(FConnection, nil, 'procedureparameters', Restrictions);

    Data.AllocRecBuf(RecBuf);
    try
      FParams.Clear;
      while True do begin
        Data.GetNextRecord(RecBuf);
        if Data.Eof then
          break;

        Param := AddParam;

        Data.GetFieldAsVariant(Data.Fields[3], RecBuf, v);
        s := VarToStr(v);
        if s = '' then
          Param.SetName('RESULT')
        else
          Param.SetName(s);

        Data.GetFieldAsVariant(Data.Fields[5], RecBuf, v);
        s := VarToStr(v);
        Param.SetParamType(ParamTypeFromNexusDB[nxStrToParamType(s)]);

        Data.GetFieldAsVariant(Data.Fields[6], RecBuf, v);
        s := VarToStr(v);
        NexusType := FieldDataTypesMapSQL(s);
        DBType := TNexusConverterManager.GetDBType(NexusType);

        Data.GetFieldAsVariant(Data.Fields[7], RecBuf, v);
        if not VarIsNull(v) then
          DBLength := v
        else
          DBLength := 0;

        Data.GetFieldAsVariant(Data.Fields[9], RecBuf, v);
        if not VarIsNull(v) then
          DBScale := v
        else
          DBScale := 0;

        DetectDataType(DBType, DBLength, DBScale, DataType, SubDataType, Len, Scale);

        Param.SetDataType(DataType);
        Param.SetSubDataType(SubDataType);
        Param.SetSize(Len);
        Param.SetScale(Scale);
      end;
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  finally
    MetaData.Free;
    Restrictions.Free;
  end;
end;

function TNexusCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
var
  f, i: integer;
begin
  if NeedDescribe then
    InitProcParams(Name);

  if (FParams.Count > 0) and (FParams[0].GetParamType = pdResult) then begin
    f := 1;
    Result := ':' + FParams[0].GetName + ' = call' + Name + '(';
    // FSQL := '? = call' + Name + '(';
  end
  else begin
    f := 0;
    Result := 'call ' + Name + '(';
    // FSQL := Result;
  end;

  for i := f to FParams.Count - 1 do begin
    if i > f then begin
      Result := Result + ', ';
      // FSQL := FSQL + ', ';
    end;
    Result := Result + ':' + FParams[i].GetName;
    // FSQL := FSQL + '?';
  end;

  Result := Result + ')';
  FSQL := Name; // FSQL + ')';
  FUserSQL := Result;
end;

function TNexusCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TNexusCommand.SetCursorState(Value: TCursorState);
begin
  FCursorState := Value;
end;

{ TNexusRecordSet }

constructor TNexusRecordSet.Create;
begin
  inherited Create;

  FFetchAll := False;
  FFetchRows := 25;
  FMapperList := TObjectList.Create;
  FCursorRecBuf := nil;
  FCursorUpdate := True;
  FServerCursorUsed := False;
end;

destructor TNexusRecordSet.Destroy;
begin
  inherited;
  FMapperList.Free;
end;

procedure TNexusRecordSet.Check(ErrorCode: cardinal);
begin
  FCommand.FConnection.Check(ErrorCode, Component);
end;

function TNexusRecordSet.GetDisconnectedMode: boolean;
begin
  if (FCommand <> nil) and (FCommand.FConnection <> nil) then
    Result := FCommand.FConnection.DisconnectedMode
  else
    Result := False;
end;

procedure TNexusRecordSet.CheckActive;
begin
  if FCommand.FCursor = nil then
    FCommand.FConnection.Check(DBIERR_SERVERCOMMLOST, Component); // 'Lost connection to server during query'
end;

procedure TNexusRecordSet.FreeMappers;
begin
  FMapperFieldOffset := nil;
  FMapperList.Clear;
end;

function TNexusRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRoAfterUpdate:
      FroAfterUpdate := Value;
    prServerCursor:
      FServerCursorUsed := Value;
    prCursorUpdate:
      FCursorUpdate := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TNexusRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prServerCursor:
      Value := FServerCursorUsed;
    prCursorUpdate:
      Value := FCursorUpdate;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TNexusRecordSet.CreateCommand;
begin
  SetCommand(TNexusCommand.Create);
end;

procedure TNexusRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TNexusCommand(Value);
end;

function TNexusRecordSet.GetDatabaseCursor: TnxAbstractCursor;
begin
  CheckActive;
  Result := FCommand.FCursor;
end;

function TNexusRecordSet.NeedInitFieldsOnPrepare: boolean;
begin
  Result := inherited NeedInitFieldsOnPrepare;

  if not Result and FCommand.FConnection.FDetectFieldsOnPrepare then
    Result := FCommand.FParsedSQLType in [qtSelect, qtCursor];
end;

procedure TNexusRecordSet.InternalPrepare;
begin
  inherited;

  FCommand.Prepare;

  if (FCommand.FFieldsDescriptor <> nil) and (FCommand.FFieldsDescriptor.FieldCount > 0) then
    FCommand.CommandType := ctCursor;
end;

procedure TNexusRecordSet.Prepare;
begin
  FCommand.FRecordSetExec := True;
  try
    inherited;
  finally
    FCommand.FRecordSetExec := False;
    FCommand.FreeCursor;
  end;
end;

procedure TNexusRecordSet.InternalUnPrepare;
begin
  inherited;

  FCommand.CommandType := ctUnknown;
end;

procedure TNexusRecordSet.DetectIdentityField;
var
  FieldDesc: TCRFieldDesc;
  i: integer;
begin
  FIdentityField := nil;

  for i := 0 to FFields.Count - 1 do begin
    FieldDesc := TCRFieldDesc(FFields[i]);
    if FieldDesc.IsAutoIncrement then begin
      FIdentityField := FieldDesc;
      Exit;
    end;
  end;
end;

class function TNexusRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  case DataType of
    dtInt8, dtUInt8:
      Result := SizeOf(Byte);
  else
    Result := inherited GetBufferSize(DataType, LengthInChars);
  end;
end;

procedure TNexusRecordSet.CreateFieldDescs;

  procedure InternalCreateFieldDescs;
  var
    Field: TCRFieldDesc;
    FieldCount: integer;
    i: integer;
  begin;
    Assert(FCommand.FFieldsDescriptor <> nil);
    FreeMappers;
    FieldCount := FCommand.FFieldsDescriptor.FieldCount;
    SetLength(FMapperFieldOffset, FieldCount);

    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, FTablesInfo);

    for i := 0 to FieldCount - 1 do begin
      if not FCommand.FFieldsDescriptor.FieldDescriptor[i].fdNoClientAccess then begin
        Field := TCRFieldDesc(CreateFieldDesc);
        DescribeFieldDesc(Field, FCommand.FFieldsDescriptor.FieldDescriptor[i]);
        Field.FieldNo := FFields.Count + 1;
        Field.ActualFieldNo := i;
        FFields.Add(Field);
      end;

      FMapperFieldOffset[i] := FCommand.FFieldsDescriptor.FieldDescriptor[i].fdOffset;
    end;

{
    if FServerCursorUsed then begin
      FBookmarkField := TCRFieldDesc(GetFieldDescType.Create);
      FBookmarkField.FieldNo := FFields.Count + 1;
      FBookmarkField.ActualFieldNo := MaxWord;
      FBookmarkField.ReadOnly := True;
      FBookmarkField.Hidden := True;
      FBookmarkField.HiddenObject := True;
      FBookmarkField.DataType := dtBytes;

      if Assigned(FCommand.FTableDescriptor.IndicesDescriptor) then
        GetDatabaseCursor.GetIndexID(IndexID)
      else
        IndexID := -1;
      BookmarkSize := FCommand.FTableDescriptor.BookmarkSize[IndexID];
      FBookmarkField.Length := BookmarkSize;
      FBookmarkField.Size := BookmarkSize;

      Fields.Add(FBookmarkField);
    end;
}
  end;

var
  NeedsGet: Boolean;
begin
  if FCommand.CommandType = ctUnknown then begin // This is a FieldDefs.Update call
    try
      FCommand.BindParams(0, NeedsGet);

      FCommand.InternalExecute(omReadWrite {omReadOnly}, NeedsGet); { TODO : use omReadOnly mode }

      if FCommand.FCursor <> nil then begin
        FCommand.FnxEngine := FCommand.FConnection.nxEngine;
        FCommand.FnxEngine.RegisterClient(FCommand);
        FCommand.CommandType := ctCursor;
      end;
    finally
      FCommand.SetCursorState(csInactive); // copied from OpenCursor method to prevent blocking execute after FieldDefs.Update
    end;

    try
      if FCommand.FFieldsDescriptor <> nil then
        InternalCreateFieldDescs;
    finally
      InternalClose;
      FOriginalSQL := FCommand.SQL;
    end;
  end
  else begin
    if (FCommand.CommandType <> ctCursor) or (FCommand.FFieldsDescriptor = nil) then
      raise Exception.Create(SNotRows);

    InternalCreateFieldDescs;
  end;
end;

procedure TNexusRecordSet.DescribeFieldDesc(Field: TCRFieldDesc; nxFieldDesc: TnxFieldDescriptor);
var
  DataType: Word;
  SubDataType: Word;
  Length: Integer;
  Scale: Integer;
  Fixed: boolean;
  IsAutoIncrement: Boolean;
begin
  Field.Name := CRFunctions.UTF8Decode(AnsiString(nxFieldDesc.Name));
  Field.ActualName := CRFunctions.UTF8Decode(AnsiString(nxFieldDesc.Name));
  if FTablesInfo.Count > 0 then
    Field.TableInfo := FTablesInfo[0];

  Field.DBType := TNexusConverterManager.GetDBType(nxFieldDesc.fdType);
  Field.DBLength := nxFieldDesc.fdUnits;
  Field.DBScale := nxFieldDesc.fdDecPl;

  DetectFieldType(Field.Name, Field.DBType, Field.DBLength, Field.DBScale,
    DataType, SubDataType, Length, Scale, Fixed, IsAutoIncrement);

  case DataType of
    dtString:
      if not FLongStrings and (Length > 255) then
        DataType := dtMemo
      else
        if not FFlatBuffers and (Length >= FlatBufferLimit) then
          if Field.DBType <> nxShortString then
            DataType := dtExtString;
    dtWideString:
      if not FLongStrings and (Length > 255) then
        DataType := dtWideMemo
      else
        if not FFlatBuffers and (Length >= FlatBufferLimit) then
          DataType := dtExtWideString;
    dtBytes, dtVarBytes:
      if not FFlatBuffers and (Length >= FlatBufferLimit) then
        DataType := dtExtVarBytes;
  end;

  Field.DataType := DataType;
  Field.SubDataType := SubDataType;
  Field.Length := Length;
  Field.Scale := Scale;
  Field.Fixed := Fixed;
  Field.IsAutoIncrement := IsAutoIncrement;

  Field.Size := GetBufferSize(Field.DataType, Field.Length);

  Field.Hidden := nxFieldDesc.fdNoClientAccess;
  Field.ReadOnly := nxFieldDesc.fdNoClientAccess;
  Field.Required := nxFieldDesc.fdRequired;
end;

function TNexusRecordSet.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

procedure TNexusRecordSet.ClearFields;
begin
  FreeMappers;
  inherited;
end;

procedure TNexusRecordSet.DetectFieldType(const FieldName: string; DBType: Word; DBLength: Integer; DBScale: SmallInt;
  out DataType, SubDataType: Word; out Length, Scale: Integer; out Fixed: boolean; out IsAutoIncrement: Boolean);
{$IFNDEF LITE}
var
  FetchConverter: TFetchConverter;
{$ENDIF}
begin
{$IFNDEF LITE}
  FetchConverter := GetMapFetchConverter(FieldName, DBType, DBLength, DBScale);
  if FetchConverter <> nil then begin
    DataType := FetchConverter.InternalDataType;
    SubDataType := FCommand.GetInternalType(DBType, DBLength, DBScale);
    Length := DBLength;
    Scale := DBScale;
  end
  else
{$ENDIF}
    FCommand.DetectDataType(DBType, DBLength, DBScale, DataType, SubDataType, Length, Scale);

  Fixed := (DBType = nxChar) or (DBType=nxWideChar) or (DBType = nxGuid);
  IsAutoIncrement := (DBType = nxAutoInc);
end;


procedure TNexusRecordSet.InternalOpen(DisableInitFields: boolean = False);
begin
  if FServerCursorUsed then begin
    if CachedUpdates then
      raise Exception.Create('SCUandServerCursors');

    if GetDisconnectedMode then // Server cursor has no sence in disconnected mode
      raise Exception.Create('SDMandServerCursors');
  end;

  inherited;
end;

procedure TNexusRecordSet.InternalClose;
begin
  FCommand.FreeCursor;

  FreeMem(FCursorRecBuf);
  FCursorRecBuf := nil;
  FFetchStatus := TnxResult(-1);

  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;

  if not Prepared then
    InternalUnprepare;
end;

procedure TNexusRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0);
begin
  FCommand.FRecordSetExec := True;
  try
    if FServerCursorUsed then begin
      FFetchRows := 1;
      FFetchAll := False;
    end;

    inherited;

    if FCommand.FCursor <> nil then begin
      FCommand.CommandType := ctCursor;
      FCommand.SetCursorState(csExecuted);

      if FServerCursorUsed and (FIndexFieldNames <> '') then
        SetIndexFieldNames(FIndexFieldNames);

      Check(GetDatabaseCursor.SetToBegin);
      FFetchStatus := TnxResult(-1);
    end
    else
      FCommand.CommandType := ctStatement;

  except
    FCommand.FRecordSetExec := False;
    if Assigned(FCommand.AfterExecute) then
      FCommand.AfterExecute(False);
    raise;
  end;
  FCommand.FRecordSetExec := False;
  if Assigned(FCommand.AfterExecute) then
    FCommand.AfterExecute(True);
end;

function TNexusRecordSet.IsFullReopen: boolean;
begin
  Result := inherited IsFullReopen or not FServerCursorUsed;
end;

procedure TNexusRecordSet.Reopen;
begin
  if not IsFullReopen then begin
    FreeMem(FCursorRecBuf);
    FCursorRecBuf := nil; // for First fetch
    SetToBegin;
  end;

  inherited;
end;

procedure TNexusRecordSet.SortItems;
begin
  if FServerCursorUsed then
    Exit;

  inherited SortItems;
end;

procedure TNexusRecordSet.SetIndexFieldNames(const Value: string);
var
  IndexName: string;
  Status: TnxResult;
begin
  FIndexFieldNames := Value;

  if FServerCursorUsed then begin
    if FCommand.FCursor <> nil then begin
      FCommand.UpdateIndexDefs;
      if Value <> '' then
        IndexName := FCommand.FIndexDefs.FindIndexForFields(Value).Name
      else
        IndexName := FCommand.GetDefaultIndexName;

      Status := GetDatabaseCursor.SwitchToIndex(IndexName, -1, True);
      if (Status = DBIERR_NOCURRREC) or (Status = DBIERR_RECDELETED) or
         (Status = DBIERR_BOF) or (Status = DBIERR_EOF) then
      begin
        Status := GetDatabaseCursor.SwitchToIndex(IndexName, -1, False);
        Check(Status);
        SetToBegin;
      end
      else
        Check(Status);
    end;
  end;

  inherited SetIndexFieldNames(Value);
end;

procedure TNexusRecordSet.SetToBegin;
begin
  if FServerCursorUsed then begin
    Check(GetDatabaseCursor.SetToBegin);
    Fetch(False);
  end;

  inherited;
end;

procedure TNexusRecordSet.SetToEnd;
begin
  if FServerCursorUsed then begin
    Check(GetDatabaseCursor.SetToEnd);
    Fetch(True);
  end
  else
    FetchAll;

  inherited;
end;

procedure TNexusRecordSet.SetToBookmark(Bookmark: PRecBookmark);
begin
  if FServerCursorUsed then begin
    try
      FFetchFromBookmark := True;

      Check(GetDatabaseCursor.RecNoSupported);
      Check(GetDatabaseCursor.RecNoSet(Bookmark.Order, False));
      Fetch;

      CurrentItem := LastItem;
      Bookmark.Item := CurrentItem;
      if CurrentItem.Flag = flUsed then begin
        FBOF := False;
        FEOF := False;
      end
    finally
      FFetchFromBookmark := False;
    end;
  end
  else
    inherited;
end;

function TNexusRecordSet.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  if FServerCursorUsed then begin
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

function TNexusRecordSet.NeedGetRecordAfterGotoBookmark: boolean;
begin
  Result := FServerCursorUsed;
end;

procedure TNexusRecordSet.ReadFieldValue(Field: TCRFieldDesc; ValueBuf, DataBuf: IntPtr; DataLenPtr: PWord);
var
  Buf: IntPtr;
  sa: AnsiString;
  Len: Integer;
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
  FieldDescriptor: TnxFieldDescriptor;
begin
  case Field.DataType of
    dtBoolean:
      WordBool(DataBuf^) := TnxBoolean(ValueBuf^);

    dtUInt8:
      Byte(DataBuf^) := TnxByte8(ValueBuf^);
    dtUInt16:
      Word(DataBuf^) := TnxWord16(ValueBuf^);
    dtUInt32:
      Cardinal(DataBuf^) := TnxWord32(ValueBuf^);
    dtInt8:
      ShortInt(DataBuf^) := TnxInt8(ValueBuf^);
    dtSmallint:
      if Field.SubDataType = dtUInt8 then
        SmallInt(DataBuf^) := TnxByte8(ValueBuf^)
      else
        SmallInt(DataBuf^) := TnxInt16(ValueBuf^);
    dtInteger:
      if Field.SubDataType = dtUInt16 then
        Integer(DataBuf^) := TnxWord16(ValueBuf^)
      else
        Integer(DataBuf^) := TnxInt32(ValueBuf^);
    dtLargeint:
      if Field.SubDataType = dtUInt32 then
        Integer(DataBuf^) := TnxWord32(ValueBuf^)
      else
        Int64(DataBuf^) := TnxInt64(ValueBuf^);
    dtSingle:
      Single(DataBuf^) := TnxSingle(ValueBuf^);
    dtFloat: begin
      FieldDescriptor := FCommand.FFieldsDescriptor.FieldDescriptor[Field.ActualFieldNo];
      if FieldDescriptor.fdType = nxtExtended then
        Double(DataBuf^) := TnxExtended(ValueBuf^)
      else
        Double(DataBuf^) := TnxDouble(ValueBuf^);
    end;
    dtCurrency:
      Double(DataBuf^) := TnxCurrency(ValueBuf^);
    dtExtended:
      Extended(DataBuf^) := TnxExtended(ValueBuf^);
    dtBCD:
      Currency(DataBuf^) := TnxCurrency(ValueBuf^);
  {$IFNDEF FPC}
    dtFMTBCD: begin
      FieldDescriptor := FCommand.FFieldsDescriptor.FieldDescriptor[Field.ActualFieldNo];
      FillChar(DataBuf, SizeOfTBCD, 0);
      Move(ValueBuf^, DataBuf^, FieldDescriptor.fdLength);
    end;
  {$ENDIF}
    dtGuid: begin
      sa := AnsiString(GUIDToString(TGuid(ValueBuf^)));
      Len := Length(sa);
      Move(sa[1], DataBuf^, Len);
      Marshal.WriteByte(DataBuf, Field.Length, 0);
        DataLenPtr^ := Word(Len);
    end;
    dtString: begin
      if Field.SubDataType = dtShortStirng then begin
        Len := Byte(ValueBuf^);
        if Len > Field.Length then
          Len := Field.Length;
        if Len > 0 then
          Move(TByteArray(ValueBuf^)[1], DataBuf^,  Len);
        TByteArray(DataBuf^)[Len] := 0;
      end
      else begin
        Len := {$IFDEF VER18P}AnsiStrings.{$ENDIF}StrLen(PAnsiChar(ValueBuf));
        if Len > Field.Length then
          Len := Field.Length;
        Move(ValueBuf^, DataBuf^, Len);
        TByteArray(DataBuf^)[Len] := 0;
      end;
        DataLenPtr^ := Word(Len);
    end;
    dtWideString: begin
      Len := Length(PWideChar(ValueBuf));
      if Len > Field.Length then
        Len := Field.Length;
      if Len > 0 then
        Move(ValueBuf^, DataBuf^, Len * 2);
      TWordArray(DataBuf^)[Len] := 0;
        DataLenPtr^ := Word(Len);
    end;
    dtExtString: begin
      Len := {$IFDEF VER18P}AnsiStrings.{$ENDIF}StrLen(PAnsiChar(ValueBuf));
      if Len > Field.Length then
        Len := Field.Length;
      begin
        if Field.Fixed and TrimFixedChar then
          Marshal.WriteIntPtr(DataBuf, StringHeap.AllocTrimmedStr(ValueBuf, Len))
        else
          Marshal.WriteIntPtr(DataBuf, StringHeap.AllocStr(ValueBuf, Len));
        DataLenPtr^ := Word(Len);
      end;
    end;
    dtExtWideString: begin
      Len := Length(PWideChar(ValueBuf));
      if Len > Field.Length then
        Len := Field.Length;
      begin
        if Field.Fixed and TrimFixedChar then
          Marshal.WriteIntPtr(DataBuf, StringHeap.AllocTrimmedWideStr(ValueBuf, Len))
        else
          Marshal.WriteIntPtr(DataBuf, StringHeap.AllocWideStr(ValueBuf, Len));
        DataLenPtr^ := Word(Len);
      end;
    end;
    dtBytes, dtVarBytes: begin
      Len := Field.Length;
      Move(ValueBuf^, DataBuf^, Len);
        DataLenPtr^ := Word(Len);
    end;
    dtExtVarBytes: begin
      Len := Field.Length;
      begin
        Buf := StringHeap.NewBuf(Len);
        Marshal.WriteIntPtr(DataBuf, Buf);
        Move(ValueBuf^, Buf^, Len);
        DataLenPtr^ := Word(Len);
      end;
    end;
    dtDateTime: begin
      DateTime := TnxDateTime(ValueBuf^);
      TimeStamp := MSecsToTimeStamp(DateTime);
      Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(TimeStampToDateTime(TimeStamp)));
    end;
    dtDate: begin
      TimeStamp.Time := 0;
      TimeStamp.Date := TnxDate(ValueBuf^);
      Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(TimeStampToDateTime(TimeStamp)));
    end;
    dtTime: begin
      TimeStamp.Date := DateDelta;
      TimeStamp.Time := TnxTime(ValueBuf^);
      Marshal.WriteInt64(DataBuf, BitConverter.DoubleToInt64Bits(TimeStampToDateTime(TimeStamp)));
    end;
  else
    Assert(False);
  end;
end;

procedure TNexusRecordSet.ReadFieldBlob(Field: TCRFieldDesc; BlobNr: TnxInt64; DataBuf: IntPtr);
var
  Blob: TBlob;
  Len: TnxWord32;
  PieceLen: Integer;
  Offset: Integer;
  Piece: PPieceHeader;
  Stream: TnxStaticMemoryStream;
  cnt: cardinal;
begin
    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DataBuf)));

    if FServerCursorUsed then
      (Blob as TNexusBlob).Init(BlobNr)
    else
      if BlobNr <> 0 then begin
        Check(GetDatabaseCursor.BlobGetLength(Field.ActualFieldNo, BlobNr, Len, False));

(*    {$IFNDEF VER10P}
        if Field.DataType = dtWideMemo then begin
          Len := Len and (not 1);
          if Len > 0 then begin
            SetLength(ws, Len div 2);
            Offset := 0;
            BytesRead := 0;
            Stream := TnxStaticMemoryStream.Create(@ws[0], Len);
            try
              while Len > 0 do begin
                Check(GetDatabaseCursor.BlobRead(Field.ActualFieldNo, BlobNr, Offset, Len, Stream, False));
                BytesRead := TnxWord32(Stream.Position) - BytesRead;
                Dec(Len, BytesRead);
                Inc(Offset, BytesRead);
              end;
            finally
              Stream.Free;
            end;

            str := nxWideStringToString(ws, 0);
            Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(FieldBuf)));
            Blob.AsString := str;
          end;
        end
        else
     {$ENDIF}
*)      begin
          Offset := 0;
          while Len > 0 do begin
            if Len > UInt(DefaultPieceSize) then
              PieceLen := DefaultPieceSize
            else
              PieceLen := Len;

            Blob.AllocPiece(Piece, PieceLen);
            Blob.AppendPiece(Piece);
            Stream := TnxStaticMemoryStream.Create(PtrOffset(Piece, sizeof(TPieceHeader)), PieceLen);
            try
              while PieceLen > 0 do begin
                Check(GetDatabaseCursor.BlobRead(Field.ActualFieldNo, BlobNr, Offset, PieceLen, Stream, False));
                cnt := Stream.Position - Piece.Used;
                Assert(cnt > 0);
                Piece.Used := cardinal(Stream.Position);
                Dec(PieceLen, cnt);
                Dec(Len, cnt);
                Inc(Offset, cnt);
              end;
            finally
              Stream.Free;
            end;
          end;
        end;
      end;
end;

procedure TNexusRecordSet.ReadFieldValues(RecBuf: IntPtr; Source: PnxByteArray; Row: Integer);
var
  i: integer;
  Field: TCRFieldDesc;
  DataBuf: IntPtr;
  DataLenPtr: PWord;
  ValueBuf: IntPtr;
  BlobNr: TnxInt64;
  IsNull: Boolean;
begin
  CreateComplexFields(RecBuf, True);

  for i := 0 to FFields.Count - 1 do begin
    Field := TCRFieldDesc(FFields[i]);
    if (Field.FieldDescKind <> fdkData) or Field.HiddenObject or (Field.ActualFieldNo = -1) {KeyOnly SmartFetchState} then
      continue;

    if FCommand.FFieldsDescriptor.IsRecordFieldNull(Field.ActualFieldNo, Source) then
      Marshal.WriteByte(RecBuf, DataSize + i, 1)
    else begin
      Marshal.WriteByte(RecBuf, DataSize + i, 0);
        DataBuf := PtrOffset(RecBuf, Field.DataOffset);
        if Field.HasValueLen then
          DataLenPtr := PtrOffset(RecBuf, Field.Offset)
        else
          DataLenPtr := nil;

      if Field.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
        FCommand.FFieldsDescriptor.GetRecordField(Field.ActualFieldNo, Source, IsNull, @BlobNr);
        if IsNull then
          BlobNr := 0;
        ReadFieldBlob(Field, BlobNr, DataBuf);
      end
      else begin
        ValueBuf := PtrOffset(Source, FMapperFieldOffset[Field.ActualFieldNo]);
        ReadFieldValue(Field, ValueBuf, DataBuf, DataLenPtr);
      end;
    end;
  end;

(*  if FServerCursorUsed then begin
    Assert(FBookmarkField <> nil);
    FieldBuf := PtrOffset(RecBuf, FBookmarkField.Offset);
    Check(GetDatabaseCursor.GetBookmark(FieldBuf, FBookmarkField.Length));
    Marshal.WriteByte(RecBuf, DataSize + FBookmarkField.FieldNo - 1, 0);
  end; *)
end;

procedure TNexusRecordSet.WriteFieldValue(Field: TFieldDesc; CursorBuf, ValueBuf: IntPtr; ValueLen: Word);
var
  Len: Integer;
  Guid: TGuid;
  Bcd: TnxFmtBcd;
  DateTime: TDateTime;
  TimeStamp: TTimeStamp;
  FieldDescriptor: TnxFieldDescriptor;
begin
  case Field.DataType of
    dtBoolean:
      TnxBoolean(CursorBuf^) := WordBool(ValueBuf^);

    dtUInt8:
      TnxByte8(CursorBuf^) := Byte(ValueBuf^);
    dtUInt16:
      TnxWord16(CursorBuf^) := Word(ValueBuf^);
    dtUInt32:
      TnxWord32(CursorBuf^) := Cardinal(ValueBuf^);
    dtInt8:
      TnxInt8(CursorBuf^) := ShortInt(ValueBuf^);
    dtSmallint:
      TnxInt16(CursorBuf^) := SmallInt(ValueBuf^);
    dtInteger:
      TnxInt32(CursorBuf^) := Integer(ValueBuf^);
    dtLargeint:
      TnxInt64(CursorBuf^) := Int64(ValueBuf^);
    dtSingle:
      TnxSingle(CursorBuf^) := Single(ValueBuf^);
    dtFloat: begin
      FieldDescriptor := FCommand.FFieldsDescriptor.FieldDescriptor[Field.ActualFieldNo];
      if FieldDescriptor.fdType = nxtExtended then
        TnxExtended(CursorBuf^) := Double(ValueBuf^)
      else
        TnxDouble(CursorBuf^) := Double(ValueBuf^);
    end;
    dtCurrency:
      TnxCurrency(CursorBuf^) := Double(ValueBuf^);
    dtExtended:
      TnxExtended(CursorBuf^) := Extended(ValueBuf^);
    dtBCD:
      TnxCurrency(CursorBuf^) := Currency(ValueBuf^);
  {$IFNDEF FPC}
    dtFMTBCD: begin
      FieldDescriptor := FCommand.FFieldsDescriptor.FieldDescriptor[Field.ActualFieldNo];
      Bcd := nxNormalizeBcd(TnxFmtBcd(ValueBuf^), FieldDescriptor.fdUnits, FieldDescriptor.fdDecPl, [tiTrailing]);
      Move(Bcd, CursorBuf^, FieldDescriptor.fdLength);
    end;
  {$ENDIF}
    dtGuid: begin
      Guid := StringToGUID(string(Marshal.PtrToStringAnsi(ValueBuf, ValueLen)));
      Move(Guid, CursorBuf^, SizeOf(TGuid));
    end;
    dtString:
      if Field.SubDataType = dtSingleChar then
        if Byte(ValueBuf^) = $00 then
          Byte(CursorBuf^) := $20
        else
          Byte(CursorBuf^) := Byte(ValueBuf^)
      else if Field.SubDataType = dtShortStirng then begin
        if ValueLen > Field.Length then
          Len := Field.Length
        else
          Len := ValueLen;
        TByteArray(CursorBuf^)[0] := Len;
        if Len > 0 then
          Move(ValueBuf^, TByteArray(CursorBuf^)[1], Len);
      end
      else begin
        Move(ValueBuf^, CursorBuf^, ValueLen);
        Marshal.WriteByte(CursorBuf, ValueLen, 0);
      end;
    dtWideString:
      if Field.SubDataType = dtSingleChar then
        if Word(ValueBuf^) = $00 then
          Word(CursorBuf^) := $20
        else
          Word(CursorBuf^) := Word(ValueBuf^)
      else begin
        Len := ValueLen * 2;
        Move(ValueBuf^, CursorBuf^, Len);
        Marshal.WriteUInt16(CursorBuf, Len, 0);
      end;
    dtExtString:
      if ValueBuf <> nil then begin
        ValueBuf := PIntPtr(ValueBuf)^;
        Move(ValueBuf^, CursorBuf^, ValueLen);
        Marshal.WriteByte(CursorBuf, ValueLen, 0);
      end
      else
        Marshal.WriteByte(CursorBuf, 0);
    dtExtWideString:
      if ValueBuf <> nil then begin
        ValueBuf := PIntPtr(ValueBuf)^;
        Len := ValueLen * 2;
        Move(ValueBuf^, CursorBuf^, Len);
        Marshal.WriteUInt16(CursorBuf, Len, 0);
      end
      else
        Marshal.WriteUInt16(CursorBuf, 0);
    dtBytes:
      Move(ValueBuf^, CursorBuf^, ValueLen);
    dtExtVarBytes:
      if ValueBuf <> nil then begin
        ValueBuf := PIntPtr(ValueBuf)^;
        Move(ValueBuf^, CursorBuf^, ValueLen);
      end;
    dtDateTime: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValueBuf));
      DateTime := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
      TnxDateTime(CursorBuf^) := DateTime;
    end;
    dtDate: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValueBuf));
      TimeStamp := DateTimeToTimeStamp(DateTime);
      TnxDate(CursorBuf^) := TimeStamp.Date;
    end;
    dtTime: begin
      DateTime := BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ValueBuf));
      TimeStamp := DateTimeToTimeStamp(DateTime);
      TnxTime(CursorBuf^) := TimeStamp.Time;
    end;
  else
    Assert(False);
  end;
end;

procedure TNexusRecordSet.InitFetchedBlock(Block: PBlockHeader; RowsObtained: Integer; FetchBack: boolean);
var
  RecNo: TnxWord32;
begin
  if not FServerCursorUsed then
    inherited
  else begin
    Check(GetDatabaseCursor.RecNoGet(RecNo, False));
    LastItem.Order := RecNo;
  end;
end;

function TNexusRecordSet.FetchingAccessible(FetchBack: boolean): boolean;
begin
  CheckActive;
  Assert(FCommand.FFieldsDescriptor <> nil);
  Result := True;
end;

procedure TNexusRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  pRec: IntPtr;
  RecNo: TnxWord32;
begin
  RowsObtained := 0;

  if FServerCursorUsed and (FCursorRecBuf = nil) then begin // First fetch
    GetMem(FCursorRecBuf, FCommand.FFieldsDescriptor.RecordLength);

    Check(GetDatabaseCursor.SetToEnd);
    repeat
      FFetchStatus := GetDatabaseCursor.RecordGetPrior(nxltNoLock, FCursorRecBuf);
    until FFetchStatus <> DBIERR_NX_FilterTimeout;

    if (FFetchStatus = DBIERR_BOF) or (FFetchStatus = DBIERR_EOF) then
      FRecordCount := 0
    else begin
      Check(FFetchStatus);
      Check(GetDatabaseCursor.RecNoGet(RecNo, False));
      FRecordCount := RecNo;
    end;
    Check(GetDatabaseCursor.SetToBegin);
  end
  else
    ReallocMem(FCursorRecBuf, FCommand.FFieldsDescriptor.RecordLength);

  pRec := PtrOffset(Block, SizeOf(TBlockHeader) + SizeOf(TItemHeader));

  while True do begin
    if FFetchFromBookmark then
      FFetchStatus := GetDatabaseCursor.RecordGet(nxltNoLock, FCursorRecBuf)
    else
      repeat
        if FetchBack then
          FFetchStatus := GetDatabaseCursor.RecordGetPrior(nxltNoLock, FCursorRecBuf)
        else
          FFetchStatus := GetDatabaseCursor.RecordGetNext(nxltNoLock, FCursorRecBuf);
      until FFetchStatus <> DBIERR_NX_FilterTimeout;

    if (FFetchStatus = DBIERR_BOF) or (FFetchStatus = DBIERR_EOF) then begin
      if not FServerCursorUsed then
        FCommand.SetCursorState(csFetched);
      Break;
    end
    else
      Check(FFetchStatus);

    Inc(RowsObtained);
    ReadFieldValues(pRec, FCursorRecBuf, RowsObtained - 1);

    if RowsObtained = FFetchRows then
      break;
    pRec := PtrOffset(pRec, SizeOf(TItemHeader) + RecordSize);
  end;

  if (FCommand.GetCursorState = csFetched) or (FSmartFetchState = sfDataByKey) then begin
    FCommand.FreeCursor;
    FreeMem(FCursorRecBuf);
    FCursorRecBuf := nil;
    FCommand.SetCursorState(csFetched);
  end;
end;

function TNexusRecordSet.ProcessFetchedException(E: Exception): boolean;
begin
  FCommand.FreeCursor;
  FreeMem(FCursorRecBuf);
  FCursorRecBuf := nil;
  Result := False;
end;

function TNexusRecordSet.GetCursorRecord(NewRecord: boolean): PnxByteArray;
begin
  CheckActive;

  if NewRecord then begin
    ReallocMem(FCursorRecBuf, FCommand.FFieldsDescriptor.RecordLength);
    FCommand.FFieldsDescriptor.InitRecord(FCursorRecBuf);
  end
  else
    Check(FFetchStatus);

  Result := FCursorRecBuf;
end;

procedure TNexusRecordSet.InternalAppend(RecBuf: IntPtr);
begin
  if FServerCursorUsed and FCursorUpdate then
    InternalAppendOrUpdate(RecBuf, True)
  else
    inherited;

  if FServerCursorUsed then
    Inc(FRecordCount);
end;

procedure TNexusRecordSet.InternalUpdate(RecBuf: IntPtr);
begin
  if FServerCursorUsed and FCursorUpdate then
    InternalAppendOrUpdate(RecBuf, False)
  else
    inherited;
end;

procedure TNexusRecordSet.InternalDelete;
var
  Status: TnxResult;
begin
  if FServerCursorUsed and FCursorUpdate then begin
    GetCursorRecord(False);
    Status := GetDatabaseCursor.RecordDelete(nil);
    if ErrCat(Status) <> ERRCAT_NOTFOUND then
      Check(Status);
  end
  else
    inherited;
end;

procedure TNexusRecordSet.InternalAppendOrUpdate(RecBuf: IntPtr; const IsAppend: boolean);
var
  CursorRecord: PnxByteArray;
  Field: TFieldDesc;
  CursorBuf: IntPtr;
  ValueBuf: IntPtr;
  ValueLen: Word;
  Blob: TNexusBlob;
  BlobNr: TnxInt64;
  IsNull: Boolean;
  i: integer;
begin
  CursorRecord := GetCursorRecord(IsAppend);

  for i := 0 to FFields.Count - 1 do begin
    Field := FFields[i];
    if (Field.FieldDescKind <> fdkData) or Field.ReadOnly or Field.HiddenObject then
      continue;

    if GetNull(Field, RecBuf) then begin
      if Field.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
        Blob := TNexusBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
        if Blob.BlobNr <> 0 then begin
          GetDatabaseCursor.BlobDelete(Field.ActualFieldNo, Blob.BlobNr);
          Blob.Init(0);
        end;
      end;
      FCommand.FFieldsDescriptor.SetRecordFieldNull(Field.ActualFieldNo, CursorRecord, True);
    end
    else begin
      FCommand.FFieldsDescriptor.SetRecordFieldNull(Field.ActualFieldNo, CursorRecord, False);

      CursorBuf := PtrOffset(CursorRecord, FMapperFieldOffset[Field.ActualFieldNo]);

      ValueBuf := PtrOffset(RecBuf, Field.DataOffset);
      if Field.HasValueLen then
        ValueLen := Marshal.ReadUInt16(RecBuf, Field.Offset)
      else
        ValueLen := 0;

      if Field.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
        Blob := TNexusBlob(GetGCHandleTarget(Marshal.ReadIntPtr(ValueBuf)));
        FCommand.FFieldsDescriptor.GetRecordField(Field.ActualFieldNo, CursorRecord, IsNull, @BlobNr);
        if IsNull then
          BlobNr := 0;
        if (BlobNr = 0) and (Blob.BlobNr <> 0) then begin
          BlobNr := Blob.BlobNr;
          FCommand.FFieldsDescriptor.SetRecordField(Field.ActualFieldNo, CursorRecord, @BlobNr);
        end;
      end
      else
        WriteFieldValue(Field, CursorBuf, ValueBuf, ValueLen);
    end;
  end;

  if IsAppend then
    Check(GetDatabaseCursor.RecordInsert(nxltNoLock, CursorRecord))
  else
    Check(GetDatabaseCursor.RecordModify(CursorRecord, True));
end;

procedure TNexusRecordSet.InsertRecord(RecBuf: IntPtr);
  procedure AppItem;
  var
    Block: PBlockHeader;
    Item: PItemHeader;
  begin
    Assert(IntPtr(BlockMan.FirstBlock) = nil);

    // Nearly copied from TBlockManager.AddFreeBlock
    BlockMan.AllocBlock(Block, 1);
    Item := PtrOffset(Block, sizeof(TBlockHeader));
    Item.Prev := nil;
    Item.Next := nil;
    Item.Block := Block;
    Item.Flag := flFree;

    BlockMan.FirstFree := Item;
    Block.UsedItems := 0;
    //------------------

    CurrentItem := AppendItem;
  end;

var
  Appended: boolean;
begin
  if FServerCursorUsed and FCursorUpdate then begin
    if IntPtr(CurrentItem) = nil then begin
      if IntPtr(FirstItem) <> nil then
        CurrentItem := FirstItem
      else
      if IntPtr(LastItem) <> nil then
        CurrentItem := LastItem;
    end;

    InternalAppend(RecBuf);

    Appended := IntPtr(CurrentItem) = nil;
    if Appended then
      AppItem
    else if HasComplexFields then
      FreeComplexFields(PtrOffset(CurrentItem.Block, sizeof(TBlockHeader) + sizeof(TItemHeader)), True);

    PutRecord(RecBuf);
    ReorderItems(CurrentItem, roInsert);

    FirstItem := nil;
    CurrentItem := nil;
    LastItem := nil;

    try
      FFetchFromBookmark := True;
      Fetch;
    finally
      FFetchFromBookmark := False;
    end;
    CurrentItem := LastItem;
  end
  else
  if FServerCursorUsed and not FCursorUpdate then begin
    if FRecordCount = 0 then begin
      InternalAppend(RecBuf);

      AppItem;
      PutRecord(RecBuf);
      ReorderItems(CurrentItem, roInsert);

      CurrentItem := nil;
      FBof := False;
    end
    else begin
      InternalAppend(RecBuf);

      if HasComplexFields then
        FreeComplexFields(RecBuf, True);
      SetToEnd;
      CurrentItem := LastItem;

      FBof := IntPtr(FirstItem) = nil;
      FEof := IntPtr(LastItem) = nil;
    end;
  end
  else
    inherited;
end;

procedure TNexusRecordSet.UpdateRecord(RecBuf: IntPtr);
begin
  if FServerCursorUsed then begin
    Assert(IntPtr(CurrentItem) <> nil);
    InternalUpdate(RecBuf);

    PutRecord(RecBuf);
    if OmitRecord(CurrentItem) then
      Dec(FRecordCount);

    if FroAfterUpdate or (FIndexFieldNames <> '') then
      try
        if HasComplexFields then
          AddRefComplexFields(RecBuf);

        FFetchFromBookmark := True;
        Fetch;
      finally
        FFetchFromBookmark := False;
      end;
  end
  else
    inherited;
end;

procedure TNexusRecordSet.DeleteRecord;
begin
  if FServerCursorUsed then begin
    InternalDelete;
    RemoveRecord;

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

procedure TNexusRecordSet.CreateComplexField(RecBuf: IntPtr; Field: TFieldDesc);
var
  Blob: TBlob;
begin
  if FServerCursorUsed then begin
    case Field.DataType of
      dtBlob, dtMemo, dtWideMemo: begin
        Blob := TNexusBlob.Create(Self, Field);
        if Field.DataType = dtWideMemo then
          Blob.IsUnicode := True;
        // RollBack is always on for LOB fields. Otherwise modification
        // that cannot be canceled is possible.
        Blob.EnableRollback;
        SetBlob(Field, RecBuf, Blob);
      end;
    else
      inherited;
    end;
  end
  else
    inherited;
end;

function TNexusRecordSet.CanFetchBack: boolean;
begin
  Result := FServerCursorUsed;
end;

{ TNexusBlobData }

destructor TNexusBlobData.Destroy;
begin
  FBlobNr := 0;

  inherited;
end;

function TNexusBlobData.GetSize: Cardinal;
begin
  if FBlobNr <> 0 then
    FRecordSet.Check(FRecordSet.GetDatabaseCursor.BlobGetLength(FField.ActualFieldNo, FBlobNr, Result, False))
  else
    Result := 0;
end;

procedure TNexusBlobData.SetSize(Value: cardinal);
begin
  if GetSize > Value then
    Truncate(Value);
end;

function TNexusBlobData.Read(Position, Count: Cardinal; Dest: IntPtr): Cardinal;
var
  cnt: cardinal;
  Stream: TnxStaticMemoryStream;
begin
  Result := 0;

  if FBlobNr <> 0 then begin
    if Count = 0 then
      Count := GetSize;

    Stream := TnxStaticMemoryStream.Create(Dest, Count);
    try
      cnt := 0;
      while Count > 0 do begin
        FRecordSet.Check(FRecordSet.GetDatabaseCursor.BlobRead(FField.ActualFieldNo, FBlobNr, Position, Count, Stream, False));
        cnt := Stream.Position - cnt;
        if cnt = 0 then break;
        Dec(Count, cnt);
        Inc(Position, cnt);
        Inc(Result, cnt);
        cnt := TnxWord32(Stream.Position);
      end;
    finally
      Stream.Free;
    end;
  end;
end;

procedure TNexusBlobData.Write(Position, Count: Cardinal; Source: IntPtr);
begin
  if FBlobNr = 0 then
    FRecordSet.Check(FRecordSet.GetDatabaseCursor.BlobCreate(FField.ActualFieldNo, FBlobNr));

  if Position > GetSize then
    Position := GetSize;

  FRecordSet.Check(FRecordSet.GetDatabaseCursor.BlobWrite(FField.ActualFieldNo, FBlobNr, Position, Count, Source^));

  FIsNull := False;
end;

procedure TNexusBlobData.Truncate(NewSize: Cardinal);
begin
  if FBlobNr <> 0 then
    FRecordSet.Check(FRecordSet.GetDatabaseCursor.BlobTruncate(FField.ActualFieldNo, FBlobNr, NewSize));
end;

{ TNexusBlob }

constructor TNexusBlob.Create(RecordSet: TNexusRecordSet; Field: TFieldDesc);
begin
  inherited Create;

  TNexusBlobData(FData).FRecordSet := RecordSet;
  TNexusBlobData(FData).FField := Field;
  TNexusBlobData(FData).FBlobNr := 0;
end;

function TNexusBlob.GetBlobNr: TnxInt64;
begin
  Result := TNexusBlobData(FData).FBlobNr;
end;

function TNexusBlob.CreateBlobData: TCRBlobData;
begin
  Result := TNexusBlobData.Create;
end;

procedure TNexusBlob.SetIsUnicode(Value: boolean);
begin
  FIsUnicode := Value;
end;

function TNexusBlob.GetSizeAnsi: Cardinal;
begin
  if not IsUnicode then
    Result := GetSize
  else
    Result := GetSize div 2;
end;

procedure TNexusBlob.Init(const BlobNr: TnxInt64);
begin
  TNexusBlobData(FData).FBlobNr := BlobNr;

  if BlobNr = 0  then
    TNexusBlobData(FData).FIsNull := True
  else
    TNexusBlobData(FData).FIsNull := False;
end;

function TNexusBlob.Read(Position: cardinal; Count: cardinal; Dest: IntPtr): cardinal;
begin
  CheckValid; // DEBUG
  CheckValue;

  Result := FData.Read(Position, Count, Dest);
end;

procedure TNexusBlob.Write(Position: cardinal; Count: cardinal; Source: IntPtr);
begin
  CheckValid; // DEBUG

  FData.Write(Position, Count, Source);
end;

procedure TNexusBlob.Clear;
begin
  FData.Clear;
end;

procedure TNexusBlob.Truncate(NewSize: cardinal);
begin
  CheckValue;

  FData.Truncate(NewSize)
end;

{ TNexusMetaData }

constructor TNexusMetaData.Create;
begin
  inherited;
end;

destructor TNexusMetaData.Destroy;
begin
  inherited;
end;

function TNexusMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TNexusRecordSet.Create;
end;

function TNexusMetaData.GetConnection: TNexusConnection;
begin
  Result := TNexusRecordSet(FRecordSet).FCommand.FConnection;
end;

function TNexusMetaData.GetTables(Restrictions: TStrings): TData;
const
  dnTABLE_NAME = 3;
  dnTABLE_TYPE = 4;
var
  List: TStringList;
  TableName: string;
  i: integer;
begin
  TableName := LowerCase(Trim(Restrictions.Values['TABLE_NAME']));

  GetConnection.Connect('');
  List := TStringList.Create;
  try
    GetConnection.ChekDataBase;
    GetConnection.Check(GetConnection.nxDataBase.TableGetList(List));

    CreateTablesFields;
    FMemData.Open;
    FMemDataHelper.AllocBuffer;
    for i := 0 to List.Count - 1 do begin
      if (TableName = '') or (LowerCase(List[i]) = TableName) then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := List[i];
        FMemDataHelper.FieldValues[dnTABLE_TYPE] := 'TABLE';
        FMemDataHelper.AppendRecord;
      end;
    end;
  finally
    List.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetColumns(Restrictions: TStrings): TData;
const
  dnTABLE_NAME  = 3;
  dnCOLUMN_NAME = 4;
  dnPOSITION    = 5;
  dnDATA_TYPE   = 6;
  dnLENGTH      = 7;
  dnPRECISION   = 8;
  dnSCALE       = 9;
  dnNULLABLE    = 10;
  dnDEFAULT     = 11;
var
  Cursor: TnxAbstractCursor;
  FieldsDescriptor: TnxFieldsDescriptor;
  FieldDesc: TnxFieldDescriptor;
  TableName, ColumnName: string;
  v: Variant;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ColumnName := LowerCase(Trim(Restrictions.Values['COLUMN_NAME']));
  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  GetConnection.Connect('');
  GetConnection.ChekDataBase;
  GetConnection.Check(GetConnection.nxDataBase.CursorOpen(Cursor, TableName, '', omReadOnly,
    smShared, GetConnection.CommandTimeoutMSec));
  Assert(Cursor <> nil);
  try
    FieldsDescriptor := Cursor.TableDescriptor.FieldsDescriptor;

    CreateColumnsFields;
    FMemData.Open;
    FMemDataHelper.AllocBuffer;
    for i := 0 to FieldsDescriptor.FieldCount - 1 do begin
      FieldDesc := FieldsDescriptor.FieldDescriptor[i];
      if (ColumnName = '') or (LowerCase(FieldDesc.Name) = ColumnName) then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
        FMemDataHelper.FieldValues[dnCOLUMN_NAME] := FieldDesc.Name;
        FMemDataHelper.FieldValues[dnPOSITION] := i + 1;
        FMemDataHelper.FieldValues[dnDATA_TYPE] := GetEnumName(TypeInfo(TnxFieldType), Integer(FieldDesc.fdType));
        FMemDataHelper.FieldValues[dnLENGTH] := FieldDesc.fdLength;
        FMemDataHelper.FieldValues[dnPRECISION] := FieldDesc.fdUnits;
        FMemDataHelper.FieldValues[dnSCALE] := FieldDesc.fdDecPl;
        if FieldDesc.fdRequired then
          FMemDataHelper.FieldValues[dnNULLABLE] := 0
        else
          FMemDataHelper.FieldValues[dnNULLABLE] := 1;

        if FieldDesc.fdDefaultValue is TnxConstDefaultValueDescriptor then begin
          v := TnxConstDefaultValueDescriptor(FieldDesc.fdDefaultValue).AsVariant;
          if VarIsStr(v) then
            FMemDataHelper.FieldValues[dnDEFAULT] := AnsiQuotedStr(v, '''')
          else
            FMemDataHelper.FieldValues[dnDEFAULT] := v;
        end
        else
        if FieldDesc.fdDefaultValue is TnxCurrentDateTimeDefaultValueDescriptor then
          FMemDataHelper.FieldValues[dnDEFAULT] := CURRENT_TIME
        else
        if FieldDesc.fdDefaultValue is TnxEmptyDefaultValueDescriptor then
          FMemDataHelper.FieldValues[dnDEFAULT] := EMPTY_STRING;

        FMemDataHelper.AppendRecord;
      end;
    end;
  finally
    Cursor.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  dnPROC_NAME = 3;
  dnPROC_TYPE = 4;
var
  ProcedureName, ProcedureType: string;
begin
  ProcedureName := LowerCase(Trim(Restrictions.Values['PROCEDURE_NAME']));
  ProcedureType := LowerCase(Trim(Restrictions.Values['PROCEDURE_TYPE']));

  GetConnection.Connect('');
  CreateProceduresFields;
  FMemData.Open;

  if (ProcedureType = '') or (ProcedureType = 'procedure') then begin
    FRecordSet.SetSQL('SELECT PROCEDURE_NAME FROM #PROCEDURES');
    FRecordSet.Open;
    FMemDataHelper.AllocBuffer;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      if (ProcedureName = '') or (LowerCase(FRecordSetHelper.FieldValues[1]) = ProcedureName)
      then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnPROC_NAME] := FRecordSetHelper.FieldValues[1];
        FMemDataHelper.FieldValues[dnPROC_TYPE] := 'PROCEDURE';
        FMemDataHelper.AppendRecord;
      end;
    end;
    FRecordSet.Close;
  end;

  if (ProcedureType = '') or (ProcedureType = 'function') then begin
    FRecordSet.SetSQL('SELECT FUNCTION_NAME FROM #FUNCTIONS');
    FRecordSet.Open;
    FMemDataHelper.AllocBuffer;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      if (ProcedureName = '') or (LowerCase(FRecordSetHelper.FieldValues[1]) = ProcedureName)
      then begin
        FMemDataHelper.InitRecord;
        FMemDataHelper.FieldValues[dnPROC_NAME] := FRecordSetHelper.FieldValues[1];
        FMemDataHelper.FieldValues[dnPROC_TYPE] := 'FUNCTION';
        FMemDataHelper.AppendRecord;
      end;
    end;
    FRecordSet.Close;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
const
  SQL1 = 'SELECT * FROM #PROCEDURE_ARGUMENTS ' +
    'WHERE lower(PROCEDURE_NAME) LIKE ''%s'' AND lower(PROCEDURE_ARGUMENT_NAME) LIKE ''%s''';

  SQL2 = 'SELECT * FROM #FUNCTION_ARGUMENTS ' +
    'WHERE lower(FUNCTION_NAME) LIKE ''%s'' AND (lower(FUNCTION_ARGUMENT_NAME) LIKE ''%s'' %s)';
  SQL2_WHERE = 'OR cast(FUNCTION_ARGUMENT_NAME as ShortString) IS NULL';

  dnPROC_NAME  = 3;
  dnPARAM_NAME = 4;
  dnPOSITION   = 5;
  dnDIRECTION  = 6;
  dnDATA_TYPE  = 7;
  dnLENGTH     = 8;
  dnPRECISION  = 9;
  dnSCALE      = 10;

  snPROC_NAME  = 2;
  snPARAM_NAME = 4;
  snPOSITION   = 3;
  snDIRECTION  = 6;
  snDATA_TYPE  = 5;
  snLENGTH     = 7;
  snPRECISION  = 7;
  snSCALE      = 8;

  procedure CopyProcedureParametersData;
  begin
    FMemDataHelper.AllocBuffer;
    FRecordSetHelper.AllocBuffer;
    while FRecordSetHelper.NextRecord do begin
      FMemDataHelper.InitRecord;
      CopyRecord([snPROC_NAME, snPARAM_NAME, snPOSITION, snDIRECTION, snDATA_TYPE, snLENGTH, snPRECISION, snSCALE],
        [dnPROC_NAME, dnPARAM_NAME, dnPOSITION, dnDIRECTION, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE]);
      FMemDataHelper.AppendRecord;
    end;
  end;

var
  ProcedureName, ParameterName: string;
  SqlWhere: string;
  ProcedureCount: integer;
begin
  ProcedureName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  ParameterName := Trim(Restrictions.Values['PARAMETER_NAME']);
  if ProcedureName = '' then
    ProcedureName := '%';
  if ParameterName = '' then begin
    ParameterName := '%';
    SqlWhere := SQL2_WHERE;
  end
  else
    SqlWhere := '';

  GetConnection.Connect('');
  CreateProcedureParametersFields;
  FMemData.Open;

  FRecordSet.SetSQL(Format(SQL1, [LowerCase(ProcedureName), LowerCase(ParameterName)]));
  FRecordSet.Open;
  CopyProcedureParametersData;
  ProcedureCount := FMemData.RecordCount;
  FRecordSet.Close;

  if (ProcedureCount = 0) or (ProcedureName = '%') then begin
    FRecordSet.SetSQL(Format(SQL2, [LowerCase(ProcedureName), LowerCase(ParameterName), SqlWhere]));
    FRecordSet.Open;
    CopyProcedureParametersData;
    FRecordSet.Close;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  Cursor: TnxAbstractCursor;
  IndicesDescriptor: TnxMainIndicesDescriptor;
  TableName, IndexName: string;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := LowerCase(Trim(Restrictions.Values['INDEX_NAME']));
  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  GetConnection.Connect('');
  GetConnection.ChekDataBase;
  GetConnection.Check(GetConnection.nxDataBase.CursorOpen(Cursor, TableName, '', omReadOnly,
    smShared, GetConnection.CommandTimeoutMSec));
  Assert(Cursor <> nil);
  try
    CreateIndexesFields;
    FMemData.Open;
    IndicesDescriptor := Cursor.TableDescriptor.IndicesDescriptor;

    if Assigned(IndicesDescriptor) then begin
      FMemDataHelper.AllocBuffer;

      for i := 0 to IndicesDescriptor.IndexCount - 1 do begin
        if (IndexName = '') or (LowerCase(IndicesDescriptor.IndexDescriptor[i].Name) = IndexName) then begin
          FMemDataHelper.InitRecord;
          FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
          FMemDataHelper.FieldValues[dnINDEX_NAME] := IndicesDescriptor.IndexDescriptor[i].Name;
          if IndicesDescriptor.IndexDescriptor[i].Dups <> idAll then
            FMemDataHelper.FieldValues[dnUNIQUE] := 1
          else
            FMemDataHelper.FieldValues[dnUNIQUE] := 0;
          FMemDataHelper.AppendRecord;
        end;
      end;
    end;
  finally
    Cursor.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetIndexColumns(Restrictions: TStrings): TData;
const
  SeqAccessIndexName = 'Sequential Access Index';
const
  dnTABLE_NAME      = 3;
  dnINDEX_CATALOG   = 4;
  dnINDEX_SCHEMA    = 5;
  dnINDEX_NAME      = 6;
  dnCOLUMN_NAME     = 7;
  dnCOLUMN_POSITION = 8;
  dnSORT_ORDER      = 9;
var
  Cursor: TnxAbstractCursor;
  IndicesDescriptor: TnxMainIndicesDescriptor;
  IndexDescriptor: TnxIndexDescriptor;
  KeyDescriptor: TnxCompKeyDescriptor;
  FieldsDescriptor: TnxFieldsDescriptor;
  TableName, IndexName: string;
  i, j: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := LowerCase(Trim(Restrictions.Values['INDEX_NAME']));
  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  GetConnection.Connect('');
  GetConnection.ChekDataBase;
  GetConnection.Check(GetConnection.nxDataBase.CursorOpen(Cursor, TableName, '', omReadOnly,
    smShared, GetConnection.CommandTimeoutMSec));
  Assert(Cursor <> nil);
  try
    CreateIndexColumnsFields;
    FMemData.Open;
    IndicesDescriptor := Cursor.TableDescriptor.IndicesDescriptor;

    if Assigned(IndicesDescriptor) then begin
      FMemDataHelper.AllocBuffer;

      for i := 0 to IndicesDescriptor.IndexCount - 1 do begin
        IndexDescriptor := IndicesDescriptor.IndexDescriptor[i];
        if (IndexName = '') or (LowerCase(IndexDescriptor.Name) = IndexName) then begin
          if IndexDescriptor.KeyDescriptor is TnxCompKeyDescriptor then begin
            KeyDescriptor := TnxCompKeyDescriptor(IndexDescriptor.KeyDescriptor);
            for j := 0 to KeyDescriptor.KeyFieldCount - 1 do begin
              FMemDataHelper.InitRecord;
              FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
              FMemDataHelper.FieldValues[dnINDEX_NAME] := IndexDescriptor.Name;
              FMemDataHelper.FieldValues[dnCOLUMN_NAME] := KeyDescriptor.KeyFields[j].Field.Name;
              FMemDataHelper.FieldValues[dnCOLUMN_POSITION] := j + 1;
              if KeyDescriptor.KeyFields[j].Ascend then
                FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC'
              else
                FMemDataHelper.FieldValues[dnSORT_ORDER] := 'DESC';
              FMemDataHelper.AppendRecord;
            end;
          end
          else begin
            FMemDataHelper.InitRecord;
            FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
            FMemDataHelper.FieldValues[dnINDEX_NAME] := IndexDescriptor.Name;
            FMemDataHelper.FieldValues[dnCOLUMN_NAME] := '';
            if IndexDescriptor.Name = SeqAccessIndexName then begin
              FieldsDescriptor := Cursor.TableDescriptor.FieldsDescriptor;
              for j := 0 to FieldsDescriptor.FieldCount - 1 do
                if FieldsDescriptor.FieldDescriptor[j].fdType = nxtAutoInc then begin
                  FMemDataHelper.FieldValues[dnCOLUMN_NAME] := FieldsDescriptor.FieldDescriptor[j].Name;
                  break;
                end;
            end;

            FMemDataHelper.FieldValues[dnCOLUMN_POSITION] := 1;
            FMemDataHelper.FieldValues[dnSORT_ORDER] := 'ASC';
            FMemDataHelper.AppendRecord;
          end;
        end;
      end;
    end;
  finally
    Cursor.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetConstraints(Restrictions: TStrings): TData;
const
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
  dnINDEX_CATALOG   = 6;
  dnINDEX_SCHEMA    = 7;
  dnINDEX_NAME      = 8;
var
  Cursor: TnxAbstractCursor;
  TableName, ConstraintName: string;
  List: TStringList;
  DictionaryItem: TnxDictionaryItem;
  i: integer;
begin
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := LowerCase(Trim(Restrictions.Values['CONSTRAINT_NAME']));
  if TableName = '' then
    raise Exception.CreateFmt(SRestrictionMustBeSet, ['TABLE_NAME']);

  GetConnection.Connect('');
  GetConnection.ChekDataBase;
  GetConnection.Check(GetConnection.nxDataBase.CursorOpen(Cursor, TableName, '', omReadOnly,
    smShared, GetConnection.CommandTimeoutMSec));
  Assert(Cursor <> nil);
  try
    List := TStringList.Create;
    try
      Cursor.TableDescriptor.GetConstraints(List);

      CreateConstraintsFields;
      FMemData.Open;
      FMemDataHelper.AllocBuffer;
      for i := 0 to List.Count - 1 do begin
        if (ConstraintName = '') or (LowerCase(List[i]) = ConstraintName) then begin
          FMemDataHelper.InitRecord;
          FMemDataHelper.FieldValues[dnTABLE_NAME] := TableName;
          FMemDataHelper.FieldValues[dnCONSTRAINT_NAME] := List[i];
          DictionaryItem := Cursor.TableDescriptor.GetConstraintByName(List[i]);
          if DictionaryItem is TnxIndexDescriptor then begin
            FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'INDEX';
            FMemDataHelper.FieldValues[dnINDEX_NAME] := TnxIndexDescriptor(DictionaryItem).Name;
          end;
          FMemDataHelper.AppendRecord;
        end;
      end;
    finally
      List.Free;
    end;
  finally
    Cursor.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

function TNexusMetaData.GetDatabases(Restrictions: TStrings): TData;
const
  dnDB_NAME = 1;
var
  List: TObjectList;
  i: integer;
begin
  GetConnection.Connect('');
  List := TObjectList.Create;
  try
    GetConnection.Check(GetConnection.nxEngine.Session.AliasGetList(List));

    CreateDatabasesFields;
    FMemData.Open;
    FMemDataHelper.AllocBuffer;
    for i := 0 to List.Count - 1 do begin
      FMemDataHelper.InitRecord;
      FMemDataHelper.FieldValues[dnDB_NAME] := TnxAliasDescriptor(List.Items[i]).adAlias;
      FMemDataHelper.AppendRecord;
    end;
  finally
    List.Free;
  end;

  FMemData.SetToBegin;
  Result := FMemData;
end;

{$IFNDEF LITE}

{ TNexusLoader }

constructor TNexusLoader.Create;
begin
  inherited;

  FDirectLoad := True;
  FStream := nil;
  FCursor := nil;
  FBuffer := nil;
end;

destructor TNexusLoader.Destroy;
begin
  inherited;
end;

function TNexusLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectLoad:
      FDirectLoad := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TNexusLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirectLoad:
      Value := FDirectLoad;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TNexusLoader.CreateCommand;
begin
  FCommand := TNexusCommand.Create;
end;

function TNexusLoader.IsBlobField(FieldType: TnxFieldType): boolean;
begin
  Result := FieldType in [nxtBlob, nxtBlobMemo, nxtBlobGraphic, nxtBlobWideMemo];
end;

procedure TNexusLoader.LoadStream;
var
  Error: TnxResult;
begin
  FStream.Position := 0;
  Error := FCursor.RecordInsertBatch(FStream);
  try
    TNexusConnection(FConnection).Check(Error);
  except
    if FTransaction <> nil then
      FTransaction.Rollback;
    raise;
  end;

  ResetLoader;
end;

procedure TNexusLoader.ResetBuffers;
var
  i: integer;
begin
  FillChar(FBuffer, FRecordSize, 0);

  for i := 0 to Length(FBlobStreams) - 1 do
    if FBlobStreams[i] <> nil then begin
      FBlobStreams[i].Size := 0;
      FBlobStreams[i].Position := 0;
    end;
end;

procedure TNexusLoader.ResetLoader;
begin
  FStream.Capacity := csNexusLoaderBlockSize;
  FStream.Size := 0;
  FStream.Position := 0;
  FStartOffset := 0;
end;

procedure TNexusLoader.InitLoader;
var
  i: integer;
  CursorID: TnxCursorID;
begin
  if FStream.Size = 0 then begin
    CursorID := 0;
    FStream.Write(CursorID, SizeOf(CursorID));
    i := 0;
    FStream.Write(i, SizeOf(i));

    FStartOffset := FStream.Position;
  end;
end;

procedure TNexusLoader.DoLoadRow;
var
  i, j: Cardinal;
  Offset, BlobOffset: UInt32;
begin
  if FDirectLoad then begin
    InitLoader;

    FStream.Position := SizeOf(TnxCursorID);
    FStream.Read(i, SizeOf(i));
    Inc(i);
    FStream.Position := SizeOf(TnxCursorID);
    FStream.Write(i, SizeOf(i));
    FStream.Position := FStream.Size;

    j := FRecordSize;
    for i:= 0 to Length(FBlobStreams) - 1 do
      if FBlobStreams[i] <> nil then
        j := j + FBlobStreams[i].Size;
    FStream.Write(j, SizeOf(j));

    Offset := FStream.Position;

    FStream.Write(FBuffer^, FRecordSize);
    for i:= 0 to Length(FBlobStreams) - 1 do begin
      if FBlobStreams[i] <> nil then begin
        BlobOffset := FStream.Size;
        FFieldDescriptor.SetRecordField(i, PnxByteArray(UInt32(FStream.Memory) + Offset), @BlobOffset);
        FStream.CopyFrom(FBlobStreams[i], 0);
      end;
    end;

    ResetBuffers;

    if FStream.Size > csNexusLoaderBlockSize then
      LoadStream;
  end
  else
    inherited;
end;

procedure TNexusLoader.DoPrepare;
var
  i: integer;
begin
  if FDirectLoad then begin
    Assert(FStream = nil);

    TNexusConnection(FConnection).ChekDataBase;
    if FTransaction <> nil then
      FTransaction.StartTransaction;

    FStream := TnxMemoryStream.Create;
    TNexusConnection(FConnection).nxDataBase.CursorOpen(FCursor, TableName, '',
      omWriteOnly, smShared, TNexusConnection(FConnection).CommandTimeoutMSec, False);
    FFieldDescriptor := FCursor.TableDescriptor.FieldsDescriptor;
    FRecordSize := FFieldDescriptor.RecordLength;
    FBuffer := Marshal.AllocHGlobal(FRecordSize);
    FillChar(FBuffer, FRecordSize, 0);

    SetLength(FBlobStreams, FFieldDescriptor.FieldCount);
    for i := 0 to FFieldDescriptor.FieldCount - 1 do
      if IsBlobField(FFieldDescriptor.FieldDescriptor[i].fdType) then
        FBlobStreams[i] := TnxMemoryStream.Create
      else
        FBlobStreams[i] := nil;

    ResetLoader;
    ResetBuffers;
  end
  else
    inherited;
end;

procedure TNexusLoader.Finish;
var
  i: integer;
begin
  if FDirectLoad then begin
    Assert(FStream <> nil);

    // we should not do commit if we executed rollback before
    if (FTransaction <> nil) and FTransaction.GetInTransaction then
      FTransaction.Commit;

    for i := 0 to Length(FBlobStreams) - 1 do
      if FBlobStreams[i] <> nil then
        FBlobStreams[i].Destroy;
    SetLength(FBlobStreams, 0);

    Marshal.FreeHGlobal(FBuffer);
    FFieldDescriptor := nil;
    FRecordSize := 0;
    FreeAndNil(FCursor);
    FreeAndNil(FStream);
  end
  else
    inherited;
end;

procedure TNexusLoader.DoLoad;
begin
  inherited;

  if FDirectLoad and (FStream.Size > 0) then
    LoadStream;
end;

function TNexusLoader.IsPutColumnDataAllowed: boolean;
begin
  if FDirectLoad then
    Result := True
  else
    Result := inherited IsPutColumnDataAllowed;
end;

procedure TNexusLoader.DoPutColumnData(Col: integer; Row: integer; const Value: variant);
var
  FieldIndex: integer;
  FieldType: TnxFieldType;
  Str: AnsiString;
  ws: WideString;
  Blob: TBlob;
  SafeArray: PVarArray;
  Len: integer;
begin
  if FDirectLoad then begin
    FieldIndex := FFieldDescriptor.GetFieldFromName(Columns[Col].Name);
    if FieldIndex = -1 then
      raise Exception.CreateFmt('Invalid field name %s', [Columns[Col].Name]);

    FieldType := FFieldDescriptor.FieldDescriptor[FieldIndex].fdType;
    if IsBlobField(FieldType) then begin
      if VarType(Value) = varArray + varByte then begin
        SafeArray := VarArrayAsPSafeArray(Value);
        Len := SafeArray.Bounds[0].ElementCount;
        FBlobStreams[FieldIndex].WriteBuffer(Len, 4);
        FBlobStreams[FieldIndex].WriteBuffer(SafeArray.Data^, Len);
      end
      else
      if (VarType(Value) = varSharedObject) then begin
        Blob := TVarData(Value).VPointer;
        Blob.Defrag;
        if FieldType = nxtBlobWideMemo then begin
          ws := Blob.AsWideString;
          Len := Length(ws) * 2;
          FBlobStreams[FieldIndex].WriteBuffer(Len, 4);
          FBlobStreams[FieldIndex].WriteBuffer(pointer(ws)^, Len);
        end
        else begin
          Str := Blob.AsAnsiString;
          Len := Length(Str);
          FBlobStreams[FieldIndex].WriteBuffer(Len, 4);
          FBlobStreams[FieldIndex].WriteBuffer(pointer(Str)^, Len);
        end;
      end
      else
      if FieldType = nxtBlobWideMemo then begin
        ws := VarToWideStr(Value);
        Len := Length(ws) * 2;
        FBlobStreams[FieldIndex].WriteBuffer(Len, 4);
        FBlobStreams[FieldIndex].WriteBuffer(pointer(ws)^, Len);
      end
      else begin
        Str := AnsiString(VarToStr(Value));
        Len := Length(Str);
        FBlobStreams[FieldIndex].WriteBuffer(Len, 4);
        FBlobStreams[FieldIndex].WriteBuffer(pointer(Str)^, Len);
      end;
    end
    else
      if nxVarIsEmpty(Value) then
        FFieldDescriptor.SetRecordFieldNull(FieldIndex, FBuffer, True)
      else begin
        FFieldDescriptor.SetRecordFieldNull(FieldIndex, FBuffer, False);
        with FFieldDescriptor.FieldDescriptor[FieldIndex] do
          VariantToNative(fdType, UsedStorageCodePage, fdUnits, fdDecPl, Value, @PnxByteArray(FBuffer)[fdOffset], fdLength);
      end;
  end
  else
    inherited;
end;

{$ENDIF} // LITE

{ TNexusUtils }

class function TNexusUtils.GetServerEngine(Obj: TNexusConnection): TnxBaseServerEngine;
begin
  Result := Obj.FnxEngine.FServerEngine;
end;

{$ENDIF} // DUMMY

end.
