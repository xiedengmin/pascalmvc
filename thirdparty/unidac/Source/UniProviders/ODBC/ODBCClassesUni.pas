
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I ODBCDac.inc}
unit ODBCClassesUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Variants, SyncObjs, FMTBcd,
{$IFDEF VER12P}{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}{$ENDIF}
{$IFDEF NEXTGEN}
  Generics.Collections,
{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRAccess, CRParser, CRDataTypeMap, MemData,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCError, ODBCParser;
{$ELSE}
  ODBCCallUni, ODBCErrorUni, ODBCParserUni;
{$ENDIF}

const
  dtBlobLocator   = 100;
  dtClobLocator   = 101;
  dtDBClobLocator = 102;
  dtNumeric       = 103; // SubDataType for NUMERIC & DECIMAL

  dsMaxStringSize = 8192;
  DefaultStrFieldLength = dsMaxStringSize div 2 - 1;
  DefaultStrParamSize = 4000;

type
  TDSNType = (ntAuto, ntName, ntFile, ntConnectionString);

  TODBCRecordSet = class;
  TODBCMetaDataRecordSet = class;

{ TODBCConnection }

  TODBCConnection = class(TCRConnection)
  private
    FODBCEnv: IODBCEnvironment;
    FSQLHDbc: TSQLHDbc;
    FUseUnicode: boolean;
    FConnectionTimeout: integer;
    FDSNType: TDSNType;
    FDriverODBCVer: word;
    FIdentCase: TIdentCase;
    FDetectFieldsOnPrepare: boolean;
    FVarBinaryAsBlob: boolean;
    FLongVarBinaryAsBlob: boolean;
    FColumnWiseBinding: boolean;

    function GetDriverInfo(InfoType: word): string;
    procedure ObtainDriverInfo;

  protected
    FCachedCatalog: string;
    FCachedSchema: string;

    function GetCli: TODBCCliAPI;
    function GetODBCEnv: IODBCEnvironment; virtual;

    function GetMetaDataRecordSet: TODBCMetaDataRecordSet; virtual;

    function CreateSQLInfo: TSQLInfo; override;
    function GetConnectionString: string; virtual;
    procedure ApplyConnectProps; virtual;
    function IsCommentAllowed: boolean; virtual;
    function IsEmptyStringAllowed: boolean; virtual;
    function IsBlockFetchAllowed: boolean; virtual;
    function IsReturnValueAllowed: boolean; virtual;
    function OutParamIsInOut: boolean; virtual;
    function IsDriverPresent(const Name: string): boolean;

    property EnableBCD;
    property EnableFMTBCD;

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

    procedure Check(ErrorCode: smallint);
    procedure ProcessError(HandleType: smallint; Handle: TSQLHandle; ErrorCode: smallint; Component: TObject); virtual;

    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;
    procedure Ping; override;
    function CheckIsValid: boolean; override;

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function GetClientVersion: string; override;
    function GetDriverODBCVersion: string;

    function CanChangeDatabase: boolean; override;
    function GetCurrentCatalog: string; virtual;
    function GetCachedCatalog: string; virtual;
    function GetCurrentSchema: string; virtual;
    function GetCachedSchema: string; virtual;

    property Cli: TODBCCliAPI read GetCli;
    property SQLHDbc: TSQLHDbc read FSQLHDbc;
    property ODBCEnv: IODBCEnvironment read FODBCEnv;
  end;

{ TODBCTransaction }

  TODBCTransaction = class(TCRTransaction)
  public
    procedure StartTransaction; override;
    procedure Commit; override;
    procedure Rollback; override;
    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

{ TODBCParamDesc }

  TODBCParamDesc = class (TParamDesc)
  private
    FBuffer: IntPtr;
    FBufferSize: integer;
    FBufferAllocated: Boolean;
    FIndicator: IntPtr;

  protected
    FEnableMSec: boolean;

    procedure AllocBindBuffer(Iters: Integer);
    procedure FreeBuffer; override;

    procedure WriteBuffer(Iters, Offset: integer; AllowDataAtExec, AllowEmptyString: boolean; var DataAtExec: boolean;
      var Precision, Scale: integer);
    function WriteAnsiString(Buf: IntPtr; const Value: AnsiString): integer;
    function WritePAnsiString(Buf: IntPtr; pValue: PAnsiChar; Len: Integer): integer;
    function WriteWideString(Buf: IntPtr; const Value: WideString): integer;
    function WritePWideString(Buf: IntPtr; pValue: PWideChar; Len: Integer): integer;
    function WriteBytes(Buf: IntPtr; const Value: variant): integer;
    procedure ReadBuffer(Iters, Offset: integer);
    function ReadAnsiString(Buf: IntPtr; Len: integer): AnsiString;
    function ReadPAnsiString(Buf: IntPtr; Len: integer): AnsiString;
    function ReadPUtf8Bytes(Buf: IntPtr; Len: integer): TBytes;
    function ReadWideString(Buf: IntPtr; Len: integer): WideString;
    function ReadPWideString(Buf: IntPtr; Len: integer): WideString;
    function ReadBytes(Buf: IntPtr; Len: integer): variant;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetMinDefaultSize: Integer; override;

    procedure SetDataType(Value: word); override;
    procedure SetParamType(Value: TParamDirection); override;
    procedure SetSize(Value: integer); override;

    function GetBlob(Index: integer): TBlob;
  end;

{ TODBCSQLInfo }

  TODBCSQLInfo = class(TSQLInfo)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TODBCConnection;
  public
    constructor Create(ParserClass: TSQLParserClass); override;
    function IdentCase: TIdentCase; override;
  end;

{ TODBCFieldDesc }

  TODBCFieldDesc = class (TCRFieldDesc)
  private
    FFetchFieldOffset: Integer;
    FFetchFieldSize: Integer;
    FBindFieldSize: Integer;
    FIsConverted: boolean;
  protected
    procedure SetDataType(Value: Word); override;
  public
    property IsConverted: boolean read FIsConverted;
    property FetchFieldOffset: Integer read FFetchFieldOffset write FFetchFieldOffset;
    property FetchFieldSize: Integer read FFetchFieldSize write FFetchFieldSize;
    property BindFieldSize: Integer read FBindFieldSize write FBindFieldSize;
  end;

{ TODBCCommand }

  TODBCCommand = class(TCRCommand)
  private
    FCursorState: TCursorState;
    FRowsAffected: integer;
    FRecordSetExec: boolean;
    FCommandTimeout: integer;

    FBatchSupported: boolean;
    FParamsProcessedType: NativeInt;
    FParamsProcessedArray: array of integer;

    FDataAtExecParams: array of integer;
    FLockReadOutParams: boolean;

    procedure BindParams;
    procedure ReadOutParams;
    function WriteBlobs: integer;
    procedure WriteBlob(const Value: variant; DataType: word);
    function ReadBlob(Blob: TBlob; ActualFieldNo: integer; DataType: word; var SharedPiece: PPieceHeader): boolean;
    function RemoveComments(const Value: string): string;

  protected
    FStmt: TSQLHStmt;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TODBCConnection;

    function Cli: TODBCCliAPI;
    procedure AllocStatement; virtual;
    procedure FreeStatement;
    procedure Check(ErrorCode: smallint);

    procedure DetectCommandType; virtual;
    procedure InternalPrepare; virtual;
    procedure InternalExecute; virtual;
    function GetODBCParamType(ParamType: TParamDirection): smallint;
    function GetCDataType(DataType, SubDataType: word; NumericAsString: Boolean): smallint; virtual;
    function GetSQLDataType(DataType, SubDataType: word): smallint; virtual;
    procedure SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer); virtual;
    function GetBlobSize(SQLLength: Cardinal): Integer; virtual;
    function GetInternalType(SQLDataType: smallint; Unsigned: boolean): Word; virtual;
    procedure DetectDataType(SQLDataType: smallint; SQLLength: Cardinal; SQLScale: smallint;
      FetchConverter: TFetchConverter; out DataType, SubDataType: word; out Length, Scale: integer;
      out Fixed: boolean; Unsigned, LongStrings, FlatBuffers, FieldsAsString, UnknownAsString: boolean); virtual;
    function DetectSpecificType(const TypeName: string; var SQLLength: Integer; var SQLScale: Smallint): integer; virtual;

    procedure CreateBatchCommand; override;
    procedure InternalExecuteBatch(Iters, Offset: integer); override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure SetConnection(Value: TCRConnection); override;

    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;
    class function GetParamDescClass: TParamDescClass; override;

    procedure Prepare; override;
    procedure Unprepare; override;
    function GetPrepared: boolean; override;
    procedure Execute; override;
    procedure Close; override;
    procedure InitProcParams(const Name: string);
    function CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string; override;
    function CheckMoreResults: boolean;

    function GetCursorState: TCursorState; override;
    procedure SetCursorState(Value: TCursorState); override;

    property EnableBCD;
    property EnableFMTBCD;

    property SQLHStmt: TSQLHStmt read FStmt;
  end;

{ TODBCRecordSet }

  TODBCFetchMode = record
    HasLobs: boolean;
    OneByOne: boolean;
    ByColumns: boolean;
  end;

  TODBCRecordSet = class(TCRRecordSet)
  private
    FRowsObtainedBuf: IntPtr;
    FHasConvertedFields: boolean;
    FFetchMode: TODBCFetchMode;
    FFetchBufferDataSize: Integer;
    FFetchBufferRowSize: Integer;
    FFetchBufferIsBinded: Boolean;
    FFieldsAsString: boolean;
    FUnknownAsString: boolean;
    FHasMoreResults: boolean;
    FOpenNext: Boolean;

    function Cli: TODBCCliAPI;
    procedure DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
    function  BindColumnsForBlockFetch: Smallint;
    procedure BindColumns(RecBuf: IntPtr; FetchRows: integer);

  protected
    FCommand: TODBCCommand;

    procedure Check(ErrorCode: smallint);
    procedure CreateCommand; override;
    procedure SetCommand(Value: TCRCommand); override;
    procedure InternalPrepare; override;
    procedure InternalUnPrepare; override;
    procedure CreateFieldDescs; override;
    procedure InternalClose; override;
    function GetIndicatorItemSize: Integer; override;
    function IdentityFieldIsData: boolean; override;

    function ExtFieldsInfoIsInternal: boolean; override;
    procedure RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo); override;
    class function IsConvertedFieldType(DataType: word): boolean;

    function NumericAsString: Boolean; virtual;
    function GetFieldFetchBlockSize(Field: TFieldDesc): integer; virtual;
    function GetFieldBindSize(Field: TFieldDesc): integer; virtual;
    procedure ReadFetchBlock(FetchBuffer: IntPtr; RecBuf: IntPtr; RowsObtained: Integer; var SharedPiece: PPieceHeader);
    function ReadFetchBlockField(Field: TFieldDesc; SourcePtr: IntPtr; SourceSize: Integer; DestPtr: IntPtr; DestLenPtr: PWord; var SharedPiece: PPieceHeader): boolean; virtual;

    procedure InitBlock(Block: PBlockHeader); override;
    procedure AllocFetchBuffer; override;
    procedure FreeFetchBuffer; override;
    procedure FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer); override;
    function NeedUnPrepareAfterFetch: boolean; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    function GetFieldDescType: TFieldDescClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    class function GetBufferSize(DataType: Word; LengthInChars: integer): integer; override;

    function CheckNextResult: Boolean;
    procedure ExecCommand(Iters: integer = 1; Offset: integer = 0); override;
    procedure SetToEnd; override;

    function GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean; override;
    procedure SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean); override;
  end;

{ TODBCMetaDataCommand }

  TODBCMetaDataKind = (mkTables, mkColumns, mkProcedures, mkProcedureColumns,
    mkStatistics, mkSpecialColumns, mkTypeInfo, mkPrimaryKey, mkForeignKeys);

  TMetaDataArgs = class
  public
    CatalogName: string;
    SchemaName: string;
    ObjectName: string;
    ObjectType: string;
    ColumnName: string;
    Param1, Param2, Param3: smallint;
  end;

  TODBCMetaDataCommand = class(TODBCCommand)
  private
    FMetaDataKind: TODBCMetaDataKind;
    FMetaDataArgs: TMetaDataArgs;

  protected
    procedure DetectCommandType; override;
    procedure InternalPrepare; override;
    procedure InternalExecute; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property MetaDataKind: TODBCMetaDataKind read FMetaDataKind write FMetaDataKind;
    property MetaDataArgs: TMetaDataArgs read FMetaDataArgs;
  end;

{ TODBCMetaDataRecordSet }

  TODBCMetaDataRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{ TODBCMetaData }

  TODBCMetaData = class (TCRMetaData)
  protected
    function CreateRecordSet: TCRRecordSet; override;

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    procedure InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string); override;

    function GetTables(Restrictions: TStrings): TData; override;
    procedure CopyTablesData(Restrictions: TStrings); virtual;

    function GetColumns(Restrictions: TStrings): TData; override;
    procedure CopyColumnsData(Restrictions: TStrings); virtual;

    function GetProcedures(Restrictions: TStrings): TData; override;
    procedure CopyProceduresData(Restrictions: TStrings); virtual;

    function GetProcedureParameters(Restrictions: TStrings): TData; override;
    procedure CopyProcedureParametersData(Restrictions: TStrings); virtual;

    function GetIndexes(Restrictions: TStrings): TData; override;
    procedure CopyIndexesData(Restrictions: TStrings); virtual;

    function GetIndexColumns(Restrictions: TStrings): TData; override;
    procedure CopyIndexColumnsData(Restrictions: TStrings); virtual;

    function GetConstraints(Restrictions: TStrings): TData; override;
    procedure CreateConstraintsFields; override;
    procedure CopyConstraintPKData(Restrictions: TStrings; ListConstraintsName: TStringList);
    procedure CopyConstraintFKData(Restrictions: TStrings; ListConstraintsName: TStringList);
    procedure CopyConstraintUNData(Restrictions: TStrings; ListConstraintsName: TStringList);

    function GetConstraintColumns(Restrictions: TStrings): TData; override;
    procedure CreateConstraintColumnsFields; override;
    procedure CopyConstraintColumnsPKData(Restrictions: TStrings; ListConstraintsName: TStringList);
    procedure CopyConstraintColumnsFKData(Restrictions: TStrings; ListConstraintsName: TStringList);
    procedure CopyConstraintColumnsUNData(Restrictions: TStrings; ListConstraintsName: TStringList);

    function GetSpecialColumns(Restrictions: TStrings): TData;
    procedure CreateSpecialColumnsFields;
    procedure CopySpecialColumnsData(Restrictions: TStrings);

    function GetTypeInfo: TData;
    procedure CreateTypeInfoFields;
    procedure CopyTypeInfoData;

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$IFNDEF LITE}
  TODBCLoader = class (TCRSimpleLoader)
  protected
    procedure CreateCommand; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;
{$ENDIF}

implementation

uses
  Math, StrUtils, CRProps, MemUtils, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCProps, ODBCDataTypeMap;
{$ELSE}
  ODBCConstsUni, ODBCPropsUni, ODBCDataTypeMapUni;
{$ENDIF}

const
  BCDStrSize = 40;
  GuidStrSize = 38;

var
  LocaleDecSeparator: char;

{ TODBCConnection }

constructor TODBCConnection.Create;
begin
  inherited;

  FDetectFieldsOnPrepare := True;
  FColumnWiseBinding := False;
end;

destructor TODBCConnection.Destroy;
begin
  Disconnect;

  FODBCEnv := nil;

  inherited;
end;

function TODBCConnection.GetDriverInfo(InfoType: word): string;
const
  BufSize = 256;
var
  Buf: IntPtr;
  Len: smallint;
begin
  Buf := Marshal.AllocHGlobal(BufSize * ODBCCharSize);
  try
  {$IFDEF USE_UNICODE_DRIVER}
    Check(Cli.SQLGetInfoW(FSQLHDbc, InfoType, Buf, BufSize, Len));
  {$ELSE}
    Check(Cli.SQLGetInfoA(FSQLHDbc, InfoType, Buf, BufSize, Len));
  {$ENDIF}
    Result := FromODBCString(Buf);
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

procedure TODBCConnection.ObtainDriverInfo;
var
  DrvODBCVer: string;
  p, Min, Maj: integer;
  ValueNInt: NativeInt;
begin
  DrvODBCVer := GetDriverODBCVersion;
  p := Pos('.', DrvODBCVer);
  if p > 0 then begin
    Maj := StrToIntDef(Copy(DrvODBCVer, 1, p - 1), 0);
    Min := StrToIntDef(Copy(DrvODBCVer, p + 1, MaxInt), 0);
  end
  else begin
    Maj := StrToIntDef(DrvODBCVer, 0);
    Min := 0;
  end;
  FDriverODBCVer := Maj shl 8 or Min;

  Check(Cli.SQLGetInfoInt(FSQLHDbc, SQL_IDENTIFIER_CASE, ValueNInt, 4, nil));
  case Word(ValueNInt) of
    SQL_IC_UPPER:
      FIdentCase := icUpper;
    SQL_IC_LOWER:
      FIdentCase := icLower;
    SQL_IC_MIXED:
      FIdentCase := icMixed;
    SQL_IC_SENSITIVE:
      FIdentCase := icMixedCaseSensitive;
  else
    FIdentCase := icMixed;
  end;
end;

function TODBCConnection.GetCli: TODBCCliAPI;
begin
  Result := FODBCEnv.Cli;
end;

function TODBCConnection.GetODBCEnv: IODBCEnvironment;
begin
  Result := GetODBCEnvironment;
end;

function TODBCConnection.GetMetaDataRecordSet: TODBCMetaDataRecordSet;
begin
  Result := TODBCMetaDataRecordSet.Create;
  Result.SetConnection(Self);
end;

function TODBCConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prUseUnicode:
      FUseUnicode := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prDSNType:
      FDSNType := TDSNType(Value);
    prDetectFieldsOnPrepare:
      FDetectFieldsOnPrepare := Value;
    prVarBinaryAsBlob:
      FVarBinaryAsBlob := Value;
    prLongVarBinaryAsBlob:
      FLongVarBinaryAsBlob := Value;
    prColumnWiseBinding:
      FColumnWiseBinding := Value;
    prDriverManager:
      ODBCDLL := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prMaxStringSize:
      Value := DefaultStrParamSize;
    prUseUnicode:
      Value := FUseUnicode;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prDSNType:
      Value := Variant(FDSNType);
    prDetectFieldsOnPrepare:
      Value := FDetectFieldsOnPrepare;
    prVarBinaryAsBlob:
      Value := FVarBinaryAsBlob;
    prLongVarBinaryAsBlob:
      Value := FLongVarBinaryAsBlob;
    prColumnWiseBinding:
      Value := FColumnWiseBinding;
    prDriverManager:
      Value := ODBCDLL;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TODBCConnection.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    ProcessError(SQL_HANDLE_DBC, FSQLHDbc, ErrorCode, Component);
end;

procedure TODBCConnection.ProcessError(HandleType: smallint; Handle: TSQLHandle;
  ErrorCode: smallint; Component: TObject);
var
  Error: EODBCError;
  Fail, NeedFreeError: boolean;
begin
  NeedFreeError := True;
  Error := Cli.GetODBCError(HandleType, Handle, ErrorCode);
  try
  {$IFNDEF LITE}
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

procedure TODBCConnection.Connect(const ConnectString: string);
var
  ConStr: string;
{$IFNDEF USE_UNICODE_DRIVER}
  ConStrA: AnsiString;
{$ENDIF}
  Len: smallint;
begin
  if FConnected then
    Exit;

  if FODBCEnv = nil then
    FODBCEnv := GetODBCEnv;

  FODBCEnv.InitEnvironment;

  Check(Cli.SQLAllocHandle(SQL_HANDLE_DBC, Cli.SQLHEnv, FSQLHDbc));
  try
    if FConnectionTimeout <> 0 then
      Check(Cli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_LOGIN_TIMEOUT, FConnectionTimeout, 0));

    ConStr := GetConnectionString;

  {$IFDEF USE_UNICODE_DRIVER}
    Check(Cli.SQLDriverConnectW(FSQLHDbc, 0, PWideChar(ConStr), Length(ConStr),
      nil, 0, Len, SQL_DRIVER_NOPROMPT));
  {$ELSE}
    ConStrA := ToODBCString(ConStr);
    Check(Cli.SQLDriverConnectA(FSQLHDbc, 0, PAnsiChar(ConStrA), Length(ConStrA),
      nil, 0, Len, SQL_DRIVER_NOPROMPT));
  {$ENDIF}

    FConnected := True;
    FNativeConnection := True;

    ObtainDriverInfo;

    ApplyConnectProps;

    inherited;

  except
    on EFailOver do;
    else begin
      if FSQLHDbc <> nil then begin
        Cli.SQLFreeHandle(SQL_HANDLE_DBC, FSQLHDbc);
        FSQLHDbc := nil;
      end;
      FConnected := False;
      raise;
    end;
  end;
end;

procedure TODBCConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FConnected := False;
  try
    if FNativeConnection then
      try
        Check(Cli.SQLDisconnect(FSQLHDbc));
      finally
        Cli.SQLFreeHandle(SQL_HANDLE_DBC, FSQLHDbc);
      end;
  finally
    FSQLHDbc := nil;
    FNativeConnection := True;
    FCachedCatalog := '';
    FCachedSchema := '';
  end;

  inherited;
end;

procedure TODBCConnection.Ping;
var
  Command: TODBCCommand;
begin
  Command := TODBCCommand.Create;
  try
    Command.SetConnection(Self);
    Command.SetSQL('SELECT 1');
    try
      Command.Execute;
    except
      on E: EODBCError do
        if (E.State = '08001') or (E.State = '08S01') then
          raise;
      else
        raise;
    end;
  finally
    Command.Free;
  end;
end;

function TODBCConnection.CheckIsValid: boolean;
begin
  Result := True;
end;

procedure TODBCConnection.Assign(Source: TCRConnection);
var
  Src: TODBCConnection;
begin
  inherited;

  Src := TODBCConnection(Source);
  FUseUnicode := Src.FUseUnicode;
  FConnectionTimeout := Src.FConnectionTimeout;
  FDSNType := Src.FDSNType;
  FVarBinaryAsBlob := Src.FVarBinaryAsBlob;
  FLongVarBinaryAsBlob := Src.FLongVarBinaryAsBlob;
  FColumnWiseBinding := Src.FColumnWiseBinding;
end;

procedure TODBCConnection.AssignConnect(Source: TCRConnection);
var
  Src: TODBCConnection;
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Src := TODBCConnection(Source);
      Assign(Src);

      FODBCEnv := Src.FODBCEnv;
      FSQLHDbc := Src.FSQLHDbc;
      FDriverODBCVer := Src.FDriverODBCVer;
      FIdentCase := Src.FIdentCase;
      FCachedCatalog := Src.FCachedCatalog;
      FCachedSchema := Src.FCachedSchema;

      FInternalTransaction.AssignConnect(Src.FInternalTransaction);

      FConnected := FSQLHDbc <> nil;
      FNativeConnection := False;
    end;
  end;
end;

function TODBCConnection.GetServerVersion: string;
begin
  Result := GetDriverInfo(SQL_DBMS_VER);
end;

function TODBCConnection.GetServerVersionFull: string;
begin
  Result := GetDriverInfo(SQL_DBMS_NAME) + ' ' + GetDriverInfo(SQL_DBMS_VER);
end;

function TODBCConnection.GetClientVersion: string;
begin
  Result := GetDriverInfo(SQL_DRIVER_VER);
end;

function TODBCConnection.GetDriverODBCVersion: string;
begin
  Result := GetDriverInfo(SQL_DRIVER_ODBC_VER);
end;

function TODBCConnection.CanChangeDatabase: boolean;
begin
  Result := False;
end;

function TODBCConnection.GetCurrentCatalog: string;
begin
  Result := '';
end;

function TODBCConnection.GetCachedCatalog: string;
begin
  if FCachedCatalog = '' then
    FCachedCatalog := GetCurrentCatalog;

  Result := FCachedCatalog;
end;

function TODBCConnection.GetCurrentSchema: string;
begin
  Result := '';
end;

function TODBCConnection.GetCachedSchema: string;
begin
  if FCachedSchema = '' then
    FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

function TODBCConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TODBCCommand;
end;

function TODBCConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TODBCRecordSet;
end;

function TODBCConnection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TODBCTransaction;
end;

function TODBCConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TODBCLoader;
end;

function TODBCConnection.GetMetaDataClass: TCRMetaDataClass;
begin
  Result := TODBCMetaData;
end;

class function TODBCConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TODBCCommand.GetMapRulesClass;
end;

function TODBCConnection.CreateSQLInfo: TSQLInfo;
begin
  Result := inherited CreateSQLInfo;
  TODBCSQLInfo(Result).FConnection := Self;
end;

function TODBCConnection.GetConnectionString: string;
var
  ActualDSNType: TDSNType;
begin
  ActualDSNType := FDSNType;

  if ActualDSNType = ntAuto then begin
    if Pos('=', FServer) > 0 then
      ActualDSNType := ntConnectionString
    else
    if Pos('.dsn', FServer) > 0 then
      ActualDSNType := ntFile
    else
      ActualDSNType := ntName;
  end;

  case ActualDSNType of
    ntName:
      Result := Format('DSN=%s;UID=%s;PWD=%s', [FServer, FUsername, FPassword]);
    ntFile:
      Result := Format('FILEDSN=%s;UID=%s;PWD=%s', [FServer, FUsername, FPassword]);
    ntConnectionString: begin
      Result := FServer;
      if (Pos('UID=', Result) = 0) and (FUsername <> '') then
        Result := Result + ';UID=' +FUsername;
      if (Pos('PWD=', Result) = 0) and (FPassword <> '') then
        Result := Result + ';PWD=' +FPassword;
    end;
  end;
end;

procedure TODBCConnection.ApplyConnectProps;
begin
end;

function TODBCConnection.IsCommentAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.IsEmptyStringAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.IsBlockFetchAllowed: boolean;
begin
  Result := Hi(FDriverODBCVer) >= 3;
end;

function TODBCConnection.IsReturnValueAllowed: boolean;
begin
  Result := True;
end;

function TODBCConnection.OutParamIsInOut: boolean;
begin
  Result := False;
end;

function TODBCConnection.IsDriverPresent(const Name: string): boolean;
const
  BufSize = 256;
var
  Buf: IntPtr;
  Res, Len, Len2: smallint;
  s: string;
begin
  Result := False;

  Buf := Marshal.AllocHGlobal(BufSize * ODBCCharSize);
  try
  {$IFDEF USE_UNICODE_DRIVER}
    Res := Cli.SQLDriversW(Cli.SQLHEnv, SQL_FETCH_FIRST, Buf, BufSize, Len, nil, 0, Len2);
  {$ELSE}
    Res := Cli.SQLDriversA(Cli.SQLHEnv, SQL_FETCH_FIRST, Buf, BufSize, Len, nil, 0, Len2);
  {$ENDIF}
    repeat
      if Res = SQL_NO_DATA then
        break;
      s := FromODBCString(Buf);
      if SameText(s, Name) then begin
        Result := True;
        Exit;
      end;
    {$IFDEF USE_UNICODE_DRIVER}
      Res := Cli.SQLDriversW(Cli.SQLHEnv, SQL_FETCH_NEXT, Buf, BufSize, Len, nil, 0, Len2);
    {$ELSE}
      Res := Cli.SQLDriversA(Cli.SQLHEnv, SQL_FETCH_NEXT, Buf, BufSize, Len, nil, 0, Len2);
    {$ENDIF}
    until False;
  finally
    Marshal.FreeHGlobal(Buf);
  end;
end;

{ TODBCTransaction }

procedure TODBCTransaction.StartTransaction;
var
  Connection: TODBCConnection;
  ODBCLevel: integer;
begin
  if FConnections.Count = 0 then
    raise Exception.Create(SNoConnectionsInTransaction);

  Connection := TODBCConnection(FConnections[0]);

  if not Connection.GetConnected then
    raise Exception.Create(SConnectionInTransactionNotActive);

  case FIsolationLevel of
    ilReadUncommitted:
      ODBCLevel := SQL_TXN_READ_UNCOMMITTED;
    ilReadCommitted:
      ODBCLevel := SQL_TXN_READ_COMMITTED;
    ilRepeatableRead:
      ODBCLevel := SQL_TXN_REPEATABLE_READ;
    ilIsolated, ilSnapshot:
      ODBCLevel := SQL_TXN_SERIALIZABLE;
  else
    raise Exception.Create(SUnsupportedIsolationLevel);
  end;

  with Connection do begin
    Check(Cli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_TXN_ISOLATION, ODBCLevel, 0));
    Check(Cli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF, 0));
  end;

  FActive := True;
  FNativeTransaction := True;
end;

procedure TODBCTransaction.Commit;
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  if FNativeTransaction then
    with Connection do begin
      Check(Cli.SQLEndTran(SQL_HANDLE_DBC, FSQLHDbc, SQL_COMMIT));
      Check(Cli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0));
    end;

  FActive := False;
end;

procedure TODBCTransaction.Rollback;
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  if FNativeTransaction then
    with Connection do begin
      Check(Cli.SQLEndTran(SQL_HANDLE_DBC, FSQLHDbc, SQL_ROLLBACK));
      Check(Cli.SQLSetConnectAttrInt(FSQLHDbc, SQL_ATTR_AUTOCOMMIT, SQL_AUTOCOMMIT_ON, 0));
    end;

  FActive := False;
end;

procedure TODBCTransaction.Savepoint(const Name: string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecuteSQL('SAVEPOINT ' + Name);
end;

procedure TODBCTransaction.ReleaseSavepoint(const Name: string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecuteSQL('RELEASE SAVEPOINT ' + Name);
end;

procedure TODBCTransaction.RollbackToSavepoint(const Name: string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecuteSQL('ROLLBACK TO SAVEPOINT ' + Name);
end;

{ TODBCParamDesc }

constructor TODBCParamDesc.Create;
begin
  inherited Create;

  FBufferSize := -1;
  FEnableMSec := true;
end;

destructor TODBCParamDesc.Destroy;
begin
  FreeBuffer;

  inherited;
end;

procedure TODBCParamDesc.AllocBindBuffer(Iters: Integer);
begin
  if FBufferAllocated then
    Exit;

  if FIndicator = nil then
    if Iters > 1 then begin
      FIndicator := Marshal.AllocHGlobal(SizeOf(IntPtr) * Iters);
      FillChar(FIndicator, SizeOf(IntPtr) * Iters, $FF);
    end
    else begin
      FIndicator := Marshal.AllocHGlobal(SizeOf(IntPtr));
      FillChar(FIndicator, SizeOf(IntPtr), $FF);
    end;

  if (FBufferSize >= 0) or (FDataType = dtUnknown) then
    Exit;

  case FDataType of
    dtFixedChar, dtString, dtBytes, dtVarBytes:
      FBufferSize := FSize + 1;
    dtFixedWideChar, dtWideString:
      FBufferSize := (FSize + 1) * 2;
    dtBlob, dtMemo:
      FBufferSize := 1;
    dtWideMemo:
      FBufferSize := 2;
    dtInt8, dtUInt8:
      FBufferSize := SizeOf(Byte);
    dtSmallInt, dtWord:
      FBufferSize := SizeOf(SmallInt);
    dtInteger, dtLongWord:
      FBufferSize := SizeOf(Integer);
    dtInt64, dtUInt64:
      FBufferSize := SizeOf(Int64);
    dtBoolean:
      FBufferSize := sizeOf(SmallInt);
    dtSingle, dtFloat, dtCurrency:
      FBufferSize := sizeof(Double);
    dtBCD, dtFMTBCD:
      FBufferSize := BCDStrSize + 1;
    dtDate:
      FBufferSize := SizeOf(TSQLDateStruct);
    dtTime:
      FBufferSize := SizeOf(TSQLTimeStruct);
    dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}:
      FBufferSize := SizeOf(TSQLTimeStampStruct);
    dtGuid:
      FBufferSize := GuidStrSize;
  else
    raise Exception.Create(SDataTypeNotSupported);
  end;

  if FBufferSize > 0 then
    if Iters > 1 then begin
      FBuffer := Marshal.AllocHGlobal(FBufferSize * Iters);
      FillChar(FBuffer, FBufferSize * Iters, $00);
    end
    else begin
      FBuffer := Marshal.AllocHGlobal(FBufferSize);
      FillChar(FBuffer, FBufferSize, $00);
    end;

  FBufferAllocated := True;
end;

procedure TODBCParamDesc.FreeBuffer;
begin
  if not FBufferAllocated then
    Exit;

  inherited;

  FBufferAllocated := False;

  if FIndicator <> nil then begin
    Marshal.FreeHGlobal(FIndicator);
    FIndicator := nil;
  end;

  if (FBufferSize > 0) and (FBuffer <> nil) then begin
    Marshal.FreeHGlobal(FBuffer);
    FBuffer := nil;
    FBufferSize := -1;
  end;
end;

procedure TODBCParamDesc.WriteBuffer(Iters, Offset: integer; AllowDataAtExec, AllowEmptyString: boolean;
  var DataAtExec: boolean; var Precision, Scale: integer);
var
  i: integer;
  ValuePtr: PVariant;
  BufferPtr,
  IndicatorPtr: IntPtr;
  Len: integer;
  i64: Int64;
  ui64: UInt64;
  Cur: currency;
  Str: string;
  Bcd: TBcd;
  Year, MSec: word;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
  TsRec: TSQLTimeStampStruct;
  Blob: TBlob;
begin
  Precision := 0;
  Scale := 0;
  DataAtExec := False;

  for i := 0 to Iters - 1 do begin
    ValuePtr := GetItemPtr(Offset + i);
    BufferPtr := PtrOffset(FBuffer, i * FBufferSize);
    IndicatorPtr := PtrOffset(FIndicator, i * SizeOf(IntPtr));

    if (PVarData(ValuePtr).VType in [varEmpty, varNull]) or
       (not AllowEmptyString and VarIsStr(ValuePtr^) and (ValuePtr^ = ''))
    then begin
      NativeInt(IndicatorPtr^) := NativeInt(SQL_NULL_DATA);
      Continue;
    end;

    Len := 0;
    case FDataType of
      dtFixedChar, dtString:
        Len := WriteAnsiString(BufferPtr, AnsiString(ValuePtr^));
      dtFixedWideChar, dtWideString:
        Len := WriteWideString(BufferPtr, WideString(ValuePtr^));
      dtBytes, dtVarBytes:
        Len := WriteBytes(BufferPtr, ValuePtr^);
      dtUInt8:
        Byte(BufferPtr^) := Byte(ValuePtr^);
      dtInt8:
        ShortInt(BufferPtr^) := ShortInt(ValuePtr^);
      dtSmallInt:
        SmallInt(BufferPtr^) := Smallint(ValuePtr^);
      dtWord:
        Word(BufferPtr^) := Word(ValuePtr^);
      dtInteger:
        Integer(BufferPtr^) := Integer(ValuePtr^);
      dtLongWord: begin
        i64 := ValuePtr^;
        Cardinal(BufferPtr^) := Cardinal(i64);
      end;
      dtInt64: begin
        i64 := ValuePtr^;
        Int64(BufferPtr^) := i64;
      end;
      dtUInt64: begin
        ui64 := ValuePtr^;
        UInt64(BufferPtr^) := ui64;
      end;
      dtBoolean:
        WordBool(BufferPtr^) := WordBool(Boolean(ValuePtr^));
      dtSingle, dtFloat, dtCurrency:
        Double(BufferPtr^) := Double(ValuePtr^);
      dtBCD: begin
        Cur := Currency(ValuePtr^);
        Str := CurrToStr(Cur);
        if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
          Str := StringReplace(Str, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);
        Len := WriteAnsiString(BufferPtr, AnsiString(Str));
      end;
      dtFMTBCD: begin
        if VarType(ValuePtr^) = VarFMTBcd then
          Bcd := VarToBcd(ValuePtr^)
        else
          Bcd := StrToBcd(ValuePtr^);
        Precision := Bcd.Precision;
        Scale := Bcd.SignSpecialPlaces and $3F;
        Str := BcdToStr(Bcd);
        if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
          Str := StringReplace(Str, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);
        Len := WriteAnsiString(BufferPtr, AnsiString(Str));
      end;
      dtDate: begin
        DecodeDate(ValuePtr^, Year, DateRec.Month, DateRec.Day);
        DateRec.Year := Year;
        TSQLDateStruct(BufferPtr^) := DateRec;
      end;
      dtTime: begin
        DecodeTime(ValuePtr^, TimeRec.Hour, TimeRec.Minute, TimeRec.Second, MSec);
        TSQLTimeStruct(BufferPtr^) := TimeRec;
      end;
      dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
        DecodeDate(ValuePtr^, Year, TsRec.Month, TsRec.Day);
        TsRec.Year := Year;
        DecodeTime(ValuePtr^, TsRec.Hour, TsRec.Minute, TsRec.Second, MSec);
        if FEnableMSec then
          TsRec.Fraction := MSec * 1000000
        else
          TsRec.Fraction := 0;
        TSQLTimeStampStruct(BufferPtr^) := TsRec;
      end;
      dtGuid:
        Len := WriteAnsiString(BufferPtr, AnsiString(ValuePtr^));
      dtBlob, dtMemo, dtWideMemo: begin
        Blob := GetBlob(Offset + i);
        if Blob <> nil then
          Len := Blob.Size
        else
          Len := 1;

        if Len = 0 then
          Len := SQL_NULL_DATA
        else
        if AllowDataAtExec then begin
          Len := SQL_DATA_AT_EXEC;
          DataAtExec := True;
        end
        else begin
          if Blob = nil then
            raise Exception.Create(SCannotConvertType);
          Len := Blob.Size;
        end;
      end;
      else
        raise Exception.Create(SDataTypeNotSupported);
    end;

    Marshal.WriteNativeInt(IndicatorPtr, NativeInt(Len));
  end;
end;

function TODBCParamDesc.WriteAnsiString(Buf: IntPtr; const Value: AnsiString): integer;
var
  Len: integer;
  pValue: PAnsiChar;
{$IFDEF MSWINDOWS}
  {$IFDEF IS_UTF8}
  AStr: AnsiString;
  {$ENDIF}
{$ELSE}
  {$IFNDEF IS_UTF8}
  AStr: AnsiString;
  {$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  {$IFDEF IS_UTF8}
  AStr := CRFunctions.Utf8ToAnsi(Value);
  pValue := PAnsiChar(AStr);
  Len := Length(AStr);
  {$ELSE}
  pValue := PAnsiChar(Value);
  Len := Length(Value);
  {$ENDIF}
{$ELSE}
  {$IFDEF IS_UTF8}
  pValue := PAnsiChar(Value);
  Len := Length(Value);
  {$ELSE}
  AStr := CRFunctions.AnsiToUtf8(Value);
  pValue := PAnsiChar(AStr);
  Len := Length(AStr);
  {$ENDIF}
{$ENDIF}

  Result := WritePAnsiString(Buf, pValue, Len);
end;

function TODBCParamDesc.WritePAnsiString(Buf: IntPtr; pValue: PAnsiChar; Len: Integer): integer;
begin
  if Len < FBufferSize then
    Result := Len
  else
    Result := FBufferSize - 1;

  if FConvertEOL then
    Result := RemoveCRString(pValue, Len, Buf, Result)
  else
    CopyBuffer(pValue, Buf, Result + 1); // with zero terminator
end;

function TODBCParamDesc.WriteWideString(Buf: IntPtr; const Value: WideString): integer;
var
{$IFNDEF MACOS}
  pValue: PWideChar;
{$ELSE}
  DestBuf: TBytes;
{$ENDIF}
begin
{$IFNDEF MACOS}
  pValue := PWideChar(Value);
  Result := Length(Value);
  if Result > 0 then
    Result := WritePWideString(Buf, pValue, Result);
{$ELSE}
  DestBuf := Encoding.UTF8.GetBytes(Value);
  Result := Length(DestBuf);
  if Result > 0 then
    Result := WritePAnsiString(Buf, @DestBuf[0], Result);
{$ENDIF}
end;

function TODBCParamDesc.WritePWideString(Buf: IntPtr; pValue: PWideChar; Len: Integer): integer;
begin
  Result := Len * 2;
  if Result >= FBufferSize then
    Result := FBufferSize - 2;

  if FConvertEOL then
    Result := RemoveCRUnicode(pValue, Len, Buf, Result shr 1) * 2
  else
    CopyBuffer(pValue, Buf, Result + 2); // with zero terminator
end;

function TODBCParamDesc.WriteBytes(Buf: IntPtr; const Value: variant): integer;
var
  Len: integer;
  Bytes: TBytes;
  SafeArray: PVarArray;
begin
{$IFNDEF VER9P}
  SetLength(Bytes, 0); // anti-warning
{$ENDIF}

  if TVarData(Value).VType = varArray + varByte then begin
    SafeArray := VarArrayAsPSafeArray(Value);

    Len := SafeArray.Bounds[0].ElementCount;
    if Len > FBufferSize then
      Len := FBufferSize;

    if Len > 0 then
      Move(SafeArray.Data^, Buf^, Len);
  end
  else begin
    Bytes := Encoding.Default.GetBytes(VarToStr(Value));
    Len := Length(Bytes);
    if Len > FBufferSize then
      Len := FBufferSize;

    if Len > 0 then
      Move(Bytes[0], Buf, Len);
  end;

  Result := Len;
end;

procedure TODBCParamDesc.ReadBuffer(Iters, Offset: integer);
var
  i: integer;
  ValuePtr: PVariant;
  BufferPtr,
  IndicatorPtr: IntPtr;
  Len: integer;
  Str: string;
  Date: TDateTime;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
  TsRec: TSQLTimeStampStruct;
  Blob: TBlob;
  i64: Int64;
begin
  for i := 0 to Iters - 1 do begin
    ValuePtr := GetItemPtr(Offset + i);
    BufferPtr := PtrOffset(FBuffer, i * FBufferSize);
    IndicatorPtr := PtrOffset(FIndicator, i * SizeOf(IntPtr));

    Len := Marshal.ReadInt32(IndicatorPtr);
    if Len = SQL_NULL_DATA then begin
      Value := Null;
      Exit;
    end;

    case FDataType of
      dtFixedChar, dtString:
        ValuePtr^ := ReadAnsiString(BufferPtr, Len);
      dtFixedWideChar, dtWideString:
        ValuePtr^ := ReadWideString(BufferPtr, Len);
      dtBytes, dtVarBytes:
        ValuePtr^ := ReadBytes(BufferPtr, Len);
      dtUInt8:
        ValuePtr^ := Byte(BufferPtr^);
      dtInt8:
        ValuePtr^ := ShortInt(BufferPtr^);
      dtWord:
        ValuePtr^ := Word(BufferPtr^);
      dtSmallInt:
        ValuePtr^ := Smallint(BufferPtr^);
      dtLongWord:
        ValuePtr^ := Cardinal(BufferPtr^);
      dtInteger:
        ValuePtr^ := Integer(BufferPtr^);
      dtInt64:
        ValuePtr^ := Int64(BufferPtr^);
      dtUInt64: begin
        i64 := Int64(BufferPtr^); // Delphi 6 bug on cast Int64 to UInt64
        ValuePtr^ := UInt64(i64);              // Delphi 6 bug on cast Int64 to UInt64
      end;
      dtBoolean:
        ValuePtr^ := Boolean(WordBool(BufferPtr^));
      dtFloat, dtCurrency:
        ValuePtr^ := Double(BufferPtr^);
      dtBCD, dtFMTBCD: begin
        Str := string(ReadAnsiString(BufferPtr, 0));
        if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
          Str := StringReplace(Str, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
        if FDataType = dtFMTBCD then
          ValuePtr^ := VarFMTBcdCreate(StrToBcd(Str))
        else
          ValuePtr^ := StrToCurr(Str);
      end;
      dtDate: begin
        DateRec := TSQLDateStruct(BufferPtr^);
        if TryEncodeDate(DateRec.Year, DateRec.Month, DateRec.Day, Date) then
          ValuePtr^ := Date
        else
          ValuePtr^ := MinDateTime;
      end;
      dtTime: begin
        TimeRec := TSQLTimeStruct(BufferPtr^);
        if TryEncodeTime(TimeRec.Hour, TimeRec.Minute, TimeRec.Second, 0, Date) then
          ValuePtr^ := Date
        else
          ValuePtr^ := 0;
      end;
      dtDateTime{$IFNDEF FPC}, dtSQLTimeStamp{$ENDIF}: begin
        TsRec := TSQLTimeStampStruct(BufferPtr^);
        if TryEncodeDateTime(TsRec.Year, TsRec.Month, TsRec.Day,
          TsRec.Hour, TsRec.Minute, TsRec.Second, TsRec.Fraction div 1000000, Date)
        then
          ValuePtr^ := Date
        else
          ValuePtr^ := MinDateTime;
      end;
      dtGuid: begin
        Str := string(ReadAnsiString(BufferPtr, 0));
        if (Str <> '') and (Str[1] <> '{') then
          Str := '{' + Str + '}';
        ValuePtr^ := Str;
      end;
      dtBlob, dtMemo, dtWideMemo: begin
        Blob := GetBlob(i);
        Blob.FirstPiece.Used := Len;
      end;
    else
      raise Exception.Create(SDataTypeNotSupported);
    end;
  end;
end;

function TODBCParamDesc.ReadAnsiString(Buf: IntPtr; Len: integer): AnsiString;
{$IFNDEF MSWINDOWS}
var
  SourceBuf: TBytes;
{$ENDIF}
begin
  if Len > 0 then begin
  {$IFDEF MSWINDOWS}
    Result := ReadPAnsiString(Buf, Len);
  {$ELSE}
    SourceBuf := ReadPUtf8Bytes(Buf, Len);
    {$IFDEF NEXTGEN}
    Result := AnsiString(Encoding.UTF8.GetString(SourceBuf));
    {$ELSE}
    Result := Encoding.UTF8.GetAnsiString(SourceBuf);
    {$ENDIF}
  {$ENDIF}
  end
  else
    Result := '';
end;

function TODBCParamDesc.ReadPAnsiString(Buf: IntPtr; Len: integer): AnsiString;
begin
  if FConvertEOL then begin
    SetLengthA(Result, FSize * 2);
    if FSize > 0 then begin
      Len := AddCRString(Buf, Len, PAnsiChar(Result), FSize * 2);
      SetLengthA(Result, Len);
    end;
  end
  else begin
    SetLengthA(Result, Len);
    if Len > 0 then
      Move(Buf^, PAnsiChar(Result)^, Len);
  end;
end;

function TODBCParamDesc.ReadPUtf8Bytes(Buf: IntPtr; Len: integer): TBytes;
begin
  if FConvertEOL then begin
    SetLength(Result, FSize * 2);
    if FSize > 0 then begin
      Len := AddCRString(Buf, Len, @Result[0], FSize * 2);
      SetLength(Result, Len);
    end;
  end
  else begin
    SetLength(Result, Len);
    if Len > 0 then
      Move(Buf^, Result[0], Len);
  end;
end;

function TODBCParamDesc.ReadWideString(Buf: IntPtr; Len: integer): WideString;
{$IFDEF MACOS}
var
  SourceBuf: TBytes;
{$ENDIF}
begin
  if Len > 0 then begin
  {$IFDEF MACOS}
    SourceBuf := ReadPUtf8Bytes(Buf, Len);
    Result := Encoding.UTF8.GetWideString(SourceBuf);
  {$ELSE}
    Result := ReadPWideString(Buf, Len);
  {$ENDIF}
  end
  else
    Result := '';
end;

function TODBCParamDesc.ReadPWideString(Buf: IntPtr; Len: integer): WideString;
begin
  if FConvertEOL then begin
    SetLength(Result, FSize * 2);
    if FSize > 0 then begin
      Len := AddCRUnicode(Buf, FSize, PWideChar(Result), FSize * 2);
      SetLength(Result, Len);
    end;
  end
  else begin
    SetLength(Result, Len);
    if Len > 0 then
      Move(Buf^, PWideChar(Result)^, Len);
  end;
end;

function TODBCParamDesc.ReadBytes(Buf: IntPtr; Len: integer): variant;
begin
  Result := VarArrayCreate([0, Len - 1], varByte);
  if Len > 0 then
    Move(Buf^, TVarData(Result).VArray.Data^, Len);
end;

function TODBCParamDesc.GetMinDefaultSize: Integer;
begin
  Result := 255;
end;

procedure TODBCParamDesc.SetDataType(Value: word);
begin
  if Value <> FDataType then begin
    FreeBuffer;

    FDataType := Value;
  end;
end;

procedure TODBCParamDesc.SetParamType(Value: TParamDirection);
begin
  if Value <> FParamType then begin
    FreeBuffer;

    FParamType := Value;
  end;
end;

procedure TODBCParamDesc.SetSize(Value: integer);
begin
  if Value <> FSize then begin
    case FDataType of
      dtFixedChar, dtString,
      dtFixedWideChar, dtWideString,
      dtBytes, dtVarBytes:
        FreeBuffer;
    end;

    FSize := Value;
  end;
end;

function TODBCParamDesc.GetBlob(Index: integer): TBlob;
var
  Obj: TObject;
  ValuePtr: PVarData;
begin
  ValuePtr := PVarData(GetItemPtr(Index));

  case ValuePtr.VType of
    varSharedObject: begin
      Obj := TObject(ValuePtr.VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Result := TBlob(Obj);
    end;
  else
    Result := nil;
  end;
end;

{ TODBCSQLInfo }

constructor TODBCSQLInfo.Create(ParserClass: TSQLParserClass);
begin
  inherited Create(ParserClass);
  FConnection := nil;
end;

function TODBCSQLInfo.IdentCase: TIdentCase;
begin
  if FConnection <> nil then
    Result := FConnection.FIdentCase
  else
    Result := icMixed;
end;

{ TODBCFieldDesc }

procedure TODBCFieldDesc.SetDataType(Value: Word);
begin
  inherited;
  FIsConverted := TODBCRecordSet.IsConvertedFieldType(Value);
end;

{ TODBCCommand }

constructor TODBCCommand.Create;
begin
  inherited;

  FParamsProcessedType := 0;
end;

destructor TODBCCommand.Destroy;
begin

  inherited;
end;

function TODBCCommand.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommandTimeout := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCCommand.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsProcessed:
      Value := FRowsAffected;
    prCommandTimeout:
      Value := FCommandTimeout;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TODBCCommand.SetConnection(Value: TCRConnection);
begin
  if Value <> FConnection then begin
    inherited;

    FConnection := TODBCConnection(Value);
  end;
end;

class function TODBCCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TODBCSQLInfo;
end;

class function TODBCCommand.GetParserClass: TSQLParserClass;
begin
  Result := TODBCParser;
end;

class function TODBCCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TODBCMapRules;
end;

class function TODBCCommand.GetParamDescClass: TParamDescClass;
begin
  Result := TODBCParamDesc;
end;

procedure TODBCCommand.Prepare;
begin
  if FStmt <> nil then
    Exit;

  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);

  AllocStatement;
  try
    InternalPrepare;
  except
    FreeStatement;
    raise;
  end;

  FCursorState := csPrepared;
end;

procedure TODBCCommand.Unprepare;
begin
  if FStmt = nil then
    Exit;

  FreeStatement;
  FCursorState := csInactive;
end;

function TODBCCommand.GetPrepared: boolean;
begin
  Result := FStmt <> nil;
end;

procedure TODBCCommand.Execute;
var
  NeedPrepare: boolean;
  OldIters, OldOffset, AffectedRows, i: integer;
begin
  if GetCursorState > csPrepared then
    Exit;

  try
    NeedPrepare := not GetPrepared;
    if NeedPrepare then
      Prepare;

    try
      if (FBatchIters = 1) and (FBatchOffset = 0) then
        InternalExecute
      else begin
        FLockReadOutParams := False;
        FBatchSupported := not IsODBCError(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_PARAM_BIND_TYPE, SQL_PARAM_BIND_BY_COLUMN, 0))
                       and not IsODBCError(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_PARAMSET_SIZE, FBatchIters, 0));
        if FBatchSupported then
          InternalExecute
        else begin
          FLockReadOutParams := True;
          OldOffset := FBatchOffset;
          OldIters := FBatchIters;
          AffectedRows := 0;
          try
            FBatchIters := 1;
            for i := 0 to OldIters - 1 do begin
              FBatchOffset := OldOffset + i;
              InternalExecute;
              ReadOutParams;
              Inc(AffectedRows, FRowsAffected);
            end;
          finally
            FLockReadOutParams := False;
            FBatchOffset := OldOffset;
            FBatchIters := OldIters;
          end;
          FRowsAffected := AffectedRows;
        end;
      end;
    finally
      if NeedPrepare then
        UnPrepare;
    end;

    if not FRecordSetExec and not NeedPrepare then
      Close;

  except
    if not FRecordSetExec and Assigned(FAfterExecute) then
      FAfterExecute(False);
    raise;
  end;
  if not FRecordSetExec and Assigned(FAfterExecute) then
    FAfterExecute(True);
end;

procedure TODBCCommand.Close;
begin
  if FStmt = nil then
    Exit;

  Cli.SQLCloseCursor(FStmt);
end;

procedure TODBCCommand.InitProcParams(const Name: string);
var
  RecordSet: TODBCMetaDataRecordSet;
  Info: TSQLObjectInfo;
  RecBuf: IntPtr;
  Param: TParamDesc;
  v: variant;
  s: string;
  SQLParamType, SQLDataType: smallint;
  SQLLength: integer;
  SQLScale: smallint;
  DataType, SubDataType: word;
  Len, Scale: integer;
  Fixed: boolean;
  ReturnParamField: TFieldDesc;
  ParamNameField: TFieldDesc;
  ParamLengthField: TFieldDesc;
  ParamScalehField: TFieldDesc;
  ParamTypeField: TFieldDesc;
  ParamStrTypeField: TFieldDesc;
begin
  SQLInfo.SplitObjectName(Name, Info);
  Info.Name := SQLInfo.NormalizeName(Info.Name, False, True);
  Info.Schema := SQLInfo.NormalizeName(Info.Schema, False, True);
  if Info.Schema = '' then
    Info.Schema := FConnection.GetCachedSchema;
  Info.Catalog := SQLInfo.NormalizeName(Info.Catalog, False, True);
  if Info.Catalog = '' then
    Info.Catalog := FConnection.GetCachedCatalog;

  RecordSet := TODBCMetaDataRecordSet.Create;
  try
    RecordSet.SetConnection(FConnection);
    with TODBCMetaDataCommand(RecordSet.GetCommand) do begin
      FMetaDataKind := mkProcedureColumns;
      with FMetaDataArgs do begin
        CatalogName := Info.Catalog;
        SchemaName := Info.Schema;
        ObjectName := Info.Name;
        ColumnName := '%';
      end;
    end;

    RecordSet.Open;

    ReturnParamField := RecordSet.Fields[4];
    ParamNameField := RecordSet.Fields[3];
    ParamLengthField := RecordSet.Fields[7];
    ParamScalehField := RecordSet.Fields[9];
    ParamTypeField := RecordSet.Fields[5];
    ParamStrTypeField := RecordSet.Fields[6];

    RecordSet.AllocRecBuf(RecBuf);
    try
      FParams.Clear;
      repeat
        RecordSet.GetNextRecord(RecBuf);
        if RecordSet.Eof then
          break;

        RecordSet.GetFieldAsVariant(ReturnParamField, RecBuf, v);
        SQLParamType := v;
        if SQLParamType = SQL_RESULT_COL then
          continue;
        if (SQLParamType = SQL_RETURN_VALUE) and not FConnection.IsReturnValueAllowed then
          continue;

        Param := AddParam;

        case SQLParamType of
          SQL_PARAM_TYPE_UNKNOWN:
            Param.SetParamType(pdUnknown);
          SQL_PARAM_INPUT:
            Param.SetParamType(pdInput);
          SQL_PARAM_INPUT_OUTPUT:
            Param.SetParamType(pdInputOutput);
          SQL_PARAM_OUTPUT:
            if FConnection.OutParamIsInOut then
              Param.SetParamType(pdInputOutput)
            else
              Param.SetParamType(pdOutput);
          SQL_RETURN_VALUE:
            Param.SetParamType(pdResult);
        end;

        RecordSet.GetFieldAsVariant(ParamNameField, RecBuf, v);
        s := VarToStr(v);
        if (s <> '') and (s[1] = '@') then
          Delete(s, 1, 1);
        Param.SetName(s);

        RecordSet.GetFieldAsVariant(ParamLengthField, RecBuf, v);
        if VarIsNull(v) then
          SQLLength := 0
        else
          SQLLength := v;

        RecordSet.GetFieldAsVariant(ParamScalehField, RecBuf, v);
        if VarIsNull(v) then
          SQLScale := 0
        else
          SQLScale := v;

        RecordSet.GetFieldAsVariant(ParamTypeField, RecBuf, v);
        if VarIsNull(v) then begin
          RecordSet.GetFieldAsVariant(ParamStrTypeField, RecBuf, v);
          SQLDataType := DetectSpecificType(VarToStr(v), SQLLength, SQLScale);
        end
        else
          SQLDataType := v;

        DetectDataType(SQLDataType, SQLLength, SQLScale, nil,
          DataType, SubDataType, Len, Scale, Fixed, False, True, True, False, False);

        Param.SetDataType(DataType);
        Param.SetSubDataType(SubDataType);
        Param.SetSize(Len);
      until False;

    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
    RecordSet.Close;
  finally
    RecordSet.Free;
  end;
end;

function TODBCCommand.CreateProcCall(const Name: string; NeedDescribe: boolean; IsQuery: boolean): string;
var
  s, si: string;
  f, i: integer;
begin
  if NeedDescribe then
    InitProcParams(Name);

  if (FParams.Count > 0) and (FParams[0].GetParamType = pdResult) then begin
    f := 1;
    s := '{' + ':' + FParams[0].GetName + ' = call ' + Name + '(';
    si := '{? = call ' + Name + '(';
  end
  else begin
    f := 0;
    s := '{call ' + Name + '(';
    si := s;
  end;

  for i := f to FParams.Count - 1 do begin
    if i > f then begin
      s := s + ', ';
      si := si + ', ';
    end;
    s := s + ':' + FParams[i].GetName;
    si := si + '?';
  end;

  s := s + ')}';
  si := si + ')}';

  FSQL := si;
  FUserSQL := s;
  Result := s;
end;

function TODBCCommand.CheckMoreResults: boolean;
var
  Res: Smallint;
begin
  Result := False;

  if FStmt = nil then
    Exit;

  Cli.SQLFreeStmt(FStmt, SQL_UNBIND);

  Res := Cli.SQLMoreResults(FStmt);
  if Res = SQL_NO_DATA then
    Exit
  else
    Check(Res);

  Result := True;
end;

function TODBCCommand.GetCursorState: TCursorState;
begin
  Result := FCursorState;
end;

procedure TODBCCommand.SetCursorState(Value: TCursorState);
begin
  if Value <> FCursorState then
    FCursorState := Value;
end;

function TODBCCommand.Cli: TODBCCliAPI;
begin
  Result := FConnection.Cli;
end;

procedure TODBCCommand.AllocStatement;
begin
  Check(Cli.SQLAllocHandle(SQL_HANDLE_STMT, FConnection.FSQLHDbc, FStmt));
  if FCommandTimeout <> 0 then
    Check(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_QUERY_TIMEOUT, FCommandTimeout, 0));
end;

procedure TODBCCommand.FreeStatement;
begin
  Cli.SQLFreeHandle(SQL_HANDLE_STMT, FStmt);
  FStmt := nil;
end;

procedure TODBCCommand.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    FConnection.ProcessError(SQL_HANDLE_STMT, FStmt, ErrorCode, Component);
end;

procedure TODBCCommand.DetectCommandType;
var
  ColumnCount: smallint;
begin
  ColumnCount := 0;
  Check(Cli.SQLNumResultCols(FStmt, ColumnCount));
  if ColumnCount > 0 then
    FCommandType := ctCursor
  else
    FCommandType := ctStatement;
end;

procedure TODBCCommand.InternalPrepare;
var
  SQL: string;
{$IFNDEF USE_UNICODE_DRIVER}
  SQLA: AnsiString;
{$ENDIF}
begin
  if Trim(FSQL) = '' then
    raise Exception.Create(SEmptySQLStatement);

  if not FConnection.IsCommentAllowed then
    SQL := RemoveComments(FSQL)
  else
    SQL := FSQL;

{$IFDEF USE_UNICODE_DRIVER}
  Check(Cli.SQLPrepareW(FStmt, PWideChar(SQL), Length(SQL)));
{$ELSE}
  SQLA := ToODBCString(SQL);
  Check(Cli.SQLPrepareA(FStmt, PAnsiChar(SQLA), Length(SQLA)));
{$ENDIF}
end;

procedure TODBCCommand.InternalExecute;
var
  Res: smallint;
  ValueNInt: NativeInt;
  i: integer;
begin
  BindParams;

  Res := Cli.SQLExecute(FStmt);
  if Res = SQL_NEED_DATA then
    Res := WriteBlobs;
  if Res = SQL_NO_DATA then
    FRowsAffected := 0
  else
    Check(Res);

  if Res <> SQL_NO_DATA then begin
    Check(Cli.SQLRowCount(FStmt, ValueNInt));
    FRowsAffected := ValueNInt;
    case FParamsProcessedType of
      SQL_PARC_BATCH: begin
        for i := 0 to FBatchIters - 1 do
          if FParamsProcessedArray[i] = SQL_SUCCESS then
            Inc(FParamsProcessed);
        SetLength(FParamsProcessedArray, 0);
      end;
      SQL_PARC_NO_BATCH:
        if not FBatchSupported then
          Inc(FParamsProcessed);
    else
      FParamsProcessed := 0;
    end;
  end;

  // for stored procedures
  if not FConnection.FDetectFieldsOnPrepare or (FCommandType = ctStatement) then begin
    DetectCommandType;
    // sometimes without SQLMoreResultsODBC returns zero columns count for select queries
    if FCommandType = ctStatement then begin
      Res := Cli.SQLMoreResults(FStmt);
      if Res = SQL_SUCCESS then
        DetectCommandType;
    end;
  end;

  if not FLockReadOutParams then
    ReadOutParams;
end;

procedure TODBCCommand.BindParams;
var
  i, j, DataSize, Scale, ValPrec, ValScale, BufSize: integer;
  Param: TODBCParamDesc;
  ValPtr: IntPtr;
  DataAtExec: boolean;
  DataType: word;
  Blob: TBlob;
  Piece: PPieceHeader;
begin
  SetLength(FDataAtExecParams, 0);
  for i := 0 to Params.Count - 1 do begin
    Param := TODBCParamDesc(Params[i]);

    Param.AllocBindBuffer(FBatchIters);

    if Param.GetParamType in [pdUnknown, pdInput, pdInputOutput] then
      Param.WriteBuffer(FBatchIters, FBatchOffset, Param.GetParamType = pdInput, FConnection.IsEmptyStringAllowed,
        DataAtExec, ValPrec, ValScale)
    else begin
      DataAtExec := False;
      ValPrec := 0;
      ValScale := 0;
    end;

    if DataAtExec then begin
      SetLength(FDataAtExecParams, Length(FDataAtExecParams) + 1);
      FDataAtExecParams[High(FDataAtExecParams)] := i;
    end;

    {if (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) and
      (Param.ParamType in [pdOutput, pdResult])
    then
      continue;}

    DataType := Param.GetDataType;
    DataSize := Param.GetSize;
    Scale := Param.GetScale;
    case Param.GetDataType of
      dtBCD: begin
        if DataSize < 18 then
          DataSize := 18;
        if Scale < 4 then
          Scale := 4;
      end;
      dtFMTBCD: begin
        if DataSize < ValPrec then
          DataSize := ValPrec;
        if Scale < ValScale then
          Scale := ValScale;
      end;
      dtBlob, dtMemo, dtWideMemo:
        if (FBatchIters > 1) or (FBatchOffset > 0) then
          DataSize := 0
        else
          DataSize := MaxInt;
      dtUnknown:
        DataSize := 1;
    end;
    SetSpecificParamPrec(Param, DataSize, Scale);

    BufSize := Param.FBufferSize;
    if BufSize = -1 then
      BufSize := 0;

    if DataAtExec then
      ValPtr := IntPtr(NativeInt(i))
    else
      ValPtr := Param.FBuffer;

    if DataType in [dtBlob, dtMemo, dtWideMemo] then
      for j := 0 to FBatchIters - 1 do begin
        Blob := Param.GetBlob(FBatchOffset + j);

        if (Blob <> nil) and ((DataType = dtMemo) or (DataType = dtWideMemo)) then begin
          if Blob.IsUnicode then
            DataType := dtWideMemo
          else
            DataType := dtMemo
        end;

        if Blob <> nil then begin
          BufSize := Blob.Size;
          if BufSize > DataSize then
            DataSize := BufSize;
        end;

        if not DataAtExec then begin
          if Param.GetParamType = pdInput then begin
            if Blob <> nil then begin
              Blob.Defrag;
              Piece := Blob.FirstPiece;
              if IntPtr(Piece) <> nil then begin
                BufSize := Piece.Size;
                ValPtr := PtrOffset(Piece, SizeOf(TPieceHeader));
              end;
            end;
          end
          else begin
            if Blob = nil then
              raise Exception.Create(SCannotConvertType);

            if Param.GetSize <> 0 then
              BufSize := Param.GetSize
            else
              BufSize := DefaultStrParamSize;

            if DataType = dtWideMemo then
              BufSize := BufSize * 2;

            Blob.Defrag;
            Piece := Blob.FirstPiece;

            if IntPtr(Piece) = nil then begin
              Blob.AllocPiece(Piece, BufSize);
              Blob.AppendPiece(Piece);
            end
            else
            if Piece.Size < BufSize then
              Blob.ReallocPiece(Piece, BufSize);

            ValPtr := PtrOffset(Piece, SizeOf(TPieceHeader));
          end;
        end;
      end;

    Check(Cli.SQLBindParameter(FStmt, i + 1,
      GetODBCParamType(Param.GetParamType),
      GetCDataType(DataType, Param.GetSubDataType, False),
      GetSQLDataType(DataType, Param.GetSubDataType),
      DataSize, Scale, ValPtr, BufSize, Param.FIndicator));
  end;
end;

procedure TODBCCommand.ReadOutParams;
var
  i: integer;
  Param: TODBCParamDesc;
begin
  for i := 0 to Params.Count - 1 do begin
    Param := TODBCParamDesc(Params[i]);
    if Param.GetParamType <> pdInput then begin
      //if not (Param.DataType in [dtBlob, dtMemo, dtWideMemo]) then
        Param.ReadBuffer(FBatchIters, FBatchOffset);
    end;
  end;
end;

function TODBCCommand.GetODBCParamType(ParamType: TParamDirection): smallint;
begin
  case ParamType of
    pdInput, pdUnknown:
      Result := SQL_PARAM_INPUT;
    pdInputOutput:
      Result := SQL_PARAM_INPUT_OUTPUT;
    pdOutput, pdResult:
      Result := SQL_PARAM_OUTPUT;
  else
    Assert(False);
    Result := 0;
  end;
end;

function TODBCCommand.GetCDataType(DataType, SubDataType: word; NumericAsString: Boolean): smallint;
begin
  case DataType of
    dtFixedChar, dtString, dtExtString, dtMemo:
      Result := SQL_C_CHAR;
    dtFixedWideChar, dtWideString, dtExtWideString, dtWideMemo:
    {$IFDEF MACOS}
      Result := SQL_C_CHAR;
    {$ELSE}
      Result := SQL_C_WCHAR;
    {$ENDIF}
    dtBytes, dtVarBytes, dtExtVarBytes, dtBlob:
      Result := SQL_C_BINARY;
    dtUInt8:
      Result := SQL_C_UTINYINT;
    dtInt8:
      Result := SQL_C_STINYINT;
    dtWord:
      Result := SQL_C_USHORT;
    dtSmallint:
      Result := SQL_C_SSHORT;
    dtInteger:
      Result := SQL_C_SLONG;
    dtLongWord:
      Result := SQL_C_ULONG;
    dtInt64:
      Result := SQL_C_SBIGINT;
    dtUInt64:
      Result := SQL_C_UBIGINT;
    dtSingle, dtFloat, dtCurrency:
      if (SubDataType = dtNumeric) and NumericAsString then
        Result :=  SQL_C_CHAR
      else
        Result := SQL_C_DOUBLE;
    dtBoolean:
      Result := SQL_C_SSHORT;
    dtDate:
      Result := SQL_C_TYPE_DATE;
    dtTime:
      Result := SQL_C_TYPE_TIME;
    dtDateTime {$IFNDEF FPC},dtSQLTimeStamp{$ENDIF}:
      Result := SQL_C_TYPE_TIMESTAMP;
    dtBCD, dtFMTBCD:
      Result := SQL_C_CHAR;
    dtGuid:
      Result := SQL_C_CHAR;
    dtUnknown:
      Result := SQL_C_CHAR; // NULL as string
  else
    Assert(False);
    Result := 0;
  end;
end;

function TODBCCommand.GetSQLDataType(DataType, SubDataType: word): smallint;
begin
  case DataType of
    dtFixedChar:
      Result := SQL_CHAR;
    dtString, dtExtString:
      if SubDataType = dtFixedChar then
        Result := SQL_CHAR
      else
        Result := SQL_VARCHAR;
    dtMemo:
      Result := SQL_LONGVARCHAR;
    dtFixedWideChar:
      Result := SQL_WCHAR;
    dtWideString, dtExtWideString:
      if SubDataType = dtFixedWideChar then
        Result := SQL_WCHAR
      else
        Result := SQL_WVARCHAR;
    dtWideMemo:
      Result := SQL_WLONGVARCHAR;
    dtBytes, dtVarBytes, dtExtVarBytes:
      Result := SQL_VARBINARY;
    dtBlob:
      Result := SQL_LONGVARBINARY;
    dtInt8, dtUInt8:
      Result := SQL_TINYINT;
    dtSmallint, dtWord:
      Result := SQL_SMALLINT;
    dtInteger, dtLongWord:
      Result := SQL_INTEGER;
    dtInt64, dtUInt64:
      Result := SQL_BIGINT;
    dtSingle, dtFloat, dtCurrency:
      Result := SQL_DOUBLE;
    dtBoolean:
      Result := SQL_BIT;
    dtDate:
      Result := SQL_TYPE_DATE;
    dtTime:
      Result := SQL_TYPE_TIME;
    dtDateTime {$IFNDEF FPC},dtSQLTimeStamp{$ENDIF}:
      Result := SQL_TYPE_TIMESTAMP;
    dtBCD, dtFMTBCD:
      Result := SQL_NUMERIC;
    dtGuid:
      Result := SQL_GUID;
    dtUnknown:
      Result := SQL_VARCHAR; // NULL as string
  else
    Assert(False);
    Result := 0;
  end;
end;

procedure TODBCCommand.SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer);
begin
  // Empty
end;

function TODBCCommand.GetBlobSize(SQLLength: Cardinal): Integer;
begin
  if SQLLength >= Cardinal(MaxInt) then
    Result := 0
  else
    Result := Integer(SQLLength);
end;

function TODBCCommand.GetInternalType(SQLDataType: smallint; Unsigned: boolean): Word;
begin
  case SQLDataType of
    SQL_CHAR:
      Result := dtFixedChar;
    SQL_VARCHAR:
      Result := dtString;
    SQL_WCHAR, SQL_GRAPHIC:
      Result := dtFixedWideChar;
    SQL_WVARCHAR, SQL_VARGRAPHIC:
      Result := dtWideString;
    SQL_LONGVARCHAR:
      Result := dtMemo;
    SQL_WLONGVARCHAR, SQL_LONGVARGRAPHIC:
      Result := dtWideMemo;
    SQL_CLOB:
      Result := dtClobLocator;
    SQL_DBCLOB:
      Result := dtDBClobLocator;
    SQL_BIT:
      Result := dtBoolean;
    SQL_TINYINT:
      if Unsigned then
        Result := dtUInt8
      else
        Result := dtInt8;
    SQL_SMALLINT:
      if Unsigned then
        Result := dtWord
      else
        Result := dtSmallint;
    SQL_INTEGER:
      if Unsigned then
        Result := dtUInt32
      else
        Result := dtInteger;
    SQL_BIGINT:
      if Unsigned then
        Result := dtUInt64
      else
        Result := dtLargeint;
    SQL_REAL:
      Result := dtSingle;
    SQL_FLOAT, SQL_DOUBLE:
      Result := dtFloat;
    SQL_DECIMAL, SQL_NUMERIC:
      Result := dtNumeric;
    SQL_DECFLOAT:
      Result := dtFMTBCD;
    SQL_BINARY:
      Result := dtBytes;
    SQL_VARBINARY:
      Result := dtVarBytes;
    SQL_LONGVARBINARY:
      Result := dtBlob;
    SQL_BLOB, SQL_XML, SQL_XML_DB2, SQL_VARIANT:
      Result := dtBlobLocator;
    SQL_TYPE_DATE, SQL_DATE:
      Result := dtDate;
    SQL_TYPE_TIME, SQL_TIME:
      Result := dtTime;
    SQL_TYPE_TIMESTAMP, SQL_TIMESTAMP:
      Result := dtDateTime;
    SQL_GUID:
      Result := dtGuid;
  else
    Result := dtUnknown;
  end;
end;

procedure TODBCCommand.DetectDataType(SQLDataType: smallint; SQLLength: Cardinal; SQLScale: smallint;
  FetchConverter: TFetchConverter; out DataType, SubDataType: word; out Length, Scale: integer;
  out Fixed: boolean; Unsigned, LongStrings, FlatBuffers, FieldsAsString, UnknownAsString: boolean);
const
  MaxBCDIntPrec = MaxBcdPrecision - MaxBcdScale;
var
  IntPrec: integer;
begin
  Length := 0;
  Scale := 0;

  SubDataType := GetInternalType(SQLDataType, Unsigned);

  if FetchConverter <> nil then
    DataType := FetchConverter.InternalDataType
  else begin
    if FieldsAsString then begin
      case SQLDataType of
        SQL_CHAR, SQL_VARCHAR, SQL_LONGVARCHAR, SQL_CLOB,
        SQL_WCHAR, SQL_WVARCHAR, SQL_GRAPHIC, SQL_VARGRAPHIC,
        SQL_WLONGVARCHAR, SQL_LONGVARGRAPHIC, SQL_DBCLOB: ;
      else
        SQLDataType := SQL_VARCHAR;
        SQLLength := 255;
      end;
    end else if SubDataType = dtUnknown then
      if UnknownAsString then begin
        SQLDataType := SQL_VARCHAR;
        SQLLength := 255;
      end
      else
        SQLDataType := SQL_LONGVARCHAR;

    case SQLDataType of
      SQL_CHAR, SQL_VARCHAR: begin
        if (SQLLength > 0) and ((SQLLength < 255) or
           (LongStrings and (SQLLength <= DefaultStrFieldLength)))
        then
          DataType := dtString
        else
          DataType := dtMemo;

        Length := SQLLength;
      {$IFDEF MSWINDOWS}{$IFDEF IS_UTF8}
        Length := Length * 4;
      {$ENDIF}{$ENDIF}
      end;
      SQL_WCHAR, SQL_WVARCHAR, SQL_GRAPHIC, SQL_VARGRAPHIC: begin
        if (SQLLength > 0) and ((SQLLength < 255) or
            (LongStrings and (SQLLength <= DefaultStrFieldLength)))
        then
          DataType := dtWideString
        else
          DataType := dtWideMemo;

        Length := SQLLength;
      end;
      SQL_LONGVARCHAR:
        DataType := dtMemo;
      SQL_WLONGVARCHAR, SQL_LONGVARGRAPHIC:
        DataType := dtWideMemo;
      SQL_CLOB:
        DataType := dtMemo;
      SQL_DBCLOB:
        DataType := dtWideMemo;
      SQL_BIT: begin
        DataType := dtBoolean;
        Length := SQLLength;
      end;
      SQL_TINYINT: begin
        if Unsigned then
          DataType := dtUInt8
        else
          DataType := dtInt8;
        Length := SQLLength;
      end;
      SQL_SMALLINT: begin
        if Unsigned then
          DataType := dtWord
        else
          DataType := dtSmallint;
        Length := SQLLength;
      end;
      SQL_INTEGER: begin
        if Unsigned then
          DataType := dtUInt32
        else
          DataType := dtInteger;
        Length := SQLLength;
      end;
      SQL_BIGINT: begin
        DataType := dtLargeint;
        Length := SQLLength;
      end;
      SQL_REAL: begin
        DataType := dtFloat;
        Length := SQLLength;
        Scale := SQLScale;
      end;
      SQL_FLOAT, SQL_DOUBLE: begin
        DataType := dtFloat;
        Length := SQLLength;
        Scale := SQLScale;
      end;
      SQL_DECIMAL, SQL_NUMERIC: begin
        if EnableFMTBCD or FConnection.EnableFMTBCD then begin
          DataType := dtFMTBCD;
          if (SQLLength > MaxFMTBcdDigits) or (SQLLength <= 0) then
            Length := MaxFMTBcdDigits
          else
            Length := SQLLength;
          if Word(SQLScale) > Length then // if length was reduced
            Scale := Length
          else
            Scale := SQLScale;
        end
        else
        if EnableBCD or FConnection.EnableBCD then begin
          DataType := dtBCD;
          if SQLScale > MaxBcdScale then
            Scale := MaxBcdScale
          else
            Scale := SQLScale;

          IntPrec := Integer(SQLLength) - SQLScale;
          if IntPrec > MaxBCDIntPrec then
            Length := MaxBCDIntPrec + Scale
          else
            Length := IntPrec + Scale;
        end
        else begin
          DataType := dtFloat;
          Length := SQLLength;
          Scale := SQLScale;
        end;
      end;
      SQL_DECFLOAT: begin
        if EnableFMTBCD or FConnection.EnableFMTBCD then begin
          DataType := dtFMTBCD;
          // DECFLOAT34
          if (SQLLength = 34) and (SQLScale = 0) then begin
            Length := 32;
            Scale := 8;
          end
          // DECFLOAT16
          else if (SQLLength = 16) and (SQLScale = 0) then begin
            Length := 24;
            Scale := 8;
          end
          else begin
            if (SQLLength > MaxFMTBcdDigits) or (SQLLength <= 0) then
              Length := MaxFMTBcdDigits
            else
              Length := SQLLength;
            if SQLScale <= 0 then
              Scale := 8 // for TClientDataSet compatibility
            else begin
              Scale := SQLScale;
              if Scale > Length then // if length was reduced
                Scale := Length;
            end;
          end;
        end
        else begin
          DataType := dtString;
          if SQLLength <= 16 then
            Length := 64
          else if SQLLength <= 34 then
            Length := 129
          else
            Length := 255;
        end;
      end;
      SQL_BINARY: begin
        DataType := dtBytes;
        Length := SQLLength;
      end;
      SQL_VARBINARY, SQL_LONGVARBINARY: begin
        if (SQLLength <> 0) and (SQLLength <= DefaultStrFieldLength) then
          DataType := dtVarBytes
        else
          DataType := dtBlob;

        if SQLDataType = SQL_LONGVARBINARY then
          Length := GetBlobSize(SQLLength)
        else
          Length := SQLLength;
      end;
      SQL_BLOB, SQL_XML, SQL_XML_DB2, SQL_VARIANT:
        DataType := dtBlob;
      SQL_TYPE_DATE, SQL_DATE:
        DataType := dtDate;
      SQL_TYPE_TIME, SQL_TIME:
        DataType := dtTime;
      SQL_TYPE_TIMESTAMP, SQL_TIMESTAMP:
        DataType := dtDateTime;
      SQL_GUID: begin
        DataType := dtGuid;
        Length := GuidStrSize;
      end;
    else
      raise Exception.Create(SDataTypeNotSupported);
    end;

    if FConnection.FUseUnicode then
      case DataType of
        dtFixedChar:
          DataType := dtFixedWideChar;
        dtString:
          DataType := dtWideString;
        dtMemo:
          DataType := dtWideMemo;
      end;
  end;

  if DataType in [dtFixedWideChar, dtWideString, dtWideMemo] then
    case SubDataType of
      dtFixedChar:
        SubDataType := dtFixedWideChar;
      dtString:
        SubDataType := dtWideString;
      dtMemo:
        SubDataType := dtWideMemo;
    end;

  if SubDataType in [dtFixedChar, dtFixedWideChar] then
    Fixed := True
  else
    Fixed := False;

  if not FlatBuffers and not Fixed and (Length >= FlatBufferLimit) then
    if DataType = dtString then
      DataType := dtExtString
    else if DataType = dtWideString then
      DataType := dtExtWideString
    else if DataType = dtVarBytes then
      DataType := dtExtVarBytes;
end;

function TODBCCommand.DetectSpecificType(const TypeName: string; var SQLLength: Integer; var SQLScale: SmallInt): integer;
begin
{$IFDEF FPC}
  Result := 0; // Lazarus anti-warning
{$ENDIF}
  raise Exception.Create(SDataTypeNotSupported);
end;

procedure TODBCCommand.CreateBatchCommand;
begin
  // Empty
end;

procedure TODBCCommand.InternalExecuteBatch(Iters, Offset: integer);
begin
  FBatchIters := Iters;
  FBatchOffset := Offset;

  try
    FParamsProcessed := 0;
    Check(Cli.SQLGetInfoInt(FConnection.FSQLHDbc, SQL_PARAM_ARRAY_ROW_COUNTS, FParamsProcessedType, 4, nil));
    case FParamsProcessedType of
      SQL_PARC_NO_BATCH:
        Check(Cli.SQLSetStmtAttrPtr(FStmt, SQL_ATTR_PARAMS_PROCESSED_PTR, @FParamsProcessed, 0));
      SQL_PARC_BATCH: begin
        SetLength(FParamsProcessedArray, FBatchIters);
        Check(Cli.SQLSetStmtAttrPtr(FStmt, SQL_ATTR_PARAM_STATUS_PTR, @FParamsProcessedArray[0], 0));
      end;
    end;

    Execute;
  finally
    FParamsProcessedType := 0;
  end;
end;

function TODBCCommand.WriteBlobs: integer;
var
  MaxParamIndex, ParamIndex, ValueIndex: integer;
  Param: TODBCParamDesc;
  ValPtr: IntPtr;
begin
  MaxParamIndex := High(FDataAtExecParams);
  ParamIndex := 0;
  ValueIndex := FBatchOffset;
  repeat
    Result := Cli.SQLParamData(FStmt, ValPtr);
    if Result = SQL_NO_DATA then
      Exit
    else
    if Result <> SQL_NEED_DATA then begin
      Check(Result);
      Exit;
    end;

    Param := TODBCParamDesc(Params[FDataAtExecParams[ParamIndex]]);
    WriteBlob(Param.ItemValue[ValueIndex], Param.GetDataType);
    Inc(ParamIndex);
    if ParamIndex > MaxParamIndex then begin
      ParamIndex := 0;
      Inc(ValueIndex);
    end;
  until False;
end;

procedure TODBCCommand.WriteBlob(const Value: variant; DataType: word);
var
  sa: AnsiString;
  ws: WideString;
  Obj: TObject;
  Blob: TBlob;
  Piece: PPieceHeader;
  BlobData: IntPtr;
  len: integer;
  lb, hb: integer;
begin
  case VarType(Value) of
    varSharedObject: begin
      Obj := TObject(TVarData(Value).VPointer);
      if not (Obj is TBlob) then
        raise Exception.Create(SCannotConvertType);
      Blob := TBlob(Obj);
      Piece := Blob.FirstPiece;
      while IntPtr(Piece) <> nil do begin
        BlobData := PtrOffset(Piece, SizeOf(TPieceHeader));
        Check(Cli.SQLPutData(FStmt, BlobData, Piece.Used));
        Piece := Piece.Next;
      end;
    end;
    varArray or varByte: begin
      if VarArrayDimCount(Value) <> 1 then
        raise Exception.Create(SCannotConvertType);
      lb := VarArrayLowBound(Value, 1);
      hb := VarArrayHighBound(Value, 1);
      BlobData := VarArrayLock(Value);
      try
        Check(Cli.SQLPutData(FStmt, BlobData, hb - lb + 1));
      finally
        VarArrayUnlock(Value);
      end;
    end;
  else
    if DataType = dtWideMemo then begin
      ws := VarToWideStr(Value);
      BlobData := Marshal.StringToHGlobalUni(ws);
      len := Length(ws) * 2;
    end
    else begin
      sa := AnsiString(VarToStr(Value));
      BlobData := Marshal.StringToHGlobalAnsi(sa);
      len := Length(sa);
    end;

    try
      Check(Cli.SQLPutData(FStmt, BlobData, len));
    finally
      FreeString(BlobData);
    end;
  end;
end;

function TODBCCommand.ReadBlob(Blob: TBlob; ActualFieldNo: integer; DataType: word;
  var SharedPiece: PPieceHeader): boolean;
var
  Res: SmallInt;
  Len: Integer;
  CDataType: SmallInt;
  ValueNInt: NativeInt;
  CurPieceSize: Integer;
  Piece, Piece2: PPieceHeader;
  NullTerminatorSize: Cardinal;
begin
  CDataType := GetCDataType(DataType, 0, False);

  case CDataType of
    SQL_C_CHAR:
      NullTerminatorSize := 1;
    SQL_C_WCHAR:
      NullTerminatorSize := 2;
    else
      NullTerminatorSize := 0;
  end;

  CurPieceSize := DefaultPieceSize;

  Result := True;
  repeat
    if IntPtr(SharedPiece) = nil then
      Blob.AllocPiece(Piece, CurPieceSize)
    else if CurPieceSize > DefaultPieceSize then
      Blob.AllocPiece(Piece, CurPieceSize)
    else
      Piece := SharedPiece;

    Res := Cli.SQLGetData(FStmt, ActualFieldNo, CDataType,
      PtrOffset(Piece, SizeOf(TPieceHeader)), CurPieceSize, ValueNInt);
    Len := ValueNInt;

    if (Res = SQL_SUCCESS_WITH_INFO) and (Len = SQL_NO_TOTAL) then
      Len := Blob.PieceSize;

    if Res = SQL_NO_DATA then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      break;
    end;

    if IsODBCError(Res) then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      Check(Res);
    end;

    if (Len = SQL_NULL_DATA) or (Len = 0) then begin
      if IntPtr(Piece) <> IntPtr(SharedPiece) then
        Marshal.FreeHGlobal(Piece);
      Result := False;
      break;
    end;

    // for SQL_C_CHAR and  SQL_C_WCHAR
    // Len = Length(string) for last part of text
    // Len = Length(string) + NullTerminatorSize for previous parts of text
    if Len >= Piece.Size then begin
      Piece.Used := Piece.Size - Integer(NullTerminatorSize);

      CurPieceSize := Len - Piece.Used + Integer(NullTerminatorSize);
      if CurPieceSize > Blob.LargePieceSize then
        CurPieceSize := Blob.LargePieceSize
      else if CurPieceSize < DefaultPieceSize then
        CurPieceSize := DefaultPieceSize;
    end
    else
      Piece.Used := Len;

    if Piece = SharedPiece then begin
      if Piece.Used < Piece.Size div 2 then begin
        Blob.AllocPiece(Piece2, Piece.Used);
        CopyBuffer(PtrOffset(Piece, SizeOf(TPieceHeader)),
          PtrOffset(Piece2, SizeOf(TPieceHeader)), Piece.Used);
        Piece2.Used := Piece.Used;
        Piece := Piece2;
      end
      else
        SharedPiece := nil;
    end
    else
      if Piece.Used < Piece.Size div 2 then
        Blob.CompressPiece(Piece);

    Blob.AppendPiece(Piece);
  until Res = SQL_SUCCESS;
end;

function TODBCCommand.RemoveComments(const Value: string): string;
var
  Parser: TSQLParser;
  Lexem: string;
begin
  Parser := GetParserClass.Create(Value);
  try
    Parser.OmitBlank := False;
    Parser.OmitComment := True;
    Parser.Uppered := False;
    Parser.QuotedString := True;

    Result := '';
    while Parser.GetNext(Lexem) <> lcEnd do
      Result := Result + Lexem;
  finally
    Parser.Free;
  end;
end;

{ TODBCRecordSet }

constructor TODBCRecordSet.Create;
begin
  inherited Create;

  FFetchAll := True;
  FFetchRows := 25;
  FRowsObtainedBuf := Marshal.AllocHGlobal(SizeOf(IntPtr));
end;

destructor TODBCRecordSet.Destroy;
begin
  Close;
  Marshal.FreeHGlobal(FRowsObtainedBuf);

  inherited;
end;

function TODBCRecordSet.GetFieldDescType: TFieldDescClass;
begin
  Result := TODBCFieldDesc;
end;

function TODBCRecordSet.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      FCommand.FCommandTimeout := Value;
    prFieldsAsString:
      FFieldsAsString := Value;
    prOpenNext:
      FOpenNext := Value;
    prUnknownAsString:
      FFieldsAsString := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TODBCRecordSet.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prCommandTimeout:
      Value := FCommand.FCommandTimeout;
    prFieldsAsString:
      Value := FFieldsAsString;
    prOpenNext:
      Value := FOpenNext;
    prUnknownAsString:
      Value := FUnknownAsString;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TODBCRecordSet.CheckNextResult: Boolean;
var
  Res: Smallint;
begin
  Result := False;
  try
    if FCommand.FStmt = nil then
      Exit;

    if not FHasMoreResults then begin
      Cli.SQLFreeStmt(FCommand.FStmt, SQL_UNBIND);

      Res := Cli.SQLMoreResults(FCommand.FStmt);
      if Res = SQL_NO_DATA then
        Exit
      else
        Check(Res);
    end;
    FHasMoreResults := False;

    FCommand.DetectCommandType;
    if FCommand.CommandType = ctCursor then begin
      FCommand.SetCursorState(csExecuted); // to prevent endless loop, just let it Fetch below
      Result := True;
    end;

  finally
    if Result = False then begin
      FOpenNext := False;
      InternalClose;
    end;
  end;
end;

procedure TODBCRecordSet.ExecCommand(Iters: integer = 1; Offset: integer = 0);
var
  NeedPrepare: boolean;
begin
  FCommand.FRecordSetExec := True;
  try
    FHasMoreResults := False;

    NeedPrepare := not Prepared;
    if NeedPrepare then
      // performance issue
      // call inherited to avoid call DetectCommandType before Command execute
      inherited InternalPrepare;
    try
      inherited;

      // performance issue
      // call DetectCommandType after Command execute on implicit Command prepare
      if NeedPrepare then
        FCommand.DetectCommandType;

      if FCommand.CommandType = ctCursor then
        FCommand.SetCursorState(csExecuted)
    except
      if NeedPrepare then
        InternalUnprepare;
      raise;
    end;

    if FCommand.CommandType <> ctCursor then
      if NeedPrepare then
        InternalUnprepare;

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

procedure TODBCRecordSet.SetToEnd;
begin
  FetchAll;

  inherited;
end;

function TODBCRecordSet.GetNull(Field: TFieldDesc; RecBuf: IntPtr): boolean;
begin
  if Field.FieldDescKind <> fdkCalculated then
    Result := Marshal.ReadInt32(RecBuf, FDataSize + (Field.FieldNo - 1) * SizeOf(IntPtr)) = SQL_NULL_DATA
  else
    Result := Marshal.ReadInt32(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * SizeOf(IntPtr)) = SQL_NULL_DATA;
  if Result then
    Result := GetNullByBlob(Field, RecBuf);
end;

procedure TODBCRecordSet.SetNull(Field: TFieldDesc; RecBuf: IntPtr; Value: boolean);
var
  Flag: integer;
  Blob: TBlob;
begin
  if Value then
    Flag := SQL_NULL_DATA
  else
    Flag := 0;

  if Field.FieldDescKind <> fdkCalculated then
    Marshal.WriteNativeInt(RecBuf, FDataSize + (Field.FieldNo - 1) * SizeOf(IntPtr), NativeInt(Flag))
  else
    Marshal.WriteNativeInt(RecBuf, FRecordSize + FCalcDataSize + (Field.ActualFieldNo - 1) * SizeOf(IntPtr), NativeInt(Flag));

  if Value and Field.IsBlob then begin // clear Blob value
    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf, Field.DataOffset)));
    if Blob <> nil then
      Blob.Clear;
  end;

{$IFNDEF LITE}
  SetEncrypted(Field, RecBuf, not Value);
{$ENDIF}
end;

procedure TODBCRecordSet.Check(ErrorCode: smallint);
begin
  if IsODBCError(ErrorCode) then
    FCommand.FConnection.ProcessError(SQL_HANDLE_STMT, FCommand.FStmt, ErrorCode, Component);
end;

procedure TODBCRecordSet.CreateCommand;
begin
  SetCommand(TODBCCommand.Create);
end;

procedure TODBCRecordSet.SetCommand(Value: TCRCommand);
begin
  inherited;

  FCommand := TODBCCommand(Value);
end;

procedure TODBCRecordSet.InternalPrepare;
begin
  inherited;

  try
    FHasMoreResults := False;

    if FCommand.FConnection.FDetectFieldsOnPrepare then
      FCommand.DetectCommandType;
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TODBCRecordSet.InternalUnPrepare;
begin
  if FOpenNext then
    Exit;

  FHasMoreResults := False;

  inherited;

  FCommand.CommandType := ctUnknown;
end;

function TODBCRecordSet.Cli: TODBCCliAPI;
begin
  Result := FCommand.Cli;
end;

class function TODBCRecordSet.GetBufferSize(DataType: Word; LengthInChars: integer): integer;
begin
  case DataType of
    dtInt8, dtUint8:
      Result := SizeOf(Byte);
  else
    Result := inherited GetBufferSize(DataType, LengthInChars);
  end;
end;

procedure TODBCRecordSet.DescribeFieldDesc(Field: TCRFieldDesc; Index: integer);
const
  BufSize = 256;
var
  Buf: IntPtr;
  TableName: string;
  TableInfo: TCRTableInfo;
  ValueSize: smallint;
  SQLDataType: smallint;
  SQLScale: smallint;
  Nullable: smallint;
  ValueUNInt: NativeUInt;
  ValueNInt: NativeInt;
  SQLLength: Cardinal;
  DataType, SubDataType: word;
  Len, Scale: integer;
  Fixed, Unsigned: boolean;
  FetchConverter: TFetchConverter;
begin
  Field.FieldNo := Index + 1;
  Field.ActualFieldNo := Index + 1;

  Buf := Marshal.AllocHGlobal(BufSize * ODBCCharSize);
  try
  {$IFDEF USE_UNICODE_DRIVER}
    Check(Cli.SQLDescribeColW(FCommand.FStmt, Index + 1, Buf, BufSize, ValueSize,
      SQLDataType, ValueUNInt, SQLScale, Nullable));
  {$ELSE}
    Check(Cli.SQLDescribeColA(FCommand.FStmt, Index + 1, Buf, BufSize, ValueSize,
      SQLDataType, ValueUNInt, SQLScale, Nullable));
  {$ENDIF}

    if FCommand.FConnection.FVarBinaryAsBlob and (SQLDataType = SQL_VARBINARY) then
      SQLLength := 0
    else if FCommand.FConnection.FLongVarBinaryAsBlob and (SQLDataType = SQL_LONGVARBINARY) then
      SQLLength := 0
    else
      SQLLength := ValueUNInt;

    Field.Name := FromODBCString(Buf);
    Field.ActualName := Field.Name;

  {$IFDEF USE_UNICODE_DRIVER}
    Check(Cli.SQLColAttributeW(FCommand.FStmt, Index + 1, SQL_DESC_TABLE_NAME, Buf, BufSize, ValueSize, nil));
  {$ELSE}
    Check(Cli.SQLColAttributeA(FCommand.FStmt, Index + 1, SQL_DESC_TABLE_NAME, Buf, BufSize, ValueSize, nil));
  {$ENDIF}
    TableName := FromODBCString(Buf);
    if TableName <> '' then begin
      TableInfo := FTablesInfo.FindByName(TableName);
      if TableInfo = nil then begin
        TableInfo := FTablesInfo.Add;
        TableInfo.TableName := TableName;
        TableInfo.TableAlias := '';
      end;
      Field.TableInfo := TableInfo;
    end;
  finally
    Marshal.FreeHGlobal(Buf);
  end;

  Field.Required := Nullable = SQL_NO_NULLS;

  Check(Cli.SQLColAttributeInt(FCommand.FStmt, Index + 1, SQL_DESC_AUTO_UNIQUE_VALUE,
    nil, 0, ValueSize, ValueNInt));

  if ValueNInt = SQL_TRUE then begin
    Field.IsAutoIncrement := True;
    Field.Required := False;
  end;

  Check(Cli.SQLColAttributeInt(FCommand.FStmt, Index + 1, SQL_DESC_UPDATABLE,
    nil, 0, ValueSize, ValueNInt));

  if ValueNInt = SQL_ATTR_READONLY then
    Field.ReadOnly := True;

  Check(Cli.SQLColAttributeInt(FCommand.FStmt, Index + 1, SQL_DESC_UNSIGNED,
    nil, 0, ValueSize, ValueNInt));
  Unsigned := ValueNInt = SQL_TRUE;

  Field.DBType := TODBCConverterManager(DataTypeMap.GetConverterManager).GetDBType(SQLDataType, Unsigned);
  Field.DBLength := SQLLength;
  Field.DBScale := SQLScale;

  FetchConverter := GetMapFetchConverter(Field.Name, Field.DBType, Field.DBLength, Field.DBScale);

  FCommand.DetectDataType(SQLDataType, SQLLength, SQLScale, FetchConverter,
    DataType, SubDataType, Len, Scale, Fixed, Unsigned, FLongStrings, FFlatBuffers,
    FFieldsAsString, FUnknownAsString);

  Field.DataType := DataType;
  Field.SubDataType := SubDataType;
  Field.Length := Len;
  Field.Scale := Scale;
  Field.Fixed := Fixed;
  Field.Size := GetBufferSize(Field.DataType, Field.Length);

  TODBCFieldDesc(Field).FetchFieldSize := GetFieldFetchBlockSize(Field);
  TODBCFieldDesc(Field).BindFieldSize := GetFieldBindSize(Field);
end;

procedure TODBCRecordSet.CreateFieldDescs;
var
  OldCursorState: TCursorState;
  Field: TCRFieldDesc;
  FieldsCount: smallint;
  i: integer;
begin
  OldCursorState := FCommand.GetCursorState;
  if OldCursorState = csInactive then
    InternalPrepare;

  try
    if FCommand.CommandType <> ctCursor then
      raise Exception.Create(SNotRows);

    FTablesInfo.CaseSensitive := FCommand.SQLInfo.IdentCase <> icMixed;
    FCommand.SQLInfo.ParseTablesInfo(FCommand.SQL, FTablesInfo);

    FHasConvertedFields := False;
    Check(Cli.SQLNumResultCols(FCommand.FStmt, FieldsCount));
    for i := 0 to FieldsCount - 1 do begin
      Field := TCRFieldDesc(CreateFieldDesc);
      try
        DescribeFieldDesc(Field, i);
        FHasConvertedFields := FHasConvertedFields or TODBCFieldDesc(Field).IsConverted;
      except
        Field.Free;
        raise;
      end;

      FFields.Add(Field);
    end;

  finally
    if OldCursorState = csInactive then
      InternalUnPrepare;
  end;
end;

function TODBCRecordSet.IdentityFieldIsData: boolean;
begin
  Result := True;
end;

procedure TODBCRecordSet.InternalClose;
begin
  if FOpenNext then
    Exit;

  FCommand.Close;

  FHasMoreResults := False;

  if FCommand.GetCursorState > csPrepared then
    FCommand.SetCursorState(csPrepared);

  inherited;

  if not Prepared then
    InternalUnprepare;
end;

function TODBCRecordSet.GetIndicatorItemSize: Integer;
begin
  Result := SizeOf(IntPtr);
end;

procedure TODBCRecordSet.FetchBlock(Block: PBlockHeader; FetchBack: boolean; out RowsObtained: Integer);
var
  Item, RecBuf: IntPtr;
  Res: smallint;
  SharedPiece: PPieceHeader;
begin
  if not FFetchBufferIsBinded then
    if not FFetchMode.OneByOne then
      // some drivers for ODBC ver. 3 don't support Block Fetch
      // in this case BindColumnsForBlockFetch returns error
      if not IsODBCError(BindColumnsForBlockFetch) then
        // If the specified rowset size exceeds the maximum rowset size supported by the data source,
        // the driver substitutes that value and returns SQLSTATE 01S02 (Option value changed)
        // with status SQL_SUCCESS_WITH_INFO
        Check(Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_ARRAY_SIZE, FetchRows, 0))
      else begin
        Check(Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_ARRAY_SIZE, 1, 0));
        FFetchMode.OneByOne := True;
        FFetchMode.ByColumns := False;
      end
    else begin
      Check(Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_ARRAY_SIZE, 1, 0));
      FFetchMode.ByColumns := False;
    end;

  Item := PtrOffset(Block, SizeOf(TBlockHeader));
  RecBuf := PtrOffset(Item, SizeOf(TItemHeader));

  if not FFetchMode.OneByOne then begin
    if not FFetchBufferIsBinded then
      BindColumns(RecBuf, FFetchRows);
    Res := Cli.SQLFetch(FCommand.FStmt);
    if Res = SQL_NO_DATA then
      RowsObtained := 0
    else begin
      Check(Res);
      RowsObtained := Marshal.ReadInt32(FRowsObtainedBuf);
    end;

    // convert fetched data
    ReadFetchBlock(FFetchBuffer, RecBuf, RowsObtained, SharedPiece);
  end
  else begin
    RowsObtained := 0;
    SharedPiece := nil;
    while True do begin
      BindColumns(RecBuf, 1);
      Res := Cli.SQLFetch(FCommand.FStmt);
      if Res = SQL_NO_DATA then
        Break
      else
        Check(Res);

      Inc(RowsObtained);
      ReadFetchBlock(FFetchBuffer, RecBuf, 1, SharedPiece);

      if RowsObtained = FFetchRows then
        Break;

      RecBuf := PtrOffset(RecBuf, RecordSize + SizeOf(TItemHeader));
    end;

    if IntPtr(SharedPiece) <> nil then
      Marshal.FreeHGlobal(SharedPiece);
  end;

{$IFNDEF LITE}
  if FSmartFetchState = sfDataByKey then
    FCommand.SetCursorState(csFetched)
  else
{$ENDIF}
  if RowsObtained < FFetchRows then begin
    FCommand.SetCursorState(csFetched);
    FHasMoreResults := FCommand.CheckMoreResults;
  end;
end;

function TODBCRecordSet.NeedUnPrepareAfterFetch: boolean;
begin
  Result := (FSmartFetchState = sfDataByKey) or
            (not Prepared and not FHasMoreResults);
end;

function TODBCRecordSet.ExtFieldsInfoIsInternal: boolean;
begin
  Result := False;
end;

procedure TODBCRecordSet.RequestFieldsInfo(Tables: TSQLObjectsInfo; Columns: TCRColumnsInfo);
var
  i, p: integer;
  ColumnInfo, NewColumnInfo: TCRColumnInfo;
  Located: boolean;
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
  SQLInfo: TSQLInfo;
  IdentCase: TIdentCase;
  ColumnNameField: TFieldDesc;
  ColumnDefField: TFieldDesc;

  function Locate1(Field: TFieldDesc; Value: string): boolean;
  var
    v: variant;
  begin
    RecordSet.SetToBegin;
    while True do begin
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then
        break;
      RecordSet.GetFieldAsVariant(Field, RecBuf, v);
      if (IdentCase <> icMixed) and (VarToStr(v) = Value) or
        (IdentCase = icMixed) and SameText(VarToStr(v), Value)
      then begin
        Result := True;
        Exit;
      end;
    end;
    Result := False;
  end;

begin
  for i := 0 to High(Tables) do
    with Tables[i] do begin
      if (FFieldOrigins <> foNone) or (i = FUpdatingTableInfoIdx) then
        Flag := 1
      else
        Flag := 0; // don't query fields for this table
    end;

  SQLInfo := FCommand.SQLInfo;
  IdentCase := SQLInfo.IdentCase;
  RecordSet := FCommand.FConnection.GetMetaDataRecordSet;
  try
    TODBCMetaDataCommand(RecordSet.GetCommand).MetaDataKind := mkColumns;

    for i := 0 to High(Tables) do begin
      if Tables[i].Flag = 0 then
        continue;

      with TODBCMetaDataCommand(RecordSet.GetCommand).MetaDataArgs do begin
        CatalogName := Tables[i].Catalog;
        SchemaName := Tables[i].Schema;
        ObjectName := Tables[i].Name;
        ColumnName := '%';
      end;

      RecordSet.Open;
      RecordSet.AllocRecBuf(RecBuf);
      try
        p := 0;
        while p < Columns.Count do begin
          ColumnInfo := Columns[p];
          if (ColumnInfo.TableIndex <> -1) and (ColumnInfo.TableIndex <> i) then begin
            Inc(p);
            continue;
          end;

          ColumnNameField := RecordSet.Fields[3];
          ColumnDefField := RecordSet.Fields[12];

          if ColumnInfo.Name = '*' then begin
            RecordSet.SetToBegin;
            repeat
              RecordSet.GetNextRecord(RecBuf);
              if RecordSet.Eof then
                break;

              Inc(p);
              NewColumnInfo := TCRColumnInfo.Create;
              Columns.Insert(p, NewColumnInfo);
              NewColumnInfo.Table := ColumnInfo.Table;
              NewColumnInfo.TableIndex := i;

              RecordSet.GetFieldAsVariant(ColumnNameField, RecBuf, v);
              NewColumnInfo.Name := VarToStr(v);
              NewColumnInfo.Alias := NewColumnInfo.Name;

              if FDefaultValues and (i = FUpdatingTableInfoIdx) then begin
                RecordSet.GetFieldAsVariant(ColumnDefField, RecBuf, v);
                NewColumnInfo.Expr := VarToStr(v);
              end;
            until False;
          end
          else
          if (ColumnInfo.Name <> '') then begin
            if ColumnInfo.TableIndex <> -1 then begin
              if FDefaultValues and (i = FUpdatingTableInfoIdx) then
                Located := Locate1(ColumnNameField, ColumnInfo.Name)
              else
                Located := False;
            end
            else
              Located := Locate1(ColumnNameField, ColumnInfo.Name);

            if Located then begin
              if FDefaultValues then begin
                RecordSet.GetFieldAsVariant(ColumnDefField, RecBuf, v);
                ColumnInfo.Expr := VarToStr(v);
              end;

              ColumnInfo.TableIndex := i;
            end;
          end;

          Inc(p);
        end;
      finally
        RecordSet.FreeRecBuf(RecBuf);
      end;
      RecordSet.Close;
    end;
  finally
    RecordSet.Free;
  end;
end;

class function TODBCRecordSet.IsConvertedFieldType(DataType: word): boolean;
begin
  case DataType of
  {$IFDEF MSWINDOWS}
    {$IFDEF IS_UTF8}
    dtFixedChar, dtString, dtExtString:
      Result := True;
    {$ENDIF}
  {$ELSE}
    {$IFNDEF IS_UTF8}
    dtFixedChar, dtString, dtExtString:
      Result := True;
    {$ENDIF}
  {$ENDIF}
  {$IFDEF MACOS}
    dtFixedWideChar, dtWideString, dtExtWideString:
      Result := True;
  {$ENDIF}
    dtVarBytes, dtDate, dtTime, dtDateTime, dtGuid, dtBCD, dtFMTBCD:
      Result := True;
  else
    Result := False;
  end;
end;

function TODBCRecordSet.NumericAsString: Boolean;
begin
  Result := False;
end;

function TODBCRecordSet.GetFieldFetchBlockSize(Field: TFieldDesc): integer;

  function GetAnsiStringBufferSize: Integer;
  begin
    Result := Field.Length + 1;
  end;

  function GetWideStringBufferSize: Integer;
  begin
    Result := (Field.Length + 1) * 2;
  end;

begin
  case Field.DataType of
    dtFixedChar, dtString, dtExtString:
      Result := GetAnsiStringBufferSize;
    dtFixedWideChar, dtWideString, dtExtWideString:
      Result := GetWideStringBufferSize;
    dtMemo:
      if Field.Length <> 0 then
        Result := GetAnsiStringBufferSize
      else
        Result := 0;
    dtWideMemo:
      if Field.Length <> 0 then
        Result := GetWideStringBufferSize
      else
        Result := 0;
    dtVarBytes, dtExtVarBytes:
      Result := Field.Length;
    dtBlob:
      if Field.Length <> 0 then
        Result := Field.Length
      else
        Result := 0;
    dtDate:
      Result := SizeOf(TSQLDateStruct);
    dtTime:
      Result := SizeOf(TSQLTimeStruct);
    dtDateTime:
      Result := SizeOf(TSQLTimeStampStruct);
    dtBCD, dtFMTBCD:
      Result := BCDStrSize + 1;
    dtGuid:
      Result := GuidStrSize + 1;
    dtSingle, dtFloat, dtCurrency:
      if (Field.SubDataType = dtNumeric) and NumericAsString then
        Result := BCDStrSize + 1
      else
        Result := Field.Size;
  else
    Result := Field.Size;
  end;

  Result := Result + (Result and 1); // align 2
end;

function TODBCRecordSet.GetFieldBindSize(Field: TFieldDesc): integer;

  function GetAnsiStringBufferSize: Integer;
  begin
    Result := Field.Length + 1;
  end;

  function GetWideStringBufferSize: Integer;
  begin
    Result := (Field.Length + 1) * 2;
  end;

begin
  case Field.DataType of
    dtFixedChar, dtString, dtExtString:
      Result := GetAnsiStringBufferSize;
    dtFixedWideChar, dtWideString, dtExtWideString:
      Result := GetWideStringBufferSize;
    dtMemo:
      if Field.Length <> 0 then
        Result := GetAnsiStringBufferSize
      else
        Result := -1;
    dtWideMemo:
      if Field.Length <> 0 then
        Result := GetWideStringBufferSize
      else
        Result := -1;
    dtBytes, dtVarBytes, dtExtVarBytes:
      Result := Field.Length;
    dtBlob:
      if Field.Length <> 0 then
        Result := Field.Length
      else
        Result := -1;
    dtBCD, dtFMTBCD:
      Result := BCDStrSize + 1;
    dtGuid:
      Result := GuidStrSize + 1;
  else
    Result := 0;
  end;

  if FFetchMode.ByColumns and (Result <> -1) then
    Result := Result + (Result and 1); // align 2
end;

procedure TODBCRecordSet.AllocFetchBuffer;
var
  i: integer;
  Field: TODBCFieldDesc;
begin
  FFetchMode.HasLobs := False;
  FFetchMode.OneByOne := (FFetchRows = 1) or not FCommand.FConnection.IsBlockFetchAllowed;
  FFetchMode.ByColumns := FCommand.FConnection.FColumnWiseBinding;

  if FFetchBuffer <> nil then
    FreeFetchBuffer;

  for i := 0 to FFields.Count - 1 do begin
    Field := TODBCFieldDesc(FFields[i]);
    if Field.FetchFieldSize <= 0 then
      FFetchMode.HasLobs := True;
  end;

  if FFetchMode.HasLobs then
    FFetchMode.OneByOne := True;

  FFetchBufferDataSize := 0;
  for i := 0 to FFields.Count - 1 do begin
    Field := TODBCFieldDesc(FFields[i]);
    if FFetchMode.ByColumns then begin
      Field.FetchFieldOffset := FFetchBufferDataSize;
      FFetchBufferDataSize := FFetchBufferDataSize + (Field.FetchFieldSize + SizeOf(IntPtr))* FFetchRows;
    end
    else if not FFetchMode.OneByOne or Field.IsComplex or TODBCFieldDesc(Field).IsConverted then begin
      Field.FetchFieldOffset := FFetchBufferDataSize;
      FFetchBufferDataSize := FFetchBufferDataSize + Field.FetchFieldSize;
    end
    else
      Field.FetchFieldOffset := -1
  end;

  if FFetchMode.ByColumns  then begin
    FFetchBufferSize := FFetchBufferDataSize; // with Indicators
    FFetchBufferRowSize := 0;
    FFetchBufferDataSize := 0;
  end
  else if FFetchMode.OneByOne then begin
    FFetchBufferRowSize := FFetchBufferDataSize;
    FFetchBufferSize := FFetchBufferRowSize;
  end
  else begin
    FFetchBufferRowSize := FFetchBufferDataSize + FFields.Count * SizeOf(IntPtr);
    FFetchBufferSize := FFetchBufferRowSize * FFetchRows;
  end;

  if FFetchBufferSize > 0 then
    FFetchBuffer := Marshal.AllocHGlobal(FFetchBufferSize);
end;

procedure TODBCRecordSet.FreeFetchBuffer;
begin
  FFetchBufferIsBinded := False;

  inherited;
end;

procedure TODBCRecordSet.InitBlock(Block: PBlockHeader);
var
  i: integer;
  Item, RecBuf: IntPtr;
begin
  if not HasComplexFields then
    Exit;

  Item := PtrOffset(Block, SizeOf(TBlockHeader));
  for i := 0 to FFetchRows - 1 do begin
    RecBuf := PtrOffset(Item, sizeof(TItemHeader));
    CreateComplexFields(RecBuf, True);
    Item := PtrOffset(Item, RecordSize + sizeof(TItemHeader));
  end;
end;

procedure TODBCRecordSet.ReadFetchBlock(FetchBuffer: IntPtr; RecBuf: IntPtr; RowsObtained: Integer; var SharedPiece: PPieceHeader);
var
  i, j: integer;
  Ind: NativeInt;
  Field: TODBCFieldDesc;
  IndOffset: Integer;
  DestRowOffset: Integer;
  SourceSize: Integer;
  SourcePtr: IntPtr;
  SourceIndPtr: IntPtr;
  DestPtr: IntPtr;
  DestIndPtr: IntPtr;
  DestLenPtr: PWord;
begin
  IndOffset := 0;
  DestRowOffset := RecordSize + SizeOf(TItemHeader);

  for j := 0 to FFields.Count - 1 do begin
    Field := TODBCFieldDesc(Fields[j]);

    if Field.FetchFieldOffset >= 0 then
      SourcePtr := PtrOffset(FetchBuffer, Field.FetchFieldOffset)
    else
      SourcePtr := nil;
    if FFetchMode.OneByOne then
      SourceIndPtr := PtrOffset(RecBuf, DataSize + IndOffset)
    else if FFetchMode.ByColumns then
      SourceIndPtr := PtrOffset(FetchBuffer, Field.FetchFieldOffset + Field.FetchFieldSize * FetchRows)
    else
      SourceIndPtr := PtrOffset(FetchBuffer, FFetchBufferDataSize + IndOffset);

    DestPtr := PtrOffset(RecBuf, Field.DataOffset);
    DestIndPtr := PtrOffset(RecBuf, DataSize + IndOffset);
    if Field.HasValueLen then
      DestLenPtr := PtrOffset(RecBuf, Field.Offset)
    else
      DestLenPtr := nil;

    if (Field.FieldDescKind = fdkData) and (Field.ActualFieldNo > -1) {KeyOnly SmartFetchState} then begin
      for i := 0 to RowsObtained - 1 do begin
        Ind := PNativeInt(SourceIndPtr)^;
        if not FFetchMode.OneByOne then
          PNativeInt(DestIndPtr)^ := Ind;

        if Ind <> NativeInt(SQL_NULL_DATA) then begin
          SourceSize := PInteger(SourceIndPtr)^;
          if not ReadFetchBlockField(Field, SourcePtr, SourceSize, DestPtr, DestLenPtr, SharedPiece) then
            PNativeInt(DestIndPtr)^ := NativeInt(SQL_NULL_DATA);
        end;

        if FFetchMode.ByColumns then begin
          SourcePtr := PtrOffset(SourcePtr, Field.FetchFieldSize);
          SourceIndPtr := PtrOffset(SourceIndPtr, SizeOf(IntPtr));
        end
        else begin
          if SourcePtr <> nil then
            SourcePtr := PtrOffset(SourcePtr, FFetchBufferRowSize);
          SourceIndPtr := PtrOffset(SourceIndPtr, FFetchBufferRowSize);
        end;

        DestPtr := PtrOffset(DestPtr, DestRowOffset);
        DestIndPtr := PtrOffset(DestIndPtr, DestRowOffset);
        if Field.HasValueLen then
          DestLenPtr := PtrOffset(DestLenPtr, DestRowOffset);
      end;
    end;

    IndOffset := IndOffset + SizeOf(IntPtr);
  end;
end;

{$IFDEF MSWINDOWS}
{$IFDEF IS_UTF8}
  {$DEFINE CONVERT_ANSI_UTT8}
{$ENDIF}
{$ENDIF}
{$IFNDEF MSWINDOWS}
{$IFNDEF IS_UTF8}  
  {$DEFINE CONVERT_ANSI_UTT8}
{$ENDIF}
{$ENDIF}

function TODBCRecordSet.ReadFetchBlockField(Field: TFieldDesc; SourcePtr: IntPtr; SourceSize: Integer; DestPtr: IntPtr; DestLenPtr: PWord; var SharedPiece: PPieceHeader): boolean;

{$IFDEF MSWINDOWS}{$IFDEF IS_UTF8}
  function ReadAnsiString(SourcePtr: IntPtr; var Size: integer): AnsiString;
  var
    buf: TBytes;
  begin
    buf := BytesOf(SourcePtr, Size);
    Result := Encoding.ANSI.GetAnsiString(buf);
    Size := LengthA(Result);
  end;
{$ENDIF}{$ENDIF}
    
{$IFNDEF MSWINDOWS}{$IFNDEF IS_UTF8}
  function ReadAnsiString(SourcePtr: IntPtr; var Size: integer): AnsiString;
  var
    buf: TBytes;
  begin
    buf := BytesOf(SourcePtr, Size);
    Result := Encoding.UTF8.GetAnsiString(buf);
    Size := LengthA(Result);
  end;
{$ENDIF}{$ENDIF}

{$IFDEF MACOS}
  function ReadWideString(SourcePtr: IntPtr; var Size: integer): WideString; {$IFDEF USE_INLINE}inline;{$ENDIF}
  var
    Buf: TBytes;
  begin
    Buf := BytesOf(SourcePtr, Size);
    Result := Encoding.UTF8.GetWideString(Buf);
    Size := Length(Result) * 2;
  end;
{$ENDIF}  

  procedure ReadBlob(Blob: TBlob);
  var
    Piece: PPieceHeader;
  begin
    if Field.Length = 0 then begin
      if IntPtr(SharedPiece) = nil then
        TBlob.AllocPiece(SharedPiece, DefaultPieceSize);
      Result := FCommand.ReadBlob(Blob, Field.ActualFieldNo, Field.DataType, SharedPiece);
    end
    else if SourceSize > 0 then begin
      Blob.AllocPiece(Piece, SourceSize);
      Blob.AppendPiece(Piece);
      CopyBuffer(SourcePtr, PtrOffset(Blob.FirstPiece, sizeof(TPieceHeader)), SourceSize);
      Blob.FirstPiece.Used := SourceSize;
    end;
  end;

  procedure ReadMemo(Blob: TBlob);
  begin
    ReadBlob(Blob);
  {$IFDEF MSWINDOWS}
    {$IFDEF IS_UTF8}
    Blob.AsAnsiString := Encoding.ANSI.GetAnsiString(Blob.AsBytes);
    {$ENDIF}
    {$ELSE}
    {$IFNDEF IS_UTF8}
    Blob.AsAnsiString := Encoding.UTF8.GetAnsiString(Blob.AsBytes);
    {$ENDIF}
  {$ENDIF}
  end;

  procedure ReadWideMemo(Blob: TBlob);
  begin
    ReadBlob(Blob);
  {$IFDEF MACOS}
    Blob.AsWideString := Encoding.UTF8.GetWideString(Blob.AsBytes);
  {$ENDIF}
  end;

  function ReadNumericAsStr: string;
  begin
    Result := string(Marshal.PtrToStringAnsi(SourcePtr, SourceSize));
    if Result <> '' then begin
      if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
        Result := StringReplace(Result, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
      if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> LocaleDecSeparator then
        Result := StringReplace(Result, LocaleDecSeparator, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, []);
    end;
  end;

var
  Buf: IntPtr;
  Blob: TBlob;
  Date: TDateTime;
  DateRec: TSQLDateStruct;
  TimeRec: TSQLTimeStruct;
  TsRec: TSQLTimeStampStruct;
  Str: string;
  Cur: currency;
  Dbl: Double;
  i64: int64;
  AStr: AnsiString;
{$IFDEF MACOS}  
  WStr: WideString;
{$ENDIF}  
  Bcd: TBcd;
begin
  Result := True;

  if SourceSize = SQL_NO_TOTAL then
    SourceSize := DefaultPieceSize;

  case Field.DataType of
    dtFixedChar, dtString: begin
      if SourcePtr <> nil then begin // if need copy value
      {$IFDEF CONVERT_ANSI_UTT8}
        AStr := ReadAnsiString(SourcePtr, SourceSize);
        if SourceSize > 0 then
          Move(AStr[1], DestPtr^, SourceSize);
      {$ELSE}
        Move(SourcePtr^, DestPtr^, SourceSize);
      {$ENDIF}
      end;
      Marshal.WriteByte(DestPtr, SourceSize, 0);
      DestLenPtr^ := Word(SourceSize);
    end;
    dtFixedWideChar, dtWideString: begin
      if SourcePtr <> nil then begin // if need copy value
      {$IFDEF MACOS}
        WStr := ReadWideString(SourcePtr, SourceSize);
        if SourceSize > 0 then
          Move(WStr[1], DestPtr^, SourceSize);
      {$ELSE}
        Move(SourcePtr^, DestPtr^, SourceSize);
      {$ENDIF}      
      end;
      Marshal.WriteInt16(DestPtr, SourceSize, 0);
      SourceSize := SourceSize shr 1;
      DestLenPtr^ := Word(SourceSize);
    end;
    dtExtString: begin
    {$IFDEF CONVERT_ANSI_UTT8}
      AStr := ReadAnsiString(SourcePtr, SourceSize);
      if Field.Fixed and TrimFixedChar then
        Buf := FStringHeap.AllocTrimmedStr(PAnsiChar(AStr), SourceSize)
      else
        Buf := FStringHeap.AllocStr(PAnsiChar(AStr), SourceSize);
    {$ELSE}    
      if Field.Fixed and TrimFixedChar then
        Buf := FStringHeap.AllocTrimmedStr(SourcePtr, SourceSize)
      else
        Buf := FStringHeap.AllocStr(SourcePtr, SourceSize);
    {$ENDIF}    
      Marshal.WriteIntPtr(DestPtr, Buf);
      DestLenPtr^ := Word(SourceSize);
    end;
    dtExtWideString: begin
    {$IFDEF MACOS}
      WStr := ReadWideString(SourcePtr, SourceSize);
      SourceSize := SourceSize shr 1;
      if Field.Fixed and TrimFixedChar then
        Buf := FStringHeap.AllocTrimmedWideStr(PWideChar(WStr), SourceSize)
      else
        Buf := FStringHeap.AllocWideStr(PWideChar(WStr), SourceSize);
    {$ELSE}    
      SourceSize := SourceSize shr 1;
      if Field.Fixed and TrimFixedChar then
        Buf := FStringHeap.AllocTrimmedWideStr(SourcePtr, SourceSize)
      else
        Buf := FStringHeap.AllocWideStr(SourcePtr, SourceSize);
    {$ENDIF}    
      Marshal.WriteIntPtr(DestPtr, Buf);
      DestLenPtr^ := Word(SourceSize);
    end;
    dtMemo: begin
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DestPtr)));
      ReadMemo(Blob);
    end;
    dtWideMemo: begin
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DestPtr)));
      ReadWideMemo(Blob);
    end;
    dtVarBytes: begin
      if SourcePtr <> nil then
        Move(SourcePtr^, DestPtr^, SourceSize);
      DestLenPtr^ := Word(SourceSize);
    end;
    dtExtVarBytes: begin
      Buf := FStringHeap.NewBuf(SourceSize);
      Move(SourcePtr^, Buf^, SourceSize);
      Marshal.WriteIntPtr(DestPtr, Buf);
      DestLenPtr^ := Word(SourceSize);
    end;
    dtBlob: begin
      Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(DestPtr)));
      ReadBlob(Blob);
    end;
    dtDate: begin
      DateRec := TSQLDateStruct(SourcePtr^);
      if not TryEncodeDate(DateRec.Year, DateRec.Month, DateRec.Day, Date) then
        Date := MinDateTime;
      Marshal.WriteInt64(DestPtr, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtTime: begin
      TimeRec := TSQLTimeStruct(SourcePtr^);
      if not TryEncodeTime(TimeRec.Hour, TimeRec.Minute, TimeRec.Second, 0, Date) then
        Date := 0;
      Marshal.WriteInt64(DestPtr, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtDateTime: begin
      TsRec := TSQLTimeStampStruct(SourcePtr^);
      if not TryEncodeDateTime(TsRec.Year, TsRec.Month, TsRec.Day,
        TsRec.Hour, TsRec.Minute, TsRec.Second, TsRec.Fraction div 1000000, Date)
      then
        Date := MinDateTime;
      Marshal.WriteInt64(DestPtr, BitConverter.DoubleToInt64Bits(Date));
    end;
    dtBCD: begin
      Str := ReadNumericAsStr;
      if Str <> '' then begin
        Cur := StrToCurrDef(Str, 0);
        i64 := PInt64(@Cur)^;
        Marshal.WriteInt64(DestPtr, i64);
      end
      else
        Result := False;
    end;
    dtFMTBCD: begin
      Str := ReadNumericAsStr;
      if Str <> '' then begin
        if not TryStrToBcd(Str, Bcd) then begin
          Bcd := NullBcd;
          Bcd.Precision := 1;
        end;
        PBcd(DestPtr)^ := Bcd;
      end
      else
        Result := False;
    end;
    dtSingle, dtFloat, dtCurrency:
      if (Field.SubDataType = dtNumeric) and NumericAsString then begin
        Str := ReadNumericAsStr;
        if Str <> '' then begin
          Dbl := StrToFloatDef(Str, 0);
          if Field.DataType = dtSingle then
            Single(DestPtr^) := Dbl
          else
            Double(DestPtr^) := Dbl;
        end
        else
          Result := False;
      end
      else if SourcePtr <> nil then
        Move(SourcePtr^, DestPtr^, Field.Size);
    dtGuid: begin
      AStr := Marshal.PtrToStringAnsi(SourcePtr, SourceSize);
      if (AStr <> '') and (AStr[1] <> '{') then
        AStr := '{' + {$IFDEF NEXTGEN}string{$ENDIF}(AStr) + '}';
      SourceSize := Length(AStr);
      if SourceSize > 0 then begin
        if SourceSize > GuidStrSize then
          SourceSize := GuidStrSize;
        Move({$IFNDEF NEXTGEN}AStr[1]{$ELSE}AStr.Ptr^{$ENDIF}, DestPtr^, SourceSize);
      end;
      Marshal.WriteByte(DestPtr, SourceSize, 0);
      DestLenPtr^ := Word(SourceSize);
    end;
    else begin
      if SourcePtr <> nil then // if need copy value
        Move(SourcePtr^, DestPtr^, Field.Size);
    end;
  end;
end;

function TODBCRecordSet.BindColumnsForBlockFetch: Smallint;
begin
  if FFetchMode.OneByOne then
    Result := SQL_SUCCESS
  else if FFetchMode.ByColumns then
    Result := Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_BIND_TYPE, SQL_BIND_BY_COLUMN, 0)
  else
    Result := Cli.SQLSetStmtAttrInt(FCommand.FStmt, SQL_ATTR_ROW_BIND_TYPE, FFetchBufferRowSize, 0);

  if IsODBCError(Result) then
    Exit;

  Result := Cli.SQLSetStmtAttrPtr(FCommand.FStmt, SQL_ATTR_ROWS_FETCHED_PTR, FRowsObtainedBuf, 0);
end;

procedure TODBCRecordSet.BindColumns(RecBuf: IntPtr; FetchRows: integer);
var
  i: Integer;
  Field: TODBCFieldDesc;
  CDataType: smallint;
  ValueBuf: IntPtr;
  IndPtr: IntPtr;
begin
  for i := 0 to FFields.Count - 1 do begin
    Field := TODBCFieldDesc(Fields[i]);
    if (Field.FieldDescKind = fdkData) and (Field.ActualFieldNo > -1) {KeyOnly SmartFetchState} then begin
      if Field.BindFieldSize = -1 then begin
        if FFetchMode.OneByOne then
          Marshal.WriteIntPtr(RecBuf, DataSize + SizeOf(IntPtr) * i, nil)
        else
          Assert(False, 'Invalid fetch mode.');
        Continue;
      end;

      CDataType := FCommand.GetCDataType(Field.DataType, Field.SubDataType, NumericAsString);

      if FFetchMode.ByColumns then begin
        ValueBuf := PtrOffset(FFetchBuffer, Field.FFetchFieldOffset);
        IndPtr := PtrOffset(ValueBuf, Field.FetchFieldSize * FFetchRows);
      end
      else if FFetchMode.OneByOne then begin
        if Field.FFetchFieldOffset >= 0 then
          ValueBuf := PtrOffset(FFetchBuffer, Field.FFetchFieldOffset)
        else
          ValueBuf := PtrOffset(RecBuf, Field.DataOffset);
        IndPtr := PtrOffset(RecBuf, DataSize + SizeOf(IntPtr) * i);
      end
      else begin
        ValueBuf := PtrOffset(FFetchBuffer, Field.FFetchFieldOffset);
        IndPtr := PtrOffset(FFetchBuffer, FFetchBufferDataSize + SizeOf(IntPtr) * i);
      end;

      Check(Cli.SQLBindCol(FCommand.FStmt, Field.ActualFieldNo, CDataType, ValueBuf, Field.FetchFieldSize, IndPtr));
    end;
  end;

  FFetchBufferIsBinded := True;
end;

{ TODBCMetaDataCommand }

constructor TODBCMetaDataCommand.Create;
begin
  inherited;

  FMetaDataArgs := TMetaDataArgs.Create;
end;

destructor TODBCMetaDataCommand.Destroy;
begin
  FMetaDataArgs.Free;

  inherited;
end;

procedure TODBCMetaDataCommand.DetectCommandType;
begin
  FCommandType := ctCursor;
end;

procedure TODBCMetaDataCommand.InternalPrepare;
begin
  //Check(Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_METADATA_ID, SQL_TRUE, 0));
end;

procedure TODBCMetaDataCommand.InternalExecute;
var
  CatalogLen: Integer;
  SchemaLen: Integer;
{$IFDEF USE_UNICODE_DRIVER}
  pCatalog: PWideChar;
  pSchema: PWideChar;
{$ELSE}
  pCatalog: PAnsiChar;
  pSchema: PAnsiChar;
  CatalogA: AnsiString;
  SchemaA: AnsiString;
  ObjectNameA: AnsiString;
  ObjectTypeA: AnsiString;
  ColumnNameA: AnsiString;
{$ENDIF}
begin
  with FMetaDataArgs do begin
    if CatalogName <> '' then begin
    {$IFDEF USE_UNICODE_DRIVER}
      pCatalog := PWideChar(CatalogName);
      CatalogLen := Length(CatalogName);
    {$ELSE}
      CatalogA := ToODBCString(CatalogName);
      pCatalog := PAnsiChar(CatalogA);
      CatalogLen := LengthA(CatalogA);
    {$ENDIF}
    end
    else begin
      pCatalog := nil;
      CatalogLen := 0;
    end;

    if SchemaName <> '' then begin
    {$IFDEF USE_UNICODE_DRIVER}
      pSchema := PWideChar(SchemaName);
      SchemaLen := Length(SchemaName);
    {$ELSE}
      SchemaA := ToODBCString(SchemaName);
      pSchema := PAnsiChar(SchemaA);
      SchemaLen := LengthA(SchemaA);
    {$ENDIF}
    end
    else begin
      pSchema := nil;
      SchemaLen := 0;
    end;

    case FMetaDataKind of
      mkTables: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLTablesW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName),
          PWideChar(ObjectType), Length(ObjectType)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        ObjectTypeA := ToODBCString(ObjectType);
        Check(Cli.SQLTablesA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA),
          PAnsiChar(ObjectTypeA), Length(ObjectTypeA)));
      {$ENDIF}
      end;
      mkColumns: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLColumnsW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName),
          PWideChar(ColumnName), Length(ColumnName)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        ColumnNameA := ToODBCString(ColumnName);
        Check(Cli.SQLColumnsA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA),
          PAnsiChar(ColumnNameA), Length(ColumnNameA)));
      {$ENDIF}
      end;
      mkProcedures: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLProceduresW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        Check(Cli.SQLProceduresA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA)));
      {$ENDIF}
      end;
      mkProcedureColumns: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLProcedureColumnsW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName),
          PWideChar(ColumnName), Length(ColumnName)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        ColumnNameA := ToODBCString(ColumnName);
        Check(Cli.SQLProcedureColumnsA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA),
          PAnsiChar(ColumnNameA), Length(ColumnNameA)));
      {$ENDIF}
      end;
      mkStatistics: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLStatisticsW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName),
          Param1, Param2));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        Check(Cli.SQLStatisticsA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA),
          Param1, Param2));
      {$ENDIF}
      end;
      mkSpecialColumns: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLSpecialColumnsW(FStmt, Param1,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName),
          Param2, Param3));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        Check(Cli.SQLSpecialColumnsA(FStmt, Param1,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA),
          Param2, Param3));
      {$ENDIF}
      end;
      mkTypeInfo:
        Check(Cli.SQLGetTypeInfo(FStmt, Param1));
      mkPrimaryKey: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLPrimaryKeysW(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        Check(Cli.SQLPrimaryKeysA(FStmt,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA)));
      {$ENDIF}
      end;
      mkForeignKeys: begin
      {$IFDEF USE_UNICODE_DRIVER}
        Check(Cli.SQLForeignKeysW(FStmt, nil, 0, nil, 0, nil, 0,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PWideChar(ObjectName), Length(ObjectName)));
      {$ELSE}
        ObjectNameA := ToODBCString(ObjectName);
        Check(Cli.SQLForeignKeysA(FStmt, nil, 0, nil, 0, nil, 0,
          pCatalog, CatalogLen,
          pSchema, SchemaLen,
          PAnsiChar(ObjectNameA), Length(ObjectNameA)));
     {$ENDIF}
      end
    else
      Assert(False);
    end;

    DetectCommandType;
  end;
end;

{ TODBCMetaDataRecordSet }

procedure TODBCMetaDataRecordSet.CreateCommand;
begin
  SetCommand(TODBCMetaDataCommand.Create);
end;

{ TODBCMetaData }

constructor TODBCMetaData.Create;
begin
  inherited;
end;

destructor TODBCMetaData.Destroy;
begin
  inherited;
end;

function TODBCMetaData.CreateRecordSet: TCRRecordSet;
begin
  Result := TODBCMetaDataRecordSet.Create;
end;

function TODBCMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
begin
  if MetaDataKind = 'specialcolumns' then
    Result := GetSpecialColumns(Restrictions)
  else
  if MetaDataKind = 'typeinfo' then
    Result := GetTypeInfo
  else
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions);
end;

procedure TODBCMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('SpecialColumns');
  List.Add('TypeInfo');
  List.Sort;
end;

procedure TODBCMetaData.InternalGetRestrictionsList(List: TStringList; const MetaDataKind: string);
begin
  List.Clear;

  if MetaDataKind = 'specialcolumns' then begin
    List.Add('TABLE_CATALOG');
    List.Add('TABLE_SCHEMA');
    List.Add('TABLE_NAME');
    List.Add('COLUMN_TYPE');
    List.Add('SCOPE');
    List.Add('NULLABLE');
  end
  else
    inherited;
end;

function TODBCMetaData.GetTables(Restrictions: TStrings): TData;
var
  Scope: string;
  Command: TODBCMetaDataCommand;
  MetaDataArgs: TMetaDataArgs;
begin
  Command := TODBCMetaDataCommand(FRecordSet.GetCommand);
  Command.FMetaDataKind := mkTables;

  MetaDataArgs := Command.FMetaDataArgs;
  Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
  if Scope = 'LOCAL' then begin
    MetaDataArgs.CatalogName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedCatalog;
    MetaDataArgs.SchemaName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedSchema;
  end
  else begin
    MetaDataArgs.CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
    MetaDataArgs.CatalogName := Command.SQLInfo.NormalizeName(MetaDataArgs.CatalogName, False, True);
    MetaDataArgs.SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
    MetaDataArgs.SchemaName := Command.SQLInfo.NormalizeName(MetaDataArgs.SchemaName, False, True);
  end;
    MetaDataArgs.ObjectType := Trim(Restrictions.Values['TABLE_TYPE']);
    if MetaDataArgs.ObjectType = '' then
      MetaDataArgs.ObjectType := 'TABLE,SYSTEM TABLE,VIEW,SYNONYM,ALIAS';
  MetaDataArgs.ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
  if MetaDataArgs.ObjectName <> '' then
    MetaDataArgs.ObjectName := Command.SQLInfo.NormalizeName(MetaDataArgs.ObjectName, False, True)
  else
    MetaDataArgs.ObjectName := '%';

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateTablesFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateTablesFields;
  FMemData.Open;
  CopyTablesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyTablesData(Restrictions: TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snTABLE_TYPE    = 4;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnTABLE_TYPE    = 4;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snTABLE_NAME, snTABLE_TYPE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnTABLE_TYPE]);
    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetColumns(Restrictions: TStrings): TData;
var
  Command: TODBCMetaDataCommand;
  MetaDataArgs: TMetaDataArgs;
begin
  Command := TODBCMetaDataCommand(FRecordSet.GetCommand);
  Command.FMetaDataKind := mkColumns;

  MetaDataArgs := Command.FMetaDataArgs;
  MetaDataArgs.CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
  MetaDataArgs.CatalogName := Command.SQLInfo.NormalizeName(MetaDataArgs.CatalogName, False, True);
  MetaDataArgs.SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
  MetaDataArgs.SchemaName := Command.SQLInfo.NormalizeName(MetaDataArgs.SchemaName, False, True);
  MetaDataArgs.ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
  if MetaDataArgs.ObjectName <> '' then
    MetaDataArgs.ObjectName := Command.SQLInfo.NormalizeName(MetaDataArgs.ObjectName, False, True)
  else
    MetaDataArgs.ObjectName := '%';
  MetaDataArgs.ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);
  if MetaDataArgs.ColumnName <> '' then
    MetaDataArgs.ColumnName := Command.SQLInfo.NormalizeName(MetaDataArgs.ColumnName, False, True)
  else
    MetaDataArgs.ColumnName := '%';

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateColumnsFields;
  FMemData.Open;
  CopyColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyColumnsData(Restrictions: TStrings);
const
  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnTABLE_NAME    = 3;
  dnCOLUMN_NAME   = 4;
  dnPOSITION      = 5;
  dnDATA_TYPE     = 6;
  dnLENGTH        = 7;
  dnPRECISION     = 8;
  dnSCALE         = 9;
  dnNULLABLE      = 10;
  dnDEFAULT       = 11;

  snCATALOG       = 1;
  snSCHEMA        = 2;
  snTABLE_NAME    = 3;
  snCOLUMN_NAME   = 4;
  snPOSITION      = 17;
  snTYPE_NAME     = 6;
  snCOLUMN_SIZE   = 7;
  snSCALE         = 9;
  snNULLABLE      = 11;
  snDEFAULT       = 13;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snTABLE_NAME, snCOLUMN_NAME, snTYPE_NAME, snCOLUMN_SIZE, snCOLUMN_SIZE, snSCALE, snNULLABLE],
      [dnCATALOG, dnSCHEMA, dnTABLE_NAME, dnCOLUMN_NAME, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE, dnNULLABLE]);

    if FRecordSet.Fields.Count >= 17 then begin
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSetHelper.FieldValues[snPOSITION];
      FMemDataHelper.FieldValues[dnDEFAULT] := FRecordSetHelper.FieldValues[snDEFAULT];
    end
    else
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetProcedures(Restrictions: TStrings): TData;
var
  Scope: string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkProcedures;
    with FMetaDataArgs do begin
      Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
      if Scope = 'LOCAL' then begin
        CatalogName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedCatalog;
        SchemaName := TODBCCommand(FRecordSet.GetCommand).FConnection.GetCachedSchema;
      end
      else begin
        CatalogName := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
        CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
        SchemaName := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
        SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      end;
      ObjectName := Trim(Restrictions.Values['PROCEDURE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateProceduresFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateProceduresFields;
  FMemData.Open;
  CopyProceduresData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyProceduresData(Restrictions: TStrings);
const
  snCATALOG       = 1;
  snSCHEMA        = 2;
  snPROC_NAME     = 3;
  snPROC_TYPE     = 8;

  dnCATALOG       = 1;
  dnSCHEMA        = 2;
  dnPROC_NAME     = 3;
  dnPROC_TYPE     = 4;
var
  ProcType: integer;
  s: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snCATALOG, snSCHEMA, snPROC_NAME],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME]);

    ProcType := FRecordSetHelper.FieldValues[snPROC_TYPE];
    if ProcType = SQL_PT_FUNCTION then
      s := 'FUNCTION'
    else
      s := 'PROCEDURE';

    FMemDataHelper.FieldValues[dnPROC_TYPE] := s;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetProcedureParameters(Restrictions: TStrings): TData;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkProcedureColumns;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['PROCEDURE_NAME']);
      if ObjectName <> '' then
        ObjectName := SQLInfo.NormalizeName(ObjectName, False, True)
      else
        ObjectName := '%';
      ColumnName := Trim(Restrictions.Values['PARAMETER_NAME']);
      if ColumnName <> '' then
        ColumnName := SQLInfo.NormalizeName(ColumnName, False, True)
      else
        ColumnName := '%';
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateProcedureParametersFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateProcedureParametersFields;
  FMemData.Open;
  CopyProcedureParametersData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyProcedureParametersData(Restrictions: TStrings);
const
  dnCATALOG    = 1;
  dnSCHEMA     = 2;
  dnPROC_NAME  = 3;
  dnPARAM_NAME = 4;
  dnPOSITION   = 5;
  dnDIRECTION  = 6;
  dnDATA_TYPE  = 7;
  dnLENGTH     = 8;
  dnPRECISION  = 9;
  dnSCALE      = 10;

  snCATALOG    = 1;
  snSCHEMA     = 2;
  snPROC_NAME  = 3;
  snPARAM_NAME = 4;
  snPOSITION   = 18;
  snPARAM_TYPE = 5;
  snTYPE_NAME  = 7;
  snCOLUMN_SIZE = 8;
  snSCALE      = 10;

var
  ParamType: integer;
  Direction: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    ParamType := FRecordSetHelper.FieldValues[snPARAM_TYPE];
    if ParamType = SQL_RESULT_COL then
      continue;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snCATALOG, snSCHEMA, snPROC_NAME, snPARAM_NAME, snTYPE_NAME, snCOLUMN_SIZE, snCOLUMN_SIZE, snSCALE],
      [dnCATALOG, dnSCHEMA, dnPROC_NAME, dnPARAM_NAME, dnDATA_TYPE, dnLENGTH, dnPRECISION, dnSCALE]);

    case ParamType of
      SQL_PARAM_INPUT:
        Direction := 'IN';
      SQL_PARAM_INPUT_OUTPUT:
        Direction := 'IN/OUT';
      SQL_PARAM_OUTPUT, SQL_RETURN_VALUE:
        Direction := 'OUT';
    end;

    FMemDataHelper.FieldValues[dnDIRECTION] := Direction;

    if FRecordSet.Fields.Count >= 18 then
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSetHelper.FieldValues[snPOSITION]
    else
      FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetIndexes(Restrictions: TStrings): TData;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkStatistics;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      Param1 := SQL_INDEX_ALL;
      Param2 := SQL_QUICK;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateIndexesFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateIndexesFields;
  FMemData.Open;
  CopyIndexesData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyIndexesData(Restrictions: TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snNON_UNIQUE      = 4;
  snINDEX_QUAL      = 5;
  snINDEX_NAME      = 6;
  snTYPE            = 7;
  snPOSITION        = 8;
  snCOLUMN_NAME     = 9;
  snSORT_ORDER      = 10;

  dnTABLE_CATALOG = 1;
  dnTABLE_SCHEMA  = 2;
  dnTABLE_NAME    = 3;
  dnINDEX_CATALOG = 4;
  dnINDEX_SCHEMA  = 5;
  dnINDEX_NAME    = 6;
  dnUNIQUE        = 7;
var
  OldIndexName, IndexName: string;
  i: integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  OldIndexName := '';
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    IndexName := VarToStr(FRecordSetHelper.FieldValues[snINDEX_NAME]);
    if IndexName = OldIndexName then
      continue;
    OldIndexName := IndexName;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snTABLE_CATALOG, snTABLE_SCHEMA, snINDEX_NAME],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME]);

    if FRecordSetHelper.FieldValues[snNON_UNIQUE] = SQL_FALSE then
      i := 1
    else
      i := 0;
    FMemDataHelper.FieldValues[dnUNIQUE] := i;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetIndexColumns(Restrictions: TStrings): TData;
var
  Uniqueness: string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkStatistics;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      Uniqueness := Trim(Restrictions.Values['UNIQUE']);
      if Uniqueness = '1' then
        Param1 := SQL_INDEX_UNIQUE
      else
        Param1 := SQL_INDEX_ALL;
      Param2 := SQL_QUICK;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateIndexColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateIndexColumnsFields;
  FMemData.Open;
  CopyIndexColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CopyIndexColumnsData(Restrictions: TStrings);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snNON_UNIQUE      = 4;
  snINDEX_QUAL      = 5;
  snINDEX_NAME      = 6;
  snTYPE            = 7;
  snPOSITION        = 8;
  snCOLUMN_NAME     = 9;
  snSORT_ORDER      = 10;

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
  s: string;
  v: variant;
  i: integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    FMemDataHelper.InitRecord;
    CopyRecord(
      [snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snTABLE_CATALOG, snTABLE_SCHEMA, snINDEX_NAME, snCOLUMN_NAME, snPOSITION],
      [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnINDEX_CATALOG, dnINDEX_SCHEMA, dnINDEX_NAME, dnCOLUMN_NAME, dnPOSITION]);

    v := FRecordSetHelper.FieldValues[snSORT_ORDER];
    if not VarIsNull(v) then begin
      s := VarToStr(v);
      if s = 'D' then
        s := 'DESC'
      else
        s := 'ASC';
      FMemDataHelper.FieldValues[dnSORT_ORDER] := s;
    end
    else
      FMemDataHelper.FieldValues[dnSORT_ORDER] := v;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetConstraints(Restrictions: TStrings): TData;
var
  ListConstraintsName: TStringList;
begin
  ListConstraintsName := TStringList.Create;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkPrimaryKey;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateConstraintsFields;
  FMemData.Open;
  CopyConstraintPKData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do
    FMetaDataKind := mkForeignKeys;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  FMemData.Open;
  CopyConstraintFKData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do
  begin
    FMetaDataKind := mkStatistics;
    with FMetaDataArgs do
      Param1 := SQL_INDEX_UNIQUE;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  FMemData.Open;
  CopyConstraintUNData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
  ListConstraintsName.Free;
end;

procedure TODBCMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 128);
  AddField('CONSTRAINT_TYPE', dtString, 11);
  FMemData.InitFields;
end;

procedure TODBCMetaData.CopyConstraintPKData(Restrictions: TStrings; ListConstraintsName: TStringList);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snCONSTRAINT_NAME = 6;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
var
  ConstraintName: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME]);
    FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'PRIMARY KEY';

    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]);

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;

procedure TODBCMetaData.CopyConstraintFKData(Restrictions: TStrings; ListConstraintsName: TStringList);
var
  ConstraintName: string;
const
  snTABLE_CATALOG   = 5;
  snTABLE_SCHEMA    = 6;
  snTABLE_NAME      = 7;
  snCONSTRAINT_NAME = 12;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME]);
    FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'FOREIGN KEY';

    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]);

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;


procedure TODBCMetaData.CopyConstraintUNData(Restrictions: TStrings; ListConstraintsName: TStringList);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snCONSTRAINT_NAME = 6;
  snTYPE            = 7;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCONSTRAINT_TYPE = 5;
var
  ConstraintName: string;
  i: Integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME]);
    FMemDataHelper.FieldValues[dnCONSTRAINT_TYPE] := 'UNIQUE';

    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]);

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;

function TODBCMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
var
  ListConstraintsName: TStringList;
begin
  ListConstraintsName := TStringList.Create;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkPrimaryKey;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateConstraintColumnsFields;
  FMemData.Open;
  CopyConstraintColumnsPKData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do
    FMetaDataKind := mkForeignKeys;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  FMemData.Open;
  CopyConstraintColumnsFKData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  with TODBCMetaDataCommand(FRecordSet.GetCommand) do
  begin
    FMetaDataKind:= mkStatistics;
    with FMetaDataArgs do
      Param1 := SQL_INDEX_UNIQUE;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateConstraintColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  FMemData.Open;
  CopyConstraintColumnsUNData(Restrictions, ListConstraintsName);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
  ListConstraintsName.Free;
end;

procedure TODBCMetaData.CreateConstraintColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('CONSTRAINT_NAME', dtString, 128);
  AddField('COLUMN_NAME', dtString, 11);
  FMemData.InitFields;
end;

procedure TODBCMetaData.CopyConstraintColumnsPKData(Restrictions: TStrings; ListConstraintsName: TStringList);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snCONSTRAINT_NAME = 6;
  snCOLUMN_NAME     = 4;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCOLUMN_NAME     = 5;
var
  ConstraintName: string;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCOLUMN_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME, dnCOLUMN_NAME]);

    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]+  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCOLUMN_NAME]));

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;

procedure TODBCMetaData.CopyConstraintColumnsFKData(Restrictions: TStrings; ListConstraintsName: TStringList);
var
  ConstraintName: string;
const
  snTABLE_CATALOG   = 5;
  snTABLE_SCHEMA    = 6;
  snTABLE_NAME      = 7;
  snCONSTRAINT_NAME = 12;
  snCOLUMN_NAME     = 8;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCOLUMN_NAME     = 5;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;
    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCOLUMN_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME, dnCOLUMN_NAME]);


    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]+  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCOLUMN_NAME]));

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;


procedure TODBCMetaData.CopyConstraintColumnsUNData(Restrictions: TStrings; ListConstraintsName: TStringList);
const
  snTABLE_CATALOG   = 1;
  snTABLE_SCHEMA    = 2;
  snTABLE_NAME      = 3;
  snCONSTRAINT_NAME = 6;
  snTYPE            = 7;
  snCOLUMN_NAME     = 9;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCONSTRAINT_NAME = 4;
  dnCOLUMN_NAME     = 5;
var
  ConstraintName: string;
  i: Integer;
begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    i := FRecordSetHelper.FieldValues[snTYPE];
    if i = SQL_TABLE_STAT then
      continue;

    CopyRecord([snTABLE_CATALOG, snTABLE_SCHEMA, snTABLE_NAME, snCONSTRAINT_NAME, snCOLUMN_NAME], [dnTABLE_CATALOG, dnTABLE_SCHEMA, dnTABLE_NAME, dnCONSTRAINT_NAME, dnCOLUMN_NAME]);

    ConstraintName := VarToStr(FMemDataHelper.FieldValues[dnTABLE_CATALOG]) + ' ' + VarToStr(FMemDataHelper.FieldValues[dnTABLE_SCHEMA]) + ' ' +
      VarToStr(FMemDataHelper.FieldValues[dnTABLE_NAME]) +  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCONSTRAINT_NAME]+  ' ' + VarToStr(FMemDataHelper.FieldValues[dnCOLUMN_NAME]));

    if ListConstraintsName.IndexOf(ConstraintName) = - 1 then begin
      FMemDataHelper.AppendRecord;
      ListConstraintsName.Add(ConstraintName);
    end;
  end;
end;

function TODBCMetaData.GetSpecialColumns(Restrictions: TStrings): TData;
var
  ColumnType, Scope, Nullable: string;
begin
  with TODBCMetaDataCommand(FRecordSet.GetCommand) do begin
    FMetaDataKind := mkSpecialColumns;
    with FMetaDataArgs do begin
      CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
      CatalogName := SQLInfo.NormalizeName(CatalogName, False, True);
      SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
      SchemaName := SQLInfo.NormalizeName(SchemaName, False, True);
      ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
      ObjectName := SQLInfo.NormalizeName(ObjectName, False, True);
      ColumnType := AnsiUpperCase(Trim(Restrictions.Values['COLUMN_TYPE']));
      if ColumnType = 'ROWVER' then
        Param1 := SQL_ROWVER
      else
        Param1 := SQL_BEST_ROWID;
      Scope := UpperCase(Trim(Restrictions.Values['SCOPE']));
      if Scope = 'SESSION' then
        Param2 := SQL_SCOPE_SESSION
      else
      if Scope = 'TRANSACTION' then
        Param2 := SQL_SCOPE_TRANSACTION
      else
        Param2 := SQL_SCOPE_CURROW;
      Nullable := Trim(Restrictions.Values['NULLABLE']);
      if Nullable = '0' then
        Param3 := SQL_NO_NULLS
      else
        Param3 := SQL_NULLABLE;
    end;
  end;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateSpecialColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateSpecialColumnsFields;
  FMemData.Open;
  CopySpecialColumnsData(Restrictions);
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CreateSpecialColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 100);
  AddField('TABLE_SCHEMA', dtString, 100);
  AddField('TABLE_NAME', dtString, 100);
  AddField('COLUMN_NAME', dtString, 100);
  AddField('COLUMN_POSITION', dtInt32);
  FMemData.InitFields;
end;

procedure TODBCMetaData.CopySpecialColumnsData(Restrictions: TStrings);
const
  snSCOPE           = 1;
  snCOLUMN_NAME     = 2;

  dnTABLE_CATALOG   = 1;
  dnTABLE_SCHEMA    = 2;
  dnTABLE_NAME      = 3;
  dnCOLUMN_NAME     = 4;
  dnPOSITION        = 5;
var
  CatalogName, SchemaName, ObjectName: string;
begin
  CatalogName := Trim(Restrictions.Values['TABLE_CATALOG']);
  if CatalogName <> '' then
    CatalogName := FRecordSet.GetCommand.SQLInfo.NormalizeName(CatalogName, False, True)
  else
    CatalogName := TODBCConnection(FRecordSet.GetCommand.GetConnection).GetCachedCatalog;
  SchemaName := Trim(Restrictions.Values['TABLE_SCHEMA']);
  if SchemaName <> '' then
    SchemaName := FRecordSet.GetCommand.SQLInfo.NormalizeName(SchemaName, False, True)
  else
    SchemaName := TODBCConnection(FRecordSet.GetCommand.GetConnection).GetCachedSchema;
  ObjectName := Trim(Restrictions.Values['TABLE_NAME']);
  ObjectName := FRecordSet.GetCommand.SQLInfo.NormalizeName(ObjectName, False, True);

  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;

    CopyRecord([snCOLUMN_NAME], [dnCOLUMN_NAME]);

    FMemDataHelper.FieldValues[dnTABLE_CATALOG] := CatalogName;
    FMemDataHelper.FieldValues[dnTABLE_SCHEMA] := SchemaName;
    FMemDataHelper.FieldValues[dnTABLE_NAME] := ObjectName;
    FMemDataHelper.FieldValues[dnPOSITION] := FRecordSet.RecordNo;

    FMemDataHelper.AppendRecord;
  end;
end;

function TODBCMetaData.GetTypeInfo: TData;
begin
  TODBCMetaDataCommand(FRecordSet.GetCommand).FMetaDataKind := mkTypeInfo;

  try
    FRecordSet.Open;
  except
    on EODBCError do begin
      CreateSpecialColumnsFields;
      FMemData.Open;
      Result := FMemData;
      Exit;
    end;
  end;

  CreateTypeInfoFields;
  FMemData.Open;
  CopyTypeInfoData;
  FRecordSet.Close;

  FMemData.SetToBegin;
  Result := FMemData;
end;

procedure TODBCMetaData.CreateTypeInfoFields;
begin
  FMemData.Fields.Clear;

  AddField('TYPE_NAME', dtString, 100);
  AddField('DATA_TYPE', dtSmallint);
  AddField('COLUMN_SIZE', dtInt32);
  AddField('LITERAL_PREFIX', dtString, 100);
  AddField('LITERAL_SUFFIX', dtString, 100);
  AddField('CREATE_PARAMS', dtString, 100);
  AddField('NULLABLE', dtSmallint);
  AddField('CASE_SENSITIVE', dtSmallint);
  AddField('SEARCHABLE', dtSmallint);
  AddField('UNSIGNED_ATTRIBUTE', dtSmallint);
  AddField('FIXED_PREC_SCALE', dtSmallint);
  AddField('AUTO_UNIQUE_VALUE', dtSmallint);
  AddField('LOCAL_TYPE_NAME', dtString, 100);
  AddField('MINIMUM_SCALE', dtSmallint);
  AddField('MAXIMUM_SCALE', dtSmallint);
  AddField('SQL_DATA_TYPE', dtSmallint);
  AddField('SQL_DATETIME_SUB', dtSmallint);
  AddField('NUM_PREC_RADIX', dtInt32);
  AddField('INTERVAL_PRECISION', dtSmallint);

  FMemData.InitFields;
end;

procedure TODBCMetaData.CopyTypeInfoData;
const
  snTYPE_NAME = 1;
  snDATA_TYPE = 2;
  snCOLUMN_SIZE = 3;
  snLITERAL_PREFIX = 4;
  snLITERAL_SUFFIX = 5;
  snCREATE_PARAMS = 6;
  snNULLABLE = 7;
  snCASE_SENSITIVE = 8;
  snSEARCHABLE = 9;
  snUNSIGNED_ATTRIBUTE = 10;
  snFIXED_PREC_SCALE = 11;
  snAUTO_UNIQUE_VALUE = 12;
  snLOCAL_TYPE_NAME = 13;
  snMINIMUM_SCALE = 14;
  snMAXIMUM_SCALE = 15;
  snSQL_DATA_TYPE = 16;
  snSQL_DATETIME_SUB = 17;
  snNUM_PREC_RADIX = 18;
  snINTERVAL_PRECISION = 19;

  dnTYPE_NAME = 1;
  dnDATA_TYPE = 2;
  dnCOLUMN_SIZE = 3;
  dnLITERAL_PREFIX = 4;
  dnLITERAL_SUFFIX = 5;
  dnCREATE_PARAMS = 6;
  dnNULLABLE = 7;
  dnCASE_SENSITIVE = 8;
  dnSEARCHABLE = 9;
  dnUNSIGNED_ATTRIBUTE = 10;
  dnFIXED_PREC_SCALE = 11;
  dnAUTO_UNIQUE_VALUE = 12;
  dnLOCAL_TYPE_NAME = 13;
  dnMINIMUM_SCALE = 14;
  dnMAXIMUM_SCALE = 15;
  dnSQL_DATA_TYPE = 16;
  dnSQL_DATETIME_SUB = 17;
  dnNUM_PREC_RADIX = 18;
  dnINTERVAL_PRECISION = 19;

begin
  FMemDataHelper.AllocBuffer;
  FRecordSetHelper.AllocBuffer;
  while FRecordSetHelper.NextRecord do begin
    FMemDataHelper.InitRecord;

    CopyRecord([snTYPE_NAME, snDATA_TYPE, snCOLUMN_SIZE, snLITERAL_PREFIX, snLITERAL_SUFFIX, snCREATE_PARAMS, snNULLABLE, snCASE_SENSITIVE, snSEARCHABLE, snUNSIGNED_ATTRIBUTE,
                snFIXED_PREC_SCALE, snAUTO_UNIQUE_VALUE, snLOCAL_TYPE_NAME, snMINIMUM_SCALE, snMAXIMUM_SCALE, snSQL_DATA_TYPE, snSQL_DATETIME_SUB, snNUM_PREC_RADIX, snINTERVAL_PRECISION],
               [dnTYPE_NAME, dnDATA_TYPE, dnCOLUMN_SIZE, dnLITERAL_PREFIX, dnLITERAL_SUFFIX, dnCREATE_PARAMS, dnNULLABLE, dnCASE_SENSITIVE, dnSEARCHABLE, dnUNSIGNED_ATTRIBUTE,
                dnFIXED_PREC_SCALE, dnAUTO_UNIQUE_VALUE, dnLOCAL_TYPE_NAME, dnMINIMUM_SCALE, dnMAXIMUM_SCALE, dnSQL_DATA_TYPE, dnSQL_DATETIME_SUB, dnNUM_PREC_RADIX, dnINTERVAL_PRECISION]);


    FMemDataHelper.AppendRecord;
  end;
end;

{$IFNDEF LITE}

{ TODBCLoader }

constructor TODBCLoader.Create;
begin
  inherited;
end;

destructor TODBCLoader.Destroy;
begin
  inherited;
end;

function TODBCLoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := inherited SetProp(Prop, Value);
end;

function TODBCLoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := inherited GetProp(Prop, Value);
end;

procedure TODBCLoader.CreateCommand;
begin
  FCommand := TODBCCommand.Create;
end;

{$ENDIF}

initialization
  LocaleDecSeparator := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;

end.
