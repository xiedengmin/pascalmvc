
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SDAC Base level
//////////////////////////////////////////////////

{$I Sdac.inc}
unit MSClassesUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, ActiveX,
{$ENDIF}
  SysUtils, Classes, Variants, FMTBcd, SyncObjs,
{$IFNDEF FPC}
  DBConsts,
{$ENDIF}
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
{$IFDEF MSWINDOWS}{$IFNDEF LITE}
  {$IFNDEF UNIDACPRO}MSUDT{$ELSE}MSUDTUni{$ENDIF},
{$ENDIF}{$ENDIF}
{$IFNDEF LITE}
  CRDataTypeMap,
  {$IFNDEF UNIDACPRO}MSDataTypeMap,{$ELSE}MSDataTypeMapUni,{$ENDIF}
{$ENDIF}
  DAConsts, CLRClasses, CRTypes, CRFunctions, CRParser, CRVio,
  MemUtils, MemData, CRAccess, CRThread,
{$IFNDEF UNIDACPRO}
  MSConsts, SqlClasses, OLEDBC;
{$ELSE}
  MSConstsUni, SqlClassesUni, OLEDBCUni;
{$ENDIF}

const
  MaxNonBlobFieldLen = 8000;  // Maximum length of "char", "varchar", "nchar", "nvarchar", fields
  LeftQuote = '[';
  RightQuote = ']';

  BytesByRef = [dtBlob, dtBytes, dtVarBytes];
  CharsByRef = [dtMemo, dtWideMemo, dtXML, dtString, dtWideString];

  DefaultSDACDatabase = 'master';
  DefaultSDACSchema = 'dbo';
  DefaultSDACPort = 1433;
  DefaultSDACUdpPort = 1434;
  DefaultPacketSize = 4096;
  // Compact Edition specific
  DefaultMaxDatabaseSize = 128;
  DefaultMaxBufferSize = 640;
  DefaultTempFileMaxSize = 128;
  DefaultDefaultLockEscalation = 100;
  DefaultDefaultLockTimeout = 2000;
  DefaultAutoShrinkThreshold = 60;
  DefaultFlushInterval = 10;

  SET_BROWSE_ON  = 'SET NO_BROWSETABLE ON';
  SET_BROWSE_OFF = 'SET NO_BROWSETABLE OFF';

type
  TMSProvider = (prAuto, prSQL, prNativeClient, prCompact, prDirect, prMSOLEDB);

  TMSInitMode = (imReadOnly, imReadWrite, imExclusive, imShareRead);

  TMSAuthentication = (auWindows, auServer);

  TNativeClientVersion = (ncAuto, nc2005, nc2008, nc2012);

  TApplicationIntent = (aiReadWrite, aiReadOnly);

  TCompactVersion = (cvAuto, cv30, cv35, cv40);

  TCompactCommitMode = (cmAsynchCommit, cmSynchCommit);

  TMSOutputEncoding = (oeANSI, oeUTF8, oeUnicode);

  TMSLastIdentityValueFunction = (vfScopeIdentity, vfIdentCurrent, vfIdentity);

const
  DefaultInitMode = imReadWrite;
  DefValProvider = prAuto;
  DefValProviderCompact = prCompact;
  DefValAuthentication = auServer;
  DefaultTransactionCommitMode = cmAsynchCommit;
  DefValForceCreateDatabase = True;
  DefValApplicationIntent = aiReadWrite;

type
  TMSCursorType = (ctDefaultResultSet, ctStatic, ctKeyset, ctDynamic, ctBaseTable);
  TMSCursorTypes = set of TMSCursorType;
const
  ctTableType = 10;
  ServerCursorTypes: TMSCursorTypes = [ctStatic, ctKeyset, ctDynamic];

type
  TCursorTypeChangedProc = procedure of object;

  TMSFieldDesc = class(TSqlFieldDesc)
  protected
    FAssemblyTypename: string;
    FUDTSchemaname: string;
    FUDTName: string;
    FUDTCatalogname: string;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    FUDTDispatcher: TUDTDispatcher;
  {$ENDIF}
  {$ENDIF}

  public
    destructor Destroy; override;

    property AssemblyTypename: string read FAssemblyTypename write FAssemblyTypename;
    property UDTSchemaname: string read FUDTSchemaname write FUDTSchemaname;
    property UDTName: string read FUDTName write FUDTName;
    property UDTCatalogname: string read FUDTCatalogname write FUDTCatalogname;
  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    property UDTDispatcher: TUDTDispatcher read FUDTDispatcher write FUDTDispatcher;
  {$ENDIF}
  {$ENDIF}
  end;

  TMSSQLConnectionOptions = record
    ApplicationIntent: TApplicationIntent;
    ApplicationName: string;
    Authentication: TMSAuthentication;
    AutoShrinkThreshold: integer;
    AutoTranslate: boolean;
    BCDPrecision: integer;
    BCDScale: integer;
    CompactVersion: TCompactVersion;
    ConnectionTimeout: integer;
    DefaultLockEscalation: integer;
    DefaultLockTimeout: integer;
    Encrypt: boolean;
    FailoverPartner: string;
    FlushInterval: integer;
    ForceCreateDatabase: boolean;
    InitialFileName: string;
    InitMode: TMSInitMode;
    Language: string;
    LocaleIdentifier: cardinal;
    LockEscalation: integer;
    LockTimeout: integer;
    MaxBufferSize: integer;
    MaxDatabaseSize: integer;
    MultipleActiveResultSets: boolean;
    MultipleConnections: boolean;
    NativeClientVersion: TNativeClientVersion;
    NetworkLibrary: string;
    OldPassword: string;
    PacketSize: integer;
    PersistSecurityInfo: boolean;
    QuotedIdentifier: boolean;
    SetLockTimeout: boolean; // UniDAC behavior
    TempFileDirectory: string;
    TempFileMaxSize: integer;
    TransactionCommitMode: TCompactCommitMode;
    TrustServerCertificate: boolean;
    WideMemos: boolean;
    WorkstationID: string;
    MultiSubnetFailover: boolean;
  end;

  TMSSQLConnection = class;

  TMSSQLConnection = class(TCRConnection)
  private
    FInGetServerEdition: boolean;
  protected
    FConnector: TCRConnector;

    FProvider: TMSProvider;
    FIPVersion: TIPVersion;
    FHost: string;
    FInstanceName: string;
    FPort: integer;
    FServerEdition: string;
    FIsSQLAzureEdition: boolean;

    procedure SetProvider(const Value: TMSProvider);
    procedure SetQuotedIdentifier(const Value: boolean);
    procedure SetLockTimeout(Timeout: Integer);
    procedure SetDatabase(const Value: string);

    procedure CreateConnector;
    procedure InitConnector;
    function GetServerEdition: string;

  {$IFNDEF LITE}
  {$IFDEF MSWINDOWS}
    procedure Enlist(Transaction: TMTSTransaction); override;
    procedure UnEnlist(Transaction: TMTSTransaction); override;
  {$ENDIF}
  {$ENDIF}

    function CheckCommand(Command: TCRCommand): boolean; override;
    procedure InitCommandProp(Command: TCRCommand); override;
    function CheckRecordSet(RecordSet: TCRRecordSet): boolean; override;

    property EnableBCD;
    property EnableFMTBCD;

  public
    Options: TMSSQLConnectionOptions;

    constructor Create; override;
    destructor Destroy; override;

    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
  {$IFNDEF LITE}
    function GetLoaderClass: TCRLoaderClass; override;
    function GetMetaDataClass: TCRMetaDataClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;
  {$ENDIF}

    procedure Assign(Source: TCRConnection); override;
    procedure AssignConnect(Source: TCRConnection); override;

    function GetConnector: TCRConnector; override;
    procedure InitConnection;
    procedure Connect(const ConnectString: string); override;
    procedure Disconnect; override;

    procedure SetServer(const Value: string); override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: Integer; const Value: variant): boolean; override;

    function CheckIsValid: boolean; override;
    procedure Ping; override;
    function IsSQLAzureEdition: boolean;
    function CanChangeDatabase: boolean; override;
    procedure SetIsolationLevel(const Value: TCRIsolationLevel); override;

    function GetServerVersion: string; override;
    function GetServerVersionFull: string; override;
    function ServerMajorVer: integer;

    function GetClientVersion: string; override;
    function GetClientVersionFull: string;
    function ClientMajorVer: integer;

    procedure ChangePassword(const NewPassword: string);

    property Provider: TMSProvider read FProvider;
    property IPVersion: TIPVersion read FIPVersion;
    property InstanceName: string read FInstanceName;
    property Host: string read FHost;
    property Port: integer read FPort write FPort;
  end;

  TMSSQLCommandHelper = class
  public
    class function GetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
      ParamsInfo: TDAParamsInfo; Iters: integer): string; reintroduce;
  end;

{$IFNDEF LITE}

  TMSMetaDataType = (
    _otDatabases,
    _otTables, _otTableConstraints, _otColumns,
    _otIndexes, _otIndexColumns,
    _otStoredProcs, _otStoredProcParams, _otColumnPrivileges,
    _otForeignKeys, _otPrimaryKeys,
    _otLinkedServers, _otServerTypes, _otSchemata, _otStatistics,
    _otAliases, _otSynonyms, _otViews,
    _otSystemTables, _otGlobalTempTables, _otLocalTempTables, _otSystemViews,
    _otAliasesInfo, _otTablesInfo, _otSynonymsInfo, _otSystemTablesInfo, _otViewsInfo, _otGlobalTempTablesInfo,
    _otLocalTempTablesInfo, _otExternalTablesInfo, _otSystemViewsInfo, _otTablePrivileges,
    _otAssemblies, _otAssemblyDependencies, _otUserTypes, _otXMLCollections,
    _otCheckConstraints, _otCheckConstraintsByTable, _otConstraintColumnUsage,
    _otTableStatistics, _otTableTypes, _otTableTypePrimaryKeys, _otTableTypeColumns,
    _otUnknown
  );

  TMSSQLMetaData = class (TCRMetaData)
  private
    function GetSchemaNameFunc: string;
    function GetConnection: TMSSQLConnection;
  protected
    FRecordSet: TSqlRecordSet;

    function ConvertToMetaDataType(const MetaDataKind: string): TMSMetaDataType;
    function RequestIRowset(const MetaDataType: TMSMetaDataType; Restrictions: TStrings): TData; virtual;

    function InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData; override;
    procedure InternalGetMetaDataKindsList(List: TStringList); override;
    function GetProcedures(Restrictions: TStrings): TData; override;
    function GetIndexes(Restrictions: TStrings): TData; override;
    function GetConstraintColumns(Restrictions: TStrings): TData; override;

  {$IFNDEF DBX_METADATA}
    procedure CreateTablesFields; override;
    procedure CreateColumnsFields; override;
    procedure CreateProceduresFields; override;
    procedure CreateProcedureParametersFields; override;
    procedure CreateIndexColumnsFields; override;
    procedure CreateConstraintsFields; override;
    procedure CreateDatabasesFields; override;
  {$ENDIF}
  end;

{$ENDIF}

  TMSParamDesc = class(TParamDesc)
  protected
    FTableTypeName: string;
  public
    function GetAsBlobRef: TBlob;
    function GetValue: variant; override;
    procedure SetValue(const Value: variant); override;

    function GetNull: boolean; override;
    procedure SetNull(const Value: boolean); override;

    property TableTypeName: string read FTableTypeName write FTableTypeName;
  end;

  TMSSQLInfo = class(TSQLInfo)
  public
    function LeftQuote: Char; override;
    function RightQuote: Char; override;
    function IsQuoted(const Value: string): boolean; override;
    function ProcedureOverloadSeparator: Char; override;

    procedure SplitObjectName(const Name: string; out DataBase: string; out Owner: string; out ObjName: string); reintroduce;

    function NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string; override;
  end;

  TMSSQLTableObject = class(TSharedObject)
  protected
    FTableTypeName: string;
    FRecordSet: TSqlRecordSet;
  public
    function GetIsNull: boolean;
    procedure SetData(RecordSet: TSqlRecordSet);
    property TableTypeName: string read FTableTypeName write FTableTypeName;
    property RecordSet: TSqlRecordSet read FRecordSet;
  end;

  TMSLoaderColumn = class(TSqlLoaderColumn)
  private
    FIsWide: Boolean;
  public
    procedure UpdateDataType(Value: word); override;

    property IsWide: Boolean read FIsWide write FIsWide;
  end;

  function GetProviderName(const Provider: TMSProvider; const CompactVersion: TCompactVersion = cvAuto;
    const NativeClientVersion: TNativeClientVersion = ncAuto): string;
  function GetProvider(const ProviderName: string): TMSProvider;
  function GetCompactVersion(const ProviderName: string): TCompactVersion;
  function GetNativeClientVersion(const ProviderName: string;
    const OldNativeClientVersion: TNativeClientVersion): TNativeClientVersion;
  function IsCompactEdition(ServerVersion: integer): boolean;

  function ConvertOLEDBTypeToInternalFormat(
    const OLEDBType: DBTYPE;
    const IsLong, IsFixed: Boolean;
    const EnableBCD, EnableFMTBCD, SensibleBCDMapping: boolean;
  {$IFDEF LITE}
    const BCDPrecision, BCDScale: integer;
  {$ENDIF}
    const Precision, Scale: integer;
    const WideStrings, WideMemos: boolean;
    const IsParam: boolean;
    var InternalType: word; Provider: TMSProvider): boolean;

  function ConvertOLEDBParamTypeToCR(const Value: word): TParamDirection;

var
  MSSQLInfo: TMSSQLInfo;
{$IFDEF AUTOTEST}
  __SetCommandPropCount: integer;
  __SetRecordSetCommandPropCount: integer;
{$ENDIF}

implementation


uses
{$IFDEF MSWINDOWS}
  Registry,
{$ENDIF}
  StrUtils, Types,
  CRProps, CRBigInteger, CRVioUdp,
{$IFNDEF UNIDACPRO}
  MSProps, MSParser, MSSQLGenerator
  {$IFDEF MSWINDOWS}, OleDBIntf, OLEDBAccess{$ENDIF} {$IFDEF TDS}, Tds7Classes{$ENDIF};
{$ELSE}
  MSPropsUni, MSParserUni, MSSQLGeneratorUni
  {$IFDEF MSWINDOWS}, OleDBIntfUni, OLEDBAccessUni{$ENDIF} {$IFDEF TDS}, Tds7ClassesUni{$ENDIF};
{$ENDIF}

{ Functions }

function GetProviderName(const Provider: TMSProvider; const CompactVersion: TCompactVersion = cvAuto;
  const NativeClientVersion: TNativeClientVersion = ncAuto): string;
begin
  case Provider of
    prAuto, prSQL:
      Result := SProviderSQLOLEDB;
    prMSOLEDB:
      Result := SProviderMSOLEDBSQL;
    prNativeClient:
      case NativeClientVersion of
        ncAuto, nc2005:
          Result := SProviderNativeClient;
        nc2008:
          Result := SProviderNativeClient10;
        nc2012:
          Result := SProviderNativeClient11;
      else
        Assert(False);
        Result := '';
      end;
    prCompact:
      case CompactVersion of
        cv30:
          Result := SProviderCompact;
        cv35:
          Result := SProviderCompact35;
        cvAuto, cv40:
          Result := SProviderCompact40;
      else
        Assert(False);
        Result := '';
      end;
    prDirect:
      Result := SProviderTDS;
  else
    Assert(False);
    Result := '';
  end;
end;

function GetProvider(const ProviderName: string): TMSProvider;
var
  Name: string;
begin
  Name := UpperCase(ProviderName);
  if Name = SProviderTDS then
    Result := prDirect
  else
  if Name = SProviderSQLOLEDB then
    Result := prSQL
  else
  if Name = SProviderMSOLEDBSQL then
    Result := prMSOLEDB
  else
    if (Name = SProviderNativeClient) or (Name = SProviderNativeClient10) or (Name = SProviderNativeClient11) then
      Result := prNativeClient
    else
      if (Name = SProviderCompact) or (Name = SProviderCompact35) or (Name = SProviderCompact40) then
        Result := prCompact
      else
        Result := prAuto;
end;

function GetCompactVersion(const ProviderName: string): TCompactVersion;
var
  Name: string;
begin
  Name := UpperCase(ProviderName);
  if Name = SProviderCompact then
    Result := cv30
  else
    if Name = SProviderCompact35 then
      Result := cv35
    else
      if Name = SProviderCompact40 then
        Result := cv40
      else
        Result := cvAuto;
end;

function GetNativeClientVersion(const ProviderName: string;
  const OldNativeClientVersion: TNativeClientVersion): TNativeClientVersion;
var
  Name: string;
begin
  Name := UpperCase(ProviderName);
  if Name = SProviderNativeClient11 then
    Result := nc2012
  else
  if Name = SProviderNativeClient10 then
    Result := nc2008
  else
    if OldNativeClientVersion = nc2005 then
      Result := nc2005
    else
      Result := ncAuto;
end;

function IsCompactEdition(ServerVersion: integer): boolean;
begin
  Result := (ServerVersion = 3) or (ServerVersion = 4);
end;

function ConvertOLEDBTypeToInternalFormat(
  const OLEDBType: DBTYPE;
  const IsLong, IsFixed: boolean;
  const EnableBCD, EnableFMTBCD, SensibleBCDMapping: boolean;
{$IFDEF LITE}
  const BCDPrecision, BCDScale: integer;
{$ENDIF}
  const Precision, Scale: integer;
  const WideStrings, WideMemos: boolean;
  const IsParam: boolean;
  var InternalType: word; Provider: TMSProvider): boolean;
begin
  Result := True;
  case OLEDBType of // List of types must be synchronized with InternalInitFields types list
    // Integer fields
    DBTYPE_BOOL:
      InternalType := dtBoolean;
    DBTYPE_UI1:
      InternalType := dtUInt8;
    DBTYPE_I2:
      InternalType := dtInt16;
    DBTYPE_I4:
      InternalType := dtInt32;
    DBTYPE_UI2: {WAR For OLE DB info only. Signed/unsigned conversion}
      if Provider = prCompact then
        InternalType := dtWord
      else
        InternalType := dtInt32;
    DBTYPE_UI4: {WAR For OLE DB info only. Signed/unsigned conversion}
      if Provider = prCompact then
        InternalType := dtUInt32
      else
        InternalType := dtInt32;
    DBTYPE_I8:
      InternalType := dtInt64;
    DBTYPE_UI8:
      InternalType := dtUInt64;

    // Float fields
    DBTYPE_NUMERIC:
      if SensibleBCDMapping and EnableBCD and (Precision <= MaxBCDPrecision - MaxBCDScale + 1) and (Scale <= MaxBCDScale) then //max currency value
        InternalType := dtBCD
      else
      if EnableFMTBCD then begin
      {$IFDEF LITE}
        if (BCDPrecision > 0) and (Precision <= MaxBCDPrecision - MaxBCDScale) and
           (Precision <= BCDPrecision) and (Scale <= BCDScale) then
          InternalType := dtBCD
        else
      {$ENDIF}
          InternalType := dtFmtBCD;
      end
      else
      if not SensibleBCDMapping and EnableBCD then
        InternalType := dtBCD
      else
        InternalType := dtFloat;
    DBTYPE_R4:
      InternalType := dtSingle;
    DBTYPE_R8:
      InternalType := dtFloat;
    DBTYPE_CY:
      InternalType := dtCurrency;

    // Multibyte fields
    DBTYPE_DBTIMESTAMP, DBTYPE_DATE:
      InternalType := dtDateTime;
    DBTYPE_STR: begin
      if IsLong then
        InternalType := dtMemo
      else
        InternalType := dtString;
    end;
    DBTYPE_WSTR: begin
      if IsLong then begin
        if WideStrings and WideMemos then
          InternalType := dtWideMemo
        else
          InternalType := dtMemo;
      end
      else
        if WideStrings then
          InternalType := dtWideString
        else
          InternalType := dtString;
    end;
    DBTYPE_BYTES: begin
      if IsLong then
        InternalType := dtBlob
      else
        if IsFixed then
          InternalType := dtBytes
        else
          InternalType := dtVarBytes;
    end;
    DBTYPE_GUID:
      InternalType := dtGuid;
    DBTYPE_VARIANT:
    {$IFDEF LITE}
      InternalType := dtString;
    {$ELSE}
      InternalType := dtVariant;
    {$ENDIF}
    DBTYPE_XML:
    {$IFDEF LITE}
      InternalType := {$IFDEF VER12P}dtWideMemo{$ELSE}dtMemo{$ENDIF}; // CR-DBXSDA 22872
    {$ELSE}
      InternalType := dtXML;
    {$ENDIF}
    DBTYPE_UDT:
      InternalType := dtBlob; //Subdata type sdtMSUDT
    DBTYPE_DBDATE:
      InternalType := dtDate;
    DBTYPE_DBTIME:
      InternalType := dtTime;
    DBTYPE_DBDATETIMEOFFSET:
      InternalType := dtDateTime;
    DBTYPE_TABLE:
      InternalType := dtTable;
    else
      Result := False;
  end;
end;

{$WARNINGS OFF}
function ConvertOLEDBParamTypeToCR(const Value: word): TParamDirection;
begin
  case Value of
    DBPARAMTYPE_INPUT:
      Result := pdInput;
    DBPARAMTYPE_INPUTOUTPUT:
      Result := pdInputOutput;
    DBPARAMTYPE_RETURNVALUE:
      Result := pdResult;
    else
      Assert(False, Format('Invalid value %d', [Value]));
  end;
end;
{$WARNINGS ON}

{ TMSSQLInfo }

function TMSSQLInfo.LeftQuote: Char;
begin
  Result := '[';
end;

function TMSSQLInfo.RightQuote: Char;
begin
  Result := ']';
end;

function TMSSQLInfo.ProcedureOverloadSeparator: Char;
begin
  Result := ';';
end;

function TMSSQLInfo.IsQuoted(const Value: string): boolean;
var
  len: integer;
begin
  len := Length(Value);
  if (len <= 1) then
    Result := False
  else
    Result := ((Value[1] = LeftQuote) and (Value[len] = RightQuote)) or  // [ ]
              ((Value[1] = '"') and (Value[len] = '"')) // " "
end;

procedure TMSSQLInfo.SplitObjectName(const Name: string; out DataBase: string; out Owner: string; out ObjName: string);
var
  Info: TSQLObjectInfo;
begin
  inherited SplitObjectName(Name, Info);

  DataBase := Info.Catalog;
  Owner := Info.Schema;
  ObjName := Info.Name;
end;

function TMSSQLInfo.NamesFromList(List: TStrings; NormalizedName: boolean = True; Delimiter: string = ';'): string;
begin
  Result := inherited NamesFromList(List, NormalizedName, ',');
end;

{ TMSSQLConnection }

constructor TMSSQLConnection.Create;
begin
  inherited;

  Options.ApplicationIntent := DefValApplicationIntent;
  Options.Authentication := DefValAuthentication;
  Options.AutoShrinkThreshold := DefaultAutoShrinkThreshold;
  Options.AutoTranslate := DefValAutoTranslate;
  Options.CompactVersion := cvAuto;
  Options.ConnectionTimeout := DefValConnectionTimeout;
  Options.DefaultLockEscalation := DefaultDefaultLockEscalation;
  Options.DefaultLockTimeout := DefaultDefaultLockTimeout;
  Options.FailoverPartner := '';
  Options.FlushInterval := DefaultFlushInterval;
  Options.ForceCreateDatabase := DefValForceCreateDatabase;
  Options.InitMode := DefaultInitMode;
  Options.LockEscalation := DefaultDefaultLockEscalation;
  Options.LockTimeout := DefaultDefaultLockTimeout;
  Options.MaxBufferSize := DefaultMaxBufferSize;
  Options.MaxDatabaseSize := DefaultMaxDatabaseSize;
  Options.MultipleActiveResultSets := DefValMultipleActiveResultSets;
  Options.MultipleConnections := DefValMultipleConnections;
  Options.NativeClientVersion := ncAuto;
  Options.OldPassword := '';
  Options.PacketSize := DefaultPacketSize;
  Options.QuotedIdentifier := True;
  Options.TempFileDirectory := '';
  Options.TempFileMaxSize := DefaultTempFileMaxSize;
  Options.TransactionCommitMode := DefaultTransactionCommitMode;
  Options.WideMemos := True;
  Options.MultiSubnetFailover := DefValMultiSubnetFailover;
{$IFDEF MSWINDOWS}
  Options.LocaleIdentifier := GetSystemDefaultLCID;
{$ELSE}
  Options.LocaleIdentifier := $409; // Cardinal(TLanguages.UserDefaultLocale); <- actually structure with unknown internals
{$ENDIF}

  FProvider := DefValProvider;
  FIPVersion := DefValIPVersion;
  FPort := DefaultSDACPort;
  FDatabase := '';
  FConnector := nil;
end;

destructor TMSSQLConnection.Destroy;
begin
  inherited;

  if FNativeConnection then
    FConnector.Free;
end;

function TMSSQLConnection.GetCommandClass: TCRCommandClass;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    Result := TOLEDBCommand
  else
{$ENDIF}
{$IFDEF TDS}
    Result := TTDS7Command;
{$ELSE}
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}
end;

function TMSSQLConnection.GetRecordSetClass: TCRRecordSetClass;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    Result := TOLEDBRecordSet
  else
{$ENDIF}
{$IFDEF TDS}
    Result := TTDS7RecordSet;
{$ELSE}
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}
end;

function TMSSQLConnection.GetTransactionClass: TCRTransactionClass;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    Result := TOLEDBTransaction
  else
{$ENDIF}
{$IFDEF TDS}
    Result := TTDS7Transaction;
{$ELSE}
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}
end;

{$IFNDEF LITE}

function TMSSQLConnection.GetLoaderClass: TCRLoaderClass;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    Result := TOLEDBLoader
  else
{$ENDIF}
{$IFDEF TDS}
    Result := TTDS7Loader;
{$ELSE}
    Result := TCRLoader;
{$ENDIF}
end;

function TMSSQLConnection.GetMetaDataClass: TCRMetaDataClass;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    Result := TOLEDBMetaData
  else
{$ENDIF}
{$IFDEF TDS}
    if ServerMajorVer < 9 then
      Result := TTDS7MetaData2000
    else
      Result := TTDS7MetaData;
{$ELSE}
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}
end;

class function TMSSQLConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TMSMapRules;
end;

{$ENDIF}

procedure TMSSQLConnection.CreateConnector;
begin
{$IFDEF MSWINDOWS}
  if FProvider <> prDirect then
    FConnector := TOLEDBConnector.Create(Self)
  else
{$ENDIF}
{$IFDEF TDS}
  begin
    FConnector := TTDS7Connector.Create(Self);
  end;
{$ELSE}
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}
end;

procedure TMSSQLConnection.InitConnector;
begin
  FConnector.Disconnect;
{$IFDEF TDS}
  if FProvider = prDirect then begin
    TTDS7Connector(FConnector).InitConnectionSettings;
    FInternalTransaction.Reset;
  end;
{$ENDIF}
end;

function TMSSQLConnection.GetConnector: TCRConnector;
begin
  Result := FConnector;
end;

procedure TMSSQLConnection.Connect(const ConnectString: string);
begin
  if not FConnected then begin
    try
      if FNativeConnection then begin
        if Assigned(FConnector) then
          InitConnector
        else
          CreateConnector;
      end;

      FConnector.Connect;

      inherited;

      InitConnection;
    except
      on EFailOver do;
      else begin
        FConnected := False;
        raise;
      end;
    end;
  end;
end;

procedure TMSSQLConnection.InitConnection;
var
  vIsolationLevel: Variant;
  Params: string;
begin
  FServerEdition := '';
  FIsSQLAzureEdition := False;
  FConnected := True;

  Params := '';
  if (FProvider = prDirect){$IFNDEF MSWINDOWS} or (FProvider = prAuto){$ENDIF} then
    Params := Params + SET_BROWSE_ON + ';' + DALineSeparator; // forces server to send TDS_COLINFO_TOKEN with table names per column

  if not Options.QuotedIdentifier then
    Params := Params + TCustomMSSQLGenerator.GenerateQuotedIdentifierSQL(Options.QuotedIdentifier) + ';' + DALineSeparator;

  if (FProvider <> prCompact) and Options.SetLockTimeout then
    Params := Params + TCustomMSSQLGenerator.GenerateLockTimeoutSQL(Options.DefaultLockTimeout) + ';' + DALineSeparator;

  if FIsolationLevel = ilReadCommitted then begin
    GetInternalTransaction.GetProp(prIsolationLevel, vIsolationLevel);
    if TCRIsolationLevel(vIsolationLevel) <> ilReadCommitted then
      FIsolationLevel := TCRIsolationLevel(vIsolationLevel);
  end;

  if FIsolationLevel <> ilReadCommitted then
    Params := Params + TCustomMSSQLGenerator.GenerateIsolationLevelSQL(FIsolationLevel) + ';' + DALineSeparator;

  if Params <> '' then
    ExecuteSQL(Params);
end;

procedure TMSSQLConnection.Disconnect;
begin
  if not FConnected then
    Exit;

  FConnected := False;

  if FNativeConnection then
    FConnector.Disconnect;
end;

function TMSSQLConnection.CheckIsValid: boolean;
begin
  FIsValid := FConnected;
  if FIsValid then
    try
      ExecuteSQL(SCheckConnection);
    except
      FIsValid := False;
    end;
  Result := FIsValid;
end;

procedure TMSSQLConnection.SetServer(const Value: string);
var
  SlashPos: integer;
begin
  inherited SetServer(Value);

  SlashPos := Pos('\', Value);
  if SlashPos = 0 then begin
    FHost := Value;
    FInstanceName := '';
  end
  else begin
    FHost := Copy(Value, 1, SlashPos - 1);
    FInstanceName := Copy(Value, SlashPos + 1, Length(Value));
  end;
end;

procedure TMSSQLConnection.SetDatabase(const Value: string);
begin
  if FConnected and (Value = '') then
    DatabaseError(SWrongDatabaseName);

  FDatabase := Value;

  if {FConnected} FConnector <> nil then
    FConnector.SetDatabase(Value);
end;

function TMSSQLConnection.CanChangeDatabase: boolean;
begin
  Result := FProvider <> prCompact;
end;

procedure TMSSQLConnection.Ping;
begin
  ExecuteSQL('SELECT 1');
end;

procedure TMSSQLConnection.SetQuotedIdentifier(const Value: boolean);
begin
  if FConnected then
    ExecuteSQL(TCustomMSSQLGenerator.GenerateQuotedIdentifierSQL(Value));
end;

procedure TMSSQLConnection.SetLockTimeout(Timeout: Integer);
begin
  ExecuteSQL(TCustomMSSQLGenerator.GenerateLockTimeoutSQL(Timeout));
end;

function TMSSQLConnection.GetServerVersion: string;
begin
  Result := FConnector.ServerVersion;
end;

function TMSSQLConnection.GetServerVersionFull: string;
begin
  Result := FConnector.ServerName + ' ' + FConnector.ServerVersion;
end;

function TMSSQLConnection.ServerMajorVer: integer;
begin
  Result := FConnector.ServerMajorVersion;
end;

function TMSSQLConnection.GetClientVersion: string;
begin
  Result := FConnector.GetClientVersion;
end;

function TMSSQLConnection.GetClientVersionFull: string;
begin
  Result := FConnector.GetClientVersionFull;
end;

function TMSSQLConnection.ClientMajorVer: integer;
begin
  Result := FConnector.GetClientMajorVersion;
end;

procedure TMSSQLConnection.ChangePassword(const NewPassword: string);
var
  OldConnected: boolean;
  OldPassword: string;
begin
  OldConnected := FConnected;
  OldPassword := FPassword;
  try
    Disconnect;
    SetProp(prOldPassword, FPassword);
    FPassword := NewPassword;
    try
      Connect('');
    except
      FPassword := OldPassword;
      raise;
    end;
  finally
    SetProp(prOldPassword, '');
    if not OldConnected then
      Disconnect;
  end;
end;

function TMSSQLConnection.GetServerEdition: string;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: Variant;
begin
  if FProvider = prCompact then
    FServerEdition := 'compact edition';

  if (FServerEdition = '') and not FInGetServerEdition then begin
    FInGetServerEdition := True;
    RecordSet := GetRecordSetClass.Create;
    try
      RecordSet.SetConnection(Self);
      RecordSet.SetProp(prFetchAll, True);
      RecordSet.SetSQL('SELECT CAST(SERVERPROPERTY(''EDITION'') AS VARCHAR(128))');
      RecordSet.Open;
    {$IFDEF AUTOTEST}
      Dec(__ServerExecuteCount);
      Dec(__SetCommandPropCount);
      Dec(__SetRecordSetCommandPropCount);
    {$ENDIF}
      RecordSet.AllocRecBuf(RecBuf);
      try
        RecordSet.GetNextRecord(RecBuf);
        if not RecordSet.Eof then begin
          RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
          FServerEdition := VarToStr(v);
        end;
      finally
        if RecBuf <> nil then
          RecordSet.FreeRecBuf(RecBuf);
      end;
    finally
      RecordSet.Close;
      RecordSet.Free;
      FInGetServerEdition := False;
    end;
  end;

  Result := FServerEdition;
end;

procedure TMSSQLConnection.SetIsolationLevel(const Value: TCRIsolationLevel);
begin
  if FConnected and (FProvider <> prCompact) then
    ExecuteSQL(TCustomMSSQLGenerator.GenerateIsolationLevelSQL(Value));

  FIsolationLevel := Value;
end;

function TMSSQLConnection.IsSQLAzureEdition: boolean;
begin
  if not FIsSQLAzureEdition then begin
    if FServerEdition = '' then
      FIsSQLAzureEdition := LowerCase(GetServerEdition) = 'sql azure';
  end;

  Result := FIsSQLAzureEdition;
end;

procedure TMSSQLConnection.SetProvider(const Value: TMSProvider);
begin
{$IFNDEF MSWINDOWS}
  if not (Value in [prDirect, prAuto]) then
    raise Exception.Create(SOnlyDirectModeSupported);
{$ENDIF}
{$IFNDEF TDS}
  if Value = prDirect then
    raise Exception.Create(SDirectModeNotSupported);
{$ENDIF}

  if Value <> FProvider then begin
    FProvider := Value;

    if Assigned(FConnector) then
      FreeAndNil(FConnector);

    if not (FInternalTransaction is GetTransactionClass) then begin
      FInternalTransaction.Free;
      FInternalTransaction := GetTransactionClass.Create;
      FInternalTransaction.AddConnection(Self);
    end;
  end;
end;

{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
procedure TMSSQLConnection.Enlist(Transaction: TMTSTransaction);
begin
  FConnector.Enlist(Transaction);
end;

procedure TMSSQLConnection.UnEnlist(Transaction: TMTSTransaction);
begin
  FConnector.UnEnlist(Transaction);
end;
{$ENDIF}
{$ENDIF}

function TMSSQLConnection.CheckCommand(Command: TCRCommand): boolean;
begin
  Result := IsClass(Command, GetCommandClass);
  if Result then
    Command.SetCursorState(csInactive); // To prevent blocking execute after previous exec
end;

procedure TMSSQLConnection.InitCommandProp(Command: TCRCommand);
begin
  TSqlCommand(Command).CommandTimeout := Options.ConnectionTimeout;
end;

function TMSSQLConnection.CheckRecordSet(RecordSet: TCRRecordSet): boolean;
begin
  Result := IsClass(RecordSet, GetRecordSetClass);
end;

procedure TMSSQLConnection.AssignConnect(Source: TCRConnection);
begin
  if Source <> Self then begin
    Disconnect;
    if Source <> nil then begin
      Assign(Source);
      FInternalTransaction.AssignConnect(TMSSQLConnection(Source).FInternalTransaction);
      FConnector := TMSSQLConnection(Source).FConnector;
      FConnected := TMSSQLConnection(Source).FConnected;
      FNativeConnection := False;
    end;
  end;
end;

procedure TMSSQLConnection.Assign(Source: TCRConnection);
var
  Src: TMSSQLConnection;
begin
  inherited;

  Src := TMSSQLConnection(Source);
  FHost := Src.FHost;
  FInstanceName := Src.FInstanceName;
  FDatabase := Src.FDatabase;
  FPort := Src.FPort;
  FIPVersion := Src.FIPVersion;
  SetProvider(Src.FProvider);

  Options := Src.Options; // copy values
end;

function TMSSQLConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prApplicationIntent:
      Value := integer(Options.ApplicationIntent);
    prApplicationName:
      Value := Options.ApplicationName;
    prAuthentication:
      Value := integer(Options.Authentication);
    prAutoTranslate:
      Value := Options.AutoTranslate;
    prConnectionTimeout:
      Value := Options.ConnectionTimeout;
    prDefaultLockTimeout:
      Value := Options.DefaultLockTimeout;
    prDatabase:
      Value := FDatabase;
    prEncrypt:
      Value := Options.Encrypt;
    prFailoverPartner:
      Value := Options.FailoverPartner;
    prForceCreateDatabase:
      Value := Options.ForceCreateDatabase;
    prInitialFileName:
      Value := Options.InitialFileName;
    prLanguage:
      Value := Options.Language;
    prMARS:
      Value := Options.MultipleActiveResultSets;
    prMaxDatabaseSize:
      Value := Options.MaxDatabaseSize;
    prMultipleConnections:
      Value := Options.MultipleConnections;
    prNetworkLibrary:
      Value := Options.NetworkLibrary;
    prPacketSize:
      Value := Options.PacketSize;
    prPersistSecurityInfo:
      Value := Options.PersistSecurityInfo;
    prIPVersion:
      Value := integer(FIPVersion);
    prProvider:
      Value := integer(FProvider);
    prQuotedIdentifier:
      Value := Options.QuotedIdentifier;
    prSetLockTimeout:
      Value := Options.SetLockTimeout;
    prPort:
      Value := FPort;
    prTrustServerCertificate:
      Value := Options.TrustServerCertificate;
    prWorkstationID:
      Value := Options.WorkstationID;
    prMultiSubnetFailover:
      Value := Options.MultiSubnetFailover;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TMSSQLConnection.SetProp(Prop: Integer; const Value: variant): Boolean;
begin
  Result := True;
  case Prop of
    prApplicationIntent:
      Options.ApplicationIntent := TApplicationIntent(integer(Value));
    prApplicationName:
      Options.ApplicationName := Value;
    prAuthentication:
      Options.Authentication := TMSAuthentication(integer(Value));
    prAutoShrinkThreshold:
      Options.AutoShrinkThreshold := Value;
    prAutoTranslate:
      Options.AutoTranslate := Value;
    prBCDPrecision:
      Options.BCDPrecision := Value;
    prBCDScale:
      Options.BCDScale := Value;
    prCompactVersion:
      Options.CompactVersion := TCompactVersion(integer(Value));
    prConnectionTimeout:
      Options.ConnectionTimeout := Value;
    prDefaultLockEscalation:
      Options.DefaultLockEscalation := Value;
    prDefaultLockTimeout:
      Options.DefaultLockTimeout := Value;
    prEncrypt:
      Options.Encrypt := Value;
    prFailoverPartner:
      Options.FailoverPartner := Value;
    prFlushInterval:
      Options.FlushInterval := Value;
    prForceCreateDatabase:
      Options.ForceCreateDatabase := Value;
    prInitialFileName:
      Options.InitialFileName := Value;
    prInitMode:
      Options.InitMode := TMSInitMode(integer(Value));
    prLanguage:
      Options.Language := Value;
    prLocaleIdentifier:
      Options.LocaleIdentifier := Value;
    prLockEscalation:
      Options.LockEscalation := Value;
    prLockTimeout:
      Options.LockTimeout := Value;
    prMARS:
      Options.MultipleActiveResultSets := Value;
    prMaxBufferSize:
      Options.MaxBufferSize := Value;
    prMaxDatabaseSize:
      Options.MaxDatabaseSize := Value;
    prMultipleConnections:
      Options.MultipleConnections := Value;
    prNativeClientVersion:
      Options.NativeClientVersion := TNativeClientVersion(integer(Value));
    prNetworkLibrary:
      Options.NetworkLibrary := Value;
    prOldPassword:
      Options.OldPassword := Value;
    prPacketSize:
      Options.PacketSize := Value;
    prPersistSecurityInfo:
      Options.PersistSecurityInfo := Value;
    prPort:
      FPort := Value;
    prIPVersion:
      FIPVersion := TIPVersion(integer(Value));
    prProvider:
      SetProvider(TMSProvider(integer(Value)));
    prQuotedIdentifier: begin
      if Options.QuotedIdentifier <> boolean(Value) then
        SetQuotedIdentifier(Value);
      Options.QuotedIdentifier := Value;
    end;
    prSetLockTimeout:
      Options.SetLockTimeout := Value;
    prTempFileDirectory:
      Options.TempFileDirectory := Value;
    prTempFileMaxSize:
      Options.TempFileMaxSize := Value;
    prTransactionCommitMode:
      Options.TransactionCommitMode := TCompactCommitMode(integer(Value));
    prTrustServerCertificate:
      Options.TrustServerCertificate := Value;
    prWideMemos:
      Options.WideMemos := Value;
    prWorkstationID:
      Options.WorkstationID := Value;
    prDatabase:
      SetDatabase(Value);
    prMultiSubnetFailover:
      Options.MultiSubnetFailover := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TMSParamDesc }

function TMSParamDesc.GetAsBlobRef: MemData.TBlob;
begin
  Result := MemData.TBlob(TVarData(FData).VPointer);
end;

function TMSParamDesc.GetValue: variant;
var
  Blob: TBlob;
begin
  if not GetNull and (TVarData(FData).VType = varSharedObject) then begin
    Blob := GetAsBlobRef;
    if Blob.IsUnicode then
      Result := Blob.AsWideString
    else
      Result := Blob.AsString;
  end
  else
    Result := inherited GetValue;
end;

procedure TMSParamDesc.SetValue(const Value: variant);
var
  ValueType: TVarType;
  ConvertedValue: Variant;
  Value1: Variant;
  i, l: integer;
begin
  ValueType := TVarData(Value).VType;

  if ValueType = varArray or varVariant then
    inherited SetValue(Value)
  else
  if not (ValueType in [varEmpty, varNull]) then begin
    if FDataType = dtDate then
      inherited SetValue(Trunc(Double(Value))) // drop time info
    else
    if (FDataType in [dtBytes, dtVarBytes{, dtBlob}]) and (ValueType <> varArray + varByte) then begin
      case ValueType of
        varShortInt, varByte: {1 byte}
          l := 1;
        varSmallint, varWord: {2 bytes}
          l := 2;
        varInteger, varLongWord: {4 bytes}
          l := 4;
        varInt64{$IFDEF VER12P}, varUInt64{$ENDIF}: {8 bytes}
          l := 8;
        else
          l := - 1;
      end;

      if l <> -1 then begin
        // reverse bytes order
        Value1 := VarArrayCreate([0, l - 1], varByte);
        for i := 0 to l - 1 do
          PAnsiChar(TVarData(Value1).VArray.Data)[i] := PAnsiChar(@TVarData(Value).VInteger)[l - 1 - i];
        ConvertedValue := Value1;
      end
      else begin
        if VarIsStr(Value) then
          ConvertedValue := Encoding.Default.GetBytes(AnsiString(Value))
        else
          ConvertedValue := Value;
      end;

      inherited SetValue(ConvertedValue);
    end
    else
    if (((FParamType = pdInput) and (FDataType in CharsByRef + BytesByRef)) or (FDataType in [dtBlob, dtMemo, dtWideMemo])) and
      (ValueType <> varArray + varByte) and (ValueType <> varSharedObject)
    then begin
      if FDataType in [dtWideString, dtWideMemo, dtXML] then
        inherited SetValue(WideString(VarToWideStr(Value)))
      else
        inherited SetValue(AnsiString(VarToStr(Value)));
    end
    else
      inherited;
  end
  else
    inherited;
end;

function TMSParamDesc.GetNull: boolean;
var
  Obj: TSharedObject;
  ValuePtr: PVarData;
begin
   case FDataType of
     dtBlob, dtMemo, dtWideMemo, dtXML: begin
       ValuePtr := PVarData(GetItemPtr(0));
       if (ValuePtr.VType = varSharedObject) then begin
         Obj := TSharedObject(ValuePtr.VPointer);
         if Obj <> nil then
           Result := TBlob(Obj).IsNull
         else
           Result := True;
       end
       else
         Result := inherited GetNull;
     end;
     else
       Result := inherited GetNull;
   end;
end;

procedure TMSParamDesc.SetNull(const Value: boolean);
var
  Obj: TSharedObject;
  ValuePtr: PVarData;
begin
   if Value then begin
     case FDataType of
       dtBlob, dtMemo, dtWideMemo, dtXML: begin
         ValuePtr := PVarData(GetItemPtr(0));
         if (ValuePtr.VType = varSharedObject) then begin
           Obj := TSharedObject(ValuePtr.VPointer);
           if Obj <> nil then
             TBlob(Obj).Clear;
         end;
       end;
     end;
   end;

  inherited;
end;

{ TMSFieldDesc }

destructor TMSFieldDesc.Destroy;
begin
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
  if FUDTDispatcher <> nil then begin
    FUDTDispatcher.ReleaseUDTProxy;
    IDispatch(FUDTDispatcher)._Release;
  end;
{$ENDIF}
{$ENDIF}
  inherited;
end;

{ TMSSQLCommandHelper }

class function TMSSQLCommandHelper.GetBatchSQL(const SQL: string; ParsedSQLType: TParsedSQLType;
  ParamsInfo: TDAParamsInfo; Iters: integer): string;
var
  OriginalSQL: string;
  BatchSQL: StringBuilder;
  i, ParamIndex, OpenBracket, CloseBracket, SQLLength: integer;
  HasParams: boolean;

  procedure AppendParams(Iteration, StartIndex: integer);
  var
    i, n: integer;
  begin
    n := StartIndex;
    for i := 0 to ParamsInfo.Count - 1 do begin
      if i = 0 then
        BatchSQL.Append(OriginalSQL, OpenBracket - 1, StartIndex - OpenBracket + 1)
      else
        BatchSQL.Append(OriginalSQL, n, ParamsInfo[i].StartPosition - n - 1);

      BatchSQL.Append('?');

      n := ParamsInfo[i].EndPosition - 1;
      Inc(ParamIndex);
      if i = (ParamsInfo.Count - 1) then
        BatchSQL.Append(OriginalSQL, n, CloseBracket - n);
    end;
  end;

begin
  OriginalSQL := Trim(SQL);
  Result := OriginalSQL;

  SQLLength := Length(OriginalSQL);
  BatchSQL := StringBuilder.Create(SQLLength * Iters);
  ParamIndex := 1;
  HasParams := ParamsInfo.Count > 0;
  try
    case ParsedSQLType of
      qtInsert:
        if HasParams then begin
          OpenBracket := ParamsInfo[0].StartPosition;
          while OpenBracket >= 1 do begin
            if OriginalSQL[OpenBracket] = '(' then
              Break;
            Dec(OpenBracket);
          end;

          CloseBracket := ParamsInfo[ParamsInfo.Count - 1].EndPosition;
          while CloseBracket <= SQLLength do begin
            if OriginalSQL[CloseBracket] = ')' then
              Break;
            Inc(CloseBracket);
          end;

          BatchSQL.Append(OriginalSQL, 0, OpenBracket - 1);

          for i := 0 to Iters - 1 do begin
            if i > 0 then
              BatchSQL.Append(',');

            AppendParams(i, ParamsInfo[0].StartPosition - 1);
          end;

          if CloseBracket < SQLLength then
            BatchSQL.Append(OriginalSQL, CloseBracket, SQLLength - CloseBracket);
        end
        else
          for i := 0 to Iters - 1 do begin
            BatchSQL.Append(OriginalSQL);
            if OriginalSQL[SQLLength] <> ';' then
              BatchSQL.Append(';');
          end;
      qtUpdate,
      qtDelete: begin
        OpenBracket := 1;
        CloseBracket := SQLLength;
        for i := 0 to Iters - 1 do begin
          if HasParams then
            AppendParams(i, ParamsInfo[0].StartPosition - 1)
          else
            BatchSQL.Append(OriginalSQL);

          if OriginalSQL[SQLLength] <> ';' then
            BatchSQL.Append(';');
        end;
      end;
    end;

    Result := BatchSQL.ToString;
  finally
    BatchSQL.Free;
  end;
end;

{$IFNDEF LITE}
{ TMSSQLMetaData }

function TMSSQLMetaData.ConvertToMetaDataType(const MetaDataKind: string): TMSMetaDataType;
begin
  if CompareText(MetaDataKind, 'databases') = 0 then
    Result := _otDatabases
  else if CompareText(MetaDataKind, 'tables') = 0 then
    Result := _otTables
  else if CompareText(MetaDataKind, 'constraints') = 0 then
    Result := _otTableConstraints
  else if CompareText(MetaDataKind, 'columns') = 0 then
    Result := _otColumns
  else if CompareText(MetaDataKind, 'indexes') = 0 then
    Result := _otIndexes
  else if CompareText(MetaDataKind, 'indexcolumns') = 0 then
    Result := _otIndexColumns
  else if CompareText(MetaDataKind, 'procedures') = 0 then
    Result := _otStoredProcs
  else if CompareText(MetaDataKind, 'procedureparameters') = 0 then
    Result := _otStoredProcParams
  else if CompareText(MetaDataKind, 'columnprivileges') = 0 then
    Result := _otColumnPrivileges
  else if CompareText(MetaDataKind, 'foreignkeys') = 0 then
    Result := _otForeignKeys
  else if CompareText(MetaDataKind, 'primarykeys') = 0 then
    Result := _otPrimaryKeys
  else if CompareText(MetaDataKind, 'linkedservers') = 0 then
    Result := _otLinkedServers
  else if CompareText(MetaDataKind, 'servertypes') = 0 then
    Result := _otServerTypes
  else if CompareText(MetaDataKind, 'schemata') = 0 then
    Result := _otSchemata
  else if CompareText(MetaDataKind, 'statistics') = 0 then
    Result := _otStatistics
  else if CompareText(MetaDataKind, 'aliases') = 0 then
    Result := _otAliases
  else if CompareText(MetaDataKind, 'synonyms') = 0 then
    Result := _otSynonyms
  else if CompareText(MetaDataKind, 'views') = 0 then
    Result := _otViews
  else if CompareText(MetaDataKind, 'systemtables') = 0 then
    Result := _otSystemTables
  else if CompareText(MetaDataKind, 'globaltemptables') = 0 then
    Result := _otGlobalTempTables
  else if CompareText(MetaDataKind, 'localtemptables') = 0 then
    Result := _otLocalTempTables
  else if CompareText(MetaDataKind, 'systemviews') = 0 then
    Result := _otSystemViews
  else if CompareText(MetaDataKind, 'aliasesinfo') = 0 then
    Result := _otAliasesInfo
  else if CompareText(MetaDataKind, 'tablesinfo') = 0 then
    Result := _otTablesInfo
  else if CompareText(MetaDataKind, 'synonymsinfo') = 0 then
    Result := _otSynonymsInfo
  else if CompareText(MetaDataKind, 'systemtablesinfo') = 0 then
    Result := _otSystemTablesInfo
  else if CompareText(MetaDataKind, 'viewsinfo') = 0 then
    Result := _otViewsInfo
  else if CompareText(MetaDataKind, 'globaltemptablesinfo') = 0 then
    Result := _otGlobalTempTablesInfo
  else if CompareText(MetaDataKind, 'localtemptablesinfo') = 0 then
    Result := _otLocalTempTablesInfo
  else if CompareText(MetaDataKind, 'externaltablesinfo') = 0 then
    Result := _otExternalTablesInfo
  else if CompareText(MetaDataKind, 'systemviewsinfo') = 0 then
    Result := _otSystemViewsInfo
  else if CompareText(MetaDataKind, 'tableprivileges') = 0 then
    Result := _otTablePrivileges
  else if CompareText(MetaDataKind, 'assemblies') = 0 then
    Result := _otAssemblies
  else if CompareText(MetaDataKind, 'assemblydependencies') = 0 then
    Result := _otAssemblyDependencies
  else if CompareText(MetaDataKind, 'usertypes') = 0 then
    Result := _otUserTypes
  else if CompareText(MetaDataKind, 'xmlcollections') = 0 then
    Result := _otXMLCollections
  else if CompareText(MetaDataKind, 'checkconstraints') = 0 then
    Result := _otCheckConstraints
  else if CompareText(MetaDataKind, 'checkconstraintsbytable') = 0 then
    Result := _otCheckConstraintsByTable
  else if CompareText(MetaDataKind, 'constraintcolumnusage') = 0 then
    Result := _otConstraintColumnUsage
  else if CompareText(MetaDataKind, 'tablestatistics') = 0 then
    Result := _otTableStatistics
  else if CompareText(MetaDataKind, 'tabletypes') = 0 then
    Result := _otTableTypes
  else if CompareText(MetaDataKind, 'tabletypeprimarykeys') = 0 then
    Result := _otTableTypePrimaryKeys
  else if CompareText(MetaDataKind, 'tabletypecolumns') = 0 then
    Result := _otTableTypeColumns
  else
    Result := _otUnknown;
end;

function TMSSQLMetaData.GetConnection: TMSSQLConnection;
begin
  Result := TMSSQLConnection(FRecordSet.GetCommand.GetConnection);
end;


function TMSSQLMetaData.RequestIRowset(const MetaDataType: TMSMetaDataType; Restrictions: TStrings): TData;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise Exception.Create(SUnsupportedMetaDataKind);
end;

function TMSSQLMetaData.InternalGetMetaData(const MetaDataKind: string; Restrictions: TStrings): TData;
const
  DBSchemaRowsetsSupportedBySQLAzure = [
    _otDatabases, _otTables
  {$IFDEF MSWINDOWS}
    , _otAliases, _otGlobalTempTables, _otLocalTempTables,
    _otSynonyms, _otSystemTables, _otSystemViews, _otViews
  {$ENDIF}
  ];
  MetaDataSupportedBySQLAzure = [
    _otColumns, _otStoredProcs, _otStoredProcParams,
    _otIndexes, _otIndexColumns, _otTableConstraints

  ];
  MetaDataSupportedInDirect = [
    _otColumns, _otStoredProcs, _otStoredProcParams, _otIndexes,
    _otIndexColumns, _otTableConstraints, _otTables, _otDatabases
  ];

var
  MetaDataType: TMSMetaDataType;
begin
  MetaDataType := ConvertToMetaDataType(MetaDataKind);

  if MetaDataType = _otUnknown then
    Result := inherited InternalGetMetaData(MetaDataKind, Restrictions)
  else
  if GetConnection.IsSQLAzureEdition and
    not (MetaDataType in DBSchemaRowsetsSupportedBySQLAzure) then begin
    if MetaDataType in MetaDataSupportedBySQLAzure then
      Result := inherited InternalGetMetaData(MetaDataKind, Restrictions)
    else
      raise Exception.Create(SAzureNotSupportMetaDataKind);
  end
  else
    if MetaDataType in MetaDataSupportedInDirect then
      Result := inherited InternalGetMetaData(MetaDataKind, Restrictions)
    else
      Result := RequestIRowset(MetaDataType, Restrictions);
end;

function TMSSQLMetaData.GetSchemaNameFunc: string;
begin
  Assert(FRecordSet <> nil);
  Assert(GetConnection <> nil);
  if GetConnection.ServerMajorVer >= 9 then
    Result := 'schema_name'
  else
    Result := 'user_name';
end;

procedure TMSSQLMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Sort;
end;

function TMSSQLMetaData.GetProcedures(Restrictions: TStrings): TData;
const
  fmtGetNonSystemProceduresSQL =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(uid) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(xtype AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  crdate AS DATE_CREATED,'#$D#$A +
    '  CAST(null AS DATETIME) AS DATE_MODIFIED,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN xtype IN (''P'',''X'') THEN 1 '#$D#$A +
    '    ELSE 0 '#$D#$A +
    '  END AS OVERLOAD '#$D#$A +
    'FROM sysobjects o'#$D#$A +
    'WHERE %1:s (xtype in (''P'',''FN'',''TF'',''X''))'#$D#$A;
  fmtGetNonSystemProceduresSQL2005 =
    'UNION '#$D#$A +
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(uid) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(xtype AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  crdate AS DATE_CREATED,'#$D#$A +
    '  CAST(null AS DATETIME) AS DATE_MODIFIED,'#$D#$A +
    '  procedure_number AS OVERLOAD '#$D#$A +
    'FROM sysobjects o JOIN sys.numbered_procedures p on (p.object_id = o.id)'#$D#$A +
    'WHERE %1:s (xtype in (''P'',''X''))';

  fmtGetSystemProceduresSQL2000 =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(uid) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(xtype AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  crdate AS DATE_CREATED,'#$D#$A +
    '  CAST(null AS DATETIME) AS DATE_MODIFIED,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN xtype IN (''P'',''X'') THEN 1 '#$D#$A +
    '    ELSE 0 '#$D#$A +
    '  END AS OVERLOAD '#$D#$A +
    'FROM sysobjects o'#$D#$A +
    'WHERE %1:s (xtype in (''P'',''FN'',''TF'',''X'',''IF''))'#$D#$A;

  fmtGetSystemProceduresSQL2005 =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(schema_id) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(type AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  create_date AS DATE_CREATED,'#$D#$A +
    '  modify_date AS DATE_MODIFIED,'#$D#$A +
    '  CASE '#$D#$A +
    '    WHEN type IN (''P'',''X'') THEN 1 '#$D#$A +
    '    ELSE 0 '#$D#$A +
    '  END AS OVERLOAD '#$D#$A +
    'FROM sys.all_objects o'#$D#$A +
    'WHERE %1:s (type in (''P'',''FN'',''TF'',''X'',''IF''))'#$D#$A +
    'UNION '#$D#$A +
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS PROCEDURE_CATALOG,'#$D#$A +
    '  CAST(%0:s(schema_id) AS VARCHAR(128)) AS PROCEDURE_SCHEMA,'#$D#$A +
    '  CAST(name AS VARCHAR(128)) AS PROCEDURE_NAME,'#$D#$A +
    '  CAST(type AS VARCHAR(2)) AS PROCEDURE_TYPE,'#$D#$A +
    '  create_date AS DATE_CREATED,'#$D#$A +
    '  modify_date AS DATE_MODIFIED,'#$D#$A +
    '  procedure_number AS OVERLOAD '#$D#$A +
    'FROM sys.all_objects o JOIN sys.numbered_procedures p on (p.object_id = o.object_id)'#$D#$A +
    'WHERE %1:s (type in (''P'',''X''))';

  fmtOrderBySQL = ' ORDER BY 2, 3';

var
  {Catalog, }ProcSchema, ProcName, Scope, WhereClause, SQL: string;
begin
  if GetConnection.Provider = prCompact then begin
    CreateProceduresFields;
    FMemData.Open;
    Result := FMemData;
    Exit;
  end;

//  Catalog := Trim(Restrictions.Values['PROCEDURE_CATALOG']);
  ProcSchema := Trim(Restrictions.Values['PROCEDURE_SCHEMA']);
  ProcName := Trim(Restrictions.Values['PROCEDURE_NAME']);
  Scope := AnsiUpperCase(Trim(Restrictions.Values['SCOPE']));

  WhereClause := '';
  if (Scope = 'LOCAL') or (GetConnection.ServerMajorVer < 9) then
    AddWhere(WhereClause, GetSchemaNameFunc + '(uid)', ProcSchema)
  else
    AddWhere(WhereClause, GetSchemaNameFunc + '(schema_id)', ProcSchema);
  AddWhere(WhereClause, 'name', ProcName);
  if WhereClause <> '' then
    WhereClause := WhereClause + ' AND';

  if Scope = 'LOCAL' then begin
    if GetConnection.ServerMajorVer >= 9 then // SQL Server 2005
      SQL := Format(fmtGetNonSystemProceduresSQL + fmtGetNonSystemProceduresSQL2005, [GetSchemaNameFunc, WhereClause])
    else
      SQL := Format(fmtGetNonSystemProceduresSQL, [GetSchemaNameFunc, WhereClause]);
  end
  else begin
    if GetConnection.ServerMajorVer >= 9 then // SQL Server 2005
      SQL := Format(fmtGetSystemProceduresSQL2005, [GetSchemaNameFunc, WhereClause])
    else
      SQL := Format(fmtGetSystemProceduresSQL2000, [GetSchemaNameFunc, WhereClause]);
  end;

  FRecordSet.SetSQL(SQL + fmtOrderBySQL);
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMSSQLMetaData.GetIndexes(Restrictions: TStrings): TData;
const
  fmtGetIndexesSQL =
    'SELECT '#$D#$A +
    '  %0:s AS TABLE_CATALOG,'#$D#$A +
    '  CAST(%3:s(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  %0:s AS INDEX_CATALOG,'#$D#$A +
    '  CAST(%3:s(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  (CASE WHEN x.status & 0x2 <> 0 THEN 1 ELSE 0 END) AS [UNIQUE] '#$D#$A +
    'FROM %1:s..sysobjects o, %1:s..sysindexes x '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %2:s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name';

  fmtGetIndexesSQLAzure =
    'SELECT '#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS TABLE_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS TABLE_SCHEMA,'#$D#$A +
    '  CAST(o.name AS VARCHAR(128)) AS TABLE_NAME,'#$D#$A +
    '  CAST(db_name() AS VARCHAR(128)) AS INDEX_CATALOG,'#$D#$A +
    '  CAST(schema_name(o.uid) AS VARCHAR(128)) AS INDEX_SCHEMA,'#$D#$A +
    '  CAST(x.name AS VARCHAR(128)) AS INDEX_NAME,'#$D#$A +
    '  CASE x.is_unique '#$D#$A +
    '    WHEN 0 THEN 0 ELSE 1 '#$D#$A +
    '  END AS [UNIQUE] '#$D#$A +
    'FROM sysobjects o, sys.indexes x '#$D#$A +
    'WHERE '#$D#$A +
    '  o.id = x.object_id '#$D#$A +
    '  AND o.xtype <> ''S'' '#$D#$A +
    '  AND LEFT(x.name, 8/*_WA_Sys_*/) <> ''_WA_Sys_'''#$D#$A +
    '  %s '#$D#$A +
    'ORDER BY o.uid, o.name, x.name';

  fmtCompactGetIndexesSQL =
    'SELECT TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, '#$D#$A +
    '  INDEX_CATALOG, INDEX_SCHEMA, INDEX_NAME, '#$D#$A +
    '  CASE [UNIQUE] WHEN 0 THEN 0 ELSE 1 END AS [UNIQUE] '#$D#$A + // UNIQUE is boolean
    'FROM INFORMATION_SCHEMA.INDEXES '#$D#$A +
    '  %s '#$D#$A +
    'GROUP BY TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, '#$D#$A +
    '  INDEX_CATALOG, INDEX_SCHEMA, INDEX_NAME, [UNIQUE] '#$D#$A +
    'ORDER BY TABLE_NAME, INDEX_NAME';

var
  Catalog, TableSchema, TableName, IndexName,
  WhereClause, CatalogVal, CatalogPrefix: string;
begin
  Catalog := Trim(Restrictions.Values['TABLE_CATALOG']);
  TableSchema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  IndexName := Trim(Restrictions.Values['INDEX_NAME']);

  if GetConnection.IsSQLAzureEdition then begin
    WhereClause := '';
    AddWhere(WhereClause, 'schema_name(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtGetIndexesSQLAzure, [WhereClause]));
  end
  else
  if GetConnection.Provider <> prCompact then begin
    if Catalog <> '' then begin
      CatalogPrefix := MSSQLInfo.NormalizeName(Catalog);

      CatalogVal := MSSQLInfo.UnQuote(CatalogPrefix); // remove [] ; add ' '
      CatalogVal := AnsiQuotedStr(CatalogVal, '''');
    end
    else begin
      CatalogPrefix := '';
      CatalogVal := 'CAST(db_name() AS VARCHAR(128))';
    end;

    WhereClause := '';
    AddWhere(WhereClause, GetSchemaNameFunc + '(o.uid)', TableSchema);
    AddWhere(WhereClause, 'o.name', TableName);
    AddWhere(WhereClause, 'x.name', IndexName);
    if WhereClause <> '' then
      WhereClause := ' AND ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtGetIndexesSQL, [CatalogVal, CatalogPrefix, WhereClause, GetSchemaNameFunc]));
  end
  else begin
    WhereClause := '';
    AddWhere(WhereClause, 'TABLE_NAME', TableName);
    AddWhere(WhereClause, 'INDEX_NAME', IndexName);
    if WhereClause <> '' then
      WhereClause := 'WHERE ' + WhereClause;

    FRecordSet.SetSQL(Format(fmtCompactGetIndexesSQL, [WhereClause]));
  end;
  FRecordSet.Open;
  Result := FRecordSet;
end;

function TMSSQLMetaData.GetConstraintColumns(Restrictions: TStrings): TData;
const
  fmtGetConstraintColumnsSQL =
    'SELECT ' +
    '  TABLE_CATALOG AS TABLE_CATALOG, ' +
	   '  TABLE_SCHEMA AS TABLE_SCHEMA, ' +
    '  TABLE_NAME AS TABLE_NAME, ' +
	   '  CONSTRAINT_NAME AS CONSTRAINT_NAME, ' +
    '  COLUMN_NAME AS COLUMN_NAME, ' +
	   '  ORDINAL_POSITION AS COLUMN_POSITION ' +
    'FROM information_schema.KEY_COLUMN_USAGE ' +
    '%s ' +
    'ORDER BY ' +
    '  TABLE_SCHEMA, ' +
    '  TABLE_NAME, ' +
    '  CONSTRAINT_NAME ';
var
  Schema, TableName, ConstraintName, ColumnName, WhereClause: string;
begin
  if (GetConnection.ServerMajorVer < 10) and (GetConnection.Provider <> prCompact) then begin
    CreateConstraintColumnsFields;
    FMemData.Open;
    Result := FMemData;
    exit;
  end;

  Schema := Trim(Restrictions.Values['TABLE_SCHEMA']);
  TableName := Trim(Restrictions.Values['TABLE_NAME']);
  ConstraintName := Trim(Restrictions.Values['CONSTRAINT_NAME']);
  ColumnName := Trim(Restrictions.Values['COLUMN_NAME']);

  WhereClause := '';
  AddWhere(WhereClause, 'TABLE_SCHEMA', Schema);
  AddWhere(WhereClause, 'TABLE_NAME', TableName);
  AddWhere(WhereClause, 'CONSTRAINT_NAME', ConstraintName);
  AddWhere(WhereClause, 'COLUMN_NAME', ColumnName);

  if WhereClause <> '' then
    WhereClause := 'WHERE ' + WhereClause;

  FRecordSet.Close;
  FRecordSet.SetSQL(Format(fmtGetConstraintColumnsSQL, [WhereClause]));
  FRecordSet.Open;
  Result := FRecordSet;
end;

{$IFNDEF DBX_METADATA}
procedure TMSSQLMetaData.CreateTablesFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('TABLE_TYPE', dtString, 20);
  AddField('DATE_CREATED', dtDateTime);
  AddField('DATE_MODIFIED', dtDateTime);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('COLUMN_NAME', dtString, 128);
  AddField('POSITION', dtInt32);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  AddField('NULLABLE', dtInt32);
  AddField('DEFAULT_VALUE', dtMemo);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateProceduresFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 128);
  AddField('PROCEDURE_SCHEMA', dtString, 128);
  AddField('PROCEDURE_NAME', dtString, 128);
  AddField('PROCEDURE_TYPE', dtString, 0);
  AddField('DATE_CREATED', dtDateTime);
  AddField('DATE_MODIFIED', dtDateTime);
  AddField('OVERLOAD', dtInt32);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateProcedureParametersFields;
begin
  FMemData.Fields.Clear;
  AddField('PROCEDURE_CATALOG', dtString, 128);
  AddField('PROCEDURE_SCHEMA', dtString, 128);
  AddField('PROCEDURE_NAME', dtString, 128);
  AddField('PARAMETER_NAME', dtString, 128);
  AddField('POSITION', dtInt32);
  AddField('DIRECTION', dtString, 10);
  AddField('DATA_TYPE', dtInt32);
  AddField('DATA_LENGTH', dtInt32);
  AddField('DATA_PRECISION', dtInt32);
  AddField('DATA_SCALE', dtInt32);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateIndexColumnsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('INDEX_CATALOG', dtString, 128);
  AddField('INDEX_SCHEMA', dtString, 128);
  AddField('INDEX_NAME', dtString, 128);
  AddField('COLUMN_NAME', dtString, 128);
  AddField('COLUMN_POSITION', dtInt32);
  AddField('SORT_ORDER', dtString, 4);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateConstraintsFields;
begin
  FMemData.Fields.Clear;
  AddField('TABLE_CATALOG', dtString, 128);
  AddField('TABLE_SCHEMA', dtString, 128);
  AddField('TABLE_NAME', dtString, 128);
  AddField('CONSTRAINT_NAME', dtString, 128);
  AddField('CONSTRAINT_TYPE', dtString, 11);
  AddField('INDEX_CATALOG', dtString, 0);
  AddField('INDEX_SCHEMA', dtString, 0);
  AddField('INDEX_NAME', dtString, 0);
  FMemData.InitFields;
end;

procedure TMSSQLMetaData.CreateDatabasesFields;
begin
  FMemData.Fields.Clear;
  AddField('DATABASE_NAME', dtString, 128);
  FMemData.InitFields;
end;
{$ENDIF DBX_METADATA}

{$ENDIF}

{ TMSSQLTableObject }

function TMSSQLTableObject.GetIsNull: boolean;
begin
  Result := (FRecordSet = nil) or not FRecordSet.Active;
end;

procedure TMSSQLTableObject.SetData(RecordSet: TSqlRecordSet);
begin
  FRecordSet := RecordSet;
end;

{ TMSLoaderColumn }

procedure TMSLoaderColumn.UpdateDataType(Value: word);
begin
  if (Value = dtString) and FIsWide then
    Value := dtWideString;

  if (Value = dtMemo) and FIsWide then
    Value := dtWideMemo;

  inherited UpdateDataType(Value);
end;

initialization

  MSSQLInfo := TMSSQLInfo.Create(nil);

finalization
  MSSQLInfo.Free;

end.
