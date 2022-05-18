
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Sdac.inc}
{$IFDEF VER10P}
{$I SQLServerUniProvider.inc}
{$ENDIF}
unit SQLServerUniProvider;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes,
  CRTypes, CRFunctions, CRAccess, CRParser, CRConnectionPool, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, DBAccess,
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  UniProvider,
{$IFNDEF UNIDACPRO}
  {$IFDEF MSWINDOWS}OLEDBAccess,{$ENDIF}
  {$IFDEF TDS}TdsClasses, Tds7Classes, {$ENDIF}
  SqlClasses, MSClasses, MSServerEnumerator;
{$ELSE}
  {$IFDEF MSWINDOWS}OLEDBAccessUni,{$ENDIF}
  {$IFDEF TDS}TdsClassesUni, Tds7ClassesUni,{$ENDIF}
  SqlClassesUni, MSClassesUni, MSServerEnumeratorUni;
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFDEF TDS}
  {$DEFINE BOTH_PROV}
{$ENDIF}
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TSQLServerUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
    procedure CreateDumpOptions; override;
  {$ENDIF}

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;

    function GetParserClass: TSQLParserClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
  {$IFNDEF STD}
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
    function CheckParamName(const ParamName: string): string; override;

    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
    function DefaultTableSchema: string; override;
  end;

  TMSConnectDialogService = class (TConnectDialogService)
  private
    FProvider: TMSProvider;
    FAuthentication: TMSAuthentication;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function UseDatabaseHistory: boolean; override;
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TMSSqlFormatter = class(TUniSqlFormatter)
  protected
    function IsServerKeywordLexem(Parser: TSQLParser; const Lexem: string): Boolean; override;
  public
    constructor Create; override;

    function LeftQuote: Char; override;
    function RightQuote: Char; override;
  end;

  TMSSqlUtils = class
  private
    class function GetConnection(Connection: TCustomDAConnection): TMSSQLConnection;
  public
    class procedure ChangePassword(Connection: TCustomDAConnection; NewPassword: string);
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  TypInfo, CRProps, CRVio,DAConsts,
{$IFNDEF UNIDACPRO}
  MSProps, MSParser, MSConnectionPool, MSServices, MSScriptProcessor,
  MSDataTypeMap, MSConnectionString;
{$ELSE}
  MSPropsUni, MSParserUni, MSConnectionPoolUni, MSServicesUni, MSScriptProcessorUni,
  MSDataTypeMapUni, MSConnectionStringUni;
{$ENDIF}

var
  MSFunctions, MSMacros: TStrValueStringList;

class function TSQLServerUniProvider.GetProviderName: string;
begin
  Result := 'SQL Server';
end;

class function TSQLServerUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TMSConnectionStringBuilder;
end;

function TSQLServerUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TSQLServerUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TSQLServerUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

function TSQLServerUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMSConnectionParameters;
end;

function TSQLServerUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMSConnectionPoolManager;
end;

function TSQLServerUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMSSQLConnection;
end;

function TSQLServerUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TMSServerEnumerator;
end;

function TSQLServerUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomMSDataSetService;
end;

function TSQLServerUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMSScriptProcessor;
end;

{$IFNDEF STD}

function TSQLServerUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomMSDumpProcessor;
end;

{$ENDIF}

function TSQLServerUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMSConnectDialogService;
end;

function TSQLServerUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomMSFieldTypeMap;
end;

function TSQLServerUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMSSqlFormatter;
end;

function TSQLServerUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TMSConverterManager;
end;

function TSQLServerUniProvider.CheckParamName(const ParamName: string): string;
begin
  Result := GetParamNameWODog(ParamName);
end;

procedure TSQLServerUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TMSSQLConnection then
    TMSSQLConnection(Obj).SetProp(prSetLockTimeout, True)
{$IFDEF TDS}
 else
  if Obj is TTDS7Command then
    TTDS7Command(Obj).SetProp(prSensibleBCDMapping, True)
{$ENDIF}
{$IFDEF MSWINDOWS}
 else
  if Obj is TOLEDBCommand then
    TOLEDBCommand(Obj).SetProp(prSensibleBCDMapping, True)
{$ENDIF}
  ;

  inherited;
end;

procedure TSQLServerUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);

    FConnectionOptions.Add(TBooleanOption.Create('QuotedIdentifier', prQuotedIdentifier, [TMSSQLConnection, TMSConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('Encrypt', prEncrypt, [TMSSQLConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('Provider', prProvider, [TMSSQLConnection, TMSConnectionParameters, TMSServerEnumerator, TMSConnectDialogService], Variant(prAuto), TypeInfo(TMSProvider)));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TMSSQLConnection, TMSConnectionParameters], Variant(ivIPv4), TypeInfo(TIPVersion)));
  {$IFDEF MSWINDOWS}
    FConnectionOptions.Add(TEnumeratorOption.Create('NativeClientVersion', prNativeClientVersion, [TMSSQLConnection, TMSConnectionParameters], Variant(ncAuto), TypeInfo(TNativeClientVersion)));
  {$ENDIF}
    FConnectionOptions.Add(TBooleanOption.Create('ForceCreateDatabase', prForceCreateDatabase, [TMSSQLConnection, TMSConnectionParameters], DefValForceCreateDatabase));

    // TMSConnection options
  {$IFDEF MSWINDOWS}
    FConnectionOptions.Add(TEnumeratorOption.Create('Authentication', prAuthentication, [TMSSQLConnection, TMSConnectionParameters, TMSConnectDialogService], Variant(auServer), TypeInfo(TMSAuthentication)));
  {$ENDIF}
    FConnectionOptions.Add(TEnumeratorOption.Create('ApplicationIntent', prApplicationIntent, [TMSSQLConnection, TMSConnectionParameters], Variant(DefValApplicationIntent), TypeInfo(TApplicationIntent)));
    FConnectionOptions.Add(TBooleanOption.Create('MultiSubnetFailover', prMultiSubnetFailover, [TMSSQLConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TMSSQLConnection], 15));
    FConnectionOptions.Add(TStringOption.Create('Language', prLanguage, [TMSSQLConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('PersistSecurityInfo', prPersistSecurityInfo, [TMSSQLConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('AutoTranslate', prAutoTranslate, [TMSSQLConnection, TMSConnectionParameters], True));
    FConnectionOptions.Add(TStringOption.Create('NetworkLibrary', prNetworkLibrary, [TMSSQLConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TMSSQLConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('WorkstationID', prWorkstationID, [TMSSQLConnection, TMSConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('PacketSize', prPacketSize, [TMSSQLConnection, TMSConnectionParameters], 4096));
    FConnectionOptions.Add(TStringOption.Create('InitialFileName', prInitialFileName, [TMSSQLConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('MultipleActiveResultSets', prMARS, [TMSSQLConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('MultipleConnections', prMultipleConnections, [TMSSQLConnection], True));
    FConnectionOptions.Add(TStringOption.Create('FailoverPartner', prFailoverPartner, [TMSSQLConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('TrustServerCertificate', prTrustServerCertificate, [TMSSQLConnection, TMSConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('LockTimeout', prDefaultLockTimeout, [TMSSQLConnection], DefaultDefaultLockTimeout));
    FConnectionOptions.Add(TBooleanOption.Create('UuidWithBraces', prUuidWithBraces, [TMSSQLConnection], True));
  {$IFDEF VER10P}
    FConnectionOptions.Add(TBooleanOption.Create('UseWideMemos', prWideMemos, [TMSSQLConnection], True));
  {$ENDIF}

    // TCompactConnection options
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactInitMode', prInitMode, [TMSSQLConnection], Variant(imReadWrite), TypeInfo(TMSInitMode)));
    FConnectionOptions.Add(TIntegerOption.Create('CompactLocaleIdentifier', prLocaleIdentifier, [TMSSQLConnection], {$IFDEF MSWINDOWS}GetSystemDefaultLCID{$ELSE}0{$ENDIF}));
    FConnectionOptions.Add(TIntegerOption.Create('CompactLockEscalation', prLockEscalation, [TMSSQLConnection], 100));
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactTransactionCommitMode', prTransactionCommitMode, [TMSSQLConnection], Variant(cmAsynchCommit), TypeInfo(TCompactCommitMode)));
    FConnectionOptions.Add(TIntegerOption.Create('CompactMaxDatabaseSize', prMaxDatabaseSize, [TMSSQLConnection], 128));
    FConnectionOptions.Add(TIntegerOption.Create('CompactMaxBufferSize', prMaxBufferSize, [TMSSQLConnection], 640));
    FConnectionOptions.Add(TStringOption.Create('CompactTempFileDirectory', prTempFileDirectory, [TMSSQLConnection], ''));
    FConnectionOptions.Add(TIntegerOption.Create('CompactTempFileMaxSize', prTempFileMaxSize, [TMSSQLConnection], 128));
    FConnectionOptions.Add(TIntegerOption.Create('CompactDefaultLockEscalation', prDefaultLockEscalation, [TMSSQLConnection], 100));
    FConnectionOptions.Add(TIntegerOption.Create('CompactAutoShrinkThreshold', prAutoShrinkThreshold, [TMSSQLConnection], 60));
    FConnectionOptions.Add(TIntegerOption.Create('CompactFlushInterval', prFlushInterval, [TMSSQLConnection], 10));
    FConnectionOptions.Add(TEnumeratorOption.Create('CompactVersion', prCompactVersion, [TMSSQLConnection], Variant(cvAuto), TypeInfo(TCompactVersion)));

    FConnectionOptions.AddDeprecated('OLEDBProvider', FConnectionOptions.OptionByName('Provider'));
  end;
end;

procedure TSQLServerUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF MSWINDOWS}TOLEDBCommand{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Command{$ENDIF}], 0));
    FSQLOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [{$IFDEF MSWINDOWS}TOLEDBCommand{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Command{$ENDIF}], False));
    FSQLOptions.Add(TBooleanOption.Create('NonBlocking', prNonBlocking, [{$IFDEF MSWINDOWS}TOLEDBCommand{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Command{$ENDIF}], False));
  end;
end;

procedure TSQLServerUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);

    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], 0));
    FDataSetOptions.Add(TBooleanOption.Create('UniqueRecords', prUniqueRecords, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], True));
    FDataSetOptions.Add(TBooleanOption.Create('HideSystemUniqueFields', prHideSystemUniqueFields, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], True));
    FDataSetOptions.Add(TBooleanOption.Create('CursorUpdate', prCursorUpdate, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], True));
    FDataSetOptions.Add(TBooleanOption.Create('QueryIdentity', prQueryIdentity, [TCustomMSDataSetService], True));
    FDataSetOptions.Add(TBooleanOption.Create('CheckRowVersion', prCheckRowVersion, [TCustomMSDataSetService], False));
    FDataSetOptions.Add(TBooleanOption.Create('DisableMultipleResults', prDisableMultipleResults, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], False));
    FDataSetOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [{$IFDEF MSWINDOWS}TOLEDBCommand{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Command{$ENDIF}], False));
    FDataSetOptions.Add(TEnumeratorOption.Create('LastIdentityValueFunction', prLastIdentityValueFunction, [TCustomMSDataSetService], Variant(vfScopeIdentity), TypeInfo(TMSLastIdentityValueFunction)));
    FDataSetOptions.Add(TEnumeratorOption.Create('CursorType', prCursorType, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], Variant(ctDefaultResultSet), TypeInfo(TMSCursorType)));
    FDataSetOptions.Add(TBooleanOption.Create('NonBlocking', prNonBlocking, [{$IFDEF MSWINDOWS}TOLEDBRecordSet{$ENDIF} {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7RecordSet{$ENDIF}], False));
  end;
end;

procedure TSQLServerUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TSQLServerUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
  {$IFDEF MSWINDOWS}
    FLoaderOptions.Add(TBooleanOption.Create('KeepIdentity', prKeepIdentity, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
    FLoaderOptions.Add(TBooleanOption.Create('KeepNulls', prKeepNulls, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerBatch', prRowsPerBatch, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], 0));
    FLoaderOptions.Add(TIntegerOption.Create('KilobytesPerBatch', prKilobytesPerBatch, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], 0));
    FLoaderOptions.Add(TBooleanOption.Create('LockTable', prLockTable, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
    FLoaderOptions.Add(TBooleanOption.Create('CheckConstraints', prCheckConstraints, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
    FLoaderOptions.Add(TBooleanOption.Create('FireTrigger', prFireTrigger, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TOLEDBLoader {$IFDEF BOTH_PROV},{$ENDIF} {$IFDEF TDS}TTDS7Loader{$ENDIF}], False));
  {$ENDIF}
  end;
end;

procedure TSQLServerUniProvider.CreateDumpOptions;
begin
  if FDumpOptions = nil then begin
    FDumpOptions := TOptionsList.Create(GetProviderName);
    FDumpOptions.Add(TBooleanOption.Create('IdentityInsert', prIdentityInsert, [TCustomMSDumpProcessor], False));
  end;
end;
{$ENDIF}

function TSQLServerUniProvider.DefaultTableSchema: string;
begin
  Result := DefaultSDACSchema;
end;

{ TMSConnectDialogService }

function TMSConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prProvider:
      FProvider := TMSProvider(Value);
    prAuthentication:
      FAuthentication := TMSAuthentication(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMSConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := {$IFDEF MSWINDOWS}FProvider = prCompact{$ELSE}False{$ENDIF};
end;

function TMSConnectDialogService.UsernameEnabled: boolean;
begin
  Result := {$IFDEF MSWINDOWS}(FProvider <> prCompact) and (FAuthentication <> auWindows){$ELSE}True{$ENDIF};
end;

function TMSConnectDialogService.PasswordEnabled: boolean;
begin
  Result := {$IFDEF MSWINDOWS}(FProvider = prCompact) or (FAuthentication <> auWindows){$ELSE}True{$ENDIF};
end;

function TMSConnectDialogService.ServerEnabled: boolean;
begin
  Result := {$IFDEF MSWINDOWS}FProvider <> prCompact{$ELSE}True{$ENDIF};
end;

function TMSConnectDialogService.PortEnabled: boolean;
begin
  Result := {$IFDEF MSWINDOWS}FProvider <> prCompact{$ELSE}True{$ENDIF};
end;

{ TMSSqlFormatter }

constructor TMSSqlFormatter.Create;
begin
  inherited;

  FFunctions := MSFunctions;
  FPredefinedMacros := MSMacros;
end;

function TMSSqlFormatter.LeftQuote: Char;
begin
  Result := '[';
end;

function TMSSqlFormatter.RightQuote: Char;
begin
  Result := ']';
end;

function TMSSqlFormatter.IsServerKeywordLexem(Parser: TSQLParser; const Lexem: string): Boolean;
var
  St: string;
begin
  if Lexem = ':' then begin
    Parser.GetNext(St);
    Parser.Back;
    Result := UpperCase(St) = 'RETURN_VALUE';
  end
  else
    Result := False;
end;

{ TMSSqlUtils }

class function TMSSqlUtils.GetConnection(Connection: TCustomDAConnection): TMSSQLConnection;
var
  CRConnection: TCRConnection;
begin
  CRConnection := TDBAccessUtils.GetIConnection(Connection);

  if CRConnection = nil then
    raise Exception.Create(SConnectionIsClosed)
  else if not (CRConnection is TMSSQLConnection) then
    raise Exception.CreateFmt(SIncorrectConnectionType, ['TMSSQLConnection', CRConnection.ClassName]);

  Result := TMSSQLConnection(CRConnection);
end;

class procedure TMSSqlUtils.ChangePassword(Connection: TCustomDAConnection; NewPassword: string);
begin
  GetConnection(Connection).ChangePassword(NewPassword);
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TSQLServerUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSQLServerUniProvider);

  MSFunctions := TStrValueStringList.Create;
  MSFunctions.Add('USER', 'USER');
  MSFunctions.Add('CHAR_LENGTH', 'LEN(%s)');
  MSFunctions.Add('LOCATE', 'CHARINDEX(%s, %s)');
  MSFunctions.Add('SUBSTRING', 'SUBSTRING(%s, %s, %s)');
  MSFunctions.Add('CONCAT', '%s + %s');
  MSFunctions.Add('CHAR', 'CHAR(%s)');
  MSFunctions.Add('TRIM', 'LTRIM(RTRIM(%s))');
  MSFunctions.Add('TRUNCATE', 'ROUND(%s, %s, 1)');
  MSFunctions.Add('CEILING', 'CEILING(%s)');
  MSFunctions.Add('UPPER',   'UPPER(%s)');
  MSFunctions.Add('LOWER',   'LOWER(%s)');
  // Date-time
  MSFunctions.Add('CURRENT_DATE', 'GETDATE()');
  MSFunctions.Add('YEAR', 'YEAR(%s)');
  MSFunctions.Add('MONTH', 'MONTH(%s)');
  MSFunctions.Add('DAY', 'DAY(%s)');
  MSFunctions.Add('DATEDIFF', 'DATEDIFF(%s, %s, %s)');
  MSFunctions.Add('DATEADD', 'DATEADD(%s, %s, %s)');
  // Date-time literals
  MSFunctions.Add('__DATE_TIME_LITERAL', 'CONVERT(DATETIME, %s)');
  MSFunctions.Add('__DATE_LITERAL', 'CONVERT(DATE, %s)');
  MSFunctions.Add('__TIME_LITERAL', 'CONVERT(TIME, %s)');
  // CONVERT functions
  MSFunctions.Add('TODATE', 'CONVERT(DATETIME, %s)');
  MSFunctions.Add('TONUMBER', 'CONVERT(FLOAT(53), %s)');
  MSFunctions.Add('TOCHAR', 'CONVERT(VARCHAR, %s, 20)');

  MSMacros := TStrValueStringList.Create;
  MSMacros.Add('PROVIDER', 'SQL Server');
  MSMacros.Add('SQLSERVER', '');
  MSMacros.Add('SQL SERVER', '');
  // DataType macros
  MSMacros.Add('DATETIME', 'DATETIME');
  MSMacros.Add('DOUBLE', 'FLOAT(53)');
  MSMacros.Add('VARCHAR', 'VARCHAR');
  MSMacros.Add('CALL', 'EXEC');

finalization
  UniProviders.UnRegisterProvider(TSQLServerUniProvider);

  MSFunctions.Free;
  MSMacros.Free;

end.
