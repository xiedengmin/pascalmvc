
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I LiteDac.inc}
{$IFDEF VER10P}
{$I SQLiteUniProvider.inc}
{$ENDIF}
unit SQLiteUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRAccess, CRConnectionPool, CRDataTypeMap,  CRTypes, CRParser, CRConnectionString, CRServerEnumerator,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  Uni, UniProvider,
{$IFNDEF UNIDACPRO}
  LiteCall, LiteClasses, LiteConnectionPool, LiteCollation, LiteFunction;
{$ELSE}
  LiteCallUni, LiteClassesUni, LiteConnectionPoolUni, LiteCollationUni, LiteFunctionUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TSQLiteUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsServerSupported: boolean; override;
    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;

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

    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
  end;

  TLiteConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TLiteFormatter = class(TUniSqlFormatter)
  protected
  public
    constructor Create; override;
  end;

  TLiteUtils = class
  private
    class function GetConnection(Connection: TCustomDAConnection): TSQLiteConnection;
  public
    { SQLite user collations }
    class procedure RegisterCollation(Connection: TCustomDAConnection; Name: string; LiteCollation: TLiteCollation);
    class procedure UnRegisterCollation(Connection: TCustomDAConnection; Name: string);
    class procedure RegisterAnsiCollation(Connection: TCustomDAConnection; Name: string; LiteAnsiCollation: TLiteAnsiCollation);
    class procedure UnRegisterAnsiCollation(Connection: TCustomDAConnection; Name: string);
    class procedure RegisterWideCollation(Connection: TCustomDAConnection; Name: string; LiteWideCollation: TLiteWideCollation);
    class procedure UnRegisterWideCollation(Connection: TCustomDAConnection; Name: string);

    { SQLite user functions }
    class procedure RegisterFunction(Connection: TCustomDAConnection; const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction);
    class procedure RegisterAggregateFunction(Connection: TCustomDAConnection; const Name: string; ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction);
    class procedure UnRegisterFunction(Connection: TCustomDAConnection; Name: string; ParamCount: Integer);

    { SQLite encryption }
    class procedure EncryptDatabase(Connection: TCustomDAConnection; NewKey: string);

    { misc functions }
    class procedure ReleaseDatabaseMemory(Connection: TCustomDAConnection);
    class function IsDatabaseReadOnly(Connection: TCustomDAConnection; const DatabaseName: string = ''): boolean;
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  CRProps, DAConsts, UniConsts,
{$IFNDEF STD}
  DALoader, UniLoader,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  LiteConsts, LiteProps, LiteParser, LiteServices, LiteDataTypeMap, LiteConnectionString;
{$ELSE}
  LiteConstsUni, LitePropsUni, LiteParserUni, LiteServicesUni, LiteDataTypeMapUni, LiteConnectionStringUni;
{$ENDIF}

var
  LiteFunctions, LiteMacros: TStrValueStringList;

class function TSQLiteUniProvider.GetProviderName: string;
begin
  Result := 'SQLite';
end;

class function TSQLiteUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TLiteConnectionStringBuilder;
end;

function TSQLiteUniProvider.IsServerSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TSQLiteUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TSQLiteUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TSQLiteUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TLiteParser;
end;

function TSQLiteUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TLiteConnectionParameters;
end;

function TSQLiteUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TLiteConnectionPoolManager;
end;

function TSQLiteUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TLiteConverterManager;
end;

function TSQLiteUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TSQLiteConnection;
end;

function TSQLiteUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TLiteServerEnumerator;
end;

function TSQLiteUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomLiteDataSetService;
end;

function TSQLiteUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TLiteScriptProcessor;
end;

{$IFNDEF STD}

function TSQLiteUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomLiteDumpProcessor;
end;

{$ENDIF}

function TSQLiteUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TLiteConnectDialogService;
end;

function TSQLiteUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomLiteFieldTypeMap;
end;

function TSQLiteUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TLiteFormatter;
end;

procedure TSQLiteUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
{$IFNDEF STD}
  if Obj is TUniLoader then
    GetLoaderOptions.ImportOptions(Options, Obj, nil);
{$ENDIF}

  inherited;
end;

procedure TSQLiteUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ASCIIDataBase', prASCIIDataBase, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('BusyTimeout', prBusyTimeout, [TSQLiteConnection, TLiteConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ClientLibrary', prClientLibrary, [TSQLiteConnection, TLiteConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prStaticLibrary, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('EncryptionAlgorithm', prEncryptionAlgorithm, [TSQLiteConnection, TLiteConnectionParameters], Variant(leDefault), TypeInfo(TLiteEncryptionAlgorithm)));
    FConnectionOptions.Add(TStringOption.Create('EncryptionKey', prEncryptionKey, [TSQLiteConnection, TLiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('DateFormat', prDateFormat, [TSQLiteConnection, TLiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('TimeFormat', prTimeFormat, [TSQLiteConnection, TLiteConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('NativeDate', prNativeDate, [TSQLiteConnection, TLiteConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('DefaultCollations', prDefaultCollations, [TSQLiteConnection, TLiteConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('EnableSharedCache', prEnableSharedCache, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('EnableLoadExtension', prEnableLoadExtension, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('ReadUncommitted', prReadUncommitted, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('ForeignKeys', prForeignKeys, [TSQLiteConnection, TLiteConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('ForceCreateDatabase', prForceCreateDatabase, [TSQLiteConnection, TLiteConnectionParameters], False));
    FConnectionOptions.Add(TStringOption.Create('CipherLicense', prCipherLicense, [TSQLiteConnection, TLiteConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('ConnectMode', prConnectMode, [TSQLiteConnection, TLiteConnectionParameters], Variant(DefValConnectMode), TypeInfo(TConnectMode)));
    FConnectionOptions.Add(TEnumeratorOption.Create('LockingMode', prLockingMode, [TSQLiteConnection, TLiteConnectionParameters], Variant(DefValLockingMode), TypeInfo(TLockingMode)));
    FConnectionOptions.Add(TEnumeratorOption.Create('Synchronous', prSynchronous, [TSQLiteConnection, TLiteConnectionParameters], Variant(DefValSynchronous), TypeInfo(TSynchronous)));
    FConnectionOptions.Add(TEnumeratorOption.Create('JournalMode', prJournalMode, [TSQLiteConnection, TLiteConnectionParameters], Variant(DefValJournalMode), TypeInfo(TJournalMode)));
  end;
end;

procedure TSQLiteUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
  end;
end;

procedure TSQLiteUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TSQLiteRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TSQLiteRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UnknownAsString', prUnknownAsString, [TSQLiteRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('AdvancedTypeDetection', prAdvancedTypeDetection, [TSQLiteRecordSet], False));
  end;
end;

procedure TSQLiteUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TSQLiteUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('AutoCommit', prAutoCommit, [TSQLiteLoader], True));
    FLoaderOptions.Add(TIntegerOption.Create('AutoCommitRowCount', prAutoCommitRowCount, [TSQLiteLoader], 1000));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TSQLiteLoader], False));
  end;
end;
{$ENDIF}

{ TLiteConnectDialogService }

function TLiteConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TLiteConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TLiteConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TLiteConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TLiteFormatter }

constructor TLiteFormatter.Create;
begin
  inherited;

  FFunctions := LiteFunctions;
  FPredefinedMacros := LiteMacros;
end;

class function TLiteUtils.GetConnection(Connection: TCustomDAConnection): TSQLiteConnection;
var
  CRConnection: TCRConnection;
begin
  CRConnection := TDBAccessUtils.GetIConnection(Connection);

  if CRConnection = nil then
    raise Exception.Create(SConnectionIsClosed)
  else if not (CRConnection is TSQLiteConnection) then
    raise Exception.CreateFmt(SIncorrectConnectionType, ['TSQLiteConnection', CRConnection.ClassName]);

  Result := TSQLiteConnection(CRConnection);
end;

class procedure TLiteUtils.RegisterCollation(Connection: TCustomDAConnection; Name: string; LiteCollation: TLiteCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterCollation(Name, LiteCollation);
end;

class procedure TLiteUtils.UnRegisterCollation(Connection: TCustomDAConnection; Name: string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterCollation(Name);
end;

class procedure TLiteUtils.RegisterAnsiCollation(Connection: TCustomDAConnection; Name: string; LiteAnsiCollation: TLiteAnsiCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterAnsiCollation(Name, LiteAnsiCollation);
end;

class procedure TLiteUtils.UnRegisterAnsiCollation(Connection: TCustomDAConnection; Name: string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterAnsiCollation(Name);
end;

class procedure TLiteUtils.RegisterWideCollation(Connection: TCustomDAConnection; Name: string; LiteWideCollation: TLiteWideCollation);
begin
  GetConnection(Connection).GetCollationManager.RegisterWideCollation(Name, LiteWideCollation);
end;

class procedure TLiteUtils.UnRegisterWideCollation(Connection: TCustomDAConnection; Name: string);
begin
  GetConnection(Connection).GetCollationManager.UnRegisterWideCollation(Name);
end;

class procedure TLiteUtils.RegisterFunction(Connection: TCustomDAConnection; const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction);
begin
  GetConnection(Connection).GetFunctionManager.RegisterFunction(Name, ParamCount, LiteFunction);
end;

class procedure TLiteUtils.RegisterAggregateFunction(Connection: TCustomDAConnection; const Name: string; ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction);
begin
  GetConnection(Connection).GetFunctionManager.RegisterAggregateFunction(Name, ParamCount, StepFunction, FinalFunction);
end;

class procedure TLiteUtils.UnRegisterFunction(Connection: TCustomDAConnection; Name: string; ParamCount: Integer);
begin
  GetConnection(Connection).GetFunctionManager.UnRegisterFunction(Name, ParamCount);
end;

class procedure TLiteUtils.EncryptDatabase(Connection: TCustomDAConnection; NewKey: string);
begin
  GetConnection(Connection).EncryptDatabase(NewKey);
end;

class procedure TLiteUtils.ReleaseDatabaseMemory(Connection: TCustomDAConnection);
begin
  GetConnection(Connection).ReleaseDatabaseMemory;
end;

class function TLiteUtils.IsDatabaseReadOnly(Connection: TCustomDAConnection; const DatabaseName: string): boolean;
var
  Res: integer;
begin
  Res := GetConnection(Connection).GetDatabaseReadOnly(DatabaseName);
  if Res <> -1 then
    Result := boolean(Res)
  else
    raise Exception.CreateFmt(SDatabaseDoesntExist, [DatabaseName]);
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TSQLiteUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSQLiteUniProvider);

  LiteFunctions := TStrValueStringList.Create;
  //LiteFunctions.Add('USER', '');
  LiteFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  //LiteFunctions.Add('LOCATE',      'STRPOS(%1:s, %0:s)');
  LiteFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  LiteFunctions.Add('CONCAT',      '%s || %s');
  //LiteFunctions.Add('CHAR',        'CHR(%s)');
  LiteFunctions.Add('TRIM',        'TRIM(%s)');
  //LiteFunctions.Add('TRUNCATE',    'TRUNC(%s, %s)');
  //LiteFunctions.Add('CEILING',     'CEILING(%s)');
  LiteFunctions.Add('UPPER',       'UPPER(%s)');
  LiteFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  LiteFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  //LiteFunctions.Add('YEAR',         'EXTRACT(YEAR FROM %s)');
  //LiteFunctions.Add('MONTH',        'EXTRACT(MONTH FROM %s)');
  //LiteFunctions.Add('DAY',          'EXTRACT(DAY FROM %s)');
  LiteFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  //LiteFunctions.Add('DATEDIFF',     'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  LiteFunctions.Add('__DATE_TIME_LITERAL', '%s');
  LiteFunctions.Add('__DATE_LITERAL',      '%s');
  LiteFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  LiteFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  LiteFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  LiteFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  LiteMacros := TStrValueStringList.Create;
  LiteMacros.Add('PROVIDER', 'SQLite');
  LiteMacros.Add('SQLITE',   '');
  // DataType macros
  LiteMacros.Add('DATETIME', 'TEXT');
  LiteMacros.Add('DOUBLE',   'DOUBLE');
  LiteMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TSQLiteUniProvider);

  LiteFunctions.Free;
  LiteMacros.Free;

end.
