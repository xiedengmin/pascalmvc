
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I PgDac.inc}
{$IFDEF VER10P}
{$I PostgreSQLUniProvider.inc}
{$ENDIF}
unit PostgreSQLUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
  CRAccess, CRConnectionPool, CRTypes, CRParser, CRDataTypeMap, MemData, CRConnectionString, CRServerEnumerator,
  DBAccess, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  UniProvider;

type

  TCustomPostgreSQLUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}
  public
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
    function GetAlerterClass: TCRAlerterClass; override;
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
    function GetParamObjectClass(Param: TDAParam): TClass; override;
    function CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject; override;
    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;

    // for Failover Demo
    class function GetSocket(Connection: TCustomDAConnection): {$IFNDEF CLR}longint{$ELSE}IntPtr{$ENDIF};
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TPostgreSQLUniProvider = class(TCustomPostgreSQLUniProvider)
  protected
    procedure CreateConnectionOptions; override;
  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;
  end;

  TPgConnectDialogService = class(TConnectDialogService)
  public
    function GetDefaultDatabase: string; override;
  end;

  TPgSqlFormatter = class(TUniSqlFormatter)
  protected
  public
    constructor Create; override;
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  DAConsts, UniConsts, CRProps, CRVio, CRVioTcp,
{$IFNDEF UNIDACPRO}
  PgProps, PgParser, PgClasses, PgObjects, PgConnectionPool, PgServices, PgScriptProcessor,
  PgSQLNet, PgDataTypeMap, PgSQLGenerator, PgConnectionString, PgServerEnumerator;
{$ELSE}
  PgPropsUni, PgParserUni, PgClassesUni, PgObjectsUni, PgConnectionPoolUni, PgServicesUni,
  PgScriptProcessorUni, PgSQLNetUni, PgDataTypeMapUni, PgSQLGeneratorUni, PgConnectionStringUni, PgServerEnumeratorUni;
{$ENDIF}

var
  PgFunctions, PgMacros: TStrValueStringList;

{ TCustomPostgreSQLUniProvider }

procedure TCustomPostgreSQLUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeOut, [TPgSQLConnection, TPgConnectionParameters], 15));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TPgSQLConnection, TPgConnectionParameters], Variant(ivIPv4), TypeInfo(TIPVersion)));
    FConnectionOptions.Add(TStringOption.Create('MessagesCharset', prMessagesCharset, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('MultipleConnections', prMultipleConnections, [TPgSQLConnection, TPgConnectionParameters], DefValMultipleConnections));
    FConnectionOptions.Add(TBooleanOption.Create('UuidWithBraces', prUuidWithBraces, [TPgSQLConnection], True));

    // SSL
    FConnectionOptions.Add(TEnumeratorOption.Create('SSLMode', prSSLMode, [TPgSQLConnection, TPgConnectionParameters], Variant(smDisable), TypeInfo(TSSLMode)));
    FConnectionOptions.Add(TStringOption.Create('SSLCACert', prSSLCA, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCert', prSSLCert, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLKey', prSSLKey, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCipherList', prSSLCipher, [TPgSQLConnection, TPgConnectionParameters], ''));

    // HTTP
    FConnectionOptions.Add(TBooleanOption.Create('UseHttp', prUseHttp, [TPgSQLConnection, TPgConnectionParameters], False));
    FConnectionOptions.Add(TStringOption.Create('HttpUrl', prHttpUrl, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpUsername', prHttpUsername, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpPassword', prHttpPassword, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyHostname', prProxyHostname, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TPgSQLConnection, TPgConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUsername', prProxyUsername, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('HttpTrustServerCertificate', prHttpTrustServerCertificate, [TPgSQLConnection, TPgConnectionParameters], False));
  end;
end;

procedure TCustomPostgreSQLUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TPgSQLCommand], 0));
    FSQLOptions.Add(TBooleanOption.Create('UnpreparedExecute', prSimpleQueryExecute, [TPgSQLCommand], False));
    FSQLOptions.Add(TBooleanOption.Create('UseParamTypes', prUseParamTypes, [TPgSQLCommand], False));
  end;
end;

procedure TCustomPostgreSQLUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('AutoDeleteBlob', prAutoDeleteBlob, [TCustomPgDataSetService], True));
    FDataSetOptions.Add(TBooleanOption.Create('CacheBlobs', prCacheBlobs, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TPgSQLRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('CursorWithHold', prCursorWithHold, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredBlobRead', prDeferredBlobRead, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TPgSQLRecordSet], True));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomPgDataSetService], ''));
    FDataSetOptions.Add(TBooleanOption.Create('OIDAsInt', prOIDAsInt, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TIntegerOption.Create('PrefetchRows', prPrefetchRows, [TPgSQLRecordSet], 0));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomPgDataSetService], Variant(_smPost), TypeInfo(_TSequenceMode)));
    FDataSetOptions.Add(TBooleanOption.Create('UnknownAsString', prUnknownAsString, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UnpreparedExecute', prSimpleQueryExecute, [TPgSQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UseParamTypes', prUseParamTypes, [TPgSQLRecordSet], False));
  end;
end;

procedure TCustomPostgreSQLUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TCustomPostgreSQLUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('TextMode', prTextMode, [TPgSQLLoader], False));
    FLoaderOptions.Add(TIntegerOption.Create('BufferSize', prBufferSize, [TPgSQLLoader], WriteBufferSize));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TPgSQLLoader], False));
  end;
end;
{$ENDIF}

function TCustomPostgreSQLUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TCustomPostgreSQLUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TCustomPostgreSQLUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TPgParser;
end;

function TCustomPostgreSQLUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TPgConnectionParameters;
end;

function TCustomPostgreSQLUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TPgConnectionPoolManager;
end;

function TCustomPostgreSQLUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TPgSQLConnection;
end;

function TCustomPostgreSQLUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TPgServerEnumerator;
end;

function TCustomPostgreSQLUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomPgDataSetService;
end;

function TCustomPostgreSQLUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TPgScriptProcessor;
end;

{$IFNDEF STD}

function TCustomPostgreSQLUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TPgSQLAlerter;
end;

function TCustomPostgreSQLUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomPgDumpProcessor;
end;

{$ENDIF}

function TCustomPostgreSQLUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TPgConnectDialogService;
end;

function TCustomPostgreSQLUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomPgFieldTypeMap;
end;

function TCustomPostgreSQLUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TPgSqlFormatter;
end;

function TCustomPostgreSQLUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TPgConverterManager;
end;

function TCustomPostgreSQLUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftOraBlob:
      Result := TPgSQLLargeObject;
    ftCursor:
      Result := TPgRefCursor;
  else
    Result := inherited GetParamObjectClass(Param);
  end;
end;

function TCustomPostgreSQLUniProvider.CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject;
begin
  case Param.DataType of
    ftOraBlob:
      Result := TPgSQLLargeObject.Create(nil);
    ftCursor:
      Result := TPgRefCursor.Create;
  else
    Result := inherited CreateParamObject(Param, IsUnicode);
  end;
end;

procedure TCustomPostgreSQLUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TPgSQLConnection then begin
    TPgSQLConnection(Obj).SetProp(prEnablePgTimeStamps, False);
    TPgSQLConnection(Obj).SetProp(prEnableGeometrics, False);
    TPgSQLConnection(Obj).SetProp(prEnableComposites, False);
    TPgSQLConnection(Obj).SetProp(prEnableDomains, True);
    TPgSQLConnection(Obj).SetProp(prIntervalAsString, True);
  end;

  inherited;
end;

class function TCustomPostgreSQLUniProvider.GetSocket(Connection: TCustomDAConnection): {$IFNDEF CLR}longint{$ELSE}IntPtr{$ENDIF};
var
  ICon: TPgSQLConnection;
begin
  Assert(Connection.Connected);
  ICon := TDBAccessUtils.GetIConnection(Connection) as TPgSQLConnection;
  Result := (ICon.GetProtocol.Net.Vio as TCRVioTcp).GetSocket;
end;

{ TPostgreSQLUniProvider }

class function TPostgreSQLUniProvider.GetProviderName: string;
begin
  Result := 'PostgreSQL';
end;

class function TPostgreSQLUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TPgConnectionStringBuilder;
end;

procedure TPostgreSQLUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    inherited;

    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('ProtocolVersion', prProtocolVersion, [TPgSQLConnection, TPgConnectionParameters], Variant(pvAuto), TypeInfo(TProtocolVersion)));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TPgSQLConnection, TPgConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TPgSQLConnection, TPgConnectionParameters], False));
  end;
end;

{ TPgConnectDialogService }

function TPgConnectDialogService.GetDefaultDatabase: string;
begin
  Result := 'template1';
end;

{ TPgSqlFormatter }

constructor TPgSqlFormatter.Create;
begin
  inherited;

  FFunctions := PgFunctions;
  FPredefinedMacros := PgMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TPostgreSQLUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TPostgreSQLUniProvider);

  PgFunctions := TStrValueStringList.Create;
  PgFunctions.Add('USER', 'USER()');
  PgFunctions.Add('CHAR_LENGTH', 'CHAR_LENGTH(%s)');
  PgFunctions.Add('LOCATE', 'STRPOS(%1:s, %0:s)');
  PgFunctions.Add('SUBSTRING', 'SUBSTRING(%s, %s, %s)');
  PgFunctions.Add('CONCAT', '%s || %s');
  PgFunctions.Add('CHAR', 'CHR(%s)');
  PgFunctions.Add('TRIM', 'TRIM(%s)');
  PgFunctions.Add('TRUNCATE', 'TRUNC(%s, %s)');
  PgFunctions.Add('CEILING', 'CEILING(%s)');
  PgFunctions.Add('UPPER',   'UPPER(%s)');
  PgFunctions.Add('LOWER',   'LOWER(%s)');
  // Date-time
  PgFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  PgFunctions.Add('YEAR', 'EXTRACT(YEAR FROM %s)');
  PgFunctions.Add('MONTH', 'EXTRACT(MONTH FROM %s)');
  PgFunctions.Add('DAY', 'EXTRACT(DAY FROM %s)');
  PgFunctions.Add('DATEADD', '(%2:s + INTERVAL'' %1:s %0:s'')');
  PgFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  PgFunctions.Add('__DATE_TIME_LITERAL', 'TIMESTAMP %s');
  PgFunctions.Add('__DATE_LITERAL', 'DATE %s');
  PgFunctions.Add('__TIME_LITERAL', 'TIME %s');
  // CONVERT functions
  PgFunctions.Add('TODATE', 'CAST(%s AS TIMESTAMP)');
  PgFunctions.Add('TONUMBER', 'CAST(%s AS NUMERIC)');
  PgFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  PgMacros := TStrValueStringList.Create;
  PgMacros.Add('PROVIDER', 'PostgreSQL');
  PgMacros.Add('POSTGRESQL', '');
  // DataType macros
  PgMacros.Add('DATETIME', 'TIMESTAMP');
  PgMacros.Add('DOUBLE', 'DOUBLE PRECISION');
  PgMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TPostgreSQLUniProvider);

  PgFunctions.Free;
  PgMacros.Free;

end.
