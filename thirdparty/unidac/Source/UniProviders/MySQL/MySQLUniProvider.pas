
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
{$IFDEF VER10P}
{$I MySQLUniProvider.inc}
{$ENDIF}
unit MySQLUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB, 
  MemData, CRAccess, CRConnectionPool, CRTypes, CRParser, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  DBAccess, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  UniProvider;

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TMySQLUniProvider = class(TUniProvider)
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
    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
  end;

  TMyConnectDialogService = class (TConnectDialogService)
  private
    FEmbedded: boolean;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function ServerEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TMySqlFormatter = class(TUniSqlFormatter)
  protected
    function GetFunction(const FunctionName: string; const Params: TStringArray): string; override;
    function GetDateDiff(const Params: TStringArray): string;
  public
    constructor Create; override;

    function LeftQuote: Char; override;
    function RightQuote: Char; override;
  end;

  function GetConnectionThreadID(Connection: TCustomDAConnection): integer;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

var
  MyFunctions: TStrValueStringList;

implementation

uses
  CRProps, CRFunctions, DAConsts, UniConsts, CRVio,
{$IFNDEF UNIDACPRO}
  MyProps, MyServices, MyClasses, MyConnectionPool, MyScriptProcessor,
  MyParser, MyDataTypeMap, MyConnectionString, MyServerEnumerator;
{$ELSE}
  MyPropsUni, MyServicesUni, MyClassesUni, MyConnectionPoolUni, MyScriptProcessorUni,
  MyParserUni, MyDataTypeMapUni, MyConnectionStringUni, MyServerEnumeratorUni;
{$ENDIF}

var
  MyMacros: TStrValueStringList;

{ TMySQLUniProvider }

class function TMySQLUniProvider.GetProviderName: string;
begin
  Result := 'MySQL';
end;

class function TMySQLUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TMyConnectionStringBuilder;
end;

function TMySQLUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TMySQLUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TMySQLUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TMyUniParser;
end;

function TMySQLUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMyConnectionParameters;
end;

function TMySQLUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMyConnectionPoolManager;
end;

function TMySQLUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMySQLConnection;
end;

function TMySQLUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TMyServerEnumerator;
end;

function TMySQLUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomMyDataSetService;
end;

function TMySQLUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMyScriptProcessor;
end;

{$IFNDEF STD}

function TMySQLUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomMyDumpProcessor;
end;

{$ENDIF}

function TMySQLUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMyConnectDialogService;
end;

function TMySQLUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomMyFieldTypeMap;
end;

function TMySQLUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMySqlFormatter;
end;

function TMySQLUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TMyConverterManager;
end;

procedure TMySQLUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TMySQLConnection then begin
    TMySQLConnection(Obj).SetProp(prCheckPrecision, True);
  end
  else
  if Obj is TCustomMyDataSetService then begin
    TCustomMyDataSetService(Obj).SetProp(prAutoIncrementReadOnly, False);
  end;

  inherited;
end;

procedure TMySQLUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TMySQLConnection, TMyConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TMySQLConnection, TMyConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('OptimizedBigint', prOptimizedBigint, [TMySQLConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('NullForZeroDelphiDate', prNullForZeroDelphiDate, [TMySQLConnection], False));

    FConnectionOptions.Add(TBooleanOption.Create('Compress', prCompress, [TMySQLConnection, TMyConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('Protocol', prProtocol, [TMySQLConnection, TMyConnectionParameters], Variant(mpDefault), TypeInfo(TMyProtocol)));
    FConnectionOptions.Add(TBooleanOption.Create('Embedded', prEmbedded, [TMySQLConnection, TMyConnectionParameters, TMyConnectDialogService], False));
    FConnectionOptions.Add(TStringOption.Create('EmbeddedParams', prEmbParams, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Interactive', prInteractive, [TMySQLConnection, TMyConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TMySQLConnection, TMyConnectionParameters], Variant(ivIPv4), TypeInfo(TIPVersion)));

    // SSL
    FConnectionOptions.Add(TStringOption.Create('SSLCipherList', prSSLCipher, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCACert', prSSLCA, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLKey', prSSLKey, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCert', prSSLCert, [TMySQLConnection, TMyConnectionParameters], ''));

    // HTTP
    FConnectionOptions.Add(TStringOption.Create('HttpUrl', prHttpUrl, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpUsername', prHttpUsername, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpPassword', prHttpPassword, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyHostname', prProxyHostname, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TMySQLConnection, TMyConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUsername', prProxyUsername, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TMySQLConnection, TMyConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('HttpTrustServerCertificate', prHttpTrustServerCertificate, [TMySQLConnection, TMyConnectionParameters], False));
  end;
end;

procedure TMySQLUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TMySQLCommand], 0));
  end;
end;

procedure TMySQLUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TMySQLRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TMySQLRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('NullForZeroDate', prNullForZeroDate, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CheckRowVersion', prCheckRowVersion, [TCustomMyDataSetService], False));
    FDataSetOptions.Add(TBooleanOption.Create('EnableBoolean', prEnableBoolean, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CreateConnection', prCreateConnection, [TMySQLRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('BinaryAsString', prBinaryAsString, [TMySQLRecordSet], True));
  end;
end;

procedure TMySQLUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TMySQLUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('LockTable', prLock, [TMySQLLoader], False));
    FLoaderOptions.Add(TBooleanOption.Create('Delayed', prDelayed, [TMySQLLoader], False));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerQuery', prRowsPerQuery, [TMySQLLoader], 0));
    FLoaderOptions.Add(TEnumeratorOption.Create('DuplicateKeys', prDuplicateKeys, [TMySQLLoader], Variant(_dkNone), TypeInfo(_TMyDuplicateKeys)));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TMySQLLoader], False));
  end;
end;

procedure TMySQLUniProvider.CreateDumpOptions;
begin
  if FDumpOptions = nil then begin
    FDumpOptions := TOptionsList.Create(GetProviderName);
    FDumpOptions.Add(TBooleanOption.Create('AddLock', prAddLock, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('DisableKeys', prDisableKeys, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('HexBlob', prHexBlob, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('UseExtSyntax', prUseExtSyntax, [TCustomMyDumpProcessor], True));
    FDumpOptions.Add(TBooleanOption.Create('UseDelayedIns', prUseDelayedIns, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TIntegerOption.Create('CommitBatchSize', prCommitBatchSize, [TCustomMyDumpProcessor], 0));
    FDumpOptions.Add(TEnumeratorOption.Create('InsertType', prInsertType, [TCustomMyDumpProcessor], Variant(_itInsert), TypeInfo(_TMyInsertType)));
    FDumpOptions.Add(TBooleanOption.Create('BackupTables', prBackupTables, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('BackupViews', prBackupViews, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('BackupData', prBackupData, [TCustomMyDumpProcessor], True));
    FDumpOptions.Add(TBooleanOption.Create('BackupTriggers', prBackupTriggers, [TCustomMyDumpProcessor], False));
    FDumpOptions.Add(TBooleanOption.Create('BackupStoredProcs', prBackupStoredProcs, [TCustomMyDumpProcessor], False));
  end;
end;
{$ENDIF}

{ TMyConnectDialogService }

function TMyConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prEmbedded:
      FEmbedded := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TMyConnectDialogService.ServerEnabled: boolean;
begin
  Result := not FEmbedded;
end;

function TMyConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := not FEmbedded;
end;

function TMyConnectDialogService.PortEnabled: boolean;
begin
  Result := not FEmbedded;
end;

{ TMySqlFormatter }

constructor TMySqlFormatter.Create;
begin
  inherited;

  FFunctions := MyFunctions;
  FPredefinedMacros := MyMacros;
end;

function TMySqlFormatter.LeftQuote: Char;
begin
  Result := '`';
end;

function TMySqlFormatter.RightQuote: Char;
begin
  Result := '`';
end;

function TMySqlFormatter.GetFunction(const FunctionName: string; const Params: TStringArray): string;
begin
  if UpperCase(FunctionName) = 'DATEDIFF' then
    Result := GetDateDiff(Params)
  else
    Result := inherited GetFunction(FunctionName, Params);
end;

function TMySqlFormatter.GetDateDiff(const Params: TStringArray): string;
const
  subFmt = 'PERIOD_DIFF(EXTRACT(YEAR_MONTH FROM %1:s), EXTRACT(YEAR_MONTH FROM %0:s))';
  secFmt = '(TIME_TO_SEC(%1:s) - TIME_TO_SEC(%0:s))';
var
  datepart, startDate, endDate: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEDIFF']);
  datepart := LowerCase(Params[0]);
  startDate := Params[1];
  endDate := Params[2];
  if datepart = 'year' then
    Result := Format('FLOOR((' + subFmt + ')/12)', [startDate, endDate])
  else
  if datepart = 'month' then
    Result := Format(subFmt, [startDate, endDate])
  else
  if datepart = 'day' then
    Result := 'TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + ')'
  else
  if datepart = 'hour' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*24 + ' +
      'TRUNCATE(' + Format(secFmt, [startDate, endDate]) + '/3600, 0)'
  else
  if datepart = 'minute' then
    Result :=  '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*1440 + ' +
      ' TRUNCATE(' + Format(secFmt, [startDate, endDate]) + '/60, 0)'
  else
  if datepart = 'second' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*86400 + ' +
      Format(secFmt, [startDate, endDate])
  else
  if datepart = 'millisecond' then
    Result := '(TO_DAYS(' + endDate + ')' + ' - ' + ' TO_DAYS(' + startDate + '))*86400000 + ' +
      Format(secFmt, [startDate, endDate]) + '*1000)'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

function GetConnectionThreadID(Connection: TCustomDAConnection): integer;
begin
  TDBAccessUtils.InternalConnect(Connection);
  try
    Result := (TDBAccessUtils.GetIConnection(Connection) as TMySQLConnection).GetThreadId;
  finally
    TDBAccessUtils.InternalDisconnect(Connection);
  end;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TMySQLUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TMySQLUniProvider);

  MyFunctions := TStrValueStringList.Create;
  MyFunctions.Add('USER', 'USER()');
  MyFunctions.Add('CHAR_LENGTH', 'CHAR_LENGTH(%s)');
  MyFunctions.Add('LOCATE', 'LOCATE(%s, %s)');
  MyFunctions.Add('SUBSTRING', 'SUBSTRING(%s, %s, %s)');
  MyFunctions.Add('CONCAT', 'CONCAT(%s, %s)');
  MyFunctions.Add('CHAR', 'CHAR(%s)');
  MyFunctions.Add('TRIM', 'TRIM(%s)');
  MyFunctions.Add('TRUNCATE', 'TRUNCATE(%s, %s)');
  MyFunctions.Add('CEILING', 'CEILING(%s)');
  MyFunctions.Add('UPPER',   'UPPER(%s)');
  MyFunctions.Add('LOWER',   'LOWER(%s)');
  // Date-time
  MyFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  MyFunctions.Add('YEAR', 'YEAR(%s)');
  MyFunctions.Add('MONTH', 'MONTH(%s)');
  MyFunctions.Add('DAY', 'EXTRACT(DAY FROM %s)');
  MyFunctions.Add('DATEADD', '(%2:s + INTERVAL ''%1:s'' %0:s)');
  // Date-time literals
  MyFunctions.Add('__DATE_TIME_LITERAL', 'CAST(%s AS DATETIME)');
  MyFunctions.Add('__DATE_LITERAL', 'CAST(%s AS DATE)');
  MyFunctions.Add('__TIME_LITERAL', 'CAST(%s AS TIME)');
  // CONVERT functions
  MyFunctions.Add('TODATE', 'CAST(%s AS DATETIME)');
  MyFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  MyFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  MyMacros := TStrValueStringList.Create;
  MyMacros.Add('PROVIDER', 'MySQL');
  MyMacros.Add('MYSQL', '');
  // DataType macros
  MyMacros.Add('DATETIME', 'DATETIME');
  MyMacros.Add('DOUBLE', 'DOUBLE');
  MyMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TMySQLUniProvider);

  MyFunctions.Free;
  MyMacros.Free;

end.

