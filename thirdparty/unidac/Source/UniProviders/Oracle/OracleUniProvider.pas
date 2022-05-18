
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Odac.inc}
{$IFDEF VER10P}
{$I OracleUniProvider.inc}
{$ENDIF}
unit OracleUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
  CRAccess, CRConnectionPool, CRTypes, CRParser, CRDataTypeMap, MemData,
  DBAccess, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  CRConnectionString, CRServerEnumerator,
  UniProvider,
{$IFNDEF UNIDACPRO}
  OraClasses, OraScriptProcessor;
{$ELSE}
  OraClassesUni, OraScriptProcessorUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TOracleUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}
    procedure CreateTransactionOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function NeedRecreateProcCall: boolean; override;
    function NeedComplexUpdateFieldDefList: boolean; override;
    function NeedBlobUnicode(Param: TDAParam): boolean; override;

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
  end;

  TOraConnectDialogService = class (TConnectDialogService)
  private
    FDirect: boolean;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetConnectMode: integer; override;
  end;

  TOraSqlFormatter = class(TUniSqlFormatter)
  protected
    function GetFunction(const FunctionName: string; const Params: TStringArray): string; override;
    function GetDateAdd(const Params: TStringArray): string;
    function GetDateDiff(const Params: TStringArray): string;
  public
    constructor Create; override;
  end;

  TUniOraScriptProcessor = class (TCustomOraScriptProcessor)
  protected
    procedure SetConnectMode(Connection: TCustomDAConnection; ConnectMode: TConnectMode); override;
  end;

  TOraUtils = class
  private
    class function GetConnection(Connection: TCustomDAConnection): TOCIConnection;
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
  TypInfo, CRProps, CRFunctions, CRVio, DAConsts, UniConsts,
{$IFNDEF UNIDACPRO}
  OraProps, OraCall, OraParser, OraConnectionPool, OraServices, OraSQLGenerator,
  OraNumber, OraDateTime, OraInterval, OraDataTypeMap, OraConnectionString, OraServerEnumerator;
{$ELSE}
  OraPropsUni, OraCallUni, OraParserUni, OraConnectionPoolUni, OraServicesUni, OraSQLGeneratorUni,
  OraNumberUni, OraDateTimeUni, OraIntervalUni, OraDataTypeMapUni, OraConnectionStringUni, OraServerEnumeratorUni;
{$ENDIF}

var
  OraFunctions, OraMacros: TStrValueStringList;

{ TOracleUniProvider }

class function TOracleUniProvider.GetProviderName: string;
begin
  Result := 'Oracle';
end;

class function TOracleUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TOraConnectionStringBuilder;
end;

function TOracleUniProvider.NeedRecreateProcCall: boolean;
begin
  Result := True;
end;

function TOracleUniProvider.NeedComplexUpdateFieldDefList: boolean;
begin
  Result := True;
end;

function TOracleUniProvider.NeedBlobUnicode(Param: TDAParam): boolean;
var
  IConnection: TCRConnection;
  ICommand: TCRCommand;
  OCISvcCtx: TOCISvcCtx;
begin
  if Param.DataType = ftOraClob then begin
    Result := False;
    ICommand := TDBAccessUtils.GetICommand(Param.Collection.Owner as TComponent);
    if ICommand <> nil then begin
      OCISvcCtx := TOCICommand(ICommand).OCISvcCtx;
      if OCISvcCtx = nil then begin
        IConnection := ICommand.GetConnection;
        if IConnection <> nil then
          OCISvcCtx := TOCIConnection(ICommand.GetConnection).OCISvcCtx
      end;
      if OCISvcCtx <> nil then
        Result := OCISvcCtx.UnicodeEnv;
    end;
  end
  else
    Result := inherited NeedBlobUnicode(Param);
end;

function TOracleUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TOraParser;
end;

function TOracleUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TOraConnectionParameters;
end;

function TOracleUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TOraConnectionPoolManager;
end;

function TOracleUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TOCIConnection;
end;

function TOracleUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TOraServerEnumerator;
end;

function TOracleUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomOraDataSetService;
end;

function TOracleUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TUniOraScriptProcessor;
end;

{$IFNDEF STD}

function TOracleUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TOCIAlerter;
end;

function TOracleUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomOraDumpProcessor;
end;

{$ENDIF}

function TOracleUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomOraFieldTypeMap;
end;

function TOracleUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TOraConnectDialogService;
end;

function TOracleUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TOraSqlFormatter;
end;

function TOracleUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TOraConverterManager;
end;

function TOracleUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  case Param.DataType of
    ftOraClob, ftOraBlob:
      Result := TOraLob;
    ftCursor:
      Result := TOraCursor;
  else
    Result := inherited GetParamObjectClass(Param);
  end;
end;

function TOracleUniProvider.CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject;
begin
  case Param.DataType of
    ftOraClob, ftOraBlob: begin
      Result := TOraLob.Create(nil);
      if Param.DataType = ftOraClob then begin
        if TDBAccessUtils.GetNational(Param) then
          TOraLob(Result).LobType := ltNClob
        else
          TOraLob(Result).LobType := ltClob;
        TOraLob(Result).IsUnicode := IsUnicode;
      end
      else
        TOraLob(Result).LobType := ltBlob;
    end;
    ftCursor:
      Result := TOraCursor.Create;
  else
    Result := inherited CreateParamObject(Param, IsUnicode);
  end;
end;

procedure TOracleUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj.ClassType = TOCIConnection then begin
  {$IFNDEF FPC}
    TOCIConnection(Obj).SetProp(prEnableSQLTimeStamp, True);
  {$ELSE}
    TOCIConnection(Obj).SetProp(prTimeStampAsString, True);
  {$ENDIF}
    TOCIConnection(Obj).SetProp(prIntervalAsString, True);
  end
  else
  if Obj.ClassType = TOCICommand then begin
    TOCICommand(Obj).SetProp(prCheckParamHasDefault, False);
  end;

  inherited;
end;

procedure TOracleUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('CharLength', prCharLength, [TOCIConnection, TOraConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UnicodeEnvironment', prUnicodeEnvironment, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseOCI7', prUseOCI7, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('OptimizerMode', prOptimizerMode, [TOCIConnection, TOraConnectionParameters], Variant(omDefault), TypeInfo(TOptimizerMode)));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeOut, [TOCIConnection, TOraConnectionParameters], 0));
    FConnectionOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCIConnection, TOraConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('StatementCacheSize', prStatementCacheSize, [TOCIConnection, TOraConnectionParameters], 20));
    FConnectionOptions.Add(TStringOption.Create('ClientIdentifier', prClientIdentifier, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prDirect, [TOCIConnection, TOraConnectionParameters, TOraServerEnumerator, TOraConnectDialogService], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TOCIConnection, TOraConnectionParameters], DefValIPVersion, TypeInfo(TIPVersion)));
    FConnectionOptions.Add(TEnumeratorOption.Create('ConnectMode', prConnectMode, [TOCIConnection, TOraConnectionParameters], Variant(cmNormal), TypeInfo(TConnectMode)));
    FConnectionOptions.Add(TStringOption.Create('HomeName', prHomeName, [TOCIConnection, TOraConnectionParameters, TOraServerEnumerator], ''));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('PoolingType', prPoolingType, [TOraConnectionParameters], Variant(optLocal), TypeInfo(TOraPoolingType)));
    FConnectionOptions.Add(TStringOption.Create('DateFormat', prDateFormat, [TOCIConnection], ''));
    FConnectionOptions.Add(TStringOption.Create('DateLanguage', prDateLanguage, [TOCIConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('EnableIntegers', prEnableIntegers, [TOCIConnection], True));
    FConnectionOptions.Add(TBooleanOption.Create('EnableLargeint', prEnableLargeint, [TOCIConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionSmallint', prSmallintPrecision, [TOCIConnection], 4));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionInteger', prIntegerPrecision, [TOCIConnection], 9));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionLargeint', prLargeintPrecision, [TOCIConnection], 18));
    FConnectionOptions.Add(TIntegerOption.Create('PrecisionFloat', prFloatPrecision, [TOCIConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('PrecisionBCD', prBCDPrecision, [TOCIConnection], '14,4'));
    FConnectionOptions.Add(TStringOption.Create('PrecisionFMTBCD', prFMTBCDPrecision, [TOCIConnection], '39,39'));
    FConnectionOptions.Add(TBooleanOption.Create('ThreadSafety', prThreadSafety, [TOCIConnection], True));

    // SSL
    FConnectionOptions.Add(TStringOption.Create('SSLCipherList', prSSLCipher, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCACert', prSSLCA, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLKey', prSSLKey, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLCert', prSSLCert, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLServerCertDN', prSSLServerCertDN, [TOCIConnection, TOraConnectionParameters], ''));

    // HTTP
    FConnectionOptions.Add(TStringOption.Create('HttpUrl', prHttpUrl, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpUsername', prHttpUsername, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('HttpPassword', prHttpPassword, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyHostname', prProxyHostname, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TOCIConnection, TOraConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUsername', prProxyUsername, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TOCIConnection, TOraConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('HttpTrustServerCertificate', prHttpTrustServerCertificate, [TOCIConnection, TOraConnectionParameters], False));
  end;
end;

procedure TOracleUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := ToptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TOCICommand], 0));
    FSQLOptions.Add(TBooleanOption.Create('NonBlocking', prNonBlocking, [TOCICommand], False));
    FSQLOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCICommand], False));
    FSQLOptions.Add(TBooleanOption.Create('TemporaryLobUpdate', prTemporaryLobUpdate, [TOCICommand], True));
  end;
end;

procedure TOracleUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('AutoClose', prAutoClose, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CacheLobs', prCacheLobs, [TOCICommand], True));
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeOut, [TOCIRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredLobRead', prDeferredLobRead, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TOCIRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('HideRowId', prHideRowId, [TOCIRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TOCICommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TOCIRecordSet], False));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomOraDataSetService], ''));
    FDataSetOptions.Add(TBooleanOption.Create('NonBlocking', prNonBlocking, [TOCICommand], False));
    FDataSetOptions.Add(TIntegerOption.Create('PrefetchLobSize', prPrefetchLobSize, [TOCICommand], 0));
    FDataSetOptions.Add(TIntegerOption.Create('PrefetchRows', prPrefetchRows, [TOCICommand], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ProcNamedParams', prProcNamedParams, [TOCICommand], False));
  {$IFNDEF FPC}
    FDataSetOptions.Add(TBooleanOption.Create('RawAsString', prRawAsString, [TOCICommand], False));
  {$ENDIF}
    FDataSetOptions.Add(TBooleanOption.Create('ScrollableCursor', prScrollableCursor, [TOCICommand], False));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomOraDataSetService], Variant(_smPost), TypeInfo(_TSequenceMode)));
    FDataSetOptions.Add(TBooleanOption.Create('StatementCache', prStatementCache, [TOCICommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('TemporaryLobUpdate', prTemporaryLobUpdate, [TOCICommand], True));
  end;
end;

procedure TOracleUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TOracleUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TBooleanOption.Create('DirectPath', prDirectPath, [TOCILoader], True));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TOCILoader], False));
  end;
end;
{$ENDIF}

procedure TOracleUniProvider.CreateTransactionOptions;
begin
  if FTransactionOptions = nil then begin
    FTransactionOptions := TOptionsList.Create(GetProviderName);
    FTransactionOptions.Add(TStringOption.Create('TransactionName', prTransactionName, [TOCITransaction], ''));
  end;
end;

{ TOraConnectDialogService }

function TOraConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirect:
      FDirect := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TOraConnectDialogService.GetConnectMode: integer;
begin
  if FDirect then
    Result := 2
  else
    Result := 1;
end;

{ TOraSqlFormatter }

constructor TOraSqlFormatter.Create;
begin
  inherited;

  FFunctions := OraFunctions;
  FPredefinedMacros := OraMacros;
end;

function TOraSqlFormatter.GetFunction(const FunctionName: string; const Params: TStringArray): string;
begin
  if UpperCase(FunctionName) = 'DATEADD' then
    Result := GetDateAdd(Params)
  else
  if UpperCase(FunctionName) = 'DATEDIFF' then
    Result := GetDateDiff(Params)
  else
    Result := inherited GetFunction(FunctionName, Params);
end;

function TOraSqlFormatter.GetDateAdd(const Params: TStringArray): string;
var
  datepart, number, date: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEADD']);
  datepart := LowerCase(Params[0]);
  number := Params[1];
  date := Params[2];
  if datepart = 'year' then
    Result := ' ADD_MONTHS(' + date + ', (' + number + ')*12)'
  else
  if datepart = 'month' then
    Result := ' ADD_MONTHS(' + date + ', ' + number + ')'
  else
  if datepart = 'day' then
    Result := date + ' + ' + number
  else
  if datepart = 'hour' then
    Result := date + ' + (' + number + ')/24'
  else
  if datepart = 'minute' then
    Result := date + ' + (' + number + ')/1440'
  else
  if datepart = 'second' then
    Result := date + ' + (' + number + ')/86400'
  else
  if datepart = 'millisecond' then
    Result := date + ' + (' + number + ')/86400000'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

function TOraSqlFormatter.GetDateDiff(const Params: TStringArray): string;
const
  monthFmt = '(EXTRACT(YEAR FROM %1:s) - EXTRACT(YEAR FROM %0:s))*12 + EXTRACT(MONTH FROM %1:s) - EXTRACT(MONTH FROM %0:s)';
var
  datepart, startDate, endDate: string;
begin
  if Length(Params) <> 3 then
    raise Exception.CreateFmt(SWrongArgCnt, ['DATEDIFF']);
  datepart := LowerCase(Params[0]);
  startDate := Params[1];
  endDate := Params[2];
  if datepart = 'year' then
    Result := Format('FLOOR((' + monthFmt + ')/12)', [startDate, endDate])
  else
  if datepart = 'month' then
    Result := Format(monthFmt, [startDate, endDate])
  else
  if datepart = 'day' then
    Result := 'FLOOR(' + endDate + ' - ' + startDate + ')'
  else
  if datepart = 'hour' then
    Result := '(' + endDate + ' - ' + startDate + ')*24'
  else
  if datepart = 'minute' then
    Result := '(' + endDate + ' - ' + startDate + ')*1440'
  else
  if datepart = 'second' then
    Result := '(' + endDate + ' - ' + startDate + ')*86400'
  else
  if datepart = 'millisecond' then
    Result := '(' + endDate + ' - ' + startDate + ')*86400000'
  else
    raise Exception.CreateFmt(SUnknownDatepart, [Datepart]);
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TOracleUniProvider]);
end;

{ TUniOraScriptProcessor }

procedure TUniOraScriptProcessor.SetConnectMode(Connection: TCustomDAConnection;
  ConnectMode: TConnectMode);
begin
  SetSpecificOption(Connection, 'Oracle.ConnectMode', GetEnumName(TypeInfo (TConnectMode), Integer(ConnectMode)));
end;

{ TOraUtils }

class function TOraUtils.GetConnection(Connection: TCustomDAConnection): TOCIConnection;
var
  CRConnection: TCRConnection;
begin
  CRConnection := TDBAccessUtils.GetIConnection(Connection);

  if CRConnection = nil then
    raise Exception.Create(SConnectionIsClosed)
  else if not (CRConnection is TOCIConnection) then
    raise Exception.CreateFmt(SIncorrectConnectionType, ['TOCIConnection', CRConnection.ClassName]);

  Result := TOCIConnection(CRConnection);
end;

class procedure TOraUtils.ChangePassword(Connection: TCustomDAConnection; NewPassword: string);
begin
  GetConnection(Connection).ChangePassword(NewPassword);
end;

initialization
  UniProviders.RegisterProvider(TOracleUniProvider);

  OraFunctions := TStrValueStringList.Create;
  OraFunctions.Add('USER',        'USER');
  OraFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  OraFunctions.Add('LOCATE',      'INSTR(%1:s, %0:s)');
  OraFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  OraFunctions.Add('CONCAT',      '%s || %s');
  OraFunctions.Add('CHAR',        'CHR(%s)');
  OraFunctions.Add('TRIM',        'TRIM(%s)');
  OraFunctions.Add('TRUNCATE',    'TRUNC(%s, %s)');
  OraFunctions.Add('CEILING',     'CEIL(%s)');
  OraFunctions.Add('UPPER',       'UPPER(%s)');
  OraFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  OraFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  OraFunctions.Add('YEAR',         'EXTRACT(YEAR FROM %s)');
  OraFunctions.Add('MONTH',        'EXTRACT(MONTH FROM %s)');
  OraFunctions.Add('DAY',          'EXTRACT(DAY FROM %s)');
  // Date-time literals
  OraFunctions.Add('__DATE_TIME_LITERAL', 'TO_DATE(%s, ''yyyy-mm-dd hh24:mi:ss'')');
  OraFunctions.Add('__DATE_LITERAL',      'TO_DATE(%s, ''yyyy-mm-dd'')');
  OraFunctions.Add('__TIME_LITERAL',      'TO_DATE(%s, ''hh24:mi:ss'')');
  // CONVERT functions
  OraFunctions.Add('TODATE',   'TO_DATE(%s, ''yyyy-mm-dd hh:mi:ss'')');
  OraFunctions.Add('TONUMBER', 'TO_NUMBER(%s)');
  OraFunctions.Add('TOCHAR',   'TO_CHAR(%s)');

  OraMacros := TStrValueStringList.Create;
  OraMacros.Add('PROVIDER', 'Oracle');
  OraMacros.Add('ORACLE',   '');
  // DataType macros
  OraMacros.Add('DATETIME', 'DATE');
  OraMacros.Add('DOUBLE',   'NUMBER');
  OraMacros.Add('VARCHAR',  'VARCHAR2');

finalization
  UniProviders.UnRegisterProvider(TOracleUniProvider);

  OraFunctions.Free;
  OraMacros.Free;

end.
