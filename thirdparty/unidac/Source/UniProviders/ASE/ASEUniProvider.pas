
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
{$IFDEF VER10P}
{$I ASEUniProvider.inc}
{$ENDIF}
unit ASEUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
  MemData, CRTypes, CRParser, CRDataTypeMap, CRAccess,
  CRConnectionPool, CRConnectionString, CRServerEnumerator,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, DAScript,
  {$IFNDEF STD}DADump,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ODBCUniProvider,{$ELSE}{$IFDEF MOBILE}ODBCUniProvider,{$ENDIF}{$ENDIF}
  UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TASEUniProvider = class({$IFDEF ODBC_PROVIDER}TODBCUniProvider{$ELSE}TUniProvider{$ENDIF})
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

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
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
  {$IFNDEF ODBC_PROVIDER}
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
  {$ENDIF}
  {$IFNDEF STD}
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TASEConnectDialogService = class(TConnectDialogService)
  public
  end;

  TASEFormatter = class(TUniSqlFormatter)
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
  DAConsts, UniConsts, CRProps, CRVio,
{$IFNDEF UNIDACPRO}
  {$IFDEF TDS}Tds5Classes,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ODBCProps, ODBCClasses, ODBCParser, ODBCServices,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClasses,{$ENDIF}
  ASEProps, ASEParser, ASEConnectionPool, ASEDataTypeMap, ASEConnectionString,
  ASEConnection, ASEServices, ASESQLGenerator, ASEServerEnumerator;
{$ELSE}
  {$IFDEF TDS}Tds5ClassesUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClassesUni,{$ENDIF}
  ASEPropsUni, ASEParserUni, ASEConnectionPoolUni, ASEDataTypeMapUni, ASEConnectionStringUni,
  ASEConnectionUni, ASEServicesUni, ASESQLGeneratorUni, ASEServerEnumeratorUni;
{$ENDIF}

var
  ASEFunctions, ASEMacros: TStrValueStringList;

class function TASEUniProvider.GetProviderName: string;
begin
  Result := 'ASE';
end;

class function TASEUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TASEConnectionStringBuilder;
end;

function TASEUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TASEUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

function TASEUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TASEConnectionParameters;
end;

function TASEUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TASEConnectionPoolManager;
end;

function TASEUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TASEConnection;
end;

function TASEUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TASEServerEnumerator;
end;

function TASEUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomASEDataSetService;
end;

function TASEUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TASEScriptProcessor;
end;

function TASEUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TASEConnectDialogService;
end;

{$IFNDEF ODBC_PROVIDER}

function TASEUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomASEFieldTypeMap;
end;

{$ENDIF}

{$IFNDEF STD}

function TASEUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomASEDumpProcessor;
end;

{$ENDIF}

function TASEUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TASEFormatter;
end;

function TASEUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TASEConverterManager;
end;

procedure TASEUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('AnsiNull', prAnsiNull, [TASEConnection], True));
    FConnectionOptions.Add(TStringOption.Create('ApplicationName', prApplicationName, [TASEConnection], ''));
    FConnectionOptions.Add(TStringOption.Create('CharSet', prCharSet, [TASEConnection], ''));
    FConnectionOptions.Add(TStringOption.Create('ClientHostName', prClientHostName, [TASEConnection], ''));
  {$IFDEF ODBC_PROVIDER}
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TODBCConnection], False));
  {$ENDIF}
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TASEConnection], 15));
  {$IFDEF ODBC_PROVIDER}
    FConnectionOptions.Add(TBooleanOption.Create('DetectFieldsOnPrepare', prDetectFieldsOnPrepare, [TASEConnection], True));
  {$ENDIF}
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prDirect, [TASEConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('EncryptPassword', prEncryptPassword, [TASEConnection], Variant(epDisable), TypeInfo(TASEEncryptPassword)));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TASEConnection], DefValIPVersion, TypeInfo(TIPVersion)));
    FConnectionOptions.Add(TBooleanOption.Create('MultipleConnections', prMultipleConnections, [TASEConnection], True));
    FConnectionOptions.Add(TEnumeratorOption.Create('PrepareMethod', prPrepareMethod, [TASEConnection], Variant(pmPartial), TypeInfo(TASEPrepareMethod)));
    FConnectionOptions.Add(TBooleanOption.Create('QuotedIdentifier', prQuotedIdentifier, [TASEConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('SelectMethod', prSelectMethod, [TASEConnection], Variant(smDirect), TypeInfo(TASESelectMethod)));
    FConnectionOptions.Add(TIntegerOption.Create('TextSize', prTextSize, [TASEConnection], 0));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TASEConnection], False));
  end;
end;

procedure TASEUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF ODBC_PROVIDER}TASECommand{$IFDEF TDS},{$ENDIF}{$ENDIF}{$IFDEF TDS}TTDS5RecordSet{$ENDIF}], 0));
  end;
end;

procedure TASEUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF ODBC_PROVIDER}TASERecordSet{$IFDEF TDS},{$ENDIF}{$ENDIF}{$IFDEF TDS}TTDS5RecordSet{$ENDIF}], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [{$IFDEF ODBC_PROVIDER}TASERecordSet{$IFDEF TDS},{$ENDIF}{$ENDIF}{$IFDEF TDS}TTDS5RecordSet{$ENDIF}], False));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [{$IFDEF ODBC_PROVIDER}TASERecordSet{$IFDEF TDS},{$ENDIF}{$ENDIF}{$IFDEF TDS}TTDS5RecordSet{$ENDIF}], False));
  end;
end;

{$IFNDEF STD}
procedure TASEUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerBatch', prRowsPerBatch, [{$IFDEF ODBC_PROVIDER}TASELoader{$IFDEF TDS},{$ENDIF}{$ENDIF}{$IFDEF TDS}TTDS5Loader{$ENDIF}], 0));
  end;
end;
{$ENDIF}

{ TASEFormatter }

constructor TASEFormatter.Create;
begin
  inherited;

  FFunctions := ASEFunctions;
  FPredefinedMacros := ASEMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TASEUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TASEUniProvider);

  ASEFunctions := TStrValueStringList.Create;
  ASEFunctions.Add('USER', 'USER');
  ASEFunctions.Add('CHAR_LENGTH', 'CHAR_LENGTH(%s)');
  ASEFunctions.Add('LOCATE', 'CHARINDEX(%s, %s)');
  ASEFunctions.Add('SUBSTRING', 'SUBSTRING(%s, %s, %s)');
  ASEFunctions.Add('CONCAT', '%s + %s');
  ASEFunctions.Add('CHAR', 'CHAR(%s)');
  ASEFunctions.Add('TRIM', 'LTRIM(RTRIM(%s))');
  ASEFunctions.Add('TRUNCATE', 'ROUND(%s, %s)');
  ASEFunctions.Add('CEILING', 'CEILING(%s)');
  ASEFunctions.Add('UPPER',       'UPPER(%s)');
  ASEFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  ASEFunctions.Add('CURRENT_DATE', 'GETDATE');
  ASEFunctions.Add('YEAR', 'YEAR(%s)');
  ASEFunctions.Add('MONTH', 'MONTH(%s)');
  ASEFunctions.Add('DAY', 'DAY(%s)');
  ASEFunctions.Add('DATEADD', 'DATEADD(%s, %s, %s)');
  ASEFunctions.Add('DATEDIFF', 'DATEDIFF(%s, %s, %s)');
  // Date-time literals
  ASEFunctions.Add('__DATE_TIME_LITERAL', 'CONVERT(DATETIME, %s)');
  ASEFunctions.Add('__DATE_LITERAL', 'CONVERT(DATETIME, %s)');
  ASEFunctions.Add('__TIME_LITERAL', 'CONVERT(DATETIME, %s)');
  // CONVERT functions
  ASEFunctions.Add('TODATE', 'CONVERT(DATETIME, %s)');
  ASEFunctions.Add('TONUMBER', 'CONVERT(FLOAT(53), %s)');
  ASEFunctions.Add('TOCHAR', 'CONVERT(VARCHAR, %s, 20)');

  ASEMacros := TStrValueStringList.Create;
  ASEMacros.Add('PROVIDER', 'ASE');
  ASEMacros.Add('ASE', '');
  // DataType macros
  ASEMacros.Add('DATETIME', 'DATETIME');
  ASEMacros.Add('DOUBLE', 'FLOAT(53)');
  ASEMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TASEUniProvider);

  ASEFunctions.Free;
  ASEMacros.Free;

end.
