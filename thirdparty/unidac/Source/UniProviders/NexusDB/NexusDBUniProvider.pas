//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I NexusDac.inc}
{$IFDEF VER10P}
{$I NexusDBUniProvider.inc}
{$ENDIF}
unit NexusDBUniProvider;
{$ENDIF}

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, MemData,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  CRTypes, CRParser, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  Uni, UniProvider;

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TNexusDBUniProvider = class(TUniProvider)
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
  {$IFNDEF STD}
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TNexusConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
  end;

  TNexusFormatter = class(TUniSqlFormatter)
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
  DAConsts, UniConsts, CRProps,
{$IFNDEF UNIDACPRO}
  NexusProps, NexusClasses, NexusParser, NexusServices, NexusConnectionPool,
  NexusDataTypeMap, NexusConnectionString;
{$ELSE}
  NexusPropsUni, NexusClassesUni, NexusParserUni, NexusServicesUni, NexusConnectionPoolUni,
  NexusDataTypeMapUni, NexusConnectionStringUni;
{$ENDIF}

{$IFDEF DUMMY}
const
  SNeedRecompileNexusDB = 'NexusDB provider should be recompiled.'#13#10 +
    'For more information read the "Using UniDAC with NexusDB" topic of the UniDAC help';
{$ENDIF}

var
  NexusFunctions, NexusMacros: TStrValueStringList;

class function TNexusDBUniProvider.GetProviderName: string;
begin
  Result := 'NexusDB';
end;

class function TNexusDBUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TNexusConnectionStringBuilder;
end;

function TNexusDBUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TNexusDBUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TNexusDBUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TNexusDBUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TNexusDBUniProvider.GetParserClass: TSQLParserClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusParser;
{$ELSE}
  Result := TSQLParser;
{$ENDIF}
end;

function TNexusDBUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusConnectionParameters;
{$ELSE}
  Result := TCRConnectionParameters;
{$ENDIF}
end;

function TNexusDBUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusConnectionPoolManager;
{$ELSE}
  Result := TCRConnectionPoolManager;
{$ENDIF}
end;

function TNexusDBUniProvider.GetConnectionClass: TCRConnectionClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusConnection;
{$ELSE}
  raise Exception.Create(SNeedRecompileNexusDB);
{$IFDEF FPC}
  Result := TCRConnection;
{$ENDIF}
{$ENDIF}
end;

function TNexusDBUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusServerEnumerator;
{$ELSE}
  Result := TCRServerEnumerator;
{$ENDIF}
end;

function TNexusDBUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
{$IFNDEF DUMMY}
  Result := TCustomNexusDataSetService;
{$ELSE}
  Result := TDADataSetService;
{$ENDIF}
end;

function TNexusDBUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
{$IFNDEF DUMMY}
  Result := TNexusScriptProcessor;
{$ELSE}
  Result := TDAScriptProcessor;
{$ENDIF}
end;

{$IFNDEF STD}

function TNexusDBUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
{$IFNDEF DUMMY}
  Result := TCustomNexusDumpProcessor;
{$ELSE}
  Result := TDADumpProcessor;
{$ENDIF}
end;

{$ENDIF}

function TNexusDBUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
{$IFNDEF DUMMY}
  Result := TCustomNexusFieldTypeMap;
{$ELSE}
  Result := TDAFieldTypeMap;
{$ENDIF}
end;

function TNexusDBUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TNexusConnectDialogService;
end;

function TNexusDBUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TNexusFormatter;
end;

function TNexusDBUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TNexusConverterManager;
end;

procedure TNexusDBUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
{$IFNDEF DUMMY}
  {$IFNDEF NEXUS_EMBEDDED}
    FConnectionOptions.Add(TEnumeratorOption.Create('Protocol', prProtocol, [TNexusConnection, TNexusConnectionParameters], Variant(npTCP), TypeInfo(TNexusProtocol)));
    FConnectionOptions.Add(TStringOption.Create('SecretKey', prSecretKey, [TNexusConnection, TNexusConnectionParameters], ''));
  {$ENDIF}
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TNexusConnection, TNexusConnectionParameters], 15));
    FConnectionOptions.Add(TIntegerOption.Create('HeartbeatInterval', prHeartbeatInterval, [TNexusConnection, TNexusConnectionParameters], 10));
    FConnectionOptions.Add(TIntegerOption.Create('LostConnectionTimeout', prLostConnectionTimeout, [TNexusConnection, TNexusConnectionParameters], 10));
    FConnectionOptions.Add(TIntegerOption.Create('WatchdogInterval', prWatchdogInterval, [TNexusConnection, TNexusConnectionParameters], 10));
    FConnectionOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TNexusConnection, TNexusConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('DatabaseReadOnly', prDatabaseReadOnly, [TNexusConnection, TNexusConnectionParameters], False));
{$ENDIF}
  end;
end;

procedure TNexusDBUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
  {$IFNDEF DUMMY}
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TNexusCommand], 15));
    FSQLOptions.Add(TBooleanOption.Create('ReadOnly', prCommandReadOnly, [TNexusCommand], False));
  {$ENDIF}
  end;
end;

procedure TNexusDBUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
  {$IFNDEF DUMMY}
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TNexusRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CursorUpdate', prCursorUpdate, [TNexusRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('ServerCursor', prServerCursor, [TNexusRecordSet], False));

    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TNexusCommand], 15));
    FDataSetOptions.Add(TBooleanOption.Create('ReadOnly', prCommandReadOnly, [TNexusCommand], False));
  {$ENDIF}
  end;
end;

{$IFNDEF STD}
procedure TNexusDBUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
  {$IFNDEF DUMMY}
    FLoaderOptions.Add(TBooleanOption.Create('DirectLoad', prDirectLoad, [TNexusLoader], True));
  {$ENDIF}
  end;
end;
{$ENDIF}

{ TNexusConnectDialogService }

function TNexusConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

{ TNexusFormatter }

constructor TNexusFormatter.Create;
begin
  inherited;

  FFunctions := NexusFunctions;
  FPredefinedMacros := NexusMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TNexusDBUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TNexusDBUniProvider);

  NexusFunctions := TStrValueStringList.Create;
  //NexusFunctions.Add('USER', '');
  NexusFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  //NexusFunctions.Add('LOCATE', 'STRPOS(%1:s, %0:s)');
  NexusFunctions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  NexusFunctions.Add('CONCAT', '%s || %s');
  //NexusFunctions.Add('CHAR', 'CHR(%s)');
  NexusFunctions.Add('TRIM', 'TRIM(%s)');
  //NexusFunctions.Add('TRUNCATE', 'TRUNC(%s, %s)');
  //NexusFunctions.Add('CEILING', 'CEILING(%s)');
  NexusFunctions.Add('UPPER',        'UPPER(%s)');
  NexusFunctions.Add('LOWER',        'LOWER(%s)');
  // Date-time
  NexusFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  //NexusFunctions.Add('YEAR', 'EXTRACT(YEAR FROM %s)');
  //NexusFunctions.Add('MONTH', 'EXTRACT(MONTH FROM %s)');
  //NexusFunctions.Add('DAY', 'EXTRACT(DAY FROM %s)');
  NexusFunctions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //NexusFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  NexusFunctions.Add('__DATE_TIME_LITERAL', 'TIMESTAMP%s');
  NexusFunctions.Add('__DATE_LITERAL', 'DATE%s');
  NexusFunctions.Add('__TIME_LITERAL', 'TIME%s');
  // CONVERT functions
  NexusFunctions.Add('TODATE', 'CAST(%s AS VARCHAR)');
  NexusFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  NexusFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  NexusMacros := TStrValueStringList.Create;
  NexusMacros.Add('PROVIDER', 'NexusDB');
  NexusMacros.Add('NexusDB', '');
  // DataType macros
  NexusMacros.Add('DATETIME', 'TEXT');
  NexusMacros.Add('DOUBLE', 'DOUBLE');
  NexusMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TNexusDBUniProvider);

  NexusFunctions.Free;
  NexusMacros.Free;

end.
