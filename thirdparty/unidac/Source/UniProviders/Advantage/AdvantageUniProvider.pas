
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ADSDac.inc}
{$IFDEF VER10P}
{$I AdvantageUniProvider.inc}
{$ENDIF}
unit AdvantageUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, CRConnectionString,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  CRTypes, CRParser, CRDataTypeMap,
  DAScript, UniProvider, ODBCUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TAdvantageUniProvider = class(TODBCUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsServerSupported: boolean; override;
    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;

    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TADSConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TADSFormatter = class(TUniSqlFormatter)
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
  ODBCProps, ODBCClasses, ODBCParser, ODBCServices,
  ADSProps, ADSClasses, ADSServices, ADSConnectionPool, ADSDataTypeMap, ADSConnectionString;
{$ELSE}
  ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni,
  ADSPropsUni, ADSClassesUni, ADSServicesUni, ADSConnectionPoolUni, ADSDataTypeMapUni, ADSConnectionStringUni;
{$ENDIF}

var
  ADSFunctions, ADSMacros: TStrValueStringList;

class function TAdvantageUniProvider.GetProviderName: string;
begin
  Result := 'Advantage';
end;

class function TAdvantageUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TAdvantageConnectionStringBuilder;
end;

function TAdvantageUniProvider.IsServerSupported: boolean;
begin
  Result := False;
end;

function TAdvantageUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TAdvantageUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TAdvantageUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TAdvantageUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TAdvantageUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TADSConnectionParameters;
end;

function TAdvantageUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TADSConnectionPoolManager;
end;

function TAdvantageUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TADSConnection;
end;

function TAdvantageUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomADSDataSetService;
end;

function TAdvantageUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TADSScriptProcessor;
end;

function TAdvantageUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TADSConnectDialogService;
end;

function TAdvantageUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TADSFormatter;
end;

function TAdvantageUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TADSConverterManager;
end;

procedure TAdvantageUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TODBCConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TADSConnection, TADSConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('ServerTypes', prADSServerTypes, [TADSConnection, TADSConnectionParameters], 'ADS,AIS'));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TADSConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('DefaultType', prADSDefaultType, [TADSConnection], Variant(dtAdvantage), TypeInfo(TADSDefaultType)));
  end;
end;

procedure TAdvantageUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TADSCommand], 0));
  end;
end;

procedure TAdvantageUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TADSRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TADSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TADSRecordSet], False));
  end;
end;

{ TADSConnectDialogService }

function TADSConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TADSConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TADSFormatter }

constructor TADSFormatter.Create;
begin
  inherited;

  FFunctions := ADSFunctions;
  FPredefinedMacros := ADSMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TAdvantageUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TAdvantageUniProvider);

  ADSFunctions := TStrValueStringList.Create;
  ADSFunctions.Add('USER', 'USER');
  ADSFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  ADSFunctions.Add('LOCATE', 'POSSTR(%1:s, %0:s)');
  ADSFunctions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  ADSFunctions.Add('CONCAT', '%s || %s');
  ADSFunctions.Add('CHAR', 'CHR(%s)');
  ADSFunctions.Add('TRIM', 'TRIM(%s)');
  ADSFunctions.Add('TRUNCATE', 'TRUNCATE(%s, %s)');
  ADSFunctions.Add('CEILING', 'CEILING(%s)');
  ADSFunctions.Add('UPPER',       'UPPER(%s)');
  ADSFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  ADSFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  ADSFunctions.Add('YEAR', 'YEAR(%s)');
  ADSFunctions.Add('MONTH', 'MONTH(%s)');
  ADSFunctions.Add('DAY', 'DAY(%s)');
  //ADSFunctions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //ADSFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  ADSFunctions.Add('__DATE_TIME_LITERAL', 'CAST(%s AS TIMESTAMP)');
  ADSFunctions.Add('__DATE_LITERAL', 'CAST(%s AS DATE)');
  ADSFunctions.Add('__TIME_LITERAL', 'CAST(%s AS TIME)');
  // CONVERT functions
  ADSFunctions.Add('TODATE', 'CAST(%s AS TIMESTAMP)');
  ADSFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  ADSFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  ADSMacros := TStrValueStringList.Create;
  ADSMacros.Add('PROVIDER', 'Advantage');
  ADSMacros.Add('ADVANTAGE', '');
  // DataType macros
  ADSMacros.Add('DATETIME', 'TIMESTAMP');
  ADSMacros.Add('DOUBLE', 'DOUBLE');
  ADSMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TAdvantageUniProvider);

  ADSFunctions.Free;
  ADSMacros.Free;

end.
