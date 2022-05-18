
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SugarDac.inc}
{$IFDEF VER10P}
{$I SugarCRMUniProvider.inc}
{$ENDIF}
unit SugarCRMUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, CRConnectionString,
  MemData, CRTypes, CRParser, CRDataTypeMap, CRServerEnumerator, DAScript,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, UniProvider,
  ODBCUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TSugarCRMUniProvider = class(TODBCUniProvider)
  protected
    procedure CreateConnectionOptions; override;
  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function GetConnectionClass: TCRConnectionClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
  end;

  TSugarConnectDialogService = class(TConnectDialogService)
  public
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TSugarFormatter = class(TUniSqlFormatter)
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
  DAConsts, CRProps, UniConsts,
{$IFNDEF UNIDACPRO}
  ODBCProps,
  SugarProps, SugarClasses,
  SugarConnectionPool, SugarConnectionString;
{$ELSE}
  ODBCPropsUni,
  SugarPropsUni, SugarClassesUni,
  SugarConnectionPoolUni, SugarConnectionStringUni;
{$ENDIF}

var
  SugarFunctions, SugarMacros: TStrValueStringList;

class function TSugarCRMUniProvider.GetProviderName: string;
begin
  Result := 'SugarCRM';
end;

class function TSugarCRMUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TSugarConnectionStringBuilder;
end;

function TSugarCRMUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TSugarConnection;
end;

function TSugarCRMUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TSugarConnectionParameters;
end;

function TSugarCRMUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TSugarConnectionPoolManager;
end;

function TSugarCRMUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TSugarConnectDialogService;
end;

function TSugarCRMUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TSugarCRMUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TSugarFormatter;
end;

procedure TSugarCRMUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TSugarConnection, TSugarConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TSugarConnection, TSugarConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TSugarConnection, TSugarConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TSugarConnection, TSugarConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TSugarConnection, TSugarConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TSugarConnection, TSugarConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TSugarConnection, TSugarConnectionParameters], ''));
  end;
end;

{ TSugarConnectDialogService }

function TSugarConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TSugarConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TSugarFormatter }

constructor TSugarFormatter.Create;
begin
  inherited;

  FFunctions := SugarFunctions;
  FPredefinedMacros := SugarMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TSugarCRMUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSugarCRMUniProvider);

  SugarFunctions := TStrValueStringList.Create;
  SugarFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  SugarFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  SugarFunctions.Add('CONCAT',      '%s || %s');
  SugarFunctions.Add('TRIM',        'TRIM(%s)');
  SugarFunctions.Add('UPPER',       'UPPER(%s)');
  SugarFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  SugarFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  SugarFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  SugarFunctions.Add('__DATE_TIME_LITERAL', '%s');
  SugarFunctions.Add('__DATE_LITERAL',      '%s');
  SugarFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  SugarFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  SugarFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  SugarFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  SugarMacros := TStrValueStringList.Create;
  SugarMacros.Add('PROVIDER', 'SugarCRM');
  SugarMacros.Add('SUGARCRM',   '');
  // DataType macros
  SugarMacros.Add('DATETIME', 'TEXT');
  SugarMacros.Add('DOUBLE',   'DOUBLE');
  SugarMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TSugarCRMUniProvider);

  SugarFunctions.Free;
  SugarMacros.Free;

end.
