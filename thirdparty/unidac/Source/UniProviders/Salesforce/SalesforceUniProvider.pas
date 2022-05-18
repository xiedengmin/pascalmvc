
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SalesforceDac.inc}
{$IFDEF VER10P}
{$I SalesforceUniProvider.inc}
{$ENDIF}
unit SalesforceUniProvider;

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
  TSalesforceUniProvider = class(TODBCUniProvider)
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

  TSalesforceConnectDialogService = class(TConnectDialogService)
  public
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TSalesforceFormatter = class(TUniSqlFormatter)
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
  SalesforceProps, SalesforceClasses,
  SalesforceConnectionPool, SalesforceConnectionString;
{$ELSE}
  ODBCPropsUni,
  SalesforcePropsUni, SalesforceClassesUni,
  SalesforceConnectionPoolUni, SalesforceConnectionStringUni;
{$ENDIF}

var
  SalesforceFunctions, SalesforceMacros: TStrValueStringList;

class function TSalesforceUniProvider.GetProviderName: string;
begin
  Result := 'Salesforce';
end;

class function TSalesforceUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TSalesforceConnectionStringBuilder;
end;

function TSalesforceUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TSalesforceConnection;
end;

function TSalesforceUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TSalesforceConnectionParameters;
end;

function TSalesforceUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TSalesforceConnectionPoolManager;
end;

function TSalesforceUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TSalesforceConnectDialogService;
end;

function TSalesforceUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TSalesforceUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TSalesforceFormatter;
end;

procedure TSalesforceUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TSalesforceConnection, TSalesforceConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('IncludeDeleted', prIncludeDeleted, [TSalesforceConnection, TSalesforceConnectionParameters], False));
    FConnectionOptions.Add(TStringOption.Create('SecurityToken', prSecurityToken, [TSalesforceConnection, TSalesforceConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TSalesforceConnection, TSalesforceConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TSalesforceConnection, TSalesforceConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TSalesforceConnection, TSalesforceConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TSalesforceConnection, TSalesforceConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TSalesforceConnection, TSalesforceConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TSalesforceConnection, TSalesforceConnectionParameters], ''));
  end;
end;

{ TSalesforceConnectDialogService }

function TSalesforceConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TSalesforceConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TSalesforceFormatter }

constructor TSalesforceFormatter.Create;
begin
  inherited;

  FFunctions := SalesforceFunctions;
  FPredefinedMacros := SalesforceMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TSalesforceUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSalesforceUniProvider);

  SalesforceFunctions := TStrValueStringList.Create;
  SalesforceFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  SalesforceFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  SalesforceFunctions.Add('CONCAT',      '%s || %s');
  SalesforceFunctions.Add('TRIM',        'TRIM(%s)');
  SalesforceFunctions.Add('UPPER',       'UPPER(%s)');
  SalesforceFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  SalesforceFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  SalesforceFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  SalesforceFunctions.Add('__DATE_TIME_LITERAL', '%s');
  SalesforceFunctions.Add('__DATE_LITERAL',      '%s');
  SalesforceFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  SalesforceFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  SalesforceFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  SalesforceFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  SalesforceMacros := TStrValueStringList.Create;
  SalesforceMacros.Add('PROVIDER', 'Salesforce');
  SalesforceMacros.Add('SALESFORCE',   '');
  // DataType macros
  SalesforceMacros.Add('DATETIME', 'TEXT');
  SalesforceMacros.Add('DOUBLE',   'DOUBLE');
  SalesforceMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TSalesforceUniProvider);

  SalesforceFunctions.Free;
  SalesforceMacros.Free;

end.
