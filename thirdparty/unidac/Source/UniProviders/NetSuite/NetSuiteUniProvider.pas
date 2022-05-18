
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NetSuiteDac.inc}
{$IFDEF VER10P}
{$I NetSuiteUniProvider.inc}
{$ENDIF}
unit NetSuiteUniProvider;

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
  TNetSuiteUniProvider = class(TODBCUniProvider)
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

  TNetSuiteConnectDialogService = class(TConnectDialogService)
  public
    function ServerEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TNetSuiteFormatter = class(TUniSqlFormatter)
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
  NetSuiteConsts, NetSuiteProps, NetSuiteClasses,
  NetSuiteConnectionPool, NetSuiteConnectionString;
{$ELSE}
  ODBCPropsUni,
  NetSuiteConstsUni, NetSuitePropsUni, NetSuiteClassesUni,
  NetSuiteConnectionPoolUni, NetSuiteConnectionStringUni;
{$ENDIF}

var
  NetSuiteFunctions, NetSuiteMacros: TStrValueStringList;

class function TNetSuiteUniProvider.GetProviderName: string;
begin
  Result := 'NetSuite';
end;

class function TNetSuiteUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TNetSuiteConnectionStringBuilder;
end;

function TNetSuiteUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TNetSuiteConnection;
end;

function TNetSuiteUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TNetSuiteConnectionParameters;
end;

function TNetSuiteUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TNetSuiteConnectionPoolManager;
end;

function TNetSuiteUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TNetSuiteConnectDialogService;
end;

function TNetSuiteUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TNetSuiteUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TNetSuiteFormatter;
end;

procedure TNetSuiteUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TNetSuiteConnection, TNetSuiteConnectionParameters], 15));

    FConnectionOptions.Add(TEnumeratorOption.Create('AuthenticationType', prAuthentication, [TNetSuiteConnection, TNetSuiteConnectionParameters], Variant(atTokenBased), TypeInfo(TAuthenticationType)));
    FConnectionOptions.Add(TStringOption.Create('ConsumerKey', prConsumerKey, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ConsumerSecret', prConsumerSecret, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('TokenId', prToken, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('TokenSecret', prTokenSecret, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));

    FConnectionOptions.Add(TStringOption.Create('AccountId', prAccountId, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ApplicationId', prApplicationId, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('CustomTables', prCustomTables, [TNetSuiteConnection, TNetSuiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('CustomFields', prCustomFields, [TNetSuiteConnection, TNetSuiteConnectionParameters], False));
    FConnectionOptions.Add(TStringOption.Create('RoleId', prRoleId, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Sandbox', prSandbox, [TNetSuiteConnection, TNetSuiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TNetSuiteConnection, TNetSuiteConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TNetSuiteConnection, TNetSuiteConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TNetSuiteConnection, TNetSuiteConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TNetSuiteConnection, TNetSuiteConnectionParameters], ''));
  end;
end;

{ TNetSuiteConnectDialogService }

function TNetSuiteConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

function TNetSuiteConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;
function TNetSuiteConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TNetSuiteFormatter }

constructor TNetSuiteFormatter.Create;
begin
  inherited;

  FFunctions := NetSuiteFunctions;
  FPredefinedMacros := NetSuiteMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TNetSuiteUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TNetSuiteUniProvider);

  NetSuiteFunctions := TStrValueStringList.Create;
  NetSuiteFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  NetSuiteFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  NetSuiteFunctions.Add('CONCAT',      '%s || %s');
  NetSuiteFunctions.Add('TRIM',        'TRIM(%s)');
  NetSuiteFunctions.Add('UPPER',       'UPPER(%s)');
  NetSuiteFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  NetSuiteFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  NetSuiteFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  NetSuiteFunctions.Add('__DATE_TIME_LITERAL', '%s');
  NetSuiteFunctions.Add('__DATE_LITERAL',      '%s');
  NetSuiteFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  NetSuiteFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  NetSuiteFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  NetSuiteFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  NetSuiteMacros := TStrValueStringList.Create;
  NetSuiteMacros.Add('PROVIDER', 'NetSuite');
  NetSuiteMacros.Add('NETSUITE',   '');
  // DataType macros
  NetSuiteMacros.Add('DATETIME', 'TEXT');
  NetSuiteMacros.Add('DOUBLE',   'DOUBLE');
  NetSuiteMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TNetSuiteUniProvider);

  NetSuiteFunctions.Free;
  NetSuiteMacros.Free;

end.
