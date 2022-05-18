
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I QuickBooksDac.inc}
{$IFDEF VER10P}
{$I QuickBooksUniProvider.inc}
{$ENDIF}
unit QuickBooksUniProvider;
{$ENDIF}

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
  TQuickBooksUniProvider = class(TODBCUniProvider)
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

  TQuickBooksConnectDialogService = class(TConnectDialogService)
  public
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TQuickBooksFormatter = class(TUniSqlFormatter)
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
  QuickBooksProps, QuickBooksClasses,
  QuickBooksConnectionPool, QuickBooksConnectionString;
{$ELSE}
  ODBCPropsUni,
  QuickBooksPropsUni, QuickBooksClassesUni,
  QuickBooksConnectionPoolUni, QuickBooksConnectionStringUni;
{$ENDIF}

var
  QuickBooksFunctions, QuickBooksMacros: TStrValueStringList;

class function TQuickBooksUniProvider.GetProviderName: string;
begin
  Result := 'QuickBooks';
end;

class function TQuickBooksUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TQuickBooksConnectionStringBuilder;
end;

function TQuickBooksUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TQuickBooksConnection;
end;

function TQuickBooksUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TQuickBooksConnectionParameters;
end;

function TQuickBooksUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TQuickBooksConnectionPoolManager;
end;

function TQuickBooksUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TQuickBooksConnectDialogService;
end;

function TQuickBooksUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TQuickBooksUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TQuickBooksFormatter;
end;

procedure TQuickBooksUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TQuickBooksConnection, TQuickBooksConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('CompanyId', prCompanyId, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('RefreshToken', prRefreshToken, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('Sandbox', prSandbox, [TQuickBooksConnection, TQuickBooksConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TQuickBooksConnection, TQuickBooksConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TQuickBooksConnection, TQuickBooksConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TQuickBooksConnection, TQuickBooksConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    // deprecated options
    FConnectionOptions.Add(TStringOption.Create('AccessToken', prAccessToken, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('AccessTokenSecret', prAccessTokenSecret, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ConsumerKey', prConsumerKey, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ConsumerKeySecret', prConsumerKeySecret, [TQuickBooksConnection, TQuickBooksConnectionParameters], ''));
  end;
end;

{ TQuickBooksConnectDialogService }

function TQuickBooksConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TQuickBooksConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TQuickBooksConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

function TQuickBooksConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TQuickBooksConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TQuickBooksFormatter }

constructor TQuickBooksFormatter.Create;
begin
  inherited;

  FFunctions := QuickBooksFunctions;
  FPredefinedMacros := QuickBooksMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TQuickBooksUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TQuickBooksUniProvider);

  QuickBooksFunctions := TStrValueStringList.Create;
  QuickBooksFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  QuickBooksFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  QuickBooksFunctions.Add('CONCAT',      '%s || %s');
  QuickBooksFunctions.Add('TRIM',        'TRIM(%s)');
  QuickBooksFunctions.Add('UPPER',       'UPPER(%s)');
  QuickBooksFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  QuickBooksFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  QuickBooksFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  QuickBooksFunctions.Add('__DATE_TIME_LITERAL', '%s');
  QuickBooksFunctions.Add('__DATE_LITERAL',      '%s');
  QuickBooksFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  QuickBooksFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  QuickBooksFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  QuickBooksFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  QuickBooksMacros := TStrValueStringList.Create;
  QuickBooksMacros.Add('PROVIDER', 'QuickBooks');
  QuickBooksMacros.Add('QUICKBOOKS',   '');
  // DataType macros
  QuickBooksMacros.Add('DATETIME', 'TEXT');
  QuickBooksMacros.Add('DOUBLE',   'DOUBLE');
  QuickBooksMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TQuickBooksUniProvider);

  QuickBooksFunctions.Free;
  QuickBooksMacros.Free;

end.
