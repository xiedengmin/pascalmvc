
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I FreshBooksDac.inc}
{$IFDEF VER10P}
{$I FreshBooksUniProvider.inc}
{$ENDIF}
unit FreshBooksUniProvider;

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
  TFreshBooksUniProvider = class(TODBCUniProvider)
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

  TFreshBooksConnectDialogService = class(TConnectDialogService)
  public
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TFreshBooksFormatter = class(TUniSqlFormatter)
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
  FreshBooksConsts, FreshBooksProps, FreshBooksClasses,
  FreshBooksConnectionPool, FreshBooksConnectionString;
{$ELSE}
  ODBCPropsUni,
  FreshBooksConstsUni, FreshBooksPropsUni, FreshBooksClassesUni,
  FreshBooksConnectionPoolUni, FreshBooksConnectionStringUni;
{$ENDIF}

var
  FreshBooksFunctions, FreshBooksMacros: TStrValueStringList;

class function TFreshBooksUniProvider.GetProviderName: string;
begin
  Result := 'FreshBooks';
end;

class function TFreshBooksUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TFreshBooksConnectionStringBuilder;
end;

function TFreshBooksUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TFreshBooksConnection;
end;

function TFreshBooksUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TFreshBooksConnectionParameters;
end;

function TFreshBooksUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TFreshBooksConnectionPoolManager;
end;

function TFreshBooksUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TFreshBooksConnectDialogService;
end;

function TFreshBooksUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TFreshBooksUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TFreshBooksFormatter;
end;

procedure TFreshBooksUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TFreshBooksConnection, TFreshBooksConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('AccessToken', prAccessToken, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('ApiVersion', prApiVersion, [TFreshBooksConnection, TFreshBooksConnectionParameters], DefApiVersion, TypeInfo(TApiVersion)));
    FConnectionOptions.Add(TStringOption.Create('AuthenticationToken', prAuthenticationToken, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('CompanyName', prCompanyName, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('RefreshToken', prRefreshToken, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TFreshBooksConnection, TFreshBooksConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TFreshBooksConnection, TFreshBooksConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TFreshBooksConnection, TFreshBooksConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TFreshBooksConnection, TFreshBooksConnectionParameters], ''));
  end;
end;

{ TFreshBooksConnectDialogService }

function TFreshBooksConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TFreshBooksConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TFreshBooksConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TFreshBooksConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TFreshBooksFormatter }

constructor TFreshBooksFormatter.Create;
begin
  inherited;

  FFunctions := FreshBooksFunctions;
  FPredefinedMacros := FreshBooksMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TFreshBooksUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TFreshBooksUniProvider);

  FreshBooksFunctions := TStrValueStringList.Create;
  FreshBooksFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  FreshBooksFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  FreshBooksFunctions.Add('CONCAT',      '%s || %s');
  FreshBooksFunctions.Add('TRIM',        'TRIM(%s)');
  FreshBooksFunctions.Add('UPPER',       'UPPER(%s)');
  FreshBooksFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  FreshBooksFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  FreshBooksFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  FreshBooksFunctions.Add('__DATE_TIME_LITERAL', '%s');
  FreshBooksFunctions.Add('__DATE_LITERAL',      '%s');
  FreshBooksFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  FreshBooksFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  FreshBooksFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  FreshBooksFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  FreshBooksMacros := TStrValueStringList.Create;
  FreshBooksMacros.Add('PROVIDER', 'FreshBooks');
  FreshBooksMacros.Add('FRESHBOOKS',   '');
  // DataType macros
  FreshBooksMacros.Add('DATETIME', 'TEXT');
  FreshBooksMacros.Add('DOUBLE',   'DOUBLE');
  FreshBooksMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TFreshBooksUniProvider);

  FreshBooksFunctions.Free;
  FreshBooksMacros.Free;

end.
