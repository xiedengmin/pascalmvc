
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I BigCommerceDac.inc}
{$IFDEF VER10P}
{$I BigCommerceUniProvider.inc}
{$ENDIF}
unit BigCommerceUniProvider;

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
  TBigCommerceUniProvider = class(TODBCUniProvider)
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

  TBigCommerceConnectDialogService = class(TConnectDialogService)
  public
    function PasswordEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TBigCommerceFormatter = class(TUniSqlFormatter)
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
  BigCommerceConsts, BigCommerceProps, BigCommerceClasses,
  BigCommerceConnectionPool, BigCommerceConnectionString;
{$ELSE}
  ODBCPropsUni,
  BigCommerceConstsUni, BigCommercePropsUni, BigCommerceClassesUni,
  BigCommerceConnectionPoolUni, BigCommerceConnectionStringUni;
{$ENDIF}

var
  BigCommerceFunctions, BigCommerceMacros: TStrValueStringList;

class function TBigCommerceUniProvider.GetProviderName: string;
begin
  Result := 'BigCommerce';
end;

class function TBigCommerceUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TBigCommerceConnectionStringBuilder;
end;

function TBigCommerceUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TBigCommerceConnection;
end;

function TBigCommerceUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TBigCommerceConnectionParameters;
end;

function TBigCommerceUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TBigCommerceConnectionPoolManager;
end;

function TBigCommerceUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TBigCommerceConnectDialogService;
end;

function TBigCommerceUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TBigCommerceUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TBigCommerceFormatter;
end;

procedure TBigCommerceUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TBigCommerceConnection, TBigCommerceConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('AccessToken', prAccessToken, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('Authentication', prAuthentication, [TBigCommerceConnection, TBigCommerceConnectionParameters], Variant(atBasic), TypeInfo(TAuthenticationType)));
    FConnectionOptions.Add(TStringOption.Create('AuthenticationToken', prAuthenticationToken, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ClientId', prClientId, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('StoreId', prStoreId, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TBigCommerceConnection, TBigCommerceConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TBigCommerceConnection, TBigCommerceConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TBigCommerceConnection, TBigCommerceConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TBigCommerceConnection, TBigCommerceConnectionParameters], ''));
  end;
end;

{ TBigCommerceConnectDialogService }

function TBigCommerceConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TBigCommerceConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TBigCommerceConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TBigCommerceFormatter }

constructor TBigCommerceFormatter.Create;
begin
  inherited;

  FFunctions := BigCommerceFunctions;
  FPredefinedMacros := BigCommerceMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TBigCommerceUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TBigCommerceUniProvider);

  BigCommerceFunctions := TStrValueStringList.Create;
  BigCommerceFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  BigCommerceFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  BigCommerceFunctions.Add('CONCAT',      '%s || %s');
  BigCommerceFunctions.Add('TRIM',        'TRIM(%s)');
  BigCommerceFunctions.Add('UPPER',       'UPPER(%s)');
  BigCommerceFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  BigCommerceFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  BigCommerceFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  BigCommerceFunctions.Add('__DATE_TIME_LITERAL', '%s');
  BigCommerceFunctions.Add('__DATE_LITERAL',      '%s');
  BigCommerceFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  BigCommerceFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  BigCommerceFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  BigCommerceFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  BigCommerceMacros := TStrValueStringList.Create;
  BigCommerceMacros.Add('PROVIDER', 'BigCommerce');
  BigCommerceMacros.Add('BIGCOMMERCE',   '');
  // DataType macros
  BigCommerceMacros.Add('DATETIME', 'TEXT');
  BigCommerceMacros.Add('DOUBLE',   'DOUBLE');
  BigCommerceMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TBigCommerceUniProvider);

  BigCommerceFunctions.Free;
  BigCommerceMacros.Free;

end.
