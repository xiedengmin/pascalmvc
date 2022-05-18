
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ZohoDac.inc}
{$IFDEF VER10P}
{$I ZohoCRMUniProvider.inc}
{$ENDIF}
unit ZohoCRMUniProvider;

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
  TZohoCRMUniProvider = class(TODBCUniProvider)
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

  TZohoConnectDialogService = class(TConnectDialogService)
  public
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TZohoFormatter = class(TUniSqlFormatter)
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
  ZohoConsts, ZohoProps, ZohoClasses,
  ZohoConnectionPool, ZohoConnectionString;
{$ELSE}
  ODBCPropsUni,
  ZohoConstsUni, ZohoPropsUni, ZohoClassesUni,
  ZohoConnectionPoolUni, ZohoConnectionStringUni;
{$ENDIF}

var
  ZohoFunctions, ZohoMacros: TStrValueStringList;

class function TZohoCRMUniProvider.GetProviderName: string;
begin
  Result := 'Zoho CRM';
end;

class function TZohoCRMUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TZohoConnectionStringBuilder;
end;

function TZohoCRMUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TZohoConnection;
end;

function TZohoCRMUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TZohoConnectionParameters;
end;

function TZohoCRMUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TZohoConnectionPoolManager;
end;

function TZohoCRMUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TZohoConnectDialogService;
end;

function TZohoCRMUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TZohoCRMUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TZohoFormatter;
end;

procedure TZohoCRMUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TZohoConnection, TZohoConnectionParameters], 15));
    FConnectionOptions.Add(TEnumeratorOption.Create('APIVersion', prApiVersion, [TZohoConnection], Variant(apiVer2), TypeInfo(TApiVersion)));
    FConnectionOptions.Add(TStringOption.Create('AccessToken', prAccessToken, [TZohoConnection, TZohoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('AuthenticationToken', prAuthenticationToken, [TZohoConnection, TZohoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('RefreshToken', prRefreshToken, [TZohoConnection, TZohoConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TZohoConnection, TZohoConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TZohoConnection, TZohoConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('EnableNonApprovedRecords', prEnableNonApprovedRecords, [TZohoConnection, TZohoConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TZohoConnection, TZohoConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TZohoConnection, TZohoConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TZohoConnection, TZohoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TZohoConnection, TZohoConnectionParameters], ''));
  end;
end;

{ TZohoConnectDialogService }

function TZohoConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TZohoConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TZohoConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TZohoConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TZohoFormatter }

constructor TZohoFormatter.Create;
begin
  inherited;

  FFunctions := ZohoFunctions;
  FPredefinedMacros := ZohoMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TZohoCRMUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TZohoCRMUniProvider);

  ZohoFunctions := TStrValueStringList.Create;
  ZohoFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  ZohoFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  ZohoFunctions.Add('CONCAT',      '%s || %s');
  ZohoFunctions.Add('TRIM',        'TRIM(%s)');
  ZohoFunctions.Add('UPPER',       'UPPER(%s)');
  ZohoFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  ZohoFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  ZohoFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  ZohoFunctions.Add('__DATE_TIME_LITERAL', '%s');
  ZohoFunctions.Add('__DATE_LITERAL',      '%s');
  ZohoFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  ZohoFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  ZohoFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  ZohoFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  ZohoMacros := TStrValueStringList.Create;
  ZohoMacros.Add('PROVIDER', 'ZohoCRM');
  ZohoMacros.Add('ZOHOCRM',   '');
  // DataType macros
  ZohoMacros.Add('DATETIME', 'TEXT');
  ZohoMacros.Add('DOUBLE',   'DOUBLE');
  ZohoMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TZohoCRMUniProvider);

  ZohoFunctions.Free;
  ZohoMacros.Free;

end.
