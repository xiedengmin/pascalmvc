
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ExactTargetDac.inc}
{$IFDEF VER10P}
{$I SalesforceMCUniProvider.inc}
{$ENDIF}
unit SalesforceMCUniProvider;

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
  TSalesforceMCUniProvider = class(TODBCUniProvider)
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

  TExactTargetConnectDialogService = class(TConnectDialogService)
  public
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TExactTargetFormatter = class(TUniSqlFormatter)
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
  ExactTargetConsts, ExactTargetProps, ExactTargetClasses,
  ExactTargetConnectionPool, ExactTargetConnectionString;
{$ELSE}
  ODBCPropsUni,
  ExactTargetConstsUni, ExactTargetPropsUni, ExactTargetClassesUni,
  ExactTargetConnectionPoolUni, ExactTargetConnectionStringUni;
{$ENDIF}

var
  ExactTargetFunctions, ExactTargetMacros: TStrValueStringList;

class function TSalesforceMCUniProvider.GetProviderName: string;
begin
  Result := 'Salesforce MC';
end;

class function TSalesforceMCUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TExactTargetConnectionStringBuilder;
end;

function TSalesforceMCUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TExactTargetConnection;
end;

function TSalesforceMCUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TExactTargetConnectionParameters;
end;

function TSalesforceMCUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TExactTargetConnectionPoolManager;
end;

function TSalesforceMCUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TExactTargetConnectDialogService;
end;

function TSalesforceMCUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TSalesforceMCUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TExactTargetFormatter;
end;

procedure TSalesforceMCUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TExactTargetConnection, TExactTargetConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('AppClientID', prAppClientID, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('AppClientSecret', prAppClientSecret, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('AppSandbox', prAppSandbox, [TExactTargetConnection, TExactTargetConnectionParameters], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('Authentication', prAuthentication, [TExactTargetConnection, TExactTargetConnectionParameters], Variant(atUserAndPassword), TypeInfo(TAuthenticationType)));
    FConnectionOptions.Add(TStringOption.Create('PartnerIDs', prPartnerIDs, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TExactTargetConnection, TExactTargetConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TExactTargetConnection, TExactTargetConnectionParameters], False));


    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TExactTargetConnection, TExactTargetConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TExactTargetConnection, TExactTargetConnectionParameters], ''));
  end;
end;

{ TExactTargetConnectDialogService }

function TExactTargetConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TExactTargetConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TExactTargetFormatter }

constructor TExactTargetFormatter.Create;
begin
  inherited;

  FFunctions := ExactTargetFunctions;
  FPredefinedMacros := ExactTargetMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TSalesforceMCUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TSalesforceMCUniProvider);

  ExactTargetFunctions := TStrValueStringList.Create;
  ExactTargetFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  ExactTargetFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  ExactTargetFunctions.Add('CONCAT',      '%s || %s');
  ExactTargetFunctions.Add('TRIM',        'TRIM(%s)');
  ExactTargetFunctions.Add('UPPER',       'UPPER(%s)');
  ExactTargetFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  ExactTargetFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  ExactTargetFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  ExactTargetFunctions.Add('__DATE_TIME_LITERAL', '%s');
  ExactTargetFunctions.Add('__DATE_LITERAL',      '%s');
  ExactTargetFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  ExactTargetFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  ExactTargetFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  ExactTargetFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  ExactTargetMacros := TStrValueStringList.Create;
  ExactTargetMacros.Add('PROVIDER', 'SalesforceMC');
  ExactTargetMacros.Add('SALESFORCEMC',   '');
  // DataType macros
  ExactTargetMacros.Add('DATETIME', 'TEXT');
  ExactTargetMacros.Add('DOUBLE',   'DOUBLE');
  ExactTargetMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TSalesforceMCUniProvider);

  ExactTargetFunctions.Free;
  ExactTargetMacros.Free;

end.
