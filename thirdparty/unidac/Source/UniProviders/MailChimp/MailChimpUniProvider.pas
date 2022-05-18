
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MailChimpDac.inc}
{$IFDEF VER10P}
{$I MailChimpUniProvider.inc}
{$ENDIF}
unit MailChimpUniProvider;

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
  TMailChimpUniProvider = class(TODBCUniProvider)
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

  TMailChimpConnectDialogService = class(TConnectDialogService)
  public
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function ServerEnabled: boolean; override;
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TMailChimpFormatter = class(TUniSqlFormatter)
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
  MailChimpConsts, MailChimpProps, MailChimpClasses,
  MailChimpConnectionPool, MailChimpConnectionString;
{$ELSE}
  ODBCPropsUni,
  MailChimpConstsUni, MailChimpPropsUni, MailChimpClassesUni,
  MailChimpConnectionPoolUni, MailChimpConnectionStringUni;
{$ENDIF}

var
  MailChimpFunctions, MailChimpMacros: TStrValueStringList;

class function TMailChimpUniProvider.GetProviderName: string;
begin
  Result := 'MailChimp';
end;

class function TMailChimpUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TMailChimpConnectionStringBuilder;
end;

function TMailChimpUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMailChimpConnection;
end;

function TMailChimpUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMailChimpConnectionParameters;
end;

function TMailChimpUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMailChimpConnectionPoolManager;
end;

function TMailChimpUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMailChimpConnectDialogService;
end;

function TMailChimpUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TMailChimpUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMailChimpFormatter;
end;

procedure TMailChimpUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TMailChimpConnection, TMailChimpConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('ApiKey', prApiKey, [TMailChimpConnection, TMailChimpConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('ApiVersion', prApiVersion, [TMailChimpConnection], Variant(apiVer3), TypeInfo(TApiVersion)));
    FConnectionOptions.Add(TEnumeratorOption.Create('MergeCustomFields', prMergeCustomFields, [TMailChimpConnection], Variant(mcfJoinCommon), TypeInfo(TMergeCustomFields)));
    FConnectionOptions.Add(TBooleanOption.Create('UseMergeTagAsFieldName', prMergeTagAsFieldName, [TMailChimpConnection, TMailChimpConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TMailChimpConnection, TMailChimpConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TMailChimpConnection, TMailChimpConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TMailChimpConnection, TMailChimpConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TMailChimpConnection, TMailChimpConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TMailChimpConnection, TMailChimpConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TMailChimpConnection, TMailChimpConnectionParameters], ''));
  end;
end;

{ TMailChimpConnectDialogService }

function TMailChimpConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TMailChimpConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TMailChimpConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

function TMailChimpConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TMailChimpConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TMailChimpFormatter }

constructor TMailChimpFormatter.Create;
begin
  inherited;

  FFunctions := MailChimpFunctions;
  FPredefinedMacros := MailChimpMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TMailChimpUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TMailChimpUniProvider);

  MailChimpFunctions := TStrValueStringList.Create;
  MailChimpFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  MailChimpFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  MailChimpFunctions.Add('CONCAT',      '%s || %s');
  MailChimpFunctions.Add('TRIM',        'TRIM(%s)');
  MailChimpFunctions.Add('UPPER',       'UPPER(%s)');
  MailChimpFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  MailChimpFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  MailChimpFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  MailChimpFunctions.Add('__DATE_TIME_LITERAL', '%s');
  MailChimpFunctions.Add('__DATE_LITERAL',      '%s');
  MailChimpFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  MailChimpFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  MailChimpFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  MailChimpFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  MailChimpMacros := TStrValueStringList.Create;
  MailChimpMacros.Add('PROVIDER', 'MailChimp');
  MailChimpMacros.Add('MAILCHIMP',   '');
  // DataType macros
  MailChimpMacros.Add('DATETIME', 'TEXT');
  MailChimpMacros.Add('DOUBLE',   'DOUBLE');
  MailChimpMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TMailChimpUniProvider);

  MailChimpFunctions.Free;
  MailChimpMacros.Free;

end.
