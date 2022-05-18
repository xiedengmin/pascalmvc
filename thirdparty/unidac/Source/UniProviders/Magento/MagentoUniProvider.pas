
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MagentoDac.inc}
{$IFDEF VER10P}
{$I MagentoUniProvider.inc}
{$ENDIF}
unit MagentoUniProvider;

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
  TMagentoUniProvider = class(TODBCUniProvider)
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

  TMagentoConnectDialogService = class(TConnectDialogService)
  public
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TMagentoFormatter = class(TUniSqlFormatter)
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
  MagentoConsts, MagentoProps, MagentoClasses,
  MagentoConnectionPool, MagentoConnectionString;
{$ELSE}
  ODBCPropsUni,
  MagentoConstsUni, MagentoPropsUni, MagentoClassesUni,
  MagentoConnectionPoolUni, MagentoConnectionStringUni;
{$ENDIF}

var
  MagentoFunctions, MagentoMacros: TStrValueStringList;

class function TMagentoUniProvider.GetProviderName: string;
begin
  Result := 'Magento';
end;

class function TMagentoUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TMagentoConnectionStringBuilder;
end;

function TMagentoUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMagentoConnection;
end;

function TMagentoUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMagentoConnectionParameters;
end;

function TMagentoUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMagentoConnectionPoolManager;
end;

function TMagentoUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TMagentoConnectDialogService;
end;

function TMagentoUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TMagentoUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMagentoFormatter;
end;

procedure TMagentoUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TMagentoConnection, TMagentoConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('ApiKey', prApiKey, [TMagentoConnection, TMagentoConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('ApiVersion', prApiVersion, [TMagentoConnection, TMagentoConnectionParameters], DefApiVersion, TypeInfo(TApiVersion)));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TMagentoConnection, TMagentoConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TMagentoConnection, TMagentoConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TMagentoConnection, TMagentoConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TMagentoConnection, TMagentoConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TMagentoConnection, TMagentoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TMagentoConnection, TMagentoConnectionParameters], ''));
  end;
end;

{ TMagentoConnectDialogService }

function TMagentoConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TMagentoConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TMagentoFormatter }

constructor TMagentoFormatter.Create;
begin
  inherited;

  FFunctions := MagentoFunctions;
  FPredefinedMacros := MagentoMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TMagentoUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TMagentoUniProvider);

  MagentoFunctions := TStrValueStringList.Create;
  MagentoFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  MagentoFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  MagentoFunctions.Add('CONCAT',      '%s || %s');
  MagentoFunctions.Add('TRIM',        'TRIM(%s)');
  MagentoFunctions.Add('UPPER',       'UPPER(%s)');
  MagentoFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  MagentoFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  MagentoFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  MagentoFunctions.Add('__DATE_TIME_LITERAL', '%s');
  MagentoFunctions.Add('__DATE_LITERAL',      '%s');
  MagentoFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  MagentoFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  MagentoFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  MagentoFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  MagentoMacros := TStrValueStringList.Create;
  MagentoMacros.Add('PROVIDER', 'Magento');
  MagentoMacros.Add('MAGENTO',   '');
  // DataType macros
  MagentoMacros.Add('DATETIME', 'TEXT');
  MagentoMacros.Add('DOUBLE',   'DOUBLE');
  MagentoMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TMagentoUniProvider);

  MagentoFunctions.Free;
  MagentoMacros.Free;

end.
