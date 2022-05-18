
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DynamicsDac.inc}
{$IFDEF VER10P}
{$I DynamicsCRMUniProvider.inc}
{$ENDIF}
unit DynamicsCRMUniProvider;

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
  TDynamicsCRMUniProvider = class(TODBCUniProvider)
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

  TDynamicsConnectDialogService = class(TConnectDialogService)
  public
    function DatabaseEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TDynamicsFormatter = class(TUniSqlFormatter)
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
  DynamicsProps, DynamicsClasses,
  DynamicsConnectionPool, DynamicsConnectionString;
{$ELSE}
  ODBCPropsUni,
  DynamicsPropsUni, DynamicsClassesUni,
  DynamicsConnectionPoolUni, DynamicsConnectionStringUni;
{$ENDIF}

var
  DynamicsFunctions, DynamicsMacros: TStrValueStringList;

class function TDynamicsCRMUniProvider.GetProviderName: string;
begin
  Result := 'Dynamics CRM';
end;

class function TDynamicsCRMUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TDynamicsConnectionStringBuilder;
end;

function TDynamicsCRMUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TDynamicsConnection;
end;

function TDynamicsCRMUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TDynamicsConnectionParameters;
end;

function TDynamicsCRMUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TDynamicsConnectionPoolManager;
end;

function TDynamicsCRMUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TDynamicsConnectDialogService;
end;

function TDynamicsCRMUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TDynamicsCRMUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TDynamicsFormatter;
end;

procedure TDynamicsCRMUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TDynamicsConnection, TDynamicsConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TDynamicsConnection, TDynamicsConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UTCDates', prUTCDates, [TDynamicsConnection, TDynamicsConnectionParameters], False));

    FConnectionOptions.Add(TStringOption.Create('ProxyServer', prProxyHostname, [TDynamicsConnection, TDynamicsConnectionParameters], ''));
    FConnectionOptions.Add(TIntegerOption.Create('ProxyPort', prProxyPort, [TDynamicsConnection, TDynamicsConnectionParameters], 0));
    FConnectionOptions.Add(TStringOption.Create('ProxyUser', prProxyUsername, [TDynamicsConnection, TDynamicsConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ProxyPassword', prProxyPassword, [TDynamicsConnection, TDynamicsConnectionParameters], ''));
  end;
end;

{ TDynamicsConnectDialogService }

function TDynamicsConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := False;
end;

function TDynamicsConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TDynamicsFormatter }

constructor TDynamicsFormatter.Create;
begin
  inherited;

  FFunctions := DynamicsFunctions;
  FPredefinedMacros := DynamicsMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Cloud Providers', [TDynamicsCRMUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TDynamicsCRMUniProvider);

  DynamicsFunctions := TStrValueStringList.Create;
  DynamicsFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  DynamicsFunctions.Add('SUBSTRING',   'SUBSTR(%s, %s, %s)');
  DynamicsFunctions.Add('CONCAT',      '%s || %s');
  DynamicsFunctions.Add('TRIM',        'TRIM(%s)');
  DynamicsFunctions.Add('UPPER',       'UPPER(%s)');
  DynamicsFunctions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  DynamicsFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  DynamicsFunctions.Add('DATEADD',      'DATE(%2:s, ''%1:s %0:s'')');
  // Date-time literals
  DynamicsFunctions.Add('__DATE_TIME_LITERAL', '%s');
  DynamicsFunctions.Add('__DATE_LITERAL',      '%s');
  DynamicsFunctions.Add('__TIME_LITERAL',      '%s');
  // CONVERT functions
  DynamicsFunctions.Add('TODATE',   'CAST(%s AS VARCHAR)');
  DynamicsFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  DynamicsFunctions.Add('TOCHAR',   'CAST(%s AS VARCHAR)');

  DynamicsMacros := TStrValueStringList.Create;
  DynamicsMacros.Add('PROVIDER', 'DynamicsCRM');
  DynamicsMacros.Add('DYNAMICSCRM',   '');
  // DataType macros
  DynamicsMacros.Add('DATETIME', 'TEXT');
  DynamicsMacros.Add('DOUBLE',   'DOUBLE');
  DynamicsMacros.Add('VARCHAR',  'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TDynamicsCRMUniProvider);

  DynamicsFunctions.Free;
  DynamicsMacros.Free;

end.
