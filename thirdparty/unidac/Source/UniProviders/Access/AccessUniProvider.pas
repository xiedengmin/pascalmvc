
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I AccessDac.inc}
{$IFDEF VER10P}
{$I AccessUniProvider.inc}
{$ENDIF}
unit AccessUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, CRConnectionString,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  CRTypes, CRParser, CRDataTypeMap,
  DAScript, UniProvider, ODBCUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TAccessUniProvider = class(TODBCUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsServerSupported: boolean; override;
    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;

    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TAccessConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
  end;

  TAccessFormatter = class(TUniSqlFormatter)
  protected
    function NeedUnquote: boolean; override;

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
  DAConsts, UniConsts, CRProps,
{$IFNDEF UNIDACPRO}
  ODBCProps, ODBCClasses, ODBCParser, ODBCServices,
  AccessProps, AccessClasses, AccessServices, AccessDataTypeMap, AccessConnectionString;
{$ELSE}
  ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni,
  AccessPropsUni, AccessClassesUni, AccessServicesUni, AccessDataTypeMapUni, AccessConnectionStringUni;
{$ENDIF}

var
  AccessFunctions, AccessMacros: TStrValueStringList;

class function TAccessUniProvider.GetProviderName: string;
begin
  Result := 'Access';
end;

class function TAccessUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TAccessConnectionStringBuilder;
end;

function TAccessUniProvider.IsServerSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TAccessUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.IsPoolingSupported: boolean;
begin
  Result := False;
end;

function TAccessUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TAccessUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TAccessUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TAccessConnection;
end;

function TAccessUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomAccessDataSetService;
end;

function TAccessUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TAccessConnectDialogService;
end;

function TAccessUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TAccessFormatter;
end;

function TAccessUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TAccessConverterManager;
end;

procedure TAccessUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TAccessConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TAccessConnection], 15));
    FConnectionOptions.Add(TBooleanOption.Create('ExclusiveLock', prExclusiveLock, [TAccessConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('ExtendedAnsiSQL', prExtendedAnsiSQL, [TAccessConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('ForceCreateDatabase', prForceCreateDatabase, [TAccessConnection], False));
    FConnectionOptions.Add(TStringOption.Create('SystemDatabase', prSystemDatabase, [TAccessConnection], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TAccessConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('DriverVersion', prDriverVersion, [TAccessConnection], Variant(dvAuto), TypeInfo(TDriverVersion)));
  end;
end;

procedure TAccessUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TAccessCommand], 0));
  end;
end;

procedure TAccessUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TAccessRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TAccessRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TAccessRecordSet], True));
  end;
end;

{ TAccessConnectDialogService }

function TAccessConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TAccessConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

{ TAccessFormatter }

constructor TAccessFormatter.Create;
begin
  inherited;

  FFunctions := AccessFunctions;
  FPredefinedMacros := AccessMacros;
end;

function TAccessFormatter.NeedUnquote: boolean;
begin
  Result := True;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TAccessUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TAccessUniProvider);

  AccessFunctions := TStrValueStringList.Create;
  AccessFunctions.Add('USER', 'USER');
  AccessFunctions.Add('CHAR_LENGTH', 'LEN(%s)');
  AccessFunctions.Add('LOCATE', 'POSSTR(%1:s, %0:s)');
  AccessFunctions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  AccessFunctions.Add('CONCAT', '%s + %s');
  AccessFunctions.Add('CHAR', 'CHAR(%s)');
  AccessFunctions.Add('TRIM', 'TRIM(%s)');
  AccessFunctions.Add('TRUNCATE', 'TRUNC(%s, %s)');
  AccessFunctions.Add('CEILING', 'CEILING(%s)');
  AccessFunctions.Add('UPPER', 'UCASE(%s)');
  AccessFunctions.Add('LOWER', 'LCASE(%s)');
  // Date-time
  AccessFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  AccessFunctions.Add('YEAR', 'YEAR(%s)');
  AccessFunctions.Add('MONTH', 'MONTH(%s)');
  AccessFunctions.Add('DAY', 'DAY(%s)');
  //AccessFunctions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //AccessFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  AccessFunctions.Add('__DATE_TIME_LITERAL', '#%s#');
  AccessFunctions.Add('__DATE_LITERAL', '#%s#');
  AccessFunctions.Add('__TIME_LITERAL', '#%s#');
  // CONVERT functions
  AccessFunctions.Add('TODATE', '(%s)');
  AccessFunctions.Add('TONUMBER', '(%s)');
  AccessFunctions.Add('TOCHAR', '(%s)');

  AccessMacros := TStrValueStringList.Create;
  AccessMacros.Add('PROVIDER', 'Access');
  AccessMacros.Add('ACCESS', '');
  // DataType macros
  AccessMacros.Add('DATETIME', 'DATE');
  AccessMacros.Add('DOUBLE', 'DOUBLE');
  AccessMacros.Add('VARCHAR', 'TEXT');

finalization
  UniProviders.UnRegisterProvider(TAccessUniProvider);

  AccessFunctions.Free;
  AccessMacros.Free;

end.
