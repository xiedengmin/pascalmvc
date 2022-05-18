
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
{$IFDEF VER10P}
{$I DBFUniProvider.inc}
{$ENDIF}
unit DBFUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
  CRAccess, MemData, CRTypes, CRParser, CRDataTypeMap,
  CRConnectionPool, CRConnectionString,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, DAScript,
  {$IFDEF ODBC_PROVIDER}ODBCUniProvider,{$ELSE}{$IFDEF MOBILE}ODBCUniProvider,{$ENDIF}{$ENDIF}
  UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TDBFUniProvider = class({$IFDEF ODBC_PROVIDER}TODBCUniProvider{$ELSE}TUniProvider{$ENDIF})
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

    function GetParserClass: TSQLParserClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  {$IFNDEF ODBC_PROVIDER}
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
  {$ENDIF}
  end;

  TDBFConnectDialogService = class(TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
    function ServerEnabled: boolean; override;
    function UsernameEnabled: boolean; override;
    function PasswordEnabled: boolean; override;
    function PortEnabled: boolean; override;
  end;

  TDBFFormatter = class(TUniSqlFormatter)
  protected
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
  {$IFDEF ODBC_PROVIDER}ODBCProps, ODBCClasses, ODBCParser, ODBCServices,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClasses,{$ENDIF}
  DBFConsts, DBFProps, DBFParser, DBFDataTypeMap, DBFConnectionString, DBFConnection, DBFServices;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClassesUni,{$ENDIF}
  DBFConstsUni, DBFPropsUni, DBFParserUni, DBFDataTypeMapUni, DBFConnectionStringUni, DBFConnectionUni, DBFServicesUni;
{$ENDIF}

var
  DBFFunctions, DBFMacros: TStrValueStringList;

class function TDBFUniProvider.GetProviderName: string;
begin
  Result := 'DBF';
end;

class function TDBFUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TDBFConnectionStringBuilder;
end;

function TDBFUniProvider.IsServerSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TDBFUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.IsPoolingSupported: boolean;
begin
  Result := False;
end;

function TDBFUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TDBFParser;
end;

function TDBFUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TCRConnectionParameters;
end;

function TDBFUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TCRConnectionPoolManager;
end;

function TDBFUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TDBFConnection;
end;

function TDBFUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomDBFDataSetService;
end;

function TDBFUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TDBFConnectDialogService;
end;

function TDBFUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TDBFFormatter;
end;

function TDBFUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TDBFConverterManager;
end;

{$IFNDEF ODBC_PROVIDER}
function TDBFUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomDBFFieldTypeMap;
end;
{$ENDIF}

procedure TDBFUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TStringOption.Create('CollatingSequence', prCollatingSequence, [TDBFConnection], ''));
  {$IFDEF ODBC_PROVIDER}
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TODBCConnection], False));
  {$ENDIF}
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TDBFConnection], 15));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TDBFConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('Direct', prDirect, [TDBFConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('DBFFormat', prDBFFormat, [TDBFConnection], Variant(dfAuto), TypeInfo(TDBFFormat)));
    FConnectionOptions.Add(TEnumeratorOption.Create('CodePage', prCodePage, [TDBFConnection], Variant(dpDefault), TypeInfo(TDBFCodePage)));
    FConnectionOptions.Add(TEnumeratorOption.Create('ConnectMode', prConnectMode, [TDBFConnection], Variant(cmShared), TypeInfo(TDBFConnectMode)));
    FConnectionOptions.Add(TEnumeratorOption.Create('IndexOnReading', prIndexOnReading, [TDBFConnection], Variant(ikNative), TypeInfo(TDBFIndexKind)));
    FConnectionOptions.Add(TBooleanOption.Create('IgnoreDataErrors', prIgnoreDataErrors, [TDBFConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('IgnoreMetadataErrors', prIgnoreMetadataErrors, [TDBFConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('IgnoreBrokenTables', prIgnoreBrokenTables, [TDBFConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('IgnoreIndexErrors', prIgnoreIndexErrors, [TDBFConnection], False));
    FConnectionOptions.Add(TEnumeratorOption.Create('IdentifierCase', prIdentifierCase, [TDBFConnection], Variant(icOriginal), TypeInfo(TDBFIdentifierCase)));
    FConnectionOptions.Add(TBooleanOption.Create('AllFieldsAsNullable', prAllFieldsAsNullable, [TDBFConnection], False));
  end;
end;

procedure TDBFUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF ODBC_PROVIDER}TDBFCommand{$ENDIF}], 0));
  end;
end;

procedure TDBFUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [{$IFDEF ODBC_PROVIDER}TDBFRecordSet{$ENDIF}], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [{$IFDEF ODBC_PROVIDER}TDBFRecordSet{$ENDIF}], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [{$IFDEF ODBC_PROVIDER}TDBFRecordSet{$ENDIF}], True));
  end;
end;

{ TDBFConnectDialogService }

function TDBFConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

function TDBFConnectDialogService.ServerEnabled: boolean;
begin
  Result := False;
end;

function TDBFConnectDialogService.UsernameEnabled: boolean;
begin
  Result := False;
end;

function TDBFConnectDialogService.PasswordEnabled: boolean;
begin
  Result := False;
end;

function TDBFConnectDialogService.PortEnabled: boolean;
begin
  Result := False;
end;

{ TDBFFormatter }

constructor TDBFFormatter.Create;
begin
  inherited;

  FFunctions := DBFFunctions;
  FPredefinedMacros := DBFMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TDBFUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TDBFUniProvider);

  DBFFunctions := TStrValueStringList.Create;
  DBFFunctions.Add('USER', 'USER');
  DBFFunctions.Add('CHAR_LENGTH', 'LEN(%s)');
  DBFFunctions.Add('LOCATE', 'POSSTR(%1:s, %0:s)');
  DBFFunctions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  DBFFunctions.Add('CONCAT', '%s + %s');
  DBFFunctions.Add('CHAR', 'CHAR(%s)');
  DBFFunctions.Add('TRIM', 'TRIM(%s)');
  DBFFunctions.Add('TRUNCATE', 'TRUNC(%s, %s)');
  DBFFunctions.Add('CEILING', 'CEILING(%s)');
  DBFFunctions.Add('UPPER',   'UPPER(%s)');
  DBFFunctions.Add('LOWER',   'LOWER(%s)');
  // Date-time
  DBFFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  DBFFunctions.Add('YEAR', 'YEAR(%s)');
  DBFFunctions.Add('MONTH', 'MONTH(%s)');
  DBFFunctions.Add('DAY', 'DAY(%s)');
  //DBFFunctions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //DBFFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  DBFFunctions.Add('__DATE_TIME_LITERAL', '(%s)');
  DBFFunctions.Add('__DATE_LITERAL', '(%s)');
  DBFFunctions.Add('__TIME_LITERAL', '(%s)');
  // CONVERT functions
  DBFFunctions.Add('TODATE', '(%s)');
  DBFFunctions.Add('TONUMBER', '(%s)');
  DBFFunctions.Add('TOCHAR', '(%s)');

  DBFMacros := TStrValueStringList.Create;
  DBFMacros.Add('PROVIDER', 'DBF');
  DBFMacros.Add('DBF', '');
  // DataType macros
  DBFMacros.Add('DATETIME', 'DATE');
  DBFMacros.Add('DOUBLE', 'DOUBLE');
  DBFMacros.Add('VARCHAR', 'TEXT');

finalization
  UniProviders.UnRegisterProvider(TDBFUniProvider);

  DBFFunctions.Free;
  DBFMacros.Free;

end.
