
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF IOS}
{$IFNDEF ANDROID}
{$I ODBCDac.inc}
{$IFDEF VER10P}
{$I ODBCUniProvider.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}

unit ODBCUniProvider;

interface

{$IFNDEF IOS}
{$IFNDEF ANDROID}

uses
  SysUtils, Classes, Variants, DB, 
  CRTypes, CRAccess, CRParser, CRConnectionPool, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess, DAScript,
  {$IFNDEF STD}DADump,{$ENDIF}
  Uni, UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFNDEF STD} or pidOSX32{$IFDEF VER25P} or pidLinux64{$IFDEF VER26P} or pidOSX64{$ENDIF}{$ENDIF}{$ENDIF})]
{$ENDIF}
  TODBCUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;

    function GetParserClass: TSQLParserClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
  {$IFNDEF STD}
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TODBCConnectDialogService = class(TConnectDialogService)
  end;

  TODBCFormatter = class(TUniSqlFormatter)
  protected
  public
    constructor Create; override;
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

{$ENDIF}
{$ENDIF}

implementation

{$IFNDEF IOS}
{$IFNDEF ANDROID}

uses
  DAConsts, UniConsts, CRProps,
{$IFNDEF UNIDACPRO}
  ODBCProps, ODBCClasses, ODBCParser, ODBCServices, ODBCConnectionPool,
  ODBCDataTypeMap, ODBCConnectionString, ODBCConsts;
{$ELSE}
  ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni, ODBCConnectionPoolUni,
  ODBCDataTypeMapUni, ODBCConnectionStringUni, ODBCConstsUni;
{$ENDIF}

var
  ODBCFunctions, ODBCMacros: TStrValueStringList;

class function TODBCUniProvider.GetProviderName: string;
begin
  Result := 'ODBC';
end;

class function TODBCUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TODBCConnectionStringBuilder;
end;

function TODBCUniProvider.IsDatabaseSupported: boolean;
begin
  Result := False;
end;

function TODBCUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TODBCUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TODBCUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TODBCParser;
end;

function TODBCUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TODBCConnectionParameters;
end;

function TODBCUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TODBCConnectionPoolManager;
end;

function TODBCUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TODBCConnection;
end;

function TODBCUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TODBCServerEnumerator;
end;

function TODBCUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomODBCDataSetService;
end;

function TODBCUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TODBCScriptProcessor;
end;

{$IFNDEF STD}

function TODBCUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomODBCDumpProcessor;
end;

{$ENDIF}

function TODBCUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TODBCConnectDialogService;
end;

function TODBCUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomODBCFieldTypeMap;
end;

function TODBCUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TODBCFormatter;
end;

function TODBCUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TODBCConverterManager;
end;

procedure TODBCUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TODBCConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TODBCConnection, TODBCConnectionParameters], 15));
    FConnectionOptions.Add(TBooleanOption.Create('DetectFieldsOnPrepare', prDetectFieldsOnPrepare, [TODBCConnection], True));
    FConnectionOptions.Add(TEnumeratorOption.Create('DSNType', prDSNType, [TODBCConnection, TODBCConnectionParameters], Variant(ntAuto), TypeInfo(TDSNType)));
    FConnectionOptions.Add(TBooleanOption.Create('LongVarBinaryAsBlob', prLongVarBinaryAsBlob, [TODBCConnection], True));
    FConnectionOptions.Add(TBooleanOption.Create('VarBinaryAsBlob', prVarBinaryAsBlob, [TODBCConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TODBCConnection], False));
    FConnectionOptions.Add(TStringOption.Create('DriverManager', prDriverManager, [TODBCConnection], ODBCDLLName));
  end;
end;

procedure TODBCUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TODBCCommand], 0));
  end;
end;

procedure TODBCUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TODBCRecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TODBCRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TODBCRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TODBCRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('UnknownAsString', prUnknownAsString, [TODBCRecordSet], False));
  end;
end;

{ TODBCFormatter }

constructor TODBCFormatter.Create;
begin
  inherited;

  FFunctions := ODBCFunctions;
  FPredefinedMacros := ODBCMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TODBCUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TODBCUniProvider);

  ODBCFunctions := TStrValueStringList.Create;
  ODBCFunctions.Add('USER', 'USER');
  ODBCFunctions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  ODBCFunctions.Add('LOCATE', 'POSSTR(%1:s, %0:s)');
  ODBCFunctions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  ODBCFunctions.Add('CONCAT', '%s || %s');
  ODBCFunctions.Add('CHAR', 'CHR(%s)');
  ODBCFunctions.Add('TRIM', 'TRIM(%s)');
  ODBCFunctions.Add('TRUNCATE', 'TRUNCATE(%s, %s)');
  ODBCFunctions.Add('CEILING', 'CEILING(%s)');
  ODBCFunctions.Add('UPPER',   'UCASE(%s)');
  ODBCFunctions.Add('LOWER',   'LCASE(%s)');
  // Date-time
  ODBCFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  ODBCFunctions.Add('YEAR', 'YEAR(%s)');
  ODBCFunctions.Add('MONTH', 'MONTH(%s)');
  ODBCFunctions.Add('DAY', 'DAY(%s)');
  //ODBCFunctions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //ODBCFunctions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  ODBCFunctions.Add('__DATE_TIME_LITERAL', 'CAST(%s AS TIMESTAMP)');
  ODBCFunctions.Add('__DATE_LITERAL', 'CAST(%s AS DATE)');
  ODBCFunctions.Add('__TIME_LITERAL', 'CAST(%s AS TIME)');
  // CONVERT functions
  ODBCFunctions.Add('TODATE', 'CAST(%s AS TIMESTAMP)');
  ODBCFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  ODBCFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  ODBCMacros := TStrValueStringList.Create;
  ODBCMacros.Add('PROVIDER', 'ODBC');
  ODBCMacros.Add('ODBC', '');
  // DataType macros
  ODBCMacros.Add('DATETIME', 'TIMESTAMP');
  ODBCMacros.Add('DOUBLE', 'DOUBLE');
  ODBCMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TODBCUniProvider);

  ODBCFunctions.Free;
  ODBCMacros.Free;

{$ENDIF}
{$ENDIF}

end.
