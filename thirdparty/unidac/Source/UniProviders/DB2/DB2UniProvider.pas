
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DB2Dac.inc}
{$IFDEF VER10P}
{$I DB2UniProvider.inc}
{$ENDIF}
unit DB2UniProvider;

interface

uses
  SysUtils, Classes, Variants, DB, CRAccess, CRConnectionPool, CRConnectionString, CRServerEnumerator,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  CRTypes, CRParser, CRDataTypeMap,
  DAScript, UniProvider, ODBCUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TDB2UniProvider = class(TODBCUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
  end;

  TDB2Formatter = class(TUniSqlFormatter)
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
  ODBCProps, ODBCClasses, ODBCParser, ODBCServices,
  DB2Props, DB2Classes, DB2Services, DB2ConnectionPool, DB2DataTypeMap, DB2ConnectionString;
{$ELSE}
  ODBCPropsUni, ODBCClassesUni, ODBCParserUni, ODBCServicesUni,
  DB2PropsUni, DB2ClassesUni, DB2ServicesUni, DB2ConnectionPoolUni, DB2DataTypeMapUni, DB2ConnectionStringUni;
{$ENDIF}

var
  DB2Functions, DB2Macros: TStrValueStringList;

class function TDB2UniProvider.GetProviderName: string;
begin
  Result := 'DB2';
end;

class function TDB2UniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TDB2ConnectionStringBuilder;
end;

function TDB2UniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TDB2UniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TDB2UniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TDB2ConnectionParameters;
end;

function TDB2UniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TDB2ConnectionPoolManager;
end;

function TDB2UniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TDB2Connection;
end;

function TDB2UniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TDB2ServerEnumerator;
end;

function TDB2UniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomDB2DataSetService;
end;

function TDB2UniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TDB2ScriptProcessor;
end;

function TDB2UniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TDB2Formatter;
end;

function TDB2UniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TDB2ConverterManager;
end;

procedure TDB2UniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TBooleanOption.Create('ColumnWiseBinding', prColumnWiseBinding, [TODBCConnection], False));
    FConnectionOptions.Add(TIntegerOption.Create('ConnectionTimeout', prConnectionTimeout, [TDB2Connection, TDB2ConnectionParameters], 15));
    FConnectionOptions.Add(TStringOption.Create('FunctionPath', prFunctionPath, [TDB2Connection, TDB2ConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('Schema', prSchema, [TDB2Connection, TDB2ConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TDB2Connection], False));
  end;
end;

procedure TDB2UniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDB2Command], 0));
  end;
end;

procedure TDB2UniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TIntegerOption.Create('CommandTimeout', prCommandTimeout, [TDB2RecordSet], 0));
    FDataSetOptions.Add(TBooleanOption.Create('ExtendedFieldsInfo', prExtendedFieldsInfo, [TDB2RecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TDB2RecordSet], False));
    FDataSetOptions.Add(TStringOption.Create('KeySequence', prKeySequence, [TCustomDB2DataSetService], ''));
    FDataSetOptions.Add(TEnumeratorOption.Create('SequenceMode', prSequenceMode, [TCustomDB2DataSetService], Variant(smPost), TypeInfo(TDB2SequenceMode)));
  end;
end;

{ TDB2Formatter }

constructor TDB2Formatter.Create;
begin
  inherited;

  FFunctions := DB2Functions;
  FPredefinedMacros := DB2Macros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TDB2UniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TDB2UniProvider);

  DB2Functions := TStrValueStringList.Create;
  DB2Functions.Add('USER', 'USER');
  DB2Functions.Add('CHAR_LENGTH', 'LENGTH(%s)');
  DB2Functions.Add('LOCATE', 'POSSTR(%1:s, %0:s)');
  DB2Functions.Add('SUBSTRING', 'SUBSTR(%s, %s, %s)');
  DB2Functions.Add('CONCAT', '%s || %s');
  DB2Functions.Add('CHAR', 'CHR(%s)');
  DB2Functions.Add('TRIM', 'TRIM(%s)');
  DB2Functions.Add('TRUNCATE', 'TRUNCATE(%s, %s)');
  DB2Functions.Add('CEILING', 'CEILING(%s)');
  DB2Functions.Add('UPPER',       'UPPER(%s)');
  DB2Functions.Add('LOWER',       'LOWER(%s)');
  // Date-time
  DB2Functions.Add('CURRENT_DATE', 'CURRENT_DATE');
  DB2Functions.Add('YEAR', 'YEAR(%s)');
  DB2Functions.Add('MONTH', 'MONTH(%s)');
  DB2Functions.Add('DAY', 'DAY(%s)');
  //DB2Functions.Add('DATEADD', 'DATE(%2:s, ''%1:s %0:s'')');
  //DB2Functions.Add('DATEDIFF', 'EXTRACT(%s FROM (%2:s - %1:s))');
  // Date-time literals
  DB2Functions.Add('__DATE_TIME_LITERAL', 'CAST(%s AS TIMESTAMP)');
  DB2Functions.Add('__DATE_LITERAL', 'CAST(%s AS DATE)');
  DB2Functions.Add('__TIME_LITERAL', 'CAST(%s AS TIME)');
  // CONVERT functions
  DB2Functions.Add('TODATE', 'CAST(%s AS TIMESTAMP)');
  DB2Functions.Add('TONUMBER', 'CAST(%s AS DOUBLE)');
  DB2Functions.Add('TOCHAR', 'CAST(%s AS VARCHAR)');

  DB2Macros := TStrValueStringList.Create;
  DB2Macros.Add('PROVIDER', 'DB2');
  DB2Macros.Add('DB2', '');
  // DataType macros
  DB2Macros.Add('DATETIME', 'TIMESTAMP');
  DB2Macros.Add('DOUBLE', 'DOUBLE');
  DB2Macros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TDB2UniProvider);

  DB2Functions.Free;
  DB2Macros.Free;

end.
