
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
{$IFDEF VER10P}
{$I MongoDBUniProvider.inc}
{$ENDIF}
unit MongoDBUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRAccess, CRConnectionPool, CRDataTypeMap,  CRTypes, CRParser, CRConnectionString, CRServerEnumerator,
  MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, DBAccess,
  DAScript, {$IFNDEF STD}DADump,{$ENDIF}
  Uni, UniProvider,
{$IFNDEF UNIDACPRO}
  MongoClasses, MongoObjects;
{$ELSE}
  MongoClassesUni, MongoObjectsUni;
{$ENDIF}

type
{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFNDEF STD} or pidOSX32{$IFDEF VER18P} or pidiOSSimulator{$IFNDEF VER22P} or pidiOSDevice{$ELSE} or pidiOSDevice32 or pidiOSDevice64{$ENDIF}{$IFDEF VER25P} or pidLinux64{$IFDEF VER26P} or pidOSX64{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF})]
{$ENDIF}
  TMongoDBUniProvider = class(TUniProvider)
  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsServerSupported: boolean; override;
    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsParamsSupported: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function IsPoolingSupported: boolean; override;
    function IsStoredProcSupported: boolean; override;
    function IsUpdateSQLSupported: boolean; override;
    function IsMacrosSupported: boolean; override;
    function NeedComplexUpdateFieldDefList: boolean; override;

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

    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
  end;

  TMongoFormatter = class(TUniSqlFormatter)
  protected
  public
    constructor Create; override;

    procedure Expand(var SQL: string); override;
  end;

  TMongoFieldTypeMap = class(TDAFieldTypeMap)
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  CRProps, DAConsts, UniConsts,
{$IFNDEF STD}
  DALoader, UniLoader,
{$ENDIF}
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoProps, MongoConnectionPool, MongoServices, MongoDataTypeMap, MongoConnectionString;
{$ELSE}
  MongoConstsUni, MongoPropsUni, MongoConnectionPoolUni, MongoServicesUni, MongoDataTypeMapUni, MongoConnectionStringUni;
{$ENDIF}

{ TMongoDBUniProvider }

class function TMongoDBUniProvider.GetProviderName: string;
begin
  Result := 'MongoDB';
end;

class function TMongoDBUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TMongoConnectionStringBuilder;
end;

function TMongoDBUniProvider.IsServerSupported: boolean;
begin
  Result := True;
end;

function TMongoDBUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TMongoDBUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TMongoDBUniProvider.IsParamsSupported: boolean;
begin
  Result := False;
end;

function TMongoDBUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TMongoDBUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TMongoDBUniProvider.IsStoredProcSupported: boolean;
begin
  Result := False;
end;

function TMongoDBUniProvider.IsUpdateSQLSupported: boolean;
begin
  Result := False;
end;

function TMongoDBUniProvider.IsMacrosSupported: boolean;
begin
  Result := False;
end;

function TMongoDBUniProvider.NeedComplexUpdateFieldDefList: boolean;
begin
  Result := True;
end;

function TMongoDBUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TMongoParser;
end;

function TMongoDBUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TMongoConnectionParameters;
end;

function TMongoDBUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TMongoConnectionPoolManager;
end;

function TMongoDBUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TMongoConverterManager;
end;

function TMongoDBUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TMongoConnector;
end;

function TMongoDBUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TCRServerEnumerator;
end;

function TMongoDBUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TMongoDataSetService;
end;

function TMongoDBUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TMongoScriptProcessor;
end;

{$IFNDEF STD}

function TMongoDBUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TMongoDumpProcessor;
end;

{$ENDIF}

function TMongoDBUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TConnectDialogService;
end;

function TMongoDBUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TMongoFieldTypeMap;
end;

function TMongoDBUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TMongoFormatter;
end;

procedure TMongoDBUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
{$IFNDEF STD}
  if Obj is TUniLoader then
    GetLoaderOptions.ImportOptions(Options, Obj, nil);
{$ENDIF}

  inherited;
end;

procedure TMongoDBUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TStringOption.Create('AdditionalServers', prAdditionalServers, [TMongoConnector, TMongoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('BSONLibrary', prBSONLibrary, [TMongoConnector, TMongoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ClientLibrary', prClientLibrary, [TMongoConnector, TMongoConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('ConnectionOptions', prConnectionOptions, [TMongoConnector, TMongoConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('LowerCaseObjectId', prLowercaseObjectId, [TMongoConnector, TMongoConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('SQLEngine', prSQLEngine, [TMongoConnector, TMongoConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TMongoConnector, TMongoConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('DescribeAmount', prDescribeAmount, [TMongoConnector, TMongoConnectionParameters], 25));
  end;
end;

procedure TMongoDBUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
  end;
end;

procedure TMongoDBUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('AllowAddField', prImplicitAddFields, [TMongoRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('AllowChangeType', prImplicitChangeType, [TMongoRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('ComplexAsString', prComplexAsString, [TMongoRecordSet], False));
    FDataSetOptions.Add(TIntegerOption.Create('DescribeAmount', prDescribeAmount, [TMongoRecordSet], 25));
    FDataSetOptions.Add(TEnumeratorOption.Create('DescribeMethod', prDescribeMethod, [TMongoRecordSet], Variant(dmGrid), TypeInfo(TDescribeMethod)));
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TMongoRecordSet], True));
  end;
end;

procedure TMongoDBUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
  end;
end;

{$IFNDEF STD}
procedure TMongoDBUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
  end;
end;
{$ENDIF}

{ TMongoFormatter }

constructor TMongoFormatter.Create;
begin
  inherited;
end;

procedure TMongoFormatter.Expand(var SQL: string);
begin
  // do nothing
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TMongoDBUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TMongoDBUniProvider);

finalization
  UniProviders.UnRegisterProvider(TMongoDBUniProvider);

end.
