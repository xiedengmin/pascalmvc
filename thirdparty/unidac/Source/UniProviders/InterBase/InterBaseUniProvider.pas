
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I IbDac.inc}
{$IFDEF VER10P}
{$I InterBaseUniProvider.inc}
{$ENDIF}
unit InterBaseUniProvider;
{$ENDIF}

interface

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  SysUtils, Classes, TypInfo, DB,
  CRTypes, MemData, CRAccess, CRConnectionPool, CRParser, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  DBAccess, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  DAScript, {$IFNDEF STD}DADump,{$ENDIF} UniProvider,
{$IFNDEF UNIDACPRO}
  IBCServices, IBCClasses, IBCConnectionPool, IBCScriptProcessor,
  IBCParser, IBCDataTypeMap, IBCSQLGenerator;
{$ELSE}
  IBCServicesUni, IBCClassesUni, IBCConnectionPoolUni, IBCScriptProcessorUni,
  IBCParserUni, IBCDataTypeMapUni, IBCSQLGeneratorUni;
{$ENDIF}

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TInterBaseUniProvider = class(TUniProvider)
  private
  {$IFNDEF STD}
    procedure OnAssignLoaderAutoCommit(InternalObject: TObject; Value: variant);
  {$ENDIF}
    procedure OnAssignSQLAutoCommit(InternalObject: TObject; Value: variant);

  protected
    procedure CreateConnectionOptions; override;
    procedure CreateSQLOptions; override;
    procedure CreateDataSetOptions; override;
    procedure CreateScriptOptions; override;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; override;
  {$ENDIF}
    procedure CreateTransactionOptions; override;

  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    function IsDatabaseSupported: boolean; override;
    function IsPortSupported: boolean; override;
    function IsDataSetNeedTransaction: boolean; override;
    function IsInOutParamSupported: boolean; override;
    function NeedNativeParseSQL: boolean; override;

    function GetParserClass: TSQLParserClass; override;
    function GetConnectionParametersClass: TCRConnectionParametersClass; override;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; override;
    function GetConnectionClass: TCRConnectionClass; override;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; override;
    function GetDataSetServiceClass: TDADataSetServiceClass; override;
    function GetScriptProcessorClass: TDAScriptProcessorClass; override;
  {$IFNDEF STD}
    function GetAlerterClass: TCRAlerterClass; override;
    function GetDumpProcessorClass: TDADumpProcessorClass; override;
  {$ENDIF}
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; override;
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; override;
    function GetSqlFormatterClass: TUniSqlFormatterClass; override;
    function GetConverterManagerClass: TConverterManagerClass; override;
    function GetParamObjectClass(Param: TDAParam): TClass; override;
    function CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject; override;

    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
  end;

  TIBCConnectDialogService = class (TConnectDialogService)
  public
    function UseDatabaseHistory: boolean; override;
  end;

  TUniIBCScriptProcessor = class (TCustomIBCScriptProcessor)
  protected
    procedure SetAutoDDL(Value: boolean); override;
    procedure SetDatabase(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetRole(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetCharset(Connection: TCustomDAConnection; const Value: string); override;
    procedure SetSQLDialect(Connection: TCustomDAConnection; Value: integer); override;
    procedure CreateDatabase(Connection: TCustomDAConnection; const Params: string); override;
    procedure DropDatabase(Connection: TCustomDAConnection); override;
  end;

  TIBCSqlFormatter = class(TUniSqlFormatter)
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
  DAConsts, CRProps, CRVio,
{$IFNDEF STD}
  DALoader, UniLoader,
{$ENDIF}
  Uni, UniScript,
{$IFNDEF UNIDACPRO}
  IBCProps, IBCConnectionString;
{$ELSE}
  IBCPropsUni, IBCConnectionStringUni;
{$ENDIF}

var
  IBCFunctions, IBCMacros: TStrValueStringList;

{ TInterBaseUniProvider }

{$IFNDEF STD}
procedure TInterBaseUniProvider.OnAssignLoaderAutoCommit(InternalObject: TObject; Value: variant);
begin
  if (InternalObject <> nil) and (InternalObject is TUniLoader) then
    TDALoaderUtils.SetAutoCommit(TUniLoader(InternalObject), boolean(Value));
end;
{$ENDIF}

procedure TInterBaseUniProvider.OnAssignSQLAutoCommit(InternalObject: TObject; Value: variant);
begin
  if (InternalObject <> nil) and (InternalObject is TUniSQL) then
    TDBAccessUtils.SetAutoCommit(TUniSQL(InternalObject), boolean(Value));
end;

class function TInterBaseUniProvider.GetProviderName: string;
begin
  Result := 'InterBase';
end;

class function TInterBaseUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TIBCConnectionStringBuilder;
end;

function TInterBaseUniProvider.IsDatabaseSupported: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.IsPortSupported: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.IsDataSetNeedTransaction: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.IsInOutParamSupported: boolean;
begin
  Result := False;
end;

function TInterBaseUniProvider.NeedNativeParseSQL: boolean;
begin
  Result := True;
end;

function TInterBaseUniProvider.GetParserClass: TSQLParserClass;
begin
  Result := TIBCParser;
end;

function TInterBaseUniProvider.GetConnectionParametersClass: TCRConnectionParametersClass;
begin
  Result := TIBCConnectionParameters;
end;

function TInterBaseUniProvider.GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass;
begin
  Result := TIBCConnectionPoolManager;
end;

function TInterBaseUniProvider.GetConnectionClass: TCRConnectionClass;
begin
  Result := TGDSConnection;
end;

function TInterBaseUniProvider.GetServerEnumeratorClass: TCRServerEnumeratorClass;
begin
  Result := TIBCServerEnumerator;
end;

function TInterBaseUniProvider.GetDataSetServiceClass: TDADataSetServiceClass;
begin
  Result := TCustomIBCDataSetService;
end;

function TInterBaseUniProvider.GetScriptProcessorClass: TDAScriptProcessorClass;
begin
  Result := TUniIBCScriptProcessor;
end;

{$IFNDEF STD}

function TInterBaseUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TGDSAlerter;
end;

function TInterBaseUniProvider.GetDumpProcessorClass: TDADumpProcessorClass;
begin
  Result := TCustomIBCDumpProcessor;
end;

{$ENDIF}

function TInterBaseUniProvider.GetConnectDialogServiceClass: TConnectDialogServiceClass;
begin
  Result := TIBCConnectDialogService;
end;

function TInterBaseUniProvider.GetFieldTypeMapClass: TDAFieldTypeMapClass;
begin
  Result := TCustomIBCFieldTypeMap;
end;

function TInterBaseUniProvider.GetSqlFormatterClass: TUniSqlFormatterClass;
begin
  Result := TIBCSqlFormatter;
end;

function TInterBaseUniProvider.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := TIBCConverterManager;
end;

function TInterBaseUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  if Param.DataType in (BlobTypes + MemoTypes) then
    Result := TIBCBlob
  else
    Result := inherited GetParamObjectClass(Param);
end;

function TInterBaseUniProvider.CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject;
var
  Connection: TCustomDAConnection;
begin
  if Param.DataType in (BlobTypes + MemoTypes) then begin
    Connection := nil;
    if Param is TUniParam then
      Connection := TUniParam(Param).GetConnection;
    if Connection <> nil then
      Result := TIBCBlob.Create(TGDSConnection(TDBAccessUtils.GetIConnection(Connection)),
        TGDSTransaction(TDBAccessUtils.GetIConnection(Connection).GetInternalTransaction))
    else
      Result := TIBCBlob.Create(TGDSConnection(nil), TGDSTransaction(nil));
    TIBCBlob(Result).IsUnicode := IsUnicode;
  end
  else
    Result := inherited CreateParamObject(Param, IsUnicode);
end;

procedure TInterBaseUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TGDSConnection then begin
    TGDSConnection(Obj).SetProp(prSimpleNumericMap, True);
  end
{$IFNDEF STD}
  else
  if Obj is TUniLoader then
    GetLoaderOptions.ImportOptions(Options, Obj, nil)
{$ENDIF}
  else
  if Obj is TUniSQL then
    GetSQLOptions.ImportOptions(Options, Obj, nil);

  inherited;
end;

procedure TInterBaseUniProvider.CreateConnectionOptions;
begin
  if FConnectionOptions = nil then begin
    FConnectionOptions := TOptionsList.Create(GetProviderName);
    FConnectionOptions.Add(TIntegerOption.Create('CharLength', prCharLength, [TGDSConnection], 0));
    FConnectionOptions.Add(TStringOption.Create('Charset', prCharset, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('Role', prRole, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TBooleanOption.Create('UseUnicode', prUseUnicode, [TGDSConnection, TIBCConnectionParameters], False));
    FConnectionOptions.Add(TIntegerOption.Create('SQLDialect', prSQLDialect, [TGDSConnection, TIBCConnectionParameters], 3));
    FConnectionOptions.Add(TStringOption.Create('ClientLibrary', prClientLibrary, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('Protocol', prProtocol, [TGDSConnection, TIBCConnectionParameters], Variant(_TCP), TypeInfo(_TIBCProtocol)));
    FConnectionOptions.Add(TBooleanOption.Create('TrustedAuthentication', prTrustedAuthentication, [TGDSConnection, TIBCConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('NoDBTriggers', prNoDBTriggers, [TGDSConnection, TIBCConnectionParameters], False));
    FConnectionOptions.Add(TBooleanOption.Create('SimpleNumericMap', prSimpleNumericMap, [TGDSConnection, TIBCConnectionParameters], True));
    FConnectionOptions.Add(TBooleanOption.Create('ForceUnloadClientLibrary', prForceUnloadClientLibrary, [TGDSConnection], False));
    FConnectionOptions.Add(TBooleanOption.Create('EnableMemos', prEnableMemos, [TGDSConnection], False));
    FConnectionOptions.Add(TStringOption.Create('Params', prConnectionParams, [TGDSConnection], ''));
    FConnectionOptions.Add(TEnumeratorOption.Create('IPVersion', prIPVersion, [TGDSConnection, TIBCConnectionParameters], Variant(ivIPBoth), TypeInfo(TIPVersion)));
    // SSL
    FConnectionOptions.Add(TBooleanOption.Create('UseSSL', prUseSSL, [TGDSConnection, TIBCConnectionParameters], False));
    FConnectionOptions.Add(TStringOption.Create('SSLServerPublicFile', prServerPublicFile, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLServerPublicPath', prServerPublicPath, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLClientCertFile', prClientCertFile, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLClientPassPhraseFile', prClientPassPhraseFile, [TGDSConnection, TIBCConnectionParameters], ''));
    FConnectionOptions.Add(TStringOption.Create('SSLClientPassPhrase', prClientPassPhrase, [TGDSConnection, TIBCConnectionParameters], ''));
  end;
end;

procedure TInterBaseUniProvider.CreateSQLOptions;
begin
  if FSQLOptions = nil then begin
    FSQLOptions := TOptionsList.Create(GetProviderName);
    FSQLOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TGDSCommand], False));
    FSQLOptions.Add(TBooleanOption.Create('AutoCommit', prAutoCommit, [TUniSQL], False, OnAssignSQLAutoCommit));
  end;
end;

procedure TInterBaseUniProvider.CreateDataSetOptions;
begin
  if FDataSetOptions = nil then begin
    FDataSetOptions := TOptionsList.Create(GetProviderName);
    FDataSetOptions.Add(TBooleanOption.Create('FetchAll', prFetchAll, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('AutoClose', prAutoClose, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('FieldsAsString', prFieldsAsString, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredBlobRead', prDeferredBlobRead, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('CacheBlobs', prCacheBlobs, [TGDSCommand], True));
    FDataSetOptions.Add(TBooleanOption.Create('StreamedBlobs', prStreamedBlobs, [TGDSCommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('ComplexArrayFields', prComplexArrayFields, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('DeferredArrayRead', prDeferredArrayRead, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('CacheArrays', prCacheArrays, [TGDSCommand], True));
    FDataSetOptions.Add(TStringOption.Create('KeyGenerator', prKeySequence, [TCustomIBCDataSetService], ''));
    FDataSetOptions.Add(TEnumeratorOption.Create('GeneratorMode', prGeneratorMode, [TCustomIBCDataSetService], Variant(_gmPost), TypeInfo(_TGeneratorMode)));
    FDataSetOptions.Add(TStringOption.Create('GeneratorStep', prGeneratorStep, [TCustomIBCDataSetService], 1));
    FDataSetOptions.Add(TBooleanOption.Create('BooleanDomainFields', prBooleanDomainFields, [TGDSRecordSet], True));
    FDataSetOptions.Add(TBooleanOption.Create('DescribeParams', prUseDescribeParams, [TGDSCommand], False));
    FDataSetOptions.Add(TBooleanOption.Create('SetDomainNames', prSetDomainNames, [TGDSRecordSet], False));
    FDataSetOptions.Add(TBooleanOption.Create('AutoCommit', prAutoCommit, [TCustomIBCDataSetService], True));
    FDataSetOptions.Add(TBooleanOption.Create('QueryRowsAffected', prQueryRowsAffected, [TGDSRecordSet], True));
  end;
end;

procedure TInterBaseUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then begin
    FScriptOptions := TOptionsList.Create(GetProviderName);
    FScriptOptions.Add(TBooleanOption.Create('AutoDDL', prAutoDDL, [TCustomIBCScriptProcessor], True));
  end;
end;

{$IFNDEF STD}
procedure TInterBaseUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then begin
    FLoaderOptions := TOptionsList.Create(GetProviderName);
    FLoaderOptions.Add(TEnumeratorOption.Create('InsertMode', prInsertMode, [TGDSLoader], Variant(_imInsert), TypeInfo(_TIBCInsertMode)));
    FLoaderOptions.Add(TIntegerOption.Create('RowsPerBatch', prRowsPerBatch, [TGDSLoader], 50));
    FLoaderOptions.Add(TBooleanOption.Create('AutoCommit', prAutoCommit, [TUniLoader], True, OnAssignLoaderAutoCommit));
    FLoaderOptions.Add(TBooleanOption.Create('QuoteNames', prQuoteNames, [TGDSLoader], False));
  end;
end;
{$ENDIF}

procedure TInterBaseUniProvider.CreateTransactionOptions;
begin
  if FTransactionOptions = nil then begin
    FTransactionOptions := TOptionsList.Create(GetProviderName);
    FTransactionOptions.Add(TStringOption.Create('Params', prTransactionParams, [TGDSTransaction], ''));
  end;
end;

{ TIBCConnectDialogService }

function TIBCConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := True;
end;

{ TUniIBCScriptProcessor }

procedure TUniIBCScriptProcessor.SetAutoDDL(Value: boolean);
begin
  TUniScript(FOwner).SpecificOptions.Values['InterBase.AutoDDL'] := BoolToStr(Value, True);
end;

procedure TUniIBCScriptProcessor.SetDatabase(Connection: TCustomDAConnection; const Value: string);
var
  Host, FileName, PortSN: string;
  Protocol: _TIBCProtocol;
begin
  if GetDBNamePos(Value) > 0 then begin
    ParseDatabaseName(Value, Host, PortSN, Protocol, FileName, False);
    Connection.Server := Host;
    TUniConnection(Connection).SpecificOptions.Values['InterBase.Protocol'] :=
      Copy(GetEnumName(TypeInfo(_TIBCProtocol), Integer(Protocol)), 2, MaxInt);
  end
  else
    FileName := Value;

  TUniConnection(Connection).Database := FileName;
end;

procedure TUniIBCScriptProcessor.SetRole(Connection: TCustomDAConnection; const Value: string);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.Role'] := Value;
end;

procedure TUniIBCScriptProcessor.SetCharset(Connection: TCustomDAConnection; const Value: string);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.Charset'] := Value;
end;

procedure TUniIBCScriptProcessor.SetSQLDialect(Connection: TCustomDAConnection; Value: integer);
begin
  TUniConnection(Connection).SpecificOptions.Values['InterBase.SQLDialect'] := IntToStr(Value);
end;

procedure TUniIBCScriptProcessor.CreateDatabase(Connection: TCustomDAConnection; const Params: string);
var
  ICon: TGDSConnection;
  List: TStringList;
  IsPooling: boolean;
begin
  IsPooling := Connection.Pooling;
  if IsPooling then
    Connection.Pooling := False;
  try
    TDBAccessUtils.CreateIConnection(Connection);
    ICon := TGDSConnection(TDBAccessUtils.GetIConnection(Connection));
    ICon.SetServer(Connection.Server);
    List := TStringList.Create;
    try
      List.Text := Params;
      ICon.SetParams(List);
    finally
      List.Free;
    end;
    ICon.CreateDatabase;
  finally
    if IsPooling then
      Connection.Pooling := True;
  end;
end;

procedure TUniIBCScriptProcessor.DropDatabase(Connection: TCustomDAConnection);
var
  ICon: TGDSConnection;
begin
  if not Connection.Connected then
    DatabaseError(SConnectionIsClosed);

  TDBAccessUtils.DisconnectTransaction(Connection);

  ICon := TGDSConnection(TDBAccessUtils.GetIConnection(Connection));
  ICon.DropDatabase;
end;

{ TIBCSqlFormatter }

constructor TIBCSqlFormatter.Create;
begin
  inherited;

  FFunctions := IBCFunctions;
  FPredefinedMacros := IBCMacros;
end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TInterBaseUniProvider]);
end;

initialization
  UniProviders.RegisterProvider(TInterBaseUniProvider);

  IBCFunctions := TStrValueStringList.Create;
  IBCFunctions.Add('USER', 'USER');
  IBCFunctions.Add('CHAR_LENGTH', 'strlen(%s)');
  //IBCFunctions.Add('LOCATE', '');
  IBCFunctions.Add('SUBSTRING', 'substring(%s from %s for %s)');
  IBCFunctions.Add('CONCAT', '%s || %s');
  IBCFunctions.Add('CHAR', 'ascii_char(%s)');
  IBCFunctions.Add('TRIM', 'TRIM(%s)');
  IBCFunctions.Add('TRUNCATE', 'TRUNCATE(%s)');
  IBCFunctions.Add('CEILING', 'CEILING(%s)');
  // Date-time
  IBCFunctions.Add('CURRENT_DATE', 'CURRENT_DATE');
  IBCFunctions.Add('YEAR', 'EXTRACT(YEAR FROM %s)');
  IBCFunctions.Add('MONTH', 'EXTRACT(MONTH FROM %s)');
  IBCFunctions.Add('DAY', 'EXTRACT(DAY FROM %s)');
  // Date-time literals
  IBCFunctions.Add('__DATE_TIME_LITERAL', 'CAST(%s AS TIMESTAMP)');
  IBCFunctions.Add('__DATE_LITERAL', 'CAST(%s AS DATE)');
  IBCFunctions.Add('__TIME_LITERAL', 'CAST(%s AS TIME)');
  // CONVERT functions
  IBCFunctions.Add('TODATE', 'CAST(%s AS TIMESTAMP)');
  IBCFunctions.Add('TONUMBER', 'CAST(%s AS DOUBLE PRECISION)');
  IBCFunctions.Add('TOCHAR', 'CAST(%s AS VARCHAR(24))');

  IBCMacros := TStrValueStringList.Create;
  IBCMacros.Add('PROVIDER', 'InterBase');
  IBCMacros.Add('INTERBASE', '');
  // DataType macros
  IBCMacros.Add('DATETIME', 'TIMESTAMP');
  IBCMacros.Add('DOUBLE', 'DOUBLE PRECISION');
  IBCMacros.Add('VARCHAR', 'VARCHAR');

finalization
  UniProviders.UnRegisterProvider(TInterBaseUniProvider);

  IBCFunctions.Free;
  IBCMacros.Free;

end.

