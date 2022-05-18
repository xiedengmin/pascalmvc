
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniProvider;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
  Generics.Collections,
{$ENDIF}
  SysUtils, Classes, Variants, TypInfo, DB, 
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF}, 
  CRTypes, CRParser, CRAccess, CRConnectionPool, CRDataTypeMap, CRConnectionString, CRServerEnumerator,
  MemData, DBAccess, DAScript, DADump, DALoader, DAAlerter;

type
  TOptionsList = class;
  TConnectDialogService = class;
  TUniSqlFormatter = class;

  TConnectDialogServiceClass = class of TConnectDialogService;
  TUniSqlFormatterClass = class of TUniSqlFormatter;

  TUniProvider = class(TComponent)
  protected
    FConnectionOptions: TOptionsList;
    FSQLOptions: TOptionsList;
    FDataSetOptions: TOptionsList;
    FScriptOptions: TOptionsList;
    FLoaderOptions: TOptionsList;
    FDumpOptions: TOptionsList;
    FAlerterOptions: TOptionsList;
    FTransactionOptions: TOptionsList;

    procedure CreateConnectionOptions; virtual; abstract;
    procedure CreateSQLOptions; virtual; abstract;
    procedure CreateDataSetOptions; virtual; abstract;
    procedure CreateScriptOptions; virtual;
  {$IFNDEF STD}
    procedure CreateLoaderOptions; virtual;
    procedure CreateDumpOptions; virtual;
    procedure CreateAlerterOptions; virtual;
  {$ENDIF}
    procedure CreateTransactionOptions; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetProviderName: string; virtual; // CLR does not support abstract class methods
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; virtual;

    function IsServerSupported: boolean; virtual;
    function IsDatabaseSupported: boolean; virtual;
    function IsPortSupported: boolean; virtual;
    function IsDataSetNeedTransaction: boolean; virtual;
    function IsParamsSupported: boolean; virtual;
    function IsInOutParamSupported: boolean; virtual;
    function IsPoolingSupported: boolean; virtual;
    function IsStoredProcSupported: boolean; virtual;
    function IsUpdateSQLSupported: boolean; virtual;
    function IsMacrosSupported: boolean; virtual;
    function NeedRecreateProcCall: boolean; virtual;
    function NeedComplexUpdateFieldDefList: boolean; virtual;
    function NeedNativeParseSQL: boolean; virtual;
    function NeedBlobUnicode(Param: TDAParam): boolean; virtual;

    function GetConnectionParametersClass: TCRConnectionParametersClass; virtual; abstract;
    function GetConnectionPoolingManagerClass: TCRConnectionPoolManagerClass; virtual; abstract;
    function GetConnectionClass: TCRConnectionClass; virtual; abstract;
    function GetServerEnumeratorClass: TCRServerEnumeratorClass; virtual; abstract;
    function GetParserClass: TSQLParserClass; virtual; abstract;
    function GetDataSetServiceClass: TDADataSetServiceClass; virtual; abstract;
    function GetScriptProcessorClass: TDAScriptProcessorClass; virtual; abstract;
  {$IFNDEF STD}
    function GetAlerterClass: TCRAlerterClass; virtual;
    function GetDumpProcessorClass: TDADumpProcessorClass; virtual; abstract;
  {$ENDIF}
    function GetFieldTypeMapClass: TDAFieldTypeMapClass; virtual; abstract;
    function GetConnectDialogServiceClass: TConnectDialogServiceClass; virtual; abstract;
    function GetSqlFormatterClass: TUniSqlFormatterClass; virtual; abstract;
    function GetConverterManagerClass: TConverterManagerClass; virtual; abstract;
    function GetParamObjectClass(Param: TDAParam): TClass; virtual;
    function CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject; virtual;
    function CheckParamName(const ParamName: string): string; virtual;

    function GetComponentOptionsList(Component: TComponent): TOptionsList;
    procedure SetObjectProps(Obj: TObject; Options: TStrings); virtual;

    function GetConnectionOptions: TOptionsList;
    function GetSQLOptions: TOptionsList;
    function GetDataSetOptions: TOptionsList;
    function GetScriptOptions: TOptionsList;
  {$IFNDEF STD}
    function GetLoaderOptions: TOptionsList;
    function GetDumpOptions: TOptionsList;
    function GetAlerterOptions: TOptionsList;
  {$ENDIF}
    function GetTransactionOptions: TOptionsList; virtual;

    function DefaultTableSchema: string; virtual;
    function ValidateOption(Owner: TComponent; const Prefix, Name, Value: string): string; overload;
    function ValidateOption(Owner: TComponent; const Prefix, Name: string): string; overload;
  end;

  TUniProviderClass = class of TUniProvider;

  TUniProviderDesc = class
  private
    FProviderName: string;
    FProviderShortName: string;
    FPackageName: string;
    FAssemblyName: string;
    FSiblingProduct: string;

    FProvider: TUniProvider;

    function GetUnitName: string;
    function GetUnitNameCLR: string;
    function GetProviderComponentName: string;
  public
    property ProviderName: string read FProviderName;
//    property ProviderShortName: string read FProviderShortName;

    property PackageName: string read FPackageName;
    property AssemblyName: string read FAssemblyName;

    property SiblingProduct: string read FSiblingProduct;

    property ProviderUnitName: string read GetUnitName;
    property ProviderUnitNameCLR: string read GetUnitNameCLR;
    property ProviderComponentName: string read GetProviderComponentName;

    property Provider: TUniProvider read FProvider;
  end;

{$IFDEF AUTOREFCOUNT}
  TUniProviders = class(TThreadList<TUniProviderDesc>)
{$ELSE}
  TUniProviders = class(TThreadList)
{$ENDIF}
  private
    function FindProviderDesc(ProviderName: string): TUniProviderDesc;
    procedure RegisterProviderDesc(ProviderName, ProviderShortName,
      PackageName, AssemblyName, SiblingProduct: string);
  public
    destructor Destroy; override;

    procedure RegisterProvider(UniProviderClass: TUniProviderClass);
    procedure UnRegisterProvider(UniProviderClass: TUniProviderClass);

    function GetProviderDesc(ProviderName: string): TUniProviderDesc;
    function GetProvider(ProviderName: string): TUniProvider;

    procedure GetProviderNames(Names: TStrings);
  end;

  TConnectDialogService = class
  public
    constructor Create; virtual;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    function GetConnectMode: integer; virtual;
    function UseDatabaseHistory: boolean; virtual;
    function GetDefaultDatabase: string; virtual;
    function UsernameEnabled: boolean; virtual;
    function PasswordEnabled: boolean; virtual;
    function ServerEnabled: boolean; virtual;
    function DatabaseEnabled: boolean; virtual;
    function PortEnabled: boolean; virtual;
  end;

  TCRDummyAlerter = class(TCRAlerter)
  public
    procedure SendEvent(const EventName, Message: string); override;
    procedure Start; override;
    procedure Stop; override;
  end;

  TOnAssignValue = procedure(InternalObject: TObject; Value: variant) of object;
  TSetPropFunc = function(Prop: integer; const Value: variant): boolean of object;
  TOnGetValuesList = procedure(List: TStrings) of object;
  TClassArray = array of TClass;

  TOption = class
  private
    FOptionName: string;
    FInternalIndex: integer;
    FInternalClasses: TClassArray;
    FDefaultValue: variant;
    FOnGetValuesList: TOnGetValuesList;
    FOnAssignValue: TOnAssignValue;
  protected

    procedure ValidationError(const Value: string);

    procedure InternalGetValuesList(List: TStrings); virtual;
  public
    constructor Create(const OptionName: string; InternalIndex: integer; InternalClasses: array of TClass;
      DefaultValue: variant; OnAssign: TOnAssignValue = nil); overload;

    function GetDefaultValue: variant;

    function GetAsString(const Value: variant): string; virtual;
    function GetAsNative(const Value: string): variant; virtual;

    function CheckValue(const Value: string): boolean; virtual;
    procedure Validate(const Value: string); virtual;
    procedure GetValuesList(List: TStrings); virtual;

    property OptionName: string read FOptionName;
    property InternalIndex: integer read FInternalIndex;
    property InternalClasses: TClassArray read FInternalClasses;

    property OnGetValuesList: TOnGetValuesList read FOnGetValuesList write FOnGetValuesList;
    property OnAssignValue: TOnAssignValue read FOnAssignValue write FOnAssignValue;
  end;

  TIntegerOption = class(TOption)
  public
    function GetAsString(const Value: variant): string; override;
    function GetAsNative(const Value: string): variant; override;

    function CheckValue(const Value: string): boolean; override;
  end;

  TStringOption = class(TOption)
  end;

  TBooleanOption = class(TOption)
  protected
    procedure InternalGetValuesList(List: TStrings); override;
  public
    function GetAsString(const Value: variant): string; override;
    function GetAsNative(const Value: string): variant; override;

    function CheckValue(const Value: string): boolean; override;
  end;

  TEnumeratorOption = class(TOption)
  private
    FTypeInfo: PTypeInfo;
    FMinValue: integer;
    FMaxValue: integer;
    FInternalType: boolean;
  protected
    procedure InternalGetValuesList(List: TStrings); override;
  public
    constructor Create(const OptionName: string; InternalIndex: integer; InternalClasses: array of TClass; DefaultValue: variant; TypeInfo: PTypeInfo);

    function GetAsString(const Value: variant): string; override;
    function GetAsNative(const Value: string): variant; override;

    function CheckValue(const Value: string): boolean; override;
  end;

  TOptionsList = class(TStringList)
  private
    FPrefix: string;
    FDeprecatedOptions: TStringList;

    function GetOption(Index: integer): TOption;
    procedure SetOption(Index: integer; const Value: TOption);
  public
    constructor Create(const Prefix: string);
    destructor Destroy; override;

    procedure Add(Value: TOption); reintroduce;
    procedure AddDeprecated(const DeprecatedName: string; Value: TOption);
    procedure Remove(const Name: string);

    function OptionByCode(Code: Integer): TOption;
    function OptionByName(const Name: string): TOption;
    function OptionByDeprecatedName(const Name: string): TOption;

    procedure ImportOptions(Source: TStrings; DestObject: TObject; SetPropFunc: TSetPropFunc);
    procedure ExportDefOptions(Dest: TStrings);

    function GetValueByName(Source: TStrings; const Name: string): variant;

    property Prefix: string read FPrefix;
    property Items[Index: integer]: TOption read GetOption write SetOption; default;
  end;

  TUniSqlFormatter = class
  protected
    FFunctions: TStrValueStringList;
    FPredefinedMacros: TStrValueStringList;
    FUserMacroNames: TStringList;
    FUserMacroValues: TStringList;
    FParserClass: TSQLParserClass;

    function IsServerKeywordLexem(Parser: TSQLParser; const Lexem: string): Boolean; virtual;
    function IsDefinedMacro(const MacroName: string): boolean;
    function Parse(Parser: TSQLParser; const EndChar: string = ''): string;
    function ProcessFunction(const Body: string): string;
    function GetFunction(const FunctionName: string; const Params: TStringArray): string; virtual;
    function ProcessDate(const Body: string): string;
    function ProcessTime(const Body: string): string;
    function ProcessTimestamp(const Body: string): string;
    function ProcessMacro(const MacroName, Body: string): string;
    function NeedUnquote: boolean; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LeftQuote: char; virtual;
    function RightQuote: char; virtual;

    procedure SetUserMacros(Names, Values: TStringList);
    function CheckIfCondition(const Body: string): boolean;
    procedure Expand(var SQL: string); virtual;
    procedure SetParserClass(Value: TSQLParserClass);
  end;

  procedure FillOptionsList(const OptionPrefix: string; OptionsList: TOptionsList; List: TStrings);
  procedure GetOptionValuesList(const OptionName: string; OptionsList: TOptionsList; List: TStrings);
  procedure ExtractOption(const Str: string; var OptionPrefix, OptionName, OptionValue: string);
  procedure WriteOptions(OptionsList: TOptionsList; List: TStrings; DestClass: TClass; SetPropFunc: TSetPropFunc);
  procedure SetSpecificOption(Connection: TCustomDAConnection; const Name, Value: string);
  procedure CheckProviderName(const ProviderName: string);

var
  UniProviders: TUniProviders;
  
implementation

uses
  CRFunctions, DAConsts, UniConsts, Uni;

{ TUniProvider }

constructor TUniProvider.Create(AOwner: TComponent);
begin
  inherited;

  CreateConnectionOptions;
  CreateSQLOptions;
  CreateDataSetOptions;
  CreateScriptOptions;
{$IFNDEF STD}
  CreateLoaderOptions;
  CreateDumpOptions;
  CreateAlerterOptions;
{$ENDIF}
  CreateTransactionOptions;
end;

destructor TUniProvider.Destroy;
begin
  FConnectionOptions.Free;
  FSQLOptions.Free;
  FDataSetOptions.Free;
  FScriptOptions.Free;
  FLoaderOptions.Free;
  FDumpOptions.Free;
  FAlerterOptions.Free;
  FTransactionOptions.Free;

  inherited;
end;

class function TUniProvider.GetProviderName: string;
begin
  Assert(False);
  Result := '';
end;

class function TUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Assert(False);
  Result := nil;
end;

function TUniProvider.IsServerSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsDatabaseSupported: boolean;
begin
  Result := False;
end;

function TUniProvider.IsPortSupported: boolean;
begin
  Result := False;
end;

function TUniProvider.IsDataSetNeedTransaction: boolean;
begin
  Result := False;
end;

function TUniProvider.IsParamsSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsInOutParamSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsPoolingSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsStoredProcSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsUpdateSQLSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.IsMacrosSupported: boolean;
begin
  Result := True;
end;

function TUniProvider.NeedRecreateProcCall: boolean;
begin
  Result := False;
end;

function TUniProvider.NeedComplexUpdateFieldDefList: boolean;
begin
  Result := False;
end;

function TUniProvider.NeedNativeParseSQL: boolean;
begin
  Result := False;
end;

function TUniProvider.NeedBlobUnicode(Param: TDAParam): boolean;
begin
{$IFDEF VER10P}
  if Param.DataType = ftWideMemo then
    Result := True
  else
{$ENDIF}
{$IFDEF FPC}
  if Param.DataType = ftWideMemo then
    Result := True
  else
{$ENDIF}
    Result := False;
end;

{$IFNDEF STD}
function TUniProvider.GetAlerterClass: TCRAlerterClass;
begin
  Result := TCRDummyAlerter;
end;
{$ENDIF}

function TUniProvider.GetParamObjectClass(Param: TDAParam): TClass;
begin
  if Param.DataType in (BlobTypes + MemoTypes) then
    Result := TCompressedBlob
  else
    raise Exception.Create(SUnknownDataType);
end;

function TUniProvider.CreateParamObject(Param: TDAParam; IsUnicode: boolean): TSharedObject;
begin
  if Param.DataType in (BlobTypes + MemoTypes) then begin
    Result := TCompressedBlob.Create;
    TCompressedBlob(Result).IsUnicode := IsUnicode;
  end
  else
    raise Exception.Create(SUnknownDataType);
end;

function TUniProvider.CheckParamName(const ParamName: string): string;
begin
  Result := ParamName;
end;

function TUniProvider.GetComponentOptionsList(Component: TComponent): TOptionsList;
begin
  if Component is TCustomDAConnection then
    Result := GetConnectionOptions
  else
  if Component is TCustomDASQL then
    Result := GetSQLOptions
  else
  if Component is TCustomDADataSet then
    Result := GetDataSetOptions
  else
  if Component is TDAScript then
    Result := GetScriptOptions
{$IFNDEF STD}
  else
  if Component is TDALoader then
    Result := GetLoaderOptions
  else
  if Component is TDADump then
    Result := GetDumpOptions
  else
  if Component is TDAAlerter then
    Result := GetAlerterOptions
{$ENDIF}
  else
  if Component is TDATransaction then
    Result := GetTransactionOptions
  else
    Result := nil;
end;

procedure TUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TCRConnection then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRConnection(Obj).SetProp)
  else
  if Obj is TCRConnectionParameters then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRConnectionParameters(Obj).SetProp)
  else
  if Obj is TCRServerEnumerator then
    GetConnectionOptions.ImportOptions(Options, Obj, TCRServerEnumerator(Obj).SetProp)
  else
  if Obj is TConnectDialogService then
    GetConnectionOptions.ImportOptions(Options, Obj, TConnectDialogService(Obj).SetProp)
  else
  if Obj is TCRCommand then
    GetSQLOptions.ImportOptions(Options, Obj, TCRCommand(Obj).SetProp)
  else
  if Obj is TCRRecordSet then begin
    GetDataSetOptions.ImportOptions(Options, Obj, TCRRecordSet(Obj).SetProp);
    GetDataSetOptions.ImportOptions(Options, TCRRecordSet(Obj).GetCommand, TCRRecordSet(Obj).GetCommand.SetProp);
  end
  else
  if Obj is TDAScriptProcessor then
    GetScriptOptions.ImportOptions(Options, Obj, TDAScriptProcessor(Obj).SetProp)
{$IFNDEF STD}
  else
  if Obj is TCRLoader then
    GetLoaderOptions.ImportOptions(Options, Obj, TCRLoader(Obj).SetProp)
  else
  if Obj is TDADumpProcessor then
    GetDumpOptions.ImportOptions(Options, Obj, TDADumpProcessor(Obj).SetProp)
  else
  if Obj is TCRAlerter then
    GetAlerterOptions.ImportOptions(Options, Obj, TCRAlerter(Obj).SetProp)
{$ENDIF}
  else
  if Obj is TDADataSetService then
    GetDataSetOptions.ImportOptions(Options, Obj, TDADataSetService(Obj).SetProp)
  else
  if Obj is TCRTransaction then
    GetTransactionOptions.ImportOptions(Options, Obj, TCRTransaction(Obj).SetProp);
end;

procedure TUniProvider.CreateScriptOptions;
begin
  if FScriptOptions = nil then
    FScriptOptions := TOptionsList.Create(GetProviderName);
end;

{$IFNDEF STD}
procedure TUniProvider.CreateLoaderOptions;
begin
  if FLoaderOptions = nil then
    FLoaderOptions := TOptionsList.Create(GetProviderName);
end;

procedure TUniProvider.CreateDumpOptions;
begin
  if FDumpOptions = nil then
    FDumpOptions := TOptionsList.Create(GetProviderName);
end;

procedure TUniProvider.CreateAlerterOptions;
begin
  if FAlerterOptions = nil then
    FAlerterOptions := TOptionsList.Create(GetProviderName);
end;
{$ENDIF}

procedure TUniProvider.CreateTransactionOptions;
begin
  if FTransactionOptions = nil then
    FTransactionOptions := TOptionsList.Create(GetProviderName);
end;

function TUniProvider.GetConnectionOptions: TOptionsList;
begin
  Result := FConnectionOptions;
end;

function TUniProvider.GetSQLOptions: TOptionsList;
begin
  Result := FSQLOptions;
end;

function TUniProvider.GetDataSetOptions: TOptionsList;
begin
  Result := FDataSetOptions;
end;

function TUniProvider.GetScriptOptions: TOptionsList;
begin
  Result := FScriptOptions;
end;

{$IFNDEF STD}
function TUniProvider.GetLoaderOptions: TOptionsList;
begin
  Result := FLoaderOptions;
end;

function TUniProvider.GetDumpOptions: TOptionsList;
begin
  Result := FDumpOptions;
end;

function TUniProvider.GetAlerterOptions: TOptionsList;
begin
  Result := FAlerterOptions;
end;
{$ENDIF}

function TUniProvider.GetTransactionOptions: TOptionsList;
begin
  Result := FTransactionOptions;
end;

function TUniProvider.DefaultTableSchema: string;
begin
  Result := '';
end;

function TUniProvider.ValidateOption(Owner: TComponent; const Prefix, Name, Value: String): string;
var
  Option: TOption;
  OptionsList: TOptionsList;
begin
  OptionsList := GetComponentOptionsList(Owner);
  if OptionsList <> nil then begin
    Option := OptionsList.OptionByName(Name);
    if Option <> nil then begin
      Option.Validate(Value);
      Result := Option.OptionName; // for depricated
    end
    else
      raise Exception.Create(Format(SInvalidOptionName, [Name, Prefix]));
  end;
end;

function TUniProvider.ValidateOption(Owner: TComponent; const Prefix, Name: string): string;
var
  Option: TOption;
  OptionsList: TOptionsList;
begin
  OptionsList := GetComponentOptionsList(Owner);
  if OptionsList <> nil then begin
    Option := OptionsList.OptionByName(Name);
    if Option <> nil then begin
      Result := Option.OptionName; // for depricated
    end
    else
      raise Exception.Create(Format(SInvalidOptionName, [Name, Prefix]));
  end;
end;

{ TUniProviderDesc }

function TUniProviderDesc.GetUnitName: string;
begin
  Result := FProviderShortName + 'UniProvider';
end;

function TUniProviderDesc.GetUnitNameCLR: string;
begin
  Result := 'Devart.UniDac.Oracle.' + GetUnitName;
end;

function TUniProviderDesc.GetProviderComponentName: string;
begin
  Result := 'T' + GetUnitName;
end;

{ TUniProviders }

function TUniProviders.FindProviderDesc(ProviderName: string): TUniProviderDesc;
var
  i: integer;
  List: TList{$IFDEF AUTOREFCOUNT}<TUniProviderDesc>{$ENDIF};
begin
  List := LockList;
  try
    for i := 0 to List.Count - 1 do begin
      Result := {$IFNDEF AUTOREFCOUNT}TUniProviderDesc{$ENDIF}(List[i]);
      if CompareText(Result.ProviderName, ProviderName) = 0 then
        exit;
    end;
    Result := nil;
  finally
    UnlockList;
  end;
end;

procedure TUniProviders.RegisterProviderDesc(ProviderName, ProviderShortName,
  PackageName, AssemblyName, SiblingProduct: string);
var
  UniProviderDesc: TUniProviderDesc;
begin
  UniProviderDesc := FindProviderDesc(ProviderName);
  if UniProviderDesc = nil then begin
    UniProviderDesc := TUniProviderDesc.Create;
    UniProviderDesc.FProviderName := ProviderName;
    UniProviderDesc.FProviderShortName := ProviderShortName;
    UniProviderDesc.FPackageName := PackageName;
    UniProviderDesc.FAssemblyName := AssemblyName;
    UniProviderDesc.FSiblingProduct := SiblingProduct;

    Add(UniProviderDesc);
  end;
end;

destructor TUniProviders.Destroy;
var
{$IFNDEF AUTOREFCOUNT}
  i: integer;
  List: TList;
{$ELSE}
  List: TList<TUniProviderDesc>;
{$ENDIF}
begin
  List := LockList;
  try
  {$IFNDEF AUTOREFCOUNT}
    for i := 0 to List.Count - 1 do
      TUniProviderDesc(List[i]).Free;
  {$ENDIF}
    List.Clear;
  finally
    UnlockList;
  end;

  inherited;
end;

procedure TUniProviders.RegisterProvider(UniProviderClass: TUniProviderClass);
var
  UniProviderDesc: TUniProviderDesc;
begin
  LockList;
  try
    UniProviderDesc := GetProviderDesc(UniProviderClass.GetProviderName);
    if UniProviderDesc.FProvider <> nil then begin
      UniProviderDesc.FProvider.Free;
      UniProviderDesc.FProvider := nil;
    end;
    UniProviderDesc.FProvider := UniProviderClass.Create(nil);
  finally
    UnLockList;
  end;  
end;

procedure TUniProviders.UnRegisterProvider(UniProviderClass: TUniProviderClass);
var
  UniProviderDesc: TUniProviderDesc;
begin
  LockList;
  try
    UniProviderDesc := GetProviderDesc(UniProviderClass.GetProviderName);
    UniProviderDesc.FProvider.Free;
    UniProviderDesc.FProvider := nil;
  finally
    UnLockList;
  end;  
end;

function TUniProviders.GetProviderDesc(ProviderName: string): TUniProviderDesc;
begin
  Result := FindProviderDesc(ProviderName);
  if Result = nil then
    raise Exception.Create(Format(SInvalidProviderName, [ProviderName]));
end;

function TUniProviders.GetProvider(ProviderName: string): TUniProvider;
var
  UniProviderDesc: TUniProviderDesc;
begin
  UniProviderDesc := FindProviderDesc(ProviderName);

  if UniProviderDesc <> nil then
    Result := UniProviderDesc.Provider
  else
    Result := nil;
end;

procedure TUniProviders.GetProviderNames(Names: TStrings);
var
  i: integer;
  List: TList{$IFDEF AUTOREFCOUNT}<TUniProviderDesc>{$ENDIF};
  UniProviderDesc: TUniProviderDesc;
begin
  List := LockList;
  try
    Names.Clear;
    for i := 0 to List.Count - 1 do begin
      UniProviderDesc := {$IFNDEF AUTOREFCOUNT}TUniProviderDesc{$ENDIF}(List[i]);
      if UniProviderDesc.Provider <> nil then
        Names.Add(UniProviderDesc.ProviderName);
    end;
  finally
    UnlockList;
  end;
end;

{ TConnectDialogService }

constructor TConnectDialogService.Create;
begin
  inherited;
end;

function TConnectDialogService.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TConnectDialogService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TConnectDialogService.GetConnectMode: integer;
begin
  Result := 1;
end;

function TConnectDialogService.UseDatabaseHistory: boolean;
begin
  Result := False;
end;

function TConnectDialogService.GetDefaultDatabase: string;
begin
  Result := '';
end;

function TConnectDialogService.UsernameEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.PasswordEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.ServerEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.DatabaseEnabled: boolean;
begin
  Result := True;
end;

function TConnectDialogService.PortEnabled: boolean;
begin
  Result := True;
end;

{ TCRDummyAlerter }

procedure TCRDummyAlerter.SendEvent(const EventName, Message: string);
begin
  raise Exception.Create(SAlertsNotSupported);
end;

procedure TCRDummyAlerter.Start;
begin
  raise Exception.Create(SAlertsNotSupported);
end;

procedure TCRDummyAlerter.Stop;
begin
  raise Exception.Create(SAlertsNotSupported);
end;

{ TOption }

constructor TOption.Create(const OptionName: string; InternalIndex: integer; InternalClasses: array of TClass;
  DefaultValue: variant; OnAssign: TOnAssignValue = nil);
var
  i: integer;
begin
  inherited Create;

  FOptionName := OptionName;
  FInternalIndex := InternalIndex;
  FDefaultValue := DefaultValue;
  FOnAssignValue := OnAssign;

  SetLength(FInternalClasses, Length(InternalClasses));
  for i := 0 to High(InternalClasses) do
    FInternalClasses[i] := InternalClasses[i];
end;

function TOption.GetAsNative(const Value: string): variant;
begin
  Result := Value;
end;

function TOption.GetAsString(const Value: variant): string;
begin
  Result := Value;
end;

function TOption.GetDefaultValue: variant;
begin
  Result := FDefaultValue;
end;

procedure TOption.InternalGetValuesList(List: TStrings);
begin

end;

procedure TOption.GetValuesList(List: TStrings);
begin
  if Assigned(FOnGetValuesList) then
    FOnGetValuesList(List)
  else
    InternalGetValuesList(List);  
end;

procedure TOption.ValidationError(const Value: string);
begin
  raise Exception.Create('Invalid value: ' +  Value + ' for option ' + OptionName);
end;

function TOption.CheckValue(const Value: string): boolean;
begin
  Result := True;
end;

procedure TOption.Validate(const Value: string);
var
  i: integer;
  List: TStringList;
begin
  if Assigned(FOnGetValuesList) then begin
    List := TStringList.Create;
    try
      for i := 0 to List.Count - 1 do
        if SameText(List[i], Value) then
          Exit; 
      ValidationError(Value);
    finally
      List.Free;
    end;
  end
  else
    if not CheckValue(Value) then
      ValidationError(Value);
end;

{ TIntegerOption }

function TIntegerOption.GetAsString(const Value: variant): string;
begin
  Result := IntToStr(Value);
end;

function TIntegerOption.GetAsNative(const Value: string): variant;
begin
  Result := StrToInt(Trim(Value));
end;

function TIntegerOption.CheckValue(const Value: string): boolean;
begin
  Result := StrToIntDef(Trim(Value), -MaxInt) <> -MaxInt;
end;

{ TBooleanOption }

function TBooleanOption.GetAsString(const Value: variant): string;
begin
  if Boolean(Value) then
    Result := 'True'
  else
    Result := 'False';
end;

function TBooleanOption.GetAsNative(const Value: string): variant;
begin
  if SameText(Trim(Value), 'True') then
    Result := True
  else
    Result := False;
end;

function TBooleanOption.CheckValue(const Value: string): boolean;
begin
  Result := SameText(Trim(Value), 'True') or SameText(Trim(Value), 'False');
end;

procedure TBooleanOption.InternalGetValuesList(List: TStrings);
begin
  List.Add('False');
  List.Add('True');
end;

{ TEnumeratorOption }

constructor TEnumeratorOption.Create(const OptionName: string; InternalIndex: integer;
  InternalClasses: array of TClass; DefaultValue: variant; TypeInfo: PTypeInfo);
var
  TypeData: PTypeData;
  TypeName: string;
begin
  inherited Create(OptionName, InternalIndex, InternalClasses, DefaultValue);

  FTypeInfo := TypeInfo;
  TypeData := GetTypeData(FTypeInfo);
  if TypeData <> nil then begin
    FMinValue := TypeData{$IFNDEF CLR}^{$ENDIF}.MinValue;
    FMaxValue := TypeData{$IFNDEF CLR}^{$ENDIF}.MaxValue;
  end;

{$IFDEF NEXTGEN}
  TypeName := GetTypeName(FTypeInfo);
{$ELSE}
  TypeName := string(FTypeInfo.Name);
{$ENDIF}

  FInternalType := (TypeName <> '') and (TypeName[1] = '_');
end;

function TEnumeratorOption.GetAsString(const Value: variant): string;
begin
  if FInternalType then
    Result := Copy(GetEnumName(FTypeInfo, Integer(Value)), 2, MaxInt)
  else
    Result := GetEnumName(FTypeInfo, Integer(Value));
end;

function TEnumeratorOption.GetAsNative(const Value: string): variant;
var
  RealValue: string;
begin
  Assert(FMaxValue <= High(Byte));
  RealValue := Trim(Value);
  if FInternalType then
    RealValue := '_' + RealValue;
  VarCast(Result, GetEnumValue(FTypeInfo, RealValue), varByte);
end;

function TEnumeratorOption.CheckValue(const Value: string): boolean;
var
  RealValue: string;
begin
  RealValue := Trim(Value);
  if FInternalType then
    RealValue := '_' + RealValue;
  Result := GetEnumValue(FTypeInfo, RealValue) >= 0;
end;

procedure TEnumeratorOption.InternalGetValuesList(List: TStrings);
var
  i: integer;
begin
  for i := FMinValue to FMaxValue do
    if FInternalType then
      List.Add(Copy(GetEnumName(FTypeInfo, i), 2, MaxInt))
    else
      List.Add(GetEnumName(FTypeInfo, i));
end;

{ TOptionsList }

constructor TOptionsList.Create(const Prefix: string);
begin
  inherited Create;

  FDeprecatedOptions :=  TStringList.Create;

  Sorted := True;
  FPrefix := Prefix;
end;

destructor TOptionsList.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  i: integer;
{$ENDIF}
begin
  FDeprecatedOptions.Free;

{$IFNDEF AUTOREFCOUNT}
  for i := 0 to Count - 1 do
    Objects[i].Free;
{$ENDIF}
  Clear;

  inherited;
end;

procedure TOptionsList.Add(Value: TOption);
begin
  AddObject(Value.OptionName, Value);
end;

procedure TOptionsList.AddDeprecated(const DeprecatedName: string; Value: TOption);
begin
  FDeprecatedOptions.AddObject(DeprecatedName, Value);
end;

procedure TOptionsList.Remove(const Name: string);
var
  i: Integer;
begin
  i := IndexOf(Name);
  if i >= 0 then
    Delete(i);
end;

function TOptionsList.OptionByCode(Code: Integer): TOption;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to self.Count - 1 do
    if self[i].InternalIndex = Code then begin
      Result := TOption(self[i]);
      break;
    end;
end;

function TOptionsList.OptionByName(const Name: string): TOption;
var
  i: integer;
begin
  i := IndexOf(Name);
  if i <> -1 then
    Result := TOption(Objects[i])
  else
    Result := OptionByDeprecatedName(Name);
end;

function TOptionsList.OptionByDeprecatedName(const Name: string): TOption;
var
  i: integer;
begin
  i := FDeprecatedOptions.IndexOf(Name);
  if i <> -1 then
    Result := TOption(FDeprecatedOptions.Objects[i])
  else
    Result := nil;
end;

procedure TOptionsList.ImportOptions(Source: TStrings; DestObject: TObject; SetPropFunc: TSetPropFunc);
var
  i, k, j: integer;
  Option: TOption;
  UseDefault: boolean;
  Value: Variant;
  OptionPrefix, OptionName, OptionValue: string;
begin
  for i := 0 to Count - 1 do
    for k := 0 to High(Items[i].InternalClasses) do begin
      if DestObject.ClassType = Items[i].InternalClasses[k] then begin
        Option := Items[i];
        UseDefault := True;

        for j := 0 to Source.Count - 1 do begin
          ExtractOption(Source[j], OptionPrefix, OptionName, OptionValue);
          if ((OptionPrefix = '') or SameText(Prefix, OptionPrefix)) and
            SameText(Option.OptionName, OptionName)
          then begin
            UseDefault := False;
            Break;
          end;
        end;

        if not UseDefault then begin
          Option.Validate(OptionValue);
          Value := Option.GetAsNative(OptionValue);
        end
        else
          Value := Option.GetDefaultValue;

        if Assigned(Option.OnAssignValue) then
          Option.OnAssignValue(DestObject, Value)
        else
          SetPropFunc(Option.InternalIndex, Value);

        break;
      end;
    end;
end;

procedure TOptionsList.ExportDefOptions(Dest: TStrings);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Dest.Add(FPrefix + '.' + Items[i].OptionName + '=' + Items[i].GetAsString(Items[i].GetDefaultValue));
end;

function TOptionsList.GetValueByName(Source: TStrings; const Name: string): variant;
var
  i: integer;
  Option: TOption;
  OptionPrefix, OptionName, OptionValue: string;
begin
  Result := Unassigned;
  Option := OptionByName(Name);
  if Option <> nil then begin
    for i := 0 to Source.Count - 1 do begin
      ExtractOption(Source[i], OptionPrefix, OptionName, OptionValue);
      if ((OptionPrefix = '') or SameText(Prefix, OptionPrefix)) and
        SameText(OptionName, Name)
      then begin
        Option.Validate(OptionValue);
        Result := Option.GetAsNative(OptionValue);
        exit;
      end;
    end;
    Result := Option.GetDefaultValue;
  end;
end;

function TOptionsList.GetOption(Index: integer): TOption;
begin
  Result := TOption(inherited Objects[Index]);
end;

procedure TOptionsList.SetOption(Index: integer; const Value: TOption);
begin
  inherited Objects[Index] := Value;
end;

{ TUniSqlFormatter }

const
  BeginMacroSt = '{';
  EndMacroSt = '}';
  FunctionPrefix = 'fn';
  DatePrefix = 'date';
  TimePrefix = 'time';
  TimestampPrefix = 'timestamp';
  IfPrefix = 'if';
  ElsePrefix = 'else';
  EndIfPrefix = 'endif';
  DateTimeLiteralFunctionName = '__DATE_TIME_LITERAL';
  DateLiteralFunctionName = '__DATE_LITERAL';
  TimeLiteralFunctionName = '__TIME_LITERAL';

constructor TUniSqlFormatter.Create;
begin
  inherited;
  FParserClass := TSQLParser;
end;

destructor TUniSqlFormatter.Destroy;
begin
  inherited;
end;

procedure TUniSqlFormatter.SetUserMacros(Names, Values: TStringList);
begin
  FUserMacroNames := Names;
  FUserMacroValues := Values;
end;

procedure TUniSqlFormatter.Expand(var SQL: string);
var
  Parser: TSQLParser;
begin
  // performance reason
  if Pos(string(BeginMacroSt), SQL) = 0 then
    Exit;

  Parser := FParserClass.Create(SQL);
  try
    Parser.OmitBlank := False;
    Parser.Uppered := False;
    Parser.QuotedString := True;
    SQL := Parse(Parser);
  finally
    Parser.Free;
  end;
end;

function TUniSqlFormatter.LeftQuote: char;
begin
  Result := '"';
end;

function TUniSqlFormatter.RightQuote: char;
begin
  Result := '"';
end;

function TUniSqlFormatter.IsDefinedMacro(const MacroName: string): boolean;
begin
  Result := (FUserMacroNames.IndexOf(MacroName) >= 0) or
    (FPredefinedMacros.IndexOf(MacroName) >= 0);
end;

function TUniSqlFormatter.GetFunction(const FunctionName: string; const Params: TStringArray): string;
begin
{$IFDEF FPC}
  Result := ''; // Lazarus anti-warning
{$ENDIF}
  raise Exception.CreateFmt(SUnknownFunction, [FunctionName]);
end;

function TUniSqlFormatter.ProcessFunction(const Body: string): string;
var
  Parser: TSQLParser;
  CodeLexem: integer;
  St: string;
  FuncName: string;
  Params: TStringArray;
  ParamCount: Integer;
  CurrPos: integer;
  BracketCount: Integer;
  FuncInd: integer;
begin
  Parser := FParserClass.Create(Body);
  Parser.OmitBlank := True;
  Parser.Uppered := False;
  Parser.QuotedString := True;
  try
    Parser.ToBegin;
    CodeLexem := Parser.GetNext(FuncName);
    if (CodeLexem <> lcIdent) and ((CodeLexem < 0) or Parser.IsSymbolCode(CodeLexem)) then
      raise Exception.Create(SEmptyFunctionName);

    CodeLexem := Parser.GetNext(St);
    ParamCount := 0;
    SetLength(Params, 0);
    if St = '(' then begin
      // Parsing params
      CurrPos := Parser.CurrPos;
      BracketCount := 0;
      repeat
        CodeLexem := Parser.GetNext(St);
        if (BracketCount = 0) and ((St = ',') or (St = ')')) then begin
          SetLength(Params, ParamCount + 1);
          Params[ParamCount] := Copy(Body, CurrPos + 1, Parser.CurrPos - CurrPos - 1);
          Inc(ParamCount);
          CurrPos := Parser.CurrPos;
        end
        else
          if St = '(' then
            Inc(BracketCount)
          else
          if St = ')' then
            Dec(BracketCount);
      until CodeLexem = lcEnd;

      if (BracketCount > 0) or (Body[Parser.PrevPos] <> ')') then
        raise Exception.Create(SInvalidBracketCount);
    end
    else
      if CodeLexem <> lcEnd then
        raise Exception.Create(SUnexpectedChar);

    FuncInd := FFunctions.IndexOf(FuncName);
    if FuncInd <> -1 then begin
      Result := FFunctions.Values[FuncInd];
      case ParamCount of
        0: Result := Result;
        1: Result := Format(Result, [Params[0]]);
        2: Result := Format(Result, [Params[0], Params[1]]);
        3: Result := Format(Result, [Params[0], Params[1], Params[2]]);
      else
        raise Exception.CreateFmt(SWrongArgCnt, [FuncName]);
      end;
    end
    else
      Result := GetFunction(FuncName, Params);
  finally
    Parser.Free;
  end;
end;

function TUniSqlFormatter.ProcessDate(const Body: string): string;
var
  Date: string;
  FuncInd: integer;
begin
  Date := Trim(Body);
  if (Length(Date) < 2) or (Date[1] <> '''') or (Date[Length(Date)] <> '''') then
    raise Exception.Create(SInvalidDate);

  if NeedUnquote then
    Date := Copy(Date, 2, Length(Date) - 2);

  FuncInd := FFunctions.IndexOf(DateLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(FFunctions.Values[FuncInd], [Date]);
end;

function TUniSqlFormatter.ProcessTime(const Body: string): string;
var
  Time: string;
  FuncInd: integer;
begin
  Time := Trim(Body);
  if (Length(Time) < 2) or (Time[1] <> '''') or (Time[Length(Time)] <> '''') then
    raise Exception.Create(SInvalidTime);

  if NeedUnquote then
    Time := Copy(Time, 2, Length(Time) - 2);

  FuncInd := FFunctions.IndexOf(TimeLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(FFunctions.Values[FuncInd], [Time]);
end;

function TUniSqlFormatter.ProcessTimestamp(const Body: string): string;
var
  Timestamp: string;
  FuncInd: integer;
begin
  Timestamp := Trim(Body);
  if (Length(Timestamp) < 2) or (Timestamp[1] <> '''') or (Timestamp[Length(Timestamp)] <> '''') then
    raise Exception.Create(SInvalidTimestamp);

  FuncInd := FFunctions.IndexOf(DateTimeLiteralFunctionName);
  if FuncInd = -1 then
    Assert(False);
    //raise Exception.Create('');

  Result := Format(FFunctions.Values[FuncInd], [Timestamp]);
end;

function TUniSqlFormatter.ProcessMacro(const MacroName, Body: string): string;
var
  Name, NewBody: string;
  i: integer;
  Parser: TSQLParser;
begin
  Name := Trim(AnsiUpperCase(MacroName));
  NewBody := Body;
  if NewBody <> '' then
    NewBody := ' ' + NewBody;
  i := FUserMacroNames.IndexOf(Name);
  if i >= 0 then
    Result := FUserMacroValues[i] + NewBody
  else begin
    i := FPredefinedMacros.IndexOf(Name);
    if i >= 0 then
      Result := FPredefinedMacros.Values[i] + NewBody
    else
      Result := '';
  end;

  if Result <> '' then begin
    Parser := FParserClass.Create(Result);
    try
      Parser.OmitBlank := False;
      Parser.Uppered := False;
      Parser.QuotedString := True;
      Result := Parse(Parser);
    finally
      Parser.Free;
    end;
  end;
end;

function TUniSqlFormatter.NeedUnquote: boolean;
begin
  Result := False;
end;

function TUniSqlFormatter.CheckIfCondition(const Body: string): Boolean;
var
  Name: string;
begin
  Name := Trim(AnsiUpperCase(Body));
  if Name <> '' then
    Result := IsDefinedMacro(Name)
  else
    // empty condition
    Result := True;
end;

function TUniSqlFormatter.IsServerKeywordLexem(Parser: TSQLParser; const Lexem: string): Boolean;
begin
  Result := False;
end;

function TUniSqlFormatter.Parse(Parser: TSQLParser; const EndChar: string = ''): string;
var
  CodeLexem: integer;
  St: string;
  MacroName, _MacroName, Body: string;
  IfCount: integer;
  IfResult: boolean;
  IfResults: array of boolean;
  ElseClauses: array of boolean;

  function GetIfResult: Boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 0 to IfCount - 1 do
      Result := Result and IfResults[i];
  end;

begin
  Result := '';
  St := '';
  IfCount := 0;
  IfResult := True;
  SetLength(IfResults, 0);
  SetLength(ElseClauses, 0);

  while True do begin
    CodeLexem := Parser.GetNext(St);

    if (CodeLexem = lcEnd) or (St = EndChar) then
      Break;

    if St = BeginMacroSt then begin
      CodeLexem := Parser.GetNext(MacroName);
      if (CodeLexem <> lcIdent) and ((CodeLexem < 0) or Parser.IsSymbolCode(CodeLexem)) then
        if IsServerKeywordLexem(Parser, MacroName) then begin
          Result := Result + St + MacroName;
          Continue;
        end
        else
          raise Exception.Create(SEmptyMacroOrFunction);

      CodeLexem := Parser.GetNext(St);
      Body := '';
      if CodeLexem = lcBlank then begin
        Body := Parse(Parser, '}');
      end
      else
        if (CodeLexem <> lcSymbol) or (St <> '}') then
          raise Exception.Create(SUnexpectedChar);

      _MacroName := AnsiLowerCase(MacroName);
      if _MacroName = IfPrefix then begin
        Inc(IfCount);
        SetLength(IfResults, IfCount);
        IfResults[IfCount - 1] := CheckIfCondition(Body);
        IfResult := GetIfResult;
        SetLength(ElseClauses, IfCount);
        ElseClauses[IfCount - 1] := False;
      end
      else
      if _MacroName = ElsePrefix then begin
        if IfCount = 0 then
          raise Exception.Create(SUnexpectedElse);

        if ElseClauses[IfCount - 1] then
          raise Exception.Create(SUnexpectedElse)
        else
          ElseClauses[IfCount - 1] := True;

        IfResults[IfCount - 1] := not IfResults[IfCount - 1];
        IfResult := GetIfResult;
      end
      else
      if _MacroName = EndIfPrefix then begin
        if IfCount = 0 then
          raise Exception.Create(SUnexpectedEndif);

        Dec(IfCount);
        SetLength(IfResults, IfCount);
        IfResult := GetIfResult;
        SetLength(ElseClauses, IfCount);
      end
      else
      if IfResult then begin
        if _MacroName = FunctionPrefix then begin
          Result := Result + ProcessFunction(Body);
        end
        else
        if _MacroName = DatePrefix then begin
          Result := Result + ProcessDate(Body);
        end
        else
        if _MacroName = TimePrefix then begin
          Result := Result + ProcessTime(Body);
        end
        else
        if _MacroName = TimestampPrefix then begin
          Result := Result + ProcessTimestamp(Body);
        end
        else
          Result := Result + ProcessMacro(MacroName, Body);
      end;
    end
    else
      if IfResult then begin
        if (CodeLexem = lcIdent) and
          (Length(St) > 1) and (St[1] = '"') and (St[Length(St)] = '"')
        then begin
          St[1] := LeftQuote;
          St[Length(St)] := RightQuote;
        end
        else
        if (CodeLexem = lcComment) and
          (Length(St) > 1) and (St[1] = '-') and (St[2] = '-')
          and (Pos('*/', St) = 0)
        then
          St := '/*' + Copy(St, 3, MaxInt) + ' */'; //do not delete space (rm-#90913)
        Result := Result + St;
      end;
  end;

  if IfCount <> 0 then
    raise Exception.Create(SNotCompletedIF);
end;

procedure TUniSqlFormatter.SetParserClass(Value: TSQLParserClass);
begin
  FParserClass := Value;
end;

function GetUniProviders: TUniProviders;
begin
  Result := UniProviders;
end;

procedure FillOptionsList(const OptionPrefix: string; OptionsList: TOptionsList; List: TStrings);
var
  i: integer;
begin
  for i := 0 to OptionsList.Count - 1 do
    List.Add(OptionPrefix + '.' + OptionsList[i].OptionName + '=' + OptionsList[i].GetAsString(OptionsList[i].GetDefaultValue));
end;

procedure GetOptionValuesList(const OptionName: string; OptionsList: TOptionsList; List: TStrings);
var
  Option: TOption;
begin
  Option := OptionsList.OptionByName(OptionName);
  if Option <> nil then
    Option.GetValuesList(List);
end;

procedure ExtractOption(const Str: string; var OptionPrefix, OptionName, OptionValue: string);
var
  DotPos, EqualPos: integer;
begin
  DotPos := Pos('.', Str);
  EqualPos := Pos('=', Str);
  if DotPos > EqualPos then
    DotPos := 0;

  if (DotPos > 0) and (EqualPos > 0) then
    OptionPrefix := Copy(Str, 1, DotPos - 1);

  if EqualPos > 0 then
    OptionName := Copy(Str, DotPos + 1, EqualPos - DotPos - 1);

  if EqualPos > 0 then
    OptionValue := Copy(Str, EqualPos + 1, Length(Str));
end;

procedure WriteOptions(OptionsList: TOptionsList; List: TStrings; DestClass: TClass; SetPropFunc: TSetPropFunc);
begin

end;

procedure SetSpecificOption(Connection: TCustomDAConnection; const Name, Value: string);
begin
  TUniConnection(Connection).SpecificOptions.Values[Name] := Value;
end;

procedure CheckProviderName(const ProviderName: string);
var
  UniProviderDesc: TUniProviderDesc;
begin
  if (ProviderName = '') then
    DatabaseError(SProviderNotDefined);

  if (ProviderName <> '') then begin
    UniProviderDesc := UniProviders.GetProviderDesc(ProviderName);
    raise Exception.CreateFmt(SProviderNotRegistered,
      [UniProviderDesc.ProviderName,
       UniProviderDesc.ProviderUnitName,
       UniProviderDesc.ProviderComponentName]);
  end;
end;

initialization
  UniProviders := TUniProviders.Create;
  UniProviders.RegisterProviderDesc('Access', 'Access', 'accessprovider', 'Devart.UniDac.Access', '');
  UniProviders.RegisterProviderDesc('Advantage', 'Advantage', 'adsprovider', 'Devart.UniDac.Advantage', '');
  UniProviders.RegisterProviderDesc('ASE', 'ASE', 'aseprovider', 'Devart.UniDac.ASE', '');
  UniProviders.RegisterProviderDesc('DB2', 'DB2', 'db2provider', 'Devart.UniDac.DB2', '');
  UniProviders.RegisterProviderDesc('DBF', 'DBF', 'dbfprovider', 'Devart.UniDac.DBF', '');
  UniProviders.RegisterProviderDesc('InterBase', 'InterBase', 'ibprovider', 'Devart.UniDac.InterBase', 'IBDAC');
  UniProviders.RegisterProviderDesc('MySQL', 'MySQL', 'myprovider', 'Devart.UniDac.MySQL', 'MyDAC');
  UniProviders.RegisterProviderDesc('MongoDB', 'MongoDB', 'mongoprovider', '', '');
  UniProviders.RegisterProviderDesc('NexusDB', 'NexusDB', 'nexusprovider', 'Devart.UniDac.NexusDB', '');
  UniProviders.RegisterProviderDesc('ODBC', 'ODBC', 'odbcprovider', 'Devart.UniDac.ODBC', '');
  UniProviders.RegisterProviderDesc('Oracle', 'Oracle', 'oraprovider', 'Devart.UniDac.Oracle', 'ODAC');
  UniProviders.RegisterProviderDesc('PostgreSQL', 'PostgreSQL', 'pgprovider', 'Devart.UniDac.PostgreSQL', 'PgDAC');
  UniProviders.RegisterProviderDesc('Redshift', 'Redshift', 'rsprovider', '', '');
  UniProviders.RegisterProviderDesc('SQL Server', 'SQLServer', 'msprovider', 'Devart.UniDac.SQLServer', 'SDAC');
  UniProviders.RegisterProviderDesc('SQLite', 'SQLite', 'liteprovider', 'Devart.UniDac.SQLite', 'LiteDAC');

//  UniProviders.RegisterProviderDesc('AdoNet', 'AdoNet', 'adonetprovider', '', '');
  UniProviders.RegisterProviderDesc('BigCommerce', 'BigCommerce', 'bigcommerceprovider', '', '');
  UniProviders.RegisterProviderDesc('Dynamics CRM', 'DynamicsCRM', 'dynamicsprovider', '', '');
  UniProviders.RegisterProviderDesc('FreshBooks', 'FreshBooks', 'freshbooksprovider', '', '');
  UniProviders.RegisterProviderDesc('Magento', 'Magento', 'magentoprovider', '', '');
  UniProviders.RegisterProviderDesc('MailChimp', 'MailChimp', 'mailchimpprovider', '', '');
  UniProviders.RegisterProviderDesc('NetSuite', 'NetSuite', 'netsuiteprovider', '', '');
  UniProviders.RegisterProviderDesc('OData', 'OData', 'odataprovider', '', '');
  UniProviders.RegisterProviderDesc('QuickBooks', 'QuickBooks', 'quickbooksprovider', '', '');
  UniProviders.RegisterProviderDesc('Salesforce', 'Salesforce', 'salesforceprovider', '', '');
  UniProviders.RegisterProviderDesc('Salesforce MC', 'SalesforceMC', 'exacttargetprovider', '', '');
  UniProviders.RegisterProviderDesc('SugarCRM', 'SugarCRM', 'sugarprovider', '', '');
  UniProviders.RegisterProviderDesc('Zoho CRM', 'ZohoCRM', 'zohoprovider', '', '');

finalization
  UniProviders.Free;

end.
