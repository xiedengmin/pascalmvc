
{$IFNDEF CLR}
{$I Dac.inc}

unit DBForgeStudioClientImp;
{$ENDIF}

interface

uses
{$IFDEF WIN32_64}
  CLRClasses,
  VarUtils,
  DesignIntf,
  ToolsApi,
{$ELSE}
  System.Runtime.InteropServices,
  Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.WinUtils,
  Borland.Studio.ToolsAPI,
  System.Reflection,
  System.Diagnostics,
  Devart.DbForge,
  System.Text,
{$ENDIF}
  ActiveX, Variants,
  CRTypes, CRDesignUtils, DADesignUtils,
  Windows, Classes, Controls, StdCtrls, ExtCtrls, DB, DBAccess, DASQLGenerator,
{$IFNDEF CLR}
  DBToolsIntf,
{$ENDIF}
  DBToolsClient, Graphics, Menus, Forms,
  Messages, CRFunctions;

{$IFDEF CLR}
const
  ParameterType_Input = ParameterType(0);
  ParameterType_InputOutput = ParameterType(1);
  ParameterType_Output = ParameterType(2);
  ParameterType_ReturnValue = ParameterType(3);
{$ENDIF}

type
  TSqlSourceFS = class;

  TDBForgeStudioServiceEvents = class(TInterfacedObject, IDbForgeServiceEvents)
    FOnDeactivate: TNotifyEvent;
  public
    procedure OnDeactivate; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    property Deactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

  TDBForgeStudioService = class(TCustomDBToolsService)
  private
    procedure OnServerDeactivate(Sender: TObject);
  protected
    FUsedConnectionStrList: TStringList;
    FUsedConnectionCompareFlags: array of TCompareFlag;

    FConnectionStrList: TStringList;
    FConnectionsList: TStringList;
    FDefaultConnectionList: TStringList;
    FSqlService: IDbForgeService;
    FParamTypeMaps: array of TParamTypeMap;
    FDBForgeStudioServiceEvents: TDBForgeStudioServiceEvents;

    function GetDACSqlEditorFrameClass: TCustomDACSqlEditorFrameClass; override;
    function SqlSourceClass: TCustomSqlSourceClass; override;

    function GetSqlEditor: ISqlEditor;
    function GetSqlSource(Component: TComponent; Designer: IDesigner; SqlTextPrefix: string = ''; StatementType: TStatementType = stQuery): TSqlSourceFS;
  {$IFDEF WIN32_64}
    class function AccessData(const V: PSafeArray): pointer;
    class procedure UnaccessData(const V: PSafeArray);
    class function DataHigh(const V: PSafeArray): integer;
  {$ENDIF}
    class function GetConnectionParamStr(const ParamName, ParamValue: string): string;
    function GetConnectionValueStr(ConnectionName: string): string;
    procedure ConnStrToList(ConnStr: string; const ConnList: TStrings);
    procedure CheckConnection(const Component: TComponent);

    procedure BeginConnectionStrGetting(const ConnectionStrList: TStringList);

    procedure DesignerClosing(DesignerName: string); override;

    class function GetNamespace: string; override;
  public
    constructor Create(ADADesignUtils: TDADesignUtilsClass; ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF};ADefaultConnectionStr: string); override;
    destructor Destroy; override;
    function DBForgeStudioTypeToDataType(AType: integer; OldType: TFieldType): TFieldType;
    function DataTypeToDBForgeStudioType(AType: TFieldType): integer;


    function GetNativeConnectionString(const Component: TComponent): string;
    function GetConnectionString(const Component: TComponent): TString;
    function GetConnectionStringObjectTypeAndFullName(const Component: TComponent; out ConnectionString, ObjectType, FullName: TString): boolean;

    procedure GetConnections(NameList: TStrings; Condition: string = ''); override;
    function FindConnectionName(AConnection: TCustomDAConnection): string; override;//Call GetConnections
    function GetConnectionStrList(ConnectionName: string): TStringList; override;//before!

    procedure FindInDatabaseExplorer; override;
    procedure EditDatabaseObject; override;
    procedure ExecuteSql(Debug: boolean); override;
    procedure Compile(Debug: boolean); override;
    procedure RetrieveData(AsDocument: boolean); override;
    procedure EditSql(AsQuery: boolean; StatementType: TStatementType = stQuery); override;

    procedure AddParamTypeMap(ADACType: TFieldType; ADBToolsType: integer); override;

    procedure PutConnectionParam(const ConnectionParam: string; const CompareFlag: TCompareFlag = cfNormal); override;
    procedure SkipConnectionParams(const Count: integer); override;

     property SqlService: IDbForgeService read FSqlService;
  end;

  TDACSqlEditorFrameFS = class(TCustomDACSqlEditorFrame)
  private
    FDBToolsService: TDBForgeStudioService;
    FSqlEditors: array[TStatementType] of ISqlEditor;
    FStatementType: TStatementType;
    FComponent: TComponent;
    FLastConnectionString: string;
    FInInit: boolean;

    function GetSqlEditor: ISqlEditor;
    procedure InternalResize;
  protected
    procedure SetStatementType(const Value: TStatementType); override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    procedure SetReadOnly(Value: boolean); override;

    procedure Resize; override;
    function GetSqlEditorHandle: HWND;
    procedure CheckModified; override;
    procedure WndProc(var Message: TMessage); override;

    procedure EndInit; override;
    procedure CheckConnectionChange; override;
  public
    constructor Create(AOwner: TComponent; Component: TComponent; DBToolsService: TCustomDBToolsService); override;
    destructor Destroy; override;
    procedure SetFocus; override;

    property SqlEditor: ISqlEditor read GetSqlEditor;
  end;

  TSourceNotifierFS = class(TCustomSourceNotifier)
    FSqlSourceNotifier: ISqlSourceNotifier;
    procedure OnSqlSourceDeleted; override;
  end;

  TSqlSourceFS = class(TCustomSQLSource, ISqlSource)
  protected
    FParameterCount: integer;
    FParameterSetted: array of boolean;
    FDesignerName: string;
    FLastName: string;
    FComponentSQL: string;
    FStatementType: TStatementType;
    FDBToolsService: TDBForgeStudioService;
    FLastConnection: TCustomDAConnection;
    FLastConnectionString: string;
    FID: string;

    function GetParams: TDAParams;
    function GetSqlText: string;
    procedure SetSqlText(Value: string);

    function Get_Name: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ConnectionString: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_DesignerName: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_Sql: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_Sql(const Param1: TString); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure GetParameter(const Index: integer; out Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure SetParameter(const Index: integer; Info: CommandParameterInfo); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    procedure Set_ParameterCount(const Value: integer); {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ParameterCount: integer; {$IFDEF WIN32_64}stdcall;{$ENDIF}
    function Get_ID: TString; {$IFDEF WIN32_64}stdcall;{$ENDIF}

    procedure Close; {$IFDEF WIN32_64}stdcall;{$ENDIF}

    function GetSourceNotifierClass: TCustomSourceNotifierClass; override;
    procedure FreeSourceNotifier; override;
    function GetDBToolsService: TCustomDBToolsService; override;

    property Params: TDAParams read GetParams;
  public
    constructor Create(DBToolsService: TDBForgeStudioService; Component: TComponent; Designer: IDesigner; StatementType: TStatementType = stQuery); reintroduce;
    destructor Destroy; override;
    procedure CheckRename; override;
    procedure CheckConnectionChange(InternalCheck: boolean); override;
    procedure CheckChanges; override;

    property SqlSourceNotifier: TCustomSourceNotifier read FSqlSourceNotifier;
  end;

implementation

uses
  DADesign, Registry, Download, DAConsts,
  SysUtils, ComObj, TypInfo;

const
  SConnectionName = '_ConnName_';
  SConnStrError = 'ConnectionString error';
  SCompilePrefix = 'compile';
  SEditPrefix = 'edit';

{$IFDEF WIN32_64}
  function ToWideChar(s: WideString): PWideChar;
  begin
    if s = '' then
      Result := nil
    else
      Result := SysAllocString(PWideChar(s));
  end;
{$ENDIF}

{ TSqlSourceFS }

procedure TSqlSourceFS.Close;
begin
  Assert(DBTools.DesignNotification <> nil);
  DBTools.DesignNotification.SqlSourceList.Delete(FComponent, False);
end;

function TSqlSourceFS.GetSourceNotifierClass: TCustomSourceNotifierClass;
begin
  Result := TSourceNotifierFS;
end;

procedure TSqlSourceFS.FreeSourceNotifier;
begin
  TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier := nil;
end;

function TSqlSourceFS.GetDBToolsService: TCustomDBToolsService;
begin
  Result := FDBToolsService;
end;

constructor TSqlSourceFS.Create(DBToolsService: TDBForgeStudioService; Component: TComponent; Designer: IDesigner; StatementType: TStatementType = stQuery);
var
  guid: TGUID;
begin
  inherited Create;
  Assert(DBToolsService <> nil);
  FComponent := Component;
  FDBToolsService := DBToolsService;
  FStatementType := StatementType;
  FLastName := Get_Name;
  FComponentSQL := GetSqlText;
  FDesigner := Designer;
  FDesignerName := DBTools.GetDesignerName(FDesigner);
  FLastConnection := FDBToolsService.GetConnection(Component);
  FLastConnectionString := FDBToolsService.GetNativeConnectionString(FLastConnection);

  if CoCreateGuid(guid) = S_OK then
    FID := GUIDToString(guid);
end;

destructor TSqlSourceFS.Destroy;
begin
  inherited;
end;

function TSqlSourceFS.GetParams: TDAParams;
begin
  with FDBToolsService.DADesignUtils do
    if HasParams(FComponent) then
      Result := GetParams(FComponent)
    else
      Result := nil;
end;

function TSqlSourceFS.Get_ConnectionString: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    FLastConnectionString
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TSqlSourceFS.Get_DesignerName: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar(FDesignerName){$ELSE}FDesignerName{$ENDIF};
end;

function TSqlSourceFS.Get_ID: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}FID{$IFDEF WIN32_64}){$ENDIF};
end;

function TSqlSourceFS.Get_Name: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    FComponent.Owner.Name + '-' + FComponent.Name + '-' + StatementTypeNames[FStatementType]
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TSqlSourceFS.GetSqlText: string;
var
  SQL: TStrings;
  Macros: TMacros;
  NewMacros: TDesignMacros;
begin
  with FDBToolsService.DADesignUtils do
    if IsStoredProc(FComponent) and (FStatementType = stQuery) then begin
      Result := Trim(GetFullName(FComponent));
      if Result <> '' then
        Result := GetObjectType(FComponent) + ':' + Result;
    end
    else begin
      SQL := GetSQL(FComponent, FStatementType);
      if FComponent is TCustomDAUpdateSQL then
        Result := SQL.Text
      else begin
        Macros := GetMacros(FComponent);
        NewMacros := TDesignMacros.Create(nil);
        try
          NewMacros.SetParserClass(TDBAccessUtils.GetParserClass(Macros));
          NewMacros.Assign(Macros);
          Result := SQL.Text;
          NewMacros.Expand(Result);        
        finally
          NewMacros.Free;
        end;
      end;
    end;
  if (FSqlTextPrefix <> '') and (FSqlTextPrefix <> SEditPrefix + '-' + StatementTypeNames[FStatementType]) then
    Result := FSqlTextPrefix + ':' + Result;
end;

procedure TSqlSourceFS.SetSqlText(Value: string);
var
  SQL: TStrings;
  Macros: TMacros;
  NewMacros: TDesignMacros;
begin
  with FDBToolsService.DADesignUtils do
    if not (IsStoredProc(FComponent) and (FStatementType = stQuery)) then begin
      SQL := GetSQL(FComponent, FStatementType);
      if FComponent is TCustomDAUpdateSQL then begin
        SQL.Text := Value;
        if (FDesigner <> nil) then
          FDesigner.Modified;
      end
      else begin
        Macros := GetMacros(FComponent);
        NewMacros := TDesignMacros.Create(nil);
        try
          NewMacros.SetParserClass(TDBAccessUtils.GetParserClass(Macros));
          NewMacros.Scan(Value);
          SQL.Text := Value;
          Macros.Assign(NewMacros);
          if (FDesigner <> nil) then
            FDesigner.Modified;
        finally
          NewMacros.Free;
        end;
      end;
    end;
end;

function TSqlSourceFS.Get_Sql: TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    WideString(GetSqlText)
  {$IFDEF WIN32_64}){$ENDIF};
end;

procedure TSqlSourceFS.Set_Sql(const Param1: TString);
begin
  SetSqlText(Param1);
end;

procedure TSqlSourceFS.GetParameter(const Index: integer; out Info: CommandParameterInfo);
begin
  Assert(Index < Get_ParameterCount);
  Info.Name := {$IFDEF WIN32_64}ToWideChar({$ENDIF}Params[Index].Name
    {$IFDEF WIN32_64}){$ENDIF};
  Info.DataType := FDBToolsService.DataTypeToDBForgeStudioType(Params[Index].DataType);
  Info.Value := Params[Index].Value;
  case Params[Index].ParamType of
    ptUnknown,
    ptInput:
      Info.ParameterType := ParameterType_Input;
    ptOutput:
      Info.ParameterType := ParameterType_Output;
    ptInputOutput:
      Info.ParameterType := ParameterType_InputOutput;
    ptResult:
      Info.ParameterType := ParameterType_ReturnValue;
  end;
end;

function TSqlSourceFS.Get_ParameterCount: integer;
begin
  if (Params <> nil) and (Params.Count > FParameterCount) then
    Set_ParameterCount(Params.Count);
  Result := FParameterCount;
end;

procedure TSqlSourceFS.Set_ParameterCount(const Value: integer);
var
  i, n: integer;
begin
  if Params = nil then
    Exit;
  FParameterCount := Value;
  SetLength(FParameterSetted, Value);
  n := Params.Count;
  for i := 0 to Value - 1 do begin
    if i >= n then
      Params.Add;
    FParameterSetted[i] := False;
  end;
  if (FDesigner <> nil) then
    FDesigner.Modified;
end;

procedure TSqlSourceFS.SetParameter(const Index: integer; Info: CommandParameterInfo);
var
  i, j: integer;
  TempParam: TDAParam;
begin
  Assert(Index < Get_ParameterCount);
  for i := 0 to Params.Count - 1 do
    if Params[i].Name = Info.Name then begin
      if i <> Index then begin
        TempParam := Params[Index];
        Params[Index] := Params[i];
        Params[i] := TempParam;
      end;
      Break;
    end;
  with TDAParam(Params[Index]) do begin
    Name := Info.Name;
    DataType := FDBToolsService.DBForgeStudioTypeToDataType(Info.DataType, DataType);
    Value := Variant(Info.Value);
    case Info.ParameterType of
      ParameterType_Input:
        ParamType := ptInput;
      ParameterType_Output:
        ParamType := ptOutput;
      ParameterType_InputOutput:
        ParamType := ptInputOutput;
      ParameterType_ReturnValue:
        ParamType := ptResult;
    end;
  end;
  for i := FParameterCount - 1 downto 0 do
    if not FParameterSetted[i] then
      Break
    else
      if i = 0 then
        for j := Params.Count - 1 downto FParameterCount do
          Params.Delete(j);
  if (FDesigner <> nil) then
    FDesigner.Modified;
end;

procedure TSqlSourceFS.CheckRename;
begin
  if (FLastName <> Get_Name) then begin
    if TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier <> nil then
      TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceRenamed(Get_Name);
    FLastName := Get_Name;
  end;
end;

procedure TSqlSourceFS.CheckConnectionChange(InternalCheck: boolean);
var
  NewConnection: TCustomDAConnection;
  NewConnectionString: string;
begin
  NewConnection := FDBToolsService.GetConnection(FComponent);
  if InternalCheck and (FLastConnection = NewConnection) then
    Exit;
  NewConnectionString := FDBToolsService.GetNativeConnectionString(NewConnection);
  if NewConnectionString <> FLastConnectionString then begin
    FLastConnectionString := NewConnectionString;
    if not InternalCheck and (TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier <> nil) then
      TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceChanged;
  end;
end;

procedure TSqlSourceFS.CheckChanges;
begin
  CheckRename;
  CheckConnectionChange(True);
  if GetSqlText <> FComponentSQL then begin
    if TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier <> nil then
      TSourceNotifierFS(FSqlSourceNotifier).FSqlSourceNotifier.OnSqlSourceChanged;
    FComponentSQL := GetSqlText;
  end;
end;

{ TDACSqlEditorFrameFS }

procedure TDACSqlEditorFrameFS.CheckModified;
begin
  if (SqlEditor <> nil) and (SqlEditor.Modified) then begin
    if Assigned(FOnChange) then
      FOnChange(Self);
    if Assigned(FOnExit) then
      FOnExit(Self);
    SqlEditor.Modified := False;
  end;
end;

constructor TDACSqlEditorFrameFS.Create(AOwner: TComponent; Component: TComponent;
  DBToolsService: TCustomDBToolsService);
begin
  inherited Create(AOwner, Component, DBToolsService);

  FInInit := True;
  FComponent := Component;
  FDBToolsService := TDBForgeStudioService(DBToolsService);
  BevelOuter := bvNone;
  FStatementType := stQuery;
  TabStop := True;

  DBTools.AddFrame(Self);
end;

destructor TDACSqlEditorFrameFS.Destroy;
begin
  DBTools.RemoveFrame(Self);

  inherited;
end;

procedure TDACSqlEditorFrameFS.EndInit;
begin
  FInInit := False;
end;

procedure TDACSqlEditorFrameFS.InternalResize;
begin
  if SqlEditor <> nil then
    Windows.SetWindowPos(GetSqlEditorHandle, 0, 0, 0, ClientWidth, ClientHeight, SWP_NOZORDER or SWP_SHOWWINDOW);
end;

procedure TDACSqlEditorFrameFS.Resize;
begin
  InternalResize;
  inherited;
end;

procedure TDACSqlEditorFrameFS.SetFocus;
begin
  inherited;

  if SqlEditor <> nil then
    Windows.SetFocus(GetSqlEditorHandle);
end;

procedure TDACSqlEditorFrameFS.WndProc(var Message: TMessage);
begin
  if not FInInit and (Message.Msg = WM_SETFOCUS) and (SqlEditor <> nil) then
    Windows.SetFocus(GetSqlEditorHandle)
  else
    inherited;
end;

procedure TDACSqlEditorFrameFS.CheckConnectionChange;
var
  NewConnectionString: string;
  st: TStatementType;
begin
  Assert(SqlEditor <> nil);

  NewConnectionString := FDBToolsService.GetNativeConnectionString(FComponent);
  if NewConnectionString <> FLastConnectionString then begin
    FLastConnectionString := NewConnectionString;

    if FLastConnectionString <> '' then
      for st := Low(TStatementType) to High(TStatementType) do
        if FSqlEditors[st] <> nil then
          FSqlEditors[st].SetConnection({$IFDEF WIN32_64}ToWideChar({$ENDIF}
            FLastConnectionString
          {$IFDEF WIN32_64}){$ENDIF});
  end;
end;

function TDACSqlEditorFrameFS.GetSqlEditorHandle: HWND;
begin
  Assert(SqlEditor <> nil);

  Result := SqlEditor.Handle;
end;

function TDACSqlEditorFrameFS.GetText: string;
begin
  if SqlEditor <> nil then
    Result := SqlEditor.Text
  else
    Result := '';
end;

procedure TDACSqlEditorFrameFS.SetText(const Value: string);
begin
  if SqlEditor <> nil then
    SqlEditor.Text := {$IFDEF WIN32_64}ToWideChar(Value){$ELSE}Value{$ENDIF};
end;

procedure TDACSqlEditorFrameFS.SetReadOnly(Value: boolean);
var
  st: TStatementType;
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    for st := Low(TStatementType) to High(TStatementType) do
       if FSqlEditors[st] <> nil then
          FSqlEditors[st].ReadOnly := Value;
    SetStatementType(FStatementType);
  end;
end;

function TDACSqlEditorFrameFS.GetSqlEditor: ISqlEditor;
begin
  if (not ReadOnly) or (FSqlEditors[FStatementType] <> nil) then
    Result := FSqlEditors[FStatementType]
  else
    Result := FSqlEditors[stQuery];
end;

procedure TDACSqlEditorFrameFS.SetStatementType(const Value: TStatementType);
var
  NewStatementType: TStatementType;
  FHide, FShow, FFocused: boolean;
begin
  if FReadOnly and (FSqlEditors[Value] = nil) then
    NewStatementType := stQuery
  else
    NewStatementType := Value;
  FHide := NewStatementType <> FStatementType;
  FShow := FHide;
  if FSqlEditors[NewStatementType] = nil then
    if (FSqlEditors[stQuery] <> nil) and (FSqlEditors[stQuery].Text = '') then begin
      FSqlEditors[NewStatementType] := FSqlEditors[stQuery];
      FSqlEditors[stQuery] := nil;
      FHide := False;
      FShow := False;
    end
    else begin
      FSqlEditors[NewStatementType] := FDBToolsService.GetSqlEditor;
      if FSqlEditors[NewStatementType] = nil then
        Abort;
      if FLastConnectionString <> '' then
        FSqlEditors[NewStatementType].SetConnection({$IFDEF WIN32_64}ToWideChar({$ENDIF}
          FLastConnectionString
        {$IFDEF WIN32_64}){$ENDIF});
      FSqlEditors[NewStatementType].Text := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
      FShow := True;
    end;
  if FHide and (FSqlEditors[NewStatementType] <> nil) then begin
    Windows.SetParent(GetSqlEditorHandle, MAXDWORD - 2{HWND_MESSAGE});
    FFocused := Windows.GetFocus = GetSqlEditorHandle;
  end
  else
    FFocused := False;

  FStatementType := Value;
  if FShow then begin
    Windows.SetParent(GetSqlEditorHandle, Handle);
    FSqlEditors[NewStatementType].ReadOnly := FReadOnly;
    InternalResize;
    if FFocused then
      Windows.SetFocus(GetSqlEditorHandle);
  end;
end;

{ TSourceNotifierFS }

procedure TSourceNotifierFS.OnSqlSourceDeleted;
begin
  if FSqlSourceNotifier <> nil then
    FSqlSourceNotifier.OnSqlSourceDeleted;
end;

{ TDBForgeStudioService }

constructor TDBForgeStudioService.Create(ADADesignUtils: TDADesignUtilsClass; ASqlService: {$IFDEF WIN32_64}IUnknown{$ELSE}TObject{$ENDIF}; ADefaultConnectionStr: string);
begin
  inherited Create(ADADesignUtils, ASqlService, ADefaultConnectionStr);

  FConnectionStrList := TStringList.Create;
  FConnectionsList := TStringList.Create;
  FDefaultConnectionList := TStringList.Create;
  FDADesignUtils := ADADesignUtils;

  FDBForgeStudioServiceEvents := TDBForgeStudioServiceEvents.Create();
  FDBForgeStudioServiceEvents.Deactivate := OnServerDeactivate;

{$IFDEF WIN32_64}
  ASqlService.QueryInterface(IDbForgeService, FSqlService);
{$ELSE}
  FSqlService := ASqlService as IDbForgeService;
{$ENDIF}
  ConnStrToList(ADefaultConnectionStr, FDefaultConnectionList);
end;

destructor TDBForgeStudioService.Destroy;
begin
{$IFDEF WIN32_64}
  PInteger(@FSqlService)^ := 0; // To prevent _Release calling
{$ENDIF}
  FDefaultConnectionList.Free;
  FConnectionStrList.Free;
  FConnectionsList.Free;

  inherited;
end;

procedure TDBForgeStudioService.OnServerDeactivate(Sender: TObject);
begin
//
end;

function TDBForgeStudioService.DataTypeToDBForgeStudioType(AType: TFieldType): integer;
var
  i: integer;
begin
  Assert(Length(FParamTypeMaps) > 0);
  Result := FParamTypeMaps[0].DBToolsType;
  for i := 0 to High(FParamTypeMaps) do
    if FParamTypeMaps[i].DACType = AType then begin
      Result := FParamTypeMaps[i].DBToolsType;
      Break;
    end;
end;

function TDBForgeStudioService.DBForgeStudioTypeToDataType(AType: integer; OldType: TFieldType): TFieldType;
var
  i: integer;
begin
  Result := ftUnknown;
  for i := 0 to High(FParamTypeMaps) do
    if FParamTypeMaps[i].DBToolsType = AType then begin
      if (Result = ftUnknown) or (FParamTypeMaps[i].DACType = OldType) then
        Result := FParamTypeMaps[i].DACType;
      if Result = OldType then
        Break;
    end
    else
      if Result <> ftUnknown then
        Break;
end;

procedure TDBForgeStudioService.BeginConnectionStrGetting(const ConnectionStrList: TStringList);
begin
  FUsedConnectionStrList := ConnectionStrList;
  SetLength(FUsedConnectionCompareFlags, 0);
end;

procedure TDBForgeStudioService.DesignerClosing(DesignerName: string);
begin
  FSqlService.DesignerClosing(DesignerName);
end;

class function TDBForgeStudioService.GetNamespace: string;
begin
  Result := 'Devart.DbForge';
end;

procedure TDBForgeStudioService.PutConnectionParam(const ConnectionParam: string; const CompareFlag: TCompareFlag = cfNormal);
var
  i: integer;
begin
  i := Length(FUsedConnectionCompareFlags);
  Assert (i < FDefaultConnectionList.Count);
  FUsedConnectionStrList.Values[FDefaultConnectionList.Names[i]] := ConnectionParam;
  SetLength(FUsedConnectionCompareFlags, i + 1);
  FUsedConnectionCompareFlags[i] := CompareFlag;
end;

procedure TDBForgeStudioService.SkipConnectionParams(const Count: integer);
var
  i, j: integer;
begin
  i := Length(FUsedConnectionCompareFlags);
  Assert (i + Count <= FDefaultConnectionList.Count);
  SetLength(FUsedConnectionCompareFlags, i + Count);
  for j := i to i + Count - 1 do
    FUsedConnectionCompareFlags[j] := cfNone;
end;

class function TDBForgeStudioService.GetConnectionParamStr(const ParamName, ParamValue: string): string;
var
  i: integer;
  QuoteChar: char;
begin
  Result := ParamName + '=';
  if Pos('''', ParamValue) > 0 then
    QuoteChar := '"'
  else
    if Pos('"', ParamValue) > 0 then
      QuoteChar := ''''
    else
      if (Pos(' ', ParamValue) > 0) or (Pos(';', ParamValue) > 0) then
        QuoteChar := '"'
      else
        QuoteChar := #0;
  if QuoteChar <> #0 then
    Result := Result + QuoteChar;
  for i := 1 to Length(ParamValue) do begin
    if (QuoteChar <> #0) and (ParamValue[i] = QuoteChar) then
      Result := Result + QuoteChar;
    Result := Result + ParamValue[i];
  end;
  if QuoteChar <> #0 then
    Result := Result + QuoteChar;
  Result := Result + ';';
end;

function TDBForgeStudioService.GetNativeConnectionString(const Component: TComponent): string;
var
  i, n: integer;
  DefStr: string;
  Connection: TCustomDAConnection;
  ConnectionList: TStringList;
begin
  Result := '';
  Connection := GetConnection(Component);
  if Connection <> nil then begin
    ConnectionList := TStringList.Create;
    try
      BeginConnectionStrGetting(ConnectionList);
      DADesignUtils.GetDBToolsConnectionList(Connection);
      n := ConnectionList.Count - 1;
      for i := n downto 0 do begin
        DefStr := FDefaultConnectionList.Values[ConnectionList.Names[i]];
        if (DefStr <> '') and (DefStr = ConnectionList.Values[ConnectionList.Names[i]]) then
          ConnectionList.Delete(i);
      end;
      for i := 0 to ConnectionList.Count - 1 do
         Result := Result + GetConnectionParamStr(ConnectionList.Names[i], ConnectionList.Values[ConnectionList.Names[i]]);
    finally
      ConnectionList.Free;
    end;
  end;
end;

function TDBForgeStudioService.GetConnectionString(const Component: TComponent): TString;
begin
  Result := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
    GetNativeConnectionString(Component)
  {$IFDEF WIN32_64}){$ENDIF};
end;

function TDBForgeStudioService.GetConnectionStringObjectTypeAndFullName(const Component: TComponent; out ConnectionString, ObjectType, FullName: TString): boolean;
begin
  ConnectionString := GetConnectionString(Component);
  Result := ConnectionString <> '';
  if Result then begin
    FullName := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
      DADesignUtils.GetFullName(Component){$IFDEF WIN32_64}){$ENDIF};
    ObjectType := {$IFDEF WIN32_64}ToWideChar({$ENDIF}
      DADesignUtils.GetObjectType(Component){$IFDEF WIN32_64}){$ENDIF};
    if not(Component is TCustomDAConnection) then
      Result := Length(FullName) > 0;
  end
  else begin
    FullName := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
    ObjectType := {$IFDEF WIN32_64}nil{$ELSE}''{$ENDIF};
  end;
end;

procedure TDBForgeStudioService.GetConnections(NameList: TStrings; Condition: string = '');
var
  ConnectionInfoArray: TConnectionInfoArray;
  i: integer;
  Connection: {$IFDEF WIN32_64}PConnectionInfo{$ELSE}ConnectionInfo{$ENDIF};
begin
  ConnectionInfoArray := FSqlService.GetConnections;
  NameList.BeginUpdate;
  FConnectionsList.BeginUpdate;
{$IFDEF WIN32_64}
  Connection := AccessData(ConnectionInfoArray);
{$ENDIF}
  try
    FConnectionsList.Clear;
    NameList.Clear;
    for i := 0 to {$IFDEF WIN32_64}DataHigh{$ELSE}High{$ENDIF}(ConnectionInfoArray) do begin
    {$IFDEF CLR}
      Connection := ConnectionInfoArray[i];
    {$ENDIF}
      if (Condition = '') or (Pos(Condition, string(Connection.ConnectionString)) > 0) then begin
        NameList.Add(Connection.Name);
        FConnectionsList.Add(GetConnectionValueStr(Connection.Name) + Connection.ConnectionString);
      end;
    {$IFDEF WIN32_64}
      Inc(Connection);
    {$ENDIF}
    end;
  finally
    NameList.EndUpdate;
    FConnectionsList.EndUpdate;
  {$IFDEF WIN32_64}
    UnaccessData(ConnectionInfoArray);
  {$ENDIF}
  end;
end;

function TDBForgeStudioService.FindConnectionName(AConnection: TCustomDAConnection): string;
var
  i, j: integer;
  AConnectionStrList: TStringList;
  Str1, Str2, DefStr: string;

  function ToCommonCase(const s: string): string;
  var
    ts: string;
  begin
    Result := s;
    if FUsedConnectionCompareFlags[j] = cfNormal then begin
      ts := Trim(Result);
      if (Length(ts) < 2) or (ts[1] <> '"') or (ts[Length(ts)] <> '"') then
        Result := UpperCase(Result);
    end;
  end;

begin
  AConnectionStrList := TStringList.Create;
  try
    BeginConnectionStrGetting(AConnectionStrList);
    DADesignUtils.GetDBToolsConnectionList(AConnection);
    i := Length(FUsedConnectionCompareFlags);
    SetLength(FUsedConnectionCompareFlags, FDefaultConnectionList.Count);
    for j := i to FDefaultConnectionList.Count - 1 do
      FUsedConnectionCompareFlags[j] := cfNormal;
    for i := 0 to FConnectionsList.Count - 1 do begin
      ConnStrToList(FConnectionsList[i], FConnectionStrList);
      for j := 0 to FDefaultConnectionList.Count - 1 do begin
        if FUsedConnectionCompareFlags[j] = cfNone then
          Continue;

        DefStr := FDefaultConnectionList.Values[FDefaultConnectionList.Names[j]];
        Str1 := ToCommonCase(AConnectionStrList.Values[FDefaultConnectionList.Names[j]]);
        Str2 := ToCommonCase(FConnectionStrList.Values[FDefaultConnectionList.Names[j]]);
        if (Str1 <> Str2) and (DefStr <> '') then begin
          if Str1 = '' then
            Str1 := ToCommonCase(DefStr);
          if Str2 = '' then
            Str2 := ToCommonCase(DefStr);
        end;
        if Str1 <> Str2 then
          Break
        else
          if j = FDefaultConnectionList.Count - 1 then begin
            Result := FConnectionStrList.Values[SConnectionName];
            Exit;
          end;
      end;
    end;
    Result := '';
  finally
    AConnectionStrList.Free;
  end;
end;

function TDBForgeStudioService.GetConnectionStrList(ConnectionName: string): TStringList;
var
  i, j, k: integer;
  s: string;
begin
  Result := FConnectionStrList;
  s := GetConnectionValueStr(ConnectionName);
  for i := 0 to FConnectionsList.Count - 1 do
    if (Length(FConnectionsList[i]) >= Length(s)) and
      (Copy(FConnectionsList[i], 1, Length(s)) = s) then begin
      ConnStrToList(Copy(FConnectionsList[i], Length(s) + 1, Length(FConnectionsList[i]) - Length(s)), Result);

      for j := 0 to FDefaultConnectionList.Count - 1 do begin
        k := Result.IndexOfName(FDefaultConnectionList.Names[j]);
        if k < 0 then
          Result.Add(FDefaultConnectionList[j])
        else
          if Result.Values[Result.Names[k]] = '' then
            Result[k] := FDefaultConnectionList[j];
      end;
      Exit;
    end;
  Result.Clear;
end;

procedure TDBForgeStudioService.CheckConnection(const Component: TComponent);
var
  Connection: TCustomDAConnection;
begin
  Connection := GetConnection(Component);
  if (Connection = nil) or (GetConnectionString(Component) = '') then
    DatabaseError(SConnectionNotDefined);
end;

procedure TDBForgeStudioService.FindInDatabaseExplorer;
var
  ConnectionString, ObjectType, FullName: TString;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  if GetConnectionStringObjectTypeAndFullName(FCurrentComponent, ConnectionString, ObjectType, FullName) then
    SqlService.FindInDatabaseExplorer(ConnectionString, ObjectType, FullName);
end;

procedure TDBForgeStudioService.EditDatabaseObject;
var
  ConnectionString, ObjectType, FullName: TString;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  if GetConnectionStringObjectTypeAndFullName(FCurrentComponent, ConnectionString, ObjectType, FullName) then
    SqlService.EditDatabaseObject(ConnectionString, ObjectType, FullName);
end;

procedure TDBForgeStudioService.ExecuteSql(Debug: boolean);
var
  SqlSource: TSqlSourceFS;
  SqlSourceNotifier: ISqlSourceNotifier;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner);
  SqlService.ExecuteSql(SqlSource, Debug, SqlSourceNotifier);
  TSourceNotifierFS(SqlSource.SqlSourceNotifier).FSqlSourceNotifier := SqlSourceNotifier;
  if SqlSource.SqlSourceNotifier = nil then
    DBTools.DesignNotification.SqlSourceList.Delete(SqlSource);
end;

procedure TDBForgeStudioService.Compile(Debug: boolean);
var
  SqlSource: TSqlSourceFS;
  SqlSourceNotifier: ISqlSourceNotifier;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner, SCompilePrefix);
  SqlService.ExecuteSql(SqlSource, Debug, SqlSourceNotifier);
  TSourceNotifierFS(SqlSource.SqlSourceNotifier).FSqlSourceNotifier := SqlSourceNotifier;
  if SqlSource.SqlSourceNotifier = nil then
    DBTools.DesignNotification.SqlSourceList.Delete(SqlSource);
end;

procedure TDBForgeStudioService.RetrieveData(AsDocument: boolean);
var
  SqlSource: TSqlSourceFS;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner);
  if (SqlSource.FComponent is TCustomDADataSet) and (Trim(TCustomDADataSet(SqlSource.FComponent).SQL.Text) = '') then
    DatabaseError('SQL statement is not specified.');
  SqlService.RetrieveData(SqlSource, AsDocument);
end;

procedure TDBForgeStudioService.EditSql(AsQuery: boolean; StatementType: TStatementType = stQuery);
var
  SqlSourceNotifier: ISqlSourceNotifier;
  SqlSource: TSqlSourceFS;
begin
  FDADesignUtils.CheckComponent(FCurrentComponent);
  CheckConnection(FCurrentComponent);
  SqlSource := GetSqlSource(FCurrentComponent, FCurrentDesigner, SEditPrefix + '-' + StatementTypeNames[StatementType], StatementType);
  SqlService.EditSql(SqlSource, AsQuery, SqlSourceNotifier);
  TSourceNotifierFS(SqlSource.SqlSourceNotifier).FSqlSourceNotifier := SqlSourceNotifier;   
  if SqlSource.SqlSourceNotifier = nil then
    DBTools.DesignNotification.SqlSourceList.Delete(SqlSource);  
end;

function TDBForgeStudioService.GetDACSqlEditorFrameClass: TCustomDACSqlEditorFrameClass;
begin
  Result := TDACSqlEditorFrameFS;
end;

function TDBForgeStudioService.SqlSourceClass: TCustomSqlSourceClass;
begin
  Result := TSqlSourceFS;
end;

function TDBForgeStudioService.GetSqlEditor: ISqlEditor;
begin
  SqlService.CreateSqlEditor(FDBForgeStudioServiceEvents, Result);
end;

function TDBForgeStudioService.GetSqlSource(Component: TComponent; Designer: IDesigner; SqlTextPrefix: string = ''; StatementType: TStatementType = stQuery): TSqlSourceFS;
begin
  Assert(DBTools.DesignNotification <> nil);
  Result := TSqlSourceFS(DBTools.DesignNotification.SqlSourceList.Find(Component, SqlTextPrefix));
  if Result = nil then begin
    Result := TSqlSourceFS.Create(Self, Component, Designer, StatementType);
    Result.SqlTextPrefix := SqlTextPrefix;
    DBTools.DesignNotification.SqlSourceList.Add(Result);
  end;
end;

procedure TDBForgeStudioService.AddParamTypeMap(ADACType: TFieldType; ADBToolsType: integer);
var
  n: integer;
begin
  n := Length(FParamTypeMaps);
  SetLength(FParamTypeMaps, n + 1);
  with FParamTypeMaps[n] do begin
    DACType := ADACType;
    DBToolsType := ADBToolsType;
  end;
end;

{$IFDEF WIN32_64}
class function TDBForgeStudioService.AccessData(const V: PSafeArray): pointer;
begin
  if V = nil then
    Result := nil
  else
    SafeArrayCheck(SafeArrayAccessData(V, Result));
end;

class procedure TDBForgeStudioService.UnaccessData(const V: PSafeArray);
begin
  if V <> nil then
    SafeArrayCheck(SafeArrayUnaccessData(V));
end;

class function TDBForgeStudioService.DataHigh(const V: PSafeArray): integer;
begin
  if V = nil then
    Result := -1
  else
    SafeArrayGetUBound(V, 1, Result);
end;
{$ENDIF}

function TDBForgeStudioService.GetConnectionValueStr(ConnectionName: string): string;
begin
  Result := SConnectionName + '=' + ConnectionName + ';';
end;

procedure TDBForgeStudioService.ConnStrToList(ConnStr: string; const ConnList: TStrings);
var
  ParamName, ParamValue: string;
  QuoteChar: char;
  Quoted: boolean;
  i, l: integer;
begin
  ConnStr := Trim(ConnStr);
  ConnList.BeginUpdate;
  ConnList.Clear;
  try
    if ConnStr = '' then
      Exit;
    if ConnStr[Length(ConnStr)] <> ';' then
      ConnStr := ConnStr + ';';
    repeat
      i := Pos('=', ConnStr);
      if i <= 0 then
        Break;
      ParamName := Copy(ConnStr, 1, i); //with '='
      Inc(i);
      QuoteChar := ConnStr[i];
      Quoted := AnsiChar(QuoteChar) in ['''', '"'];
      if Quoted then
        Inc(i);
      ParamValue := '';
      repeat
        if i + Ord(Quoted) > Length(ConnStr) then
          raise Exception.Create(SConnStrError);

        if Quoted then
          if ConnStr[i] = QuoteChar then begin
            Quoted := ConnStr[i + 1] <> ';';
            if (ConnStr[i + 1] = QuoteChar) or not Quoted then
              Inc(i)
            else
              raise Exception.Create(SConnStrError);
          end;

        if not Quoted and (ConnStr[i] = ';') then
          Break
        else
          ParamValue := ParamValue + ConnStr[i];
        Inc(i);
      until False;
      ConnList.Add(ParamName + ParamValue);
      l := Length(ConnStr) - i;
      if l > 0 then
        ConnStr := Copy(ConnStr, i + 1, l)
      else
        Break;
    until False;
  finally
    ConnList.EndUpdate;
  end;
end;

{ TDBForgeStudioServiceEvents }

procedure TDBForgeStudioServiceEvents.OnDeactivate;
begin
  if Assigned(FOnDeactivate) then
    FOnDeactivate(Self);
end;

end.
