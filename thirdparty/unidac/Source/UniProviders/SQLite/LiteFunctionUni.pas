
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF VIRTUAL_QUERY}
{$I LiteDac.inc}
unit LiteFunctionUni;
{$ENDIF}

interface

uses
  SysUtils, Types, Classes, Variants,
{$IFDEF CLR}
  System.Text, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRTypes, CRAccess, DAConsts, MemUtils,
{$IFDEF VIRTUAL_QUERY}
  LiteCallVirtual, LiteErrorVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteCall, LiteError;
{$ELSE}
  LiteCallUni, LiteErrorUni;
{$ENDIF}
{$ENDIF}

type
  TLiteFunction = function(InValues: array of Variant): Variant;
  TLiteStepFunction = procedure(InValues: array of Variant);
  TLiteFinalFunction = function: Variant;
  TLiteFunctionMethod = procedure(InValues: array of Variant; var ResultValue: Variant) of object;
  TLiteStepFunctionMethod = procedure(InValues: array of Variant) of object;
  TLiteFinalFunctionMethod = procedure(var ResultValue: Variant) of object;

  TCustomLiteFunctionDesc = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRConnection;
    FName: string;
    FParamCount: Integer;
    FTextRepresentation: Integer;

    procedure DoRegister(const pName: PAnsiChar; const pSelf: IntPtr); virtual; abstract;
    procedure DoRegister387(const pName: PAnsiChar; const pSelf: IntPtr); virtual; abstract;
    procedure DoRegister3250(const pName: PAnsiChar; const pSelf: IntPtr); virtual; abstract;
  protected
    procedure RegisterFunction;
    procedure UnregisterFunction;

    function GetInParams(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr): TVariantArray;
    procedure SetResult(Context: pSQLite3Context; Value: Variant);

    procedure DoFunction(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr); virtual; abstract;
    procedure DoStep(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr); virtual; abstract;
    procedure DoFinal(Context: pSQLite3Context); virtual; abstract;
  public
    constructor Create(Connection: TCRConnection; const Name: string; ParamCount: Integer);
    destructor Destroy; override;

    property Name: string read FName;
    property ParamCount: Integer read FParamCount;
    property TextRepresentation: Integer read FTextRepresentation;
  end;

  TLiteFunctionDesc = class(TCustomLiteFunctionDesc)
  private
    FLiteFunction: TLiteFunction;
    FStepFunction: TLiteStepFunction;
    FFinalFunction: TLiteFinalFunction;
    FLiteFunctionMethod: TLiteFunctionMethod;
    FStepFunctionMethod: TLiteStepFunctionMethod;
    FFinalFunctionMethod: TLiteFinalFunctionMethod;

    procedure DoRegister(const pName: PAnsiChar; const pSelf: IntPtr); override;
    procedure DoRegister387(const pName: PAnsiChar; const pSelf: IntPtr); override;
    procedure DoRegister3250(const pName: PAnsiChar; const pSelf: IntPtr); override;
  protected
    procedure DoFunction(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr); override;
    procedure DoStep(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr); override;
    procedure DoFinal(Context: pSQLite3Context); override;
  public
    constructor Create(Connection: TCRConnection; const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction); overload;
    constructor Create(Connection: TCRConnection; const Name: string; ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction); overload;
    constructor Create(Connection: TCRConnection; const Name: string; ParamCount: Integer; LiteFunctionMethod: TLiteFunctionMethod); overload;
    constructor Create(Connection: TCRConnection; const Name: string; ParamCount: Integer; StepFunctionMethod: TLiteStepFunctionMethod; FinalFunctionMethod: TLiteFinalFunctionMethod); overload;

    property LiteFunction: TLiteFunction read FLiteFunction write FLiteFunction;
  end;

  TLiteFunctionDescClass = class of TLiteFunctionDesc;

  TSQLiteFunctionManager = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRConnection;
    FFunctionList: TCRObjectList;
  protected
    function GetFunctionDescClass: TLiteFunctionDescClass; virtual;
    procedure InternalAddFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
    procedure InternalRemoveFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
    function FindFunction(const Name: string; ParamCount: Integer): TCustomLiteFunctionDesc;
  public
    constructor Create(Connection: TCRConnection);
    destructor Destroy; override;

    procedure RegisterFunction(const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction); overload;
    procedure RegisterAggregateFunction(const Name: string; ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction); overload;
    procedure RegisterFunction(const Name: string; ParamCount: Integer; LiteFunctionMethod: TLiteFunctionMethod); overload;
    procedure RegisterAggregateFunction(const Name: string; ParamCount: Integer; StepFunctionMethod: TLiteStepFunctionMethod; FinalFunctionMethod: TLiteFinalFunctionMethod); overload;
    procedure UnRegisterFunction(const Name: string; ParamCount: Integer);

    procedure RegistrAllFunctions;
    procedure UnRegistrAllFunctions;
  end;

  TSQLiteFunctionManagerClass = class of TSQLiteFunctionManager;

implementation

uses
  CRFunctions,
{$IFDEF VIRTUAL_QUERY}
  LiteClassesVirtual;
{$ELSE}
{$IFDEF UNIDACPRO}
  LiteClassesUni;
{$ELSE}
  LiteClasses;
{$ENDIF}
{$ENDIF}

var
  CallBackLiteFunctionPtr: IntPtr;
  CallBackStepFunctionPtr: IntPtr;
  CallBackFinalFunctionPtr: IntPtr;
  CallBackLiteFunctionPtr387: IntPtr;
  CallBackStepFunctionPtr387: IntPtr;
  CallBackFinalFunctionPtr387: IntPtr;
  CallBackLiteFunctionPtr3250: IntPtr;
  CallBackStepFunctionPtr3250: IntPtr;
  CallBackFinalFunctionPtr3250: IntPtr;

{ Function }

procedure CallBackLiteFunction(Context: pContext; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFunction(Context, ParamCount, pData);
end;

procedure CallBackStepFunction(Context: pContext; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoStep(Context, ParamCount, pData);
end;

procedure CallBackFinalFunction(Context: pContext); cdecl;
var
  pFunc: pFuncDef;
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFunc^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFInal(Context);
end;

procedure CallBackLiteFunction387(Context: pContext387; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFunction(Context, ParamCount, pData);
end;

procedure CallBackStepFunction387(Context: pContext387; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoStep(Context, ParamCount, pData);
end;

procedure CallBackFinalFunction387(Context: pContext387); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFinal(Context);
end;

procedure CallBackLiteFunction3250(Context: pContext387; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef3250(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFunction(Context, ParamCount, pData);
end;

procedure CallBackStepFunction3250(Context: pContext387; ParamCount: Integer; pData: IntPtr); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef3250(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoStep(Context, ParamCount, pData);
end;

procedure CallBackFinalFunction3250(Context: pContext387); cdecl;
var
  pFunc,
  pUserData: IntPtr;
  functionDesc: TCustomLiteFunctionDesc;
begin
  if Context = nil then
    Exit;
  pFunc := Context^.pFunc;
  if pFunc = nil then
    Exit;

  pUserData := pFuncDef3250(pFunc)^.pUserData;

  functionDesc := TCustomLiteFunctionDesc(pUserData);
  functionDesc.DoFinal(Context);
end;

{ TCustomLiteFunctionDesc }

constructor TCustomLiteFunctionDesc.Create(Connection: TCRConnection; const Name: string; ParamCount: Integer);
begin
  inherited Create;

  FConnection := Connection;
  FName := Name;
  FParamCount := ParamCount;

  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    FTextRepresentation := SQLITE_UTF16
  else
    FTextRepresentation := SQLITE_UTF8;
end;

destructor TCustomLiteFunctionDesc.Destroy;
begin
  inherited;
end;

procedure TCustomLiteFunctionDesc.RegisterFunction;
var
  pName: PAnsiChar;
  pSelf: IntPtr;
  sa: AnsiString;
  Ver: string;
  VerMajor, VerMinor, VerRelease, n: integer;
begin
  pSelf := Self;
  sa := TSQLiteConnection(FConnection).EncodeString(Name);
  pName := Marshal.StringToHGlobalAnsi(sa);
  try
    Ver := TSQLiteConnection(FConnection).GetClientVersion;
    n := pos('.', Ver);
    VerMajor := StrToIntDef(copy(Ver, 1, n - 1), 0);
    delete(Ver, 1, n);
    n := pos('.', Ver);
    VerMinor := StrToIntDef(copy(Ver, 1, n - 1), 0);
    delete(Ver, 1, n);
    n := pos('.', Ver);
    if n > 0 then
      VerRelease := StrToIntDef(copy(Ver, 1, n - 1), 0)
    else
      VerRelease := StrToIntDef(Ver, 0);

    if (VerMajor > 3) or ((VerMajor = 3) and (VerMinor >= 25)) then
      DoRegister3250(pName, pSelf)
    else if (VerMajor > 3) or ((VerMajor = 3) and ((VerMinor > 8) or ((VerMinor = 8) and (VerRelease >= 7)))) then
      DoRegister387(pName, pSelf)
    else
      DoRegister(pName, pSelf);
  finally
    Marshal.FreeCoTaskMem(pName);
  end;
end;

procedure TCustomLiteFunctionDesc.UnregisterFunction;
var
  pName: PAnsiChar;
  sa: AnsiString;
begin
  if FConnection.GetConnected then begin
    sa := TSQLiteConnection(FConnection).EncodeString(Name);
    pName := Marshal.StringToHGlobalAnsi(sa);
    try
      TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, nil, nil, nil, nil);
    finally
      Marshal.FreeCoTaskMem(pName);
    end;
  end;
end;

function TCustomLiteFunctionDesc.GetInParams(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr): TVariantArray;
var
  i: integer;
  liteConnection: TSQLiteConnection;
  ParamType: Integer;
  ParamSize: Integer;
  pParam: IntPtr;
  aStrParam: AnsiString;
  wStrParam: WideString;
  blobData: TBytes;
begin
  SetLength(Result, ParamCount);

  liteConnection := TSQLiteConnection(FConnection);
  for i := 0 to ParamCount - 1 do begin
    pParam := Marshal.ReadIntPtr(pData, i * sizeof(IntPtr));
    ParamType := liteConnection.API.sqlite3_value_type(pParam);

    case ParamType of
      SQLITE_INTEGER:
        Result[i] := liteConnection.API.sqlite3_value_int64(pParam);
      SQLITE_FLOAT:
        Result[i] := liteConnection.API.sqlite3_value_double(pParam);
      SQLITE_TEXT:
        if liteConnection.IsUnicodeDataBase then begin
          ParamSize := liteConnection.API.sqlite3_value_bytes16(pParam);
          wStrParam := Marshal.PtrToStringUni(liteConnection.API.sqlite3_value_text16(pParam), ParamSize shr 1);
          if liteConnection.UseUnicode then
            Result[i] := wStrParam
          else
            Result[i] := AnsiString(wStrParam);
        end
        else begin
          ParamSize := liteConnection.API.sqlite3_value_bytes(pParam);
          aStrParam := Marshal.PtrToStringAnsi(liteConnection.API.sqlite3_value_text(pParam), ParamSize);
          Result[i] := aStrParam;
        end;
      SQLITE_BLOB: begin
        ParamSize := liteConnection.API.sqlite3_value_bytes(pParam);
        SetLength(blobData, ParamSize);
        Marshal.Copy(liteConnection.API.sqlite3_value_blob(pParam), blobData, 0, ParamSize);
        Result[i] := blobData;
      end;
      SQLITE_NULL:
        Result[i] := null;
    else
      raise Exception.Create(SUnknownDataType);
    end;
  end;
end;

procedure TCustomLiteFunctionDesc.SetResult(Context: pSQLite3Context; Value: Variant);
var
  liteConnection: TSQLiteConnection;
  liteType: Integer;
  needConvertToText: boolean;
  intValue: Integer;
  floatValue: Double;
  aStrValue: AnsiString;
  pStrValue: IntPtr;
  pBlobValue: IntPtr;
  pDestrType: IntPtr;
begin
  liteConnection := TSQLiteConnection(FConnection);

  if VarType(Value) = varNull then begin
    liteConnection.API.sqlite3_result_null(Context);
    Exit;
  end;

  liteType := liteConnection.GetTypes.GetSQLiteType(liteConnection.GetTypes.GetVarType(VarType(Value)), needConvertToText);

  case liteType of
    SQLITE_INTEGER: begin
      intValue := Value;
      liteConnection.API.sqlite3_result_int64(Context, intValue);
    end;
    SQLITE_FLOAT: begin
      floatValue := Value;
      liteConnection.API.sqlite3_result_double(Context, floatValue);
    end;
    SQLITE_TEXT: begin
      if needConvertToText then
        aStrValue := liteConnection.GetTypes.ConvertToText(liteConnection.GetTypes.GetVarType(VarType(Value)), 0, Value)
      else
        aStrValue := liteConnection.GetTypes.ConvertMemo(Value);

      pStrValue := Marshal.StringToHGlobalAnsi(aStrValue);
      try
        liteConnection.API.sqlite3_result_text(Context, pStrValue, LengthA(aStrValue), SQLITE_TRANSIENT);
      finally
        FreeString(pStrValue);
      end;
    end;
    SQLITE_BLOB: begin
      pBlobValue := liteConnection.GetTypes.ConvertBlob(Value, pDestrType);
      liteConnection.API.sqlite3_result_blob(Context, pBlobValue, Length(TBytes(pBlobValue)), pDestrType);
    end;
    SQLITE_NULL:
      liteConnection.API.sqlite3_result_null(Context);
  else
    raise Exception.Create(SUnknownDataType);
  end;
end;

{ TLiteFunctionDesc }

constructor TLiteFunctionDesc.Create(Connection: TCRConnection; const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction);
begin
  inherited Create(Connection, Name, ParamCount);

  FLiteFunction := LiteFunction;
  FStepFunction := nil;
  FFinalFunction := nil;
  FLiteFunctionMethod := nil;
  FStepFunctionMethod := nil;
  FFinalFunctionMethod := nil;
end;

constructor TLiteFunctionDesc.Create(Connection: TCRConnection; const Name: string;
  ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction);
begin
  inherited Create(Connection, Name, ParamCount);

  FLiteFunction := nil;
  FStepFunction := StepFunction;
  FFinalFunction := FinalFunction;
  FLiteFunctionMethod := nil;
  FStepFunctionMethod := nil;
  FFinalFunctionMethod := nil;
end;

constructor TLiteFunctionDesc.Create(Connection: TCRConnection; const Name: string;
  ParamCount: Integer; LiteFunctionMethod: TLiteFunctionMethod);
begin
  inherited Create(Connection, Name, ParamCount);

  FLiteFunction := nil;
  FStepFunction := nil;
  FFinalFunction := nil;
  FLiteFunctionMethod := LiteFunctionMethod;
  FStepFunctionMethod := nil;
  FFinalFunctionMethod := nil;
end;

constructor TLiteFunctionDesc.Create(Connection: TCRConnection; const Name: string;
  ParamCount: Integer; StepFunctionMethod: TLiteStepFunctionMethod; FinalFunctionMethod: TLiteFinalFunctionMethod);
begin
  inherited Create(Connection, Name, ParamCount);

  FLiteFunction := nil;
  FStepFunction := nil;
  FFinalFunction := nil;
  FLiteFunctionMethod := nil;
  FStepFunctionMethod := StepFunctionMethod;
  FFinalFunctionMethod := FinalFunctionMethod;
end;

procedure TLiteFunctionDesc.DoRegister(const pName: PAnsiChar; const pSelf: IntPtr);
begin
  if (@FLiteFunction <> nil) or (@FLiteFunctionMethod <> nil) then
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, CallBackLiteFunctionPtr, nil, nil)
  else
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, nil, CallBackStepFunctionPtr, CallBackFinalFunctionPtr);
end;

procedure TLiteFunctionDesc.DoRegister387(const pName: PAnsiChar; const pSelf: IntPtr);
begin
  if (@FLiteFunction <> nil) or (@FLiteFunctionMethod <> nil) then
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, CallBackLiteFunctionPtr387, nil, nil)
  else
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, nil, CallBackStepFunctionPtr387, CallBackFinalFunctionPtr387);
end;

procedure TLiteFunctionDesc.DoRegister3250(const pName: PAnsiChar; const pSelf: IntPtr);
begin
  if (@FLiteFunction <> nil) or (@FLiteFunctionMethod <> nil) then
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, CallBackLiteFunctionPtr3250, nil, nil)
  else
    TSQLiteConnection(FConnection).API.sqlite3_create_function(TSQLiteConnection(FConnection).API.SQLite, pName, ParamCount, TextRepresentation, pSelf, nil, CallBackStepFunctionPtr3250, CallBackFinalFunctionPtr3250);
end;

procedure TLiteFunctionDesc.DoFunction(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr);
var
  ResultValue: Variant;
begin
  if Assigned(FLiteFunctionMethod) then begin
    FLiteFunctionMethod(GetInParams(Context, ParamCount, pData), ResultValue);
    SetResult(Context, ResultValue);
  end
  else begin
    ResultValue := FLiteFunction(GetInParams(Context, ParamCount, pData));
    SetResult(Context, ResultValue);
  end;
end;

procedure TLiteFunctionDesc.DoStep(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr);
begin
  if Assigned(FStepFunctionMethod) then
    FStepFunctionMethod(GetInParams(Context, ParamCount, pData))
  else
    FStepFunction(GetInParams(Context, ParamCount, pData));
end;

procedure TLiteFunctionDesc.DoFinal(Context: pSQLite3Context);
var
  ResultValue: Variant;
begin
  if Assigned(FFinalFunctionMethod) then begin
    FFinalFunctionMethod(ResultValue);
    SetResult(Context, ResultValue);
  end
  else begin
    ResultValue := FFinalFunction;
    SetResult(Context, ResultValue);
  end;
end;

{ TSQLiteFunctionManager }

constructor TSQLiteFunctionManager.Create(Connection: TCRConnection);
begin
  inherited Create;

  FConnection := Connection;
  FFunctionList := TCRObjectList.Create;
end;

destructor TSQLiteFunctionManager.Destroy;
begin
  UnRegistrAllFunctions;
  FFunctionList.Free;

  inherited;
end;

function TSQLiteFunctionManager.GetFunctionDescClass: TLiteFunctionDescClass;
begin
  Result := TLiteFunctionDesc;
end;

procedure TSQLiteFunctionManager.InternalAddFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
var
  ExistLiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  ExistLiteFunctionDesc := FindFunction(LiteFunctionDesc.Name, LiteFunctionDesc.ParamCount);
  if ExistLiteFunctionDesc <> nil then
    InternalRemoveFunction(ExistLiteFunctionDesc);

  LiteFunctionDesc.RegisterFunction;
  FFunctionList.Add(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.InternalRemoveFunction(LiteFunctionDesc: TCustomLiteFunctionDesc);
begin
  LiteFunctionDesc.UnregisterFunction;
  FFunctionList.Remove(LiteFunctionDesc);
end;

function TSQLiteFunctionManager.FindFunction(const Name: string; ParamCount: Integer): TCustomLiteFunctionDesc;
var
  i: integer;
begin
  for i := 0 to FFunctionList.Count - 1 do
  begin
    Result := TCustomLiteFunctionDesc(FFunctionList[i]);
    if (Result.Name = Name) and (Result.ParamCount = ParamCount) then
      exit;
  end;

  // if not found
  Result := nil;
end;

procedure TSQLiteFunctionManager.UnRegistrAllFunctions;
var
  i: integer;
  isConnected: boolean;
begin
  isConnected := FConnection.GetConnected;

  // unregister Functions
  for i := 0 to FFunctionList.Count - 1 do
    if isConnected then
      TCustomLiteFunctionDesc(FFunctionList[i]).UnregisterFunction;
  FFunctionList.Clear;
end;

procedure TSQLiteFunctionManager.RegisterFunction(const Name: string; ParamCount: Integer; LiteFunction: TLiteFunction);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := GetFunctionDescClass.Create(FConnection, Name, ParamCount, LiteFunction);
  InternalAddFunction(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.RegisterAggregateFunction(const Name: string; ParamCount: Integer; StepFunction: TLiteStepFunction; FinalFunction: TLiteFinalFunction);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := GetFunctionDescClass.Create(FConnection, Name, ParamCount, StepFunction, FinalFunction);
  InternalAddFunction(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.RegisterFunction(const Name: string; ParamCount: Integer; LiteFunctionMethod: TLiteFunctionMethod);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := GetFunctionDescClass.Create(FConnection, Name, ParamCount, LiteFunctionMethod);
  InternalAddFunction(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.RegisterAggregateFunction(const Name: string; ParamCount: Integer; StepFunctionMethod: TLiteStepFunctionMethod; FinalFunctionMethod: TLiteFinalFunctionMethod);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := GetFunctionDescClass.Create(FConnection, Name, ParamCount, StepFunctionMethod, FinalFunctionMethod);
  InternalAddFunction(LiteFunctionDesc);
end;

procedure TSQLiteFunctionManager.RegistrAllFunctions;
var
  i: integer;
  isConnected: boolean;
begin
  isConnected := FConnection.GetConnected;

  for i := 0 to FFunctionList.Count - 1 do begin
    if isConnected then
      TCustomLiteFunctionDesc(FFunctionList[i]).RegisterFunction;
  end;
end;

procedure TSQLiteFunctionManager.UnRegisterFunction(const Name: string; ParamCount: Integer);
var
  LiteFunctionDesc: TCustomLiteFunctionDesc;
begin
  LiteFunctionDesc := FindFunction(Name, ParamCount);
  if LiteFunctionDesc <> nil then
    InternalRemoveFunction(LiteFunctionDesc);
end;

initialization
  CallBackLiteFunctionPtr := @CallBackLiteFunction;
  CallBackStepFunctionPtr := @CallBackStepFunction;
  CallBackFinalFunctionPtr := @CallBackFinalFunction;
  CallBackLiteFunctionPtr387 := @CallBackLiteFunction387;
  CallBackStepFunctionPtr387 := @CallBackStepFunction387;
  CallBackFinalFunctionPtr387 := @CallBackFinalFunction387;
  CallBackLiteFunctionPtr3250 := @CallBackLiteFunction3250;
  CallBackStepFunctionPtr3250 := @CallBackStepFunction3250;
  CallBackFinalFunctionPtr3250 := @CallBackFinalFunction3250;

finalization
  CallBackLiteFunctionPtr := nil;
  CallBackStepFunctionPtr := nil;
  CallBackFinalFunctionPtr := nil;
  CallBackLiteFunctionPtr387 := nil;
  CallBackStepFunctionPtr387 := nil;
  CallBackFinalFunctionPtr387 := nil;
  CallBackLiteFunctionPtr3250 := nil;
  CallBackStepFunctionPtr3250 := nil;
  CallBackFinalFunctionPtr3250 := nil;

end.
