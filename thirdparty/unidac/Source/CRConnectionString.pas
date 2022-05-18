
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRConnectionString
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRConnectionString;

interface

uses
  SysUtils, Classes, TypInfo,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  DAConsts, CRTypes, CRParser;

const
  cpLoginPrompt         = -1; // boolean
  cpPooling             = -2; // boolean
  cpConnectionLifetime  = -3; // integer
  cpMaxPoolSize         = -4; // integer
  cpMinPoolSize         = -5; // integer
  cpValidateConnection  = -6; // integer
  cpPersistSecurityInfo = -7; // dummy

  varEnum = $01FF;

type
  TConnectionStringBuilder = class;
  TConnectionStringBuilderClass = class of TConnectionStringBuilder;

  TGetConnectionStringParamMethod = function(PropCode: Integer): Variant of object;
  TSetConnectionStringParamMethod = procedure(PropCode: Integer; const PropValue: Variant) of object;

  EConnectionStringError = class(Exception)
  end;

  TConnectionStringParamPriotity = (ppLowest, ppLow, ppNormal, ppHigh, ppHighest);

  TConnectionStringParam = class
  private
    FPriority: TConnectionStringParamPriotity;
    FName: string;
    FNameUpper: string;
    FSupportedNames: TStringArray;
    FSupportedNamesUpper: TStringArray;
    FCode: integer;
    FDataType: Word;
    FDefaultValue: Variant;
    FSkipValues: Variant;
    FTypeInfo: PTypeInfo;
    FEnumPrefix: string;
  protected
    function GetEnumValue(TypeInfo: PTypeInfo; const Name: string): integer;
    function GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;
  public
    constructor Create(APriority: TConnectionStringParamPriotity; const AName: string; const ASupportedNames: array of string;
      ACode: Integer; ADataType: Word; const ADefaultValue: Variant; const ASkipValues: Variant;
      ATypeInfo: PTypeInfo; const AEnumPrefix: string);

    function CheckName(NameUpper: string): Boolean;
    function GetAsString(Value: Variant): string;
    function GetAsVariant(Value: string): Variant;

    property Priority: TConnectionStringParamPriotity read FPriority;
    property Name: string read FName;
    property NameUpper: string read FNameUpper;
    property SupportedNames: TStringArray read FSupportedNames;
    property SupportedNamesUpper: TStringArray read FSupportedNamesUpper;
    property Code: Integer read FCode;
    property DataType: Word read FDataType;
    property DefaultValue: Variant read FDefaultValue;
    property SkipValues: Variant read FSkipValues;
    property TypeInfo: PTypeInfo read FTypeInfo;
  end;

  TConnectionStringBuilder = class
  private
    FParamList: TCRObjectList;

    FGetPropMethod: TGetConnectionStringParamMethod;
    FSetPropMethod: TSetConnectionStringParamMethod;

    function GetParam(Index: Integer): TConnectionStringParam;
    function GetParamCount: Integer;
    function GetConnectionString: string;
    procedure SetConnectionString(const Value: string);
    procedure SetExtStringBuilderClass(Value: TConnectionStringBuilderClass);
  protected
    FExtStringBuilderClass: TConnectionStringBuilderClass;
    FExtStringBuilder: TConnectionStringBuilder;

    procedure InitParams; virtual; abstract;
    procedure AddParam(Priority: TConnectionStringParamPriotity; const Name: string; const SupportedNames: array of string;
      Code: Integer; DataType: Word; const DefaultValue: Variant; TypeInfo: PTypeInfo = nil; const EnumPrefix: string = ''); overload;
    procedure AddParam(Priority: TConnectionStringParamPriotity; const Name: string; const SupportedNames: array of string;
      Code: Integer; DataType: Word; const DefaultValue: Variant; const SkipValues: array of Variant; TypeInfo: PTypeInfo = nil; const EnumPrefix: string = ''); overload;
    procedure DeleteParam(Code: Integer);
    function IgnoreParam(Code: Integer): boolean; virtual;
    function GetParamIndex(Priority: TConnectionStringParamPriotity; const Name: string): Integer;
    function GetParamValue(Param: TConnectionStringParam): Variant; virtual;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); virtual;
    function ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string; virtual;
    function ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant; virtual;
    function CheckParamName(const Args: array of string; const Name: string): boolean;
    procedure CheckParamValue(const Name, Value: string; IsValueQuoted: boolean = false); virtual;
    procedure AppendParam(var Result: string; const Name, Value: string);
    procedure AppendParamStr(var Result: string; const Str: string);

    procedure ResetParams; virtual;
    procedure ProcessParams(const Value: string); virtual;

    function GetParamIndexByCode(Code: Integer): Integer;
    function GetParamIndexByName(const Name: string): Integer;

    procedure ReleaseExtStringBuilder; virtual;
    procedure InitExtStringBuilder; virtual;

    property GetProp: TGetConnectionStringParamMethod read FGetPropMethod;
    property SetProp: TSetConnectionStringParamMethod read FSetPropMethod;
    property ExtStringBuilderClass: TConnectionStringBuilderClass read FExtStringBuilderClass write SetExtStringBuilderClass;
    property ExtStringBuilder: TConnectionStringBuilder read FExtStringBuilder;
  public
    constructor Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod); virtual;
    destructor Destroy; override;

    function Parse(const Value: string; AllowDuplicate: boolean = False): TStrValueStringList;

    procedure ReadParam(Param: TConnectionStringParam; const Value: string); virtual;
    procedure ReadParams(ParamStrList: TStrValueStringList);
    function ReadUnknownParam(const Name, Value: string): boolean; virtual;
    procedure ReadUnknownParams(ParamStrList: TStrValueStringList); virtual;
    procedure WriteParam(var ConnectionString: string; Param: TConnectionStringParam); virtual;
    procedure WriteParams(var ConnectionString: string);
    procedure WriteUnknownParams(var ConnectionString: string); virtual;

    function GetFullParamList: TList;
    function FindParamByCode(Code: Integer): TConnectionStringParam;
    function FindParamByName(const Name: string): TConnectionStringParam;

    property Params[Index: Integer]: TConnectionStringParam read GetParam;
    property ParamCount: Integer read GetParamCount;
    property ConnectionString: string read GetConnectionString write SetConnectionString stored False;
  end;

  TCRConnectionStringBuilder = class (TConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;
  end;

  TCRConnectionStringBuilderClass = class of TCRConnectionStringBuilder;

implementation

{$IFDEF VER5}
uses
  CRFunctions;
{$ENDIF}

{ TConnectionStringParam }

constructor TConnectionStringParam.Create(APriority: TConnectionStringParamPriotity; const AName: string; const ASupportedNames: array of string;
  ACode: Integer; ADataType: Word; const ADefaultValue: Variant; const ASkipValues: Variant;
  ATypeInfo: PTypeInfo; const AEnumPrefix: string);
var
  i: Integer;
  Len: Integer;
begin
  inherited Create;

  FPriority := APriority;

  FName := AName;
  FNameUpper := UpperCase(AName);

  Len := High(ASupportedNames) + 1;
  SetLength(FSupportedNames, Len);
  SetLength(FSupportedNamesUpper, Len);
  for i := 0 to Len - 1 do begin
    FSupportedNames[i] := ASupportedNames[i];
    FSupportedNamesUpper[i] := UpperCase(ASupportedNames[i])
  end;

  FCode := ACode;
  FDataType := ADataType;
  FDefaultValue := ADefaultValue;
  FSkipValues := ASkipValues;
  FTypeInfo := ATypeInfo;
  FEnumPrefix := AEnumPrefix;
end;

function TConnectionStringParam.GetEnumValue(TypeInfo: PTypeInfo; const Name: string): integer;
begin
  Result := TypInfo.GetEnumValue(TypeInfo, Name);
  if (Result = -1) and (FEnumPrefix <> '') then
    Result := TypInfo.GetEnumValue(TypeInfo, FEnumPrefix + Name);
end;

function TConnectionStringParam.GetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;
var
  NamePrefixLen: integer;
begin
  Result := TypInfo.GetEnumName(TypeInfo, Value);

  if FEnumPrefix <> '' then begin
    NamePrefixLen := Length(FEnumPrefix);
    if Copy(Result, 1, NamePrefixLen) = FEnumPrefix then
      Result := Copy(Result, NamePrefixLen + 1, Length(Result) - NamePrefixLen);
  end;
end;

function TConnectionStringParam.CheckName(NameUpper: string): Boolean;
var
  i: Integer;
begin
  if FNameUpper = NameUpper then begin
    Result := True;
    Exit;
  end;

  for i := 0 to Length(FSupportedNamesUpper) - 1 do
    if FSupportedNamesUpper[i] = NameUpper then begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TConnectionStringParam.GetAsString(Value: Variant): string;
begin
  case TVarData(Value).VType of
    varNull:
      raise Exception.Create('Value can''t be Null');
    varEmpty:
      raise Exception.Create('Value can''t be Empty');
    varUnknown:
      raise Exception.Create('Value can''t be Unknown');
  end;

  case FDataType of
    varString:
      if VarIsStr(Value) then
        Result := Value
      else
        Result := VarToStr(Value);
    varBoolean:
      if VarIsStr(Value) then
        Result := Value
      else if VarIsType(Value, varBoolean) then
        Result := BoolToStr(Value, True)
      else
        raise Exception.Create('Could not convert into boolean value');
    varInteger:
      if VarIsStr(Value) then
        Result := Value
      else if VarIsOrdinal(Value) then
        Result := IntToStr(Value)
      else
        raise Exception.Create('Could not convert into ordinal value');
    varEnum:
      if VarIsStr(Value) then
        Result := Value
      else if VarIsOrdinal(Value) then
        Result := GetEnumName(FTypeInfo, Integer(Value))
      else
        raise Exception.Create('Could not convert into enumeration');
    else
      raise Exception.Create('Unknown parameter type');
  end;
end;

function TConnectionStringParam.GetAsVariant(Value: string): Variant;
begin
  case FDataType of
    varString:
      Result := Value;
    varBoolean:
      Result := StrToBool(Value);
    varInteger:
      Result := StrToInt(Value);
    varEnum: begin
      Result := GetEnumValue(FTypeInfo, Value);
      // invalid enum value
      if Integer(Result) = -1 then
         Result := DefaultValue;
    end;
    else
      raise Exception.Create('Unknown parameter type');
  end;
end;

{ TConnectionStringBuilder }

constructor TConnectionStringBuilder.Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod);
begin
  inherited Create;

  FParamList := TCRObjectList.Create;
  InitParams;

  Assert(Assigned(GetPropMethod));
  Assert(Assigned(SetPropMethod));

  FGetPropMethod := GetPropMethod;
  FSetPropMethod := SetPropMethod;
end;

destructor TConnectionStringBuilder.Destroy;
begin
  ReleaseExtStringBuilder;

  FParamList.Free;

  inherited;
end;

function TConnectionStringBuilder.GetParam(Index: Integer): TConnectionStringParam;
begin
  Result := TConnectionStringParam(FParamList[Index]);
end;

function TConnectionStringBuilder.GetParamCount: Integer;
begin
  Result := FParamList.Count;
end;

function TConnectionStringBuilder.GetConnectionString: string;
begin
  Result := '';

  WriteParams(Result);
  if FExtStringBuilder <> nil then
    FExtStringBuilder.WriteParams(Result);

  WriteUnknownParams(Result);
  if FExtStringBuilder <> nil then
    FExtStringBuilder.WriteUnknownParams(Result);
end;

procedure TConnectionStringBuilder.SetConnectionString(const Value: string);
var
  OldCS: string;
begin
  OldCS := ConnectionString;
  try
    if Value <> OldCS then begin
      ResetParams;
      if FExtStringBuilder <> nil then
        FExtStringBuilder.ResetParams;

      ProcessParams(Value);
    end;
  except
    ConnectionString := OldCS;
    raise;
  end;
end;

procedure TConnectionStringBuilder.SetExtStringBuilderClass(Value: TConnectionStringBuilderClass);
begin
  if FExtStringBuilderClass <> Value then begin
    ReleaseExtStringBuilder;
    FExtStringBuilderClass := Value;
    InitExtStringBuilder;
  end;
end;

procedure TConnectionStringBuilder.AddParam(Priority: TConnectionStringParamPriotity; const Name: string; const SupportedNames: array of string;
  Code: Integer; DataType: Word; const DefaultValue: Variant; TypeInfo: PTypeInfo = nil; const EnumPrefix: string = '');
var
  Index: Integer;
  Param: TConnectionStringParam;
begin
  Param := TConnectionStringParam.Create(Priority, Name, SupportedNames, Code, DataType, DefaultValue, null, TypeInfo, EnumPrefix);

  Index := GetParamIndexByCode(Param.Code);
  if Index >= 0 then
    FParamList[Index] := Param
  else
    FParamList.Add(Param);
end;

procedure TConnectionStringBuilder.AddParam(Priority: TConnectionStringParamPriotity; const Name: string; const SupportedNames: array of string;
  Code: Integer; DataType: Word; const DefaultValue: Variant; const SkipValues: array of Variant; TypeInfo: PTypeInfo = nil; const EnumPrefix: string = '');
var
  i: Integer;
  Index: Integer;
  VarArray: Variant;
  Param: TConnectionStringParam;
begin
  VarArray := VarArrayCreate([0, High(SkipValues)], varVariant);
  for i := 0 to High(SkipValues) do
    VarArray[i] := SkipValues[i];

  Param := TConnectionStringParam.Create(Priority, Name, SupportedNames, Code, DataType, DefaultValue, VarArray, TypeInfo, EnumPrefix);

  Index := GetParamIndexByCode(Param.Code);
  if Index >= 0 then
    FParamList[Index] := Param
  else
    FParamList.Add(Param);
end;

procedure TConnectionStringBuilder.DeleteParam(Code: Integer);
var
  i: Integer;
begin
  for i := 0 to FParamList.Count - 1 do
    if TConnectionStringParam(FParamList[i]).Code = Code then begin
      FParamList.Delete(i);
      break;
    end;
end;

function TConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  Result := False;
end;

function TConnectionStringBuilder.GetParamIndex(Priority: TConnectionStringParamPriotity; const Name: string): Integer;
var
  i: Integer;
  NameUpper: string;
  Param: TConnectionStringParam;
begin
  NameUpper := UpperCase(Name);

  for i := 0 to FParamList.Count - 1 do begin
    Param := TConnectionStringParam(FParamList[i]);
    if (Param.Priority = Priority) and Param.CheckName(NameUpper) then begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function TConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
begin
  Result := GetProp(Param.Code);
end;

procedure TConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
begin
  SetProp(Param.Code, Value);
end;

function TConnectionStringBuilder.ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string;
begin
  Result := Param.GetAsString(Value);
end;

function TConnectionStringBuilder.ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant;
begin
  Result := Param.GetAsVariant(Value);
end;

function TConnectionStringBuilder.CheckParamName(const Args: array of string; const Name: string): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to Length(Args) - 1 do begin
    Result := SameText(Name, Args[i]);
    if Result then
      Break;
  end;
end;

procedure TConnectionStringBuilder.CheckParamValue(const Name, Value: string; IsValueQuoted: boolean = false);
begin
  // Empty
end;

procedure TConnectionStringBuilder.AppendParam(var Result: string; const Name, Value: string);
begin
  if Pos('=', Value) > 0 then
    if (Length(Value) < 2) or (Value[1] <> '"') or (Value[Length(Value)] <> '"') then
      AppendParamStr(Result, Name + '="' + Value + '"')
    else
      AppendParamStr(Result, Name + '=' + Value)
  else
    AppendParamStr(Result, Name + '=' + Value);
end;

procedure TConnectionStringBuilder.AppendParamStr(var Result: string; const Str: string);
begin
  if Str <> '' then begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + Str;
  end;
end;

procedure TConnectionStringBuilder.ResetParams;
var
  i: Integer;
  Param: TConnectionStringParam;
  DefValue: Variant;
begin
  for i := 0 to FParamList.Count - 1 do begin
    Param := TConnectionStringParam(FParamList[i]);
    DefValue := Param.DefaultValue;
    if not IgnoreParam(Param.Code) and (DefValue <> Null) then
      SetParamValue(Param, DefValue);
  end;
end;

procedure TConnectionStringBuilder.ProcessParams(const Value: string);
var
  Params: TStrValueStringList;
begin
  Params := Parse(Value);
  try
    ReadParams(Params);
    if FExtStringBuilder <> nil then
      FExtStringBuilder.ReadParams(Params);

    ReadUnknownParams(Params);
    if FExtStringBuilder <> nil then
      FExtStringBuilder.ReadUnknownParams(Params);

    if Params.Count > 0 then
      raise EConnectionStringError.CreateFmt(SConnectParamNameUnknown, [Params.Keys[0]]);
  finally
    Params.Free;
  end;
end;

function TConnectionStringBuilder.GetParamIndexByCode(Code: Integer): Integer;
var
  i: Integer;
  Param: TConnectionStringParam;
begin
  for i := FParamList.Count - 1 downto 0 do begin
    Param := TConnectionStringParam(FParamList[i]);
    if Param.Code = Code then begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function TConnectionStringBuilder.GetParamIndexByName(const Name: string): Integer;
var
  i: Integer;
  NameUpper: string;
  Param: TConnectionStringParam;
begin
  NameUpper := UpperCase(Name);

  for i := 0 to FParamList.Count - 1 do begin
    Param := TConnectionStringParam(FParamList[i]);
    if Param.CheckName(NameUpper) then begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

procedure TConnectionStringBuilder.ReleaseExtStringBuilder;
begin
  if FExtStringBuilder <> nil then begin
    FExtStringBuilder.Free;
    FExtStringBuilder := nil;
  end;
end;

procedure TConnectionStringBuilder.InitExtStringBuilder;
begin
  if FExtStringBuilderClass <> nil then
    FExtStringBuilder := FExtStringBuilderClass.Create(GetProp, SetProp);
end;

function TConnectionStringBuilder.Parse(const Value: string; AllowDuplicate: boolean = False): TStrValueStringList;
var
  Parser: TParser;
  Code, l: integer;
  ParamName, ParamValue, Lexem: string;
  IsParamValueQuoted: boolean;
begin
  Result := TStrValueStringList.Create;
  try
    Parser := TParser.Create(Trim(Value));
    try
      Parser.OmitInlineComment := True;
      Parser.OmitBlank := False;
      Parser.QuotedString := True;
      repeat
        ParamName := '';
        ParamValue := '';
        Code := Parser.GetNext(Lexem);
        while (Code <> lcEnd) and (Code <> lxSemicolon{;}) do begin
          if (Code = lxEqual{=}) then begin
            if (Trim(ParamName) = '') then
              raise EConnectionStringError.Create(SConnectParamNameMissing);
            break;
          end;
          ParamName := ParamName + Lexem;
          Code := Parser.GetNext(Lexem);
        end;
        ParamName := Trim(ParamName);

        if ParamName <> '' then begin
          Code := Parser.GetNext(Lexem);
          while (Code <> lcEnd) and (Code <> lxSemicolon{;}) do begin
            ParamValue := ParamValue + Lexem;
            Code := Parser.GetNext(Lexem);
          end;
          ParamValue := Trim(ParamValue);
          IsParamValueQuoted := False;
          l := Length(ParamValue);
          if (l >= 2) and (ParamValue[1] = '"') and (ParamValue[l] = '"') then begin
            ParamValue := Trim(Copy(ParamValue, 2, l - 2));
            IsParamValueQuoted := True;
          end;
          CheckParamValue(ParamName, ParamValue, IsParamValueQuoted);
          if (not AllowDuplicate) and (Result.IndexOf(ParamName) <> -1) then
            raise EConnectionStringError.CreateFmt(SConnectParamAddedTwice, [ParamName]);
          Result.Add(ParamName, ParamValue);
        end;
      until Code = lcEnd;

    finally
      Parser.Free;
    end;

  except
    Result.Free;
    raise;
  end;
end;

procedure TConnectionStringBuilder.ReadParam(Param: TConnectionStringParam; const Value: string);
begin
  try
    SetParamValue(Param, ConvertStrToVar(Param, Value));
  except
    on E: EConnectionStringError do
      raise;
    on E: Exception do
      raise EConnectionStringError.Create(Format(SInvalidConnectParamValue, [Param.Name, Value]) + #13#10 + E.Message);
  end;
end;

procedure TConnectionStringBuilder.ReadParams(ParamStrList: TStrValueStringList);
var
  i: integer;
  Priority: TConnectionStringParamPriotity;
  Param: TConnectionStringParam;
  ParamIndex: Integer;
  Name: string;
  Value: string;
begin
  for Priority := High(TConnectionStringParamPriotity) downto Low(TConnectionStringParamPriotity) do begin
    i := 0;
    while i < ParamStrList.Count do begin
      Name := ParamStrList.Keys[i];
      Value := ParamStrList.Values[i];

      ParamIndex := GetParamIndex(Priority, Name);
      if ParamIndex >= 0 then begin
        Param := TConnectionStringParam(FParamList[ParamIndex]);
        if not IgnoreParam(Param.Code) then
          ReadParam(Param, Value);
        ParamStrList.Delete(i);
      end
      else
        Inc(i);
    end;
  end;
end;

function TConnectionStringBuilder.ReadUnknownParam(const Name, Value: string): boolean;
begin
  Result := False;
end;

procedure TConnectionStringBuilder.ReadUnknownParams(ParamStrList: TStrValueStringList);
var
  i: integer;
  Name: string;
  Value: string;
begin
  i := 0;
  while i < ParamStrList.Count do begin
    Name := ParamStrList.Keys[i];
    Value := ParamStrList.Values[i];
    if ReadUnknownParam(Name, Value) then
      ParamStrList.Delete(i)
    else
      Inc(i);
  end;
end;

procedure TConnectionStringBuilder.WriteParam(var ConnectionString: string; Param: TConnectionStringParam);
var
  i: Integer;
  Value: Variant;
  StrValue: string;
  DefValue: string;
  SkipValue: string;
begin
  try
    Value := GetParamValue(Param);
    if Value = Null then
      Exit;

    if (Param.DataType <> varString) and VarIsStr(Value) and (Value = '') then
      Exit;

    StrValue := ConvertVarToStr(Param, Value);
    if VarIsNull(Param.DefaultValue) then
      AppendParam(ConnectionString, Param.Name, StrValue)
    else begin
      DefValue := ConvertVarToStr(Param, Param.DefaultValue);
      if StrValue <> DefValue then
        if VarIsNull(Param.SkipValues)then
          AppendParam(ConnectionString, Param.Name, StrValue)
        else if VarIsArray(Param.SkipValues) then begin
          for i := VarArrayLowBound(Param.SkipValues, 1) to VarArrayHighBound(Param.SkipValues, 1) do begin
            SkipValue := ConvertVarToStr(Param, Param.SkipValues[i]);
            if StrValue = SkipValue then
              Exit;
          end;
          AppendParam(ConnectionString, Param.Name, StrValue);
        end
        else begin
          SkipValue := ConvertVarToStr(Param, Param.SkipValues);
          if StrValue <> SkipValue then
            AppendParam(ConnectionString, Param.Name, StrValue);
        end;
    end;
  except
    on E: EConnectionStringError do
      raise;
    on E: Exception do
      raise EConnectionStringError.Create(Format(SConnectParamInternalError, [Param.Name]) + #13#10 + E.Message);
  end;
end;

procedure TConnectionStringBuilder.WriteParams(var ConnectionString: string);
var
  i: Integer;
  Priority: TConnectionStringParamPriotity;
  Param: TConnectionStringParam;
begin
  for Priority := High(TConnectionStringParamPriotity) downto Low(TConnectionStringParamPriotity) do
    for i := 0 to FParamList.Count - 1 do begin
      Param := TConnectionStringParam(FParamList[i]);
      if not IgnoreParam(Param.Code) and (Param.Priority = Priority)then
        WriteParam(ConnectionString, Param);
    end;
end;

procedure TConnectionStringBuilder.WriteUnknownParams(var ConnectionString: string);
begin
  // Empty
end;

function TConnectionStringBuilder.GetFullParamList: TList;
var
  i: Integer;
  ExtParamList: TList;
begin
  Result := TList.Create;
  for i := 0 to FParamList.Count - 1 do
    Result.Add(FParamList[i]);

  if FExtStringBuilder <> nil then begin
    ExtParamList := FExtStringBuilder.GetFullParamList;
    try
      for i := 0 to ExtParamList.Count - 1 do
        Result.Add(ExtParamList[i]);
    finally
      ExtParamList.Free;
    end;
  end;
end;

function TConnectionStringBuilder.FindParamByCode(Code: Integer): TConnectionStringParam;
var
  Index: Integer;
begin
  Index := GetParamIndexByCode(Code);
  if Index >= 0 then
    Result := TConnectionStringParam(FParamList[Index])
  else if FExtStringBuilder <> nil then
    Result := FExtStringBuilder.FindParamByCode(Code)
  else
    Result := nil;
end;

function TConnectionStringBuilder.FindParamByName(const Name: string): TConnectionStringParam;
var
  Index: Integer;
begin
  Index := GetParamIndexByName(Name);
  if Index >= 0 then
    Result := TConnectionStringParam(FParamList[Index])
  else if FExtStringBuilder <> nil then
    Result := FExtStringBuilder.FindParamByName(Name)
  else
    Result := nil;
end;

{ TCRConnectionStringBuilder }

procedure TCRConnectionStringBuilder.InitParams;
begin
  AddParam(ppLow, 'Login Prompt', ['LoginPrompt'], cpLoginPrompt, varBoolean, DefValLoginPrompt);
  AddParam(ppLow, 'Pooling', [], cpPooling, varBoolean, DefValPooling);
  AddParam(ppLow, 'Connection LifeTime', ['ConnectionLifeTime'], cpConnectionLifetime, varInteger, DefValConnectionLifetime);
  AddParam(ppLow, 'Max Pool Size', ['MaxPoolSize'], cpMaxPoolSize, varInteger, DefValMaxPoolSize);
  AddParam(ppLow, 'Min Pool Size', ['MinPoolSize'], cpMinPoolSize, varInteger, DefValMinPoolSize);
  AddParam(ppLow, 'Validate Connection', ['ValidateConnection'], cpValidateConnection, varBoolean, DefValValidate);
  AddParam(ppLow, 'Persist Security Info', ['PersistSecurityInfo'], cpPersistSecurityInfo, varString, Null); // dummy
end;

function TCRConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  case Code of
    cpPersistSecurityInfo:
      Result := True;
    else
      Result := False;
  end;
end;

end.
