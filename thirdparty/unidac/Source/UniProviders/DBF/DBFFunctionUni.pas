
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFFunctionUni;

interface

{$IFDEF DBFENGINE}

uses
  SysUtils,
  CRTypes, CRFunctions,
  LiteCallVirtual, LiteFunctionVirtual,
{$IFNDEF UNIDACPRO}
  DBFParser;
{$ELSE}
  DBFParserUni;
{$ENDIF}

type
  TDBFArgumentType = (atString, atOrdinal, atNumeric, atBoolean, atDate, atAny);
  TDBFArgumentTypes = array of TDBFArgumentType;
  TDBFCheckArgumentNull = (caFirst, caAll, caNone);

  TDBFFunctionInfo = class
  private
    FFunctionName: string;
    FCode: integer;
    FArgumentCount: integer;
    FArgumentTypes: TDBFArgumentTypes;
    FCheckArgumentNull: TDBFCheckArgumentNull;
  public
    constructor Create(const FunctionName: string; Code: integer; ArgumentCount: integer; ArgumentTypes: TDBFArgumentTypes; CheckArgumentNull: TDBFCheckArgumentNull);
    destructor Destroy; override;
  end;

  TDBFFunctions = class
  private
    FFunctionInfos: TCRObjectList;

    function GetCount: integer;
    function GetFunctionInfo(Index: integer): TDBFFunctionInfo;

    function Check(const FunctionInfo: TDBFFunctionInfo; InValues: TVariantArray): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const FunctionName: string; Code: integer; ArgumentCount: integer; ArgumentTypes: Variant; CheckArgumentNull: TDBFCheckArgumentNull);
    function Find(const FunctionName: string; const Arguments: TVariantArray): TDBFFunctionInfo;
    procedure Execute(const FunctionInfo: TDBFFunctionInfo; const Arguments: TVariantArray; var ResultValue: Variant);

    property Count: integer read GetCount;
    property FunctionInfos[Index: Integer]: TDBFFunctionInfo read GetFunctionInfo; default;
  end;

  TDBFFunctionDesc = class(TLiteFunctionDesc)
  protected
    procedure DoFunction(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr); override;
  end;

  TDBFFunctionManager = class(TSQLiteFunctionManager)
  private
    procedure InternalFunction(InValues: array of Variant; var ResultValue: Variant);
  protected
    function GetFunctionDescClass: TLiteFunctionDescClass; override;
  public
    procedure RegisterDBFFunctions;
  end;

function VarIsDate(const Value: Variant): Boolean;
function VarIsBoolean(const Value: Variant): Boolean;
function CanCast(const Value: variant; const ToType: TVarType): boolean;
//function CastTo(const Value: variant; const ToType: TVarType): Variant;
function CastValue(const Value: variant; const ToType: TVarType; var ResultValue: variant): boolean;

var
  DBFFunctions: TDBFFunctions;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

uses
  Variants, Math,
  MemData, CRVirtualData,
  LiteClassesVirtual;

function VarIsDate(const Value: Variant): Boolean;
begin
  Result := TVarData(Value).VType = varDate;
end;

function VarIsBoolean(const Value: Variant): Boolean;
begin
  Result := TVarData(Value).VType = vtBoolean;
end;

function CanCast(const Value: variant; const ToType: TVarType): boolean;
begin
  case ToType of
    varDate: Result := VarIsNumeric(Value) or VarIsStr(Value);
  else
    Result := False;
  end;
end;

function CastValue(const Value: variant; const ToType: TVarType; var ResultValue: variant): boolean;
var
  IntValue: integer;
  DoubleValue: double;
//  DateValue: TDateTime;
begin
  Result := False;

  case ToType of
    varInteger:
      if VarIsOrdinal(Value) then begin
        ResultValue := integer(Value);
        Result := True;
      end
      else if VarIsStr(Value) and TryStrToInt(string(Value), IntValue) then begin
        ResultValue := IntValue;
        Result := True;
      end;
    varDouble:
      if VarIsFloat(Value) then begin
        ResultValue := double(Value);
        Result := True;
      end
      else if VarIsOrdinal(Value) then begin
        ResultValue := double(Value);
        Result := True;
      end
      else if VarIsStr(Value) and TryStrToFloat(string(Value), DoubleValue) then begin
        ResultValue := DoubleValue;
        Result := True;
      end;
    varDate:
      if VarIsDate(Value) then begin
        ResultValue := Value;
        Result := True;
      end
      else if VarIsNumeric(Value) then begin
        ResultValue := TDateTime(Value);
        Result := True;
      end
      else if VarIsStr(Value) then begin //and TryStrToDate(string(Value), DateValue{$IFDEF FPC}, 'YYYY-MM-DD', '-'{$ENDIF}) then begin
        ResultValue := ConvertStringToDateTime(dtDate, string(Value));
//        ResultValue := DateValue;
        Result := True;
      end;
    varString:
      if VarIsStr(Value) then begin
        ResultValue := string(Value);
        Result := True;
      end
      else if VarIsOrdinal(Value) then begin
        ResultValue := IntToStr(Value);
        Result := True;
      end
      else if VarIsFloat(Value) then begin
        ResultValue := FloatToStr(Value);
        Result := True;
      end
      else if VarIsDate(Value) then begin
        ResultValue := FormatDateTime('YYYYMMDD', Value);
        Result := True;
      end
      else begin
        ResultValue := VarToStr(Value);
        Result := True;
      end;
  else
    Result := False;
  end;
end;

{ TDBFFunctionInfo }

constructor TDBFFunctionInfo.Create(const FunctionName: string; Code: integer; ArgumentCount: integer; ArgumentTypes: TDBFArgumentTypes; CheckArgumentNull: TDBFCheckArgumentNull);
begin
  inherited Create;

  FFunctionName := FunctionName;
  FCode := Code;
  FArgumentCount := ArgumentCount;
  FArgumentTypes := ArgumentTypes;
  FCheckArgumentNull := CheckArgumentNull;
end;

destructor TDBFFunctionInfo.Destroy;
begin

  inherited;
end;

{ TDBFFunctions }

constructor TDBFFunctions.Create;
begin
  inherited;

  FFunctionInfos := TCRObjectList.Create;
end;

destructor TDBFFunctions.Destroy;
begin
  FFunctionInfos.Free;
end;

function TDBFFunctions.GetCount: integer;
begin
  Result := FFunctionInfos.Count;
end;

function TDBFFunctions.GetFunctionInfo(Index: integer): TDBFFunctionInfo;
begin
  Result := TDBFFunctionInfo(FFunctionInfos[Index]);
end;

function TDBFFunctions.Check(const FunctionInfo: TDBFFunctionInfo; InValues: TVariantArray): boolean;
var
  i: integer;
begin
  Result := True;

  for i := 0 to High(InValues) do begin
    Result := VarIsNull(InValues[i]) or VarIsEmpty(InValues[i]);
    if not Result then
      case FunctionInfo.FArgumentTypes[i] of
        atString: Result := VarIsStr(InValues[i]);
        atOrdinal: Result := VarIsOrdinal(InValues[i]);
        atNumeric: Result := VarIsNumeric(InValues[i]);
        atBoolean: Result := VarIsBoolean(InValues[i]);
        atDate: Result := VarIsDate(InValues[i]) or CanCast(InValues[i], varDate);
        atAny: Result := VarIsStr(InValues[i]) or
                    VarIsNumeric(InValues[i]) or
                    VarIsBoolean(InValues[i]) or
                    VarIsDate(InValues[i]) or CanCast(InValues[i], varDate);
      end;
  end;
end;

procedure TDBFFunctions.Add(const FunctionName: string; Code: integer; ArgumentCount: integer; ArgumentTypes: Variant; CheckArgumentNull: TDBFCheckArgumentNull);
var
  Info: TDBFFunctionInfo;
  Arr: TDBFArgumentTypes;
  i: integer;
begin
  Assert(ArgumentCount = (VarArrayHighBound(ArgumentTypes, 1) + 1));

  SetLength(Arr, ArgumentCount);
  for i := 0 to ArgumentCount - 1 do
    Arr[i] := TDBFArgumentType(ArgumentTypes[i]);
  Info := TDBFFunctionInfo.Create(FunctionName, Code, ArgumentCount, Arr, CheckArgumentNull);
  FFunctionInfos.Add(Info)
end;

function TDBFFunctions.Find(const FunctionName: string; const Arguments: TVariantArray): TDBFFunctionInfo;
var
  i: integer;
  Info: TDBFFunctionInfo;
begin
  Result := nil;

  for i := 0 to FFunctionInfos.Count - 1 do begin
    Info := GetFunctionInfo(i);
    if SameText(Info.FFunctionName, FunctionName) and (Info.FArgumentCount = Length(Arguments)) and Check(Info, Arguments) then begin
      Result := Info;
      Break;
    end;
  end;
end;

procedure TDBFFunctions.Execute(const FunctionInfo: TDBFFunctionInfo; const Arguments: TVariantArray; var ResultValue: Variant);
var
  i, n, n1, n2: integer;
  IntVal: integer;
  StrVal, s, s1, s2: string;
  FloatVal: double;
  BoolVal: boolean;
  VarVal: Variant;
  ExprLen,
  ExprDecimals,
  DecimalPos,
  StrLen,
  Selector: integer;
  ExprChar: char;
begin
  if FunctionInfo.FCheckArgumentNull = caAll then begin
    for i := 0 to High(Arguments) do
      if (VarIsNull(Arguments[i]) or VarIsEmpty(Arguments[i])) then
        Exit;
  end
  else if (FunctionInfo.FCheckArgumentNull = caFirst) and (VarIsNull(Arguments[0]) or VarIsEmpty(Arguments[0])) then
    Exit;

  if not Check(FunctionInfo, Arguments) then
    raise Exception.CreateFmt('Invalid arguments for function %s', [FunctionInfo.FFunctionName]);

  case FunctionInfo.FCode of
    lxVAL: begin
      StrVal := Arguments[0];
      if TryStrToInt(StrVal, IntVal) then
        ResultValue := IntVal
      else if TryStrToFloat(StrVal, FloatVal) then
        ResultValue := FloatVal
      else
        Exit;
    end;
    lxABS:
      ResultValue := Abs(double(Arguments[0]));
    lxFLOOR:
      ResultValue := Floor(double(Arguments[0]));
    lxCEIL:
      ResultValue := Ceil(double(Arguments[0]));
    lxROUND:
      ResultValue := Round(double(Arguments[0]));
    lxINT:
      ResultValue := Trunc(double(Arguments[0]));
    lxSQRT:
      ResultValue := Sqrt(double(Arguments[0]));
    lxLOG:
      ResultValue := Ln(double(Arguments[0]));
    lxLOG10:
      ResultValue := Log10(double(Arguments[0]));
    lxSTR: begin
      ExprLen := 10;
      ExprDecimals := 0;
      ExprChar := ' ';

      if Length(Arguments) > 1 then begin
        ExprLen := integer(Arguments[1]);
        if (ExprLen < 1) or (ExprLen > 20) then
          ExprLen := 10;
      end;
      if Length(Arguments) > 2 then begin
        ExprDecimals := integer(Arguments[2]);
        if ExprDecimals < 0 then
          ExprDecimals := 0;
      end;
      if (Length(Arguments) > 3) and (string(Arguments[3]) <> '') then
        ExprChar := string(Arguments[3])[1];

      if VarIsOrdinal(Arguments[0]) then begin
        StrVal := IntToStr(integer(Arguments[0]));
        DecimalPos := 0;
      end
      else if (ExprLen = 0) or (ExprDecimals = 0) then begin
        StrVal := IntToStr(Round(double(Arguments[0])));
        DecimalPos := 0;
      end
      else begin
        StrVal := FloatToStr(double(Arguments[0]));
        DecimalPos := Pos({$IFDEF USE_TFORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, StrVal);
        StrLen := Length(StrVal);
        i := Max(ExprDecimals, StrLen - DecimalPos);
        if (DecimalPos + i) > ExprLen then
          i := ExprLen - DecimalPos;
        StrVal := Format('%.' + IntToStr(i) + 'f', [double(Arguments[0])])
      end;

      if ExprLen > 0 then begin
        StrLen := Length(StrVal);
        if (StrLen > ExprLen) and (DecimalPos = 0) then
          StrVal := StringOfChar('*', ExprLen)
        else if StrLen < ExprLen then
          StrVal := StringOfChar(ExprChar, ExprLen - StrLen) + StrVal;
      end;

      ResultValue := StrVal;
    end;
    lxCHR:
      ResultValue := Chr(integer(Arguments[0]));
    lxASC: begin
      StrVal := string(Arguments[0]);
      if Length(StrVal) > 0 then
        ResultValue := Ord(StrVal[1])
      else
        ResultValue := 0;
    end;
    lxLTRIM:
      ResultValue := TrimLeft(string(Arguments[0]));
    lxALLTRIM,
    lxTRIM:
      ResultValue := Trim(string(Arguments[0]));
    lxLEN:
      ResultValue := Length(string(Arguments[0]));
    lxLOWER:
      ResultValue := LowerCase(string(Arguments[0]));
    lxUPPER:
      ResultValue := UpperCase(string(Arguments[0]));
    lxPROPER: begin
      StrVal := LowerCase(string(Arguments[0]));
      BoolVal := True;
      for i := 1 to Length(StrVal) do begin
        if StrVal[i] = ' ' then
          BoolVal := True
        else if BoolVal then begin
          StrVal[i] := UpperCase(StrVal[i])[1];
          BoolVal := False;
        end;
      end;
      ResultValue := StrVal;
    end;
    lxISUPPER: begin
      StrVal := string(Arguments[0]);
      if Length(StrVal) > 0 then
        ResultValue := UpperCase(StrVal[1]) = StrVal[1]
      else
        ResultValue := False;
    end;
    lxISLOWER: begin
      StrVal := string(Arguments[0]);
      if Length(StrVal) > 0 then
        ResultValue := LowerCase(StrVal[1]) = StrVal[1]
      else
        ResultValue := False;
    end;
    lxISDIGIT: begin
      StrVal := string(Arguments[0]);
      if Length(StrVal) > 0 then
        ResultValue := CharInSet(StrVal[1], ['0'..'9'])
      else
        ResultValue := False;
    end;
    lxISALPHA: begin
      StrVal := string(Arguments[0]);
      if Length(StrVal) > 0 then
        ResultValue := CharInSet(StrVal[1], ['a'..'z', 'A'..'Z'])
      else
        ResultValue := False;
    end;
    lxLEFT:
      ResultValue := Copy(string(Arguments[0]), 1, integer(Arguments[1]));
    lxRIGHT:
      ResultValue := Copy(string(Arguments[0]), Length(string(Arguments[0])) - integer(Arguments[1]) + 1, Length(string(Arguments[0])));
    lxSUBSTR:
      ResultValue := Copy(string(Arguments[0]), integer(Arguments[1]), integer(Arguments[2]));
    lxPADL: begin
      if Length(Arguments) = 3 then
        StrVal := string(Arguments[2])
      else
        StrVal := ' ';
      if StrVal = '' then
        StrVal := ' ';

      ResultValue := StringOfChar(StrVal[1], integer(Arguments[1]) - Length(string(Arguments[0]))) + string(Arguments[0]);
    end;
    lxPADR: begin
      if Length(Arguments) = 3 then
        StrVal := string(Arguments[2])
      else
        StrVal := ' ';
      if StrVal = '' then
        StrVal := ' ';

      ResultValue := string(Arguments[0]) + StringOfChar(StrVal[1], integer(Arguments[1]) - Length(string(Arguments[0])));
    end;
    lxPADC: begin
      if Length(Arguments) = 3 then
        s := string(Arguments[2])
      else
        s := ' ';
      if s = '' then
        s := ' ';

      StrVal := string(Arguments[0]);
      i := Trunc((integer(Arguments[1]) - Length(StrVal)) / 2);
      StrVal := StringOfChar(s[1], i) + StrVal + StringOfChar(s[1], i);
      if Length(StrVal) < integer(Arguments[1]) then
        StrVal := StrVal + s[1];
      ResultValue := StrVal;
    end;
    lxAT: begin
      s := string(Arguments[1]);
      StrVal := string(Arguments[0]);
      i := integer(Arguments[2]);
      if i = 1 then
        ResultValue := Pos(StrVal, s)
      else begin
        s := Copy(s, i, Length(s));
        ResultValue := Pos(StrVal, s) + i;
      end;
    end;
    lxIIF: begin
      if boolean(Arguments[0]) then
        ResultValue := Arguments[1]
      else
        ResultValue := Arguments[2];
    end;
    lxMIN:
      ResultValue := Min(double(Arguments[0]), double(Arguments[1]));
    lxMAX:
      ResultValue := Max(double(Arguments[0]), double(Arguments[1]));
    lxMOD:
      ResultValue := integer(Arguments[0]) mod integer(Arguments[1]);
    lxRECNO:;
    lxREPLICATE: begin
      StrVal := string(Arguments[0]);
      ResultValue := '';
      for i := 0 to integer(Arguments[1]) do
        ResultValue := ResultValue + StrVal;
    end;
    lxSPACE:
      ResultValue := StringOfChar(' ', integer(Arguments[0]));
    lxDTOC: begin
      if VarIsNull(Arguments[0]) or not CastValue(Arguments[0], varDate, VarVal) then begin
        StrVal := FormatDateTime('YYYY-MM-DD', 0);
        for i := 1 to Length(StrVal) do
          if CharInSet(StrVal[i], ['a'..'z', 'A'..'Z', '0'..'9']) then
            StrVal[i] := ' ';
        ResultValue := StrVal;
        Exit;
      end;

      if Length(Arguments) = 2 then
        Selector := Arguments[1]
      else
        Selector := 0;

      case Selector of
        0: ResultValue := FormatDateTime('YYYY-MM-DD', TDateTime(VarVal));
        1: ResultValue := FormatDateTime('YYYYMMDD', TDateTime(VarVal));
      end;
    end;
    lxCTOD: begin
      if VarIsNull(Arguments[0]) then begin
        ResultValue := 0;
        Exit;
      end;

      StrVal := string(Arguments[0]);
      if TryStrToInt(Copy(StrVal, 1, 4), n) and TryStrToInt(Copy(StrVal, 5, 2), n1) and TryStrToInt(Copy(StrVal, 7, 2), n2) then
        ResultValue := EncodeDate(n, n1, n2)
      else
        ResultValue := ConvertStringToDateTime(dtDate, StrVal);
    end;
    lxDTOS: begin
      if VarIsNull(Arguments[0]) or not CastValue(Arguments[0], varDate, VarVal) then begin
        ResultValue := '        ';
        Exit;
      end;

      ResultValue := FormatDateTime('YYYYMMDD', TDateTime(VarVal));
    end;
    lxTTOC: begin
      if VarIsNull(Arguments[0]) then begin
        ResultValue := '';
        Exit;
      end;

      if VarIsStr(Arguments[0]) then
        VarVal := ConvertStringToDateTime(dtDate, Arguments[0])
      else
        VarVal := TDateTime(Arguments[0]);

      if Length(Arguments) = 2 then
        Selector := Arguments[1]
      else
        Selector := 1;

      case Selector of
        1: ResultValue := FormatDateTime('yyyymmddhhnnss', TDateTime(VarVal));
        2: ResultValue := FormatDateTime('hh:nn:ss', TDateTime(VarVal));
        3: ResultValue := FormatDateTime('yyyy-mm-ddThh:nn:ss', TDateTime(VarVal));
      end;
    end;
    lxTTOD:
      ResultValue := Trunc(TDateTime(Arguments[0]));
    lxEMPTY: begin
      ResultValue := False;

      if VarIsNull(Arguments[0]) then begin
        ResultValue := True;
        Exit;
      end;

      if VarIsStr(Arguments[0]) then
        ResultValue := string(Arguments[0]) = ''
      else if VarIsNumeric(Arguments[0]) then
        ResultValue := double(Arguments[0]) = 0
      else if VarIsBoolean(Arguments[0]) then
        ResultValue := boolean(Arguments[0]);
    end;
    lxSTRTRAN: begin
      if Length(Arguments) > 2 then
        StrVal := string(Arguments[2])
      else
        StrVal := '';
      ResultValue := StringReplace(string(Arguments[0]), string(Arguments[1]), StrVal, [rfReplaceAll]);
    end;
    lxCHRTRAN: begin
      StrVal := '';
      s := string(Arguments[0]);
      s1 := string(Arguments[1]);
      s2 := string(Arguments[2]);
      n1 := Length(s);
      n2 := Length(s2);
      i := 1;
      while i <= n1 do begin
        n := Pos(s[i], s1);
        if n > 0 then begin
          if n <= n2 then
            StrVal := StrVal + s2[n];
        end
        else
          StrVal := StrVal + s[i];
        Inc(i);
      end;
      ResultValue := StrVal;
    end;
    lxDELETED:
      ResultValue := False;
    lxSTRIPF:;
    lxTRANSFORM: begin
      if VarIsNull(Arguments[0]) then begin
        ResultValue := '';
        Exit;
      end;

      ResultValue := string(Arguments[0]);

      if Length(Arguments) > 1 then begin
//        StrVal := string(Arguments[1]);
//        TODO
        raise Exception.CreateFmt('Invalid arguments for function %s', [FunctionInfo.FFunctionName]);
      end;
    end;
  end;
end;

{ TDBFFunctionDesc }

procedure TDBFFunctionDesc.DoFunction(Context: pSQLite3Context; ParamCount: Integer; pData: IntPtr);
var
  Info: TDBFFunctionInfo;
  ResultValue: Variant;
  InValues: TVariantArray;
begin
  InValues := GetInParams(Context, ParamCount, pData);
  Info := DBFFunctions.Find(Name, InValues);
  Assert(Info <> nil);

  DBFFunctions.Execute(Info, InValues, ResultValue);
  SetResult(Context, ResultValue);
end;

{ TDBFFunctionManager }

function TDBFFunctionManager.GetFunctionDescClass: TLiteFunctionDescClass;
begin
  Result := TDBFFunctionDesc;
end;

procedure TDBFFunctionManager.RegisterDBFFunctions;
var
  i: integer;
  Info: TDBFFunctionInfo;
begin
  for i := 0 to DBFFunctions.Count - 1 do begin
    Info := DBFFunctions[i];
    RegisterFunction(Info.FFunctionName, Info.FArgumentCount, InternalFunction);
  end;
end;

procedure TDBFFunctionManager.InternalFunction(InValues: array of Variant; var ResultValue: Variant);
begin
  // do nothing
end;

initialization
  DBFFunctions := TDBFFunctions.Create;
  DBFFunctions.Add('VAL',       lxVAL,        1, VarArrayOf([atAny]),     caFirst);
  DBFFunctions.Add('ABS',       lxABS,        1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('FLOOR',     lxFLOOR,      1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('CEIL',      lxCEIL,       1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('ROUND',     lxROUND,      1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('INT',       lxINT,        1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('SQRT',      lxSQRT,       1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('LOG',       lxLOG,        1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('LOG10',     lxLOG10,      1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('STR',       lxSTR,        1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('STR',       lxSTR,        2, VarArrayOf([atNumeric, atOrdinal]), caAll);
  DBFFunctions.Add('STR',       lxSTR,        3, VarArrayOf([atNumeric, atOrdinal, atOrdinal]), caAll);
  DBFFunctions.Add('STR',       lxSTR,        4, VarArrayOf([atNumeric, atOrdinal, atOrdinal, atString]), caAll);
  DBFFunctions.Add('CHR',       lxCHR,        1, VarArrayOf([atOrdinal]), caFirst);
  DBFFunctions.Add('ASC',       lxASC,        1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('LTRIM',     lxLTRIM,      1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('TRIM',      lxTRIM,       1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('ALLTRIM',   lxALLTRIM,    1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('LEN',       lxLEN,        1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('LOWER',     lxLOWER,      1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('UPPER',     lxUPPER,      1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('PROPER',    lxPROPER,     1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('ISUPPER',   lxISUPPER,    1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('ISLOWER',   lxISLOWER,    1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('ISDIGIT',   lxISDIGIT,    1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('ISALPHA',   lxISALPHA,    1, VarArrayOf([atString]),  caFirst);
  DBFFunctions.Add('LEFT',      lxLEFT,       2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('RIGHT',     lxRIGHT,      2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('SUBSTR',    lxSUBSTR,     2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('PADL',      lxPADL,       2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('PADL',      lxPADL,       3, VarArrayOf([atString, atOrdinal, atString]), caAll);
  DBFFunctions.Add('PADR',      lxPADR,       2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('PADR',      lxPADR,       3, VarArrayOf([atString, atOrdinal, atString]), caAll);
  DBFFunctions.Add('PADC',      lxPADC,       2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('PADC',      lxPADC,       3, VarArrayOf([atString, atOrdinal, atString]), caAll);
  DBFFunctions.Add('AT',        lxAT,         3, VarArrayOf([atString, atString, atOrdinal]), caAll);
  DBFFunctions.Add('IIF',       lxIIF,        3, VarArrayOf([atBoolean, atAny, atAny]), caFirst);
  DBFFunctions.Add('MIN',       lxMIN,        2, VarArrayOf([atNumeric, atNumeric]), caAll);
  DBFFunctions.Add('MAX',       lxMAX,        2, VarArrayOf([atNumeric, atNumeric]), caAll);
  DBFFunctions.Add('MOD',       lxMOD,        2, VarArrayOf([atOrdinal, atOrdinal]), caAll);
  DBFFunctions.Add('RECNO',     lxRECNO,      0, VarArrayOf([]), caAll);
  DBFFunctions.Add('REPLICATE', lxREPLICATE,  2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('SPACE',     lxSPACE,      1, VarArrayOf([atOrdinal]), caFirst);
  DBFFunctions.Add('DTOC',      lxDTOC,       1, VarArrayOf([atDate]), caNone);
  DBFFunctions.Add('DTOC',      lxDTOC,       2, VarArrayOf([atDate, atOrdinal]), caNone);
  DBFFunctions.Add('CTOD',      lxCTOD,       1, VarArrayOf([atString]), caNone);
  DBFFunctions.Add('DTOS',      lxDTOS,       1, VarArrayOf([atDate]), caNone);
  DBFFunctions.Add('DTOS',      lxDTOS,       1, VarArrayOf([atString]), caNone);
  DBFFunctions.Add('TTOC',      lxTTOC,       1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('TTOC',      lxTTOC,       1, VarArrayOf([atDate]), caFirst);
  DBFFunctions.Add('TTOC',      lxTTOC,       2, VarArrayOf([atNumeric, atOrdinal]), caAll);
  DBFFunctions.Add('TTOC',      lxTTOC,       2, VarArrayOf([atString, atOrdinal]), caAll);
  DBFFunctions.Add('TTOC',      lxTTOC,       2, VarArrayOf([atDate, atOrdinal]), caAll);
  DBFFunctions.Add('TTOD',      lxTTOD,       1, VarArrayOf([atNumeric]), caFirst);
  DBFFunctions.Add('EMPTY',     lxEMPTY,      1, VarArrayOf([atAny]), caNone);
  DBFFunctions.Add('STRTRAN',   lxSTRTRAN,    2, VarArrayOf([atString, atString]), caAll);
  DBFFunctions.Add('STRTRAN',   lxSTRTRAN,    3, VarArrayOf([atString, atString, atString]), caAll);
  DBFFunctions.Add('STRTRAN',   lxSTRTRAN,    4, VarArrayOf([atString, atString, atString, atOrdinal]), caAll);
  DBFFunctions.Add('STRTRAN',   lxSTRTRAN,    5, VarArrayOf([atString, atString, atString, atOrdinal, atOrdinal]), caAll);
  DBFFunctions.Add('CHRTRAN',   lxCHRTRAN,    3, VarArrayOf([atString, atString, atString]), caAll);
  DBFFunctions.Add('DELETED',   lxDELETED,    0, VarArrayOf([]), caAll);
  DBFFunctions.Add('STRIPF',    lxSTRIPF,     0, VarArrayOf([]), caAll);
  DBFFunctions.Add('TRANSFORM', lxTRANSFORM,  1, VarArrayOf([atString]), caFirst);
  DBFFunctions.Add('TRANSFORM', lxTRANSFORM,  2, VarArrayOf([atString, atString]), caAll);

finalization
  DBFFunctions.Free;

{$ENDIF}

end.
