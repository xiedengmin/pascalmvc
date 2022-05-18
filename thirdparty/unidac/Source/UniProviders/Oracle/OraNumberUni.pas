
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  OCINumber
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraNumberUni;

interface

uses
  SysUtils, FMTBcd,
  CRTypes,
{$IFNDEF UNIDACPRO}
  OraCall;
{$ELSE}
  OraCallUni;
{$ENDIF}

type
  TOCINumberValue = array[0..OCI_NUMBER_SIZE - 1] of byte;
  POCINumberValue = ^TOCINumberValue;

  TOCIBCDValue = array[0..SizeOf(TBcd) - 1] of byte;
  POCIBCDValue = ^TOCIBCDValue;

  TOCINumber = class
  public
    //class function Assign(err: pOCIError; const from: pTOCINumber; tonum: pTOCINumber): sword;

    class function Compare(const number1: TBytes; const number2: TBytes): sword;

    class function FromInt(Value: int64; IntLength: uword; SignFlag: uword): TBytes;
    class function ToInt(const Value: TBytes; IntLength: uword; SignFlag: uword): int64;

    class function FromReal(Value: double; RealLength: uword): TBytes;
    class function ToReal(const Value: TBytes; RealLength: uword): double;

    class function FromBCD(const Value: TBcd): TBytes;
    class function ToBCD(pValue: POCINumberValue; len: Byte): TBcd;

    class function Parse(const str: string; const fmt: string; var number: TBytes): sword;
    class function ToString(const number: TBytes; const fmt: string): string; reintroduce;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  OraConsts,
{$ELSE}
  OraConstsUni,
{$ENDIF}
  CRFunctions;

{ TOCINumber }

class function TOCINumber.Compare(const number1: TBytes; const number2: TBytes): sword;

  function IsZero(const number: TBytes): boolean;
  var
    len: integer;
  begin
    Result := false;
    len := Length(number);
    if (len = 1) and (number[0] = Byte(-128)) then begin
      Result := true;
      Exit;
    end;

    if (len = 2) and (number[0] = Byte(-1)) and (number[0] = 101) then begin
      Result := true;//1/0;  // +INF
      Exit;
    end;

    if (len = 1) and (number[0] = 0) then begin
      Result := true;//-1/0;  // -INF
      Exit;
    end;
  end;
var
  len1, len2: integer;
  i, exponent1, exponent2: integer;
  isPositive1, isPositive2: boolean;
begin
  if IsZero(number1) and IsZero(number2) then begin
    Result := 0;
    Exit;
  end;

  isPositive1 := (number1[0] and $ffffff80) <> 0;// is positive
  isPositive2 := (number2[0] and $ffffff80) <> 0;// is positive

  // sign
  if isPositive1 <> isPositive2 then begin
    if isPositive2 then // is positive
      Result := -1
    else
      Result := 1;
    Exit;
  end;

// mantissa convert

  len1 := Length(number1);
  if isPositive1 then // is positive
    exponent1 := Integer(number1[0] and $ffffff7f) - 65
  else begin
    if (len1 - 1 <> 20) or (number1[len1 - 1] = 102) then
      Dec(len1);
    exponent1 := Integer(not number1[0] and $ffffff7f) - 65;
  end;

  len2 := Length(number2);
  if isPositive2 then // is positive
    exponent2 := Integer(number2[0] and $ffffff7f) - 65
  else begin
    if (len2 - 1 <> 20) or (number2[len2 - 1] = 102) then
      Dec(len2);
    exponent2 := Integer(not number2[0] and $ffffff7f) - 65;
  end;

  if exponent2 > exponent1 then begin
    if isPositive1 then
      Result := -1
    else
      Result := 1;
    Exit;
  end
  else if exponent2 < exponent1 then begin
    if isPositive1 then
      Result := 1
    else
      Result := -1;
    Exit;
  end;

  // digits
  i := 1;
  while (i < len1) and (i < len2) do begin
    if (number1[i] > number2[i]) then begin
      Result := 1;
      Exit;
    end
    else if (number1[i] < number2[i]) then begin
      Result := -1;
      Exit;
    end;
    Inc(i);
  end;

  // last digits
  if len1 > len2 then begin
    if isPositive1 then
      Result := 1
    else
      Result := -1;
    Exit;
  end
  else if len1 < len2 then begin
    if isPositive1 then
      Result := -1
    else
      Result := 1;
    Exit;
  end;

  Result := 0;
end;

class function TOCINumber.FromInt(Value: int64; IntLength: uword; SignFlag: uword): TBytes;
var
  mantissa: array[0..19] of byte;
  temp: array[0..19] of byte;
  flag: integer;
  exponent: integer;
  length: byte;
  i, j: integer;
begin
  if Value = 0 then begin
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;

  if Value >= 0 then
    flag := 1
  else
    flag := -1;

  i := 0;
  while Value <> 0 do begin
    temp[i] := Abs(Value mod 100);
    Value := Value div 100;
    Inc(i);
  end;
  Dec(i);

  length := 0;
  exponent := i;
  j := exponent;
  while length <= exponent do begin
    mantissa[length] := temp[j];
    Inc(length);
    Dec(j)
  end;

  while i > 0 do begin
    if mantissa[i] <> 0 then
      break;
    Dec(i);
    Dec(length);
  end;

// mantissa convert

  if flag >= 0 then begin  // positive
    SetLength(Result, length + 1);
    Result[0] := Byte(exponent + 128 + 65);
    for i := 0 to length - 1 do
      Result[i + 1] := mantissa[i] + 1;
  end
  else begin
    if length < 20 then
      SetLength(Result, length + 2)
    else
      SetLength(Result, length + 1);
    Result[0] := not Byte(exponent + 128 + 65);
    i := 0;
    while i < length do begin
      Result[i + 1] := 101 - mantissa[i];
      Inc(i);
    end;

    if i < 20 then
      Result[i + 1] := 102;
  end;
end;

class function TOCINumber.ToInt(const Value: TBytes; IntLength: uword; SignFlag: uword): int64;
var
  mantissa: array[0..30] of byte;
  convDigs: integer;
  exponent: integer;
  len: byte;
  i: integer;
begin
  if (Length(Value) = 1) and (Value[0] = Byte(-128)) then begin
    Result := 0;
    Exit;
  end;

  if (Length(Value) = 2) and (Value[0] = Byte(-1)) and (Value[0] = 101) then begin
    Result := 0;  // +INF
    Exit;
  end;

  if (Length(Value) = 1) and (Value[0] = 0) then begin
    Result := 0;  // -INF
    Exit;
  end;

// mantissa convert

  len := Length(Value);
  if (Value[0] and $ffffff80) <> 0 then begin  // is positive
    mantissa[0] := Byte(Integer(Value[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(Integer(Value[i]) - 1);
  end
  else begin
    if (len - 1 <> 20) or (Value[len - 1] = 102) then
      Dec(len);
    mantissa[0] := Byte(Integer(not Value[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(101 - Integer(Value[i]));
  end;
  exponent := ShortInt(mantissa[0]);
  Dec(len);

// decode

  Result := 0;
  if len <= exponent + 1 then
    convDigs := len
  else
    convDigs := exponent + 1;

  for i := 0 to convDigs - 1 do
    Result := Result * 100 + mantissa[i + 1];

  for i := exponent - len downto 0 do
    Result := Result * 100;

  if Value[0] and $ffffff80 = 0 then  // is not positive
    Result := -Result;
end;

{
d -     value
i -     exponent
byte0 - digits
j -     oIndex
byte1 - digit
abyte0 - mantissa}

class function TOCINumber.FromReal(Value: double; RealLength: uword): TBytes;
type
  TPowerRec = record
    Exponent: Integer;
    Value1: double;
    Value2: double;
    Delta1: double;
    Delta2: double
  end;
const
  powerTable: array [0..7] of TPowerRec = (
    (Exponent: 128; Value1: 1E+256; Value2: 1E-256; Delta1: 1E+240; Delta2: 1E-272),
    (Exponent:  64; Value1: 1E+128; Value2: 1E-128; Delta1: 1E+112; Delta2: 1E-144),
    (Exponent:  32; Value1: 1E+064; Value2: 1E-064; Delta1: 1E+48;  Delta2: 1E-80),
    (Exponent:  16; Value1: 1E+032; Value2: 1E-032; Delta1: 1E+16;  Delta2: 1E-48),
    (Exponent:   8; Value1: 1E+016; Value2: 1E-016; Delta1: 1;      Delta2: 1E-32),
    (Exponent:   4; Value1: 1E+008; Value2: 1E-008; Delta1: 1E-8;   Delta2: 1E-24),
    (Exponent:   2; Value1:  10000; Value2: 0.0001; Delta1: 1E-12;  Delta2: 1E-20),
    (Exponent:   1; Value1:    100; Value2: 0.01;   Delta1: 1E-14;  Delta2: 1E-18)
  );
var
  mantissa: array[0..19] of byte;
  flag: integer;
  exponent: integer;
  digits: byte;
  oIndex: integer;
  length: byte;
  i: integer;
  k: double;
//  xpon: Int64;
  i64: Int64;
  i1, i2: Integer;
begin
  if Value = 0 then begin  // Zero
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;
  if Value = 1/0 then begin  // +INF
    SetLength(Result, 2);
    Result[0] := Byte(-1);
    Result[1] := 101;
    Exit;
  end;
  if Value = -1/0 then begin  // -INF
    SetLength(Result, 1);
    Result[0] := 0;
    Exit;
  end;

  if Value > 0 then
    flag := 1
  else
    flag := -1;
  Value := Abs(Value);
  exponent := 0;
  k := 1;
  if Value < 1 then begin
    for i := Low(powerTable) to High(powerTable) do
      if Value * k - powerTable[i].Value2 <= powerTable[i].Delta2  then begin
        exponent := exponent - powerTable[i].Exponent;
        k := k * powerTable[i].Value1;
      end;

    Dec(exponent);
  end
  else begin
    for i := Low(powerTable) to High(powerTable) do
      if powerTable[i].Value1 - Value * k <= powerTable[i].Delta1 then begin
        exponent := exponent + powerTable[i].Exponent;
        k := k * powerTable[i].Value2;
      end;
  end;

  i := (7 - exponent) shl 1;
  if i > 0 then
    Value := Value * Exponent10(i)
  else if i < 0 then
    Value := Value / Exponent10(-i);

  i64 := Trunc(Value);

//  i64 := Value.Mantissa;
//  xpon := Value.Exponent;
//
//  i := (xpon and $07) - ((not xpon) and $38) + 4;
//  if i > 0 then
//    i64 := i64 shl i
//  else if i < 0 then
//    i64 := i64 shr -i;

  i1 := i64 mod 100000000;
  i2 := i64 div 100000000;

  if (exponent < 7) and (i2 >= 10000000) then begin
    i := i1 mod 100;
    if i >= 99 then
      i1 := i1 + (100 - i)
    else if i <= 1 then
      i1 := i1 - i
    else if Frac(Value) >= 0.5 then
      Inc(i1);
  end
  else
  if i1 mod 10 = 9 then
    if Frac(Value) >= 0.5 then
      Inc(i1);

  if i2 >= 100000000 then begin
    mantissa[8] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[7] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[6] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[5] := i1 mod 100;
    i1 := i1 div 100;

    i2 := i2 + i1;

    mantissa[4] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[3] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[2] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[1] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[0] := i2 mod 100;

    digits := 8;
    if exponent < 0 then
      Inc(exponent)
    else
      Dec(exponent);
  end
  else
  begin
    mantissa[7] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[6] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[5] := i1 mod 100;
    i1 := i1 div 100;
    mantissa[4] := i1 mod 100;
    i1 := i1 div 100;

    i2 := i2 + i1;

    mantissa[3] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[2] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[1] := i2 mod 100;
    i2 := i2 div 100;
    mantissa[0] := i2 mod 100;

    digits := 8;
  end;

  oIndex := digits - 1;
  while (oIndex <> 0) and (mantissa[oIndex] = 0) do begin
    Dec(digits);
    Dec(oIndex);
  end;

  length := digits;

// mantissa convert

  if flag >= 0 then begin  // positive
    SetLength(Result, length + 1);
    Result[0] := Byte(exponent + 128 + 65);
    for i := 0 to length - 1 do
      Result[i + 1] := mantissa[i] + 1;
  end
  else begin
    if length < 20 then
      SetLength(Result, length + 2)
    else
      SetLength(Result, length + 1);
    Result[0] := not Byte(exponent + 128 + 65);
    i := 0;
    while i < length do begin
      Result[i + 1] := 101 - mantissa[i];
      Inc(i);
    end;

    if i < 20 then
      Result[i + 1] := 102;
  end;
end;

class function TOCINumber.ToReal(const Value: TBytes; RealLength: uword): double;
var
  i: integer;
  len: byte;
  mantissa: array[0..30] of byte;
  exponent: integer;
  xpon: integer;
  i1, i2: Integer;
  i64: Int64;
begin
  len := Length(Value);
  if (len = 1) and (Value[0] = Byte(-128)) then begin
    Result := 0;
    Exit;
  end;

  if (len = 2) and (Value[0] = Byte(-1)) and (Value[0] = 101) then begin
    Result := 1/0;  // +INF
    Exit;
  end;

  if (len = 1) and (Value[0] = 0) then begin
    Result := -1/0;  // -INF
    Exit;
  end;

// mantissa convert

  if (Value[0] and $ffffff80) <> 0 then begin  // is positive
    mantissa[0] := Byte(Integer(Value[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(Integer(Value[i]) - 1);
  end
  else begin
    if (len - 1 <> 20) or (Value[len - 1] = 102) then
      Dec(len);
    mantissa[0] := Byte(Integer(not Value[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(101 - Integer(Value[i]));
  end;
  exponent := ShortInt(mantissa[0]);

  i := 1;
  xpon := exponent + 1;

  i1 := 0;
  while (i < len) and (i <= 4) do begin
    i1 := i1 * 100 + mantissa[i];
    Inc(i);
    Dec(xpon);
  end;

  if i < len then begin
    // (j < 8) for result with type Double
    // for Single or Extended datatypes need other value
    i2 := 0;
    repeat
      i2 := i2 * 100 + mantissa[i];
      Inc(i);
      Dec(xpon);
    until (i >= len) or (i > 8);

    case i of
      6:
        i64 := Int64(i1) * 100 + i2;
      7:
        i64 := Int64(i1) * 10000 + i2;
      8:
        i64 := Int64(i1) * 1000000 + i2;
      else
        i64 := Int64(i1) * 100000000 + i2;
    end;

    Result := i64;
  end
  else
    Result := i1;

  if i < len then
    Result := Result + mantissa[i] / 100;

  xpon := xpon shl 1;
  if xpon > 0 then
    Result := Result * Exponent10(xpon)
  else if xpon < 0 then
    Result := Result / Exponent10(-xpon);

  if Value[0] and $ffffff80 = 0 then  // is not positive
    Result := -Result;
end;

class function TOCINumber.Parse(const str: string; const fmt: string; var number: TBytes): sword;
const
  CharToNum: array[$30..$39] of Byte = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
var
  i, ind, exponent: Integer;
  IsFirstDigit, IsPositive, flag: Boolean;
  state, DigitInd: Integer;
  ch: Char;
  bytesCount, digInd, nonZeroStart, nonZeroEnd, pointInd: integer;
begin
  ind := 1;
  while ind <= Length(str) do
    if str[ind] = ' ' then
      Inc(ind)
    else
      break;

  digInd := 0;
  nonZeroStart := -1;
  nonZeroEnd := -1;
  pointInd := -1;
  for i := ind to Length(str) do begin
    ch := str[i];
    if ch = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator then begin
      pointInd := digInd;
      continue;
    end;
    if (ch >= '0') and (ch <= '9') then begin
      Inc(digInd);
      if ch <> '0' then begin
        if nonZeroStart = -1 then
          nonZeroStart := digInd;
        if nonZeroStart <> -1 then
          nonZeroEnd := digInd;
      end;
    end;
  end;
  if pointInd = -1 then
    pointInd := digInd;

  Result := 0;
  if nonZeroEnd = -1 then begin  // Zero
    SetLength(number, 1);
    number[0] := Byte(-128);
    Exit;
  end;

  if pointInd >= nonZeroStart then
    exponent := (pointInd - nonZeroStart) div 2
  else
    exponent := -((nonZeroStart - pointInd - 1) div 2) - 1;

  IsFirstDigit := ((nonZeroStart xor pointInd) and 1) <> 0;

  bytesCount := nonZeroEnd - nonZeroStart + 2;
  if not IsFirstDigit then
    Inc(bytesCount);
  bytesCount := bytesCount shr 1;

  IsPositive := true;
  flag := false;
  // sign
  if ind <= Length(str) then begin
    ch := str[ind];
    if ch = '-' then begin
      IsPositive := false;
      Inc(Ind);
    end
    else if ch = '+' then begin
      IsPositive := true;
      Inc(Ind);
    end
    else if ((ch < '0') or (ch > '9')) and (ch <> '+') and (char(ch) <> {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then
      Result := 1;
  end;

  if IsPositive then begin  // positive
    SetLength(number, bytesCount + 1);
    number[0] := Byte(exponent + 128 + 65);
  end
  else begin
    if bytesCount < 20 then begin
      SetLength(number, bytesCount + 2);
      number[bytesCount + 1] := 102;
    end
    else
      SetLength(number, bytesCount + 1);
    number[0] := not Byte(exponent + 128 + 65);
  end;

  DigitInd := 1;
  state := 1;
  number[1] := 0;
  if bytesCount > 20 then
    Result := 1;
  while (ind <= Length(str)) and (Result = 0) and (DigitInd <= bytesCount) do begin
    ch := str[ind];
    if (state = 1) and (char(ch) = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator) then begin
      Inc(state);
    end
    else if (ch >= '0') and (ch <= '9') then begin
      if flag or (ch <> '0') then begin
        flag := true;
        if IsFirstDigit then
          number[DigitInd] := CharToNum[Byte(ch)] * 10
        else begin
          number[DigitInd] := number[DigitInd] + 1 + CharToNum[Byte(ch)];
          Inc(DigitInd);
        end;
        IsFirstDigit := not IsFirstDigit;
      end;
    end
    else if (ch = ' ') then
      break
    else
      Result := 1;
    Inc(ind);
  end;
  if Result <> 0 then
    raise Exception.Create(Format('''%s'' is not a valid Oracle Number value', [str]));
  if not IsFirstDigit then
    number[DigitInd] := number[DigitInd] + 1;

  if not IsPositive then begin  // positive
    for i := 1 to bytesCount do
      number[i] := 102 - number[i];
  end;
end;

class function TOCINumber.ToString(const number: TBytes; const fmt: string): string;
var
  mantissa: array[0..30] of byte;
  exponent: integer;
  len: byte;
  i, DotStrPos: integer;
  xpon: integer;
  j: integer;
  //Num: double;
begin
  //Num := ToReal(number, sizeof(Double));
  //Result := FloatToStr(Num);
  //exit;

  len := Length(number);
  if (len = 1) and (number[0] = Byte(-128)) then begin
    Result := '0';
    Exit;
  end;

  if (len = 2) and (number[0] = Byte(-1)) and (number[0] = 101) then begin
    Result := '0';//1/0;  // +INF
    Exit;
  end;

  if (len = 1) and (number[0] = 0) then begin
    Result := '0';//-1/0;  // -INF
    Exit;
  end;

// mantissa convert

  if (number[0] and $ffffff80) <> 0 then begin  // is positive
    mantissa[0] := Byte(Integer(number[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(Integer(number[i]) - 1);
  end
  else begin
    if (len - 1 <> 20) or (number[len - 1] = 102) then
      Dec(len);
    mantissa[0] := Byte(Integer(not number[0] and $ffffff7f) - 65);
    for i := 1 to len - 1 do
      mantissa[i] := Byte(101 - Integer(number[i]));
  end;
  exponent := ShortInt(mantissa[0]);

  i := 1;
  xpon := exponent + 1;

  SetLength(Result, (len - i) * 2);
  j := 1;
  while (i < len) do begin
    if mantissa[i] < 10 then begin
      Result[j] := '0';
      Result[j + 1] := Char($30 + mantissa[i]);
    end
    else begin
      Result[j] := Chr($30 + mantissa[i] div 10);
      Result[j + 1] := Char($30 + mantissa[i] mod 10);
    end;
    Inc(j, 2);
    Inc(i);
    Dec(xpon);
  end;

  DotStrPos := Length(Result) + xpon * 2 + 1;
  if (xpon >= 0) then begin                      // 12345v>=00.
    while xpon > 0 do begin
      Result := Result + '00';
      Dec(xpon);
    end
  end
  else begin
    if DotStrPos > 1 then begin                    //  v>12.345<
      Insert({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, Result, DotStrPos)
    end
    else begin
      while DotStrPos < 1 do begin                //.00<=v12345
        Result := '0' + Result;
        Inc(DotStrPos);
      end;
      Result := '0' + {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator + Result;
      DotStrPos := 2;
    end;
  end;

  if (Length(Result) > 1) and (DotStrPos < Length(Result)) then begin
    while Result[Length(Result)] = '0' do
      Delete(Result, Length(Result), 1);
  end;
  if (Length(Result) > 1) then begin
    while (DotStrPos > 1) and (Result[1] = '0') do
      Delete(Result, 1, 1);
  end;

  {if ThousandSeparator <> #0 then begin
    i := DotStrPos + 4;
    while i <= Length(Result) do begin
      Insert(ThousandSeparator, Result, i);
      Inc(i, 4);
    end;
    i := DotStrPos - 3;
    while i > 1 do begin
      Insert(ThousandSeparator, Result, i);
      Dec(i, 3);
    end;
  end;}

  if number[0] and $ffffff80 = 0 then  // is not positive
    Result := '-' + Result;
end;

class function TOCINumber.FromBCD(const Value: TBcd): TBytes;
var
  i, exponent: Integer;
  IsFirstDigit, IsFirstDigitBCD, IsPositive: Boolean;
  NumberInd, BCDInd: Integer;
  b: byte;
  bytesCount: integer;
  OldPrecision, Precision, Places: integer;
begin
  if (Value.Precision = 0) or (Byte(Value.Precision) > 64) or ((Value.SignSpecialPlaces and $40) > 0) then begin  // Zero
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;
  Precision := Value.Precision;
  IsPositive := (Value.SignSpecialPlaces and $80) = 0;
  Places := Value.SignSpecialPlaces and $3f;

  // last nulls
  i := ((Precision + 1) shr 1) - 1;
  while i >=0 do begin
    if Value.Fraction[i] <> 0 then begin
      if (Precision and 1) = 0 then begin
        if (Value.Fraction[i] and $0f) = 0 then begin //last digit is null
          Dec(Precision);
          Dec(Places);
        end;
      end
      else begin
        if (Value.Fraction[i] and $f0) = 0 then begin //first digit is null
          Dec(Precision);
          Dec(Places);
        end;
      end;
      break;
    end;
    Dec(Precision, 2);
    Dec(Places, 2);
    Dec(i);
  end;

  BCDInd := 0;
  IsFirstDigitBCD := true;
  // first nulls
  OldPrecision := ((Precision + 1) shr 1) - 1;
  for i := 0 to OldPrecision do begin
   if Value.Fraction[i] <> 0 then begin
     if (Value.Fraction[i] and $f0) = 0 then begin
       IsFirstDigitBCD := false;
       Dec(Precision);
     end;
     break;
   end;
   Inc(BCDInd);
   Dec(Precision, 2);
  end;

  if Precision = 0 then begin
    SetLength(Result, 1);
    Result[0] := Byte(-128);
    Exit;
  end;

  IsFirstDigit := ((Precision - Places) and 1) = 0;

  if Precision - Places >= 0 then
    exponent := ((Precision - Places + 1) div 2) - 1
  else
    exponent := ((Precision - Places) div 2) - 1;

  bytesCount := (Precision + 1 + ((Precision - Places) and 1)) shr 1;

  if IsPositive then begin  // positive
    SetLength(Result, bytesCount + 1);
    Result[0] := Byte(exponent + 128 + 65);
  end
  else begin
    if bytesCount < 20 then begin
      SetLength(Result, bytesCount + 2);
      Result[bytesCount + 1] := 102;
    end
    else
      SetLength(Result, bytesCount + 1);
    Result[0] := not Byte(exponent + 128 + 65);
  end;

  NumberInd := 1;
  Result[1] := 0;
  for i := 1 to Precision do begin
    if IsFirstDigitBCD then
      b := (Value.Fraction[BCDInd] and $f0) shr 4
    else begin
      b := Value.Fraction[BCDInd] and $0f;
      Inc(BCDInd);
    end;
    IsFirstDigitBCD := not IsFirstDigitBCD;

    if IsFirstDigit then
      Result[NumberInd] := b * 10
    else begin
      Result[NumberInd] := Result[NumberInd] + b + 1;
      Inc(NumberInd);
    end;
    IsFirstDigit := not IsFirstDigit;
  end;
  {if Result <> 0 then
    raise Exception.Create(Format('''%s'' is not a valid Oracle Number value', [str]));}
  if not IsFirstDigit then
    Result[NumberInd] := Result[NumberInd] + 1;

  if not IsPositive then begin  // positive
    for i := 1 to bytesCount do
      Result[i] := 102 - Result[i];
  end;
end;

class function TOCINumber.ToBCD(pValue: POCINumberValue; len: Byte): TBcd;
var
  i: Integer;
  b: byte;
  BCDInd: Integer;
  exponent: integer;
  Precision: integer;
  IsPositive, IsFirstDigit, IsFirstDigitBCD: Boolean;
begin
  System.FillChar(Result, SizeOf(TBcd), 0);

  if (len = 1) and (pValue[0] = $80) then begin
    Result.Precision := 1; // "0"
    Exit;
  end;

  if (len = 1) and (pValue[0] = $00) then
    Result := DoubleToBcd(-1/0); // -INF

  if (len = 2) and (pValue[0] = $FF) and (pValue[1] = $65) then
    Result := DoubleToBcd(1/0); // +INF

  // mantissa convert

  if (pValue[0] and $80) <> 0 then begin  // is positive
    IsPositive := true;
    Result.SignSpecialPlaces := 0;
    exponent := Integer(pValue[0] and $7F) - 65;
  end
  else begin
    IsPositive := false;
    Result.SignSpecialPlaces := $80;
    if (len - 1 <> 20) or (pValue[len - 1] = $66) then
      Dec(len);
    exponent := Integer((not pValue[0]) and $7F) - 65;
  end;
  exponent := exponent * 2 + 1;

  // copy digits
  if len >= 2  then begin
    if IsPositive then
      IsFirstDigit := ((pValue[1] - 1) div 10) <> 0
    else
      IsFirstDigit := ((101 - pValue[1]) div 10) <> 0;
    IsFirstDigitBCD := True;
    BCDInd := 0;

    Precision := (len - 1) * 2;
    if not IsFirstDigit then
      Dec(Precision)
    else
      Inc(exponent);

    // correct precision
    if IsPositive then
      b := Byte(pValue[len - 1] - 1)
    else
      b := Byte(101 - pValue[len - 1]);
    if (b mod 10) = 0 then
      Dec(Precision);

    // for 0,<00..0>123 numbers
    if exponent < 0 then begin
      if Precision - exponent > 64 then
        raise Exception.Create(SCannotConvertNumberToBCD);
      BCDInd := - exponent div 2;
      IsFirstDigitBCD := (exponent and 1) = 0;
      Precision := Precision - exponent;
      exponent := 0;
    end;

    i := 1;
    repeat
      if IsPositive then
        b := Byte(pValue[i] - 1)
      else
        b := Byte(101 - pValue[i]);

      // first digit from Number
      if IsFirstDigit then begin
        if IsFirstDigitBCD then
          Result.Fraction[BCDInd] := (b div 10) shl 4
        else begin
          Result.Fraction[BCDInd] := Result.Fraction[BCDInd] + (b div 10);
          Inc(BCDInd);
        end;
        IsFirstDigitBCD := not IsFirstDigitBCD;
      end
      else
        IsFirstDigit := true;

      // second digit from Number
      if BCDInd <= 31 then begin
        if IsFirstDigitBCD then
          Result.Fraction[BCDInd] := (b mod 10) shl 4
        else begin
          Result.Fraction[BCDInd] := Result.Fraction[BCDInd] + (b mod 10);
          Inc(BCDInd);
        end;
      end;
      IsFirstDigitBCD := not IsFirstDigitBCD;

      Inc(i);
    until i >= len;
    if (b mod 10) = 0 then begin
      if IsFirstDigitBCD then
        Dec(BCDInd);
      IsFirstDigitBCD := not IsFirstDigitBCD;
    end;
  end
  else begin
    Precision := 0;
    IsFirstDigitBCD := false;
    BCDInd := 0;
  end;

  exponent := Precision - exponent;
  while exponent < 0 do begin
    if BCDInd > 31 then
      raise Exception.Create(SCannotConvertNumberToBCD);
    if IsFirstDigitBCD then
      Result.Fraction[BCDInd] := 0
    else begin
      Inc(BCDInd);
    end;
    IsFirstDigitBCD := not IsFirstDigitBCD;
    Inc(Precision);
    Inc(exponent);
  end;

  Result.Precision := Precision;
  Result.SignSpecialPlaces := Result.SignSpecialPlaces or exponent;
end;

end.
