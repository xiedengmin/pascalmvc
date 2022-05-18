
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Numeric
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRNumeric;

interface

uses
  SysUtils,
{$IFNDEF FPC}
  DBConsts,
{$ENDIF}
  FMTBcd, DAConsts, CRTypes, CRFunctions, CRBigInteger, MemUtils;
{$IFDEF MSWINDOWS}
  {$HPPEMIT '#include <OleDB.h>'}
{$ENDIF}

type
  PDBNumeric = ^TDBNumeric;
  DB_NUMERIC = packed record
    precision: Byte;
    scale: Byte;
    sign: Byte;
    case Integer of
      0: (
        Val: array [0..15] of byte;
      );
      1: (
        ValLow: int64;
        ValHigh: int64;
      );
  end;
  TDBNumeric = DB_NUMERIC;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM DB_NUMERIC}
  {$EXTERNALSYM PDBNumeric}
  {$EXTERNALSYM TDBNumeric}
{$ENDIF}

const
  SizeOf_TDBNumeric = SizeOf(TDBNumeric);

function DBNumericToStr(const Value: TDBNumeric): string;
function DBNumericToBCD(Value: TDBNumeric): TBCD;
function DBNumericToDouble(const Value: TDBNumeric): double;
function BcdToDBNumeric(const Bcd: TBcd): TDBNumeric;
function DoubleToDBNumeric(Value: double; Precision, Scale: integer): TDBNumeric;

implementation

const
  TwoDigitLookup: packed array[0..99] of array[1..2] of Char =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

function DBNumericToStr(const Value: TDBNumeric): string;
var
  Quotient: array[0..7] of word;
  Remainder: word;
  Dividend: cardinal;
  Index, i: integer;
  Buf: array[1..41] of Char;
  PBuf: PChar;
  Len, Prec: integer;
  IsNegative: boolean;
begin
  Move(Value.Val[0], Quotient[0], sizeof(Quotient));
  Index := High(Quotient);
  while (Index >= 0) and (Quotient[Index] = 0) do
    Dec(Index);

  PBuf := @Buf[High(Buf)];
  while Index >= 0 do begin
    i := Index;
    Remainder := 0;
    while i >= 0 do begin
      Dividend := (cardinal(Remainder) shl 16) or Quotient[i];
      Remainder := Dividend mod 100;
      Quotient[i] := Dividend div 100;
      Dec(i);
    end;

    if Quotient[Index] = 0 then
      Dec(Index);

    Dec(PBuf, 2);
  {$IFDEF IS_UNICODE}
    PCardinal(PBuf)^ := PCardinal(@TwoDigitLookup[Remainder])^;
  {$ELSE}
    PWord(PBuf)^ := PWord(@TwoDigitLookup[Remainder])^;
  {$ENDIF}
  end;

  if PBuf^ = '0' then
    Inc(PBuf);

  Len := PtrSubstract(@Buf[High(Buf)], PBuf) div SizeOf(Char);

  if Len <= 0 then begin
    if Value.Scale > 0 then begin
      SetLength(Result, Value.Scale + 2);
      Result[1] := '0';
      Result[2] := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
      for i := 3 to Length(Result) do
        Result[i] := '0';
    end
    else
      Result := '0';
  end
  else begin
    IsNegative := Value.Sign <> 1;

    if Value.Scale > 0 then begin
      if Len > Value.Scale then begin
        Prec := Len - Value.Scale;
        SetLength(Result, Len + 1{.} + Ord(IsNegative));
        Move(PBuf[0], Result[1 + Ord(IsNegative)], Prec * SizeOf(Char));
        Result[1 + Ord(IsNegative) + Prec] := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
        Move(PBuf[Prec], Result[2 + Ord(IsNegative) + Prec], Value.Scale * SizeOf(Char));
      end
      else begin
        SetLength(Result, Value.Scale + 1{0} + 1{.} + Ord(IsNegative));
        Result[1 + Ord(IsNegative)] := '0';
        Result[2 + Ord(IsNegative)] := {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
        for i := 1 to Value.Scale - Len do
          Result[2 + Ord(IsNegative) + i] := '0';
        Move(PBuf^, Result[3 + Ord(IsNegative) + Value.Scale - Len], Len * SizeOf(Char));
      end;
    end
    else begin
      SetLength(Result, Len + Ord(IsNegative));
      Move(PBuf^, Result[1 + Ord(IsNegative)], Len * SizeOf(Char));
    end;

    if IsNegative then
      Result[1] := '-';
  end;
end;

function DBNumericToBCD(Value: TDBNumeric): TBCD;
var
  SignificantBytes: integer;

  function FindStart: boolean;
  begin
    SignificantBytes := 16;
    while (SignificantBytes > 0) and (Value.Val[SignificantBytes - 1] = 0) do
      Dec(SignificantBytes);
    Result := SignificantBytes > 0;
  end;

var
  i, j, k: integer;
  Remainder, tmp: word;
begin
  if Value.Sign = 0 then
    Result.SignSpecialPlaces := Value.Scale or $80
  else
    Result.SignSpecialPlaces := Value.Scale;

  System.FillChar(Result.Fraction, Length(Result.Fraction), 0);

  if not FindStart then begin
    Result.Precision := 8; // if value is zero
    Result.SignSpecialPlaces := 2;
    Exit;
  end;

  k := SignificantBytes - 1;
  j := 31;
  while k >= 0 do begin
    Remainder := 0;
    for i := k downto 0 do begin
      tmp := (Byte(Value.Val[i]) + Remainder);
      Value.Val[i] := tmp div 100;
      Remainder := (tmp mod 100) shl 8;
    end;
    Result.Fraction[j] := (((Remainder shr 8) mod 10)) + (((Remainder shr 8) div 10) shl 4);
    if Value.Val[k] = 0 then
      Dec(k);
    Dec(j);
  end;

  Result.Precision := Value.Precision;
  i := 31 - (Value.Precision shr 1);
  j := i;

  if (Value.Precision and 1) <> 0 then begin
    while i <= 30 do begin
      Result.Fraction[i - j] := (Result.Fraction[i] and $0f) shl 4 + Result.Fraction[i + 1] shr 4;
      Inc(i);
    end;
    Result.Fraction[i - j] := (Result.Fraction[i] and $0f) shl 4;
  end
  else begin
    while i <= 30 do begin
      Result.Fraction[i - j] := Result.Fraction[i + 1];
      Inc(i);
    end;
  end;

  for i := 31 - j + 1 to 31 do
    Result.Fraction[i] := 0;
end;

function DBNumericToDouble(const Value: TDBNumeric): double;
var
  Quotient: array[0..7] of word;
  Remainder: word;
  Dividend: cardinal;
  i, Index: integer;
  Digits: Integer;
  i1, i2: Integer;
  i64: Int64;
  mantissa: array[0..30] of byte;
begin
  Move(Value.Val[0], Quotient[0], sizeof(Quotient));
  Index := High(Quotient);
  while (Index >= 0) and (Quotient[Index] = 0) do
    Dec(Index);

  Digits := 0;
  while Index >= 0 do begin
    i := Index;
    Remainder := 0;
    while i >= 0 do begin
      Dividend := (cardinal(Remainder) shl 16) or Quotient[i];
      Remainder := Dividend mod 100;
      Quotient[i] := Dividend div 100;
      Dec(i);
    end;

    if Quotient[Index] = 0 then
      Dec(Index);

    mantissa[Digits] := Remainder;
    Inc(Digits);
  end;

  Index := Digits - 1;
  i1 := 0;
  while (Index >= 0) and (Digits - Index <= 4) do begin
    i1 := i1 * 100 + mantissa[Index];
    Dec(Index);
  end;

  if Index >= 0 then begin
    i2 := 0;
    repeat
      i2 := i2 * 100 + mantissa[Index];
      Dec(Index);
    until (Index < 0) or (Digits - Index > 8);

    case Digits - Index of
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

  if Index >= 0 then
    Result := Result + mantissa[Index] / 100;

  i := -Value.scale;
  if Value.precision > 16 then
    i := i + (Index + 1) * 2;

  if i > 0 then
    Result := Result * Exponent10(i)
  else if i < 0 then
    Result := Result / Exponent10(-i);

  if Value.Sign <> 1 then
    Result := -Result;
end;

function BcdToDBNumeric(const Bcd: TBcd): TDBNumeric;
var
  Value: TBigInteger;
  Buf: TBytes;
  BytesCount: integer;
begin
  Value := TBigInteger.Create(Bcd);
  try
    Buf := Value.GetBytesLE;
  finally
    Value.Free;
  end;

  if Length(Buf) > Length(Result.val) then
    raise Exception.Create(SNumericOverflow);

  Result.precision := Bcd.Precision;
  Result.scale := Bcd.SignSpecialPlaces and $3F;

  if IsBcdNegative(Bcd) then
    Result.Sign := 0
  else
    Result.Sign := 1;

  BytesCount := Bcd.Precision shr 1;
  if (Bcd.Precision and 1) <> 0 then
    Inc(BytesCount);

  FillChar(@Result.val[0], Length(Result.val), $00);
  if BytesCount > Length(Buf) then
    BytesCount := Length(Buf);

  Move(Buf[0], Result.val[0], BytesCount);
end;

function DoubleToDBNumeric(Value: double; Precision, Scale: integer): TDBNumeric;
var
  Bcd: TBcd;
  Str: string;
begin
  //Str := FloatToStrF(Value, ffFixed, Precision, Scale);
  if (Precision = 0) and (Scale = 0) then
    Str := FloatToStr(Value) //Format('%f', [Value])
  else
    Str := Format('%.' + IntToStr(Scale) + 'f', [Value]);
  Bcd := StrToBcd(Str);
  Result := BcdToDBNumeric(Bcd);
end;

end.

