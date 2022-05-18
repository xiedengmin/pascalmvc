
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  BigInteger
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRBigInteger;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

{$IFNDEF PUREPASCAL}
  {$IFDEF CPU64}
    {$DEFINE PUREPASCAL}
  {$ENDIF}
{$ENDIF}

uses
  Classes, SysUtils, SyncObjs,
  FMTBcd, CLRClasses, CRTypes, CRFunctions, CRRNG;

type
  TBigInteger = class(TPersistent)
  private
    FData: TLongWordArr; // stores bytes from the Big Integer
    FDataLength: integer; // number of actual chars used
    FLock: TCriticalSection;
    FIsPrepared: boolean;
    FR1, FR2, FConstant: TBigInteger;
    FPolyArrray: TIntArr;

    procedure ClearBarrettReduction;
    procedure Truncate;
    class function ShiftLeft(var Buffer: TLongWordArr; DataLength: integer; ShiftVal: integer): integer;
    class function ShiftRight(var Buffer: TLongWordArr; ShiftVal: integer): integer;
    class procedure MultiByteDivide(bi1, bi2: TBigInteger;
      outQuotient, outRemainder: TBigInteger);
    class procedure SingleByteDivide(bi1, bi2: TBigInteger;
      outQuotient, outRemainder: TBigInteger);
    function RabinMillerTest(Confidence: integer): boolean;

    class procedure Mul_1x1_GF2m(out r1, r0: cardinal; const a, b: cardinal);
    class procedure Mul_2x2_GF2m(var R: array of cardinal; a1, a0, b1, b0: cardinal);

  public
    constructor Create; overload;
    constructor Create(Value: Int64); overload;
    constructor Create(Value: Int64; IsPositive: boolean); overload;
    constructor Create(Src: TBigInteger); overload;
    constructor Create(const Value: string; Radix: integer); overload;
    constructor Create(const Data: TBytes); overload;
    constructor Create(const Data: TBytes; Offset, Count: integer); overload;
    constructor Create(const Data: array of cardinal); overload;
    constructor Create(const Value: TBcd); overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure SetToZero;

    procedure PrepareForBarrettReduction;
    function Add(bi: TBigInteger): TBigInteger; overload;
    function Add(Value: Int64): TBigInteger; overload;
    function Minus(bi: TBigInteger): TBigInteger; overload;
    function Minus(Value: Int64): TBigInteger; overload;
    function IsNegative: boolean;
    function IsNegativeOrZero: boolean;
    function Negate: TBigInteger;
    function ModMul(bi, modulus: TBigInteger): TBigInteger;
    function BarrettReduction(n: TBigInteger): TBigInteger;
    function Mul(bi: TBigInteger): TBigInteger; overload;
    function Mul(Value: Int64): TBigInteger; overload;
    function Shl_(ShiftVal: integer): TBigInteger;
    function Shr_(ShiftVal: integer): TBigInteger;
    procedure Shr_1;
    procedure SetBit(BitNum: integer);
    function GetBit(BitNum: integer): integer;
    function Equal(bi: TBigInteger): boolean;
    function NotEqual(bi: TBigInteger): boolean;
    function Greater(bi: TBigInteger): boolean;
    function Less(bi: TBigInteger): boolean;
    function GreaterOrEqual(bi: TBigInteger): boolean;
    function LessOrEqual(bi: TBigInteger): boolean;
    function Div_(bi: TBigInteger): TBigInteger;
    function Mod_(bi: TBigInteger): TBigInteger; overload;
    function Mod_(Value: Int64): TBigInteger; overload;
    function ModInverse(modulus: TBigInteger): TBigInteger;
    function Max_(bi: TBigInteger): TBigInteger;
    function Min_(bi: TBigInteger): TBigInteger;
    function Or_(bi: TBigInteger): TBigInteger;
    function And_(bi: TBigInteger): TBigInteger;
    function Xor_(bi: TBigInteger): TBigInteger;
    procedure XorSelf(bi: TBigInteger);
    function Abs: TBigInteger;
    function ToString: string; overload; {$IFDEF VER12P}override;{$ENDIF}{$IFDEF FPC}override;{$ENDIF}
    function ToString(Radix: integer): string; {$IFDEF VER12P}reintroduce;{$ENDIF} overload;
    function ModPow(exp, n: TBigInteger): TBigInteger;
    function gcd(bi: TBigInteger): TBigInteger;
    procedure GenRandomBits(Bits: integer; Rand: IScRandom);
    function BitCount: integer;
    function IsOdd: boolean;
    function LongValue: UInt64;
    function IntValue: cardinal;
    function GetBytes: TBytes; overload;
    function GetBytes(Count: integer): TBytes; overload;
    function GetBytesLE: TBytes;
    function GetData: TLongWordArr;
    class function GenPseudoPrime(Bits: integer; Confidence: integer;
      Rand: IScRandom): TBigInteger;
    function IsProbablePrime(Confidence: integer): boolean;

    function GetSetBitsArray: TIntArr;
    procedure PrepareForGF2mCalc;
    function ModSqr_GF2m(modulus: TBigInteger): TBigInteger;
    function ModMul_GF2m(bi: TBigInteger; modulus: TBigInteger): TBigInteger;
    function ModInv_GF2m(modulus: TBigInteger): TBigInteger;
    function ModDiv_GF2m(bi: TBigInteger; modulus: TBigInteger): TBigInteger;
    function Mod_GF2m(modulus: TBigInteger): TBigInteger;

    class function PutBigIntegerLE(bi: TBigInteger; var Dest: TBytes; Offset: integer): integer;
    class function GetBigIntegerLE(const Src: TBytes; Offset: integer; Count: integer): TBytes; overload;
    class function GetBigIntegerLE(const Src: TBytes): TBytes; overload;
  end;

implementation

uses
  DAConsts;

var
  TestRandom: IScRandom;

// primes smaller than 2000 to test the generated prime number
const PrimesBelow2000: array[0..302] of integer = (
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
        101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199,
        211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293,
        307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397,
        401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499,
        503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599,
        601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691,
        701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797,
        809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887,
        907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997,
        1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097,
        1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193,
        1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297,
        1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399,
        1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499,
        1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597,
        1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699,
        1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789,
        1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889,
        1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993, 1997, 1999
      );

{ TBigInteger }

destructor TBigInteger.Destroy;
begin
  // to provide security
  if Length(FData) > 0 then
    FillChar(FData[0], Length(FData)* SizeOf(cardinal), 0);

  FR1.Free;
  FR2.Free;
  FConstant.Free;
  FLock.Free;

  inherited;
end;

//***********************************************************************
// Constructor (Default Value provided by long)
//***********************************************************************
constructor TBigInteger.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
end;

constructor TBigInteger.Create(Value: Int64);
begin
  Create(Value, False);
end;

constructor TBigInteger.Create(Value: Int64; IsPositive: boolean);
var
  IsNeg: boolean;
  Carry, Val: UInt64;
  i: integer;
begin
  Create;

  IsNeg := Value < 0;
  if IsNeg and not IsPositive then
    Value := -Value;

  SetLength(FData, 3);
  FData[0] := cardinal(Value);
  Value := Value shr 32;
  FData[1] := cardinal(Value);
  if FData[1] = 0 then
    FDataLength := 1
  else
    FDataLength := 2;

  if IsNeg and not IsPositive then begin
    // 1's complement
    for i := 0 to Length(FData) - 1 do
      FData[i] := cardinal(not FData[i]);

    // add one to Result of 1's complement
    Carry := UInt64(1);
    i := 0;
    while (Carry <> 0) and (i < FDataLength) do begin
      Val := UInt64(FData[i]);
      Inc(Val);

      FData[i] := cardinal(Val);
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(Val shr 32);
      Inc(i);
    end;
  end;
end;

//***********************************************************************
// Constructor (Default Value provided by BigInteger)
//***********************************************************************
constructor TBigInteger.Create(Src: TBigInteger);
begin
  Create;

  SetLength(FData, Length(Src.FData));
  FDataLength := Src.FDataLength;
  if Length(FData) > 0 then
    Buffer.BlockCopy(Src.FData, 0, FData, 0, Length(FData) * SizeOf(cardinal));
end;

//***********************************************************************
// Constructor (Default Value provided by a string of digits of the
//              specified base)
// Example (base 10)
// -----------------
// To initialize "a" with the default Value of 1234 in base 10
//
// Example (base 16)
// -----------------
// To initialize "a" with the default Value of $1D4F in base 16
//
// Note that string Values are specified in the <sign><magnitude>
// format.
//
//***********************************************************************
constructor TBigInteger.Create(const Value: string; Radix: integer);
var
  Multiplier: TBigInteger;
  Result, TmpMul, temp: TBigInteger;
  Limit, PosVal: integer;
  Val: string;
  i: integer;
begin
  Create;

  Multiplier := TBigInteger.Create(1);
  Result := TBigInteger.Create(0);
  try
    Val := Trim(UpperCase(Value));
    Limit := 1;
    if Val[1] = '-' then
      Limit := 2;

    for i := Limit to Length(Val) do begin
      if Val[i] = '0' then
        Inc(Limit)
      else
        break;
    end;

    for i := Length(Val) downto Limit do begin
      PosVal := Ord(Val[i]);

      if (PosVal >= Ord('0')) and (PosVal <= Ord('9')) then
        PosVal := PosVal - Ord('0')
      else if (PosVal >= Ord('A')) and (PosVal <= Ord('Z')) then
        PosVal := (PosVal - Ord('A')) + 10
      else
        PosVal := 9999999; // arBitrary large

      if PosVal >= Radix then
        raise Exception.Create(seInvalidInputArgs)
      else begin
        TmpMul := Multiplier.Mul(PosVal);
        try
          temp := Result;
          Result := Result.Add(TmpMul);
          temp.Free;
        finally
          TmpMul.Free;
        end;

        if (i - 1) >= Limit then begin
          temp := Multiplier;
          Multiplier := Multiplier.Mul(Radix);
          temp.Free;
        end;
      end;
    end;

    if Val[1] = '-' then begin
      temp := Result;
      Result := Result.Negate;
      temp.Free;
      if not Result.IsNegative then
        raise Exception.Create(seNegativeUnderflow)
    end
    else
      if Result.IsNegative then
        raise Exception.Create(sePositiveOverflow);

    FDataLength := Result.FDataLength;
    SetLength(FData, Length(Result.FData));
    if Length(FData) > 0 then
      Buffer.BlockCopy(Result.FData, 0, FData, 0, Length(FData) * SizeOf(cardinal));
  finally
    Multiplier.Free;
    Result.Free;
  end;
end;

//***********************************************************************
// Constructor (Default Value provided by an array of bytes)
//
// The lowest index of the input byte array (i.e [0]) should contain the
// most significant byte of the number, and the highest index should
// contain the least significant byte.
//
// Note that this method of initialization does not allow the
// sign to be specified.
//
//***********************************************************************
constructor TBigInteger.Create(const Data: TBytes);
begin
  Create(Data, 0, Length(Data));
end;

//***********************************************************************
// Constructor (Default Value provided by an array of bytes of the
// specified length.)
//***********************************************************************
constructor TBigInteger.Create(const Data: TBytes; Offset, Count: integer);
var
  LeftOver: integer;
  i, j: integer;
begin
  Create;

  if (Offset + Count) > Length(Data) then
    raise Exception.Create(seInvalidInputArgs);

  FDataLength := Count shr 2;
  LeftOver := Count and $3;
  if LeftOver <> 0 then // length not multiples of 4
    Inc(FDataLength);

  if FDataLength = 0 then
    FDataLength := 1;
  SetLength(FData, FDataLength + 1);

  i := Count - 1;
  j := 0;
  while i >= 3 do begin
    FData[j] := cardinal((Data[Offset + i - 3] shl 24) +
                         (Data[Offset + i - 2] shl 16) +
                         (Data[Offset + i - 1] shl  8) +
                          Data[Offset + i]);
    i := i - 4;
    Inc(j);
  end;

  if LeftOver = 1 then
    FData[FDataLength - 1] := cardinal(Data[Offset])
  else if LeftOver = 2 then
    FData[FDataLength - 1] := cardinal((Data[Offset] shl 8) + Data[Offset + 1])
  else if LeftOver = 3 then
    FData[FDataLength - 1] := cardinal((Data[Offset] shl 16) + (Data[Offset + 1] shl 8) + Data[Offset + 2]);

  Truncate;
end;

//***********************************************************************
// Constructor (Default Value provided by an array of unsigned integers)
//*********************************************************************
constructor TBigInteger.Create(const Data: array of cardinal);
var
  i, j: integer;
begin
  Create;

  FDataLength := Length(Data);
  if FDataLength = 0 then
    FDataLength := 1;
  SetLength(FData, FDataLength + 1);

  j := 0;
  for i := Length(Data) - 1 downto 0 do begin
    FData[j] := Data[i];
    Inc(j);
  end;

  Truncate;
end;

constructor TBigInteger.Create(const Value: TBcd);

  procedure GetSubValue(var Index: Integer; Total: Integer; out SubValue, Radix: {$IFNDEF CPU64}Integer{$ELSE}Int64{$ENDIF});
  var
    Last: Integer;
  begin
    if Total - Index <= {$IFNDEF CPU64}3{$ELSE}8{$ENDIF} then
      Last := Total
    else
      Last := Index + {$IFNDEF CPU64}3{$ELSE}8{$ENDIF};

    SubValue := 0;
    Radix := 1;
    while Index <= Last do begin
      if Index = Total then
        if (Value.Precision and 1) = 0 then begin
          Radix := Radix * 100;
          SubValue := SubValue * 100 + ((Value.Fraction[Index] and $F0) shr 4) * 10 + (Value.Fraction[Index] and $0F);
        end
        else begin
          Radix := Radix * 10;
          SubValue := SubValue * 10 + ((Value.Fraction[Index] and $F0) shr 4)
        end
      else begin
        Radix := Radix * 100;
        SubValue := SubValue * 100 + ((Value.Fraction[Index] and $F0) shr 4) * 10 + (Value.Fraction[Index] and $0F);
      end;
      Inc(Index);
    end;
  end;

var
  i: Integer;
  k: integer;
{$IFNDEF CPU64}
  Radix1: Integer;
  Radix2: Integer;
  PosVal1: Integer;
  PosVal2: Integer;
{$ENDIF}
  Radix: Int64;
  PosVal: Int64;
  Tmp: TBigInteger;
  Result: TBigInteger;
begin
  Create;

  Result := nil;
  try
    k := Value.Precision shr 1;
    if (Value.Precision and 1) = 0 then
      Dec(k);

    i := 0;
    repeat
    {$IFNDEF CPU64}
      GetSubValue(i, k, PosVal1, Radix1);

      if i > k then begin
        if Result = nil then
          Result := TBigInteger.Create(PosVal1)
        else begin
          Tmp := Result.Mul(Radix1);
          Result.Free;
          Result := Tmp.Add(PosVal1);
          Tmp.Free;
        end;
        Break;
      end;

      GetSubValue(i, k, PosVal2, Radix2);
      PosVal := Int64(PosVal1) * Radix2 + PosVal2;
      Radix := Int64(Radix1) * Radix2;
    {$ELSE}
      GetSubValue(i, k, PosVal, Radix);
    {$ENDIF}

      if Result = nil then
        Result := TBigInteger.Create(PosVal)
      else begin
        Tmp := Result.Mul(Radix);
        Result.Free;
        Result := Tmp.Add(PosVal);
        Tmp.Free;
      end;
    until i > k;

    FDataLength := Result.FDataLength;
    SetLength(FData, Length(Result.FData));
    if Length(FData) > 0 then
      Buffer.BlockCopy(Result.FData, 0, FData, 0, Length(FData) * SizeOf(cardinal));
  finally
    Result.Free;
  end;
end;

procedure TBigInteger.Assign(Source: TPersistent);
begin
  if IsClass(Source, TBigInteger) then begin
    SetLength(FData, Length(TBigInteger(Source).FData));
    FDataLength := TBigInteger(Source).FDataLength;
    if Length(FData) > 0 then
      Buffer.BlockCopy(TBigInteger(Source).FData, 0, FData, 0, Length(FData) * SizeOf(cardinal));
  end
  else
    inherited Assign(Source);
end;

procedure TBigInteger.SetToZero;
begin
  SetLength(FData, 2);
  FData[0] := 0;
  FData[1] := 0;
  FDataLength := 1;
end;

procedure TBigInteger.Truncate;
begin
  if not IsNegative then
    while (FDataLength > 1) and (FData[FDataLength - 1] = 0) do
      Dec(FDataLength);
end;

//***********************************************************************
// Overloading of addition operator
//***********************************************************************
function TBigInteger.Add(bi: TBigInteger): TBigInteger;
var
  term1, term2: TBigInteger;
  Carry, Sum: UInt64;
  Digit1, Digit2, Val: cardinal;
  i: integer;
begin
  Result := TBigInteger.Create;
  try
    if FDataLength >= bi.FDataLength then begin
      term1 := Self;
      term2 := bi;
    end
    else begin
      term1 := bi;
      term2 := Self;
    end;

    Result.FDataLength := term1.FDataLength;
    SetLength(Result.FData, Result.FDataLength + 2{carry & digit});
    if term1.IsNegative then
      Digit1 := $FFFFFFFF
    else
      Digit1 := 0;
    if term2.IsNegative then
      Digit2 := $FFFFFFFF
    else
      Digit2 := 0;

    Buffer.BlockCopy(term2.FData, 0, Result.FData, 0, term2.FDataLength * SizeOf(cardinal));
    if term1.FDataLength > term2.FDataLength then
      FillChar(Result.FData[term2.FDataLength], (term1.FDataLength - term2.FDataLength) * SizeOf(cardinal), byte(Digit2));
    term2 := Result;

    Carry := UInt64(0);
    for i := 0 to Result.FDataLength - 1 do begin
      Sum := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(term1.FData[i]) + UInt64(term2.FData[i]) + Carry);
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(Sum shr 32);
      Result.FData[i] := cardinal(Sum);
    end;

    Carry := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(Digit1) + UInt64(Digit2) + Carry);
    if Carry <> UInt64(0) then begin // carry and digit
      Val := cardinal(Carry);
      Result.FData[Result.FDataLength] := Val;
      if (Val > 0) and (Val < $FFFFFFFF) then // carry and not digit
        Result.FDataLength := Result.FDataLength + 1;
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(Carry shr 32);
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(Digit1) + UInt64(Digit2) + Carry);
      Result.FData[Length(Result.FData) - 1] := cardinal(Carry);
    end;

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

function TBigInteger.Add(Value: Int64): TBigInteger;
var
  bi: TBigInteger;
begin
  bi := TBigInteger.Create(Value);
  try
    Result := Add(bi);
  finally
    bi.Free;
  end;
end;

//***********************************************************************
// Overloading of subtraction operator
//***********************************************************************
function TBigInteger.Minus(bi: TBigInteger): TBigInteger;
var
  CarryIn, Diff: Int64;
  Digit1, Digit2, Val: cardinal;
  i: integer;
begin
  Result := TBigInteger.Create;
  try
    if FDataLength > bi.FDataLength then
      Result.FDataLength := FDataLength
    else
      Result.FDataLength := bi.FDataLength;

    SetLength(Result.FData, Result.FDataLength + 2{carry & digit});
    if IsNegative then
      Digit1 := $FFFFFFFF
    else
      Digit1 := 0;
    if bi.IsNegative then
      Digit2 := $FFFFFFFF
    else
      Digit2 := 0;

    Buffer.BlockCopy(FData, 0, Result.FData, 0, FDataLength * SizeOf(cardinal));
    if FDataLength < bi.FDataLength then
      FillChar(Result.FData[FDataLength], (bi.FDataLength - FDataLength) * SizeOf(cardinal), byte(Digit1));

    CarryIn := 0;
    for i := 0 to bi.FDataLength - 1 do begin
      Diff := Int64(Result.FData[i]) - Int64(bi.FData[i]) - CarryIn;
      Result.FData[i] := cardinal(Diff);

      if Diff < 0 then
        CarryIn := 1
      else
        CarryIn := 0;
    end;

    for i := bi.FDataLength to Result.FDataLength - 1 do begin
      Diff := Int64(Result.FData[i]) - Int64(Digit2) - CarryIn;
      Result.FData[i] := cardinal(Diff);

      if Diff < 0 then
        CarryIn := 1
      else
        CarryIn := 0;
    end;

    CarryIn := Int64(Digit1) - Int64(Digit2) - CarryIn;
    Val := cardinal(CarryIn);
    Result.FData[Result.FDataLength] := Val;
    if (Val > 0) and (Val < $FFFFFFFF) then // carry and not digit
      Result.FDataLength := Result.FDataLength + 1;

    if CarryIn < 0 then
      CarryIn := 1
    else
      CarryIn := 0;
    CarryIn := Int64(Digit1) - Int64(Digit2) - CarryIn;
    Result.FData[Length(Result.FData) - 1] := cardinal(CarryIn);

    i := Result.FDataLength;
    while (i > 1) and (Result.FData[i] = $FFFFFFFF) and (Result.FData[i - 1] = $FFFFFFFF) and (Result.FData[i - 2] <> 0) do
      Dec(i);
    if (i > 0) and (Result.FData[i] = $FFFFFFFF) and (Result.FData[i - 1] = 0) then
      Inc(i);
    Result.FDataLength := i;

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

function TBigInteger.Minus(Value: Int64): TBigInteger;
var
  bi: TBigInteger;
begin
  bi := TBigInteger.Create(Value);
  try
    Result := Minus(bi);
  finally
    bi.Free;
  end;
end;

//***********************************************************************
// Overloading of the NEGATE operator (2's complement)
//***********************************************************************
function TBigInteger.IsNegative: boolean;
begin
  Result := (Length(FData) > FDataLength) and (FData[FDataLength] = $FFFFFFFF);
end;

function TBigInteger.IsNegativeOrZero: boolean;
begin
  Result := IsNegative or (FDataLength = 0) or ((FDataLength = 1) and (FData[0] = 0));
end;

function TBigInteger.Negate: TBigInteger;
var
  Carry, Val: UInt64;
  i, len: integer;
begin
  // handle neg of zero separately since it'll cause an overflow if we proceed.
  if (FDataLength = 1) and (FData[0] = 0) then begin
    Result := TBigInteger.Create(0);
    Exit;
  end;

  Result := TBigInteger.Create;
  try
    Result.FDataLength := FDataLength;
    SetLength(Result.FData, Result.FDataLength + 1{digit});

    // 1's complement
    if Length(FData) < Length(Result.FData) then
      len := Length(FData)
    else
      len := Length(Result.FData);
    for i := 0 to len - 1 do
      Result.FData[i] := cardinal(not FData[i]);

    for i := len to Length(Result.FData) - 1 do
      Result.FData[i] := cardinal(not 0);

    // add one to Result of 1's complement
    Carry := UInt64(1);
    i := 0;
    while (Carry <> 0) and (i < Result.FDataLength) do begin
      Val := UInt64(Result.FData[i]);
      Inc(Val);

      Result.FData[i] := cardinal(Val);
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(Val shr 32);
      Inc(i);
    end;
    if (Length(Result.FData) > 1) and (Result.FData[Length(Result.FData) - 1] = $FFFFFFFF) and ((Result.FData[Length(Result.FData) - 2] = 0)) then begin
      SetLength(Result.FData, Length(Result.FData) + 1);
      Result.FData[Length(Result.FData) - 1] := $FFFFFFFF;
      Result.FDataLength := Result.FDataLength + 1;
    end;

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

//***********************************************************************
// Overloading of multiplication operator
//***********************************************************************
{$IFNDEF PUREPASCAL}
procedure InternalMul(Val1Length: integer; Val1Start, Val2Start, Val2End, ResultStart, ResultEnd: PCardinal);
label
  l_forbegin, l_for, l_while, l_afterw, l_end;
begin
  asm
    PUSH ESI
    PUSH EDI
    PUSH EBX
    PUSH ECX
    PUSH EDX
    INC EAX
    PUSH EAX

    MOV ESI, Val1Start

  l_forbegin:
    POP EAX
    DEC EAX
    PUSH EAX
    TEST EAX, EAX
    JZ l_end

    CMP dword ptr [ESI], 0
    JZ l_for

    MOV EBX, [Val2Start]
    MOV EDI, [ResultStart]
    XOR ECX, ECX   // Carry

  l_while:
    CMP EBX, [Val2End]
    JNL l_afterw
    CMP EDI, [ResultEnd]
    JNL l_afterw

    MOV EDX, [EBX] // Val2
    MOV EAX, [ESI] // Val1
    MUL EDX
    ADD EAX, [EDI] // Result
    ADC EDX, 0
    ADD EAX, ECX   // Carry
    ADC EDX, 0
    MOV ECX, EDX
    MOV [EDI], EAX

    ADD EDI, 4
    ADD EBX, 4
    JMP l_while

  l_afterw:
    CMP EDI, [ResultEnd]
    JNL l_for
    MOV [EDI], ECX

  l_for:
    ADD dword ptr [ResultStart], 4
    ADD ESI, 4     // Va1
    JMP l_forbegin

  l_end:
    POP EAX
    POP EDX
    POP ECX
    POP EBX
    POP EDI
    POP ESI
  end;
end;
{$ENDIF}

function TBigInteger.ModMul(bi, modulus: TBigInteger): TBigInteger;
var
  Tmp: TBigInteger;
begin
  Tmp := Self.Mul(bi);
  try
    Result := Tmp.BarrettReduction(modulus);
  finally
    Tmp.Free;
  end;
end;

function TBigInteger.Mul(bi: TBigInteger): TBigInteger;

{$IFNDEF PUREPASCAL}
{$IFNDEF FPC}
  function MulUInt(Val1, Val2: cardinal): Int64;
  asm
  {$IFDEF CPU64}
        MOV EAX, Val1
        MUL Val2
        SHL RDX, 32
        ADD RAX, RDX
  {$ELSE}
        MUL Val2
  {$ENDIF}
  end;
{$ENDIF}
{$ENDIF}

var
  nbi1, nbi2, temp: TBigInteger;
  bi1Neg, bi2Neg: boolean;
{$IFNDEF PUREPASCAL}
//  b1, b2: cardinal;
{$ELSE}
  Carry, Val: Int64;
  i, j: integer;
{$ENDIF}
begin
  Result := TBigInteger.Create;
  try
    // take the absolute Value of the inputs
    bi1Neg := Self.IsNegative;
    if bi1Neg then
      nbi1 := Self.Negate
    else
      nbi1 := Self;

    bi2Neg := bi.IsNegative;
    if bi2Neg then
      nbi2 := bi.Negate
    else
      nbi2 := bi;

    try
      Result.FDataLength := nbi1.FDataLength + nbi2.FDataLength;
      SetLength(Result.FData, Result.FDataLength + 1{digit});

    {$IFNDEF PUREPASCAL}
      InternalMul(nbi1.FDataLength, @nbi1.FData[0], @nbi2.FData[0], @nbi2.FData[nbi2.FDataLength], @Result.FData[0], @Result.FData[Result.FDataLength]);
    {$ELSE}
      // multiply the absolute Values
      for i := 0 to nbi1.FDataLength - 1 do begin
        if nbi1.FData[i] = 0 then
          continue;

        Carry := 0;
        for j := 0 to nbi2.FDataLength - 1 do begin
        {$OVERFLOWCHECKS OFF}
          // k = i + j
        {$IFNDEF PUREPASCAL}
        {$IFNDEF FPC}
          b1 := nbi1.FData[i];
          b2 := nbi2.FData[j];
          Val := MulUInt(b1, b2);
        {$ELSE}
          Val := Int64(nbi1.FData[i]) * Int64(nbi2.FData[j]);
        {$ENDIF}
        {$ELSE}
          Val := Int64(nbi1.FData[i]) * Int64(nbi2.FData[j]);
        {$ENDIF}
          Val := Val + Result.FData[i + j] + Carry;
          Result.FData[i + j] := cardinal(Val);
          Carry := Val shr 32;
        end;

        if Carry <> 0 then
          Result.FData[i + nbi2.FDataLength] := cardinal(Carry);
      end;
    {$ENDIF}

      Result.Truncate;

      // if input has different signs, then Result is -ve
      if bi1Neg <> bi2Neg then begin
        temp := Result;
        Result := Result.Negate;
        temp.Free;
      end;

    finally
      if bi1Neg then
        nbi1.Free;
      if bi2Neg then
        nbi2.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TBigInteger.Mul(Value: Int64): TBigInteger;
var
  bi: TBigInteger;
begin
  bi := TBigInteger.Create(Value);
  try
    Result := Mul(bi);
  finally
    bi.Free;
  end;
end;

//***********************************************************************
// Overloading of unary << operators
//***********************************************************************
function TBigInteger.Shl_(ShiftVal: integer): TBigInteger;
begin
  Result := TBigInteger.Create(Self);
  try
    Result.FDataLength := ShiftLeft(Result.FData, Result.FDataLength, ShiftVal);
  except
    Result.Free;
    raise;
  end;
end;

// least significant Bits at lower part of Buffer
class function TBigInteger.ShiftLeft(var Buffer: TLongWordArr; DataLength: integer; ShiftVal: integer): integer;
var
  Carry, Val: UInt64;
  ShiftAmount: integer;
  i, Count: integer;
begin
  ShiftAmount := 32;

  while (DataLength > 1) and (Buffer[DataLength - 1] = 0) do
    Dec(DataLength);

  Count := ShiftVal;
  while Count > 0 do begin
    if Count < ShiftAmount then
      ShiftAmount := Count;

    Carry := UInt64(0);
    for i := 0 to DataLength - 1 do begin
      Val := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(Buffer[i]) shl ShiftAmount);
      Val := {$IFDEF VER7P}UInt64{$ENDIF}(Val or Carry);

      Buffer[i] := cardinal(Val);
      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(Val shr 32);
    end;

    if Carry <> 0 then begin
      if DataLength >= Length(Buffer) then
        SetLength(Buffer, DataLength + 1);

      Buffer[DataLength] := cardinal(Carry);
      Inc(DataLength);
    end;

    Count := Count - ShiftAmount;
  end;

  Result := DataLength;
end;

//***********************************************************************
// Overloading of unary >> operators
//***********************************************************************
function TBigInteger.Shr_(ShiftVal: integer): TBigInteger;
var
  Mask: cardinal;
  i: integer;
begin
  Result := TBigInteger.Create(Self);
  try
    Result.FDataLength := ShiftRight(Result.FData, ShiftVal);

    if IsNegative then begin
      for i := Length(Result.FData) - 1 downto Result.FDataLength do
        Result.FData[i] := $FFFFFFFF;

      Mask := $80000000;
      for i := 0 to 31 do begin
        if cardinal(Result.FData[Result.FDataLength - 1] and Mask) <> 0 then
          break;

        Result.FData[Result.FDataLength - 1] := Result.FData[Result.FDataLength - 1] or Mask;
        Mask := Mask shr 1;
      end;

      Result.FDataLength := Length(Result.FData) + 1;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TBigInteger.ShiftRight(var Buffer: TLongWordArr; ShiftVal: integer): integer;
var
  Carry, Val: UInt64;
  ShiftAmount, BufLen: integer;
  i, Count: integer;
begin
  ShiftAmount := 32;
  BufLen := Length(Buffer);

  while (BufLen > 1) and (Buffer[BufLen - 1] = 0) do
    Dec(BufLen);

  Count := ShiftVal;
  while Count > 0 do begin
    if Count < ShiftAmount then
      ShiftAmount := Count;

    Carry := UInt64(0);
    for i := BufLen - 1 downto 0 do begin
      Val := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(Buffer[i]) shr ShiftAmount);
      Val := {$IFDEF VER7P}UInt64{$ENDIF}(Val or Carry);

      Carry := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(Buffer[i]) shl (32 - ShiftAmount));
      Buffer[i] := cardinal(Val);
    end;

    Count := Count - ShiftAmount;
  end;

  while (BufLen > 1) and (Buffer[BufLen - 1] = 0) do
    Dec(BufLen);

  Result := BufLen;
end;

procedure TBigInteger.Shr_1;
var
  Val, Carry: cardinal;
  i: integer;
begin
  Carry := 0;
  for i := FDataLength - 1 downto 0 do begin
    Val := FData[i];
    FData[i] := (Val shr 1) or Carry;
    if (Val and 1) <> 0 then
      Carry := $80000000
    else
      Carry := 0;
  end;

  Truncate;
end;

//***********************************************************************
// Sets the Value of the specified Bit to 1
// The Least Significant Bit position is 0.
//***********************************************************************
procedure TBigInteger.SetBit(BitNum: integer);
var
  BytePos: integer;
  BitPos: byte;
begin
  BytePos := BitNum shr 5;
  BitPos := byte(BitNum and $1F); // get the lowest 5 Bits

  if BytePos >= (Length(FData) - 1) then
    SetLength(FData, BytePos + 1 + 1{digit});
  if BytePos >= FDataLength then
    FDataLength := integer(BytePos) + 1;

  FData[BytePos] := FData[BytePos] or cardinal(1 shl BitPos);
end;

function TBigInteger.GetBit(BitNum: integer): integer;
var
  BytePos: integer;
  BitPos: byte;
begin
  BytePos := BitNum shr 5;
  BitPos := byte(BitNum and $1F); // get the lowest 5 Bits

  if (BytePos >= FDataLength) or (BytePos >= Length(FData)) then
    Result := 0
  else
    Result := (FData[BytePos] shr BitPos) and $1;
end;

//***********************************************************************
// Overloading of equality operator
//***********************************************************************
function TBigInteger.Equal(bi: TBigInteger): boolean;
var
  i: integer;
begin
  Result := False;
  if bi = nil then
    Exit;

  if IsNegative <> bi.IsNegative then
    Exit;
  if FDataLength <> bi.FDataLength then
    Exit;

  for i := 0 to FDataLength - 1 do begin
    if FData[i] <> bi.FData[i] then
      Exit;
  end;

  Result := True;
end;

function TBigInteger.NotEqual(bi: TBigInteger): boolean;
begin
  Result := not Equal(bi);
end;

//***********************************************************************
// Overloading of inequality operator
//***********************************************************************
function TBigInteger.Greater(bi: TBigInteger): boolean;
var
  Pos: integer;
begin
  // bi1 is negative, bi2 is positive
  if IsNegative and not bi.IsNegative then begin
    Result := False;
    Exit;
  end
  else
  // bi1 is positive, bi2 is negative
  if not IsNegative and bi.IsNegative then begin
    Result := True;
    Exit;
  end;

  // same sign
  if FDataLength > bi.FDataLength then
    Result := not IsNegative
  else
    if FDataLength < bi.FDataLength then
      Result := IsNegative
    else begin
      Pos := FDataLength - 1;
      while (Pos >= 0) and (FData[Pos] = bi.FData[Pos]) do
        Dec(Pos);

      if Pos >= 0 then
        Result := FData[Pos] > bi.FData[Pos]
      else
        Result := False;
    end;
end;

function TBigInteger.Less(bi: TBigInteger): boolean;
var
  Pos: Integer;
begin
  // bi1 is negative, bi2 is positive
  if IsNegative and not bi.IsNegative then begin
    Result := True;
    Exit;
  end
  else
  // bi1 is positive, bi2 is negative
  if not IsNegative and bi.IsNegative then begin
    Result := False;
    Exit;
  end;

  // same sign
  if FDataLength < bi.FDataLength then
    Result := not IsNegative
  else
    if FDataLength > bi.FDataLength then
      Result := IsNegative
    else begin
      Pos := FDataLength - 1;
      while (Pos >= 0) and (FData[Pos] = bi.FData[Pos]) do
        Dec(Pos);

      if Pos >= 0 then
        Result := FData[Pos] < bi.FData[Pos]
      else
        Result := False;
    end;
end;

function TBigInteger.GreaterOrEqual(bi: TBigInteger): boolean;
var
  Pos: integer;
begin
  // bi1 is negative, bi2 is positive
  if IsNegative and not bi.IsNegative then begin
    Result := False;
    Exit;
  end
  else
  // bi1 is positive, bi2 is negative
  if not IsNegative and bi.IsNegative then begin
    Result := True;
    Exit;
  end;

  // same sign
  if FDataLength > bi.FDataLength then
    Result := not IsNegative
  else
    if FDataLength < bi.FDataLength then
      Result := IsNegative
    else begin
      Pos := FDataLength - 1;
      while (Pos >= 0) and (FData[Pos] = bi.FData[Pos]) do
        Dec(Pos);

      if Pos >= 0 then
        Result := FData[Pos] > bi.FData[Pos]
      else
        Result := True;
    end;
end;

function TBigInteger.LessOrEqual(bi: TBigInteger): boolean;
var
  Pos: integer;
begin
  // bi1 is negative, bi2 is positive
  if IsNegative and not bi.IsNegative then begin
    Result := True;
    Exit;
  end
  else
  // bi1 is positive, bi2 is negative
  if not IsNegative and bi.IsNegative then begin
    Result := False;
    Exit;
  end;

  // same sign
  if FDataLength < bi.FDataLength then
    Result := not IsNegative
  else
    if FDataLength > bi.FDataLength then
      Result := IsNegative
    else begin
      Pos := FDataLength - 1;
      while (Pos >= 0) and (FData[Pos] = bi.FData[Pos]) do
        Dec(Pos);

      if Pos >= 0 then
        Result := FData[Pos] < bi.FData[Pos]
      else
        Result := True;
    end;
end;

//***********************************************************************
// Private function that supports the division of two numbers with
// a divisor that has more than 1 digit.
//
// Algorithm taken from [1]
//***********************************************************************
class procedure TBigInteger.MultiByteDivide(bi1, bi2: TBigInteger;
  outQuotient, outRemainder: TBigInteger);
var
  Result, Remainder: TLongWordArr;
  Mask, Val: cardinal;
  Shift, ResultPos: integer;
  FirstDivisorByte, SecondDivisorByte: cardinal;
  q_hat, r_hat: UInt64;
{$IFNDEF PUREPASCAL}
{$IFNDEF CPU64}
  Dividend1, Dividend2: cardinal;
{$ELSE}
  Dividend: UInt64;
{$ENDIF}
{$ELSE}
  Dividend: UInt64;
{$ENDIF}
  t1, t2: {$IFDEF VER7P}UInt64{$ELSE}TBigInteger{$ENDIF};
  nbi2: TBigInteger;
  kk, ss, yy, temp: TBigInteger;
  i, h, Pos: integer;
  x, y: integer;
begin
  Mask := $80000000;
  Val  := bi2.FData[bi2.FDataLength - 1];
  Shift := 0;

  while (Mask <> 0) and (cardinal(Val and Mask) = 0) do begin
    Inc(Shift);
    Mask := Mask shr 1;
  end;

  SetLength(Remainder, bi1.FDataLength + 1);
  if bi1.FDataLength > 0 then
    Move(bi1.FData[0], Remainder[0], bi1.FDataLength * SizeOf(cardinal));

  kk := nil;
  ShiftLeft(Remainder, bi1.FDataLength, Shift);
  nbi2 := bi2.Shl_(Shift);
  try
    i := bi1.FDataLength - nbi2.FDataLength + 1;
    Pos := bi1.FDataLength;
    ResultPos := 0;

    FirstDivisorByte := nbi2.FData[nbi2.FDataLength - 1];
    SecondDivisorByte := nbi2.FData[nbi2.FDataLength - 2];

    SetLength(Result, bi1.FDataLength - nbi2.FDataLength + 1);
    kk := TBigInteger.Create; // Dividend
    kk.FDataLength := nbi2.FDataLength;
    SetLength(kk.FData, kk.FDataLength + 2);

    while i > 0 do begin
    {$IFNDEF PUREPASCAL}
      {$IFNDEF CPU64}
        Dividend2 := Remainder[pos];
        Dividend1 := Remainder[pos - 1];
        asm
          XOR    EDX, EDX
          MOV    EAX, Dividend2
          DIV    FirstDivisorByte
          MOV    dword ptr [q_hat+$04], EAX
          MOV    dword ptr [r_hat+$04], 0
          MOV    EAX, Dividend1
          DIV    FirstDivisorByte
          MOV    dword ptr [q_hat], EAX
          MOV    dword ptr [r_hat], EDX
        end;
      {$ELSE}
        Dividend := UInt64((UInt64(Remainder[pos]) shl 32) or UInt64(Remainder[pos - 1]));
        q_hat := Dividend div FirstDivisorByte;
        r_hat := Dividend mod FirstDivisorByte;
      {$ENDIF}
    {$ELSE}
      Dividend := (UInt64(Remainder[pos]) shl 32) or UInt64(Remainder[pos - 1]);
      q_hat := Dividend div FirstDivisorByte;
      r_hat := Dividend mod FirstDivisorByte;
    {$ENDIF}

      while True do begin
      {$IFDEF VER7P}
        t1 := UInt64(q_hat * SecondDivisorByte);
        t2 := UInt64(UInt64(r_hat shl 32) + Remainder[pos-2]);
      {$ELSE}
        t1 := TBigInteger.Create(Int64(q_hat * SecondDivisorByte), True);
        t2 := TBigInteger.Create(Int64((r_hat shl 32) + Remainder[pos-2]), True);
        try
      {$ENDIF}

        if (q_hat = $100000000) or {$IFDEF VER7P}(t1 > t2){$ELSE}t1.Greater(t2){$ENDIF} then begin
          Dec(q_hat);
          r_hat := {$IFDEF VER7P}UInt64{$ENDIF}(r_hat + FirstDivisorByte);

          if r_hat >= $100000000 then
            Break;
        end
        else
          Break;
      {$IFNDEF VER7P}
        finally
          t1.Free;
          t2.Free;
        end;
      {$ENDIF}
      end;

      Move(Remainder[Pos - nbi2.FDataLength], kk.FData[0], (nbi2.FDataLength + 1) * sizeof(cardinal));
      kk.FDataLength := nbi2.FDataLength + 1;
      kk.Truncate;

      ss := nil;
      yy := nil;
      try
        ss := nbi2.Mul(q_hat);
        while ss.Greater(kk) do begin
          Dec(q_hat);
          temp := ss;
          ss := ss.Minus(nbi2);
          temp.Free;
        end;
        yy := kk.Minus(ss);

        for h := 0 to nbi2.FDataLength do
          if (nbi2.FDataLength - h) < Length(yy.FData) then
            Remainder[Pos - h] := yy.FData[nbi2.FDataLength - h]
          else
            Remainder[Pos - h] := 0;
      finally
        ss.Free;
        yy.Free;
      end;

      Result[ResultPos] := cardinal(q_hat);
      Inc(ResultPos);

      Dec(Pos);
      Dec(i);
    end;
  finally
    nbi2.Free;
    kk.Free;
  end;

  outQuotient.FDataLength := ResultPos;
  if outQuotient.FDataLength = 0 then
    outQuotient.FDataLength := 1;
  SetLength(outQuotient.FData, outQuotient.FDataLength + 1{digit});
  outQuotient.FData[outQuotient.FDataLength] := 0;
  y := 0;
  for x := ResultPos - 1 downto 0 do begin
    outQuotient.FData[y] := Result[x];
    Inc(y);
  end;
  outQuotient.Truncate;

  outRemainder.FDataLength := ShiftRight(Remainder, Shift);
  SetLength(outRemainder.FData, outRemainder.FDataLength + 1{digit});
  outRemainder.FData[outRemainder.FDataLength] := 0;
  Move(Remainder[0], outRemainder.FData[0], outRemainder.FDataLength * sizeof(cardinal));
end;

//***********************************************************************
// Private function that supports the division of two numbers with
// a divisor that has only 1 digit.
//***********************************************************************
class procedure TBigInteger.SingleByteDivide(bi1, bi2: TBigInteger;
  outQuotient, outRemainder: TBigInteger);
var
  Result: TLongWordArr;
  Divisor: cardinal;
  Quotient, Remainder: UInt64;
{$IFNDEF PUREPASCAL}
{$IFNDEF CPU64}
  Dividend, Dividend2: cardinal;
{$ELSE}
  Dividend: UInt64;
{$ENDIF}
{$ELSE}
  Dividend: UInt64;
{$ENDIF}
  ResultPos: integer;
  i, j, Pos: integer;
begin
  // copy Dividend to Remainder
  outRemainder.FDataLength := bi1.FDataLength;
  SetLength(outRemainder.FData, outRemainder.FDataLength + 1{digit});
  outRemainder.FData[outRemainder.FDataLength] := 0;
  if outRemainder.FDataLength > 0 then
    Buffer.BlockCopy(bi1.FData, 0, outRemainder.FData, 0, outRemainder.FDataLength * SizeOf(cardinal));
  outRemainder.Truncate;

  Divisor := bi2.FData[0];
  Pos := outRemainder.FDataLength - 1;
  Dividend := {$IFDEF PUREPASCAL}UInt64{$ENDIF}(outRemainder.FData[Pos]);
  ResultPos := 0;
  SetLength(Result, outRemainder.FDataLength);

  if Dividend >= Divisor then begin
    Quotient := UInt64(Dividend div Divisor);
    Result[ResultPos] := cardinal(Quotient);
    Inc(ResultPos);
    outRemainder.FData[Pos] := cardinal(Dividend mod Divisor);
  end;
  Dec(Pos);

  while Pos >= 0 do begin
  {$IFNDEF PUREPASCAL}
    {$IFNDEF CPU64}
      Dividend2 := outRemainder.FData[Pos + 1];
      Dividend := outRemainder.FData[Pos];
      asm
        XOR    EDX, EDX
        MOV    EAX, Dividend2
        DIV    Divisor
        MOV    dword ptr [Quotient+$04], EAX
        MOV    dword ptr [Remainder+$04], 0
        MOV    EAX, Dividend
        DIV    Divisor
        MOV    dword ptr [Quotient], EAX
        MOV    dword ptr [Remainder], EDX
      end;
    {$ELSE}
      Dividend := UInt64((UInt64(outRemainder.FData[pos + 1]) shl 32) or UInt64(outRemainder.FData[pos]));
      Quotient := Dividend div Divisor;
      Remainder := Dividend mod Divisor;
    {$ENDIF}
  {$ELSE}
    Dividend := (UInt64(outRemainder.FData[pos + 1]) shl 32) or UInt64(outRemainder.FData[pos]);
    Quotient := Dividend div Divisor;
    Remainder := Dividend mod Divisor;
  {$ENDIF}

    Result[ResultPos] := cardinal(Quotient);
    Inc(ResultPos);
    outRemainder.FData[Pos + 1] := 0;
    outRemainder.FData[Pos] := cardinal(Remainder);
    Dec(Pos);
  end;

  outQuotient.FDataLength := ResultPos;
  SetLength(outQuotient.FData, outQuotient.FDataLength + 1{digit});
  outQuotient.FData[outQuotient.FDataLength] := 0;
  j := 0;
  for i := outQuotient.FDataLength - 1 downto 0 do begin
    outQuotient.FData[j] := Result[i];
    Inc(j);
  end;

  outQuotient.Truncate;
  if outQuotient.FDataLength = 0 then
    outQuotient.FDataLength := 1;

  outRemainder.Truncate;
end;

//***********************************************************************
// Overloading of division operator
//***********************************************************************
function TBigInteger.Div_(bi: TBigInteger): TBigInteger;
var
  Remainder: TBigInteger;
  nbi1, nbi2, temp: TBigInteger;
  DivisorNeg, DividendNeg: boolean;
begin
  Remainder := nil;
  DividendNeg := IsNegative;
  if DividendNeg then
    nbi1 := Self.Negate
  else
    nbi1 := Self;

  DivisorNeg := bi.IsNegative;
  if DivisorNeg then
    nbi2 := bi.Negate
  else
    nbi2 := bi;

  try
    if nbi1.Less(nbi2) then begin
      Result := TBigInteger.Create(0);
      Exit;
    end;

    Result := TBigInteger.Create;
    try
      Remainder := TBigInteger.Create;
      if nbi2.FDataLength = 1 then
        SingleByteDivide(nbi1, nbi2, Result, Remainder)
      else
        MultiByteDivide(nbi1, nbi2, Result, Remainder);

      if DividendNeg <> DivisorNeg then begin
        temp := Result;
        Result := Result.Negate;
        temp.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Remainder.Free;
    if DividendNeg then
      nbi1.Free;
    if DivisorNeg then
      nbi2.Free;
  end;
end;

//***********************************************************************
// Overloading of modulus operator
//***********************************************************************
function TBigInteger.Mod_(bi: TBigInteger): TBigInteger;
var
  Quotient: TBigInteger;
  nbi1, nbi2, temp: TBigInteger;
  DividendNeg, DivisorNeg: boolean;
begin
  Quotient := nil;
  temp := nil;
  DividendNeg := IsNegative;
  if DividendNeg then
    nbi1 := Self.Negate
  else
    nbi1 := Self;

  DivisorNeg := bi.IsNegative;
  if DivisorNeg then
    nbi2 := bi.Negate
  else
    nbi2 := bi;

  try
    if nbi1.Less(nbi2) then begin
      if DividendNeg then
        Result := Self.Add(nbi2)
      else
        Result := TBigInteger.Create(Self);
      Exit;
    end;

    Result := TBigInteger.Create;
    try
      Quotient := TBigInteger.Create;
      if nbi2.FDataLength = 1 then
        SingleByteDivide(nbi1, nbi2, Quotient, Result)
      else
        MultiByteDivide(nbi1, nbi2, Quotient, Result);

      if DividendNeg and ((Result.FDataLength > 1) or (Result.FData[0] <> 0)) then begin
        temp := Result.Negate;
        FreeAndNil(Result);
        Result := temp.Add(nbi2);
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    Quotient.Free;
    if DividendNeg then
      nbi1.Free;
    if DivisorNeg then
      nbi2.Free;
    temp.Free;
  end;
end;

function TBigInteger.Mod_(Value: Int64): TBigInteger;
var
  bi: TBigInteger;
begin
  bi := TBigInteger.Create(Value);
  try
    Result := Mod_(bi);
  finally
    bi.Free;
  end;
end;

//***********************************************************************
// Returns the modulo inverse of this.  Throws ArithmeticException if
// the inverse does not exist.  (i.e. gcd(this, modulus) != 1)
//***********************************************************************
function TBigInteger.ModInverse(modulus: TBigInteger): TBigInteger;
var
  p, q, r: array[0..1] of TBigInteger;
  a, b, Val1, Val2, temp: TBigInteger;
  Quotient, Remainder: TBigInteger;
  Step: integer;
begin
  if (FDataLength = 1) and (FData[0] = 1) then begin
    Result := TBigInteger.Create(1);
    Exit;
  end;

  a := nil;
  b := nil;
  Val1 := nil;
  Val2 := nil;
  p[0] := TBigInteger.Create(0);
  p[1] := TBigInteger.Create(1);
  q[0] := TBigInteger.Create; // Quotients
  q[1] := TBigInteger.Create; // Quotients
  r[0] := TBigInteger.Create; // Remainders
  r[1] := TBigInteger.Create; // Remainders
  Quotient := TBigInteger.Create;
  Remainder := TBigInteger.Create;

{$IFNDEF VER25P}
  Result := nil;
{$ENDIF}

  try
    a := TBigInteger.Create(modulus);
    b := TBigInteger.Create(Self);
    Step := 0;

    while (b.FDataLength > 1) or ((b.FDataLength = 1) and (b.FData[0] <> 0)) do begin
      if Step > 1 then begin
        FreeAndNil(Val1);
        Val1 := p[1].Mul(q[0]);
        FreeAndNil(Val2);
        Val2 := p[0].Minus(Val1);
        FreeAndNil(p[0]);
        p[0] := TBigInteger.Create(p[1]);
        FreeAndNil(p[1]);
        p[1] := Val2.Mod_(modulus); //pVal = (p[0] - (p[1] * q[0])) % modulus;
      end;

      if b.FDataLength = 1 then
        SingleByteDivide(a, b, Quotient, Remainder)
      else
        MultiByteDivide(a, b, Quotient, Remainder);

      FreeAndNil(q[0]);
      q[0] := TBigInteger.Create(q[1]);
      FreeAndNil(r[0]);
      r[0] := TBigInteger.Create(r[1]);
      FreeAndNil(q[1]);
      q[1] := TBigInteger.Create(Quotient);
      FreeAndNil(r[1]);
      r[1] := TBigInteger.Create(Remainder);
      FreeAndNil(a);
      a := TBigInteger.Create(b);
      FreeAndNil(b);
      b := TBigInteger.Create(Remainder);

      Inc(Step);
    end;

    if (r[0].FDataLength > 1) or ((r[0].FDataLength = 1) and (r[0].FData[0] <> 1)) then
      raise Exception.Create(seValueHasNoInverse);

    FreeAndNil(Val1);
    Val1 := p[1].Mul(q[0]);
    FreeAndNil(Val2);
    Val2 := p[0].Minus(Val1);
    Result := Val2.Mod_(modulus);

    try
      if Result.IsNegative then begin
        temp := Result;
        Result := Result.Add(modulus); // get the least positive modulus
        temp.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    p[0].Free;
    p[1].Free;
    q[0].Free;
    q[1].Free;
    r[0].Free;
    r[1].Free;
    Val1.Free;
    Val2.Free;
    Quotient.Free;
    Remainder.Free;
    a.Free;
    b.Free;
  end;
end;

//***********************************************************************
// Returns max(this, bi)
//***********************************************************************
function TBigInteger.Max_(bi: TBigInteger): TBigInteger;
begin
  if Self.Greater(bi) then
    Result := TBigInteger.Create(Self)
  else
    Result := TBigInteger.Create(bi);
end;

//***********************************************************************
// Returns min(this, bi)
//***********************************************************************
function TBigInteger.Min_(bi: TBigInteger): TBigInteger;
begin
  if Self.Less(bi) then
    Result := TBigInteger.Create(Self)
  else
    Result := TBigInteger.Create(bi);
end;

//***********************************************************************
// Overloading of Bitwise OR operator
//***********************************************************************
function TBigInteger.Or_(bi: TBigInteger): TBigInteger;
var
  term1, term2: TBigInteger;
  Digit: cardinal;
  i: integer;
begin
  Result := TBigInteger.Create;
  try
    if FDataLength > bi.FDataLength then begin
      term1 := Self;
      term2 := bi;
    end
    else begin
      term1 := bi;
      term2 := Self;
    end;

    Result.FDataLength := term1.FDataLength;
    SetLength(Result.FData, Result.FDataLength + 1{digit});
    if term2.FData[term2.FDataLength - 1] = $FFFFFFFF then
      Digit := $FFFFFFFF
    else
      Digit := 0;

    Buffer.BlockCopy(term2.FData, 0, Result.FData, 0, term2.FDataLength * SizeOf(cardinal));
    if term1.FDataLength > term2.FDataLength then
      FillChar(Result.FData[term2.FDataLength], (term1.FDataLength - term2.FDataLength) * SizeOf(cardinal), byte(Digit));
    term2 := Result;

    for i := 0 to Result.FDataLength - 1 do
      Result.FData[i] := term1.FData[i] or term2.FData[i];

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

//***********************************************************************
// Overloading of Bitwise AND operator
//***********************************************************************
function TBigInteger.And_(bi: TBigInteger): TBigInteger;
var
  term1, term2: TBigInteger;
  Digit: cardinal;
  i: integer;
begin
  Result := TBigInteger.Create;
  try
    if FDataLength > bi.FDataLength then begin
      term1 := Self;
      term2 := bi;
    end
    else begin
      term1 := bi;
      term2 := Self;
    end;

    Result.FDataLength := term1.FDataLength;
    SetLength(Result.FData, Result.FDataLength + 1{digit});
    if term2.FData[term2.FDataLength - 1] = $FFFFFFFF then
      Digit := $FFFFFFFF
    else
      Digit := 0;

    Buffer.BlockCopy(term2.FData, 0, Result.FData, 0, term2.FDataLength * SizeOf(cardinal));
    if term1.FDataLength > term2.FDataLength then
      FillChar(Result.FData[term2.FDataLength], (term1.FDataLength - term2.FDataLength) * SizeOf(cardinal), byte(Digit));
    term2 := Result;

    for i := 0 to Result.FDataLength - 1 do
      Result.FData[i] := term1.FData[i] and term2.FData[i];

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

//***********************************************************************
// Overloading of Bitwise XOR operator
//***********************************************************************
function TBigInteger.Xor_(bi: TBigInteger): TBigInteger;
var
  term1, term2: TBigInteger;
  i: integer;
begin
  Result := TBigInteger.Create;
  try
    if FDataLength > bi.FDataLength then begin
      term1 := Self;
      term2 := bi;
    end
    else begin
      term1 := bi;
      term2 := Self;
    end;

    Result.FDataLength := term1.FDataLength;
    SetLength(Result.FData, Result.FDataLength + 1{digit});

    for i := 0 to term2.FDataLength - 1 do
      Result.FData[i] := term1.FData[i] xor term2.FData[i];

    if term1.FDataLength > term2.FDataLength then
      Move(term1.FData[term2.FDataLength], Result.FData[term2.FDataLength], (term1.FDataLength - term2.FDataLength) * SizeOf(cardinal));
    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TBigInteger.XorSelf(bi: TBigInteger);
var
  term1, term2: TBigInteger;
  i: integer;
begin
  if FDataLength > bi.FDataLength then begin
    term1 := Self;
    term2 := bi;
  end
  else begin
    term1 := bi;
    term2 := Self;
  end;

  for i := 0 to term2.FDataLength - 1 do
    FData[i] := term1.FData[i] xor term2.FData[i];

  if bi.FDataLength > FDataLength then begin
    SetLength(FData, bi.FDataLength + 1{digit});
    Move(bi.FData[FDataLength], FData[FDataLength], (bi.FDataLength - FDataLength) * SizeOf(cardinal));
    FDataLength := bi.FDataLength;
  end;

  Truncate;
end;

//***********************************************************************
// Returns the absolute Value
//***********************************************************************
function TBigInteger.Abs: TBigInteger;
begin
  if IsNegative then
    Result := Self.Negate
  else
    Result := TBigInteger.Create(Self);
end;

//***********************************************************************
// Returns a string representing the BigInteger in base 10.
//***********************************************************************
function TBigInteger.ToString: string;
begin
  Result := ToString(10);
end;

//***********************************************************************
// Returns a string representing the BigInteger in sign-and-magnitude
// format in the specified radix.
//
// Example
// -------
// If the Value of BigInteger is -255 in base 10, then
// ToString(16) returns "-FF"
//***********************************************************************
function TBigInteger.ToString(Radix: integer): string;
var
  a: TBigInteger;
  Quotient, Remainder, biRadix: TBigInteger;
  Charset: string;
  Negative: boolean;
begin
  if (Radix < 2) or (Radix > 36) then
    raise Exception.Create(seInvalidBigIntegerRadix);

  Charset := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Result  := '';

  Quotient := nil;
  Remainder := nil;
  biRadix := nil;

  Negative := IsNegative;
  if Negative then
    a := Self.Negate
  else
    a := TBigInteger.Create(Self);

  try
    if (a.FDataLength = 1) and (a.FData[0] = 0) then
      Result := '0'
    else begin
      Quotient := TBigInteger.Create;
      Remainder := TBigInteger.Create;
      biRadix := TBigInteger.Create(Radix);

      while (a.FDataLength > 1) or ((a.FDataLength = 1) and (a.FData[0] <> 0)) do begin
        SingleByteDivide(a, biRadix, Quotient, Remainder);
        Result := Charset[Remainder.FData[0] + 1] + Result;

        FreeAndNil(a);
        a := TBigInteger.Create(Quotient);
      end;

      if Negative then
        Result := '-' + Result;
    end;
  finally
    Quotient.Free;
    Remainder.Free;
    biRadix.Free;
    a.Free;
  end;
end;

//***********************************************************************
// Modulo Exponentiation
//***********************************************************************
function TBigInteger.ModPow(exp, n: TBigInteger): TBigInteger;
var
  Temp, TempNum: TBigInteger;
  nn: TBigInteger;
  nNegative: boolean;
  Mask: cardinal;
  TotalBits, Count: integer;
  Pos, Index: integer;
begin
  if exp.IsNegative or IsNegative then
    raise Exception.Create(seNotPositiveExponents);

  nNegative := n.IsNegative;
  if nNegative then // negative n
    nn := n.Negate
  else
    nn := n;

  nn.PrepareForBarrettReduction;
  Temp := nil;
  TempNum := nil;
  try
    TotalBits := exp.BitCount;
    Count := 0;

    TempNum := Self.Mod_(nn); // ensures (Self * Self) < b^(2k)

    Result := TBigInteger.Create(1);
    try
      // perform squaring and multiply exponentiation
      for Pos := 0 to exp.FDataLength - 1 do begin
        Mask := $01;

        for Index := 0 to 31 do begin
          if cardinal(exp.FData[Pos] and Mask) <> 0 then begin
            Temp := Result.Mul(TempNum);
            FreeAndNil(Result);

            if Temp.FDataLength < nn.FDataLength then begin
              Result := Temp;
              Temp := nil;
            end
            else begin
              Result := Temp.BarrettReduction(nn);
              FreeAndNil(Temp);
            end;
          end;

          Inc(Count);
          if Count = TotalBits then
            break;

          Mask := Mask shl 1;

          Temp := TempNum.Mul(TempNum);
          FreeAndNil(TempNum);

          if Temp.FDataLength < nn.FDataLength then begin
            TempNum := Temp;
            Temp := nil;
          end
          else begin
            TempNum := Temp.BarrettReduction(nn);
            FreeAndNil(Temp);
          end;

          if (TempNum.FDataLength = 1) and (TempNum.FData[0] = 1) then
            Exit;
        end;
      end;

    except
      Result.Free;
      raise;
    end;
  finally
    if nNegative then
      nn.Free;
    Temp.Free;
    TempNum.Free;
  end;
end;

//***********************************************************************
// Fast calculation of modular reduction using Barrett's reduction.
// Requires x < b^(2k), where b is the base.  In this case, base is
// 2^32 (cardinal).
//
// Reference [4]
//***********************************************************************

procedure TBigInteger.PrepareForBarrettReduction;
var
  Temp: TBigInteger;
begin
  if FIsPrepared then
    Exit;

  FLock.Enter;
  try
    if FIsPrepared then
      Exit;

    // FR1, FR2, FConstant for BarrettReduction
    FR1 := TBigInteger.Create;
    FR1.FDataLength := FDataLength + 1;
    SetLength(FR1.FData, FR1.FDataLength + 1{digit});

    FR2 := TBigInteger.Create;
    FR2.FDataLength := FDataLength + 1;
    SetLength(FR2.FData, FR2.FDataLength + 1{digit});

    // calculate FConstant = b^(2k) / n , where b = 2^32, k = Length(n)
    Temp := TBigInteger.Create;
    try
      Temp.FDataLength := (FDataLength shl 1) + 1; //2k + 1
      SetLength(Temp.FData, Temp.FDataLength + 1{digit});
      Temp.FData[FDataLength shl 1] := $01;
      FConstant := Temp.Div_(Self);
    finally
      Temp.Free;
    end;

    FIsPrepared := True;
  finally
    FLock.Leave;
  end;
end;

procedure TBigInteger.ClearBarrettReduction;
begin
  FLock.Enter;
  try
    FIsPrepared := False;
    FreeAndNil(FR1);
    FreeAndNil(FR2);
    FreeAndNil(FConstant);
  finally
    FLock.Leave;
  end;
end;

function TBigInteger.BarrettReduction(n: TBigInteger): TBigInteger;

{$IFNDEF PUREPASCAL}
{$IFNDEF FPC}
  function MulUInt(Val1, Val2: cardinal): Int64;
  asm
  {$IFDEF CPU64}
        MOV EAX, Val1
        MUL Val2
        SHL RDX, 32
        ADD RAX, RDX
  {$ELSE}
        MUL Val2
  {$ENDIF}
  end;
{$ENDIF}
{$ENDIF}

var
  q1, q2, q3: TBigInteger;
  r: TBigInteger;
  temp: TBigInteger;
  kPlusOne, kMinusOne: integer;
{$IFNDEF PUREPASCAL}
//  b1, b2: cardinal;
{$ELSE}
  Carry, Val: Int64;
  i, j, t: integer;
{$ENDIF}
begin
  if not n.FIsPrepared then
    raise Exception.Create(seInvalidInputArgs);

  n.FLock.Enter;
  try
    q1 := nil;
    q2 := nil;
    q3 := nil;
    r := nil;
    try
      kMinusOne := n.FDataLength - 1;
      kPlusOne := n.FDataLength + 1;

      // q1 = x / b^(k-1)
      q1 := TBigInteger.Create;
      q1.FDataLength := FDataLength - kMinusOne;
      if q1.FDataLength <= 0 then begin
        q1.FDataLength := 1;
        SetLength(q1.FData, 1 + 1{digit});
      end
      else begin
        SetLength(q1.FData, q1.FDataLength + 1{digit});
        Move(FData[kMinusOne], q1.FData[0], q1.FDataLength * sizeof(cardinal));
      end;

      // q2 = q1 * C
      q2 := q1.Mul(n.FConstant);

      // q2 = q2 / b^(k+1)
      q2.FDataLength := q2.FDataLength - kPlusOne;
      if q2.FDataLength <= 0 then begin
        q2.FDataLength := 1;
        q2.FData[0] := 0;
      end
      else
        Move(q2.FData[kPlusOne], q2.FData[0], q2.FDataLength * sizeof(cardinal));
      SetLength(q2.FData, q2.FDataLength + 1{digit});
      q2.FData[q2.FDataLength] := 0;

      // FR1 = x mod b^(k+1)
      // i.e. keep the lowest (k+1) words
      if FDataLength > kPlusOne then
        n.FR1.FDataLength := kPlusOne
      else
        n.FR1.FDataLength := FDataLength;
      Move(FData[0], n.FR1.FData[0], n.FR1.FDataLength * sizeof(cardinal));
      FillChar(n.FR1.FData[n.FR1.FDataLength], (Length(n.FR1.FData) - n.FR1.FDataLength) * sizeof(cardinal), 0);

      // FR2 = (q2 * n) mod b^(k+1)
      // partial multiplication of q2 and n
      n.FR2.FDataLength := kPlusOne;
      FillChar(n.FR2.FData[0], Length(n.FR2.FData) * sizeof(cardinal), 0);

    {$IFNDEF PUREPASCAL}
      InternalMul(q2.FDataLength, @q2.FData[0], @n.FData[0], @n.FData[n.FDataLength], @n.FR2.FData[0], @n.FR2.FData[kPlusOne]);
    {$ELSE}
      for i := 0 to q2.FDataLength - 1 do begin
        if q2.FData[i] = 0 then
          continue;

        Carry := 0;
        t := i;
        j := 0;
        while (j < n.FDataLength) and (t < kPlusOne) do begin
        {$OVERFLOWCHECKS OFF}
          // t = i + j
          //Val := (UInt64(q2.FData[i]) * UInt64(n.FData[j]));
        {$IFNDEF PUREPASCAL}
        {$IFNDEF FPC}
          b1 := q2.FData[i];
          b2 := n.FData[j];
          Val := MulUInt(b1, b2);
        {$ELSE}
          Val := Int64(q2.FData[i]) * Int64(n.FData[j]);
        {$ENDIF}
        {$ELSE}
          Val := Int64(q2.FData[i]) * Int64(n.FData[j]);
        {$ENDIF}
          Val := Val + Int64(n.FR2.FData[t]) + Carry;

          n.FR2.FData[t] := cardinal(Val);
          Carry := Val shr 32;
          Inc(j);
          Inc(t);
        end;

        if t < kPlusOne then
          n.FR2.FData[t] := cardinal(Carry);
      end;
    {$ENDIF}

      while (n.FR2.FDataLength > 1) and (n.FR2.FData[n.FR2.FDataLength - 1] = 0) do
        Dec(n.FR2.FDataLength);

      r := n.FR1.Minus(n.FR2);

      if r.IsNegative then begin // negative
        q3 := TBigInteger.Create;
        q3.FDataLength := kPlusOne + 1;
        SetLength(q3.FData, q3.FDataLength + 1{digit});
        q3.FData[kPlusOne] := $01;
        temp := r;
        r := r.Add(q3);
        temp.Free;
      end;

      if r.GreaterOrEqual(n) then begin
        temp := r;
        r := r.Minus(n);
        temp.Free;
      end;

      Result := r;
      r := nil;
    finally
      q1.Free;
      q2.Free;
      q3.Free;
      r.Free;
    end;
  finally
    n.FLock.Leave;
  end;
end;

//***********************************************************************
// Returns Greatest common divisor - gcd(bi1, bi2)
// Algorithm:
//   gcd(x, 0) = x
//   gcd(x, y) = gcd(y, x mod y)
//***********************************************************************
function TBigInteger.gcd(bi: TBigInteger): TBigInteger;
var
  x, y: TBigInteger;
begin
  if IsNegative then   // negative
    x := Self.Negate
  else
    x := TBigInteger.Create(Self);

  if bi.IsNegative then   // negative
    y := bi.Negate
  else
    y := TBigInteger.Create(bi);

  try
    Result := x;
    x := nil;

    while (y.FDataLength > 1) or ((y.FDataLength = 1) and (y.FData[0] <> 0)) do begin
      x := Result;
      Result := y;
      y := x.Mod_(y);
      FreeAndNil(x);
    end;
  finally
    x.Free;
    y.Free;
  end;
end;

//***********************************************************************
// Populates "this" with the specified amount of Random Bits
//***********************************************************************
procedure TBigInteger.GenRandomBits(Bits: integer; Rand: IScRandom);
var
  Mask: cardinal;
  DWords, RemBits: integer;
begin
  if Rand = nil then
    raise Exception.Create(SInternalError);

  ClearBarrettReduction;

  DWords := Bits shr 5;
  RemBits := Bits and $1F;
  if RemBits <> 0 then
    Inc(DWords);

  FDataLength := DWords;
  if FDataLength = 0 then
    FDataLength := 1;
  SetLength(FData, FDataLength + 1);
  FData[FDataLength] := 0;

  Rand.Random(@FData[0], DWords * sizeof(cardinal));

  if RemBits <> 0 then begin
    Mask := cardinal(1 shl (RemBits - 1));
    FData[DWords - 1] := FData[DWords - 1] or Mask;

    Mask := cardinal($FFFFFFFF shr (32 - RemBits));
    FData[DWords - 1] := FData[DWords - 1] and Mask;
  end
  else
    FData[DWords - 1] := FData[DWords - 1] or $80000000;
end;

//***********************************************************************
// Returns the position of the most significant Bit in the BigInteger.
//
// Eg.  The Result is 0, if the Value of BigInteger is 0...0000 0000
//      The Result is 1, if the Value of BigInteger is 0...0000 0001
//      The Result is 2, if the Value of BigInteger is 0...0000 0010
//      The Result is 2, if the Value of BigInteger is 0...0000 0011
//***********************************************************************
function TBigInteger.BitCount: integer;
var
  Value, Mask: cardinal;
begin
  if FDataLength = 0 then begin
    Result := 0;
    Exit;
  end;

  if IsNegative then
    raise Exception.Create(seNegativeUnderflow);

  // Truncate
  while (FDataLength > 1) and (FData[FDataLength - 1] = 0) do
    Dec(FDataLength);

  if FDataLength = 1 then begin
    if FData[0] = 0 then begin
      Result := 0;
      Exit;
    end;
    if FData[0] = 1 then begin
      Result := 1;
      Exit;
    end;
  end;

  Value := FData[FDataLength - 1];
  Mask := $80000000;
  Result := 32;

  while (Result > 0) and (cardinal(Value and Mask) = 0) do begin
    Dec(Result);
    Mask := Mask shr 1;
  end;
  Result := Result + ((FDataLength - 1) shl 5);
end;

function TBigInteger.IsOdd: boolean;
begin
  Result := (FDataLength > 0) and ((FData[0] and 1) <> 0);
end;
//***********************************************************************
// Returns the lowest 8 bytes of the BigInteger as a long.
//***********************************************************************
function TBigInteger.LongValue: UInt64;
begin
  if FDataLength = 0 then
    Result := UInt64(0)
  else
    if FDataLength = 1 then
      Result := UInt64(FData[0])
    else
      Result := {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(FData[0]) or {$IFDEF VER7P}UInt64{$ENDIF}(UInt64(FData[1]) shl 32));
end;

function TBigInteger.IntValue: cardinal;
begin
  if FDataLength = 0 then
    Result := 0
  else
    Result := FData[0];
end;

//***********************************************************************
// Returns the Value of the BigInteger as a byte array.  The lowest
// Index contains the MSB.
//***********************************************************************
function TBigInteger.GetBytes: TBytes;
begin
  Result := GetBytes(-1);
end;

function TBigInteger.GetBytes(Count: integer): TBytes;
var
  NumBits, NumBytes: integer;
  i, Pos: integer;
begin
  if (Count <= 0) and (Count <> -1) then
    raise Exception.Create(seInvalidInputArgs);

  NumBits := BitCount;

  if NumBits = 0 then begin
    if Count = -1 then begin
      SetLength(Result, 1);
      Result[0] := 0;
    end
    else begin
      SetLength(Result, Count);
      FillChar(Result[0], Count, 0);
    end;
  end
  else begin
    NumBytes := NumBits shr 3;
    if Integer(NumBits and $7) <> 0 then
      Inc(NumBytes);

    if Count = -1 then
      Count := NumBytes;

    if Count < NumBytes then
      raise Exception.Create(seInvalidInputArgs);

    SetLength(Result, Count);
    Pos := Count - NumBytes;
    FillChar(Result[0], Pos, 0);

    case (NumBytes and $3) of
      1: begin
        Result[Pos]     := byte(FData[FDataLength - 1]);
        Inc(Pos);
      end;
      2: begin
        Result[Pos + 1] := byte(FData[FDataLength - 1]);
        Result[Pos]     := byte(FData[FDataLength - 1] shr 8);
        Inc(Pos, 2);
      end;
      3: begin
        Result[Pos + 2] := byte(FData[FDataLength - 1]);
        Result[Pos + 1] := byte(FData[FDataLength - 1] shr 8);
        Result[Pos]     := byte(FData[FDataLength - 1] shr 16);
        Inc(Pos, 3);
      end;
      0: begin
        Result[Pos + 3] := byte(FData[FDataLength - 1]);
        Result[Pos + 2] := byte(FData[FDataLength - 1] shr 8);
        Result[Pos + 1] := byte(FData[FDataLength - 1] shr 16);
        Result[Pos]     := byte(FData[FDataLength - 1] shr 24);
        Inc(Pos, 4);
      end;
    else
      Assert(False);
    end;

    for i := FDataLength - 2 downto 0 do begin
      Result[Pos + 3] := byte(FData[i]);
      Result[Pos + 2] := byte(FData[i] shr 8);
      Result[Pos + 1] := byte(FData[i] shr 16);
      Result[Pos]     := byte(FData[i] shr 24);
      Inc(Pos, 4);
    end;
  end;
end;

function TBigInteger.GetBytesLE: TBytes;
var
  NumBits, NumBytes: integer;
begin
  NumBits := BitCount;

  if NumBits = 0 then begin
    SetLength(Result, 1);
    Result[0] := 0;
  end
  else begin
    NumBytes := NumBits shr 3;
    if Integer(NumBits and $7) <> 0 then
      Inc(NumBytes);

    SetLength(Result, NumBytes);
    Move(FData[0], Result[0], NumBytes);
  end;
end;

function TBigInteger.GetData: TLongWordArr;
begin
  SetLength(Result, FDataLength);
  Move(FData[0], Result[0], FDataLength * sizeof(cardinal));
end;

//***********************************************************************
// Generates a positive BigInteger that is probably prime.
//***********************************************************************
class function TBigInteger.GenPseudoPrime(Bits: integer; Confidence: integer;
  Rand: IScRandom): TBigInteger;
var
  Done: boolean;
begin
  Result := TBigInteger.Create;
  try
    Done := False;

    while not Done do begin
      Result.genRandomBits(Bits, Rand);
      Result.FData[0] := Result.FData[0] or $01;   // make it odd

      // prime test
      Done := Result.IsProbablePrime(Confidence);
    end;
  except
    Result.Free;
    raise;
  end;
end;

//***********************************************************************
// Determines whether a number is probably prime, using the Rabin-Miller's
// test.  Before applying the test, the number is tested for Divisibility
// by primes < 2000
//
// Returns True if number is probably prime.
//***********************************************************************
function TBigInteger.IsProbablePrime(Confidence: integer): boolean;
var
  SelfVal, Divisor: TBigInteger;
  ResultNum: TBigInteger;
  p: integer;
begin
  Result := False;

  if IsNegative then // negative
    SelfVal := Self.Negate
  else
    SelfVal := Self;

  try
    // test for Divisibility by primes < 2000
    for p := 0 to Length(PrimesBelow2000) - 1 do begin
      ResultNum := nil;
      Divisor := TBigInteger.Create(PrimesBelow2000[p]);
      try
        if Divisor.GreaterOrEqual(SelfVal) then
          Break;
        ResultNum := SelfVal.Mod_(Divisor);
        if ResultNum.FData[0] = 0 then
          Exit;
      finally
        Divisor.Free;
        ResultNum.Free;
      end;
    end;

    Result := SelfVal.RabinMillerTest(Confidence);
  finally
    if SelfVal <> Self then
      SelfVal.Free;
  end;
end;

//***********************************************************************
// Probabilistic prime test based on Rabin-Miller's
//
// for any p > 0 with p - 1 = 2^s * t
//
// p is probably prime (strong pseudoprime) if for any a < p,
// 1) a^t mod p = 1 or
// 2) a^((2^j)*t) mod p = p-1 for some 0 <= j <= s-1
//
// Otherwise, p is composite.
//
// Returns
// -------
// True if "this" is a strong pseudoprime to Randomly chosen
// bases.  The number of chosen bases is given by the "Confidence"
// parameter.
//
// False if "this" is definitely NOT prime.
//***********************************************************************
function TBigInteger.RabinMillerTest(Confidence: integer): boolean;
var
  SelfVal: TBigInteger;
  p_sub, t, a, b, gcdTest, temp: TBigInteger;
  Mask: cardinal;
  Done: boolean;
  i, j, s, Index: integer;
  Bits, Round1, TestBits, ByteLen: integer;
begin
  if TestRandom = nil then
    raise Exception.Create(SInternalError);

  if IsNegative then      // negative
    SelfVal := Self.Negate
  else
    SelfVal := Self;

  p_sub := nil;
  t := nil;
  b := nil;
  gcdTest := nil;
  a := TBigInteger.Create;
  try
    if SelfVal.FDataLength = 1 then begin
      // test small numbers
      if (SelfVal.FData[0] = 0) or (SelfVal.FData[0] = 1) then begin
        Result := False;
        Exit;
      end
      else if (SelfVal.FData[0] = 2) or (SelfVal.FData[0] = 3) then begin
        Result := True;
        Exit;
      end;
    end;

    if cardinal(SelfVal.FData[0] and $1) = 0 then begin // even numbers
      Result := False;
      Exit;
    end;

    // calculate Values of s and t
    p_sub := SelfVal.Minus(1);

    s := 0;
    Index := 0;
    while Index < p_sub.FDataLength do begin
      Mask := $01;

      for i := 0 to 31 do begin
        if cardinal(p_sub.FData[Index] and Mask) <> 0 then begin
          Index := p_sub.FDataLength;      // to break the outer loop
          break;
        end;
        Mask := Mask shl 1;
        Inc(s);
      end;
      Inc(Index);
    end;

    t := p_sub.Shr_(s);
    Bits := SelfVal.BitCount;

    for Round1 := 0 to Confidence - 1 do begin
      Done := False;

      while not Done do begin    // generate a < n
        TestBits := 0;

        // make sure "a" has at least 2 Bits
        while TestBits < 2 do begin
          TestRandom.Random(@TestBits, SizeOf(TestBits));
          TestBits := (TestBits and $7FFFFFFF) mod (Bits + 1);
        end;

        a.GenRandomBits(TestBits, TestRandom);
        ByteLen := a.FDataLength;

        // make sure "a" is not 0
        if (ByteLen > 1) or ((ByteLen = 1) and (a.FData[0] <> 1)) then
          Done := True;
      end;

      // check whether a factor exists
      FreeAndNil(gcdTest);
      gcdTest := a.gcd(SelfVal);
      if (gcdTest.FDataLength = 1) and (gcdTest.FData[0] <> 1) then begin
        Result := False;
        Exit;
      end;

      FreeAndNil(b);
      b := a.ModPow(t, SelfVal);
      Result := False;
      if (b.FDataLength = 1) and (b.FData[0] = 1) then   // a^t mod p = 1
        Result := True;

      j := 0;
      while (not Result) and (j < s) do begin
        if b.Equal(p_sub) then begin   // a^((2^j)*t) mod p = p-1 for some 0 <= j <= s-1
          Result := True;
          break;
        end;

        temp := b;
        b := b.Mul(b);
        temp.Free;
        temp := b;
        b := b.Mod_(SelfVal);
        temp.Free;
        Inc(j);
      end;

      if not Result then
        Exit;
    end;

    Result := True;
  finally
    if SelfVal <> Self then
      SelfVal.Free;
    p_sub.Free;
    t.Free;
    a.Free;
    b.Free;
    gcdTest.Free;
  end;
end;

//***********************************************************************
// Put BigInteger into byte array into LE order
//***********************************************************************
class function TBigInteger.PutBigIntegerLE(bi: TBigInteger; var Dest: TBytes; Offset: integer): integer;
var
  Data: TBytes;
  i: integer;
begin
  Data := bi.GetBytes;
  Result := Length(Data);

  if (Result + Offset) > Length(Dest) then
    raise Exception.Create(seInvalidInputArgs);

  for i := 0 to Result - 1 do
    Dest[Offset + i] := Data[Result - 1 - i]; // Reverse
end;

class function TBigInteger.GetBigIntegerLE(const Src: TBytes; Offset: integer; Count: integer): TBytes;
var
  i: integer;
begin
  if (Count + Offset) > Length(Src) then
    raise Exception.Create(seInvalidInputArgs);

  while (Count > 1) and (Src[Offset + Count - 1] = 0) do
    Dec(Count);

  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := Src[Offset + Count - 1 - i]; // Reverse
end;

class function TBigInteger.GetBigIntegerLE(const Src: TBytes): TBytes;
var
  len: integer;
  i: integer;
begin
  len := Length(Src);
  while (len > 1) and (Src[len - 1] = 0) do
    Dec(len);

  SetLength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := Src[len - 1 - i]; // Reverse
end;

{ GF2m }

function TBigInteger.GetSetBitsArray: TIntArr;
begin
  PrepareForGF2mCalc;
  Result := FPolyArrray;
end;

procedure TBigInteger.PrepareForGF2mCalc;
var
  Mask: cardinal;
  Len: integer;
  i, j: integer;
begin
  if FIsPrepared then
    Exit;

  FLock.Enter;
  try
    if FIsPrepared then
      Exit;

    Len := 0;

    for i := FDataLength - 1 downto 0 do begin
      if FData[i] = 0 then
        Continue;

      Mask := $80000000;
      for j := 32 - 1 downto 0 do begin
        if (FData[i] and Mask) <> 0 then begin
          SetLength(FPolyArrray, Len + 1);
          FPolyArrray[Len] := (i shl 5) + j; // i * 32 + j
          Inc(Len);
        end;
        Mask := Mask shr 1;
      end;
    end;

    if (Len <> 3) and (Len <> 5) then begin
      SetLength(FPolyArrray, 0);
      raise Exception.Create(seInvalidInputArgs);
    end;

    FIsPrepared := True;
  finally
    FLock.Leave;
  end;
end;

class procedure TBigInteger.Mul_1x1_GF2m(out r1, r0: cardinal; const a, b: cardinal);
var
  h, l, s: cardinal;
  tab: array[0..7] of cardinal;
  a1, a2, a4: cardinal;
begin
  a1 := a and $3FFFFFFF;
  a2 := a1 shl 1;
  a4 := a2 shl 1;

  tab[0] := 0;
  tab[1] := a1;
  tab[2] := a2;
  tab[3] := a1 xor a2;
  tab[4] := a4;
  tab[5] := a1 xor a4;
  tab[6] := a2 xor a4;
  tab[7] := a1 xor a2 xor a4;

  s := tab[b          and 7]; l := s;
  s := tab[(b shr  3) and 7]; l := l xor (s shl  3); h := s shr 29;
  s := tab[(b shr  6) and 7]; l := l xor (s shl  6); h := h xor (s shr 26);
  s := tab[(b shr  9) and 7]; l := l xor (s shl  9); h := h xor (s shr 23);
  s := tab[(b shr 12) and 7]; l := l xor (s shl 12); h := h xor (s shr 20);
  s := tab[(b shr 15) and 7]; l := l xor (s shl 15); h := h xor (s shr 17);
  s := tab[(b shr 18) and 7]; l := l xor (s shl 18); h := h xor (s shr 14);
  s := tab[(b shr 21) and 7]; l := l xor (s shl 21); h := h xor (s shr 11);
  s := tab[(b shr 24) and 7]; l := l xor (s shl 24); h := h xor (s shr  8);
  s := tab[(b shr 27) and 7]; l := l xor (s shl 27); h := h xor (s shr  5);
  s := tab[b shr 30        ]; l := l xor (s shl 30); h := h xor (s shr  2);

  // compensate for the top two bits of a
  if ((a shr 30) and 1) <> 0 then begin
    l := l xor (b shl 30);
    h := h xor (b shr 2);
  end;

  if ((a shr 30) and 2) <> 0 then begin
    l := l xor (b shl 31);
    h := h xor (b shr 1);
  end;

  r1 := h;
  r0 := l;
end;

class procedure TBigInteger.Mul_2x2_GF2m(var r: array of cardinal; a1, a0, b1, b0: cardinal);
var
  m1, m0: cardinal;
begin
  /// r[3] = h1, r[2] = h0; r[1] = l1; r[0] = l0
  Mul_1x1_GF2m(r[3], r[2], a1, b1);
  Mul_1x1_GF2m(r[1], r[0], a0, b0);
  Mul_1x1_GF2m(m1, m0, a0 xor a1, b0 xor b1);

  // Correction on m1 ^= l1 ^ h1; m0 ^= l0 ^ h0;
  r[2] := r[2] xor m1 xor r[1] xor r[3];  // h0 ^= m1 ^ l1 ^ h1;
  r[1] := r[3] xor r[2] xor r[0] xor m1 xor m0;  // l1 ^= l0 ^ h0 ^ m0;
end;

function TBigInteger.ModSqr_GF2m(modulus: TBigInteger): TBigInteger;
const
  SQR_: array[0..15] of cardinal = (
    0, 1, 4, 5, 16, 17, 20, 21, 64, 65, 68, 69, 80, 81, 84, 85);

  function SQR1(const w: cardinal): cardinal;
  begin
    Result := (SQR_[(w shr 28) and $F] shl 24) or
              (SQR_[(w shr 24) and $F] shl 16) or
              (SQR_[(w shr 20) and $F] shl 8) or
               SQR_[(w shr 16) and $F];
  end;

  function SQR0(const w: cardinal): cardinal;
  begin
    Result := (SQR_[(w shr 12) and $F] shl 24) or
              (SQR_[(w shr  8) and $F] shl 16) or
              (SQR_[(w shr  4) and $F] shl 8) or
               SQR_[w and $F];
  end;

var
  Res: TBigInteger;
  i: integer;
begin
  Res := TBigInteger.Create;
  try
    SetLength(Res.FData, 2 * FDataLength + 1);

    for i := FDataLength - 1 downto 0 do begin
      Res.FData[2 * i + 1] := SQR1(FData[i]);
      Res.FData[2 * i] := SQR0(FData[i]);
    end;

    Res.FDataLength := 2 * FDataLength;
    Res.Truncate;

    Result := Res.Mod_GF2m(modulus);
  finally
    Res.Free;
  end;
end;

function TBigInteger.ModMul_GF2m(bi: TBigInteger; modulus: TBigInteger): TBigInteger;
var
  Res: TBigInteger;
  i, j, k: integer;
  a1, a0, b1, b0: cardinal;
  r: array[0..3] of cardinal;
begin
  if Self = bi then begin
    Result := ModSqr_GF2m(modulus);
    Exit;
  end;

  Res := TBigInteger.Create;
  try
    Res.FDataLength := FDataLength + bi.FDataLength + 4;
    SetLength(Res.FData, Res.FDataLength + 1);

    j := 0;
    while j < bi.FDataLength do begin
      b0 := bi.FData[j];
      if (j + 1) = bi.FDataLength then
        b1 := 0
      else
        b1 := bi.FData[j + 1];

      i := 0;
      while i < FDataLength do begin
        a0 := FData[i];
        if (i + 1) = FDataLength then
          a1 := 0
        else
          a1 := FData[i + 1];

        Mul_2x2_GF2m(r, a1, a0, b1, b0);

        for k := 0 to 3 do
          Res.FData[i + j + k] := Res.FData[i + j + k] xor r[k];

        Inc(i, 2);
      end;

      Inc(j, 2);
    end;

    Res.Truncate;
    Result := Res.Mod_GF2m(modulus);
  finally
    Res.Free;
  end;
end;

function TBigInteger.Mod_GF2m(modulus: TBigInteger): TBigInteger;
var
  n, dN, d0, d1: integer;
  j, k: integer;
  r, tmp: cardinal;
begin
  if (modulus = nil) or (Length(modulus.FPolyArrray) = 0) then
    raise Exception.Create(seInvalidInputArgs);

  if modulus.FPolyArrray[0] = 0 then begin
    // reduction mod 1 => return 0
    Result := TBigInteger.Create(0);
    Exit;
  end;

  Result := TBigInteger.Create(Self);
  try
    // start reduction
    dN := modulus.FPolyArrray[0] shr 5; // div 32
    j := Result.FDataLength - 1;
    while j > dN do begin
      r := Result.FData[j];
      if r = 0 then begin
        Dec(j);
        Continue;
      end;
      Result.FData[j] := 0;

      k := 1;
      while modulus.FPolyArrray[k] <> 0 do begin
        // reducing component t^p[k]
        n := modulus.FPolyArrray[0] - modulus.FPolyArrray[k];
        d0 := n and 31; // mod 32
        d1 := 32 - d0;
        n := n shr 5; // div 32
        Result.FData[j - n] := Result.FData[j - n] xor (r shr d0);
        if d0 <> 0 then
          Result.FData[j - n - 1] := Result.FData[j - n - 1] xor (r shl d1);
        Inc(k);
      end;

      // reducing component t^0
      n := dN;
      d0 := modulus.FPolyArrray[0] and 31; // mod 32;
      d1 := 32 - d0;
      Result.FData[j - n] := Result.FData[j - n] xor (r shr d0);
      if d0 <> 0 then
        Result.FData[j - n - 1] := Result.FData[j - n - 1] xor (r shl d1);
    end;

    // final round of reduction
    while j = dN do begin
      d0 := modulus.FPolyArrray[0] and 31; // mod 32;
      r := Result.FData[dN] shr d0;
      if r = 0 then
        Break;
      d1 := 32 - d0;

      if d0 <> 0 then
        Result.FData[dN] := (Result.FData[dN] shl d1) shr d1; // clear up the top d1 bits
      Result.FData[0] := Result.FData[0] xor r; // reduction t^0 component

      k := 1;
      while modulus.FPolyArrray[k] <> 0 do begin
        // reducing component t^p[k]
        n := modulus.FPolyArrray[k] shr 5; // div 32;
        d0 := modulus.FPolyArrray[k] and 31; // mod 32;
        d1 := 32 - d0;
        Result.FData[n] := Result.FData[n] xor (r shl d0);
        tmp := r shr d1;
        if (d0 <> 0) and (tmp <> 0) then
          Result.FData[n + 1] := Result.FData[n + 1] xor tmp;

        Inc(k);
      end;
    end;

    Result.Truncate;
  except
    Result.Free;
    raise;
  end;
end;
(*
function TBigInteger.ModInv_GF2m(modulus: TBigInteger): TBigInteger;
var
  b, c, u, v, tmp: TBigInteger;
begin
{$IFNDEF VER25P}
  Result := nil;
{$ENDIF}
  u := Self.Mod_GF2m(modulus);
  v := TBigInteger.Create(modulus);
  b := TBigInteger.Create(1);
  c := TBigInteger.Create(0);

  try
    if u.BitCount = 0 then
      raise Exception.Create(seInvalidInputArgs);

    while True do begin
      while not u.IsOdd do begin
        u.Shr_1;
        if b.IsOdd then
          b.XorSelf(modulus);
        b.Shr_1;
      end;

      if (u.FDataLength = 1) and (u.FData[0] = 1) then
        Break;

      if u.BitCount < v.BitCount then begin
        tmp := u; u := v; v := tmp;
        tmp := b; b := c; c := tmp;
      end;

      u.XorSelf(v);
      b.XorSelf(c);
    end;

    Result := b;
    b := nil;
    Result.Truncate;
  finally
    u.Free;
    v.Free;
    b.Free;
    c.Free;
  end;
end;
*)
function TBigInteger.ModInv_GF2m(modulus: TBigInteger): TBigInteger;

  function NBitCount(n: cardinal): integer;
  const
    bits: array [0..255] of byte = (
      0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,
      5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
      8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
    );
  begin
    if (n and $ffff0000) > 0 then begin
      if (n and $ff000000) > 0 then
        Result := bits[n shr 24] + 24
      else
        Result := bits[n shr 16] + 16;
    end
    else begin
      if (n and $ff00) > 0 then
        Result := bits[n shr 8] + 8
      else
        Result := bits[n];
    end;
  end;

var
  b, c, u, v, tmp: TBigInteger;
  uData, bData: TCardinalArray;
  i, uBits, vBits, TopDataLen, uTop: integer;
  u0, u1, b0, b1, uTmp: cardinal;
begin
{$IFNDEF VER25P}
  Result := nil;
{$ENDIF}
  u := Self.Mod_GF2m(modulus);
  v := TBigInteger.Create(modulus);
  b := TBigInteger.Create;
  c := TBigInteger.Create;

  try
    uBits := u.BitCount;
    vBits := v.BitCount;
    TopDataLen := modulus.FDataLength;
    SetLength(u.FData, TopDataLen + 1);
    u.FDataLength := TopDataLen;
    uData := @u.FData[0];

    SetLength(b.FData, TopDataLen + 1);
    b.FDataLength := TopDataLen;
    b.FData[0] := 1;
    bData := @b.FData[0];

    SetLength(c.FData, TopDataLen + 1);
    c.FDataLength := TopDataLen;

    while True do begin
      while (uBits > 0) and ((uData[0] and 1) = 0) do begin
        u0 := uData[0];
        b0 := bData[0];

        if (b0 and 1) = 1 then begin
          b0 := b0 xor modulus.FData[0];
          for i := 0 to TopDataLen - 2 do begin
            u1 := uData[i + 1];
            uData[i] := (u0 shr 1) or (u1 shl 31);
            u0 := u1;
            b1 := bData[i + 1] xor modulus.FData[i + 1];
            bData[i] := (b0 shr 1) or (b1 shl 31);
            b0 := b1;
          end;
        end
        else begin
          for i := 0 to TopDataLen - 2 do begin
            u1 := uData[i + 1];
            uData[i] := (u0 shr 1) or (u1 shl 31);
            u0 := u1;
            b1 := bData[i + 1];
            bData[i] := (b0 shr 1) or (b1 shl 31);
            b0 := b1;
          end;
        end;

        uData[TopDataLen - 1] := u0 shr 1;
        bData[TopDataLen - 1] := b0 shr 1;

        Dec(uBits);
      end;

      if uBits <= 32 then begin
        if uData[0] = 0 then // poly was reducible
          raise Exception.Create(seInvalidInputArgs);
        if uData[0] = 1 then
          Break;
      end;

      if uBits < vBits then begin
        i := uBits; uBits := vBits; vBits := i;
        tmp := u; u := v; v := tmp;
        tmp := b; b := c; c := tmp;
        uData := @u.FData[0];
        bData := @b.FData[0];
      end;

      for i := 0 to TopDataLen - 1 do begin
        uData[i] := uData[i] xor v.FData[i];
        bData[i] := bData[i] xor c.FData[i];
      end;

      if uBits = vBits then begin
        uTop := (uBits - 1) div 32;
        uTmp := uData[uTop];
        while (uTmp = 0) and (uTop <> 0) do begin
          Dec(uTop);
          uTmp := uData[uTop];
        end;
        uBits := uTop * 32 + NBitCount(uTmp);
      end;
    end;

    Result := b;
    b := nil;
    Result.Truncate;
  finally
    u.Free;
    v.Free;
    b.Free;
    c.Free;
  end;
end;

function TBigInteger.ModDiv_GF2m(bi: TBigInteger; modulus: TBigInteger): TBigInteger;
var
  biInv: TBigInteger;
//  a, b, u, v: TBigInteger;
begin
  biInv := bi.ModInv_GF2m(modulus);
  try
    Result := Self.ModMul_GF2m(biInv, modulus);
  finally
    biInv.Free;
  end;
(*
  // Algorithm Modular_Division_GF(2^m) from Chang-Shantz, S.
  // reduce x and y mod p
  u := Self.Mod_GF2m(modulus);
  a := bi.Mod_GF2m(modulus);
  b := TBigInteger.Create(modulus);
  v := TBigInteger.Create(0);

  try
    while not a.IsOdd do begin
      a.Shr_1;
      if u.IsOdd then
        u.XorSelf(modulus);
      u.Shr_1;
    end;

    repeat
      if b.Greater(a) then begin
        b.XorSelf(a);
        v.XorSelf(u);

        repeat
          b.Shr_1;
          if v.IsOdd then
            v.XorSelf(modulus);
          v.Shr_1;
        until b.IsOdd;
      end
      else
      if (a.FDataLength = 1) and (a.FData[0] = 1) then
        Break
      else begin
        a.XorSelf(b);
        u.XorSelf(v);

        repeat
          a.Shr_1;
          if u.IsOdd then
            u.XorSelf(modulus);
          u.Shr_1;
        until a.IsOdd;
      end;
    until False;

    Result := u;
    u := nil;
    Result.Truncate;
  finally
    u.Free;
    a.Free;
    b.Free;
    v.Free;
  end;
*)
end;

initialization
  TestRandom := TScRandomLFSR.Create;

finalization
  TestRandom := nil;

end.
