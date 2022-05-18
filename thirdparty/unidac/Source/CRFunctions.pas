
//////////////////////////////////////////////////
//  Devart Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRFunctions
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRFunctions;

{$IFDEF DARWIN}
  {$linkframework CoreFoundation}
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, VarUtils, SyncObjs,
  FMTBcd,
  CRTypes;

type
  CRBitConverter = class
  public
    class function Int32BitsToSingle(Value: Integer): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function SingleToInt32Bits(Value: Single): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function BytesToExtended(const Value: TBytes): Extended; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ExtendedToBytes(Value: Extended): TBytes; {$IFDEF USE_INLINE}inline;{$ENDIF}
  end;


//{$IFDEF ANDROID32}
//  function Int(const X: Double): Double;
//  function Trunc(const X: Double): Int64;
//{$ENDIF}
  function Exponent10(Exponent: Cardinal): Double;

  function GetBcdPrecision(const Bcd: TBcd): Integer;
  function GetBcdScale(const Bcd: TBcd): Integer;
  function IsBcdInt(const Bcd: TBcd): Boolean;
  function IsBcdZero(const Bcd: TBcd): Boolean;
  function NormalizeBcd(const InBCD: TBcd; var OutBcd: TBcd; Precision, Places: Integer): Boolean;

{$IFNDEF VER17P}
  function CurrencyToBcd(const Curr: Currency): TBcd;
  function BcdToCurrency(const BCD: TBcd): Currency;
{$ENDIF}

  function TryStrToGUID(const S: string; out Value: TGUID): Boolean;
  function ConvertGuidToString(const Guid: TGUID; const WithBraces: Boolean): string;

{ Delphi 6 support }

{$IFNDEF VER7P}
  procedure SafeArrayCheck(AResult: HRESULT);
  procedure SafeArrayError(AResult: HRESULT);

  function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
  function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
  function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
{$ENDIF}


{$IFNDEF VER10P}
  function AnsiDequotedStr(const S: string; AQuote: Char): string;
  function IndexStr(const AText: string; const AValues: array of string): Integer;
{$ENDIF}

  procedure AssignStrings(Source: TStrings; Dest: TStrings); overload;

{$IFNDEF FPC}
{$IFNDEF NEXTGEN}
// These functions was changed in the Delphi XE and they are copied to this unit for compatibility
  function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload;
  function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
  function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload;
  function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;
  function Utf8Encode(const WS: WideString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
  function Utf8Decode(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): WideString;
  function AnsiToUtf8(const S: AnsiString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
  function Utf8ToAnsi(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): AnsiString;
{$ELSE}
  function Utf8Encode(const S: string): AnsiString;
  function Utf8Decode(const S: AnsiString): string;
  function AnsiToUtf8(const S: AnsiString): AnsiString;
  function Utf8ToAnsi(const S: AnsiString): AnsiString;
{$ENDIF}
{$ELSE}
  function DetectAnsiEncoding: TEncoding;
  function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
  function Utf8Encode(const WS: WideString): RawByteString;
  function Utf8Decode(const S: RawByteString): WideString;
  function AnsiToUtf8(const S: AnsiString): RawByteString;
  function Utf8ToAnsi(const S: RawByteString): AnsiString;
{$ENDIF}
  // UnicodeToUtf8 without zero terminator
  function UnicodeToUtf8WoT(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
  // Utf8ToUnicode without zero terminator
  function Utf8ToUnicodeWoT(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
  function DetectUtf8LastChar(Buf: IntPtr; Size: Integer): Integer;
  function DetectUtf8LastBrockenChar(Buf: IntPtr; Size: Integer): Integer;
  procedure ConvertBigEndianBuffer(Buf: IntPtr; Count: Integer); overload;
  procedure ConvertBigEndianBuffer(SrcBuf, DestBuf: IntPtr; Count: Integer); overload;

{$IFNDEF VER12P}
{$IFDEF FPC}
  function NextCharIndex(const S: string; Index: Integer): Integer;
  function StringOfChar(Ch: Ansichar; Count: Integer): AnsiString; overload;
  function StringOfChar(Ch: Widechar; Count: Integer): WideString; overload;
{$ENDIF}
  function CharInSet(Ch: Char; const CharSet: TSysCharSet): Boolean;
{$ELSE}
  function StrToCardinal(const S: string): Cardinal;
  function StrToUInt64(const S: string): UInt64;
  function TryStrToCardinal(const S: string; out Value: Cardinal): Boolean;
  function TryStrToUInt64(const S: string; out Value: UInt64): Boolean;
{$ENDIF}

{$IFNDEF VER15P}
  function SplitString(const S, Delimiters: string): TStringArray;
{$IFNDEF FPC}
  function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
    UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
    LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer;
  function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
    LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer;
{$ENDIF}
{$ENDIF}

{ POSIX support }

{$IFDEF POSIX}
  function GetTickCount: Cardinal;

{$IFNDEF PUREPASCAL}
  function InterlockedIncrement(var I: Integer): Integer;
  function InterlockedDecrement(var I: Integer): Integer;
{$ELSE}
  function InterlockedIncrement(var I: Integer): Integer; inline;
  function InterlockedDecrement(var I: Integer): Integer; inline;
{$ENDIF}

{$IFNDEF NEXTGEN}
  function WideCharToMultiByte(CodePage: Cardinal; dwFlags: Cardinal;
    lpWideCharStr: PWideChar; cchWideChar: Integer; lpMultiByteStr: PAnsiChar;
    cchMultiByte: Integer; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PLongBool): Integer;
  function MultiByteToWideChar(CodePage: Cardinal; dwFlags: Cardinal;
    const lpMultiByteStr: PAnsiChar; cchMultiByte: Integer;
    lpWideCharStr: PWideChar; cchWideChar: Integer): Integer;
{$ENDIF}
{$ENDIF}

{ FPC support}

{$IFDEF FPC}
{$IFDEF UNIX}
  function GetTickCount: Cardinal;
{$ENDIF}
{$IFDEF CPU64}
  function VarArrayCreate(const Bounds: array of Integer; aVarType: TVarType): Variant;
{$ENDIF}
{$ENDIF}

  function PtrOffset(Value: IntPtr; Offset: Integer): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF VER12P}
  function PtrOffset(Value: IntPtr; Offset: Cardinal): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
{$IFNDEF FPC}
  function PtrOffset(Value: IntPtr; Offset: Int64): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
  function PtrSubstract(Value1: IntPtr; Value2: IntPtr): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function PtrCompare(Value1: IntPtr; Value2: IntPtr): Integer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function PtrCompare(Value1: IntPtr; Offset1: Integer; Value2: IntPtr; Offset2: Integer): Integer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function PtrCompare(Value1: IntPtr; Offset1: Int64; Value2: IntPtr; Offset2: Int64): Integer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

  function CreateEvent(InitialState: boolean = False): TEvent;
  function IsMainThread: boolean;
  function IsThread(ThreadID: TThreadID): boolean;
{$IFDEF VER12P}
  procedure SynchronizeWithMainThread(Proc: TThreadProcedure); overload;
{$ELSE}
  procedure SynchronizeWithMainThread(Proc: TProcedure); overload;
{$ENDIF}
  procedure SynchronizeWithMainThread(Method: TThreadMethod); overload;
  function CompareMethods(Method1, Method2: TMethod): boolean;

  function GetIsClass(Obj: TObject; AClass: TClass): boolean;
  function GetIsClassByName(Obj: TObject; AClass: TClass): boolean; overload;
  function GetIsClassByName(Obj: TObject; const AClassName: string): boolean; overload;

  function LengthA(const AStr: AnsiString): integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure SetLengthA(var AStr: AnsiString; NewLength: integer); {$IFDEF USE_INLINE}inline;{$ENDIF}

  function SwapCardinal(const Value: Cardinal): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal; overload;
{$IFDEF FPC}
  function GetTickInterval(StartTickCount, FinishTickCount: UInt64): UInt64; overload;
{$ENDIF}
  function GetLocalTimeZoneOffset: Integer;

  function IsNumeric(const Str: string): boolean;
  function StrToDay(const Day: string): byte;
  function StrToMonth(const Month: string): integer;
  function InternetStrToDateTime(const Value: string): TDateTime;
  function GMTToLocalDateTime(const Value: string): TDateTime;
  function LocalDateTimeToGMT(const Value: TDateTime; IncludeGMT: boolean = False): string;
  function DateTimeToHttpStr(const Value: TDateTime): string;

  function ExtractFirstWord(var Str: string; const Separator: string): string;
  function GetFirstWord(const Str: string; const Separator: string): string;
  function ByteInSet(AByte: byte; const ASet: TBytes): boolean;

{$IFDEF MSWINDOWS}
  function DetectLibraryMachineType(const LibraryName: String): TMachineType;
{$ENDIF}

  function BobJenkinsHash(const Data; Len, InitData: Integer): Integer;
  function BobJenkinsHashStr(const Str: string; InitData: Integer): Integer;
  function BobJenkinsHashAStr(const AStr: AnsiString; InitData: Integer): Integer;
  function BobJenkinsHashWStr(const WStr: WideString; InitData: Integer): Integer;

type
  TIsClassFunction = function (Obj: TObject; AClass: TClass): boolean;
var
  IsClass: TIsClassFunction;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  System.Diagnostics,
  Posix.SysTime,
  Posix.Time,
  Posix.UTime,
{$ENDIF}
{$IFDEF DARWIN}
  CFBase, CFLocale, CFString, MacTypes,
{$ENDIF}
{$IFDEF UNIX}
  unix,
  baseunix,
  unixutil,
{$ENDIF}
  StrUtils,
  SysConst;

{$IFDEF FPC}
var
  InternalAnsiEncoding: TEncoding;
{$ENDIF}

const
  WeekDayNames: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');

  MonthNames: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

resourcestring
  SDateEncodeError = 'Invalid date format';
{$IFDEF MSWINDOWS}
  SCannotCreateEvent = 'Cannot create event:'#13#10'%s.'#13#10'Error Code: %d';
{$ENDIF}

function CreateEvent(InitialState: boolean = False): TEvent;
{$IFDEF MSWINDOWS}
var
  LastErrorCode: cardinal;
  Guid: TGUID;
{$ENDIF}
begin
  Result := TEvent.Create(nil, True, InitialState, '');
{$IFDEF MSWINDOWS}
  if NativeUInt(Result.Handle) = 0 then begin
    LastErrorCode := GetLastError;
    if (LastErrorCode <> 0) and (CreateGUID(Guid) = 0) then begin
      FreeAndNil(Result);
      Result := TEvent.Create(nil, True, InitialState, GUIDToString(Guid));
      LastErrorCode := GetLastError;
    end;

    if LastErrorCode <> 0 then
      raise Exception.CreateFmt(SCannotCreateEvent, [SysErrorMessage(LastErrorCode), LastErrorCode]);
  end;
{$ENDIF}
end;



//{$IFDEF ANDROID32}
//
//function Int(const X: Double): Double;
//var
//  hhword: Word;
//  exp2: Integer;
//  power2: Integer;
//  buf: UInt64;
//begin
//  hhword := PWordArray(@X)[3];
//  exp2 := (hhword shr 4) and $7FF;
//  power2 := 52 - (exp2 - $3FF);
//  if (exp2 > 0) and (power2 > 0) then begin
//    buf := PUint64(@X)^ and ($FFFFFFFFFFFFFFFF shl power2);
//    Result := PDouble(@buf)^;
//  end
//  else
//    Result := X;
//end;
//
//function Trunc(const X: Double): Int64;
//begin
//  Result := System.Trunc(Int(X));
//end;
//
//{$ENDIF}

function Exponent10(Exponent: Cardinal): Double;
const
  max_exppower10 = 18;

  power10: array[0..max_exppower10] of Double = (
    1,
    10, 100, 1000,
    10000, 100000, 1000000,
    10000000, 100000000, 1000000000,
    10000000000, 100000000000, 1000000000000,
    10000000000000, 100000000000000, 1000000000000000,
    10000000000000000, 100000000000000000, 1000000000000000000
  );
begin
  if Exponent <= max_exppower10 then
    Result := power10[Exponent]
  else begin
    Result := power10[max_exppower10];
    Exponent := Exponent - max_exppower10;
    while Exponent > max_exppower10 do begin
      Result := Result * power10[max_exppower10];
      Exponent := Exponent - max_exppower10;
    end;
    Result := Result * power10[Exponent];
  end;
end;

function GetBcdPrecision(const Bcd: TBcd): Integer;
var
  b: Byte;
  I, J: Integer;
begin
  I := 0;
  J := Bcd.Precision - 1;

  while I < J do begin
    b := Bcd.Fraction[I shr 1];
    if b <> 0 then begin
      if b shr 4 = 0 then
        Inc(I);
      break;
    end;
    I := I + 2;
  end;

  if (Bcd.Precision and 1) <> 0 then begin
    b := Bcd.Fraction[J shr 1];
    if b shr 4 <> 0 then begin
      Result := J - I + 1;
      exit;
    end;
    Dec(J)
  end;

  while I < J do begin
    b := Bcd.Fraction[J shr 1];
    if b <> 0 then begin
      if b and $0F = 0 then
        Dec(J);
      break;
    end;
    J := J - 2;
  end;

  Result := J - I + 1;
end;

function GetBcdScale(const Bcd: TBcd): Integer;
var
  b: Byte;
  I, J: Integer;
begin
  Result := 0;

  I := Bcd.Precision - (Bcd.SignSpecialPlaces and $7F);
  if I < 0 then
    Exit;
  J := Bcd.Precision - 1;

  if (Bcd.Precision and 1) <> 0 then begin
    b := Bcd.Fraction[J shr 1];
    if b shr 4 <> 0 then begin
      Result := J - I + 1;
      exit;
    end;
    Inc(I);
  end;

  while I < J do begin
    b := Bcd.Fraction[J shr 1];
    if b <> 0 then begin
      if b and $0F <> 0 then
        Result := J - I  + 1
      else
        Result := J - I;
      exit;
    end;
    J := J - 2;
  end;

  if (J and 1) = 0 then begin
    b := Bcd.Fraction[J shr 1];
    if b shr 4 <> 0 then
      Result := 1;
  end;
end;

function IsBcdInt(const Bcd: TBcd): Boolean;
var
  b: Byte;
  I, J: Integer;
begin
  Result := True;

  I := Bcd.Precision - (Bcd.SignSpecialPlaces and $7F);
  if (I and 1) <> 0 then begin
    I := I shr 1;
    b := Bcd.Fraction[I];
    Result := b and $0F = 0;
    Inc(I);
  end
  else
    I := I shr 1;

  J := Bcd.Precision shr 1;
  while Result and (I < J) do begin
    b := Bcd.Fraction[I];
    if b <> 0 then
      Result := False;
    Inc(I);
  end;

  if Result and ((Bcd.Precision and 1) <> 0) then begin
    b := Bcd.Fraction[I];
    if b shr 4 <> 0 then
      Result := False;
  end;
end;

function IsBcdZero(const Bcd: TBcd): Boolean;
var
  i: Integer;
begin
  for i := Low(Bcd.Fraction) to High(Bcd.Fraction) do
    if Bcd.Fraction[i] <> 0 then begin
      Result := False;
      Exit;
    end;

  Result := True;
end;

function NormalizeBcd(const InBcd: TBcd; var OutBcd: TBcd; Precision, Places: Integer): Boolean;
var
  Distance: Integer;
  I: Integer;
  B: Byte;
begin
  if Precision > MaxFMTBcdFractionSize then begin
    Result := False;
    Exit;
  end;

  OutBcd := InBcd;
  Result := True;
  if not ((OutBcd.Precision = Precision) and ((OutBcd.SignSpecialPlaces and 63) = Places)) then begin
    // Verify reducing Places will only truncate 0s
    for I := 0 to (InBcd.SignSpecialPlaces and 63) - Places - 1 do begin
      if ((InBcd.Precision - I) and 1) = 0 then
        B := InBcd.Fraction[((InBcd.Precision - I + 1) shr 1) - 1] and $F
      else
        B := InBcd.Fraction[((InBcd.Precision - I + 1) shr 1) - 1] div $10;
      if B <> 0 then begin
        Result := False;
        break;
      end;
    end;
    Distance := (InBcd.Precision - (InBcd.SignSpecialPlaces and 63)) - (Precision - Places);
    if Distance < 0 then begin // Move left to right
      for I := Precision - 1 downto 0 do begin
        if I + Distance < 0 then
          B := 0
        else if ((I + Distance) and 1) = 0 then
          B := InBcd.Fraction[(I + Distance) shr 1] div $10
        else
          B := InBcd.Fraction[(I + Distance) shr 1] and $F;
        if (I and 1) = 0 then
          OutBcd.Fraction[I shr 1] := (OutBcd.Fraction[I shr 1] and $F) or (B * $10)
        else
          OutBcd.Fraction[I shr 1] := B;
      end;
    end
    else
    if Distance > 0 then begin // move right to left
      for I := 0 to Distance - 1 do begin // Verify we're only ditching leading 0s
        if (I and 1) = 0 then
          B := InBcd.Fraction[I shr 1] div $10
        else
          B := InBcd.Fraction[I shr 1] and $F;
        if B <> 0 then begin
          Result := False;
          Break;
        end;
      end;

      for I := 0 to Precision - 1 do begin
        if I + Distance > InBcd.Precision then
          B := 0
        else if ((I + Distance) and 1) = 0 then
          B := InBcd.Fraction[(I + Distance) shr 1] div $10
        else
          B := InBcd.Fraction[(I + Distance) shr 1] and $F;
        if (I and 1) = 0 then
          OutBcd.Fraction[I shr 1] := B * $10
        else
          OutBcd.Fraction[I shr 1] := B or OutBcd.Fraction[I shr 1];
      end;
    end;
  end;

  if (Precision and 1) <> 0 then
    OutBcd.Fraction[Precision shr 1] := OutBcd.Fraction[Precision shr 1] and $F0;
  if Precision < 63 then
    FillChar(OutBcd.Fraction[(Precision + 1) shr 1], Length(OutBcd.Fraction) - ((Precision + 1) shr 1), 0);

  OutBcd.Precision := Precision;
  OutBcd.SignSpecialPlaces := (InBcd.SignSpecialPlaces and $80) + (Places and 63);
end;

{$IFNDEF VER17P}
function CurrencyToBcd(const Curr: Currency): TBcd;
begin
  CurrToBCD(Curr, Result);
end;

function BcdToCurrency(const BCD: TBcd): Currency;
begin
  BCDToCurr(BCD, Result);
end;
{$ENDIF}


function TryStrToGUID(const S: string; out Value: TGUID): Boolean;
begin
  Result := True;
  try
    Value := StringToGUID(S);
  except
    Result := False;
  end;
end;

function ConvertGuidToString(const Guid: TGUID; const WithBraces: Boolean): string;
begin
  if WithBraces then
    Result := GUIDToString(Guid)
  else begin
    SetLength(Result, 36);
    StrLFmt(PChar(Result), 36,'%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x',
      [Guid.D1, Guid.D2, Guid.D3, Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
       Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]);
  end;
end;

{ Delphi 6 support }

{$IFNDEF VER7P}

procedure SafeArrayCheck(AResult: HRESULT);
begin
  if AResult and $80000000 <> 0 then
    SafeArrayError(AResult);
end;

procedure SafeArrayError(AResult: HRESULT);
begin
  case AResult of
    VAR_BADINDEX:      raise ESafeArrayBoundsError.CreateHResult(AResult, SVarArrayBounds);
    VAR_ARRAYISLOCKED: raise ESafeArrayLockedError.CreateHResult(AResult, SVarArrayLocked);
  else
    raise ESafeArrayError.CreateHResult(AResult);
  end;
end;

function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
begin
  Result := Copy(AnsiString(AText), 1, ACount);
end;

function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload;
begin
  Result := Copy(AnsiString(AText), Length(AnsiString(AText)) + 1 - ACount, ACount);
end;

function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

{$ENDIF}

{$IFNDEF VER10P}
function AnsiDequotedStr(const S: Ansistring; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if ((Result = '') or (LText^ = #0)) and
     (Length(S) > 0) and ((S[1] <> AQuote) or (S[Length(S)] <> AQuote)) then
    Result := S;
end;

function IndexStr(const AText: string; const AValues: array of string): Integer;
begin
  Result := AnsiIndexStr(AText, AValues);
end;
{$ENDIF}

procedure AssignStrings(Source: TStrings; Dest: TStrings);
begin
  Dest.Assign(Source);
end;

{ UTF8Encoding }

{$IFNDEF FPC}
{$IFNDEF NEXTGEN}

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
begin
  Result := CRFunctions.UnicodeToUtf8(Dest, MaxBytes, Source, Cardinal(-1));
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;
    
  if Dest <> nil then begin
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), Dest, Integer(MaxDestBytes), nil, nil));
    if (Result > 0) and (Result <= MaxDestBytes) then begin
      if Result = MaxDestBytes then begin
        while (Result > 1) and (Byte(Dest[Result - 1]) > $7F) and (Byte(Dest[Result - 1]) and $80 <> 0) and (Byte(Dest[Result - 1]) and $C0 <> $C0) do
          Dec(Result);
        Dest[Result - 1] := #0;
      end
      else begin
        Dest[Result] := #0;
        Inc(Result);
      end;
    end;
  end
  else
    Result := Cardinal(WideCharToMultiByte(CP_UTF8, 0, Source, Integer(SourceChars), nil, 0, nil, nil));
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
begin
  Result := CRFunctions.Utf8ToUnicode(Dest, MaxChars, Source, Cardinal(-1));
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then
    Exit;

  if (Dest <> nil) and (MaxDestChars > 0) then begin
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), Dest, Integer(MaxDestChars)));
    if (Result > 0) and (Result <= MaxDestChars) then begin
      if Result = MaxDestChars then begin
        if (Result > 1) and (Word(Dest[Result - 1]) >= $DC00) and (Word(Dest[Result - 1]) <= $DFFF) then
          Dec(Result);
        Dest[Result - 1] := #0;
      end
      else begin
        Dest[Result] := #0;
        Inc(Result);
      end;
    end;
  end
  else
    Result := Cardinal(MultibyteToWideChar(CP_UTF8, 0, Source, Integer(SourceBytes), nil, 0));
end;

function Utf8Encode(const WS: WideString):{$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
var
  Len1, Len2: Integer;
begin
  Len1 := Length(WS);
  if Len1 > 0 then begin
    Len2 := Len1 shl 2;
    SetLength(Result, Len2); // SetLength includes space for null terminator
    Len2 := CRFunctions.UnicodeToUtf8(PAnsiChar(Result), Len2 + 1, PWideChar(WS), Len1);
    Dec(Len2); // zero terminator
    SetLength(Result, Len2);
  end
  else
    Result := '';
end;

function Utf8Decode(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): WideString;
var
  Len1, Len2: Integer;
begin
  Len1 := Length(S);
  if Len1 > 0 then begin
    SetLength(Result, Len1); // SetLength includes space for null terminator
    Len2 := CRFunctions.Utf8ToUnicode(PWideChar(Result), Len1 + 1, PAnsiChar(S), Len1);
    Dec(Len2); // zero terminator
    if Len2 <> Len1 then
      SetLength(Result, Len2);
  end
  else
    Result := '';
end;

function AnsiToUtf8(const S: AnsiString): {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF};
begin
  Result := Utf8Encode(WideString(S));
end;

function Utf8ToAnsi(const S: {$IFDEF VER12P}RawByteString{$ELSE}UTF8String{$ENDIF}): AnsiString;
begin
  Result := AnsiString(Utf8Decode(S));
end;

{$ELSE !NEXTGEN}
function Utf8Encode(const S: string): AnsiString;
begin
  Result := AnsiString(MarshaledAString(System.Utf8Encode(S)));
end;

function Utf8Decode(const S: AnsiString): string;
begin
  Result := System.Utf8Decode(MarshaledAString(S));
end;

function AnsiToUtf8(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(MarshaledAString(System.AnsiToUtf8(string(S))));
end;

function Utf8ToAnsi(const S: AnsiString): AnsiString;
begin
  Result := AnsiString(System.Utf8ToAnsi(MarshaledAString(S)));
end;

{$ENDIF NEXTGEN}

{$ELSE FPC}

function DetectAnsiEncoding: TEncoding;

{$IFNDEF MSWINDOWS}
  type
    TCodePageMapEntry = record
      LocaleName: string;
      CodePage: Cardinal;
    end;

  const
    CodePageMapA: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'ar'; CodePage: 1256),
      (LocaleName: 'az-cyrl'; CodePage: 1251),
      (LocaleName: 'az-latn'; CodePage: 1254));

    CodePageMapBC: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'be'; CodePage: 1251),
      (LocaleName: 'bg'; CodePage: 1251),
      (LocaleName: 'cs'; CodePage: 1250));

    CodePageMapEF: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'el'; CodePage: 1253),
      (LocaleName: 'et'; CodePage: 1257),
      (LocaleName: 'fa'; CodePage: 1256));

    CodePageMapH: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'he'; CodePage: 1255),
      (LocaleName: 'hr'; CodePage: 1250),
      (LocaleName: 'hu'; CodePage: 1250));

    CodePageMapJK: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'ja'; CodePage: 932),
      (LocaleName: 'kk'; CodePage: 1251),
      (LocaleName: 'ko'; CodePage: 949));

    CodePageMapLM: array[0..2] of TCodePageMapEntry = (
      (LocaleName: 'lt'; CodePage: 1257),
      (LocaleName: 'lv'; CodePage: 1257),
      (LocaleName: 'mk'; CodePage: 1251));

    CodePageMapP: array[0..1] of TCodePageMapEntry = (
      (LocaleName: 'pa-arab'; CodePage: 1256),
      (LocaleName: 'pl'; CodePage: 1250));

    CodePageMapR: array[0..1] of TCodePageMapEntry = (
      (LocaleName: 'ro'; CodePage: 1250),
      (LocaleName: 'ru'; CodePage: 1251));

    CodePageMapS: array[0..4] of TCodePageMapEntry = (
      (LocaleName: 'sk'; CodePage: 1250),
      (LocaleName: 'sl'; CodePage: 1250),
      (LocaleName: 'sq'; CodePage: 1250),
      (LocaleName: 'sr-cyrl'; CodePage: 1251),
      (LocaleName: 'sr-latn'; CodePage: 1250));

    CodePageMapT: array[0..1] of TCodePageMapEntry = (
      (LocaleName: 'th'; CodePage: 874),
      (LocaleName: 'tr'; CodePage: 1254));

    CodePageMapUV: array[0..5] of TCodePageMapEntry = (
      (LocaleName: 'uk'; CodePage: 1251),
      (LocaleName: 'ur'; CodePage: 1256),
      (LocaleName: 'uz-arab'; CodePage: 1256),
      (LocaleName: 'uz-cyrl'; CodePage: 1251),
      (LocaleName: 'uz-latn'; CodePage: 1254),
      (LocaleName: 'vi'; CodePage: 1258));

    CodePageMapZH: array[0..6] of TCodePageMapEntry = (
      (LocaleName: 'zh_cn'; CodePage: 936),
      (LocaleName: 'zh_hk'; CodePage: 950),
      (LocaleName: 'zh-hans_hk'; CodePage: 936),
      (LocaleName: 'zh_mo'; CodePage: 950),
      (LocaleName: 'zh-hans_mo'; CodePage: 936),
      (LocaleName: 'zh_sg'; CodePage: 936),
      (LocaleName: 'zh_tw'; CodePage: 950));

  function GetPosixLocaleName: string;
  {$IFDEF MACOS}
  var
    Locale: CFLocaleRef;
    strRef: CFStringRef;
    tmpStr: Str255;
  begin
    Locale := CFLocaleCopyCurrent;
    try
      strRef := CFLocaleGetIdentifier(Locale);
      CFStringGetPascalString(strRef, @tmpStr, 255, kCFStringEncodingUTF8);
      Result := tmpStr;
    finally
      CFRelease(Locale);
    end;
  end;
  {$ELSE}
  const
    defaultLocale = 'en_US';
  var
    I, Len: Integer;
    Lang: PChar;
    Temp: Pointer;
    LHasDot: Boolean;
  begin
    LHasDot := False;
    Lang := FpGetEnv(PChar('LANG'));
    if Lang = nil then
      Lang := defaultLocale;

    Len := 0;
    while (Lang[Len] <> #0) and (Lang[Len] <> '.') do
    begin
      Inc(Len);
      if Lang[Len] = '.' then
        LHasDot := True;
    end;

    SetLength(Result, Len);
    Temp := PChar(Result);
    for I := 0 to Len - 1 do
      PChar(Temp)[I] := Char(Ord(Lang[I]));
    if LHasDot then
      Result := Result + '.utf8';
  end;
  {$ENDIF}

  function FindCodePage(const Name: string; const Map: array of TCodePageMapEntry;
      var CodePage: Cardinal): Boolean;
    var
      I: Integer;
    begin
      for I := Low(Map) to High(Map) do
        if Map[I].LocaleName = Name then begin
          CodePage := Map[I].CodePage;
          Exit(True);
        end;
      Result := False;
    end;

  function GetACP: Cardinal;
  var
    I: Integer;
    LName: string;
    LCodePage: Cardinal;
  begin
    LName := GetPosixLocaleName;
    I := Low(string);
    while I <= High(LName) do
    begin
      if LName[I] in ['A'..'Z'] then
        Inc(LName[I], Ord('a') - Ord('A'))
      else if LName[I] = '_' then begin
        SetLength(LName, I - Low(string));
        Break;
      end;
      Inc(I);
    end;

    Result := 1252; // Default codepage
    if Length(LName) > 0 then
      case LName[Low(string)] of
        'a':
          if FindCodePage(LName, CodePageMapA, LCodePage) then
            Result := LCodePage;
        'b','c':
          if FindCodePage(LName, CodePageMapBC, LCodePage) then
            Result := LCodePage;
        'e','f':
          if FindCodePage(LName, CodePageMapEF, LCodePage) then
            Result := LCodePage;
        'h':
          if FindCodePage(LName, CodePageMapH, LCodePage) then
            Result := LCodePage;
        'j','k':
          if FindCodePage(LName, CodePageMapJK, LCodePage) then
            Result := LCodePage;
        'l','m':
          if FindCodePage(LName, CodePageMapLM, LCodePage) then
            Result := LCodePage;
        'p':
          if FindCodePage(LName, CodePageMapP, LCodePage) then
            Result := LCodePage;
        'r':
          if FindCodePage(LName, CodePageMapR, LCodePage) then
            Result := LCodePage;
        's':
          if FindCodePage(LName, CodePageMapS, LCodePage) then
            Result := LCodePage;
        't':
          if FindCodePage(LName, CodePageMapT, LCodePage) then
            Result := LCodePage;
        'u','v':
          if FindCodePage(LName, CodePageMapUV, LCodePage) then
            Result := LCodePage;
        'z':
          begin
            LName := GetPosixLocaleName;
            I := Low(string);
            while I <= High(LName) do
            begin
              if LName[I] in ['A'..'Z'] then
                Inc(LName[I], Ord('a') - Ord('A'))
              else if LName[I] = '@' then begin
                // Non Gregorian calendars include "@calendar=<calendar>" on MACOS
                SetLength(LName, I - Low(string));
                Break;
              end;
              Inc(I);
            end;
            if FindCodePage(LName, CodePageMapZH, LCodePage) then
              Result := LCodePage
            else if (Length(LName) >= 2) and (LName[Low(string) + 1] = 'h') then
              // Fallback for Chinese in countries other than cn, hk, mo, tw, sg
              Result := 936;
          end;
      end;
  end;
{$ENDIF}

begin
  if InternalAnsiEncoding = nil then
    InternalAnsiEncoding := TEncoding.GetEncoding(GetACP);
  Result := InternalAnsiEncoding;
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := System.UnicodeToUtf8(Dest, MaxDestBytes, Source, SourceChars);
end;

function Utf8Encode(const WS: WideString): RawByteString;
begin
  Result := System.Utf8Encode(WS);
end;

function Utf8Decode(const S: RawByteString): WideString;
begin
  Result := System.Utf8Decode(S);
end;

function AnsiToUtf8(const S: AnsiString): RawByteString;
var
  Len: Integer;
  arr: TBytes;
begin
  Len := Length(S);
  if Len > 0 then begin
    SetLength(arr, Len);
    Move(S[1], arr[0], Len);
    Result := System.Utf8Encode(DetectAnsiEncoding.GetString(arr));
  end
  else
    Result := '';
end;

function Utf8ToAnsi(const S: RawByteString): AnsiString;
var
  Len: Integer;
  arr: TBytes;
begin
  if Length(S) > 0 then begin
    arr := DetectAnsiEncoding.GetBytes(System.UTF8Decode(S));
    Len := Length(arr);
    SetLength(Result, Len);
    Move(arr[0], Result[1], Len);
    SetCodePage(RawByteString(Result), DetectAnsiEncoding.CodePage, False);
  end
  else
    Result := '';
end;

{$ENDIF FPC}

function UnicodeToUtf8WoT(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
{$IFDEF FPC}
var
  Utf8Str: TBytes;
  Len: Cardinal;
  SrcLen: Cardinal;
{$ENDIF}
begin
{$IFNDEF FPC}
  Result := LocaleCharsFromUnicode(CP_UTF8, 0, Source, SourceChars, Dest, MaxDestBytes, nil, nil);
{$ELSE}
  if Dest = nil then
    Result := System.UnicodeToUtf8(Dest, MaxDestBytes, Source, SourceChars) - 1
  else if SourceChars = 0 then
    Result := 0
  else begin
    SrcLen := SourceChars - 1;
    if SrcLen > 0 then
      Result := System.UnicodeToUtf8(Dest, MaxDestBytes, Source, SrcLen) - 1
    else
      Result := 0;
    // process last char
    SetLength(Utf8Str, 7);
    Len := System.UnicodeToUtf8(IntPtr(Utf8Str), 6, PtrOffset(Source, SrcLen * 2), 1) - 1;
    if MaxDestBytes - Result >= Len then begin
      case Len of
        1:
         PByte(PtrOffset(Dest, Result))^ := Utf8Str[0];
        2:
          PWord(PtrOffset(Dest, Result))^ := PWord(IntPtr(Utf8Str))^;
        4:
          PCardinal(PtrOffset(Dest, Result))^ := PCardinal(IntPtr(Utf8Str))^;
        else
          Move(Utf8Str[0], PtrOffset(Dest, Result)^, Len);
      end;
      Result := Result + Len;
    end
    else begin
      PByte(PtrOffset(Dest, Result))^ := Byte('?');
      Inc(Result);
    end;
  end;
{$ENDIF}
end;

function Utf8ToUnicodeWoT(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
{$IFDEF FPC}
var
  WStr: TBytes;
  Len: Cardinal;
  SrcLen: Integer;
{$ENDIF}
begin
{$IFNDEF FPC}
  Result := UnicodeFromLocaleChars(CP_UTF8, 0, Source, SourceBytes, Dest, MaxDestChars);
{$ELSE}
  if Dest = nil then
    Result := System.Utf8ToUnicode(Dest, MaxDestChars, Source, SourceBytes) - 1
  else if SourceBytes = 0 then
    Result := 0
  else begin
    SrcLen := DetectUtf8LastChar(Source, SourceBytes);
    if SrcLen > 0 then
      Result := System.Utf8ToUnicode(Dest, MaxDestChars, Source, SrcLen) - 1
    else
      Result := 0;
    // process last char
    SetLength(WStr, 4);
    Len := System.Utf8ToUnicode(IntPtr(WStr), 1, PtrOffset(Source, SrcLen), SourceBytes - SrcLen) - 1;
    if Len > 0 then begin
      PWord(PtrOffset(Dest, Result * 2))^ := PWord(IntPtr(WStr))^;
      Inc(Result);
    end;
  end;
{$ENDIF}
end;

function DetectUtf8LastChar(Buf: IntPtr; Size: Integer): Integer;
var
  Ptr: PByte;
  PtrStart: IntPtr;
begin
  Result := Size - 1;

  if Size = 0 then
    Exit;

  Ptr := PtrOffset(Buf, Size);
  if Size > 6 then
    PtrStart := PtrOffset(Ptr, -6)
  else
    PtrStart := Buf;

  repeat
    Dec(Ptr);
    if (Ptr^ and $80 = 0) or (Ptr^ and $C0 = $C0) then begin
      Result := NativeUInt(Ptr) - NativeUInt(Buf);
      Exit;
    end;
  until NativeUInt(Ptr) <= NativeUInt(PtrStart);
end;

// Return broken bytes count in the buffer tail
function DetectUtf8LastBrockenChar(Buf: IntPtr; Size: Integer): Integer;
var
  j: Integer;
  Ptr: PByte;
  PtrStart: IntPtr;
  PtrEnd: IntPtr;
begin
  Result := 0;

  if Size = 0 then
    Exit;

  Ptr := PtrOffset(Buf, Size);
  if Size > 6 then
    PtrStart := PtrOffset(Ptr, -6)
  else
    PtrStart := Buf;
  PtrEnd := Ptr;

  repeat
    Dec(Ptr);

    if Ptr^ and $80 = 0 then
      Exit
    else if Ptr^ and $40 <> 0 then begin
      j := 2;
      if Ptr^ and $20 <> 0 then begin
        j := 3;
        if Ptr^ and $10 <> 0 then begin
          j := 4;
          if Ptr^ and $08 <> 0 then begin
            j := 5;
            if Ptr^ and $04 <> 0 then
              j := 6;
          end;
        end;
      end;

      Result := NativeUInt(PtrEnd) - NativeUInt(Ptr);
      if Result >= j then
        Result := 0;
      Exit;
    end;
  until NativeUInt(Ptr) <= NativeUInt(PtrStart);
end;

procedure ConvertBigEndianBuffer(Buf: IntPtr; Count: Integer);
var
  pEnd: IntPtr;
begin
{$IFDEF CPU64}
  pEnd := PtrOffset(Buf, Count and $FFFFFFF8); // align to SizeOf(UInt64)
  while NativeUInt(Buf) < NativeUInt(pEnd) do begin
    PUInt64(Buf)^ := ((PUInt64(Buf)^ shl 8) and $FF00FF00FF00FF00) or
                     ((PUInt64(Buf)^ shr 8) and $00FF00FF00FF00FF);
    Inc(PUInt64(Buf));
  end;
  if Count and $04 <> 0 then begin
    PCardinal(Buf)^ := ((PCardinal(Buf)^ shl 8) and $FF00FF00) or
                       ((PCardinal(Buf)^ shr 8) and $00FF00FF);
    Inc(PCardinal(Buf));
  end;
  if Count and $02 <> 0 then
    PWord(Buf)^ := Word((PWord(Buf)^ shl 8) or (PWord(Buf)^ shr 8));
{$ELSE}
  pEnd := PtrOffset(Buf, Count and $FFFFFFFC); // align to SizeOf(Cardinal)
  while NativeUInt(Buf) < NativeUInt(pEnd) do begin
    PCardinal(Buf)^ := ((PCardinal(Buf)^ shl 8) and $FF00FF00) or
                          ((PCardinal(Buf)^ shr 8) and $00FF00FF);
    Inc(PCardinal(Buf));
  end;
  if Count and $02 <> 0 then
    PWord(Buf)^ := Word((PWord(Buf)^ shl 8) or (PWord(Buf)^ shr 8));
{$ENDIF}
end;

procedure ConvertBigEndianBuffer(SrcBuf, DestBuf: IntPtr; Count: Integer);
var
  pEnd: IntPtr;
begin
{$IFDEF CPU64}
  pEnd := PtrOffset(SrcBuf, Count and $FFFFFFF8); // align to SizeOf(UInt64)
  while NativeUInt(SrcBuf) < NativeUInt(pEnd) do begin
    PUInt64(DestBuf)^ := ((PUInt64(SrcBuf)^ shl 8) and $FF00FF00FF00FF00) or
                         ((PUInt64(SrcBuf)^ shr 8) and $00FF00FF00FF00FF);
    Inc(PUInt64(SrcBuf));
    Inc(PUInt64(DestBuf));
  end;
  if Count and $04 <> 0 then begin
    PCardinal(DestBuf)^ := ((PCardinal(SrcBuf)^ shl 8) and $FF00FF00) or
                           ((PCardinal(SrcBuf)^ shr 8) and $00FF00FF);
    Inc(PCardinal(SrcBuf));
    Inc(PCardinal(DestBuf));
  end;
  if Count and $02 <> 0 then
    PWord(DestBuf)^ := Word((PWord(SrcBuf)^ shl 8) or (PWord(SrcBuf)^ shr 8));
{$ELSE}
  pEnd := PtrOffset(SrcBuf, Count and $FFFFFFFC); // align to SizeOf(Cardinal)
  if SrcBuf = DestBuf then begin
    while NativeUInt(SrcBuf) < NativeUInt(pEnd) do begin
      PCardinal(SrcBuf)^ := ((PCardinal(SrcBuf)^ shl 8) and $FF00FF00) or
                            ((PCardinal(SrcBuf)^ shr 8) and $00FF00FF);
      Inc(PCardinal(SrcBuf));
    end;
    if Count and $02 <> 0 then
      PWord(SrcBuf)^ := Word((PWord(SrcBuf)^ shl 8) or (PWord(SrcBuf)^ shr 8));
  end
  else begin
    while NativeUInt(SrcBuf) < NativeUInt(pEnd) do begin
      PCardinal(DestBuf)^ := ((PCardinal(SrcBuf)^ shl 8) and $FF00FF00) or
                             ((PCardinal(SrcBuf)^ shr 8) and $00FF00FF);
      Inc(PCardinal(SrcBuf));
      Inc(PCardinal(DestBuf));
    end;
    if Count and $02 <> 0 then
      PWord(DestBuf)^ := Word((PWord(SrcBuf)^ shl 8) or (PWord(SrcBuf)^ shr 8));
  end;
{$ENDIF}
end;

{$IFDEF FPC}

function NextCharIndex(const S: string; Index: Integer): Integer;
begin
  Result := Index + 1;
  assert((Index > 0) and (Index <= Length(S)));
  if SysLocale.FarEast and (S[Index] in LeadBytes) then
    Result := Index + StrCharLength(PChar(S) + Index - 1);
end;

function StringOfChar(Ch: Ansichar; Count: Integer): AnsiString;
begin
  SetLength(Result, Count);
  FillChar(Pointer(Result)^, Length(Result), Ch);
end;

function StringOfChar(Ch: Widechar; Count: Integer): WideString;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 1 to Count do
    Result[i] := Ch;
end;

{$ENDIF}

{$IFNDEF VER12P}

function CharInSet(Ch: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := Ch in CharSet;
end;

{$ELSE}

function StrToCardinal(const S: string): Cardinal;
const
  SInvalidInteger = '''''%s'''' is not a valid integer value';
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then
    raise EConvertError.CreateFmt(SInvalidInteger, [S]);
end;

function StrToUInt64(const S: string): UInt64;
const
  SInvalidInteger = '''''%s'''' is not a valid integer value';
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then
    raise EConvertError.CreateFmt(SInvalidInteger, [S]);
end;

function TryStrToCardinal(const S: string; out Value: Cardinal): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function TryStrToUInt64(const S: string; out Value: UInt64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

{$IFNDEF VER15P}
function SplitString(const S, Delimiters: string): TStringArray;
var
  i, Len: Integer;
  Positions: array of Integer;
  SplitPoints: Integer;
begin
  Result := nil;

  if S <> '' then begin
    { Determine the length of the resulting array }
    SplitPoints := 0;
    Len := Length(S);
    SetLength(Positions, Len);
    for i := 1 to Len do
      if IsDelimiter(Delimiters, S, i) then begin
        Positions[SplitPoints] := i;
        Inc(SplitPoints);
      end;
    Positions[SplitPoints] := Len + 1;

    SetLength(Result, SplitPoints + 1);

    Result[0] := Copy(S, 1, Positions[0] - 1);
    for i := 1 to SplitPoints do
      Result[i] := Copy(S, Positions[i - 1] + 1, Positions[i] - Positions[i - 1] - 1);
  end;
end;

{$IFNDEF FPC}
function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer;
begin
  Result := WideCharToMultiByte(CodePage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr,
    LocaleStrLen, DefaultChar, PBOOL(UsedDefaultChar));
end;

function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer;
begin
  Result := MultiByteToWideChar(CodePage, Flags, LocaleStr, LocaleStrLen,
    UnicodeStr, UnicodeStrLen);
end;
{$ENDIF}
{$ENDIF}

{ POSIX support }

{$IFDEF POSIX}

function GetTickCount: Cardinal;
begin
  Result := Cardinal(TStopwatch.GetTimeStamp div 10000); //prevent range check error in iOS
end;

{$IFNDEF PUREPASCAL}

function InterlockedIncrement(var I: Integer): Integer;
asm
{$IFNDEF CPU64}
      MOV   EDX,1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      INC   EAX
{$ELSE}
      MOV   EAX,1
 LOCK XADD  dword ptr [RCX],EAX
      INC   EAX
{$ENDIF}
end;

function InterlockedDecrement(var I: Integer): Integer;
asm
{$IFNDEF CPU64}
      MOV   EDX,-1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      DEC   EAX
{$ELSE}
      MOV   EAX,-1
 LOCK XADD  dword ptr [RCX],EAX
      DEC   EAX
{$ENDIF}
end;

{$ELSE}

function InterlockedIncrement(var I: Integer): Integer;
begin
  Result := AtomicIncrement(I);
end;

function InterlockedDecrement(var I: Integer): Integer;
begin
  Result := AtomicDecrement(I);
end;

{$ENDIF}

{$IFNDEF NEXTGEN}
function WideCharToMultiByte(CodePage: Cardinal; dwFlags: Cardinal;
  lpWideCharStr: PWideChar; cchWideChar: Integer; lpMultiByteStr: PAnsiChar;
  cchMultiByte: Integer; lpDefaultChar: PAnsiChar; lpUsedDefaultChar: PLongBool): Integer;
begin
  Result := LocaleCharsFromUnicode(CodePage, dwFlags, lpWideCharStr, cchWideChar, lpMultiByteStr, cchMultiByte, lpDefaultChar, lpUsedDefaultChar);
end;

function MultiByteToWideChar(CodePage: Cardinal; dwFlags: Cardinal;
  const lpMultiByteStr: PAnsiChar; cchMultiByte: Integer;
  lpWideCharStr: PWideChar; cchWideChar: Integer): Integer;
begin
  Result := UnicodeFromLocaleChars(CodePage, dwFlags, lpMultiByteStr, Integer(cchMultiByte), lpWideCharStr, cchWideChar);
end;
{$ENDIF}
{$ENDIF}

{ FPC support}

{$IFDEF FPC}

{$IFDEF UNIX}
function GetTickCount: Cardinal;
var
  tv: timeval;
begin
  fpgettimeofday(@tv, nil);
  {$RANGECHECKS OFF}
  Result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ENDIF}

{$IFDEF CPU64}
function VarArrayCreate(const Bounds: array of Integer; aVarType: TVarType): Variant;
var
  i: integer;
  Bounds64: array of Int64;
begin
  SetLength(Bounds64, Length(Bounds));
  for i := 0 to Length(Bounds) - 1 do
    Bounds64[i] := Bounds[i];
  Result :=  Variants.VarArrayCreate(Bounds64, aVarType);
end;
{$ENDIF}

{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$Q-}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE RANGECHECKS_ON}
  {$R-} // for FPC
{$ELSE}
  {$UNDEF RANGECHECKS_ON}
{$ENDIF}

function PtrOffset(Value: IntPtr; Offset: Integer): IntPtr;
begin
{$IFDEF FPC}
  Result := Value + Offset;
{$ELSE}
  Result := IntPtr(NativeInt(Value) + Offset);
{$ENDIF}
end;

{$IFDEF VER12P}
function PtrOffset(Value: IntPtr; Offset: Cardinal): IntPtr;
begin
  Result := IntPtr(NativeUInt(Value) + Offset);
end;
{$ENDIF}

{$IFNDEF FPC}
function PtrOffset(Value: IntPtr; Offset: Int64): IntPtr;
begin
  Result := IntPtr(NativeInt(Value) + Offset);
end;
{$ENDIF}

function PtrSubstract(Value1: IntPtr; Value2: IntPtr): Integer;
begin
{$IFDEF FPC}
  Result := Integer(Value1 - Value2);
{$ELSE}
  Result := Integer(NativeInt(Value1) - NativeInt(Value2));
{$ENDIF}
end;

function PtrCompare(Value1: IntPtr; Value2: IntPtr): Integer;
begin
{$IFDEF FPC}
  if Value1 > Value2 then
    Result := 1
  else if Value1 < Value2 then
    Result := -1
  else
    Result := 0
{$ELSE}
  if NativeUInt(Value1) > NativeUInt(Value2) then
    Result := 1
  else if NativeUInt(Value1) < NativeUInt(Value2) then
    Result := -1
  else
    Result := 0
{$ENDIF}
end;

function PtrCompare(Value1: IntPtr; Offset1: Integer; Value2: IntPtr; Offset2: Integer): Integer;
begin
{$IFDEF FPC}
  if Value1 + Offset1 > Value2 + Offset2 then
    Result := 1
  else if Value1 + Offset1 < Value2 + Offset2 then
    Result := -1
  else
    Result := 0
{$ELSE}
  if NativeUInt(NativeInt(Value1) + Offset1) > NativeUInt(NativeInt(Value2) + Offset2) then
    Result := 1
  else if NativeUInt(NativeInt(Value1) + Offset1) < NativeUInt(NativeInt(Value2) + Offset2) then
    Result := -1
  else
    Result := 0
{$ENDIF}
end;

function PtrCompare(Value1: IntPtr; Offset1: Int64; Value2: IntPtr; Offset2: Int64): Integer;
begin
{$IFDEF FPC}
  if Value1 + Offset1 > Value2 + Offset2 then
    Result := 1
  else if Value1 + Offset1 < Value2 + Offset2 then
    Result := -1
  else
    Result := 0
{$ELSE}
  if NativeInt(Value1) + Offset1 > NativeInt(Value2) + Offset2 then
    Result := 1
  else if NativeInt(Value1) + Offset1 < NativeInt(Value2) + Offset2 then
    Result := -1
  else
    Result := 0
{$ENDIF}
end;

{$IFDEF OVERFLOW_ON}
  {$Q+}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$IFDEF RANGECHECKS_ON}
  {$R+}
  {$UNDEF RANGECHECKS_ON}
{$ENDIF}

function IsMainThread: boolean;
begin
{$IFDEF POSIX}
  Result := TThread.CurrentThread.ThreadID = MainThreadID;
{$ELSE}
  Result := GetCurrentThreadID = MainThreadID;
{$ENDIF}
end;

function IsThread(ThreadID: TThreadID): boolean;
begin
{$IFDEF POSIX}
  Result := TThread.CurrentThread.ThreadID = ThreadID;
{$ELSE}
  Result := GetCurrentThreadId = ThreadID;
{$ENDIF}
end;

{$IFNDEF VER7P}
type
  TFakeThread = class (TThread)
  public
    procedure Synchronize(Method: TThreadMethod);
  end;

procedure TFakeThread.Synchronize(Method: TThreadMethod);
begin
  inherited Synchronize(Method);
end;
{$ENDIF}

{$IFDEF VER12P}
procedure SynchronizeWithMainThread(Proc: TThreadProcedure);
begin
  TThread.Synchronize(nil, Proc);
end;
{$ELSE}
procedure SynchronizeWithMainThread(Proc: TProcedure);
var
  Method: TMethod;
begin
  Method.Data := nil;
  Method.Code := @Proc;
{$IFDEF VER7P}
  TThread.Synchronize(nil, TThreadMethod(Method));
{$ELSE}
  TFakeThread(nil).Synchronize(TThreadMethod(Method));
{$ENDIF}
end;
{$ENDIF}

procedure SynchronizeWithMainThread(Method: TThreadMethod);
begin
{$IFDEF VER7P}
  TThread.Synchronize(nil, Method);
{$ELSE}
  TFakeThread(nil).Synchronize(Method);
{$ENDIF}
end;

function CompareMethods(Method1, Method2: TMethod): boolean;
begin
  Result := (Method1.Code = Method2.Code) and
            (Method1.Data = Method2.Data);
end;

function GetIsClass(Obj: TObject; AClass: TClass): boolean;
begin
  Result := Obj is AClass;
end;

function GetIsClassByName(Obj: TObject; AClass: TClass): boolean;
begin
  Result := GetIsClassByName(Obj, AClass.ClassName);
end;

function GetIsClassByName(Obj: TObject; const AClassName: string): boolean;
var
  ParentClass: TClass;
begin
  Result := False;
  if Obj = nil then
    Exit;

  ParentClass := Obj.ClassType;
  while ParentClass <> nil do begin
    Result := ParentClass.ClassName = AClassName;
    if not Result then
      ParentClass := ParentClass.ClassParent
    else
      Break;
  end;
end;

function LengthA(const AStr: AnsiString): integer;
begin
{$IFDEF NEXTGEN}
  Result := AStr.Length();
{$ELSE}
  Result := Length(AStr);
{$ENDIF}
end;

procedure SetLengthA(var AStr: AnsiString; NewLength: integer);
begin
{$IFDEF NEXTGEN}
  AStr.SetLength(NewLength);
{$ELSE}
  SetLength(AStr, NewLength);
{$ENDIF}
end;

function SwapCardinal(const Value: Cardinal): Cardinal;
begin
  Result := ((Value {and $FF}) shl 24) or ((Value and $FF00) shl 8) or ((Value and $FF0000) shr 8) or ((Value {and $FF000000}) shr 24);
end;

function GetTickInterval(StartTickCount, FinishTickCount: Cardinal): Cardinal;
begin
  // each 49.7 days ticks are reseted, so we should take it into attention
  // and use GetTickInterval for avoiding the out of range error
  if FinishTickCount >= StartTickCount then
    Result := FinishTickCount - StartTickCount
  else
    Result := Cardinal($FFFFFFFF) - StartTickCount + FinishTickCount + 1;
end;

{$IFDEF FPC}

function GetTickInterval(StartTickCount, FinishTickCount: UInt64): UInt64;
begin
  Result := FinishTickCount - StartTickCount;
end;

{$ENDIF}

function GetLocalTimeZoneOffset: Integer;
{$IFDEF MSWINDOWS}
var
  TZ: TTimeZoneInformation;
  DL: integer;
{$ENDIF}
{$IFDEF POSIX}
var
  T: time_t;
  TV: timeval;
  UT: tm;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DL := GetTimeZoneInformation(TZ);
  Result := -TZ.Bias;
  if DL = 2 then
    Result := Result - TZ.DaylightBias;
{$ENDIF}
{$IFDEF POSIX}
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(T, UT);
  Result := UT.tm_gmtoff div 60;
{$ENDIF}
{$IFDEF UNIX}
  Result := Tzseconds div 60;
{$ENDIF}
end;

function StrToDay(const Day: string): byte;
var
  i: integer;
begin
  for i := 0 to Length(WeekDayNames) - 1 do
    if AnsiSameText(Day, WeekDayNames[i + Low(WeekDayNames)]) then begin
      Result := i + 1;
      Exit;
    end;

  Result := 0;
end;

function StrToMonth(const Month: string): integer;
const
  Months: array[0..7] of array[1..12] of string = (
    // English
    ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
    // English alt.
    ('',    '',    '',    '',    '',    'JUNE','JULY','',    'SEPT','',    '',    ''),
    // Spanish
    ('ENO', 'FBRO','MZO', 'AB',  '',    '',    '',    'AGTO','SBRE','OBRE','NBRE','DBRE'),
    // German
    ('',    '',    'MRZ', '',    'MAI', '',    '',    '',    '',    'OKT', '',    'DEZ'),
    // Dutch
    ('',    '',    'MRT', '',    'MEI', '',    '',    '',    '',    'OKT', '',    ''),
    // Slovenian
    ('',    '',    '',    '',    'MAJ', '',    '',    'AVG', '',    '',    '',    ''),
    // French
    ('JANV','F'+Char($C9)+'V',  'MARS', 'AVR', 'MAI', 'JUIN', 'JUIL','AO'+Char($DB),    'SEPT', '', '', 'D'+Char($C9)+'C'),
    // French alt.
    ('',    'F'+Char($C9)+'VR', '',     '',    '',    '',     'JUI','AO'+Char($DB)+'T', '',     '', '', ''));

var
  i: integer;
begin
  if Month <> '' then
    for i := Low(Months) to High(Months) do begin
      for Result := Low(Months[i]) to High(Months[i]) do begin
        if AnsiSameText(Month, Months[i][Result]) then
          Exit;
      end;
    end;

  Result := 0;
end;

type
  TTimeZoneOffsetName = record
    TZName: string;
    Value: string;
  end;

function TimeZoneNameToGMTOffset(const Name: string): string;
const
  TIME_ZONES: array[0..244] of TTimeZoneOffsetName = (
    (TZName: 'A';     Value: '+0100'), // Alpha Time Zone - Military
    (TZName: 'ACDT';  Value: '+1030'), // Australian Central Daylight Time
    (TZName: 'ACST';  Value: '+0930'), // Australian Central Standard Time
    (TZName: 'ACT';   Value: '+0800'), // ASEAN Common Time
    (TZName: 'ADT';   Value: '-0300'), // Atlantic Daylight Time - North America
    (TZName: 'AEDT';  Value: '+1100'), // Australian Eastern Daylight Time
    (TZName: 'AEST';  Value: '+1000'), // Australian Eastern Standard Time
    (TZName: 'AFT';   Value: '+0430'), // Afghanistan Time
    (TZName: 'AKDT';  Value: '-0800'), // Alaska Daylight Time
    (TZName: 'AKST';  Value: '-0900'), // Alaska Standard Time
    (TZName: 'AMST';  Value: '-0300'), // Amazon Summer Time (Brazil)
    (TZName: 'AMST';  Value: '+0500'), // Armenia Summer Time
    (TZName: 'AMT';   Value: '-0400'), // Amazon Time (Brazil)
    (TZName: 'AMT';   Value: '+0400'), // Armenia Time
    (TZName: 'ART';   Value: '-0300'), // Argentina Time
    (TZName: 'AST';   Value: '-0400'), // Atlantic Standard Time - North America
    (TZName: 'AST';   Value: '+0300'), // Arabia Standard Time
    (TZName: 'AWDT';  Value: '+0900'), // Australian Western Daylight Time
    (TZName: 'AWST';  Value: '+0800'), // Australian Western Standard Time
    (TZName: 'AZOST'; Value: '-0100'), // Azores Standard Time
    (TZName: 'AZT';   Value: '+0400'), // Azerbaijan Time
    (TZName: 'B';     Value: '+0200'), // Bravo Time Zone - Military
    (TZName: 'BDT';   Value: '+0800'), // Brunei Time
    (TZName: 'BIOT';  Value: '+0600'), // British Indian Ocean Time
    (TZName: 'BIT';   Value: '-1200'), // Baker Island Time
    (TZName: 'BOT';   Value: '-0400'), // Bolivia Time
    (TZName: 'BRT';   Value: '-0300'), // Brasilia Time
    (TZName: 'BST';   Value: '+0100'), // British Summer Time - Europe
    (TZName: 'BST';   Value: '+0600'), // Bangladesh Standard Time
    (TZName: 'BTT';   Value: '+0600'), // Bhutan Time
    (TZName: 'C';     Value: '+0300'), // Charlie Time Zone - Military
    (TZName: 'CAT';   Value: '+0200'), // Central Africa Time
    (TZName: 'CCT';   Value: '+0630'), // Cocos Islands Time
    (TZName: 'CDT';   Value: '+1030'), // Central Daylight Time - Australia
    (TZName: 'CDT';   Value: '-0500'), // Central Daylight Time - North America
    (TZName: 'CEDT';  Value: '+0200'), // Central European Daylight Time
    (TZName: 'CEST';  Value: '+0200'), // Central European Summer Time
    (TZName: 'CET';   Value: '+0100'), // Central European Time
    (TZName: 'CHADT'; Value: '+1345'), // Chatham Daylight Time
    (TZName: 'CHAST'; Value: '+1245'), // Chatham Standard Time
    (TZName: 'CHOT';  Value: '+0800'), // Choibalsan
    (TZName: 'ChST';  Value: '+1000'), // Chamorro Standard Time
    (TZName: 'CHUT';  Value: '+1000'), // Chuuk Time
    (TZName: 'CIST';  Value: '-0800'), // Clipperton Island Standard Time
    (TZName: 'CIT';   Value: '+0800'), // Central Indonesia Time
    (TZName: 'CKT';   Value: '-1000'), // Cook Island Time
    (TZName: 'CLST';  Value: '-0300'), // Chile Summer Time
    (TZName: 'CLT';   Value: '-0400'), // Chile Standard Time
    (TZName: 'COST';  Value: '-0400'), // Colombia Summer Time
    (TZName: 'COT';   Value: '-0500'), // Colombia Time
    (TZName: 'CST';   Value: '+1030'), // Central Summer Time - Australia
    (TZName: 'CST';   Value: '+0930'), // Central Standard Time - Australia
    (TZName: 'CST';   Value: '-0600'), // Central Standard Time - North America
    (TZName: 'CST';   Value: '+0800'), // China Standard Time
    (TZName: 'CST';   Value: '-0500'), // Cuba Standard Time
    (TZName: 'CT';    Value: '+0800'), // China time
    (TZName: 'CVT';   Value: '-0100'), // Cape Verde Time
    (TZName: 'CWST';  Value: '+0845'), // Central Western Standard Time (Australia) unofficial
    (TZName: 'CXT';   Value: '+0700'), // Christmas Island Time - Australia
    (TZName: 'D';     Value: '+0400'), // Delta Time Zone - Military
    (TZName: 'DAVT';  Value: '+0700'), // Davis Time
    (TZName: 'DDUT';  Value: '+1000'), // Dumont d'Urville Time
    (TZName: 'DFT';   Value: '+0100'), // AIX specific equivalent of Central European Time
    (TZName: 'E';     Value: '+0500'), // Echo Time Zone - Military
    (TZName: 'EASST'; Value: '-0500'), // Easter Island Standard Summer Time
    (TZName: 'EAST';  Value: '-0600'), // Easter Island Standard Time
    (TZName: 'EAT';   Value: '+0300'), // East Africa Time
    (TZName: 'ECT';   Value: '-0400'), // Eastern Caribbean Time (does not recognise DST)
    (TZName: 'ECT';   Value: '-0500'), // Ecuador Time
    (TZName: 'EDT';   Value: '+1100'), // Eastern Daylight Time - Australia
    (TZName: 'EDT';   Value: '-0400'), // Eastern Daylight Time - North America
    (TZName: 'EEDT';  Value: '+0300'), // Eastern European Daylight Time
    (TZName: 'EEST';  Value: '+0300'), // Eastern European Summer Time
    (TZName: 'EET';   Value: '+0200'), // Eastern European Time
    (TZName: 'EGST';  Value: '+0000'), // Eastern Greenland Summer Time
    (TZName: 'EGT';   Value: '-0100'), // Eastern Greenland Time
    (TZName: 'EIT';   Value: '+0900'), // Eastern Indonesian Time
    (TZName: 'EST';   Value: '+1100'), // Eastern Summer Time - Australia
    (TZName: 'EST';   Value: '+1000'), // Eastern Standard Time - Australia
    (TZName: 'EST';   Value: '-0500'), // Eastern Standard Time - North America
    (TZName: 'F';     Value: '+0600'), // Foxtrot Time Zone - Military
    (TZName: 'FET';   Value: '+0300'), // Further-eastern European Time
    (TZName: 'FJT';   Value: '+1200'), // Fiji Time
    (TZName: 'FKST';  Value: '-0300'), // Falkland Islands Standard Time
    (TZName: 'FKST';  Value: '-0300'), // Falkland Islands Summer Time
    (TZName: 'FKT';   Value: '-0400'), // Falkland Islands Time
    (TZName: 'FNT';   Value: '-0200'), // Fernando de Noronha Time
    (TZName: 'G';     Value: '+0700'), // Golf Time Zone - Military
    (TZName: 'GALT';  Value: '-0600'), // Galapagos Time
    (TZName: 'GAMT';  Value: '-0900'), // Gambier Islands
    (TZName: 'GET';   Value: '+0400'), // Georgia Standard Time
    (TZName: 'GFT';   Value: '-0300'), // French Guiana Time
    (TZName: 'GILT';  Value: '+1200'), // Gilbert Island Time
    (TZName: 'GIT';   Value: '-0900'), // Gambier Island Time
    (TZName: 'GMT';   Value: '+0000'), // Greenwich Mean Time - Europe
    (TZName: 'GST';   Value: '-0200'), // South Georgia and the South Sandwich Islands
    (TZName: 'GST';   Value: '+0400'), // Gulf Standard Time
    (TZName: 'GYT';   Value: '-0400'), // Guyana Time
    (TZName: 'H';     Value: '+0800'), // Hotel Time Zone - Military
    (TZName: 'HAA';   Value: '-0300'), // Heure Avancee de l'Atlantique - North America
    (TZName: 'HAC';   Value: '-0500'), // Heure Avancee du Centre - North America
    (TZName: 'HADT';  Value: '-0900'), // Hawaii-Aleutian Daylight Time - North America
    (TZName: 'HAE';   Value: '-0400'), // Heure Avancee de l'Est - North America
    (TZName: 'HAEC';  Value: '+0200'), // Heure Avancee d'Europe Centrale francised name for CEST
    (TZName: 'HAP';   Value: '-0700'), // Heure Avancee du Pacifique - North America
    (TZName: 'HAR';   Value: '-0600'), // Heure Avancee des Rocheuses - North America
    (TZName: 'HAST';  Value: '-1000'), // Hawaii-Aleutian Standard Time - North America
    (TZName: 'HAT';   Value: '-0230'), // Heure Avancee de Terre-Neuve - North America
    (TZName: 'HAY';   Value: '-0800'), // Heure Avancee du Yukon - North America
    (TZName: 'HKT';   Value: '+0800'), // Hong Kong Time
    (TZName: 'HMT';   Value: '+0500'), // Heard and McDonald Islands Time
    (TZName: 'HNA';   Value: '-0400'), // Heure Normale de l'Atlantique - North America
    (TZName: 'HNC';   Value: '-0600'), // Heure Normale du Centre - North America
    (TZName: 'HNE';   Value: '-0500'), // Heure Normale de l'Est - North America
    (TZName: 'HNP';   Value: '-0800'), // Heure Normale du Pacifique - North America
    (TZName: 'HNR';   Value: '-0700'), // Heure Normale des Rocheuses - North America
    (TZName: 'HNT';   Value: '-0330'), // Heure Normale de Terre-Neuve - North America
    (TZName: 'HNY';   Value: '-0900'), // Heure Normale du Yukon - North America
    (TZName: 'HOVT';  Value: '+0700'), // Khovd Time
    (TZName: 'HST';   Value: '-1000'), // Hawaii Standard Time
    (TZName: 'I';     Value: '+0900'), // India Time Zone - Military
    (TZName: 'ICT';   Value: '+0700'), // Indochina Time
    (TZName: 'IDT';   Value: '+0300'), // Israel Daylight Time
    (TZName: 'IOT';   Value: '+0300'), // Indian Ocean Time
    (TZName: 'IRDT';  Value: '+0430'), // Iran Daylight Time
    (TZName: 'IRKT';  Value: '+0900'), // Irkutsk Time
    (TZName: 'IRST';  Value: '+0330'), // Iran Standard Time
    (TZName: 'IST';   Value: '+0100'), // Irish Summer Time - Europe
    (TZName: 'IST';   Value: '+0530'), // Indian Standard Time
    (TZName: 'IST';   Value: '+0200'), // Israel Standard Time
    (TZName: 'JST';   Value: '+0900'), // Japan Standard Time
    (TZName: 'K';     Value: '+1000'), // Kilo Time Zone - Military
    (TZName: 'KGT';   Value: '+0600'), // Kyrgyzstan time
    (TZName: 'KOST';  Value: '+1100'), // Kosrae Time
    (TZName: 'KRAT';  Value: '+0700'), // Krasnoyarsk Time
    (TZName: 'KST';   Value: '+0900'), // Korea Standard Time
    (TZName: 'L';     Value: '+1100'), // Lima Time Zone - Military
    (TZName: 'LHST';  Value: '+1030'), // Lord Howe Standard Time
    (TZName: 'LHST';  Value: '+1100'), // Lord Howe Summer Time
    (TZName: 'LINT';  Value: '+1400'), // Line Islands Time
    (TZName: 'M';     Value: '+1200'), // Mike Time Zone - Military
    (TZName: 'MAGT';  Value: '+1200'), // Magadan Time
    (TZName: 'MART';  Value: '-0930'), // Marquesas Islands Time
    (TZName: 'MAWT';  Value: '+0500'), // Mawson Station Time
    (TZName: 'MDT';   Value: '-0600'), // Mountain Daylight Time - North America
    (TZName: 'MEHSZ'; Value: '+0300'), // Mitteleuropäische Hochsommerzeit - Europe
    (TZName: 'MEST';  Value: '+0200'), // Middle European Saving Time Same zone as CEST
    (TZName: 'MESZ';  Value: '+0200'), // Mitteleuroäische Sommerzeit - Europe
    (TZName: 'MET';   Value: '+0100'), // Middle European Time Same zone as CET
    (TZName: 'MEZ';   Value: '+0100'), // Mitteleuropäische Zeit - Europe
    (TZName: 'MHT';   Value: '+1200'), // Marshall Islands
    (TZName: 'MIST';  Value: '+1100'), // Macquarie Island Station Time
    (TZName: 'MIT';   Value: '-0930'), // Marquesas Islands Time
    (TZName: 'MMT';   Value: '+0630'), // Myanmar Time
    (TZName: 'MSD';   Value: '+0400'), // Moscow Daylight Time - Europe
    (TZName: 'MSK';   Value: '+0300'), // Moscow Standard Time - Europe
    (TZName: 'MST';   Value: '-0700'), // Mountain Standard Time - North America
    (TZName: 'MST';   Value: '+0800'), // Malaysia Standard Time
    (TZName: 'MST';   Value: '+0630'), // Myanmar Standard Time
    (TZName: 'MUT';   Value: '+0400'), // Mauritius Time
    (TZName: 'MVT';   Value: '+0500'), // Maldives Time
    (TZName: 'MYT';   Value: '+0800'), // Malaysia Time
    (TZName: 'N';     Value: '-0100'), // November Time Zone - Military
    (TZName: 'NCT';   Value: '+1100'), // New Caledonia Time
    (TZName: 'NDT';   Value: '-0230'), // Newfoundland Daylight Time - North America
    (TZName: 'NFT';   Value: '+1130'), // Norfolk (Island), Time - Australia
    (TZName: 'NPT';   Value: '+0545'), // Nepal Time
    (TZName: 'NST';   Value: '-0330'), // Newfoundland Standard Time - North America
    (TZName: 'NT';    Value: '-0330'), // Newfoundland Time
    (TZName: 'NUT';   Value: '-1100'), // Niue Time
    (TZName: 'NZDT';  Value: '+1300'), // New Zealand Daylight Time
    (TZName: 'NZST';  Value: '+1200'), // New Zealand Standard Time
    (TZName: 'O';     Value: '-0200'), // Oscar Time Zone - Military
    (TZName: 'OMST';  Value: '+0700'), // Omsk Time
    (TZName: 'ORAT';  Value: '+0500'), // Oral Time
    (TZName: 'P';     Value: '-0300'), // Papa Time Zone - Military
    (TZName: 'PDT';   Value: '-0700'), // Pacific Daylight Time - North America
    (TZName: 'PET';   Value: '-0500'), // Peru Time
    (TZName: 'PETT';  Value: '+1200'), // Kamchatka Time
    (TZName: 'PGT';   Value: '+1000'), // Papua New Guinea Time
    (TZName: 'PHOT';  Value: '+1300'), // Phoenix Island Time
    (TZName: 'PKT';   Value: '+0500'), // Pakistan Standard Time
    (TZName: 'PMDT';  Value: '-0200'), // Saint Pierre and Miquelon Daylight time
    (TZName: 'PMST';  Value: '-0300'), // Saint Pierre and Miquelon Standard Time
    (TZName: 'PONT';  Value: '+1100'), // Pohnpei Standard Time
    (TZName: 'PST';   Value: '-0800'), // Pacific Standard Time - North America
    (TZName: 'PST';   Value: '+0800'), // Philippine Standard Time
    (TZName: 'PYST';  Value: '-0300'), // Paraguay Summer Time (South America)
    (TZName: 'PYT';   Value: '-0400'), // Paraguay Time (South America)
    (TZName: 'Q';     Value: '-0400'), // Quebec Time Zone - Military
    (TZName: 'R';     Value: '-0500'), // Romeo Time Zone - Military
    (TZName: 'RET';   Value: '+0400'), // Reunion Time
    (TZName: 'ROTT';  Value: '-0300'), // Rothera Research Station Time
    (TZName: 'S';     Value: '-0600'), // Sierra Time Zone - Military
    (TZName: 'SAKT';  Value: '+1100'), // Sakhalin Island time
    (TZName: 'SAMT';  Value: '+0400'), // Samara Time
    (TZName: 'SAST';  Value: '+0200'), // South African Standard Time
    (TZName: 'SBT';   Value: '+1100'), // Solomon Islands Time
    (TZName: 'SCT';   Value: '+0400'), // Seychelles Time
    (TZName: 'SGT';   Value: '+0800'), // Singapore Time
    (TZName: 'SLST';  Value: '+0530'), // Sri Lanka Time
    (TZName: 'SRT';   Value: '-0300'), // Suriname Time
    (TZName: 'SST';   Value: '-1100'), // Samoa Standard Time
    (TZName: 'SST';   Value: '+0800'), // Singapore Standard Time
    (TZName: 'SYOT';  Value: '+0300'), // Showa Station Time
    (TZName: 'T';     Value: '-0700'), // Tango Time Zone - Military
    (TZName: 'TAHT';  Value: '-1000'), // Tahiti Time
    (TZName: 'THA';   Value: '+0700'), // Thailand Standard Time
    (TZName: 'TFT';   Value: '+0500'), // Indian/Kerguelen
    (TZName: 'TJT';   Value: '+0500'), // Tajikistan Time
    (TZName: 'TKT';   Value: '+1300'), // Tokelau Time
    (TZName: 'TLT';   Value: '+0900'), // Timor Leste Time
    (TZName: 'TMT';   Value: '+0500'), // Turkmenistan Time
    (TZName: 'TOT';   Value: '+1300'), // Tonga Time
    (TZName: 'TVT';   Value: '+1200'), // Tuvalu Time
    (TZName: 'U';     Value: '-0800'), // Uniform Time Zone - Military
    (TZName: 'UCT';   Value: '+0000'), // Coordinated Universal Time
    (TZName: 'ULAT';  Value: '+0800'), // Ulaanbaatar Time
    (TZName: 'UT';    Value: '+0000'), // Universal Time - Europe
    (TZName: 'UTC';   Value: '+0000'), // Coordinated Universal Time - Europe
    (TZName: 'UYST';  Value: '-0200'), // Uruguay Summer Time
    (TZName: 'UYT';   Value: '-0300'), // Uruguay Standard Time
    (TZName: 'UZT';   Value: '+0500'), // Uzbekistan Time
    (TZName: 'V';     Value: '-0900'), // Victor Time Zone - Military
    (TZName: 'VET';   Value: '-0430'), // Venezuelan Standard Time
    (TZName: 'VLAT';  Value: '+1000'), // Vladivostok Time
    (TZName: 'VOLT';  Value: '+0400'), // Volgograd Time
    (TZName: 'VOST';  Value: '+0600'), // Vostok Station Time
    (TZName: 'VUT';   Value: '+1100'), // Vanuatu Time
    (TZName: 'W';     Value: '-1000'), // Whiskey Time Zone - Military
    (TZName: 'WAKT';  Value: '+1200'), // Wake Island Time
    (TZName: 'WAST';  Value: '+0200'), // West Africa Summer Time
    (TZName: 'WAT';   Value: '+0100'), // West Africa Time
    (TZName: 'WDT';   Value: '+0900'), // Western Daylight Time - Australia
    (TZName: 'WEDT';  Value: '+0100'), // Western European Daylight Time - Europe
    (TZName: 'WEST';  Value: '+0100'), // Western European Summer Time - Europe
    (TZName: 'WET';   Value: '+0000'), // Western European Time - Europe
    (TZName: 'WIT';   Value: '+0700'), // Western Indonesian Time
    (TZName: 'WST';   Value: '+0900'), // Western Summer Time - Australia
    (TZName: 'WST';   Value: '+0800'), // Western Standard Time - Australia
    (TZName: 'X';     Value: '-1100'), // X-ray Time Zone - Military
    (TZName: 'Y';     Value: '-1200'), // Yankee Time Zone - Military
    (TZName: 'YAKT';  Value: '+1000'), // Yakutsk Time
    (TZName: 'YEKT';  Value: '+0600'), // Yekaterinburg Time
    (TZName: 'Z';     Value: '+0000')  // Zulu Time Zone - Military
  );

var
  i: integer;
begin
  for i := Low(TIME_ZONES) to High(TIME_ZONES) do begin
    if AnsiSameText(Name, TIME_ZONES[i].TZName) then begin
      Result := TIME_ZONES[i].Value;
      Exit;
    end;
  end;

  Result := '-0000';
end;

function GMTOffsetStrToDateTime(const Value: string): TDateTime;
var
  Str: string;
begin
  Result := 0;
  Str := Trim(Value);
  Str := ExtractFirstWord(Str, ' ');
  if Str = '' then
    Exit;

  if (Str[1] <> '-') and (Str[1] <> '+') then
    Str := TimeZoneNameToGMTOffset(Str)
  else begin
    // ignore colon in the middle
    if Length(Str) = 6 then begin
      if Str[4] = ':' then
        Delete(Str, 4, 1);
    end
    // add minutes if it was omitted
    else if Length(Str) = 3 then
      Str := Str + '00';

    if (Length(Str) <> 5) or not IsNumeric(Copy(Str, 2, 2)) or not IsNumeric(Copy(Str, 4, 2)) then
      Exit;
  end;

  try
    Result := EncodeTime(StrToInt(Copy(Str, 2, 2)), StrToInt(Copy(Str, 4, 2)), 0, 0);
    if Str[1] = '-' then
      Result := -Result;
  except
    Result := 0;
  end;
end;

function TryInternetStrToDateTime(var Value: string; var DT: TDateTime): boolean;
var
  Year: integer;
  Month, Day, Hour, Min, Sec, MSec: word;
  sYear, sTime, Delim: string;
  AM, PM: boolean;
  i: integer;
begin
  Result := False;
  DT := 0;

  Value := Trim(Value);
  if Value = '' then
    Exit;

  try
    if StrToDay(Copy(Value, 1, 3)) > 0 then begin
      if (Length(Value) >= 5) and (Value[4] = ',') and (Value[5] <> ' ') then
        Insert(' ', Value, 5);

      ExtractFirstWord(Value, ' ');
      Value := TrimLeft(Value);
    end;

    i := Pos('-', Value);
    if (i > 1) and (i < Pos(' ', Value)) then
      Delim := '-'
    else
      Delim := ' ';

    // 'Mon, Jan 1 2001'   or   'Mon, 1 Jan 2001'
    if StrToMonth(GetFirstWord(Value, Delim)) > 0 then begin
      Month := StrToMonth(ExtractFirstWord(Value, Delim));
      Value := TrimLeft(Value);

      Day := StrToIntDef(ExtractFirstWord(Value, Delim), 1);
      Value := TrimLeft(Value);
    end
    else begin
      Day := StrToIntDef(ExtractFirstWord(Value, Delim), 1);
      Value := TrimLeft(Value);

      Month := StrToMonth(ExtractFirstWord(Value, Delim));
      Value := TrimLeft(Value);
    end;

    // 'Mon Jan  1 01:01:01 2001'
    sYear := ExtractFirstWord(Value, ' ');
    if not TryStrToInt(sYear, Year) then begin
      sTime := sYear;
      sYear := ExtractFirstWord(Value, ' ');
      Value := TrimRight(sTime + ' ' + Value);
      Year := StrToInt(sYear);
    end;

    if Length(sYear) = 2 then begin
      if Year <= 49 then
        Inc(Year, 2000)
      else
      if (Year >= 50) and (Year <= 99) then
        Inc(Year, 1900)
    end
    else
    if Length(sYear) = 3 then
      Inc(Year, 1900);

    DT := EncodeDate(Year, Month, Day);

    if Pos('AM', Value) > 0 then begin
      AM := True;
      PM := False;
      Value := ExtractFirstWord(Value, 'AM');
    end
    else
    if Pos('PM', Value) > 0 then begin
      AM := False;
      PM := True;
      Value := ExtractFirstWord(Value, 'PM');
    end
    else begin
      AM := False;
      PM := False;
    end;

    i := Pos('.', Value);
    if (i > 0) and (i < Pos(' ', Value)) then
      Delim := '.'
    else
      Delim := ':';

    i := Pos(Delim, Value);
    if i > 0 then begin
      sTime := ExtractFirstWord(Value, ' ');
      Hour := StrToIntDef(ExtractFirstWord(sTime, Delim), 0);
      Min := StrToIntDef(ExtractFirstWord(sTime, Delim), 0);
      Sec := StrToIntDef(ExtractFirstWord(sTime, ' '), 0);
      MSec := 0;

      Value := TrimLeft(Value);
      if AM then begin
        if Hour = 12 then
          Hour := 0;
      end
      else
      if PM then begin
        if Hour < 12 then
          Inc(Hour, 12);
      end;

      if DT >= 0 then
        DT := DT + EncodeTime(Hour, Min, Sec, MSec)
      else
        DT := DT - EncodeTime(Hour, Min, Sec, MSec);
    end;

    Value := TrimLeft(Value);
    Result := True;
  except
    DT := 0;
    Result := False;
  end;
end;

function InternetStrToDateTime(const Value: string): TDateTime;
var
  S: string;
begin
  S := Value;
  TryInternetStrToDateTime(S, Result);
end;

function GMTToLocalDateTime(const Value: string): TDateTime;
var
  S: string;
  DateTimeOffset: TDateTime;
begin
  S := Value;
  if TryInternetStrToDateTime(S, Result) then begin
    DateTimeOffset := GMTOffsetStrToDateTime(S);
    Result := Result - DateTimeOffset + (GetLocalTimeZoneOffset / (60 * 24));
  end;
end;

function TimeZoneOffsetToStr(TimeZoneOffset{minute}: integer; IncludeGMT: boolean): string;
begin
  if (TimeZoneOffset = 0) and IncludeGMT then
    Result := 'GMT'
  else begin
    Result := Format(' %0.2d%0.2d', [TimeZoneOffset div 60, TimeZoneOffset mod 60]);
    if TimeZoneOffset < 0 then
      Result[1] := '-'
    else
      Result[1] := '+';
  end;
end;

function Str2(Value: word): string;
begin
  if Value < 10 then
    Result := '0' + Chr(Value + Ord('0'))
  else
    Result := Chr((Value div 10) + Ord('0')) + Chr((Value mod 10) + Ord('0'));
end;

function LocalDateTimeToGMT(const Value: TDateTime; IncludeGMT: boolean = False): string;
var
  Year, Month, Day, DOW, Hour, Min, Sec, MSec: word;
begin
  DecodeDateFully(Value, Year, Month, Day, DOW);
  DecodeTime(Value, Hour, Min, Sec, MSec);

  Result := Format('%s, %d %s %d %s:%s:%s %s',
    [WeekDayNames[DOW], Day, MonthNames[Month], Year, Str2(Hour), Str2(Min), Str2(Sec),
    TimeZoneOffsetToStr(GetLocalTimeZoneOffset, IncludeGMT)]);
end;

function DateTimeToHttpStr(const Value: TDateTime): string;
var
  Year, Month, Day, DOW, Hour, Min, Sec, MSec: word;
begin
  DecodeDateFully(Value, Year, Month, Day, DOW);
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Assert(DOW in [1..7]);
  Assert(Month in [1..12]);

  /// 'Mon, 01 Jan 2001 01:01:01 GMT'
  Result := Format('%s, %s %s %s %s:%s:%s GMT',
    [WeekDayNames[DOW], Str2(Day), MonthNames[Month], IntToStr(Year), Str2(Hour), Str2(Min), Str2(Sec)]);
end;

{$HINTS OFF}
{$IFDEF FPC}
{$NOTES OFF}
{$ENDIF}
function IsNumeric(const Str: string): boolean;
var
  Code: Integer;
  i64: Int64;
begin
  Val(Str, i64, Code);
  Result := Code = 0;
end;
{$IFDEF FPC}
{$NOTES ON}
{$ENDIF}
{$HINTS ON}

function ExtractFirstWord(var Str: string; const Separator: string): string;
var
  p: integer;
begin
  p := AnsiPos(Separator, Str);

  if p = 0 then begin
    Result := Str;
    Str := '';
  end
  else begin
    Result := Copy(Str, 1, p - 1);
    Str := Copy(Str, p + Length(Separator), MaxInt);
  end;
end;

function GetFirstWord(const Str: string; const Separator: string): string;
var
  p: integer;
begin
  p := AnsiPos(Separator, Str);

  if p = 0 then
    Result := Str
  else
    Result := Copy(Str, 1, p - 1);
end;

function ByteInSet(AByte: byte; const ASet: TBytes): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(ASet) - 1 do begin
    if ASet[i] = AByte then begin
      Result := True;
      Exit;
    end;
  end;
end;

{ CRBitConverter}

class function CRBitConverter.Int32BitsToSingle(Value: Integer): Single;
begin
  Result := Single(Pointer(@Value)^);
end;

class function CRBitConverter.SingleToInt32Bits(Value: Single): integer;
begin
  Result := Integer(Pointer(@Value)^);
end;

class function CRBitConverter.BytesToExtended(const Value: TBytes): Extended;
begin
  Result := Extended(Pointer(@Value[0])^);
end;

class function CRBitConverter.ExtendedToBytes(Value: Extended): TBytes;
begin
  SetLength(Result, SizeOf(Extended));
  Extended(Pointer(@Result[0])^) := Value;
end;

{$IFDEF MSWINDOWS}
function DetectLibraryMachineType(const LibraryName: String): TMachineType;
var
  Handle: THandle;
  Offset: Integer;
  Head: Cardinal;
  MT: Word;
begin
  Result := mtUnknown;

  if not FileExists(LibraryName) then
    exit;

  Handle := FileOpen(LibraryName, fmOpenRead + fmShareDenyNone);
  try
    if Handle = INVALID_HANDLE_VALUE then
      Exit;
    if FileSeek(Handle, $3C, soFromBeginning) <> $3C then
      Exit;
    if FileRead(Handle, Offset, SizeOf(Offset)) <> SizeOf(Offset) then
      Exit;
    if FileSeek(Handle, Offset, soFromBeginning) <> Offset then
      Exit;
    if FileRead(Handle, Head, SizeOf(Head)) <> SizeOf(Head) then
      Exit;
    if Head <> $00004550 then
      Exit;
    if FileRead(Handle, MT, SizeOf(MT)) <> SizeOf(MT) then
      Exit;

    case MT of
     $01D3:
       Result := mtAM33;
     $8664:
       Result := mtAMD64;
     $01C0:
       Result := mtARM;
     $0EBC:
       Result := mtEBC;
     $014C:
       Result := mtI386;
     $0200:
       Result := mtIA64;
     $9041:
       Result := mtM32R;
     $0266:
       Result := mtMIPS16;
     $0366:
       Result := mtMIPSFPU;
     $0466:
       Result := mtMIPSFPU16;
     $01F0:
       Result := mtPOWERPC;
     $01F1:
       Result := mtPOWERPCFP;
     $0166:
       Result := mtR4000;
     $01A2:
       Result := mtSH3;
     $01A3:
       Result := mtSH3DSP;
     $01A6:
       Result := mtSH4;
     $01A8:
       Result := mtSH5;
     $01C2:
       Result := mtTHUMB;
     $0169:
       Result := mtWCEMIPSV2;
    end;
  finally
    FileClose(Handle);
  end;
end;
{$ENDIF}

{ BobJenkinsHash }

{$IFOPT Q+}
  {$DEFINE OVERFLOW_ON}
  {$Q-}
{$ELSE}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE RANGECHECKS_ON}
  {$R-} // for FPC
{$ELSE}
  {$UNDEF RANGECHECKS_ON}
{$ENDIF}

function BobJenkinsHash(const Data; Len, InitData: Integer): Integer;

  function Rot(x, k: Cardinal): Cardinal; {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Result := (x shl k) or (x shr (32 - k));
  end;

  procedure Mix(var a, b, c: Cardinal); {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    Dec(a, c); a := a xor Rot(c, 4); Inc(c, b);
    Dec(b, a); b := b xor Rot(a, 6); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 8); Inc(b, a);
    Dec(a, c); a := a xor Rot(c,16); Inc(c, b);
    Dec(b, a); b := b xor Rot(a,19); Inc(a, c);
    Dec(c, b); c := c xor Rot(b, 4); Inc(b, a);
  end;

  procedure Final(var a, b, c: Cardinal); {$IFDEF USE_INLINE}inline;{$ENDIF}
  begin
    c := c xor b; Dec(c, Rot(b,14));
    a := a xor c; Dec(a, Rot(c,11));
    b := b xor a; Dec(b, Rot(a,25));
    c := c xor b; Dec(c, Rot(b,16));
    a := a xor c; Dec(a, Rot(c, 4));
    b := b xor a; Dec(b, Rot(a,14));
    c := c xor b; Dec(c, Rot(b,24));
  end;

  // http://burtleburtle.net/bob/c/lookup3.c
  function HashLittle(const Data; Len, InitVal: Integer): Integer;
  type
    T12Byte = array[0..11] of byte;
    P12Byte = ^T12Byte;
    T3Cardinal = array[0..2] of Cardinal;
    P3Cardinal = ^T3Cardinal;
  var
    pb: P12Byte;
    pd: P3Cardinal absolute pb;
    a, b, c: Cardinal;
  label
    case_1, case_2, case_3, case_4, case_5, case_6,
    case_7, case_8, case_9, case_10, case_11, case_12;
  begin
    a := Cardinal($DEADBEEF) + Cardinal(Len shl 2) + Cardinal(InitVal);
    b := a;
    c := a;

    pb := @Data;

    // 4-byte aligned data
    if NativeUInt(pb) and 3 = 0 then
    begin
      while Len > 12 do
      begin
        Inc(a, pd[0]);
        Inc(b, pd[1]);
        Inc(c, pd[2]);
        Mix(a, b, c);
        Dec(Len, 12);
        pd := Pointer(NativeUInt(pd) + 12);
      end;

      case Len of
        0: begin
          Result := Integer(c);
          Exit;
        end;
        1: Inc(a, pd[0] and $FF);
        2: Inc(a, pd[0] and $FFFF);
        3: Inc(a, pd[0] and $FFFFFF);
        4: Inc(a, pd[0]);
        5:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1] and $FF);
        end;
        6:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1] and $FFFF);
        end;
        7:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1] and $FFFFFF);
        end;
        8:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1]);
        end;
        9:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1]);
          Inc(c, pd[2] and $FF);
        end;
        10:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1]);
          Inc(c, pd[2] and $FFFF);
        end;
        11:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1]);
          Inc(c, pd[2] and $FFFFFF);
        end;
        12:
        begin
          Inc(a, pd[0]);
          Inc(b, pd[1]);
          Inc(c, pd[2]);
        end;
      end;
    end
    else
    begin
      // Ignoring rare case of 2-byte aligned data. This handles all other cases.
      while Len > 12 do
      begin
        Inc(a, pb[0] + pb[1] shl 8 + pb[2] shl 16 + pb[3] shl 24);
        Inc(b, pb[4] + pb[5] shl 8 + pb[6] shl 16 + pb[7] shl 24);
        Inc(c, pb[8] + pb[9] shl 8 + pb[10] shl 16 + pb[11] shl 24);
        Mix(a, b, c);
        Dec(Len, 12);
        pb := Pointer(NativeUInt(pb) + 12);
      end;

      case Len of
        0: begin
          Result := c;
          Exit;
        end;
        1: goto case_1;
        2: goto case_2;
        3: goto case_3;
        4: goto case_4;
        5: goto case_5;
        6: goto case_6;
        7: goto case_7;
        8: goto case_8;
        9: goto case_9;
        10: goto case_10;
        11: goto case_11;
        12: goto case_12;
      end;

  case_12:
      Inc(c, pb[11] shl 24);
  case_11:
      Inc(c, pb[10] shl 16);
  case_10:
      Inc(c, pb[9] shl 8);
  case_9:
      Inc(c, pb[8]);
  case_8:
      Inc(b, pb[7] shl 24);
  case_7:
      Inc(b, pb[6] shl 16);
  case_6:
      Inc(b, pb[5] shl 8);
  case_5:
      Inc(b, pb[4]);
  case_4:
      Inc(a, pb[3] shl 24);
  case_3:
      Inc(a, pb[2] shl 16);
  case_2:
      Inc(a, pb[1] shl 8);
  case_1:
      Inc(a, pb[0]);
    end;

    Final(a, b, c);
    Result := Integer(c);
  end;

begin
  Result := HashLittle(Data, Len, InitData);
end;

function BobJenkinsHashStr(const Str: string; InitData: Integer): Integer;
begin
{$IFDEF VER12P}
  Result := BobJenkinsHash(PWideChar(Str)^, SizeOf(WideChar) * Length(Str), InitData);
{$ELSE}
  Result := BobJenkinsHash(PAnsiChar(Str)^, SizeOf(AnsiChar) * Length(Str), InitData);
{$ENDIF}
end;

function BobJenkinsHashAStr(const AStr: AnsiString; InitData: Integer): Integer;
begin
  Result := BobJenkinsHash(PAnsiChar(AStr)^, SizeOf(AnsiChar) * Length(AStr), InitData);
end;

function BobJenkinsHashWStr(const WStr: WideString; InitData: Integer): Integer;
begin
  Result := BobJenkinsHash(PWideChar(WStr)^, SizeOf(WideChar) * Length(WStr), InitData);
end;

{$IFDEF OVERFLOW_ON}
  {$Q+}
  {$UNDEF OVERFLOW_ON}
{$ENDIF}

{$IFDEF RANGECHECKS_ON}
  {$R+}
  {$UNDEF RANGECHECKS_ON}
{$ENDIF}

initialization
  if not Assigned(IsClass) then
    if IsLibrary then
      IsClass := GetIsClassByName
    else
      IsClass := GetIsClass;

finalization
{$IFDEF FPC}
  FreeAndNil(InternalAnsiEncoding);
{$ENDIF}

end.
