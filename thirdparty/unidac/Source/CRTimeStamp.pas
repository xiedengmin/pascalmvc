//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  TimeStamp
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRTimeStamp;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysConst, SysUtils, StrUtils, DateUtils, Variants,
{$IFNDEF FPC}
 {$IFNDEF VER14P}
  TypInfo,
  {$ENDIF}
  SqlTimSt,
{$ENDIF}
  CRTypes;

{$IFNDEF VER14P}
{$IFDEF FPC}
type
  PSQLTimeStamp = ^TSQLTimeStamp;
  TSQLTimeStamp = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Fractions: LongWord;
  end;

const
  NullSQLTimeStamp: TSQLTimeStamp = (Year: 0; Month: 0; Day: 0; Hour: 0; Minute: 0; Second: 0; Fractions: 0);
{$ENDIF}

type
  PSQLTimeStampOffset = ^TSQLTimeStampOffset;
  TSQLTimeStampOffset = record
    Year: Word;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Fractions: LongWord;
    TimeZoneHour: SmallInt;
    TimeZoneMinute: SmallInt;
  end;

const
  NullSQLTimeStampOffset: TSQLTimeStampOffset = (Year: 0; Month: 0; Day: 0; Hour: 0; Minute: 0; Second: 0; Fractions: 0; TimeZoneHour: 0; TimeZoneMinute: 0);
{$ENDIF}

{$IFDEF VER14}
type
  PSQLTimeStampOffset = ^TSQLTimeStampOffset;
{$ENDIF}

  function IsSQLTimeStampBlank(const TimeStamp: TSQLTimeStamp): Boolean;
  function SQLTimeStampCompare(const Value1, Value2: TSQLTimeStamp): Integer;

  function IsSQLTimeStampOffsetBlank(const TimeStampOffset: TSQLTimeStampOffset): Boolean;
  function SQLTimeStampOffsetCompare(const Value1, Value2: TSQLTimeStampOffset): Integer;
  function SQLTimeStampOffsetToDateTime(const DateTimeOffset: TSQLTimeStampOffset): TDateTime;
  function StrToSQLTimeStampOffset(const S: string; Scale: integer = 3): TSQLTimeStampOffset;
  function ConvertFromUTC(const Value: TSQLTimeStampOffset): TSQLTimeStamp;
  function ConvertToUTC(const Value: TSQLTimeStampOffset): TSQLTimeStamp;

  procedure DateTimeToString(var Result: string; const Format: string; const TimeStamp: TSQLTimeStamp; const AFormatSettings: TFormatSettings);
  function ExtractMSecFromString(const S: string; const AFormatSettings: TFormatSettings; Scale: integer): cardinal;

{$IFNDEF VER14P}
{$IFDEF FPC}
  procedure CheckSqlTimeStamp(const ASQLTimeStamp: TSQLTimeStamp);
  function DateTimeToSQLTimeStamp(const DateTime: TDateTime): TSQLTimeStamp;
  function SQLTimeStampToDateTime(const DateTime: TSQLTimeStamp): TDateTime;
  function StrToSQLTimeStamp(const S: string; Scale: integer = 3): TSQLTimeStamp;

  function VarSQLTimeStamp: TVarType;
  function VarSQLTimeStampCreate: Variant; overload;
  function VarSQLTimeStampCreate(const ASQLTimeStamp: TSQLTimeStamp): Variant; overload;
  procedure VarSQLTimeStampCreate(var aDest: Variant; const ASQLTimeStamp: TSQLTimeStamp); overload;
  function VarIsSQLTimeStamp(const aValue: Variant): Boolean;
  function VarToSQLTimeStamp(const aValue: Variant): TSQLTimeStamp;
{$ENDIF}

  procedure CheckSqlTimeStampOffset(const ASQLTimeStampOffset: TSQLTimeStampOffset);
  function DateTimeToSQLTimeStampOffset(const DateTime: TDateTime): TSQLTimeStampOffset; overload;
  function DateTimeToSQLTimeStampOffset(const DateTime: TDateTime; const TZOffsetHour, TZOffsetMinute: Integer): TSQLTimeStampOffset; overload;

  function VarSQLTimeStampOffset: TVarType;
  function VarSQLTimeStampOffsetCreate: Variant; overload;
  function VarSQLTimeStampOffsetCreate(const ASQLTimeStampOffset: TSQLTimeStampOffset): Variant; overload;
  procedure VarSQLTimeStampOffsetCreate(var aDest: Variant; const ASQLTimeStampOffset: TSQLTimeStampOffset); overload;
  function VarIsSQLTimeStampOffset(const aValue: Variant): Boolean;
  function VarToSQLTimeStampOffset(const aValue: Variant): TSQLTimeStampOffset;
{$ENDIF}

implementation

uses
{$IFDEF POSIX}
{$IFDEF MACOS}
  Macapi.CoreFoundation,
{$ENDIF}
{$ENDIF}
  CRFunctions;

{$IFNDEF VER14P}

const
  SInvalidSqlTimeStamp = 'Invalid SQL date/time values';

{$IFDEF FPC}

type
  TSQLTimeStampData = class(TPersistent)
  private
    FTimeStamp: TSQLTimeStamp;
  public
    constructor Create(const ASQLTimeStamp: TSQLTimeStamp); overload;
    constructor Create(const ASource: TSQLTimeStampData); overload;
  end;

  TSQLTimeStampVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VTimeStamp: TSQLTimeStampData;
    Reserved4: IntPtr;
  end;

  TSQLTimeStampVariantType = class(TPublishableVariantType)
  protected
    function GetInstance(const V: TVarData): TObject; override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult); override;
  end;

{ SQLTimeStamp that the complex variant points to }
var
  SQLTimeStampVariantType: TSQLTimeStampVariantType = nil;

{$ENDIF}

type
  TSQLTimeStampOffsetData = class(TPersistent)
  private
    FTimeStampOffset: TSQLTimeStampOffset;
  public
    constructor Create(const AText: string); overload;
    constructor Create(const ASQLTimeStampOffset: TSQLTimeStampOffset); overload;
    constructor Create(const ASource: TSQLTimeStampOffsetData); overload;
  end;

  TSQLTimeStampOffsetVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VTimeStampOffset: TSQLTimeStampOffsetData;
    Reserved4: IntPtr;
  end;

  TSQLTimeStampOffsetVariantType = class(TPublishableVariantType)
  protected
    function GetInstance(const V: TVarData): TObject; override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult); override;
  end;

{ SQLTimeStampOffset that the complex variant points to }
var
  SQLTimeStampOffsetVariantType: TSQLTimeStampOffsetVariantType = nil;

{$ENDIF}

{ Functions }

function IsSQLTimeStampBlank(const TimeStamp: TSQLTimeStamp): Boolean;
begin
  Result := (TimeStamp.Year = 0) and
            (TimeStamp.Month = 0) and
            (TimeStamp.Day = 0) and
            (TimeStamp.Hour = 0) and
            (TimeStamp.Minute = 0) and
            (TimeStamp.Second = 0) and
            (TimeStamp.Fractions = 0);
end;

function SQLTimeStampCompare(const Value1, Value2: TSQLTimeStamp): Integer;
var
  Status: Integer;
begin
  Status := Integer(Value1.Year) - Integer(Value2.Year);
  if Status = 0 then
    Status := Integer(Value1.Month) - Integer(Value2.Month);
  if Status = 0 then
    Status := Integer(Value1.Day) - Integer(Value2.Day);
  if Status = 0 then
    Status := Integer(Value1.Hour) - Integer(Value2.Hour);
  if Status = 0 then
    Status := Integer(Value1.Minute) - Integer(Value2.Minute);
  if Status = 0 then
    Status := Integer(Value1.Second) - Integer(Value2.Second);
  if Status = 0 then
    Status := Integer(Value1.Fractions) - Integer(Value2.Fractions);
  if Status = 0 then
    Result := 0
  else
  if Status > 0 then
    Result := 1
  else
    Result := -1;
end;

function IsSQLTimeStampOffsetBlank(const TimeStampOffset: TSQLTimeStampOffset): Boolean;
begin
  Result := (TimeStampOffset.Year = 0) and
            (TimeStampOffset.Month = 0) and
            (TimeStampOffset.Day = 0) and
            (TimeStampOffset.Hour = 0) and
            (TimeStampOffset.Minute = 0) and
            (TimeStampOffset.Second = 0) and
            (TimeStampOffset.Fractions = 0) and
            (TimeStampOffset.TimeZoneHour = 0) and
            (TimeStampOffset.TimeZoneMinute = 0);
end;

function SQLTimeStampOffsetCompare(const Value1, Value2: TSQLTimeStampOffset): integer;
var
  L, R: TSQLTimeStamp;
begin
  L := ConvertToUTC(Value1);
  R := ConvertToUTC(Value2);

  Result := L.Year - R.Year;
  if Result = 0 then
    Result := L.Month - R.Month;
  if Result = 0 then
    Result := L.Day - R.Day;
  if Result = 0 then
    Result := L.Hour - R.Hour;
  if Result = 0 then
    Result := L.Hour - R.Hour;
  if Result = 0 then
    Result := L.Minute - R.Minute;
  if Result = 0 then
    Result := L.Second - R.Second;
  if Result = 0 then
    Result := L.Fractions - R.Fractions;
end;

function SQLTimeStampOffsetToDateTime(const DateTimeOffset: TSQLTimeStampOffset): TDateTime;
begin
  if IsSQLTimeStampOffsetBlank(DateTimeOffset) then
    Result := 0
  else
  begin
    Result := EncodeDate(DateTimeOffset.Year, DateTimeOffset.Month, DateTimeOffset.Day);
    if Result >= 0 then
      Result := Result + EncodeTime(DateTimeOffset.Hour, DateTimeOffset.Minute, DateTimeOffset.Second, DateTimeOffset.Fractions)
    else
      Result := Result - EncodeTime(DateTimeOffset.Hour, DateTimeOffset.Minute, DateTimeOffset.Second, DateTimeOffset.Fractions);
  end;
end;

function IsSqlTimeStampOffsetValid(const ts: TSQLTimeStampOffset): Boolean;
begin
  if (ts.Month > 12) or (ts.Day > DaysInAMonth(ts.Year, ts.Month)) or
       (ts.Hour > 23) or (ts.Minute > 59) or (ts.Second > 59) or
       (ts.TimeZoneHour > 14) or (ts.TimeZoneHour < -12) or (ts.TimeZoneMinute > 59) or
       ((ts.TimeZoneHour = 14) and (ts.TimeZoneMinute > 0)) then
    Result := False
  else
    Result := True;
end;

function TryStrToSQLTimeStampOffset(const S: string; var TimeStampOffset: TSQLTimeStampOffset; Scale: integer = 3) : Boolean;
var
  I: Integer;
  DT: TDateTime;
  OffsetStr: string;
  Offset: TDateTime;
  Hour, Minute, Second, MSec: Word;
begin
  if S = '' then begin
    Result := False;
    Exit;
  end;

  I := Pos(FormatSettings.TimeSeparator, S);
  if I > 0 then
    while (I < Length(S)) and (S[I] <> ' ') do
      Inc(I);

  if (I > 0) and (I < Length(S)) then begin
    Result := TryStrToDateTime(Copy(S, 1, I - 1), DT);
    while (I < Length(S)) and (S[I] = ' ') do
      Inc(I);
    OffsetStr := Copy(S, I + 1, Length(S) - I);
    Offset := StrToDateTime(OffsetStr);
  end
  else begin
    Result := TryStrToDateTime(S, DT);
    Offset := 0;
  end;

  if Result then begin
    TimeStampOffset := DateTimeToSQLTimeStampOffset(DT);
    if TimeStampOffset.Fractions = 0 then
      TimeStampOffset.Fractions := ExtractMSecFromString(S, FormatSettings, Scale);

    DecodeTime(Offset, Hour, Minute, Second, MSec);
    if S[I] = '-' then
      TimeStampOffset.TimeZoneHour := -Hour
    else
      TimeStampOffset.TimeZoneHour := Hour;
    TimeStampOffset.TimeZoneMinute := Minute;
    Result := IsSqlTimeStampOffsetValid(TimeStampOffset);
  end;
  if not Result then
    TimeStampOffset := NullSQLTimeStampOffset;
end;

function StrToSQLTimeStampOffset(const S: string; Scale: integer = 3): TSQLTimeStampOffset;
const
  SCouldNotParseTimeStamp = 'Could not parse SQL TimeStamp Offset string';
begin
  if not TryStrToSqlTimeStampOffset(S, Result, Scale) then
    raise EConvertError.Create(SCouldNotParseTimeStamp);
end;

function ConvertFromUTC(const Value: TSQLTimeStampOffset): TSQLTimeStamp;
var
  Date: TDateTime;
  M: Integer;
begin
  Move(Value, Result, sizeof(TSQLTimeStamp));

  if (Value.TimeZoneHour <> 0) or (Value.TimeZoneMinute <> 0) then begin
    Date := EncodeDate(Result.Year, Result.Month, Result.Day);

    M := Value.Hour * MinsPerHour + Value.Minute;
    if Value.TimeZoneHour < 0 then
      M := M + Value.TimeZoneHour * MinsPerHour - Value.TimeZoneMinute
    else
      M := M + Value.TimeZoneHour * MinsPerHour + Value.TimeZoneMinute;

    if M < 0 then begin
      Date := IncDay(Date, -1);
      M := M + MinsPerDay;
    end
    else
    if M >= MinsPerDay then begin
      Date := IncDay(Date);
      M := M - MinsPerDay;
    end;

    DecodeDate(Date, Word(Result.Year), Result.Month, Result.Day);
    Result.Hour := M div MinsPerHour;
    Result.Minute := M mod MinsPerHour;
  end;
end;

function ConvertToUTC(const Value: TSQLTimeStampOffset): TSQLTimeStamp;
var
  Date: TDateTime;
  Y: Word;
  M: Integer;
begin
  Result.Year := Value.Year;
  Result.Month := Value.Month;
  Result.Day := Value.Day;
  Result.Hour := Value.Hour;
  Result.Minute := Value.Minute;
  Result.Second := Value.Second;
  Result.Fractions := Value.Fractions;
  if (Value.TimeZoneHour <> 0) or (Value.TimeZoneMinute <> 0) then
  begin
    Date := EncodeDate(Result.Year, Result.Month, Result.Day);

    M := Value.Hour * MinsPerHour + Value.Minute;
    if Value.TimeZoneHour > 0 then
      M := M - Value.TimeZoneHour * MinsPerHour - Value.TimeZoneMinute
    else
      M := M - Value.TimeZoneHour * MinsPerHour + Value.TimeZoneMinute;

    if M < 0 then
    begin
      Date := IncDay(Date, -1);
      M := M + MinsPerDay;
    end
    else if M >= MinsPerDay then
    begin
      Date := IncDay(Date);
      M := M - MinsPerDay;
    end;

    DecodeDate(Date, Y, Result.Month, Result.Day);
    Result.Year := Y;
    Result.Hour := M div MinsPerHour;
    Result.Minute := M mod MinsPerHour;
  end;
end;

procedure DateTimeToString(var Result: string; const Format: string;
  const TimeStamp: TSQLTimeStamp; const AFormatSettings: TFormatSettings);
var
  BufPos, AppendLevel: Integer;
  Buffer: array[0..255] of Char;
  DynBuffer: array of Char;

  procedure AppendChars(P: PChar; Count: Integer);
  var
    N, I: Integer;
  begin
    N := SizeOf(Buffer) div SizeOf(Char);
    N := N - BufPos;
    if Count > N then
    begin
      I := Length(DynBuffer);
      SetLength(DynBuffer, I + BufPos + Count);
      if BufPos > 0 then
      begin
        Move(Buffer[0], DynBuffer[I], BufPos * SizeOf(Char));
        Inc(I, BufPos);
      end;
      Move(P[0], DynBuffer[I], Count * SizeOf(Char));
      BufPos := 0;
    end
    else if Count > 0 then
    begin
      Move(P[0], Buffer[BufPos], Count * SizeOf(Char));
      Inc(BufPos, Count);
    end;
  end;

  procedure AppendString(const S: string);
  begin
    AppendChars(Pointer(S), Length(S));
  end;

  procedure AppendNumber(Number, Digits: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
  begin
    AppendChars(NumBuf, FormatBuf(NumBuf, Length(NumBuf), Format,
      Length(Format), [Digits, Number]));
  end;

  procedure AppendNumberWithScale(Number, Scale: Integer);
  const
    Format: array[0..3] of Char = '%.*d';
  var
    NumBuf: array[0..15] of Char;
    Len: integer;
  begin
    Len := FormatBuf(NumBuf, Length(NumBuf), Format, Length(Format), [Scale, Number]);
    if Len > Scale then
      Len := Scale;
    AppendChars(NumBuf, Len);
  end;

  procedure AppendFormat(Format: PChar);
  var
    Starter, Token, LastToken: Char;
    DateDecoded, Use12HourClock, BetweenQuotes: Boolean;
    P: PChar;
    Count: Integer;
    DateTime: TDateTime;
    H: Word;

    procedure GetCount;
    var
      P: PChar;
    begin
      P := Format;
      while Format^ = Starter do Inc(Format);
      Count := Format - P + 1;
    end;

    function GetDateTime: TDateTime;
    begin
      if not DateDecoded then begin
        DateTime := EncodeDate(TimeStamp.Year, TimeStamp.Month, TimeStamp.Day);
        DateDecoded := True;
      end;
      Result := DateTime;
    end;

  {$IFNDEF VER14P}
    procedure CountChars(const S: string; MaxChars: Integer; var CharCount, ByteCount: Integer);
    var
      C, L, I: Integer;
    begin
      L := Length(S);
      C := 1;
      I := 1;
      while (I < L) and (C < MaxChars) do
      begin
        Inc(C);
        if CharInSet(S[I], LeadBytes) then
        begin
          Inc(I, 2); //Jump the trailing surrogate
          if I > L then
          begin
            Dec(C);
            Dec(I);
          end;
        end
        else
          Inc(I);
      end;
      if (C = MaxChars) and (I < L) and CharInSet(S[I], LeadBytes) then
        I := NextCharIndex(S, I) - 1;

      CharCount := C;
      ByteCount := I;
    end;

    function CharToElementLen(const S: string; MaxLen: Integer): Integer;
    var
      Chars: Integer;
    begin
      Result := 0;
      if MaxLen <= 0 then Exit;
      if MaxLen > Length(S) then MaxLen := Length(S);
      CountChars(S, MaxLen, Chars, Result);
      if Result > Length(S) * SizeOf(Char) then
        Result := Length(S) * SizeOf(Char);
    end;

    function ElementToCharIndex(const S: string; Index: Integer): Integer;
    var
      I: Integer;
    begin
      Result := 0;
      if (Index <= 0) or (Index > Length(S)) then
        Exit;
      I := 1;
      while I <= Index do
      begin
        if CharInSet(S[I], LeadBytes) then
          I := NextCharIndex(S, I)
        else
          Inc(I);
        Inc(Result);
      end;
    end;

    function ElementToCharLen(const S: string; MaxLen: Integer): Integer;
    begin
      if Length(S) < MaxLen then MaxLen := Length(S);
      Result := ElementToCharIndex(S, MaxLen);
    end;

    function CharToElementIndex(const S: string; Index: Integer): Integer;
    var
      Chars: Integer;
    begin
      Result := 0;
      if (Index <= 0) or (Index > Length(S)) then
        Exit;
      CountChars(S, Index-1, Chars, Result);
      if (Chars < (Index-1)) or ((Result >= Length(S)) and
         not ((Result = 1) and (Result <= Length(S)))) then
        Result := 0  // Char index out of range
      else if (Index > 1) then
        Inc(Result);
    end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    function ConvertEraString(const Count: Integer) : string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
      P: PChar;
    begin
      Result := '';
      SystemTime.wYear  := TimeStamp.Year;
      SystemTime.wMonth := TimeStamp.Month;
      SystemTime.wDay   := TimeStamp.Day;

      FormatStr := 'gg';
      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if Count = 1 then
        begin
          case SysLocale.PriLangID of
            LANG_JAPANESE:
              Result := Copy(Result, 1, CharToElementLen(Result, 1));
            LANG_CHINESE:
              if (SysLocale.SubLangID = SUBLANG_CHINESE_TRADITIONAL)
                and (ElementToCharLen(Result, Length(Result)) = 4) then
              begin
                P := Buffer + CharToElementIndex(Result, 3) - 1;
                SetString(Result, P, CharToElementLen(P, 2));
              end;
          end;
        end;
      end;
    end;

    function ConvertYearString(const Count: Integer): string;
    var
      FormatStr: string;
      SystemTime: TSystemTime;
      Buffer: array[Byte] of Char;
    begin
      Result := '';
      SystemTime.wYear  := TimeStamp.Year;
      SystemTime.wMonth := TimeStamp.Month;
      SystemTime.wDay   := TimeStamp.Day;

      if Count <= 2 then
        FormatStr := 'yy' // avoid Win95 bug.
      else
        FormatStr := 'yyyy';

      if GetDateFormat(GetThreadLocale, DATE_USE_ALT_CALENDAR, @SystemTime,
        PChar(FormatStr), Buffer, SizeOf(Buffer)) <> 0 then
      begin
        Result := Buffer;
        if (Count = 1) and (Result[1] = '0') then
          Result := Copy(Result, 2, Length(Result)-1);
      end;
    end;
  {$ENDIF MSWINDOWS}

  {$IFDEF POSIX}
    {$IFDEF MACOS}
      function CFStringRefAsString(Value: CFStringRef; Release: Boolean): string;
      var
        Range: CFRange;
        Tmp: TCharArray;
      begin
        if Value = nil then Exit('');
        try
          Range := CFRangeMake(0, CFStringGetLength(Value));
          if Range.Length > 0 then
          begin
            SetLength(Tmp, Range.Length);
            CFStringGetCharacters(Value, Range, PWideChar(Tmp));
            SetLength(Result, System.Length(Tmp));
            Move(Tmp[0], PChar(PChar(Result))^, System.Length(Tmp) * SizeOf(Char));
          end
          else
            Result := EmptyStr;
        finally
          if Release then
            CFRelease(Value);
        end;
      end;
    {$ELSE}
      function FindEra(Date: Integer): Byte;
      var
        I: Byte;
      begin
        Result := 0;
        for I := High(AFormatSettings.EraInfo) downto Low(AFormatSettings.EraInfo) do
        begin
          if (AFormatSettings.EraInfo[I].EraStart <= Date) then
            Exit(I);
        end;
      end;
    {$ENDIF !MACOS}

    function ConvertEraString(const Count: Integer) : String;
    var
      {$IFDEF MACOS}
      Formatter: CFDateFormatterRef;
      LDate: CFGregorianDate;
      FormatString: CFStringRef;
      Temp: CFStringRef;
      DefaultTZ: CFTimeZoneRef;
      Locale: CFLocaleRef;
      {$ELSE !MACOS}
      I: Byte;
      {$ENDIF MACOS}
    begin
      Result := '';
      {$IFDEF MACOS}
      Locale := nil;
      DefaultTZ := nil;
      Formatter := nil;
      FormatString := nil;

      try
        Locale := CFLocaleCopyCurrent;
        DefaultTZ := CFTimeZoneCopyDefault;
        Formatter := CFDateFormatterCreate(kCFAllocatorDefault, Locale,
                        kCFDateFormatterFullStyle, kCFDateFormatterNoStyle);
        FormatString := CFStringCreateWithCharacters(kCFAllocatorDefault, PChar('GG'), Length('GG'));

        CFDateFormatterSetFormat(Formatter, FormatString);
        LDate.year := TimeStamp.Year;
        LDate.month := ShortInt(TimeStamp.Month);
        LDate.day := ShortInt(TimeStamp.Day);
        LDate.hour := 0; LDate.minute := 0; LDate.second := 0;
        Temp := CFDateFormatterCreateStringWithAbsoluteTime(
          kCFAllocatorDefault, Formatter, CFGregorianDateGetAbsoluteTime(LDate, DefaultTZ));
        Result := CFStringRefAsString(Temp, True);
      finally
        if FormatString <> nil then
          CFRelease(FormatString);
        if Formatter <> nil then
          CFRelease(Formatter);
        if DefaultTZ <> nil then
          CFRelease(DefaultTZ);
        if Locale <> nil then
          CFRelease(Locale);
      end;
      {$ELSE !MACOS}
      I := FindEra(Trunc(GetDateTime));
      if I > 0 then
        Result := AFormatSettings.EraInfo[I].EraName;
      {$ENDIF MACOS}
    end;

    function ConvertYearString(const Count: Integer) : String;
    var
      S: string;
      function GetEraOffset: integer;
      {$IFDEF MACOS}
      var
        StartEra, TargetDate, LengthEra: CFAbsoluteTime;
        LDate: CFGregorianDate;
        Calendar, CurrentCalendar: CFCalendarRef;
        TimeZone: CFTimeZoneRef;
      {$ENDIF MACOS}
      begin
        {$IFDEF MACOS}
        Result := 0;
        TimeZone := nil;
        CurrentCalendar := nil;
        Calendar := nil;
        try
          LDate.year := TimeStamp.Year;
          LDate.month := ShortInt(TimeStamp.Month);
          LDate.day := ShortInt(TimeStamp.Day);
          LDate.hour := 0; LDate.minute := 0; LDate.second := 0;
          TimeZone := CFTimeZoneCopyDefault;
          TargetDate := CFGregorianDateGetAbsoluteTime(LDate, TimeZone);
          CurrentCalendar := CFCalendarCopyCurrent;
          Calendar := CFCalendarCreateWithIdentifier(kCFAllocatorDefault,
                                  CFCalendarGetIdentifier(CurrentCalendar));
          if CFCalendarGetTimeRangeOfUnit(Calendar, kCFCalendarUnitEra,
                                          TargetDate, @StartEra, @LengthEra) then
          begin
            LDate := CFAbsoluteTimeGetGregorianDate(StartEra, TimeZone);
            Result := LDate.Year - 1;
          end;
        finally
          if CurrentCalendar <> nil then
            CFRelease(CurrentCalendar);
          if Calendar <> nil then
            CFRelease(Calendar);
          if TimeZone <> nil then
            CFRelease(TimeZone);
        end;
        {$ELSE !MACOS}
        Result := FindEra(Trunc(GetDateTime));
        if Result > 0 then
          Result := AFormatSettings.EraInfo[Result].EraOffset;
        {$ENDIF MACOS}
      end;
    begin
      S := IntToStr(TimeStamp.Year - GetEraOffset);
      while Length(S) < Count do
        S := '0' + S;
      if Length(S) > Count then
        S := Copy(S, Length(S) - (Count - 1), Count);
      Result := S;
    end;
  {$ENDIF POSIX}

  begin
    if (Format <> nil) and (AppendLevel < 2) then
    begin
      Inc(AppendLevel);
      LastToken := ' ';
      DateDecoded := False;
      Use12HourClock := False;
      while Format^ <> #0 do
      begin
        Starter := Format^;
        if {$IFDEF VER14P}IsLeadChar(Starter){$ELSE}CharInSet(Starter, LeadBytes){$ENDIF} then
        begin
          AppendChars(Format, StrCharLength(Format) div SizeOf(Char));
        {$IFNDEF FPC}
          Format := StrNextChar(Format);
        {$ELSE}
          Inc(Format);
        {$ENDIF}
          LastToken := ' ';
          Continue;
        end;
      {$IFNDEF FPC}
        Format := StrNextChar(Format);
      {$ELSE}
        Inc(Format);
      {$ENDIF}
        Token := Starter;
        if CharInSet(Token, ['a'..'z']) then Dec(Token, 32);
        if CharInSet(Token, ['A'..'Z']) then
        begin
          if (Token = 'M') and (LastToken = 'H') then Token := 'N';
          LastToken := Token;
        end;
        case Token of
          'Y':
            begin
              GetCount;
              if Count <= 2 then
                AppendNumber(TimeStamp.Year mod 100, 2) else
                AppendNumber(TimeStamp.Year, 4);
            end;
        {$IFNDEF FPC}
          'G':
            begin
              GetCount;
              AppendString(ConvertEraString(Count));
            end;
          'E':
            begin
              GetCount;
              AppendString(ConvertYearString(Count));
            end;
        {$ENDIF}
          'M':
            begin
              GetCount;
              case Count of
                1, 2: AppendNumber(TimeStamp.Month, Count);
                3: AppendString(AFormatSettings.ShortMonthNames[TimeStamp.Month]);
              else
                AppendString(AFormatSettings.LongMonthNames[TimeStamp.Month]);
              end;
            end;
          'D':
            begin
              GetCount;
              case Count of
                1, 2:
                  AppendNumber(TimeStamp.Day, Count);
                3:
                  AppendString(AFormatSettings.ShortDayNames[DayOfWeek(GetDateTime)]);
                4:
                  AppendString(AFormatSettings.LongDayNames[DayOfWeek(GetDateTime)]);
                5: AppendFormat(Pointer(AFormatSettings.ShortDateFormat));
              else
                AppendFormat(Pointer(AFormatSettings.LongDateFormat));
              end;
            end;
          'H':
            begin
              GetCount;
              BetweenQuotes := False;
              P := Format;
              while P^ <> #0 do
              begin
                if {$IFDEF VER14P}IsLeadChar(P^){$ELSE}CharInSet(P^, LeadBytes){$ENDIF} then
                begin
                  P := StrNextChar(P);
                  Continue;
                end;
                case P^ of
                  'A', 'a':
                    if not BetweenQuotes then
                    begin
                      if ( (StrLIComp(P, 'AM/PM', 5) = 0)
                        or (StrLIComp(P, 'A/P',   3) = 0)
                        or (StrLIComp(P, 'AMPM',  4) = 0) ) then
                        Use12HourClock := True;
                      Break;
                    end;
                  'H', 'h':
                    Break;
                  '''', '"': BetweenQuotes := not BetweenQuotes;
                end;
                Inc(P);
              end;
              H := TimeStamp.Hour;
              if Use12HourClock then
                if H = 0 then H := 12 else if H > 12 then Dec(H, 12);
              if Count > 2 then Count := 2;
              AppendNumber(H, Count);
            end;
          'N':
            begin
              GetCount;
              if Count > 2 then Count := 2;
              AppendNumber(TimeStamp.Minute, Count);
            end;
          'S':
            begin
              GetCount;
              if Count > 2 then Count := 2;
              AppendNumber(TimeStamp.Second, Count);
            end;
          'T':
            begin
              GetCount;
              if Count = 1 then
                AppendFormat(Pointer(AFormatSettings.ShortTimeFormat)) else
                AppendFormat(Pointer(AFormatSettings.LongTimeFormat));
            end;
          'Z':
            begin
              GetCount;
              if Count > 7 then Count := 7;
              AppendNumberWithScale(TimeStamp.Fractions, Count);
            end;
          'A':
            begin
              P := Format - 1;
              if StrLIComp(P, 'AM/PM', 5) = 0 then
              begin
                if TimeStamp.Hour >= 12 then Inc(P, 3);
                AppendChars(P, 2);
                Inc(Format, 4);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'A/P', 3) = 0 then
              begin
                if TimeStamp.Hour >= 12 then Inc(P, 2);
                AppendChars(P, 1);
                Inc(Format, 2);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AMPM', 4) = 0 then
              begin
                if TimeStamp.Hour < 12 then
                  AppendString(AFormatSettings.TimeAMString) else
                  AppendString(AFormatSettings.TimePMString);
                Inc(Format, 3);
                Use12HourClock := TRUE;
              end else
              if StrLIComp(P, 'AAAA', 4) = 0 then
              begin
                AppendString(AFormatSettings.LongDayNames[DayOfWeek(GetDateTime)]);
                Inc(Format, 3);
              end else
              if StrLIComp(P, 'AAA', 3) = 0 then
              begin
                AppendString(AFormatSettings.ShortDayNames[DayOfWeek(GetDateTime)]);
                Inc(Format, 2);
              end else
              AppendChars(@Starter, 1);
            end;
          'C':
            begin
              GetCount;
              AppendFormat(Pointer(AFormatSettings.ShortDateFormat));
              if (TimeStamp.Hour <> 0) or (TimeStamp.Minute <> 0) or (TimeStamp.Second <> 0) or (TimeStamp.Fractions <> 0) then
              begin
                AppendChars(' ', 1);
                AppendFormat(Pointer(AFormatSettings.LongTimeFormat));
              end;
            end;
          '/':
            if AFormatSettings.DateSeparator <> #0 then
              AppendChars(@AFormatSettings.DateSeparator, 1);
          ':':
            if AFormatSettings.TimeSeparator <> #0 then
              AppendChars(@AFormatSettings.TimeSeparator, 1);
          '''', '"':
            begin
              P := Format;
              while (Format^ <> #0) and (Format^ <> Starter) do
              begin
                if {$IFDEF VER14P}IsLeadChar(Format^){$ELSE}CharInSet(Format^, LeadBytes){$ENDIF} then
                {$IFNDEF FPC}
                  Format := StrNextChar(Format)
                {$ELSE}
                  Inc(Format)
                {$ENDIF}
                else
                  Inc(Format);
              end;
              AppendChars(P, Format - P);
              if Format^ <> #0 then Inc(Format);
            end;
        else
          AppendChars(@Starter, 1);
        end;
      end;
      Dec(AppendLevel);
    end;
  end;

begin
  BufPos := 0;
  AppendLevel := 0;
  if Format <> '' then
    AppendFormat(Pointer(Format))
  else
    AppendFormat('C');

  SetLength(Result, Length(DynBuffer) + BufPos);

  if Length(DynBuffer) > 0 then begin
    Move(DynBuffer[0], Result[1], Length(DynBuffer) * SizeOf(Char));
    if BufPos > 0 then
      Move(Buffer[0], Result[Length(DynBuffer) + 1], BufPos * SizeOf(Char));
  end
  else
    Move(Buffer[0], Result[1], BufPos * SizeOf(Char));
end;

{$IFNDEF VER7P}
function AnsiMidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
begin
  Result := MidStr(AText, AStart, ACount);
end;
{$ENDIF}

function ExtractMSecFromString(const S: string; const AFormatSettings: TFormatSettings; Scale: Integer): cardinal;
var
  DT: TDateTime;
  Hour, Min, Sec, Frac: Word;
  CurPos: Integer;
  Pattern, TimeStr: string;
  NextChar: Char;
  i: integer;
begin
  Result := 0;

  if TryStrToDateTime(S, DT{$IFDEF VER7P}, FormatSettings{$ENDIF}) then begin
    DecodeTime(DT, Hour, Min, Sec, Frac);

    Pattern := IntToStr(Hour)+AFormatSettings.TimeSeparator;
    CurPos := Pos(Pattern, S);
    if (CurPos = 0) and (Hour > 12) then begin
      Dec(Hour, 12);
      Pattern := IntToStr(Hour)+AFormatSettings.TimeSeparator;
      CurPos := Pos(Pattern, S);
    end;
    if CurPos = 0 then
      Exit;
    TimeStr := AnsiMidStr(S, CurPos+Length(Pattern)-1, MaxInt);
    Pattern := IntToStr(Min)+AFormatSettings.TimeSeparator;
    CurPos := Pos(Pattern, TimeStr);
    if CurPos = 0 then
      Exit;
    TimeStr := AnsiMidStr(TimeStr, CurPos+Length(Pattern)-1, MaxInt);
    Pattern := AFormatSettings.TimeSeparator+IntToStr(Sec);
    CurPos := Pos(Pattern, TimeStr);
    if (CurPos = 0) and (Sec < 10) then begin
      Pattern := AFormatSettings.TimeSeparator+'0'+IntToStr(Sec);
      CurPos := Pos(Pattern, TimeStr);
    end
    else if (CurPos = 1) and (Sec = 0) then begin
      if Pos(Pattern+'0', TimeStr) = 1 then
        Pattern := Pattern+'0';
    end;
    if CurPos = 0 then
      Exit;
    TimeStr := AnsiMidStr(TimeStr, CurPos+Length(Pattern)+1, MaxInt);

    Pattern := '';
    for i := 1 to Length(TimeStr) do begin
      NextChar := TimeStr[i];
      if (NextChar < '0') or (NextChar > '9') then
        break;
      Pattern := Pattern + NextChar;
    end;

    if Pattern <> '' then begin
      while Length(Pattern) < Scale do
        Pattern := Pattern + '0';
      Result := StrToInt(Pattern);
    end;
  end;
end;

{$IFNDEF VER14P}
{$IFDEF FPC}

procedure CheckSqlTimeStamp(const ASQLTimeStamp: TSQLTimeStamp);
begin  // only check if not an empty timestamp
  if ASQLTimeStamp.Year + ASQLTimeStamp.Month + ASQLTimeStamp.day +
     ASQLTimeStamp.Hour + ASQLTimeStamp.Minute + ASQLTimeStamp.Second > 0 then
  begin
    if ASQLTimeStamp.Year + ASQLTimeStamp.Month + ASQLTimeStamp.Day > 0 then
      if (ASQLTimeStamp.Year = 0) or (ASQLTimeStamp.Month = 0) or
       (ASQLTimeStamp.Day =0) or (ASQLTimeStamp.Month > 31) or (ASQLTimeStamp.Day >
       DaysInAMonth(ASQLTimeStamp.Year,ASQLTimeStamp.Month)) then
         raise EConvertError.Create(SErrInvalidTimeStamp);
    if ASQLTimeStamp.Hour + ASQLTimeStamp.Minute + ASQLTimeStamp.Second > 0 then
       if (ASQLTimeStamp.Hour > 23) or (ASQLTimeStamp.Second > 59) or
       (ASQLTimeStamp.Minute > 59) then
         raise EConvertError.Create(SInvalidSQLTimeStamp);
  end;
end;

function DateTimeToSQLTimeStamp(const DateTime: TDateTime): TSQLTimeStamp;
var
  F: Word;
begin
  DecodeDate(DateTime, Result.Year, Result.Month, Result.Day);
  DecodeTime(DateTime, Result.Hour, Result.Minute, Result.Second, F);
  Result.Fractions := F;
end;

function SQLTimeStampToDateTime(const DateTime: TSQLTimeStamp): TDateTime;
begin
  if IsSQLTimeStampBlank(DateTime) then
    Result := 0
  else
  begin
    Result := EncodeDate(DateTime.Year, DateTime.Month, DateTime.Day);
    if Result >= 0 then
      Result := Result + EncodeTime(DateTime.Hour, DateTime.Minute, DateTime.Second, DateTime.Fractions)
    else
      Result := Result - EncodeTime(DateTime.Hour, DateTime.Minute, DateTime.Second, DateTime.Fractions);
  end;
end;

function IsSqlTimeStampValid(const ts: TSQLTimeStamp): Boolean;
begin
  if (ts.Month > 12) or (ts.Day > DaysInAMonth(ts.Year, ts.Month)) or
     (ts.Hour > 23) or (ts.Minute > 59) or (ts.Second > 59)
  then
    Result := False
  else
    Result := True;
end;

function TryStrToSQLTimeStamp(const S: string; var TimeStamp: TSQLTimeStamp; Scale: integer = 3): Boolean;
var
  DT: TDateTime;
begin
  if S = '' then begin
    Result := False;
    Exit;
  end;

  Result := TryStrToDateTime(S, DT);

  if Result then begin
    TimeStamp := DateTimeToSQLTimeStamp(DT);
    if TimeStamp.Fractions = 0 then
      TimeStamp.Fractions := ExtractMSecFromString(S, FormatSettings, Scale);

    Result := IsSqlTimeStampValid(TimeStamp);
  end;

  if not Result then
    TimeStamp := NullSQLTimeStamp;
end;

function StrToSQLTimeStamp(const S: string; Scale: integer = 3): TSQLTimeStamp;
const
  SCouldNotParseTimeStamp = 'Could not parse SQL TimeStamp string';
begin
  if not TryStrToSqlTimeStamp(S, Result, Scale) then
    raise EConvertError.Create(SCouldNotParseTimeStamp);
end;

function VarSQLTimeStamp: TVarType;
begin
  Result := SQLTimeStampVariantType.VarType;
end;

function VarSQLTimeStampCreate: Variant;
begin
  VarSQLTimeStampCreate(Result, NullSQLTimeStamp);
end;

function VarSQLTimeStampCreate(const ASQLTimeStamp: TSQLTimeStamp): Variant;
begin
  VarSQLTimeStampCreate(Result, ASQLTimeStamp);
end;

procedure VarSQLTimeStampCreate(var aDest: Variant; const ASQLTimeStamp: TSQLTimeStamp);
begin
  VarClear(aDest);
  TSQLTimeStampVarData(aDest).VType := SQLTimeStampVariantType.VarType;
  TSQLTimeStampVarData(aDest).VTimeStamp := TSQLTimeStampData.Create(ASQLTimeStamp);
end;

function VarIsSQLTimeStamp(const aValue: Variant): Boolean;
begin
  Result := TVarData(aValue).VType = SQLTimeStampVariantType.VarType;
end;

function VarToSQLTimeStamp(const aValue: Variant): TSQLTimeStamp;
begin
  if TVarData(aValue).VType in [varNULL, varEMPTY] then
    Result := NullSqlTimeStamp
  else if (TVarData(aValue).VType = varDouble) or (TVarData(aValue).VType = varDate) then
    Result := DateTimeToSqlTimeStamp(TDateTime(aValue))
  else if (TVarData(aValue).VType = SQLTimeStampVariantType.VarType) then
    Result := TSQLTimeStampData(TSQLTimeStampVarData(aValue).VTimeStamp).FTimeStamp
  else
    raise EVariantError.Create(SInvalidVarCast);
end;

{$ENDIF}

procedure CheckSqlTimeStampOffset(const ASQLTimeStampOffset: TSQLTimeStampOffset);
begin  // only check if not an empty timestamp
  if ASQLTimeStampOffset.Year + ASQLTimeStampOffset.Month + ASQLTimeStampOffset.day +
     ASQLTimeStampOffset.Hour + ASQLTimeStampOffset.Minute + ASQLTimeStampOffset.Second +
     Abs(ASQLTimeStampOffset.TimeZoneHour) + ASQLTimeStampOffset.TimeZoneMinute > 0 then
  begin
    if ASQLTimeStampOffset.Year + ASQLTimeStampOffset.Month + ASQLTimeStampOffset.Day > 0 then
      if (ASQLTimeStampOffset.Year = 0) or (ASQLTimeStampOffset.Month = 0) or
       (ASQLTimeStampOffset.Day =0) or (ASQLTimeStampOffset.Month > 12) or (ASQLTimeStampOffset.Day >
       DaysInAMonth(ASQLTimeStampOffset.Year,ASQLTimeStampOffset.Month)) then
         raise EConvertError.Create(SInvalidSQLTimeStamp);
    if ASQLTimeStampOffset.Hour + ASQLTimeStampOffset.Minute + ASQLTimeStampOffset.Second +
       Abs(ASQLTimeStampOffset.TimeZoneHour) + ASQLTimeStampOffset.TimeZoneMinute > 0 then
       if (ASQLTimeStampOffset.Hour > 23) or (ASQLTimeStampOffset.Second > 59) or
       (ASQLTimeStampOffset.Minute > 59) or (abs(ASQLTimeStampOffset.TimeZoneHour) > 14) or
       ((abs(ASQLTimeStampOffset.TimeZoneHour) = 14) and (ASQLTimeStampOffset.TimeZoneMinute > 0)) or
       (ASQLTimeStampOffset.TimeZoneMinute > 59) then
         raise EConvertError.Create(SInvalidSQLTimeStamp);
  end;
end;

function DateTimeToSQLTimeStampOffset(const DateTime: TDateTime): TSQLTimeStampOffset;
var
  tz: Integer;
  TZOffsetHour: Integer;
  TZOffsetMinute: Integer;
begin
  tz := GetLocalTimeZoneOffset;
  TZOffsetHour := tz div 60;
  if tz >= 0 then
    TZOffsetMinute := tz mod 60
  else
    TZOffsetMinute := -tz mod 60;

  Result := DateTimeToSQLTimeStampOffset(DateTime, TZOffsetHour, TZOffsetMinute);
end;

function DateTimeToSQLTimeStampOffset(const DateTime: TDateTime; const TZOffsetHour, TZOffsetMinute: Integer): TSQLTimeStampOffset; overload;
var
  TLocal: TSQLTimeStamp;
begin
  TLocal := DateTimeToSQLTimeStamp(DateTime);

  Result.Year := TLocal.Year;
  Result.Month := TLocal.Month;
  Result.Day := TLocal.Day;
  Result.Hour := TLocal.Hour;
  Result.Minute := TLocal.Minute;
  Result.Second := TLocal.Second;
  Result.Fractions := TLocal.Fractions;
  Result.TimeZoneHour := TZOffsetHour;
  Result.TimeZoneMinute := TZOffsetMinute;
end;

function VarSQLTimeStampOffset: TVarType;
begin
  Result := SQLTimeStampOffsetVariantType.VarType;
end;

function VarSQLTimeStampOffsetCreate: Variant;
begin
  VarSQLTimeStampOffsetCreate(Result, NullSQLTimeStampOffset);
end;

function VarSQLTimeStampOffsetCreate(const ASQLTimeStampOffset: TSQLTimeStampOffset): Variant;
begin
  VarSQLTimeStampOffsetCreate(Result, ASQLTimeStampOffset);
end;

procedure VarSQLTimeStampOffsetCreate(var aDest: Variant; const ASQLTimeStampOffset: TSQLTimeStampOffset);
begin
  VarClear(aDest);
  TSQLTimeStampOffsetVarData(aDest).VType := SQLTimeStampOffsetVariantType.VarType;
  TSQLTimeStampOffsetVarData(aDest).VTimeStampOffset := TSQLTimeStampOffsetData.Create(ASQLTimeStampOffset);
end;

function GetSQLTimeStampOffsetFromStringVariant(const aValue: Variant): TSQLTimeStampOffset;
var
  Data: TSQLTimeStampOffsetData;
begin
  Data := TSQLTimeStampOffsetData.Create(string(aValue));
  try
    Result := Data.FTimeStampOffset;
  finally
    Data.Free;
  end;
end;

function VarIsSQLTimeStampOffset(const aValue: Variant): Boolean;
begin
  Result := TVarData(aValue).VType = SQLTimeStampOffsetVariantType.VarType;
end;

function VarToSQLTimeStampOffset(const aValue: Variant): TSQLTimeStampOffset;
begin
  if TVarData(aValue).VType in [varNULL, varEMPTY] then
    Result := NullSqlTimeStampOffset
  else if (TVarData(aValue).VType = varString){$IFDEF VER12P} or (TVarData(aValue).VType = varUString){$ENDIF} or
          (TVarData(aValue).VType = varOleStr) then
    Result := GetSQLTimeStampOffsetFromStringVariant(aValue)
  else if (TVarData(aValue).VType = varDouble) or (TVarData(aValue).VType = varDate) then
    Result := DateTimeToSqlTimeStampOffset(TDateTime(aValue))
  else if (TVarData(aValue).VType = SQLTimeStampOffsetVariantType.VarType) then
    Result := TSQLTimeStampOffsetData(TSQLTimeStampOffsetVarData(aValue).VTimeStampOffset).FTimeStampOffset
  else
    raise EVariantError.Create(SInvalidVarCast);
end;

{$ENDIF}

{$IFNDEF VER14P}
{$IFDEF FPC}

{ TSQLTimeStampData }

constructor TSQLTimeStampData.Create(const ASQLTimeStamp: TSQLTimeStamp);
begin
  CheckSqlTimeStamp( ASQLTimeStamp );
  inherited Create;
  move(ASQLTimeStamp, FTimeStamp, sizeof(TSQLTimeStamp));
end;

constructor TSQLTimeStampData.Create(const ASource: TSQLTimeStampData);
begin
  Create(ASource.FTimeStamp);
end;

{ TSQLTimeStampVariantType }

function TSQLTimeStampVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TSQLTimeStampVarData(V).VTimeStamp;
end;

procedure TSQLTimeStampVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TSQLTimeStampVarData(V).VTimeStamp);
end;

procedure TSQLTimeStampVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TSQLTimeStampVarData(Dest) do
    begin
      VType := VarType;
      VTimeStamp := TSQLTimeStampData.Create(TSQLTimeStampVarData(Source).VTimeStamp);
    end;
end;

procedure TSQLTimeStampVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);
var
  Status: Integer;
begin
  Status := SQLTimeStampCompare(TSQLTimeStampVarData(Left).VTimeStamp.FTimeStamp, TSQLTimeStampVarData(Right).VTimeStamp.FTimeStamp);

  if Status = 0 then
    Relationship := crEqual
  else if Status > 0 then
    Relationship := crGreaterThan
  else
    Relationship := crLessThan;
end;

{$ENDIF}

{ TSQLTimeStampOffsetData }

constructor TSQLTimeStampOffsetData.Create(const AText: string);
begin
  inherited Create;
  FTimeStampOffset := StrToSQLTimeStampOffset(AText);
end;

constructor TSQLTimeStampOffsetData.Create(const ASQLTimeStampOffset: TSQLTimeStampOffset);
begin
  CheckSqlTimeStampOffset(ASQLTimeStampOffset);
  inherited Create;
  move(ASQLTimeStampOffset, FTimeStampOffset, sizeof(TSQLTimeStampOffset));
end;

constructor TSQLTimeStampOffsetData.Create(const ASource: TSQLTimeStampOffsetData);
begin
  Create(ASource.FTimeStampOffset);
end;

{ TSQLTimeStampOffsetVariantType }

function TSQLTimeStampOffsetVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TSQLTimeStampOffsetVarData(V).VTimeStampOffset;
end;

procedure TSQLTimeStampOffsetVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TSQLTimeStampOffsetVarData(V).VTimeStampOffset);
end;

procedure TSQLTimeStampOffsetVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
  begin
    TSQLTimeStampOffsetVarData(Dest).VType := VarType;
    TSQLTimeStampOffsetVarData(Dest).VTimeStampOffset := TSQLTimeStampOffsetData.Create(TSQLTimeStampOffsetVarData(Source).VTimeStampOffset);
  end;
end;

procedure TSQLTimeStampOffsetVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);
var
  Status: Integer;
begin
  Status := SQLTimeStampOffsetCompare(TSQLTimeStampOffsetVarData(Left).VTimeStampOffset.FTimeStampOffset, TSQLTimeStampOffsetVarData(Right).VTimeStampOffset.FTimeStampOffset);

  if Status = 0 then
    Relationship := crEqual
  else if Status > 0 then
    Relationship := crGreaterThan
  else
    Relationship := crLessThan;
end;

{$ENDIF}

{$IFNDEF VER14P}
initialization
{$IFDEF FPC}
  SQLTimeStampVariantType := TSQLTimeStampVariantType.Create;
{$ENDIF}
  SQLTimeStampOffsetVariantType := TSQLTimeStampOffsetVariantType.Create;

finalization
{$IFDEF FPC}
  FreeAndNil(SQLTimeStampVariantType);
{$ENDIF}
  FreeAndNil(SQLTimeStampOffsetVariantType);
{$ENDIF}

end.

