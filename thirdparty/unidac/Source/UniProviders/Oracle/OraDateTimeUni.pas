
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  OCIDatetime
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraDateTimeUni;

interface

uses
  SysUtils,
  CRTypes,
{$IFNDEF UNIDACPRO}
  OraCall;
{$ELSE}
  OraCallUni;
{$ENDIF}

const
  SNotMatchLength = 'ORA-01862 the numeric value does not match the length of the format item';
  SFormatNotRecognized = 'ORA-01821 date format not recognized';
  SNonNnumericCharacter = 'ORA-01858 a non-numeric character was found where a numeric was expected';
  SNotLongEnough = 'ORA-01840 input value not long enough for date format';
  SWrongTZH = 'ORA-01874 time zone hour must be between -12 and 13';
  SWrongTZM = 'ORA-01875 time zone minute must be between -59 and 59';
  SWrongMinute = 'ORA-01851 minutes must be between 0 and 59';
  SWrongSecond = 'ORA-01852 seconds must be between 0 and 59';
  SWrongHour12 = 'ORA-01850 hour must be between 0 and 23';
  SWrongHour24 = 'ORA-01849 hour must be between 1 and 12';
  SWrongYear = 'ORA-01841 (full) year must be between -4713 and +9999, and not be 0';
  SWrongDayOfYear = 'ORA-01848 day of year must be between 1 and 365 (366 for leap year)';
  SWrongDayOfMonth = 'ORA-01847 day of month must be between 1 and last day of month';
  SNotValidMonth = 'ORA-01843 not a valid month';
  SNotValidDayOfWeek = 'ORA-01846 not a valid day of the week';
  SWrongAMPM = 'ORA-01855 AM/A.M. or PM/P.M. required';
  SWrongBCAD = 'ORA-01856 BC/B.C. or AD/A.D. required';
  SWrongLiteral = 'ORA-01861 literal does not match format string';
  SAppearsTwice = 'ORA-01810 format code appears twice';
  SWrongFSec = 'ORA-01880 the fractional seconds must be between 0 and 999999999';
  SConflictMonthJulian = 'ORA-01833 month conflicts with Julian date';
  SConflictDayJulian = 'ORA-01834 day of month conflicts with Julian date';
  SConflictWeekDayJulian = 'ORA-01835 day of week conflicts with Julian date';
  SPrecludesUseMeridian = 'ORA-01818 ''HH24'' precludes use of meridian indicator';
  SPrecludesUseBCAD = 'ORA-01819 signed year precludes use of BC/AD';
  SInvalidDay = 'ORA-01839 date not valid for month specified';
  SFieldNotFound = 'ORA-01878 specified field not found in datetime or interval';
  SDatetimesIncomparable = 'ORA-01870 the intervals or datetimes are not mutually comparable';

type
  TOCIDateTime = class
  private
    FValue: TBytes;
    FFracPrecision: byte;
    FDescriptorType: cardinal;
//    FServerTZHour: integer;
//    FServerTZMunute: integer;

//    function GetLangInd(Name: string): integer;
    procedure GetLocalTimeZone(var tzhour: sb1; var tzminute: sb1);
    procedure GetActualTimeZoneOffset(var hour: sb1; var minute: sb1);

    procedure InternalSetDate(year: sb2; month: ub1; day: ub1); overload;
    procedure InternalGetDate(out year: sb2; out month: ub1; out day: ub1); overload;
    procedure InternalSetTime(hour: ub1; minute: ub1; sec: ub1; fsec: ub4); overload;
    procedure InternalGetTime(out hour: ub1; out minute: ub1; out sec: ub1; out fsec: ub4); overload;
  protected
    procedure Alloc(Len: integer); virtual;
    procedure CheckAlloc(fSeconds: boolean; TimeZone: boolean);
  public
    constructor Create(DescriptorType: cardinal);

    function GetIsNull: boolean;
    procedure Assign(src: TOCIDateTime);
    function Compare(const date1: TOCIDateTime): integer;
    function Check: cardinal;

    procedure Parse(date_str: string; fmt: string);
    function ToString(fmt: string; fsprec: ub1): string; reintroduce;
    procedure Construct(year: sb2; month, day, hour, min, sec: ub1; fsec: ub4; timezone: string);

    procedure GetDate(var year: sb2; var month: ub1; var day: ub1);
    procedure SetDate(year: sb2; month: ub1; day: ub1);
    procedure GetTime(var hour: ub1; var minute: ub1; var sec: ub1; var fsec: ub4);
    procedure SetTime(hour: ub1; minute: ub1; sec: ub1; fsec: ub4);
    procedure GetTimeZoneOffset(out tzHour: sb1; out tzMinute: sb1);
    procedure SetTimeZoneOffset(tzHour: sb1; tzMinute: sb1);
    function GetTimeZoneName: string;

    property DescriptorType: Cardinal read FDescriptorType;
    property IsNull: boolean read GetIsNull;
    property Value: TBytes read FValue write FValue;
  end;

//  function GetServerLocalTimeZoneOffset(Value: TOCIDateTime): Integer;

  procedure UpdateDate(var Value: TBytes; year: sb2; month: ub1; day: ub1);
  procedure ExtractDate(const Value: TBytes; out year: sb2; out month: ub1; out day: ub1);
  procedure UpdateTime(var Value: TBytes; hour: ub1; minute: ub1; sec: ub1; fsec: ub4);
  procedure ExtractTime(const Value: TBytes; out hour: ub1; out minute: ub1; out sec: ub1; out fsec: ub4);
  procedure ChangeTimeZoneOffset(var year: sb2; var month: ub1; var day: ub1; var hour: ub1; var minute: ub1;
    TZHourMinute: integer; ToLocal: boolean);
  function GetTimeZoneTrimmedLen(var Value: TBytes; DescriptorType: cardinal): Integer;

implementation

uses
  CRFunctions, MemUtils,
{$IFNDEF UNIDACPRO}
  OraTimeZone;
{$ELSE}
  OraTimeZoneUni;
{$ENDIF}

const
  SPunctuation = ' -+/,.;:''';

//function GetServerLocalTimeZoneOffset(Value: TOCIDateTime): Integer;
//begin
//  Result := Value.FServerTZHour * 60 + Value.FServerTZMunute;
//end;

procedure UpdateDate(var Value: TBytes; year: sb2; month: ub1; day: ub1);
begin
  Value[0] := (year div 100) mod 100 + 100;
  Value[1] := year mod 100 + 100;
  Value[2] := Byte(month);
  Value[3] := Byte(day);
end;

procedure ExtractDate(const Value: TBytes; out year: sb2; out month: ub1; out day: ub1);
begin
  year  := (Value[0] - 100) * 100 +
           (Value[1] - 100);
  month := ShortInt(Value[2]);
  day   := ShortInt(Value[3]);
end;

procedure UpdateTime(var Value: TBytes; hour: ub1; minute: ub1; sec: ub1; fsec: ub4);
begin
  Value[4] := hour   + 1;
  Value[5] := minute + 1;
  Value[6] := sec    + 1;
  if High(Value) >= 7 then begin
    Value[7] := Byte(fsec shr 24);
    Value[8] := Byte(fsec shr 16);
    Value[9] := Byte(fsec shr 8);
    Value[10]:= Byte(fsec);
  end;
end;

procedure ExtractTime(const Value: TBytes; out hour: ub1; out minute: ub1; out sec: ub1; out fsec: ub4);
begin
  hour    := Value[4] - 1;
  minute  := Value[5] - 1;
  sec     := Value[6] - 1;
  if High(Value) >= 7 then
    fsec :=
      (Integer(Value[7]) shl 24) or
      (Integer(Value[8]) shl 16) or
      (Integer(Value[9]) shl 8) or
      Value[10]
  else
    fsec := 0;
end;

procedure ChangeTimeZoneOffset(var year: sb2; var month: ub1; var day: ub1; var hour: ub1; var minute: ub1;
  TZHourMinute: integer; ToLocal: boolean);
var
  TmpYear: integer;
  DateTime, DateTime1: TDateTime;
  NewYear, NewMonth, NewDay, NewHour, NewMinute, NewSecond, NewMilliSecond: Word;
begin
  TmpYear := year;
  // TmpYear must be >= 0
  while TmpYear <= 1900 do
    TmpYear := TmpYear + 4000;

  if (month = 0) and (day = 0) then begin
    month := 1;
    day := 1;
    TZHourMinute := TZHourMinute - 60 * 24;
  end;

  DateTime := EncodeDate(TmpYear, month, day);
  if DateTime >= 0 then
    DateTime := DateTime + EncodeTime(hour, minute, 0, 0)
  else
    DateTime := DateTime - EncodeTime(hour, minute, 0, 0);

  if ToLocal then
    DateTime1 := DateTime + TZHourMinute / (60 * 24)
  else
    DateTime1 := DateTime + -TZHourMinute / (60 * 24);

  DecodeDate(DateTime1, NewYear, NewMonth, NewDay);
  DecodeTime(DateTime1, NewHour, NewMinute, NewSecond, NewMilliSecond);

  year := NewYear + year - TmpYear;
  month := NewMonth;
  day := NewDay;
  hour := NewHour;
  minute := NewMinute;
end;

function GetTimeZoneTrimmedLen(var Value: TBytes; DescriptorType: cardinal): Integer;
begin
  Result := 13;
  if (Length(Value) < 13) or ((Value[11] = 0) and (Value[12] = 0) and (DescriptorType <> OCI_DTYPE_TIMESTAMP_TZ)) then
    Result := 11;
  if (Length(Value) < 11) or ((Result = 11) and (Value[7] = 0) and (Value[8] = 0) and (Value[9] = 0) and (Value[10] = 0)) then
    Result := 7;
end;

function DaysInAMonth(const AYear, AMonth: Word): Word;
const
  MonthDays: array [1..12] of word = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  if AMonth = 2 then begin
    if AYear mod 4 = 0 then
      Result := 29
    else
      Result := 28;
  end
  else
    Result := MonthDays[AMonth];
end;

function DaysInAYear(const AYear: Word): Word;
begin
  if AYear mod 4 = 0 then
    Result := 366
  else
    Result := 365;
end;

function StartOfTheYear(const AValue: TDateTime): TDateTime;
var
  LMonth, LDay, LYear: Word;
begin
  DecodeDate(AValue, LYear, LMonth, LDay);
  Result := EncodeDate(LYear, 1, 1);
end;

function DayOfTheYear(const AValue: TDateTime): Word;
begin
  Result := Trunc(AValue - StartOfTheYear(AValue)) + 1;
end;

function WeekOfTheYear(const AValue: TDateTime): Word;
var
  Value: TDateTime;
  DWeek: word;
begin
  Value := StartOfTheYear(AValue);
  DWeek := (DayOfWeek(Value) - 2) mod 7;
  if (7 - DWeek < 4) and (AValue - Value < 6 - DWeek) then begin
    Value := StartOfTheYear(AValue - 7);
    DWeek := (DayOfWeek(Value) - 2) mod 7;
  end;
  Value := Value - (DWeek + 1);
  if 7 - DWeek >= 4 then
    Value := Value - 7;
  Result := Trunc((AValue - Value) / 7);
  if Result > 52 then begin
    Value := StartOfTheYear(Value + 400);
    DWeek := (DayOfWeek(Value) - 2) mod 7;
    if (7 - DWeek >= 4) and (Value - AValue < DWeek) then
      Result := 1;
  end;
end;

function SecondOfTheDay(const AValue: TDateTime): Cardinal;
var
  LHours, LMinutes, LSeconds, LMilliSeconds: Word;
begin
  DecodeTime(AValue, LHours, LMinutes, LSeconds, LMilliSeconds);
  Result := LSeconds + (LMinutes + LHours * 60) * 60;
end;

function GetInt(date_str: string; Length_date_str: integer; var StrInd: integer;
  SubLength: integer; MinValue, MaxValue: integer; ErrorMessage: string): integer;
var
  i: integer;
  Str: string;
begin
  if (StrInd > Length_date_str ) or (date_str[StrInd] < '0') or (date_str[StrInd] > '9') then
    raise Exception.Create(SNonNnumericCharacter);
  try
    Str := '';
    for i := 0 to SubLength - 1 do begin
      if (StrInd > Length(date_str)) or (date_str[StrInd] < '0') or (date_str[StrInd] > '9') then break;
      Str := Str + date_str[StrInd];
      Inc(StrInd);
    end;
    Result := StrToInt(Str);
  except
    raise Exception.Create(SNonNnumericCharacter);
  end;
  if (MaxValue > 0) and ((Result < MinValue) or (Result > MaxValue)) then
    raise Exception.Create(ErrorMessage);
end;

constructor TOCIDateTime.Create(DescriptorType: cardinal);
begin
  inherited Create;

  FDescriptorType := DescriptorType;
  FFracPrecision := 9;

//  if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then begin
//    FServerTZHour := -7;
//    FServerTZMunute := 0;
//  end;

{  if Assigned(Value) then begin
    Alloc(High(Value) - Low(Value) + 1);
    ArrayCopy(Value, Low(Value), FValue, 0, High(FValue) + 1);
  end;}
end;

procedure TOCIDateTime.GetLocalTimeZone(var tzhour: sb1; var tzminute: sb1);
var
  Offset: integer;
begin
  Offset := GetLocalTimeZoneOffset;
  tzminute := Offset mod 60;
  tzhour := Offset div 60;
end;

procedure TOCIDateTime.GetActualTimeZoneOffset(var hour: sb1; var minute: sb1);
begin
  if FDescriptorType = OCI_DTYPE_TIMESTAMP then
    GetLocalTimeZone(hour, minute)
  else
    GetTimeZoneOffset(hour, minute);
end;

procedure TOCIDateTime.InternalSetDate(year: sb2; month: ub1; day: ub1);
begin
  CheckAlloc(False, False);
  UpdateDate(FValue, year, month, day);
end;

procedure TOCIDateTime.InternalGetDate(out year: sb2; out month: ub1; out day: ub1);
begin
  if IsNull then begin
    year  := 0;
    month := 0;
    day := 0;
  end
  else
    ExtractDate(FValue, year, month, day);
end;

procedure TOCIDateTime.InternalSetTime(hour: ub1; minute: ub1; sec: ub1; fsec: ub4);
var
  newLen: Integer;
begin
  CheckAlloc(fsec <> 0, False);

  UpdateTime(FValue, hour, minute, sec, fsec);

  newLen := GetTimeZoneTrimmedLen(FValue, FDescriptorType);
  if newLen < Length(FValue) then
    Alloc(newLen);
end;

procedure TOCIDateTime.InternalGetTime(out hour: ub1; out minute: ub1; out sec: ub1; out fsec: ub4);
begin
  if IsNull then begin
    hour   := 0;
    minute := 0;
    sec    := 0;
    fsec   := 0;
  end
  else
    ExtractTime(FValue, hour, minute, sec, fsec);
end;

procedure TOCIDateTime.Alloc(Len: integer);
begin
  SetLength(FValue, Len);
end;

procedure TOCIDateTime.CheckAlloc(fSeconds: boolean; TimeZone: boolean);
var
  Len: integer;
begin
  if TimeZone then
    Len := 13
  else if fSeconds then
    Len := 11
  else
    Len := 7;
  if IsNull or (High(FValue) + 1 < Len) then
    Alloc(Len);
end;

function TOCIDateTime.GetIsNull: boolean;
begin
  Result := not Assigned(FValue);
end;

procedure TOCIDateTime.Assign(src: TOCIDateTime);
begin
  if Self.ClassType <> src.ClassType then
    raise Exception.Create(SDatetimesIncomparable); //TODO

  if src.IsNull then
    SetLength(FValue, 0)
  else begin
    if IsNull or (High(FValue) + 1 < High(src.FValue) + 1) then
      Alloc(High(src.FValue) + 1);
    ArrayCopy(src.FValue, 0, FValue, 0, High(FValue) + 1 );
    FFracPrecision := src.FFracPrecision;
    FDescriptorType := src.FDescriptorType;
  end;
end;

function TOCIDateTime.Compare(const date1: TOCIDateTime): integer;
var
  i: integer;
  Last: Integer;
  V1, V2: integer;
begin
  if (Self.FDescriptorType <> date1.FDescriptorType) or IsNull or date1.IsNull then
    raise Exception.Create(SDatetimesIncomparable);

  if High(FValue) > High(date1.FValue) then
    Last := High(FValue)
  else
    Last := High(date1.FValue);

  for i := 0 to Last do begin
    if i <= High(FValue) then
      V1 := FValue[i]
    else
      V1 := 0;
    if i <= High(date1.FValue) then
      V2 := date1.FValue[i]
    else
      V2 := 0;
    if V1 > V2 then begin
      Result := 1;
      exit;
    end
    else if V1 < V2 then begin
      Result := -1;
      exit;
    end;
  end;
  Result := 0;
end;

function TOCIDateTime.Check: cardinal;
var
  year: sb2;
  month: ub1;
  day: ub1;
  hour: ub1;
  minute: ub1;
  sec: ub1;
  fsec: ub4;
  tzhour: sb1;
  tzminute: sb1;
  TmpYear: integer;
begin
  InternalGetDate(year, month, day);
  InternalGetTime(hour, minute, sec, fsec);
  Result := 0;
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    GetTimeZoneOffset(tzhour, tzminute);
    if (tzhour < -12) or (tzhour > 14) or (Integer(tzminute) * Integer(tzhour) < 0) or
      (tzminute > 59) or (tzminute < -59)
    then
      Result := Result or OCI_DT_INVALID_TIMEZONE;
  end;

  TmpYear := year;
  while TmpYear < 0 do
    TmpYear := TmpYear + 4000;
  if (year < -4712) or (year > 9999) then begin
    Result := Result or OCI_DT_INVALID_YEAR;
    if year < -4712 then
      Result := Result or OCI_DT_YEAR_BELOW_VALID;
  end;
  if year = 0 then
    Result := Result or OCI_DT_YEAR_ZERO;
  if (month < 1) or (month > 12) then begin
    Result := Result or OCI_DT_INVALID_MONTH;
    if year < 1 then
      Result := Result or OCI_DT_MONTH_BELOW_VALID;
  end;
  if (day < 1) or (day > DaysInAMonth(TmpYear, month)) then begin
    Result := Result or OCI_DT_INVALID_DAY;
    if day < 1 then
      Result := Result or OCI_DT_DAY_BELOW_VALID;
  end;
  if hour > 23 then
    Result := Result or OCI_DT_INVALID_HOUR;
  if minute > 59 then
    Result := Result or OCI_DT_INVALID_MINUTE;
  if sec > 59 then
    Result := Result or OCI_DT_INVALID_SECOND;
end;

{function TOCIDateTime.GetLangInd(Name: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Languages.Count - 1 do
    if CompareText(Languages.Name[i], Name) = 0 then begin
      Result := i;
      exit;
    end;
end;}

procedure TOCIDateTime.Parse(date_str: string; fmt: string);
var
  TmpYear: integer;
  TmpYear1, TmpMonth, TmpDay: word;

  NowYear, NowMonth, NowDay: word;
  year: sb2; month: ub1; day: ub1; hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
  tzhour, tzminute: sb1;
  hasTimeZone: boolean;
  DayOfYear, DayWeek: integer;
  BCAD, AMPM: string;

  i, j: integer;
  MaxHour: byte;
  MinYear: integer;
  Len, StrInd: integer;
//  FormatSettings: TFormatSettings;
  Sign, PosInd: integer;
  S: string;
  UseFSec: boolean;
  NewDate: TDateTime;
  TZHourMinute: integer;
  Length_date_str, Length_fmt: integer;

  function GetStr(SubLength: integer; Delimiter: string; ErrorMessage: string; MinLength: integer = MaxInt; DigitsOnly: boolean = False): string;
  var
    i: integer;
    UseDelimiter: boolean;
  begin
    Result := '';
    UseDelimiter := Pos(Delimiter, SPunctuation) > 0;
    if MinLength > SubLength then
      MinLength := SubLength;
    if Length_date_str - StrInd + 1 < MinLength then
      raise Exception.Create(SNotLongEnough);
    if DigitsOnly and (Length_date_str - StrInd + 1 > 0) and ((date_str[StrInd] < '0') or (date_str[StrInd] > '9')) then
      raise Exception.Create(SNonNnumericCharacter);
    i := 0;
    while True do begin
      if (SubLength > 0) and (i >= SubLength) then
        break;
      if Length_date_str < StrInd then
        break;
      if UseDelimiter and (Pos(date_str[StrInd], SPunctuation) > 0) then
        break;
      if DigitsOnly and ( (date_str[StrInd] < '0') or (date_str[StrInd] > '9') ) then
        break;
      Result := Result + date_str[StrInd];
      Inc(StrInd);
      Inc(i);
    end;
  end;

  function RomanToInt(S: string; ErrorMessage: string): integer;
  const
    Rom: array[1..12] of string =
      ('I', 'II', 'III', 'IV', 'V', 'VI', 'VII', 'VIII', 'IX', 'X', 'XI', 'XII');
  begin
    for Result := 1 to 12 do
      if S = Rom[Result] then
        exit;
    raise Exception.Create(ErrorMessage);
  end;

begin
  date_str := AnsiUpperCase(date_str);
  Length_date_str := Length(date_str);
  date_str := date_str + '                        ';
  fmt := AnsiUpperCase(fmt);
  Length_fmt := Length(fmt);
  fmt := fmt + '                        ';
//  GetLocaleFormatSettings(Languages.LocaleID[GetLangInd(lang_name)], FormatSettings);

  DecodeDate(Now, NowYear, NowMonth, NowDay);
  hasTimeZone := False;

  year     := High(year);
  month    := High(month);
  day      := High(day   );
  hour     := High(hour  );
  minute   := High(minute);
  sec      := High(sec   );
  fsec     := High(fsec  );
  DayOfYear      := High(DayOfYear);
  DayWeek        := High(DayWeek);
  AMPM := '';
  BCAD := '';
  MaxHour := 0;
  MinYear := -9999;

  UseFSec := True;
  Sign := 0;
  StrInd := 1;
  i := 1;
  while i <= Length_fmt do begin
    case fmt[i] of
      'A': begin
        Inc(i);
        if fmt[i] = 'D' then begin
          Inc(i);
          if MinYear = -4713 then
            raise Exception.Create(SPrecludesUseBCAD);
          MinYear := 0;
          BCAD := GetStr(2, fmt[i], SWrongBCAD);
          if (BCAD <> 'BC') and (BCAD <> 'AD') then
            raise Exception.Create(SWrongBCAD);
        end
        else if Copy(fmt, i, 3) = '.D.' then begin
          Inc(i, 3);
          if MinYear = -4713 then
            raise Exception.Create(SPrecludesUseBCAD);
          MinYear := 0;
          BCAD := GetStr(4, fmt[i], SWrongBCAD);
          if (BCAD <> 'B.C.') and (BCAD <> 'B.C.') then
            raise Exception.Create(SWrongBCAD);
        end
        else if fmt[i] = 'M' then begin
          Inc(i);
          if MaxHour = 24 then
            raise Exception.Create(SPrecludesUseMeridian);
          MaxHour := 12;
          AMPM := GetStr(2, fmt[i], SWrongAMPM);
          if (AMPM <> 'AM') and (AMPM <> 'PM') then
            raise Exception.Create(SWrongAMPM);
        end
        else if Copy(fmt, i, 3) = '.M.' then begin
          Inc(i, 3);
          if MaxHour = 24 then
            raise Exception.Create(SPrecludesUseMeridian);
          MaxHour := 12;
          AMPM := GetStr(4, fmt[i], SWrongAMPM);
          if (AMPM <> 'A.M.') and (AMPM <> 'P.M.') then
            raise Exception.Create(SWrongAMPM);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'B': begin
        Inc(i);
        if fmt[i] = 'C' then begin
          Inc(i);
          if MinYear = -4713 then
            raise Exception.Create(SPrecludesUseBCAD);
          MinYear := 0;
          BCAD := GetStr(2, fmt[i], SWrongBCAD);
          if (BCAD <> 'BC') and (BCAD <> 'AD') then
            raise Exception.Create(SWrongBCAD);
        end
        else if Copy(fmt, i, 3) = '.C.' then begin
          Inc(i, 3);
          if MinYear = -4713 then
            raise Exception.Create(SPrecludesUseBCAD);
          MinYear := 0;
          BCAD := GetStr(4, fmt[i], SWrongBCAD);
          if (BCAD <> 'B.C.') and (BCAD <> 'B.C.') then
            raise Exception.Create(SWrongBCAD);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'C': begin
        Inc(i);
        if fmt[i] = 'C' then
          Inc(i)  //TODO century
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'D': begin
        Inc(i);
        if Copy(fmt, i, 2) = 'DD' then begin
          Inc(i, 2);
          DayOfYear := GetInt(date_str, Length_date_str, StrInd, 3, 1, 366, SWrongDayOfYear);
        end
        else if fmt[i] = 'D' then begin
          Inc(i);
          Day := GetInt(date_str, Length_date_str, StrInd, 2, 1, 31, SWrongDayOfMonth);
        end
        else if fmt[i] = 'Y' then begin
          Inc(i);
          S := GetStr(3, fmt[i], SNotValidDayOfWeek);
          DayWeek := -1;
          for j := 1 to 7 do
            if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDayNames[j] = S then begin
              DayWeek := j;
              break;
            end;
          if DayWeek < 0 then
            raise Exception.Create(SNotValidDayOfWeek);
        end
        else if Copy(fmt, i, 2) = 'AY' then begin
          Inc(i);
          S := GetStr(9, fmt[i], SNotValidDayOfWeek);
          DayWeek := -1;
          for j := 1 to 7 do
            if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongDayNames[j] = S then begin
              DayWeek := j;
              break;
            end;
          if DayWeek < 0 then
            raise Exception.Create(SNotValidDayOfWeek);
        end
        else
          DayWeek := GetInt(date_str, Length_date_str, StrInd, 1, 1, 7, SNotValidDayOfWeek);
      end;
      'F': begin
        Inc(i);
        if fmt[i] = 'M' then begin
          Inc(i);
        end
        else if fmt[i] = 'X' then begin
          Inc(i);
        end
        else if fmt[i] = 'F' then begin
          Inc(i);
          if UseFSec then begin
            Len := 9;
            if (fmt[i] >= '1') and (fmt[i] <= '9') then begin
              Len := StrToInt(fmt[i]);
              Inc(i);//TODO many digits
            end;
            S := GetStr(Len, fmt[i], SNonNnumericCharacter, 0, True);//TODO many digits
            fsec := StrToIntDef(S, 0);
            if Length(IntToStr(fsec)) > Len then
              raise Exception.Create(SWrongFSec);
            fsec := StrToIntDef(Copy(S, 1, FFracPrecision), 0);
            for j := 1 to 9 - Length(S) do
              fsec := fsec * 10;
          end;
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'H': begin
        Inc(i);
        if fmt[i] = 'H' then begin
          Inc(i);
          if Copy(fmt, i, 2) = '24' then begin
            Inc(i, 2);
            if MaxHour = 12 then
              raise Exception.Create(SPrecludesUseMeridian);
            MaxHour := 24;
            Hour := GetInt(date_str, Length_date_str, StrInd, 2, 0, 23, SWrongHour24);
          end
          else
          if Copy(fmt, i, 2) = '12' then begin
            Inc(i, 2);
            MaxHour := 12;
            Hour := GetInt(date_str, Length_date_str, StrInd, 2, 0, 11, SWrongHour12);
          end
          else begin
            Hour := GetInt(date_str, Length_date_str, StrInd, 2, 0, 11, SWrongHour12);
            MaxHour := 12;
          end;
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      //TODO IW,IYY,IY,IYYY,J
      'M': begin
        Inc(i);
        if fmt[i] = 'I' then begin
          Inc(i);
          Minute := GetInt(date_str, Length_date_str, StrInd, 2, 0, 59, SWrongMinute);
        end
        else if fmt[i] = 'M' then begin
          Inc(i);
          Month := GetInt(date_str, Length_date_str, StrInd, 2, 1, 12, SNotValidMonth);
        end
        else if Copy(fmt, i, 4) = 'ONTH' then begin
          Inc(i, 4);
          S := AnsiUpperCase(GetStr(9, fmt[i], SNotValidMonth, 1));
          Month := High(Month);
          for j := 1 to 12 do
            if AnsiUpperCase({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongMonthNames[j]) = S then begin
              Month := j;
              break;
            end;
          if Month = High(Month) then
            raise Exception.Create(SNotValidMonth);
        end
        else if Copy(fmt, i, 2) = 'ON' then begin
          Inc(i, 2);
          S := AnsiUpperCase(GetStr(3, fmt[i], SNotValidMonth));
          Month := High(Month);
          for j := 1 to 12 do
            if AnsiUpperCase({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortMonthNames[j]) = S then begin
              Month := j;
              break;
            end;
          if Month = High(Month) then
            raise Exception.Create(SNotValidMonth);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'P': begin
        Inc(i);
        if fmt[i] = 'P' then begin
          Inc(i);
          if MaxHour = 24 then
            raise Exception.Create(SPrecludesUseMeridian);
          MaxHour := 12;
          AMPM := GetStr(2, fmt[i], SWrongAMPM);
          if (AMPM <> 'AM') and (AMPM <> 'PM') then
            raise Exception.Create(SWrongAMPM);
        end
        else if Copy(fmt, i, 3) = '.P.' then begin
          Inc(i, 3);
          if MaxHour = 24 then
            raise Exception.Create(SPrecludesUseMeridian);
          MaxHour := 12;
          AMPM := GetStr(3, fmt[i], SWrongAMPM);
          if (AMPM <> 'A.M.') and (AMPM <> 'P.M.') then
            raise Exception.Create(SWrongAMPM);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'R': begin
        Inc(i);
        if fmt[i] = 'M' then begin
          Inc(i);
          S := GetStr(4, fmt[i], SNotValidMonth, 1);
          month := RomanToInt(S, SNotValidMonth);
        end
        else if Copy(fmt, i, 3) = 'RRR' then begin
          Inc(i, 3);
          year := GetInt(date_str, Length_date_str, StrInd, 4, 0, 9999, SFormatNotRecognized);
          if year < 100 then begin
            if (year < 50) and ((NowYear mod 100) >= 50) then
              year := (NowYear div 100 + 1) * 100 + year
            else
            if (year >= 50) and ((NowYear mod 100) < 50) then
              year := (NowYear div 100 - 1) * 100 + year
            else
              year := (NowYear div 100) * 100 + year;
          end;
        end
        else if fmt[i] = 'R' then begin
          Inc(i);
          year := GetInt(date_str, Length_date_str, StrInd, 4, 0, 9999, SFormatNotRecognized);
          if year < 100 then begin
            if (year < 50) and ((NowYear mod 100) >= 50) then
              year := (NowYear div 100 + 1) * 100 + year
            else
            if (year >= 50) and ((NowYear mod 100) < 50) then
              year := (NowYear div 100 - 1) * 100 + year
            else
              year := (NowYear div 100) * 100 + year;
          end;
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'S': begin
        Inc(i);
        //TODO
        if Copy(fmt, i, 4) = 'YYYY'  then begin
          Inc(i, 4);
          Year := GetInt(date_str, Length_date_str, StrInd, 4, 0, 9999, SFormatNotRecognized);
          if MinYear = 0 then
            raise Exception.Create(SPrecludesUseBCAD);
          MinYear := -4713;
          if Sign < 0 then
            Year := - Year;
          if Year < -4713 then
            raise Exception.Create(SWrongYear);
        end
        else
        {if Copy(fmt, i, 3) = 'SSS'  then begin
          Inc(i, 3);
          SecondMidnight := GetInt(date_str, StrInd, 2, fmt[i], 0, 86399, SWrongSecond);
        end
        else }if fmt[i] = 'S' then begin
          Inc(i);
          sec := GetInt(date_str, Length_date_str, StrInd, 2, 0, 59, SWrongSecond);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'T': begin
        Inc(i);
        if Copy(fmt, i, 2) = 'ZH' then begin
          Inc(i, 2);
          if FDescriptorType <> OCI_DTYPE_TIMESTAMP_TZ then
            raise Exception.Create(SFormatNotRecognized);
          if Sign = 0 then
            Sign := 1;
          if date_str[StrInd] = '-' then begin
            Sign := -1;
            Inc(StrInd);
          end
          else if date_str[StrInd] = '+' then begin
            Sign := 1;
            Inc(StrInd);
          end;
          if date_str[StrInd] <> ' ' then begin
            tzhour := Sign * GetInt(date_str, Length_date_str, StrInd, 2, -12, 13, SWrongTZH);
            hasTimeZone := True;
            if tzhour < -12 then
              raise Exception.Create(SWrongTZH);
          end
          else
            tzhour := 0;
          Sign := 0;
        end
        else if Copy(fmt, i, 2) = 'ZM' then begin
          Inc(i, 2);
          if FDescriptorType <> OCI_DTYPE_TIMESTAMP_TZ then
            raise Exception.Create(SFormatNotRecognized);
          if (date_str[StrInd] = '-') or (date_str[StrInd] = '+') then
            Inc(StrInd);
          if hasTimeZone or (date_str[StrInd] <> ' ') then
            tzminute := GetInt(date_str, Length_date_str, StrInd, 2, -59, 59, SWrongTZM)
          else
            tzminute := 0;
        end
        else if Copy(fmt, i, 2) = 'ZR' then begin
          Inc(i, 2);
          if FDescriptorType <> OCI_DTYPE_TIMESTAMP_TZ then
            raise Exception.Create(SFormatNotRecognized);
          if Sign = 0 then
            Sign := 1;
          if date_str[StrInd] = '-' then begin
            Inc(StrInd);
            Sign := -1;
          end
          else if date_str[StrInd] = '+' then begin
            Inc(StrInd);
            Sign := 1;
          end;
          if date_str[StrInd] <> ' ' then begin
            tzhour := Sign * GetInt(date_str, Length_date_str, StrInd, 2, -12, 13, SWrongTZH);
            hasTimeZone := True;
            if tzhour < -12 then
              raise Exception.Create(SWrongTZH);
            Sign := 0;
            S := GetStr(1, '', SFormatNotRecognized);
            if S <> ':' then
              raise Exception.Create(SFormatNotRecognized);
            if (date_str[StrInd] = '-') or (date_str[StrInd] = '+') then
              Inc(StrInd);
            if date_str[StrInd] <> ' ' then
              tzminute := GetInt(date_str, Length_date_str, StrInd, 2, -59, 59, SWrongTZM)
            else
              tzminute := 0;
          end
          else begin
            tzhour := 0;
            tzminute := 0;
          end;
        end
        else if Copy(fmt, i, 2) = 'ZD' then begin
          Inc(I, 2);
          if FDescriptorType <> OCI_DTYPE_TIMESTAMP_TZ then
            raise Exception.Create(SFormatNotRecognized);
        end;
      end;
      'X': begin
        Inc(i);
        while StrInd <= Length_date_str do begin
          if date_str[StrInd] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator then
            break;
          if Pos(date_str[StrInd], SPunctuation) > 0 then
            Inc(StrInd)
          else
            break;
        end;
        UseFSec := (date_str[StrInd] = {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        if UseFSec then
          Inc(StrInd);
        while (StrInd <= Length_date_str) and (Pos(date_str[StrInd], SPunctuation) > 0) do
          Inc(StrInd);
      end;
      //WW W Y,YYY YEAR, SYEAR not allowd
      'Y': begin
        Inc(i);
        if Copy(fmt, i, 3) = 'YYY' then begin
          Inc(i, 3);
          Year := GetInt(date_str, Length_date_str, StrInd, 4, 0, 9999, SFormatNotRecognized);
        end
        else if Copy(fmt, i, 2) = 'YY' then begin
          Inc(i, 2);
          Year := GetInt(date_str, Length_date_str, StrInd, 3, 0, 9999, SFormatNotRecognized);
          year := (NowYear div 1000) * 1000 + year;
        end
        else if fmt[i] = 'Y' then begin
          Inc(i);
          Year := GetInt(date_str, Length_date_str, StrInd, 4, 0, 9999, SFormatNotRecognized);
          if year < 100 then
            year := (NowYear div 100) * 100 + year;
//          if (StrInd <= Length_date_str) and (date_str[StrInd] >= '0') and (date_str[StrInd] <= '9') then
//            raise Exception.Create(SNotMatchLength);
        end
        else begin
          Year := GetInt(date_str, Length_date_str, StrInd, 1, 0, 10, SFormatNotRecognized);
          year := (NowYear div 10) * 10 + year;
        end;
      end;
      '"': begin
        Inc(I);
        while (i <= Length_fmt) and (fmt[i] <> '"') and (StrInd <= Length_date_str) do begin
          if AnsiUpperCase(date_str[StrInd]) <> AnsiUpperCase(fmt[i]) then
            raise Exception.Create(SWrongLiteral);
          Inc(I);
          Inc(StrInd);
        end;
        Inc(I);
      end;
      else begin
        PosInd := Pos(fmt[i], SPunctuation);
        if PosInd > 0 then begin
          Inc(i);
          while (StrInd <= Length_date_str) and (Pos(date_str[StrInd], SPunctuation) > 0) do begin
            if date_str[StrInd] = '-' then
              Sign := -1
            else
              Sign := 0;
            Inc(StrInd);
          end;
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
    end;
  end;

  if year = High(year) then
    year := NowYear
  else
    if (BCAD = 'BC') or (BCAD = 'B.C.') then
      year := - year;

  TmpYear := year;
  while TmpYear < 0 do
    TmpYear := TmpYear + 4000;
  if DayOfYear <> High(DayOfYear) then begin
    if DayOfYear > DaysInAYear(TmpYear) then
      raise Exception.Create(SWrongDayOfYear);
    NewDate := EncodeDate(TmpYear, 1, 1) + DayOfYear;
    DecodeDate(NewDate, TmpYear1, TmpMonth, TmpDay);
    if (month <> High(month)) and (month <> TmpMonth) then
      raise Exception.Create(SConflictMonthJulian);
    if (day <> High(day)) and (day <> TmpDay) then
      raise Exception.Create(SConflictDayJulian);
    month := TmpMonth;
    day := TmpDay;
  end;

  if month = High(month) then
    month := NowMonth;
  if day = High(day) then
    day := 1;

  if day > DaysInAMonth(TmpYear, month) then
    raise Exception.Create(SInvalidDay);

  NewDate := EncodeDate(TmpYear, month, day);
  if (DayWeek <> High(DayWeek)) and (DayOfWeek(NewDate) <> DayWeek) then
    raise Exception.Create(SConflictWeekDayJulian);

  if hour = High(hour) then
    hour := 0;
  if minute = High(minute) then
    minute := 0;
  if sec = High(sec) then
    sec := 0;
  if fsec = High(fsec) then
    fsec := 0;

  if (MaxHour = 12) and ((AMPM = 'PM') or (AMPM = 'P.M.')) then begin
    hour := hour + 12;
    if hour = 24 then
      hour := 0;
  end;

  if FDescriptorType <> OCI_DTYPE_TIMESTAMP then begin
    if not hasTimeZone then
      GetLocalTimeZone(tzhour, tzminute);
    if tzhour < 0 then
      tzminute := - tzminute;
    TZHourMinute := tzhour * 60 + tzminute;
    if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then
//      TZHourMinute :=  TZHourMinute - (FServerTZHour * 60 + FServerTZMunute);
      TZHourMinute :=  TZHourMinute - TimeStampLTZServerOffset;
    ChangeTimeZoneOffset(year, month, day, hour, minute, TZHourMinute, False);
    SetTimeZoneOffset(tzhour, tzminute);
  end;
  InternalSetDate(year, month, day);
  InternalSetTime(hour, minute, sec, fsec);
end;

function TOCIDateTime.ToString(fmt: string; fsprec: ub1): string;

  function ApplyFM(FormatStr: string; TogleFM: boolean): string;
  begin
    if TogleFM then
      Result := '0'
    else
      Result := FormatStr;
  end;

  function GetFormatStr(TogleFM: boolean): string;
  begin
    if TogleFM then
      Result := '%s'
    else
      Result := '%-9s';
  end;

  function IntToRoman(Month: integer): string;
  begin
    case Month of
      1: Result := 'I';
      2: Result := 'II';
      3: Result := 'III';
      4: Result := 'IV';
      5: Result := 'V';
      6: Result := 'VI';
      7: Result := 'VII';
      8: Result := 'VIII';
      9: Result := 'IX';
      10: Result := 'X';
      11: Result := 'XI';
      12: Result := 'XII';
    else
      Result := '';
    end;
  end;

  function ToLowerCase(Case1, Case2: boolean; Str: string): string; overload;
  begin
    if Length(Str) > 0 then begin
      if Case1 then
        Result := AnsiLowerCase(Str[1])
      else
        Result := AnsiUpperCase(Str[1]);

      if Length(Str) > 1 then begin
        if Case2 or Case1 then
          Result := Result + AnsiLowerCase(Copy(Str, 2, Length(Str) - 1))
        else
          Result := Result + AnsiUpperCase(Copy(Str, 2, Length(Str) - 1));
      end;
    end
    else
      Result := '';
  end;

  function ToLowerCase(Str: string; ToLower: boolean): string; overload;
  begin
    if ToLower then
      Result := AnsiLowerCase(Str)
    else
      Result := Str;
  end;

var
  year: sb2;
  month: ub1;
  day: ub1;
  hour: ub1;
  minute: ub1;
  sec: ub1;
  fsec: ub4;
  tzhour: sb1;
  tzminute: sb1;
  DateTime: TDateTime;
//  FormatSettings: TFormatSettings;
  UpperFmt: string;
  TogleFM: boolean;
  Length_fmt: integer;

  TmpYear: integer;
  i, j, PosInd, Len: integer;
  TZHourMinute: integer;
begin
  Length_fmt := Length(fmt);
  fmt := fmt + '                        ';
  UpperFmt := AnsiUpperCase(fmt);

  InternalGetDate(year, month, day);
  InternalGetTime(hour, minute, sec, fsec);
  GetActualTimeZoneOffset(tzhour, tzminute);
  if FDescriptorType <> OCI_DTYPE_TIMESTAMP then begin
    TZHourMinute := tzhour * 60 + tzminute;
    if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then
//      TZHourMinute :=  TZHourMinute - (FServerTZHour * 60 + FServerTZMunute);
      TZHourMinute :=  TZHourMinute - TimeStampLTZServerOffset;
    ChangeTimeZoneOffset(year, month, day, hour, minute, TZHourMinute, True);
  end;

  TmpYear := year;
  while TmpYear < 0 do
    TmpYear := TmpYear + 4000;

  DateTime := EncodeDate(TmpYear, month, day);
  if DateTime >= 0 then
    DateTime := DateTime + EncodeTime(hour, minute, sec, fsec div 1000000)
  else
    DateTime := DateTime - EncodeTime(hour, minute, sec, fsec div 1000000);
//  GetLocaleFormatSettings(Languages.LocaleID[GetLangInd(lang)], FormatSettings);

  Result := '';
  TogleFM := False;
  i := 1;
  while i <= Length_fmt do begin
    case UpperFmt[i] of
      'A': begin
        Inc(i);
        if UpperFmt[i] = 'D' then begin
          Inc(i);
          if year < 0 then
            Result := Result + ToLowerCase('BC', fmt[i - 2] = 'a')
          else
            Result := Result + ToLowerCase('AD', fmt[i - 2] = 'a');
        end
        else if Copy(UpperFmt ,i, 3) = '.D.' then begin
          Inc(i, 3);
          if year < 0 then
            Result := Result + ToLowerCase('B.C.', fmt[i - 4] = 'a')
          else
            Result := Result + ToLowerCase('A.D.', fmt[i - 4] = 'a');
        end
        else if UpperFmt[i] = 'M' then begin
          Inc(i);
          if hour < 12 then
            Result := Result + ToLowerCase('AM', fmt[i - 2] = 'a')
          else
            Result := Result + ToLowerCase('PM', fmt[i - 2] = 'a');
        end
        else if Copy(UpperFmt ,i, 3) = '.M.' then begin
          Inc(i, 3);
          if hour < 12 then
            Result := Result + ToLowerCase('A.M.', fmt[i - 4] = 'a')
          else
            Result := Result + ToLowerCase('P.M.', fmt[i - 4] = 'a');
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'B': begin
        Inc(i);
        if UpperFmt[i] = 'C' then begin
          Inc(i);
          if year < 0 then
            Result := Result + ToLowerCase('BC', fmt[i - 2] = 'b')
          else
            Result := Result + ToLowerCase('AD', fmt[i - 2] = 'b');
        end
        else if Copy(UpperFmt ,i, 3) = '.C.' then begin
          Inc(i, 3);
          if year < 0 then
            Result := Result + ToLowerCase('B.C.', fmt[i - 4] = 'b')
          else
            Result := Result + ToLowerCase('A.D.', fmt[i - 4] = 'b');
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'C': begin
        Inc(i);
        if UpperFmt[i] = 'C' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), (Abs(year) - 1) div 100 + 1);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'D': begin
        Inc(i);
        if Copy(UpperFmt ,i, 2) = 'DD' then begin
          Inc(i, 2);
          Result := Result +  FormatFloat(ApplyFM('000', TogleFM), DayOfTheYear(DateTime));
        end
        else if UpperFmt[i] = 'D' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), day);
        end
        else if UpperFmt[i] = 'Y' then begin
          Inc(i);
          Result := Result + ToLowerCase(fmt[i - 2] = 'd', fmt[i - 1] = 'y', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortDayNames[DayOfWeek(DateTime)]);
        end
        else if Copy(UpperFmt ,i, 2) = 'AY' then begin
          Inc(i, 2);
          Result := Result + Format(GetFormatStr(TogleFM), [ToLowerCase(fmt[i - 3] = 'd', fmt[i - 2] = 'a', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongDayNames[DayOfWeek(DateTime)])]);
        end
        else
          Result := Result + IntToStr(DayOfWeek(DateTime));
      end;
      'F': begin
        Inc(i);
        if UpperFmt[i] = 'M' then begin
          Inc(i);
          TogleFM := not TogleFM;
        end
        else if UpperFmt[i] = 'X' then begin
          Inc(i);
        end
        else if UpperFmt[i] = 'F' then begin
          Inc(i);
          Len := fsprec;
          if (fmt[i] >= '1') and (fmt[i] <= '9') then begin
            Len := StrToInt(fmt[i]);
            Inc(i);//TODO many digits
          end;
          Result := Result + Copy(FormatFloat('000000000', fsec ), 1, Len);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'H': begin
        Inc(i);
        if UpperFmt[i] = 'H' then begin
          Inc(i);
          if Copy(UpperFmt ,i, 2) = '24' then begin
            Inc(i, 2);
            Result := Result +  FormatFloat(ApplyFM('00', TogleFM), hour);
          end
          else
          if Copy(UpperFmt ,i, 2) = '12' then begin
            Inc(i, 2);
            Result := Result +  FormatFloat(ApplyFM('00', TogleFM), ((hour + 11) mod 12) + 1);
          end
          else begin
            Result := Result +  FormatFloat(ApplyFM('00', TogleFM), ((hour + 11) mod 12) + 1);
          end;
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'I': begin
        Inc(i);
        if Copy(UpperFmt, i, 3) = 'YYY' then begin
          Inc(i, 3);
          Result := Result +  FormatFloat(ApplyFM('0000', TogleFM), Abs(year));
        end
        else if Copy(UpperFmt, i, 2) = 'YY' then begin
          Inc(i, 2);
          Result := Result +  FormatFloat(ApplyFM('000', TogleFM), Abs(year) mod 1000);
        end
        else
          case UpperFmt[i] of
            'Y': begin
              Inc(i);
              Result := Result +  FormatFloat(ApplyFM('00', TogleFM), Abs(year) mod 100);
            end;
            'W': begin
              Inc(i);
              Result := Result +  FormatFloat(ApplyFM('00', TogleFM), WeekOfTheYear(DateTime));
            end;
            else
              Result := Result +  FormatFloat('0', Abs(year) mod 10);
          end;
      end;
      'J': begin
        Inc(I);
        j := Trunc(DateTime) + 2415019 - (TmpYear - year) div 400 * 146097;
        if DateTime < 0 then
          Dec(j);
        Result := Result +  FormatFloat(ApplyFM('0000000', TogleFM), j);
      end;
      'M': begin
        Inc(i);
        if UpperFmt[i] = 'I' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), minute);
        end
        else if UpperFmt[i] = 'M' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), month);
        end
        else if Copy(UpperFmt ,i, 4) = 'ONTH' then begin
          Inc(i, 4);
          Result := Result +  Format(GetFormatStr(TogleFM), [ToLowerCase(fmt[i - 5] = 'm', fmt[i - 6] = 'o', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}LongMonthNames[month])]);
        end
        else if Copy(UpperFmt ,i, 2) = 'ON' then begin
          Inc(i, 2);
          Result := Result +  ToLowerCase(fmt[i - 3] = 'm', fmt[i - 2] = 'o', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}ShortMonthNames[month]);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'P': begin
        Inc(i);
        if UpperFmt[i] = 'M' then begin
          Inc(i);
          if hour < 12 then
            Result := Result + ToLowerCase('AM', fmt[i - 2] = 'p')
          else
            Result := Result + ToLowerCase('PM', fmt[i - 2] = 'p');
        end
        else if Copy(UpperFmt ,i, 3) = '.M.' then begin
          Inc(i, 3);
          if hour < 12 then
            Result := Result + ToLowerCase('A.M.', fmt[i - 4] = 'p')
          else
            Result := Result + ToLowerCase('P.M.', fmt[i - 4] = 'p');
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'Q': begin
        Inc(I);
        Result := Result +  IntToStr((month - 1) div 3 + 1);
      end;
      'R': begin
        Inc(i);
        if UpperFmt[i] = 'M' then begin
          Inc(i);
          Result := Result + ToLowerCase(fmt[i - 2] = 'r', fmt[i - 1] = 'm', IntToRoman(month));
        end
        else if Copy(UpperFmt ,i, 3) = 'RRR' then begin
          Inc(i, 3);
          Result := Result +  FormatFloat(ApplyFM('0000', TogleFM), Abs(year));
        end
        else if UpperFmt[i] = 'R' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), Abs(year) mod 100);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'S': begin
        Inc(i);
        if Copy(UpperFmt ,i, 4) = 'YYYY'  then begin
          Inc(i, 4);
          if year < 0 then
            Result := Result + '-'
          else
            Result := Result + ' ';
          Result := Result +  FormatFloat(ApplyFM('0000', TogleFM), abs(year));
        end
        else if Copy(UpperFmt ,i, 4) = 'SSSS'  then begin
          Inc(i, 4);
          Result := Result +  FormatFloat(ApplyFM('00000', TogleFM), SecondOfTheDay(DateTime));
        end
        else if UpperFmt[i] = 'S' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), sec);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'T': begin
        Inc(i);
        if Copy(UpperFmt ,i, 2) = 'ZH' then begin
          Inc(i, 2);
          if (tzhour < 0) or ( (tzhour = 0) and (tzminute < 0) ) then
            Result := Result + '-'
          else
            Result := Result + '+';
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), Abs(tzhour));
        end
        else if Copy(UpperFmt ,i, 2) = 'ZM' then begin
          Inc(i, 2);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), Abs(tzminute));
        end
        else if Copy(UpperFmt ,i, 2) = 'ZR' then begin
          Inc(i, 2);
          if (tzhour < 0) or ( (tzhour = 0) and (tzminute < 0) ) then
            Result := Result + '-'
          else
            Result := Result + '+';
          Result := Result + FormatFloat('00', Abs(tzhour)) + ':' + FormatFloat('00', Abs(tzminute));
        end
        else if Copy(UpperFmt ,i, 2) = 'ZD' then begin
          Inc(I, 2); //TODO
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
      'W': begin
        Inc(i);
        if UpperFmt[i] = 'W' then begin
          Inc(i, 1);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), WeekOfTheYear(DateTime));
        end
        else
          Result := Result +  IntToStr((day - 1) div 7 + 1);
      end;
      'X': begin
        Inc(i);
        Result := Result +  {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator;
      end;
      'Y': begin
        Inc(i);
        if Copy(UpperFmt ,i, 4) = ',YYY' then begin
          Inc(i, 4);
          Result := Result +  FormatFloat('0', Abs(year) div 1000) + ',' + FormatFloat('000', Abs(year) mod 1000);
        end
        else if Copy(UpperFmt ,i, 3) = 'YYY' then begin
          Inc(i, 3);
          Result := Result +  FormatFloat(ApplyFM('0000', TogleFM), Abs(year));
        end
        else if Copy(UpperFmt ,i, 2) = 'YY' then begin
          Inc(i, 2);
          Result := Result +  FormatFloat(ApplyFM('000', TogleFM), Abs(year) mod 1000);
        end
        else if UpperFmt[i] = 'Y' then begin
          Inc(i);
          Result := Result +  FormatFloat(ApplyFM('00', TogleFM), Abs(year) mod 100);
        end
        else begin
          Result := Result +  FormatFloat('0', Abs(year) mod 10);
        end;
      end;
      '"': begin
        Inc(I);
        while (i <= Length_fmt) and (fmt[i] <> '"') do begin
          Result := Result + fmt[I];
          Inc(I);
        end;
        Inc(I);
      end;
      else begin
        PosInd := Pos(fmt[i], SPunctuation);
        if PosInd > 0 then begin
          Result := Result + fmt[I];
          Inc(i);
        end
        else
          raise Exception.Create(SFormatNotRecognized);
      end;
    end;
  end;
end;

procedure TOCIDateTime.Construct(year: sb2; month, day, hour, min, sec: ub1; fsec: ub4; timezone: string);
var
  localTimeZoneOffset: Integer;
  tzhour, tzminute: sb1;
  Sign: integer;
  StrInd: integer;
begin
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    Sign := 1;
    StrInd := 1;
    if timezone = '' then begin
      localTimeZoneOffset := GetLocalTimeZoneOffset;
      tzhour := localTimeZoneOffset div 60;
      tzminute := localTimeZoneOffset mod 60;
    end
    else begin
      if timezone[StrInd] = '-' then begin
        Inc(StrInd);
        Sign := -1;
      end
      else if timezone[StrInd] = '+' then begin
        Inc(StrInd);
        Sign := 1;
      end;
      tzhour := Sign * GetInt(timezone, Length(timezone), StrInd, 2, -12, 13, SWrongTZH);
      if tzhour < -12 then
        raise Exception.Create(SWrongTZH);
      if timezone[StrInd] <> ':' then
        raise Exception.Create(SFormatNotRecognized);
      Inc(StrInd);
      if (timezone[StrInd] = '-') or (timezone[StrInd] = '+') then
        Inc(StrInd);
      tzminute := GetInt(timezone, Length(timezone), StrInd, 2, -59, 59, SWrongTZM);
      if tzhour < 0 then
        tzminute := -tzminute;
    end;

    SetTimeZoneOffset(tzhour, tzminute);
    if (tzhour <> 0) or (tzminute <> 0) then
      ChangeTimeZoneOffset(year, month, day, hour, min, tzhour * 60 + tzminute, False);
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then
//    ChangeTimeZoneOffset(year, month, day, hour, min, FServerTZHour * 60 + FServerTZMunute, True);
    ChangeTimeZoneOffset(year, month, day, hour, min, TimeStampLTZServerOffset, True);

  InternalSetDate(year, month, day);
  InternalSetTime(hour, min, sec, fsec);
end;

procedure TOCIDateTime.GetDate(var year: sb2; var month: ub1; var day: ub1);
var
  OldHour: ub1;
  OldMinute: ub1;
  OldSec: ub1;
  OldFSec: ub4;
  TZHour, TZMin: shortint;
begin
  InternalGetDate(year, month, day);
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
    GetTimeZoneOffset(TZHour, TZMin);
    if (TZHour <> 0) or (TZMin <> 0) then
      ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, TZHour * 60 + TZMin, True);
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then begin
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
//    ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, FServerTZHour * 60 + FServerTZMunute, False);
    ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, TimeStampLTZServerOffset, False);
  end;
end;

procedure TOCIDateTime.SetDate(year: sb2; month: ub1; day: ub1);
var
  OldYear: sb2;
  OldMonth: ub1;
  OldDay: ub1;
  OldHour: ub1;
  OldMinute: ub1;
  OldSec: ub1;
  OldFSec: ub4;
  TZHour, TZMin: shortint;
begin
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
    GetTimeZoneOffset(TZHour, TZMin);
    if (TZHour <> 0) or (TZMin <> 0) then begin
      ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, TZHour * 60 + TZMin, True);
      ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, TZHour * 60 + TZMin, False);
    end;
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
//    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, FServerTZHour * 60 + FServerTZMunute, False);
    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, TimeStampLTZServerOffset, False);
//    ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, FServerTZHour * 60 + FServerTZMunute, True);
    ChangeTimeZoneOffset(year, month, day, OldHour, OldMinute, TimeStampLTZServerOffset, True);
  end;
  InternalSetDate(year, month, day);
end;

procedure TOCIDateTime.GetTime(var hour: ub1; var minute: ub1; var sec: ub1; var fsec: ub4);
var
  OldYear: sb2;
  OldMonth: ub1;
  OldDay: ub1;
  TZHour, TZMin: shortint;
begin
  InternalGetTime(hour, minute, sec, fsec);
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
    GetTimeZoneOffset(TZHour, TZMin);
    if (TZHour <> 0) or (TZMin <> 0) then
      ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, TZHour * 60 + TZMin, True);
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
//    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, FServerTZHour * 60 + FServerTZMunute, False);
    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, TimeStampLTZServerOffset, False);
  end;
end;

procedure TOCIDateTime.SetTime(hour: ub1; minute: ub1; sec: ub1; fsec: ub4);
var
  OldYear: sb2;
  OldMonth: ub1;
  OldDay: ub1;
  OldHour: ub1;
  OldMinute: ub1;
  OldSec: ub1;
  OldFSec: ub4;
  TZHour, TZMin: shortint;
begin
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
    GetTimeZoneOffset(TZHour, TZMin);
    if (TZHour <> 0) or (TZMin <> 0) then begin
      ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, TZHour * 60 + TZMin, True);
      ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, TZHour * 60 + TZMin, False);
      InternalSetDate(OldYear, OldMonth, OldDay);
    end;
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then begin
    InternalGetDate(OldYear, OldMonth, OldDay);
    InternalGetTime(OldHour, OldMinute, OldSec, OldFSec);
//    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, FServerTZHour * 60 + FServerTZMunute, False);
    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, OldHour, OldMinute, TimeStampLTZServerOffset, False);
//    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, FServerTZHour * 60 + FServerTZMunute, True);
    ChangeTimeZoneOffset(OldYear, OldMonth, OldDay, hour, minute, TimeStampLTZServerOffset, True);
    InternalSetDate(OldYear, OldMonth, OldDay);
  end;
  InternalSetTime(hour, minute, sec, fsec);
end;

procedure TOCIDateTime.GetTimeZoneOffset(out tzHour: sb1; out tzMinute: sb1);
var
  tzIndex: Integer;
begin
  if FDescriptorType = OCI_DTYPE_TIMESTAMP_TZ then begin
    if IsNull or (Length(FValue) < 13) then begin
      tzHour   := 0;
      tzMinute := 0;
    end
    else if FValue[11] and $80 = 0 then begin
      tzHour   := FValue[11] - 20;
      tzMinute := FValue[12] - 60;
    end
    else begin
      tzIndex := GetTimeZoneIndex(FValue[11], FValue[12]);
      if tzIndex >= 0 then
        DetectTimeZoneOffset(Value, tzIndex, tzHour, tzMinute)
      else begin
        tzHour   := 0;
        tzMinute := 0;
      end;
    end;
  end
  else if FDescriptorType = OCI_DTYPE_TIMESTAMP_LTZ then
    if IsNull then begin
      tzHour   := 0;
      tzMinute := 0;
    end
    else
      GetLocalTimeZone(tzHour, tzMinute)
  else
    raise Exception.Create(SFieldNotFound);
end;

procedure TOCIDateTime.SetTimeZoneOffset(tzHour: sb1; tzMinute: sb1);
var
  newLen: Integer;
begin
  if FDescriptorType <> OCI_DTYPE_TIMESTAMP_TZ then
    exit;
  CheckAlloc(False, True);
  if High(FValue) >= 11 then begin
    FValue[11] := tzHour   + 20;
    FValue[12] := tzMinute + 60;
  end;

  newLen := GetTimeZoneTrimmedLen(FValue, FDescriptorType);
  if newLen < Length(FValue) then
    Alloc(newLen);
end;

function TOCIDateTime.GetTimeZoneName: string;
var
  tzhour: sb1;
  tzminute: sb1;
begin
  GetTimeZoneOffset(tzhour, tzminute);
  if (tzhour < 0) or ( (tzhour = 0) and (tzminute < 0) ) then
    Result := '-'
  else
    Result := '+';
  Result := Result + FormatFloat('00', Abs(tzhour)) + ':' + FormatFloat('00', Abs(tzminute));
end;

end.
