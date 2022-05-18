
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  OCIDatetime
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraIntervalUni;
{$ENDIF}

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
  SIntervalInvalid = 'ORA-01867: the interval is invalid';
  SLeadPricisionSmall = 'ORA-01873: the leading pricision of interval is too small';
  SInvalidMonth = 'ORA-01843: not a valid month';
  SInvalidHour = 'ORA-01850 hour must be between 0 and 23';
  SInvalidMinutes = 'ORA-01851 minutes must be between 0 and 59';
  SInvalidSeconds = 'ORA-01852 seconds must be between 0 and 59';
  SIntervalsIncomparable = 'ORA-01870 the intervals or datetimes are not mutually comparable';

type
  TOCIInterval = class
  private
    FValue: TBytes;
    FLeadPrecision: byte;
    FFracPrecision: byte;

    function GetIsNull: boolean;
  protected
    procedure Alloc; virtual; abstract;

    function GetSign(Value: string; var CurPos: integer): boolean;
    function GetNextNumber(Value: string; var CurPos: integer; Delimiter: string; MaxValue: integer; RangeErrorMsg: string; FractionalPart: boolean = False): integer;
    procedure PutNextNumber(var ResStr: string; Number: integer; Delimiter: string; MaxValue: integer; var Negative: integer; FractionalPart: boolean = False);
  public
    constructor Create;

    procedure Assign(Interval: TOCIInterval);
    function Compare(Interval: TOCIInterval): integer;

    function Check: cardinal; virtual; abstract;
    procedure Parse(Value: string); virtual; abstract;
    function ToString(Precision, Scale: byte): string; reintroduce; virtual; abstract;

    procedure FromNumber(Number: pOCINumber);

    property IsNull: boolean read GetIsNull;
    property Value: TBytes read FValue write FValue;
  end;

  TOCIIntervalYM = class(TOCIInterval)
  protected
    procedure Alloc; override;
  public
    function Check: cardinal; override;

    procedure Parse(Value: string); override;
    function ToString(Precision, Scale: byte): string; override;

    procedure SetYearMonth(Year, Month: integer);
    procedure GetYearMonth(var Year: integer; var Month: integer);
  end;

  TOCIIntervalDS = class(TOCIInterval)
  protected
    procedure Alloc; override;
  public
    function Check: cardinal; override;

    procedure Parse(Value: string); override;
    function ToString(Precision, Scale: byte): string; override;

    procedure SetDaySecond(Day, Hour, Minute, Second, fSecond: integer);
    procedure GetDaySecond(var Day: integer; var Hour: integer; var minute: integer;
      var Second: integer; var fSecond: integer);
  end;

implementation

uses
  MemUtils;

{ TOCIInterval }

constructor TOCIInterval.Create{(Parent: OCIHandle)};
begin
  inherited;

  FLeadPrecision := 2;
  FFracPrecision := 6;
end;

function TOCIInterval.GetIsNull: boolean;
begin
  Result := not Assigned(FValue);
end;

function TOCIInterval.GetSign(Value: string; var CurPos: integer): boolean;
begin
  while (CurPos <=Length(Value)) and (Value[CurPos] = ' ') do
    Inc(CurPos);
  Result := False;
  if (Value[CurPos] = '-') or (Value[CurPos] = '+') then begin
    if Value[CurPos] = '-' then
      Result := True;
    Inc(CurPos);
  end;
end;

function TOCIInterval.GetNextNumber(Value: string; var CurPos: integer; Delimiter: string; MaxValue: integer; RangeErrorMsg: string; FractionalPart: boolean): integer;
var
  i: integer;
  S: string;
  p: integer;
begin
  while (CurPos <=Length(Value)) and (Value[CurPos] = ' ') do
    Inc(CurPos);
  if Delimiter <> '' then begin
    p := CurPos;
    while (p <= Length(Value)) and (Value[p] <> Delimiter) do
      Inc(p);
    if p > Length(Value) then
      raise Exception.Create(SIntervalInvalid);
    S := Trim(Copy(Value, CurPos, p - 1 - (CurPos - 1)));
    CurPos := p + 1;
  end
  else begin
    S := Copy(Value, CurPos, Length(Value) - (CurPos - 1));
    CurPos := Length(Value) + 1;
  end;
  if (S = '') or (S[1] < '0') or (S[1] > '9') then //not negative
    raise Exception.Create(SIntervalInvalid);
  try
    if FractionalPart then begin// decimal point
      Result := StrToInt(Copy(S, 1, 9));
      for i := 1 to 9 - Length(S) do
        Result := Result * 10;
    end
    else
      Result := StrToInt(S);
  except
    raise Exception.Create(SIntervalInvalid)
  end;
  if (Result > MaxValue) and (RangeErrorMsg <> '') then
    raise Exception.Create(RangeErrorMsg);
end;

procedure TOCIInterval.PutNextNumber(var ResStr: string; Number: integer; Delimiter: string; MaxValue: integer; var Negative: integer; FractionalPart: boolean);
var
  FormatStr: string;
  Len: integer;
begin
  if Negative = 0 then begin
    if Number > 0 then
      Negative := 1
    else if Number < 0 then
      Negative := -1;
  end;
  Assert( Negative * Number >= 0 );
  Len := Length(IntToStr(MaxValue));
  if FractionalPart then // decimal point
    ResStr := ResStr + Copy(FormatFloat('000000000', Negative * Number ), 1, Len)
  else begin
    FormatStr := StringOfChar('0', Len);
    ResStr := ResStr + FormatFloat(FormatStr, Negative * Number);
  end;
  if Delimiter <> '' then
    ResStr := ResStr + Delimiter;
end;

procedure TOCIInterval.Assign(Interval: TOCIInterval);
begin
  if Self.ClassType <> Interval.ClassType then
    raise Exception.Create(SIntervalsIncomparable);

  SetLength(FValue, Length(Interval.FValue));
  ArrayCopy(Interval.FValue, 0, FValue, 0, High(FValue) + 1 );
  FLeadPrecision := Interval.FLeadPrecision;
  FFracPrecision := Interval.FFracPrecision;
end;

function TOCIInterval.Compare(Interval: TOCIInterval): integer;
var
  i: integer;
begin
  if (Self.ClassType <> Interval.ClassType) or IsNull or Interval.IsNull then
    raise Exception.Create(SIntervalsIncomparable);
  for i := 0 to High(FValue) do
    if FValue[i] > Interval.FValue[i] then begin
      Result := 1;
      exit;
    end
    else if FValue[i] < Interval.FValue[i] then begin
      Result := -1;
      exit;
    end;
  Result := 0;
end;

procedure TOCIInterval.FromNumber(Number: pOCINumber);
begin
end;


{ TOCIIntervalYM }

{procedure TOCIIntervalYM.SetValue(Value: TBytes);
begin
  if Assigned(Value) then begin
    Alloc;
    ArrayCopy(Value, 0, FValue, 0, 5);
  end;
end;}

procedure TOCIIntervalYM.Alloc;
begin
  SetLength(FValue, 5);
end;

function TOCIIntervalYM.Check: cardinal;
var
  Year: integer;
  Month: integer;
  i, Count, MaxValue: integer;
begin
  GetYearMonth(Year, Month);
  Count := FLeadPrecision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;
  MaxValue := 1;
  for i := 1 to Count do
    MaxValue := MaxValue * 10;
  MaxValue := MaxValue - 1;

  Result := 0;
  if (Year < - MaxValue) or (Year > MaxValue) then begin
    Result := Result or OCI_INTER_INVALID_YEAR;
    if Year < - MaxValue then
      Result := Result or OCI_INTER_DAY_BELOW_VALID;
  end;
  if (Month > 0) and (Year < 0) or
    (Month < 0) and (Year > 0) or (Abs(Month) > 11)
  then
    Result := Result or OCI_INTER_INVALID_MONTH;
end;

procedure TOCIIntervalYM.Parse(Value: string);
var
  Year, Month: integer;
  Negative: boolean;
  CurPos: integer;
  MaxValue: integer;
  i, Count: integer;
begin
  if IsNull then
    Alloc;

  Count := FLeadPrecision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;
  MaxValue := 1;
  for i := 1 to Count do
    MaxValue := MaxValue * 10;
  MaxValue := MaxValue - 1;

  CurPos := 1;
  Negative := GetSign(Value, CurPos);
  Year := GetNextNumber(Value, CurPos, '-', MaxValue, SLeadPricisionSmall);
  Month := GetNextNumber(Value, CurPos, '', 11,       SInvalidMonth);
  if Negative then begin
    Year := -Year;
    Month := -Month;
  end;
  SetYearMonth(Year, Month);
end;

function TOCIIntervalYM.ToString(Precision, Scale: byte): string;
var
  i, Count: integer;
  Negative: integer;
  Year, Month: integer;
  MaxValue: integer;
begin
  if IsNull then begin
    Result := '';
    exit;
  end;

  Count := Precision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;
  MaxValue := 1;
  for i := 1 to Count do
    MaxValue := MaxValue * 10;
  MaxValue := MaxValue - 1;

  GetYearMonth(Year, Month);
  Negative := 0;
  Result := '';
  PutNextNumber(Result, Year, '-', 11,       Negative);
  PutNextNumber(Result, Month, '',  MaxValue, Negative);
  if Negative >= 0 then
    Result := '+' + Result
  else
    Result := '-' + Result;
end;

procedure TOCIIntervalYM.SetYearMonth(Year, Month: integer);
begin
  if IsNull then
    Alloc;
  FValue[0] := Byte(Year shr 24) xor $80;
  FValue[1] := Byte(Year shr 16);
  FValue[2] := Byte(Year shr 8);
  FValue[3] := Byte(Year);

  FValue[4] := Byte(Month + 60);
end;

procedure TOCIIntervalYM.GetYearMonth(var Year: integer; var Month: integer);
begin
  if IsNull then begin
    Year  := 0;
    Month := 0;
    exit;
  end;
  Year :=
    (Integer(FValue[0] xor $80) shl 24) or
    (Integer(FValue[1]) shl 16) or
    (Integer(FValue[2]) shl 8) or
    FValue[3];
  Month := ShortInt(FValue[4]) - 60;
end;

{ TOCIIntervalDS }

{procedure TOCIIntervalDS.SetValue(Value: TBytes);
begin
  if Assigned(Value) then begin
    Alloc;
    ArrayCopy(Value, 0, FValue, 0, 11);
  end;
end;}

procedure TOCIIntervalDS.Alloc;
begin
  SetLength(FValue, 11);
end;

function TOCIIntervalDS.Check: cardinal;
var
  Day, Hour, Minute, Second, fSecond: integer;
  Sign: integer;
  MaxDay, Count, i: integer;
begin
  GetDaySecond(Day, Hour, Minute, Second, fSecond);
  Sign := 0;
  Count := FLeadPrecision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;

  MaxDay := 1;
  for i := 1 to Count do
    MaxDay := MaxDay * 10;
  MaxDay := MaxDay - 1;

  Result := 0;
  if (Day < - MaxDay) or (Day > MaxDay) then begin
    Result := Result or OCI_INTER_INVALID_DAY;
    if Day < - MaxDay then
      Result := Result or OCI_INTER_DAY_BELOW_VALID;
  end;
  if Sign = 0 then begin
    if Day < 0 then
      Sign := -1
    else
    if Day > 0 then
      Sign := 1;
  end;
  if (Sign * Hour < 0) or (Sign * Hour > 23) then
    Result := Result or OCI_INTER_INVALID_HOUR;
  if Sign = 0 then begin
    if Hour < 0 then
      Sign := -1
    else
    if Hour > 0 then
      Sign := 1;
  end;
  if (Sign * Minute < 0) or (Sign * Minute > 59) then
    Result := Result or OCI_INTER_INVALID_MINUTE;
  if Sign = 0 then begin
    if Minute < 0 then
      Sign := -1
    else
    if Minute > 0 then
      Sign := 1;
  end;

  if (Sign * Second < 0) or (Sign * Second > 59) then
    Result := Result or OCI_INTER_INVALID_SECOND;

  if Sign = 0 then begin
    if Second < 0 then
      Sign := -1
    else
    if Second > 0 then
      Sign := 1;
  end;

  if Sign * fSecond < 0 then
    Result := Result or OCI_INTER_INVALID_FRACSEC;
end;

procedure TOCIIntervalDS.Parse(Value: string);
var
  i, Count: integer;
  Day, Hour, Minute, Second, fSecond: integer;
  MaxDay, MaxFSec: integer;
  CurPos: integer;
  Negative: boolean;
begin
  if IsNull then
    Alloc;

  Count := FLeadPrecision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;

  MaxDay := 1;
  for i := 1 to Count do
    MaxDay := MaxDay * 10;
  MaxDay := MaxDay - 1;

  Count := FFracPrecision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;

  MaxFSec := 1;
  for i := 1 to Count do
    MaxFSec := MaxFSec * 10;
  MaxFSec := MaxFSec - 1;

  CurPos := 1;
  Negative := GetSign(Value, CurPos);
  Day     := GetNextNumber(Value, CurPos, ' ', MaxDay,  SLeadPricisionSmall);
  Hour    := GetNextNumber(Value, CurPos, ':', 23,      SInvalidHour);
  Minute  := GetNextNumber(Value, CurPos, ':', 59,      SInvalidMinutes);
  Second  := GetNextNumber(Value, CurPos, '.', 59,      SInvalidSeconds);
  fSecond := GetNextNumber(Value, CurPos, '',  MaxFSec, '', True);
  if Negative then begin
    Day     := - Day    ;
    Hour    := - Hour   ;
    Minute  := - Minute ;
    Second  := - Second ;
    fSecond := - fSecond;
  end;
  SetDaySecond(Day, Hour, Minute, Second, fSecond);
end;

function TOCIIntervalDS.ToString(Precision, Scale: byte): string;
var
  i, Count: integer;
  MaxDay, MaxFSec: integer;
  Day, Hour, Minute, Second, fSecond: integer;
  Negative: integer;
begin
  if IsNull then begin
    Result := '';
    exit;
  end;

  Count := Precision;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Precision = 0 then
    Count := 1;

  MaxDay := 1;
  for i := 1 to Count do
    MaxDay := MaxDay * 10;
  MaxDay := MaxDay - 1;

  Count := Scale;
  if (Count < 0) or (Count > 9) then
    Count := 9;
  if Count = 0 then
    Count := 1;

  MaxFSec := 1;
  for i := 1 to Count do
    MaxFSec := MaxFSec * 10;
  MaxFSec := MaxFSec - 1;

  GetDaySecond(Day, Hour, Minute, Second, fSecond);
  Negative := 0;
  Result := '';
  PutNextNumber(Result, Day,    ' ', MaxDay, Negative);
  PutNextNumber(Result, Hour,   ':', 23, Negative);
  PutNextNumber(Result, Minute, ':', 59, Negative);
  PutNextNumber(Result, Second, '.', 59, Negative);
  PutNextNumber(Result, fSecond,'',  MaxFSec, Negative, True);
  if Negative >= 0 then
    Result := '+' + Result
  else
    Result := '-' + Result;
end;

procedure TOCIIntervalDS.SetDaySecond(Day, Hour, Minute, Second, fSecond: integer);
begin
  if IsNull then
    Alloc;
  FValue[0] := Byte(Day shr 24) xor $80;
  FValue[1] := Byte(Day shr 16);
  FValue[2] := Byte(Day shr 8);
  FValue[3] := Byte(Day);

  FValue[4] := Hour   + 60;
  FValue[5] := Minute + 60;
  FValue[6] := Second + 60;

  FValue[7] := Byte(fSecond shr 24) xor $80;
  FValue[8] := Byte(fSecond shr 16);
  FValue[9] := Byte(fSecond shr 8);
  FValue[10]:= Byte(fSecond);
end;

procedure TOCIIntervalDS.GetDaySecond(var Day: integer; var Hour: integer; var minute: integer;
 var Second: integer; var fSecond: integer);
begin
  if IsNull then begin
    Day     := 0;
    Hour    := 0;
    Minute  := 0;
    Second  := 0;
    fSecond := 0;
    exit;
  end;
  Day     :=
    (Integer(FValue[0] xor $80) shl 24) or
    (Integer(FValue[1]) shl 16) or
    (Integer(FValue[2]) shl 8) or
     FValue[3];

  Hour    := ShortInt(FValue[4]) - 60;
  Minute  := ShortInt(FValue[5]) - 60;
  Second  := ShortInt(FValue[6]) - 60;
  fSecond :=
    (Integer(FValue[7] xor $80) shl 24) or
    (Integer(FValue[8]) shl 16) or
    (Integer(FValue[9]) shl 8) or
    FValue[10];

end;

end.
