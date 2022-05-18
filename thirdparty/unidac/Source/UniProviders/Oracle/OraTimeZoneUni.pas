
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  OCIDatetime
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraTimeZoneUni;

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
  TimeStampLTZServerOffset = -420; // - 7 Hours

type
  OCITimeZoneInfo = record
    tzIndex: Integer;
    tzOffset: Integer;
  end;

  // Old convert functions
  function GetTimeZoneIndexOld(Code: Word): Integer;
  function GetVectorTimeZoneOffsetOld(const Vector: TBytes): Integer;
  function ConvertToLocalTimeOld(const Value: TBytes): TBytes; overload;
  function ConvertToLocalTimeOld(const Value: TBytes; Offset: Integer): TBytes; overload;
  function ConvertToServerTZOld(const Value: TBytes; UseLocalTZ: boolean): TBytes;
  function ConvertFromServerTZOld(const Value: TBytes): TBytes;

  // New convert functions
  function GetTimeZoneIndex(CodeH, CodeM: Byte): Integer;
  procedure DetectTimeZoneOffset(Value: TBytes; tzIndex: Integer; out tzHour: sb1; out tzMinute: sb1);
  function GetVectorTimeZoneInfo(const Vector: TBytes): OCITimeZoneInfo;
  function ConvertToLocalTime(const Value: TBytes): TBytes; overload;
  function ConvertToLocalTime(const Value: TBytes; TimeZoneOffset: Integer): TBytes; overload;
  function ConvertToLocalTime(const Value: TBytes; const TimeZoneInfo: OCITimeZoneInfo; TimeZoneOffset: Integer): TBytes; overload;
  function ConvertToServerTZ(const Value: TBytes; UseLocalTZ: boolean): TBytes;
  function ConvertFromServerTZ(const Value: TBytes): TBytes;


implementation

uses
  CRFunctions,
{$IFNDEF UNIDACPRO}
  OraDateTime;
{$ELSE}
  OraDateTimeUni;
{$ENDIF}

const
  tzCodes: array[0..376] of Word = (
    30848,45184,62592,17537,44160,6273,8321,9345,7297,54400,61568,57472,13441,129,45185,43137,18562,59522,54402,8323,
    45193,57474,64642,3203,47233,48258,7298,12418,13443,6275,23682,38017,14466,24706,62594,4227,11394,3202,39041,53377,
    1154,28802,17546,48265,58498,15491,56449,45186,30850,31874,5251,57473,25730,48281,50305,49281,51329,48257,9346,5250,
    34946,41089,50313,55426,9347,40065,52353,60546,38018,131,35970,16514,13442,43138,12419,59521,36994,36993,44161,56450,
    39042,46209,1155,63618,40066,6274,64641,1171,2179,61570,14467,39065,55425,47234,130,33922,16515,36996,64643,45188,
    2180,8324,10380,11396,6276,61571,54404,53380,48260,50307,16517,10373,18565,13445,17541,14469,20613,11397,32909,29829,
    27781,33925,32917,25733,30853,34957,28805,34949,31877,25741,32925,26757,27789,29837,32901,30861,31885,26765,33933,1163,
    56458,61578,139,57481,63625,64649,59529,1162,55433,2186,64657,10378,47237,2187,3215,38041,38033,25738,49285,45192,
    52365,37017,37009,1152,1168,16512,25728,26752,27776,17536,18560,19584,20608,21632,22656,23680,24704,1200,1184,15488,
    6272,5248,4224,3200,2176,14464,13440,12416,11392,10368,9344,8320,7296,1216,12422,1158,51333,28806,64645,59533,
    57477,16518,2182,60549,52357,134,62597,23686,17542,24710,15494,28814,50309,7302,20614,56453,11398,18566,13446,63621,
    59525,4230,3206,19590,3222,28822,27782,60545,17538,46210,2178,47242,10370,63617,8322,13454,47236,13444,12420,57476,
    15492,14468,9348,52355,51331,41092,21636,16516,4228,60555,60547,53379,39044,53387,43140,29828,58499,63619,52356,23702,
    5252,7300,10372,49283,56452,28804,62595,51332,22660,23684,19588,132,140,55428,6284,30852,27780,1156,50308,49284,
    31876,56451,32900,46212,17540,59523,28830,58501,21638,61573,53381,3214,55429,6278,14470,28838,22662,50317,50325,1160,
    1176,1208,1192,1224,63627,2199,14477,53382,56454,57478,61574,62598,8332,10388,34954,11404,16527,57480,48261,17554,
    16522,13450,39081,39073,39049,23695,24719,23687,24711,3207,34951,6279,7303,10375,2183,38023,13447,16519,8327,39047,
    25735,26759,22663,30855,29831,5255,14471,30871,9351,35975,40071,42119,14478,15502,59531,40097,40081,64651,17548,37004,
    23694,43145,45201,46217,38025,48273,37001,2191,50321,53385,39057,40073,40089,30863,1232,18574,46213
  );

{
  tzNames: array[0..376] of string = (
    'Africa/Algiers','Africa/Cairo','Africa/Casablanca','Africa/Ceuta','Africa/Djibouti','Africa/Freetown'
    ,'Africa/Johannesburg','Africa/Khartoum','Africa/Mogadishu','Africa/Nairobi','Africa/Nouakchott','Africa/Tripoli','Africa/Tunis'
    ,'Africa/Windhoek','America/Adak','America/Anchorage','America/Anguilla','America/Araguaina','America/Aruba','America/Asuncion','America/Atka','America/Belem','America/Boa_Vista','America/Bogota','America/Boise','America/Buenos_Aires'
    ,'America/Cambridge_Bay','America/Cancun','America/Caracas','America/Cayenne','America/Cayman','America/Chicago','America/Chihuahua','America/Costa_Rica','America/Cuiaba','America/Curacao','America/Dawson','America/Dawson_Creek','America/Denver','America/Detroit'
    ,'America/Edmonton','America/El_Salvador','America/Ensenada','America/Fort_Wayne','America/Fortaleza','America/Godthab','America/Goose_Bay','America/Grand_Turk','America/Guadeloupe','America/Guatemala','America/Guayaquil','America/Halifax'
    ,'America/Havana','America/Indiana/Indianapolis','America/Indiana/Knox','America/Indiana/Marengo','America/Indiana/Vevay','America/Indianapolis','America/Inuvik','America/Iqaluit','America/Jamaica','America/Juneau','America/Knox_IN','America/La_Paz','America/Lima'
    ,'America/Los_Angeles','America/Louisville','America/Maceio','America/Managua','America/Manaus','America/Martinique','America/Mazatlan','America/Mexico_City','America/Miquelon','America/Montevideo','America/Montreal','America/Montserrat','America/New_York','America/Nome'
    ,'America/Noronha','America/Panama','America/Phoenix','America/Porto_Acre','America/Porto_Velho','America/Puerto_Rico','America/Rankin_Inlet','America/Regina','America/Rio_Branco','America/Santiago','America/Sao_Paulo','America/Scoresbysund','America/Shiprock','America/St_Johns'
    ,'America/St_Thomas','America/Swift_Current','America/Tegucigalpa','America/Thule','Asia/Singapore','Asia/Taipei','Asia/Tashkent','Asia/Tbilisi','Asia/Tehran','Asia/Tel_Aviv','Asia/Tokyo','Asia/Ujung_Pandang','Asia/Urumqi','Asia/Vladivostok','Asia/Yakutsk','Asia/Yekaterinburg','Asia/Yerevan'
    ,'Atlantic/Azores','Atlantic/Bermuda','Atlantic/Canary','Atlantic/Faeroe','Atlantic/Madeira','Atlantic/Reykjavik','Atlantic/St_Helena','Atlantic/Stanley','Australia/ACT','Australia/Adelaide','Australia/Brisbane','Australia/Broken_Hill','Australia/Canberra','Australia/Darwin','Australia/Hobart'
    ,'Australia/LHI','Australia/Lindeman','Australia/Lord_Howe','Australia/Melbourne','Australia/North','Australia/NSW','Australia/Perth','Australia/Queensland','Australia/South','Australia/Sydney','Australia/Tasmania','Australia/Victoria','Australia/West','Australia/Yancowinna','Brazil/Acre'
    ,'Brazil/DeNoronha','Brazil/East','Brazil/West','Canada/Atlantic','Canada/Central','Canada/East-Saskatchewan','Canada/Eastern','Canada/Mountain','Canada/Newfoundland','Canada/Pacific','Canada/Saskatchewan','Canada/Yukon','CET','Chile/Continental','Chile/EasterIsland','CST'
    ,'CST6CDT','Cuba','EET','Egypt','Eire','EST','EST5EDT','Etc/GMT','Etc/GMT+0','Etc/GMT+1','Etc/GMT+10','Etc/GMT+11','Etc/GMT+12','Etc/GMT+2','Etc/GMT+3','Etc/GMT+4','Etc/GMT+5','Etc/GMT+6','Etc/GMT+7','Etc/GMT+8','Etc/GMT+9','Etc/GMT0','Etc/GMT-0','Etc/GMT-1','Etc/GMT-10','Etc/GMT-11'
    ,'Etc/GMT-12','Etc/GMT-13','Etc/GMT-14','Etc/GMT-2','Etc/GMT-3','Etc/GMT-4','Etc/GMT-5','Etc/GMT-6','Etc/GMT-7','Etc/GMT-8','Etc/GMT-9','Etc/Greenwich','Europe/Amsterdam','Europe/Athens','Europe/Belfast','Europe/Belgrade','Europe/Berlin','Europe/Bratislava','Europe/Brussels','Europe/Bucharest'
    ,'Europe/Budapest','Europe/Copenhagen','Europe/Dublin','Europe/Gibraltar','Europe/Helsinki','Europe/Istanbul','Europe/Kaliningrad','Europe/Kiev','Europe/Lisbon','Europe/Ljubljana','Europe/London','Europe/Luxembourg','Europe/Madrid','Europe/Minsk','Europe/Monaco','Europe/Moscow','Europe/Oslo','Europe/Paris'
    ,'Europe/Prague','Europe/Riga','Europe/Rome','Europe/Samara','Europe/San_Marino','Europe/Sarajevo','Europe/Simferopol','America/Thunder_Bay','America/Tijuana','America/Tortola','America/Vancouver','America/Virgin','America/Whitehorse','America/Winnipeg','America/Yellowknife','Arctic/Longyearbyen'
    ,'Asia/Aden','Asia/Almaty','Asia/Amman','Asia/Anadyr','Asia/Aqtau','Asia/Aqtobe','Asia/Baghdad','Asia/Bahrain','Asia/Baku','Asia/Bangkok','Asia/Beirut','Asia/Bishkek','Asia/Calcutta','Asia/Chongqing','Asia/Chungking','Asia/Dacca','Asia/Damascus','Asia/Dhaka','Asia/Dubai','Asia/Gaza','Asia/Harbin'
    ,'Asia/Hong_Kong','Asia/Irkutsk','Asia/Istanbul','Asia/Jakarta','Asia/Jayapura','Asia/Jerusalem','Asia/Kabul','Asia/Kamchatka','Asia/Karachi','Asia/Kashgar','Asia/Krasnoyarsk','Asia/Kuala_Lumpur','Asia/Kuching','Asia/Kuwait','Asia/Macao','Asia/Macau','Asia/Magadan','Asia/Makassar','Asia/Manila'
    ,'Asia/Muscat','Asia/Nicosia','Asia/Novosibirsk','Asia/Omsk','Asia/Qatar','Asia/Rangoon','Asia/Riyadh','Asia/Saigon','Asia/Seoul','Asia/Shanghai','Europe/Skopje','Europe/Sofia','Europe/Stockholm','Europe/Tallinn','Europe/Tirane','Europe/Vatican','Europe/Vienna','Europe/Vilnius','Europe/Warsaw'
    ,'Europe/Zagreb','Europe/Zurich','GB','GB-Eire','GMT','GMT+0','GMT0','GMT-0','Greenwich','Hongkong','HST','Iceland','Indian/Chagos','Indian/Christmas','Indian/Cocos','Indian/Mayotte','Indian/Reunion','Iran','Israel','Jamaica','Japan','Kwajalein','Libya','MET','Mexico/BajaNorte','Mexico/BajaSur'
    ,'Mexico/General','MST','MST7MDT','Navajo','NZ','NZ-CHAT','Pacific/Auckland','Pacific/Chatham','Pacific/Easter','Pacific/Fakaofo','Pacific/Fiji','Pacific/Gambier','Pacific/Guam','Pacific/Honolulu','Pacific/Johnston','Pacific/Kiritimati','Pacific/Kwajalein','Pacific/Marquesas','Pacific/Midway','Pacific/Niue'
    ,'Pacific/Norfolk','Pacific/Noumea','Pacific/Pago_Pago','Pacific/Pitcairn','Pacific/Rarotonga','Pacific/Saipan','Pacific/Samoa','Pacific/Tahiti','Pacific/Tongatapu','Pacific/Wake','Pacific/Wallis','Poland','Portugal','PRC','PST','PST8PDT','ROC','ROK','Singapore','Turkey','US/Alaska','US/Aleutian'
    ,'US/Arizona','US/Central','US/East-Indiana','US/Eastern','US/Hawaii','US/Indiana-Starke','US/Michigan','US/Mountain','US/Pacific','US/Pacific-New','US/Samoa','UTC','W-SU','WET'
  );
}

  tzHours: array[0..376] of ShortInt = (
    +01,+02,+00,+01,+03,+00,+02,+02,+03,+03,+00,+02,+01,+02,-10,-09,-04,-03,-04,-03,
    -10,-03,-04,-05,-07,-03,-07,-06,-04,-03,-05,-06,-07,-06,-04,-04,-08,-07,-07,-05,
    -07,-06,-08,-05,-03,-03,-04,-05,-04,-06,-05,-04,-05,-05,-05,-05,-05,-05,-07,-05,
    -05,-09,-05,-04,-05,-08,-05,-03,-06,-04,-04,-07,-06,-03,-03,-05,-04,-05,-09,-02,
    -05,-07,-05,-04,-04,-06,-06,-05,-03,-02,-01,-07,-03,-04,-06,-06,-04,+08,+08,+05,
    +04,+03,+02,+09,+08,+08,+10,+09,+05,+04,-01,-04,+00,+00,+00,+00,+00,-03,+11,+10,
    +10,+10,+11,+09,+11,+11,+10,+11,+11,+09,+11,+08,+10,+10,+11,+11,+11,+08,+10,-05,
    -02,-02,-04,-04,-06,-06,-05,-07,-03,-08,-06,-08,+01,-03,-05,-06,-06,-05,+02,+02,
    +00,-05,-05,+00,+00,-01,-10,-11,-12,-02,-03,-04,-05,-06,-07,-08,-09,+00,+00,+01,
    +10,+11,+12,+13,+14,+02,+03,+04,+05,+06,+07,+08,+09,+00,+01,+02,+00,+01,+01,+01,
    +01,+02,+01,+01,+00,+01,+02,+02,+02,+02,+00,+01,+00,+01,+01,+02,+01,+03,+01,+01,
    +01,+02,+01,+04,+01,+01,+02,-05,-08,-04,-08,-04,-08,-06,-07,+01,+03,+06,+02,+12,
    +04,+05,+03,+03,+04,+07,+02,+05,+05,+08,+08,+06,+02,+06,+04,+02,+08,+08,+08,+02,
    +07,+09,+02,+04,+12,+05,+08,+07,+08,+08,+03,+08,+08,+11,+08,+08,+04,+02,+06,+06,
    +03,+06,+03,+07,+09,+08,+01,+02,+01,+02,+01,+01,+01,+02,+01,+01,+01,+00,+00,+00,
    +00,+00,+00,+00,+08,-10,+00,+05,+07,+06,+03,+04,+03,+02,-05,+09,+12,+02,+01,-08,
    -07,-06,-07,-07,-07,+13,+13,+13,+13,-05,-10,+12,-09,+10,-10,-10,+14,+12,-09,-11,
    -11,+11,+11,-11,-08,-10,+10,-11,-10,+13,+12,+12,+01,+00,+08,-08,-08,+08,+09,+08,
    +02,-09,-10,-07,-06,-05,-05,-10,-05,-05,-07,-08,-08,-11,+00,+03,+00
  );

  tzMinutes: array[0..376] of ShortInt = (
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,30,00,00,00,00,00,00,00,
    00,30,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,30,
    00,30,00,30,00,00,00,00,00,30,00,00,00,30,00,00,00,00,30,00,
    00,00,00,00,00,00,00,00,30,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,30,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,30,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,30,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,30,00,00,30,00,00,00,00,00,00,00,
    00,00,00,00,00,00,45,00,45,00,00,00,00,00,00,00,00,00,30,00,
    00,30,00,00,30,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,
    00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
  );

type
  TOCITransition = record
    DateTimeInUtc: TDateTime;
//    DateTimeInLocal: TDateTime;
    Offset: Integer;
//    DstDuration: Integer;
//    Dst: Boolean;
  end;
  POCITransition = ^TOCITransition;

  TOCITransitionArr = array[0..32767] of TOCITransition;
  POCITransitionArr = ^TOCITransitionArr;

  TOCITimeZone = record
    Code: Word;
    Name: string;
    HighIndex: Integer;
    Transitions: POCITransitionArr;
  end;
  POCITimeZone = ^TOCITimeZone;

{$I OraTimeZone.inc}

function GetTimeZoneIndexOld(Code: Word): Integer;
var
  i: Integer;
begin
  for i := 0 to High(tzCodes) do
    if tzCodes[i] = Code then begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

function GetVectorTimeZoneOffsetOld(const Vector: TBytes): Integer;
var
  tzCode: Integer;
  tzIndex: Integer;
  tzHour: Integer;
  tzMinute: Integer;
begin
  if Length(Vector) >= 6 then
    if Vector[2] and $80 <> 0 then begin
      tzCode := Vector[3] shl 8 + Vector[2];
      tzIndex := GetTimeZoneIndexOld(tzCode);
      if tzIndex >= 0 then begin
        if Vector[4] and $80 <> 0 then
          tzHour := Vector[4] - $B5
        else
          tzHour := tzHours[tzIndex];
        if tzHour >= 0 then
          tzMinute := tzMinutes[tzIndex]
        else
          tzMinute := -tzMinutes[tzIndex];
        Result := tzHour * 60 + tzMinute;
      end
      else
        Result := GetLocalTimeZoneOffset;
    end
    else begin
      tzHour := Vector[4] - $3C;
      tzMinute := Vector[5] - $3C;
      Result := tzHour * 60 + tzMinute;
    end
  else
    Result := GetLocalTimeZoneOffset;
end;

function ConvertToLocalTimeOld(const Value: TBytes): TBytes;
var
  tzHour: sb1;
  tzMinute: sb1;
begin
  if Length(Value) >= 13 then begin
    // get TZ offset
    if Value[11] and $40 = 0 then begin
      tzHour   := Value[11] - 20;
      tzMinute := Value[12] - 60;
      Result := ConvertToLocalTimeOld(Value, tzHour * 60 + tzMinute);
    end
    else begin
      SetLength(Result, 11);
      Move(Value[0], Result[0], 11);
    end;
  end
  else
    Result := Value;
end;

function ConvertToLocalTimeOld(const Value: TBytes; Offset: Integer): TBytes;
var
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
  newLen: Integer;
begin
  if Length(Value) <= 7 then
    SetLength(Result, 7)
  else
    SetLength(Result, 11);

  ExtractDate(Value, year, month, day);
  ExtractTime(Value, hour, minute, sec, fsec);
  ChangeTimeZoneOffset(year, month, day, hour, minute, Offset, True);
  UpdateDate(Result, year, month, day);
  UpdateTime(Result, hour, minute, sec, fsec);

  if Length(Value) >= 13 then begin
    // trim value length
    newLen := GetTimeZoneTrimmedLen(Result, OCI_DTYPE_TIMESTAMP);
    if newLen < Length(Result) then
      SetLength(Result, newLen);
  end;
end;

function ConvertToServerTZOld(const Value: TBytes; UseLocalTZ: boolean): TBytes;
var
  tzHour: sb1;
  tzMinute: sb1;
  tzCode: ub2;
  tzIndex: Integer;
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
begin
  if UseLocalTZ and (Length(Value) >= 13) then begin
    SetLength(Result, Length(Value));

    if (Value[11] and $80 <> 0) then begin
      tzCode := Value[12] shl 8 + Value[11];
      tzIndex := GetTimeZoneIndexOld(tzCode);
      if tzIndex >= 0 then begin
        tzHour := tzHours[tzIndex];
        if tzHour >= 0 then
          tzMinute := tzMinutes[tzIndex]
        else
          tzMinute := -tzMinutes[tzIndex];
      end
      else begin
        tzHour := 0;
        tzMinute := 0;
      end;

      Result[11] := Value[11];
      Result[12] := Value[12] or $03;
    end
    else begin
      tzHour   := Value[11] - 20;
      tzMinute := Value[12] - 60;

      Result[11] := Value[11] or $40;
      Result[12] := Value[12];
    end;

    if (tzHour <> 0) or (tzMinute <> 0) then begin
      ExtractDate(Value, year, month, day);
      ExtractTime(Value, hour, minute, sec, fsec);
      ChangeTimeZoneOffset(year, month, day, hour, minute, tzHour * 60 + tzMinute, True);
      UpdateDate(Result, year, month, day);
      UpdateTime(Result, hour, minute, sec, fsec);
    end
    else
      Move(Value[0], Result[0], 11);
  end
  else
    Result := Value;
end;

function ConvertFromServerTZOld(const Value: TBytes): TBytes;
var
  tzHour: sb1;
  tzMinute: sb1;
  tzCode: ub2;
  tzIndex: Integer;
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
begin
  if Length(Value) >= 13 then begin
    SetLength(Result, Length(Value));

    tzHour := 0;
    tzMinute := 0;

    if Value[11] and $80 <> 0 then begin
      Result[11] := Value[11];

      if Value[12] and $02 <> 0 then begin
        Result[12] := Value[12] and $FC;

        tzCode := Result[12] shl 8 + Result[11];
        tzIndex := GetTimeZoneIndexOld(tzCode);
        if tzIndex >= 0 then begin
          tzHour := tzHours[tzIndex];
          if tzHour >= 0 then
            tzMinute := tzMinutes[tzIndex]
          else
            tzMinute := -tzMinutes[tzIndex];
        end;
      end
      else
        Result[12] := Value[12];
    end
    else if Value[11] and $40 <> 0 then begin
      Result[11] := Value[11] and $3F;
      Result[12] := Value[12];

      tzHour := Value[11] - 20;
      tzMinute := Value[12] - 60;
    end
    else begin
      Result[11] := Value[11];
      Result[12] := Value[12];
    end;

    if (tzHour <> 0) or (tzMinute <> 0) then begin
      ExtractDate(Value, year, month, day);
      ExtractTime(Value, hour, minute, sec, fsec);
      ChangeTimeZoneOffset(year, month, day, hour, minute, tzHour * 60 + tzMinute, False);
      UpdateDate(Result, year, month, day);
      UpdateTime(Result, hour, minute, sec, fsec);
    end
    else
      Move(Value[0], Result[0], 11);
  end
  else
    Result := Value;
end;

function GetTimeZoneIndex(CodeH, CodeM: Byte): Integer;
var
  i: Integer;
  Code: Word;
begin
  Code := (Word(CodeH and $7F) shl 6) + (Word(CodeM and $FC) shr 2);

  for i := 0 to Length(OCITimeZones) - 1 do
    if OCITimeZones[i].Code = Code then begin
      Result := i;
      Exit;
    end;

  Result := -1;
end;

procedure DetectTimeZoneOffset(Value: TBytes; tzIndex: Integer; out tzHour: sb1; out tzMinute: sb1);
var
  i: Integer;
  dt: TDateTime;
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
  TimeZonePtr: POCITimeZone;
  TransitionPtr: POCITransition;
  TransitionPtr2: POCITransition;
begin
  ExtractDate(Value, year, month, day);
  ExtractTime(Value, hour, minute, sec, fsec);
  TimeZonePtr := @OCITimeZones[tzIndex];
  if year < 1800 then
    TransitionPtr := @TimeZonePtr.Transitions[0]
  else if year > 2100 then
    TransitionPtr := @TimeZonePtr.Transitions[TimeZonePtr.HighIndex]
  else begin
    TransitionPtr := @TimeZonePtr.Transitions[0];
    dt := Encodedate(year, month, day) + EncodeTime(hour, minute, sec, 0); // ignore MSec
    i := 1;
    while i <= TimeZonePtr.HighIndex do begin
      TransitionPtr2 := @TimeZonePtr.Transitions[i];
      if dt < TransitionPtr2.DateTimeInUtc then
        Break;
      TransitionPtr := TransitionPtr2;
      Inc(i);
    end;
  end;
  tzHour := TransitionPtr.Offset div 60;
  tzMinute := TransitionPtr.Offset mod 60;
end;

function GetVectorTimeZoneInfo(const Vector: TBytes): OCITimeZoneInfo;
var
  tzHour: Integer;
  tzMinute: Integer;
begin
  if Length(Vector) >= 6 then
    if Vector[2] and $80 <> 0 then begin
      Result.tzIndex := GetTimeZoneIndex(Vector[2], Vector[3]);
      if Result.tzIndex >= 0 then
        Result.tzOffset := 0
      else
        Result.tzOffset := GetLocalTimeZoneOffset;
    end
    else begin
      tzHour := Vector[4] - $3C;
      tzMinute := Vector[5] - $3C;
      Result.tzIndex := -1;
      Result.tzOffset := (tzHour * 60 + tzMinute);
    end
  else begin
    Result.tzIndex := -1;
    Result.tzOffset := GetLocalTimeZoneOffset;
  end;
end;

function ConvertToLocalTime(const Value: TBytes): TBytes;
var
  tzHour: sb1;
  tzMinute: sb1;
begin
  if Length(Value) >= 13 then begin
    // get TZ offset
    if Value[11] and $40 = 0 then begin
      tzHour   := Value[11] - 20;
      tzMinute := Value[12] - 60;
      Result := ConvertToLocalTime(Value, tzHour * 60 + tzMinute);
    end
    else begin
      SetLength(Result, 11);
      Move(Value[0], Result[0], 11);
    end;
  end
  else
    Result := Value;
end;

function ConvertToLocalTime(const Value: TBytes; TimeZoneOffset: Integer): TBytes;
var
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
  newLen: Integer;
begin
  if Length(Value) <= 7 then
    SetLength(Result, 7)
  else
    SetLength(Result, 11);

  ExtractDate(Value, year, month, day);
  ExtractTime(Value, hour, minute, sec, fsec);
  ChangeTimeZoneOffset(year, month, day, hour, minute, TimeZoneOffset, True);
  UpdateDate(Result, year, month, day);
  UpdateTime(Result, hour, minute, sec, fsec);

  if Length(Value) >= 13 then begin
    // trim value length
    newLen := GetTimeZoneTrimmedLen(Result, OCI_DTYPE_TIMESTAMP);
    if newLen < Length(Result) then
      SetLength(Result, newLen);
  end;
end;

function ConvertToLocalTime(const Value: TBytes; const TimeZoneInfo: OCITimeZoneInfo; TimeZoneOffset: Integer): TBytes;
var
  tzHour, tzMinute: sb1;
begin
  if TimeZoneInfo.tzIndex >= 0 then begin
    DetectTimeZoneOffset(Value, TimeZoneInfo.tzIndex, tzHour, tzMinute);
    Result := ConvertToLocalTime(Value, (tzHour * 60 + tzMinute) + TimeZoneOffset);
  end
  else
    Result := ConvertToLocalTime(Value, TimeZoneInfo.tzOffset + TimeZoneOffset);
end;

function ConvertToServerTZ(const Value: TBytes; UseLocalTZ: boolean): TBytes;
var
  tzHour: sb1;
  tzMinute: sb1;
  tzIndex: Integer;
  year: sb2; month: ub1; day: ub1;
  hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
begin
  if UseLocalTZ and (Length(Value) >= 13) then begin
    SetLength(Result, Length(Value));

    if (Value[11] and $80 <> 0) then begin
      tzIndex := GetTimeZoneIndex(Value[11], Value[12]);
      if tzIndex >= 0 then
        DetectTimeZoneOffset(Value, tzIndex, tzHour, tzMinute)
      else begin
        tzHour := 0;
        tzMinute := 0;
      end;

      Result[11] := Value[11];
      Result[12] := Value[12] or $03;
    end
    else begin
      tzHour   := Value[11] - 20;
      tzMinute := Value[12] - 60;

      Result[11] := Value[11] or $40;
      Result[12] := Value[12];
    end;

    if (tzHour <> 0) or (tzMinute <> 0) then begin
      ExtractDate(Value, year, month, day);
      ExtractTime(Value, hour, minute, sec, fsec);
      ChangeTimeZoneOffset(year, month, day, hour, minute, tzHour * 60 + tzMinute, True);
      UpdateDate(Result, year, month, day);
      UpdateTime(Result, hour, minute, sec, fsec);
    end
    else
      Move(Value[0], Result[0], 11);
  end
  else
    Result := Value;
end;

function ConvertFromServerTZ(const Value: TBytes): TBytes;

  procedure ConvertTZ(tzHour: sb1; tzMinute: sb1);
  var
    year: sb2; month: ub1; day: ub1;
    hour: ub1; minute: ub1; sec: ub1; fsec: ub4;
  begin
    ExtractDate(Value, year, month, day);
    ExtractTime(Value, hour, minute, sec, fsec);
    ChangeTimeZoneOffset(year, month, day, hour, minute, tzHour * 60 + tzMinute, False);
    UpdateDate(Result, year, month, day);
    UpdateTime(Result, hour, minute, sec, fsec);
  end;

var
  tzHour: sb1;
  tzMinute: sb1;
  tzCode: ub2;
  tzIndex: Integer;
begin
  if Length(Value) >= 13 then begin
    if Value[11] and $80 <> 0 then begin
      if Value[12] and $02 <> 0 then
        tzCode := (Value[12] and $FC) shl 8 + Value[11]
      else
        tzCode := Value[12] shl 8 + Value[11];
      tzIndex := GetTimeZoneIndexOld(tzCode);
      if tzIndex >= 0 then begin
        tzHour := tzHours[tzIndex];
        if tzHour >= 0 then
          tzMinute := tzMinutes[tzIndex]
        else
          tzMinute := -tzMinutes[tzIndex];

        SetLength(Result, Length(Value));
        Result[11] := tzHour + 20;
        Result[12] := tzMinute + 60;
        Move(Value[0], Result[0], 11);
      end
      else
        Result := Value;
    end
    else if Value[11] and $40 <> 0 then begin
      SetLength(Result, Length(Value));
      Result[11] := Value[11] and $3F;
      Result[12] := Value[12];

      tzHour := Value[11] - 20;
      tzMinute := Value[12] - 60;
      if (tzHour <> 0) or (tzMinute <> 0) then
        ConvertTZ(tzHour, tzMinute)
      else
        Move(Value[0], Result[0], 11);
    end
    else
      Result := Value;
  end
  else
    Result := Value;
end;

end.
