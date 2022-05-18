
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFUtilsUni;

interface

{$IFDEF DBFENGINE}

uses
  SysUtils, FMTBcd,
  CRTypes, CRFunctions,
  MemData, CRVirtualData,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs;
{$ELSE}
  DBFConstsUni, DBFStructsUni;
{$ENDIF}

function FastSwap(Value: SmallInt): SmallInt; overload;
function FastSwap(Value: Word): Word; overload;
function FastSwap(Value: Integer): Integer; overload;
function FastSwap(Value: Cardinal): Cardinal; overload;
function FastSwap(Value: Int64): Int64; overload;
function FastSwap(Value: Double): Double; overload;
{$IFDEF VER7P}
function FastSwap(Value: UInt64): UInt64; overload;
{$ENDIF}
function DBase7DataToDouble(D: double): double;
function DoubleToDBase7Data(D: double): double;
function DBase7DataToCurrency(C: Currency): Currency;
function CurrencyToDBase7Data(C: Currency): Currency;
function DBase7DataToInteger(I: integer): integer;
function IntegerToDBase7Data(I: integer): integer;
function VfpDataToInteger(I: integer): integer;
function IntegerToVfpData(I: integer): integer;
function DBaseTimeStampToDateTime(TS: Int64): TDateTime;
function DateTimeToDBaseTimeStamp(DT: TDateTime): Int64;

function DB3NameToString(const DBFField: PDBFField): string;
function DB7NameToString(const DBFField: PDBFField): string;
function Ver3TagNameToString(const Tag: TMDXTag): string;
function Ver2TagNameToString(const Tag: TMDXTag): string;

function Int64ToBcd( const AValue: Int64): TBcd;

function GetVirtualTypeSize(const Value: TVirtualValue): Cardinal;
function GetVirtualTypeAsBytes(const Value: TVirtualValue): TBytes;

function LanguageIdToCodePage(LanguageId: byte): TDBFCodePage;
function GetLanguageDriverName(CodePage: TDBFCodePage; DBFFormat: TDBFFormat): string;
function GetCodepage(LanguageDriverName: string): Cardinal;

function FindFileByName(const FullFileName: string): string;
{$ENDIF}

implementation

{$IFDEF DBFENGINE}

{ FastSwap }

function FastSwap(Value: SmallInt): SmallInt;
{$IFDEF PUREPASCAL}
begin
  Result := (Value and $FF shl 8) or (Value and $FF00 shr 8);
{$ELSE}
asm
{$IFDEF CPU64}
      MOV   AX, Value
{$ENDIF}
      XCHG  AH, AL
{$ENDIF PUREPASCAL}
end;

function FastSwap(Value: Word): Word;
{$IFDEF PUREPASCAL}
begin
  Result := (Value and $FF shl 8) or (Value and $FF00 shr 8);
{$ELSE}
asm
{$IFDEF CPU64}
      MOV   AX, Value
{$ENDIF}
      XCHG  AH, AL
{$ENDIF PUREPASCAL}
end;

function FastSwap(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value and $FF shl 24) or (Value and $FF00 shl 8) or (Value and $FF0000 shr 8) or (Value and Integer($FF000000) shr 24);
{$ELSE}
asm
{$IFDEF CPU64}
      MOV   EAX, Value
{$ENDIF}
      BSWAP  EAX
{$ENDIF PUREPASCAL}
end;

function FastSwap(Value: Cardinal): Cardinal;
{$IFDEF PUREPASCAL}
begin
  Result := (Value and $FF shl 24) or (Value and $FF00 shl 8) or (Value and $FF0000 shr 8) or (Value and $FF000000 shr 24);
{$ELSE}
asm
{$IFDEF CPU64}
      MOV   EAX, Value
{$ENDIF}
      BSWAP  EAX
{$ENDIF PUREPASCAL}
end;

function FastSwap(Value: Int64): Int64;
begin
  Int64Rec(Result).Lo := FastSwap(Int64Rec(Value).Hi);
  Int64Rec(Result).Hi := FastSwap(Int64Rec(Value).Lo);
end;

function FastSwap(Value: Double): Double;
begin
  Int64Rec(Result).Lo := FastSwap(Int64Rec(Value).Hi);
  Int64Rec(Result).Hi := FastSwap(Int64Rec(Value).Lo);
end;

{$IFDEF VER7P}
function FastSwap(Value: UInt64): UInt64;
begin
  Int64Rec(Result).Lo := FastSwap(Int64Rec(Value).Hi);
  Int64Rec(Result).Hi := FastSwap(Int64Rec(Value).Lo);
end;
{$ENDIF}

function DBase7DataToDouble(D: double): double;
var
  i64: Int64;
  sign: boolean;
begin
  i64 := PInt64(@d)^;
  i64 := FastSwap(i64);
  if i64 <> 0 then begin // else -NAN
    sign := (i64 and $8000000000000000) <> 0;
    if sign then
      i64 := i64 xor $8000000000000000
    else
      i64 := not i64;
  end;
  Result := PDouble(@i64)^;
end;

function DoubleToDBase7Data(D: double): double;
var
  i64: Int64;
begin
  if D >= 0 then
    i64 := PInt64(@D)^ xor $8000000000000000
  else
    i64 := not PInt64(@D)^;
  i64 := FastSwap(i64);
  Result := PDouble(@i64)^;
end;

function DBase7DataToCurrency(C: Currency): Currency;
var
  i64: Int64;
begin
  i64 := PInt64(@C)^;
  i64 := FastSwap(i64);
  i64 := i64 xor $8000000000000000;
  Result := PCurrency(@i64)^;
end;

function CurrencyToDBase7Data(C: Currency): Currency;
var
  i64: Int64;
begin
  i64 := PInt64(@C)^ xor $8000000000000000;
  i64 := FastSwap(i64);
  Result := PCurrency(@i64)^;
end;

function DBase7DataToInteger(I: integer): integer;
begin
  // 4 bytes. Leftmost bit used to indicate sign, 0 negative.
  if I <> 0 then // VFP xors 0, todo check dBase7
    Result := integer(FastSwap(I) xor integer($80000000))
  else
    Result := 0;
end;

function IntegerToDBase7Data(I: integer): integer;
begin
  if I = integer($80000000) then
    raise Exception.Create('Integer value $80000000 not allowed')
  else
  if I <> 0 then // VFP xors 0, todo check dBase7
    Result := FastSwap(I xor integer($80000000))
  else
    Result := 0;
end;

function VfpDataToInteger(I: integer): integer;
begin
  Result := integer(FastSwap(I) xor integer($80000000))
end;

function IntegerToVfpData(I: integer): integer;
begin
  Result := FastSwap(I xor integer($80000000))
end;

function BDETimeStampToDateTime(BdeTS: Int64): TDateTime;
var
  ts: TTimeStamp;
begin
  if BdeTS <> 0 then begin
    ts := MSecsToTimeStamp(BdeTS);
    Result := TimeStampToDateTime(ts)
  end
  else
    Result := 0;
end;

function DBaseTimeStampToDateTime(TS: Int64): TDateTime;
var
  i64: Int64;
begin
  if TS <> 0 then begin
    i64 := FastSwap(TS);
    Result := BDETimeStampToDateTime(Trunc(PDouble(@i64)^));
  end
  else
    Result := 0;
end;

function DateTimeToBDETimeStamp(DT: TDateTime): Double;
var
  ts: TTimeStamp;
begin
  ts := DateTimeToTimeStamp(DT);
  Result := TimeStampToMSecs(ts);
end;

function DateTimeToDBaseTimeStamp(DT: TDateTime): Int64;
begin
  if DT <> 0 then begin
    PDouble(@Result)^ := DateTimeToBDETimeStamp(DT);
    Result := FastSwap(Result);
  end
  else
    Result := 0;
end;

function DB3NameToString(const DBFField: PDBFField): string;
{$IFDEF NEXTGEN}
var
  AStr: AnsiString;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  Result := string(DBFField^.DB3Name);
{$ELSE}
  if Length(DBFField^.DB3Name) > 0 then begin
    AStr.SetLength(Length(DBFField^.DB3Name));
    Move(DBFField^.DB3Name[0], AStr.Ptr^, AStr.Length);
    Result := string(AStr);
  end
  else
    Result := '';
{$ENDIF}
end;

function DB7NameToString(const DBFField: PDBFField): string;
{$IFDEF NEXTGEN}
var
  AStr: AnsiString;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  Result := string(DBFField^.DB7Name);
{$ELSE}
  if Length(DBFField^.DB7Name) > 0 then begin
    AStr.SetLength(Length(DBFField^.DB7Name));
    Move(DBFField^.DB7Name[0], AStr.Ptr^, AStr.Length);
    Result := string(AStr);
  end
  else
    Result := '';
{$ENDIF}
end;

function Ver3TagNameToString(const Tag: TMDXTag): string;
{$IFDEF NEXTGEN}
var
  AStr: AnsiString;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  Result := string(Tag.Ver3TagName);
{$ELSE}
  if Length(Tag.Ver3TagName) > 0 then begin
    AStr.SetLength(Length(Tag.Ver3TagName));
    Move(Tag.Ver3TagName[0], AStr.Ptr^, AStr.Length);
    Result := string(AStr);
  end
  else
    Result := '';
{$ENDIF}
end;

function Ver2TagNameToString(const Tag: TMDXTag): string;
{$IFDEF NEXTGEN}
var
  AStr: AnsiString;
{$ENDIF}
begin
{$IFNDEF NEXTGEN}
  Result := string(Tag.Ver2TagName);
{$ELSE}
  if Length(Tag.Ver2TagName) > 0 then begin
    AStr.SetLength(Length(Tag.Ver2TagName));
    Move(Tag.Ver2TagName[0], AStr.Ptr^, AStr.Length);
    Result := string(AStr);
  end
  else
    Result := '';
{$ENDIF}
end;

function Int64ToBcd( const AValue: Int64): TBcd;
begin
  Result := StrToBcd(IntToStr(AValue));
end;

function GetVirtualTypeSize(const Value: TVirtualValue): Cardinal;
var
  v_type: Word;
  Blob: TBlob;
begin
  case Value.ValueType of
    vrString,
    vrAnsiString,
    vrWideString:
      Result := Length(string(Value.Value));
    vrBlob: begin
      v_type := TVarData(Value.Value).VType;
      if (v_type and varByRef) <> 0 then begin
        Blob := TVarData(Value.Value).VPointer;
        Result := Blob.Size;
      end
      else if v_type = varArray + varByte then
        Result := TVarData(Value.Value).VArray.Bounds[0].ElementCount
      else
        raise Exception.CreateFmt('Unknown v_type %d', [v_type]);
    end;
  else
    raise Exception.CreateFmt('Unknown ValueType %d', [integer(Value.ValueType)]);
  end;
end;

function GetVirtualTypeAsBytes(const Value: TVirtualValue): TBytes;
var
  aStr: AnsiString;
  v_type: Word;
  Blob: TBlob;
begin
  case Value.ValueType of
    vrString: begin
      aStr := AnsiString(Value.Value);
      SetLength(Result, LengthA(aStr));
      if LengthA(aStr) > 0 then
        Move(PAnsiChar(aStr)^, Result[0], LengthA(aStr));
    end;
    vrAnsiString: begin
      aStr := Value.AnsiStrValue;
      SetLength(Result, LengthA(aStr));
      if LengthA(aStr) > 0 then
        Move(PAnsiChar(aStr)^, Result[0], LengthA(aStr));
    end;
    vrBlob: begin
      v_type := TVarData(Value.Value).VType;
      if (v_type and varByRef) <> 0 then begin
        Blob := TVarData(Value.Value).VPointer;
        Result := Blob.AsBytes;
      end
      else if v_type = varArray + varByte then begin
        SetLength(Result, TVarData(Value.Value).VArray.Bounds[0].ElementCount);
        if TVarData(Value.Value).VArray.Bounds[0].ElementCount > 0 then
          Move(TVarData(Value.Value).VArray.Data^, Result[0], TVarData(Value.Value).VArray.Bounds[0].ElementCount);
      end
      else
        raise Exception.CreateFmt('Unknown v_type %d', [v_type]);
    end;
  else
    raise Exception.CreateFmt('Unknown ValueType %d', [integer(Value.ValueType)]);
  end;
end;

function LanguageIdToCodePage(LanguageId: byte): TDBFCodePage;
begin
  case LanguageId of
    $01: Result := dpUnitedStatesOEM;
    $6A: Result := dpGreekDOS;
    $02: Result := dpWesternEuropeanDOS;
    $64: Result := dpCentralEuropeanDOS;
    $6B: Result := dpTurkishDOS;
    $24: Result := dpPortugueseDOS;
    $67: Result := dpIcelandicDOS;
    $6C: Result := dpFrenchCanadianDOS;
    $66: Result := dpNordicDOS;
    $65: Result := dpCyrillicDOS;
    $7C: Result := dpThai;
    $7B: Result := dpJapanese;
    $7A: Result := dpChineseSimplified;
    $78: Result := dpChineseTraditional;
    $79: Result := dpKorean;
    $C8: Result := dpCentralEuropeanANSI;
    $C9: Result := dpCyrillicANSI;
    $03: Result := dpWesternEuropeanANSI;
    $CB: Result := dpGreekANSI;
    $CA: Result := dpTurkishANSI;
    $7D: Result := dpHebrewANSI;
    $7E: Result := dpArabicANSI;
    $CC: Result := dpBalticANSI;
    $04: Result := dpStandartMacintosh;
    $08: Result := dpDanishDOS;
    $09: Result := dpDutchDOS;
    $0A: Result := dpDutchDOSInternational;
    $0B: Result := dpFinnishDOS;
    $0D: Result := dpFrenchDOS;
    $0E: Result := dpFrenchDOSInternational;
    $0F: Result := dpGermanDOS;
    $10: Result := dpGermanDOSInternational;
    $11: Result := dpItalianDOS;
    $12: Result := dpItalianDOSInternational;
    $13: Result := dpJapaneseShiftJIS;
    $14: Result := dpSpanishDOSInternational;
    $15: Result := dpSwedishDOS;
    $16: Result := dpSwedishDOSInternational;
    $17: Result := dpNorwegianDOS;
    $18: Result := dpSpanishDOS;
    $19: Result := dpEnglishDOSGreatBritain;
    $1A: Result := dpEnglishDOSGreatBritainInt;
    $1B: Result := dpEnglishDOSUnitedStates;
    $1C: Result := dpFrenchDOSCanada;
    $1D: Result := dpFrenchDOSCanadaInt;
    $1F: Result := dpCzechDOS;
    $22: Result := dpHungarianDOS;
    $23: Result := dpPolishDOS;
    $25: Result := dpPortugueseDOSInternational;
    $26: Result := dpRussianDOS;
    $37: Result := dpEnglishDOSUnitedStatesInt;
    $40: Result := dpRomanianDOS;
    $4D: Result := dpChineseGBK;
    $4E: Result := dpKoreanANSI;
    $4F: Result := dpChineseBig5;
    $50: Result := dpThaiANSI;
    $58: Result := dpWesternEuropeanANSI;
    $59: Result := dpSpanishANSI;
    $68: Result := dpKamenickyCzechDOS;
    $69: Result := dpMazoviaPolishDOS;
    $86: Result := dpGreekDOS;
    $87: Result := dpSlovenianDOS;
    $88: Result := dpTurkishOEM;
    $96: Result := dpRussianMacintosh;
    $97: Result := dpEasternEuropianMacintosh;
    $98: Result := dpGreekMacintosh;
  else
    Result := dpDefault;
  end;
end;

function GetLanguageDriverName(CodePage: TDBFCodePage; DBFFormat: TDBFFormat): string;
var
  Locale: Cardinal;
  CodePageStr: string;
begin
  Result := '';

  if CodePage <> dpDefault then begin
    Locale := CodePageLocale[CodePage];
    CodePageStr := CodePageString[CodePage];

    if (DBFFormat = dfFoxPro2) or (DBFFormat = dfVisualFoxPro) then begin
      Result := 'FOX' + CodePageStr;
      if (Locale >= 1250) and (Locale <= 1257) then
        Result := Result + 'WIN'
      else
        Result := Result + IntToStr(Locale);
    end
    else begin
      Result := 'DB';
      if (Locale >= 1250) and (Locale <= 1257) then
        Result := Result + 'WIN'
      else
        Result := Result + IntToStr(Locale);
      Result := Result + CodePageStr;
    end;
  end;

  if Result = '' then
    Result := DefLanguageDriverName;
end;

function GetCodepage(LanguageDriverName: string): Cardinal;
var
  i: integer;

  function IsNumber(Ch: char): boolean;
  begin
    case Ch of
      '0'..'9':
        Result := True;
      else
        Result := False;
    end;
  end;

begin
  Result := 0;
  if Length(LanguageDriverName) < 3 then
    Exit;

  if SameText(Copy(LanguageDriverName, 1, 2), 'DB') then begin
    Delete(LanguageDriverName, 1, 2);
    if SameText(Copy(LanguageDriverName, 1, 3), 'WIN') then begin
      Delete(LanguageDriverName, 1, 3);
      Result := StrToIntDef(LanguageDriverName, 0);
    end
    else begin
      i := 1;
      while (i <= Length(LanguageDriverName)) and IsNumber(LanguageDriverName[i]) do
        Inc(i);
      Result := StrToIntDef(Copy(LanguageDriverName, 1, i - 1), 0);
    end;
  end;
end;

function FindFileByName(const FullFileName: string): string;
{$IFNDEF MSWINDOWS}
var
  DirName,
  FileName: string;
  Rec: TSearchRec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := FullFileName;
{$ELSE}
  DirName := IncludeTrailingPathDelimiter(ExtractFilePath(FullFileName));
  FileName := ExtractFileName(FullFileName);
  Result := '';

  if FindFirst(DirName + '*.*', faAnyFile - faDirectory, Rec) = 0 then begin
    try
      repeat
        if SameText(string(Rec.Name), FileName) then begin
          Result := DirName + Rec.Name;
          Break;
        end;
      until FindNext(Rec) <> 0;
    finally
      FindClose(Rec);
    end;
  end;
{$ENDIF}
end;

{$ENDIF}

end.
