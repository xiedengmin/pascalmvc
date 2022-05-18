
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Mem Data
//  Created:            06.11.03
//////////////////////////////////////////////////

{$I Dac.inc}
unit MemUtils;

interface

uses
  Classes, SysUtils, Variants,
{$IFDEF NEXTGEN}
  System.SysConst,
{$ENDIF}
{$IFDEF HAVE_COMPRESS_INTERNAL}
  ZLib, {$IFNDEF FPC}ZLibConst,{$ELSE}zstream,{$ENDIF}
{$ENDIF}
  CLRClasses,
  CRTypes;

{$IFDEF AUTOREFCOUNT}
type
  GCHandle = record
  private
    FAddrOfPinnedObject: Pointer;
    FTarget: TObject;

    function GetAddrOfPinnedObject: Pointer;
    function GetTarget: TObject;
  public
    class function Alloc(Ptr: Pointer): Pointer; overload; static;
    class function Alloc(Obj: TObject): Pointer; overload; static;

    class procedure Release(Ptr: pointer); static;

    property AddrOfPinnedObject: Pointer read GetAddrOfPinnedObject;
    property Target: TObject read GetTarget;
  end;
  PGCHandle = ^GCHandle;
{$ENDIF}

  function CompareGuid(const g1, g2: TGuid): boolean;
  function TimeStampToDateTime(const ATimeStamp: TTimeStamp): TDateTime;

  function VarEqual(const Value1, Value2: variant): boolean;
  procedure OleVarClear(pValue: POleVariant);
  function GetOleVariant(pValue: POleVariant): OleVariant;
  procedure SetOleVariant(pValue: POleVariant; const Value: OleVariant);

  procedure CopyBuffer(Source, Dest: IntPtr; Count: Integer);
  procedure CopyBufferAnsi(const Source: AnsiString; Dest: IntPtr; Count{Bytes (#0 included)}: Integer);
  procedure CopyBufferUni(const Source: WideString; Dest: IntPtr; Count{Bytes (#0 included)}: Integer);

{$IFNDEF VER17P}
  function BytesOf(const Val: IntPtr; const Len: integer): TBytes;
{$ENDIF}
  procedure FillChar(X: IntPtr; Count: Integer; Value: byte); {$IFDEF USE_INLINE}inline;{$ENDIF}
  function DynArrayCreate(const SourceArray: array of Byte): TBytes; overload;
{$IFNDEF NEXTGEN}
  function DynArrayCreate(const SourceArray: array of AnsiChar): TBytes; overload;
{$ENDIF}
  procedure ArrayCopy(SourceArray: TBytes; SourceIndex: Integer; DestinationArray: TBytes; DestinationIndex: Integer; Length: Integer);
{$IFDEF AUTOREFCOUNT}
  function AllocGCHandle(Obj: TObject; Pinned: boolean = False): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF}
  function AllocGCHandle(Obj: Pointer; Pinned: boolean = False): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetGCHandleTarget(Handle: IntPtr): TObject; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetAddrOfPinnedObject(Handle: IntPtr): IntPtr; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure FreeGCHandle(Handle: IntPtr); {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocValueBuffer(Size: Integer): TValueBuffer; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure FreeValueBuffer(Buffer: TValueBuffer); {$IFDEF USE_INLINE}inline;{$ENDIF}

  procedure FreeString(P: IntPtr); {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: IntPtr): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: ShortInt): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: Byte): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: Word): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: Integer): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function AllocOrdinal(out Obj: Cardinal): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Double): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Byte): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: SmallInt): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Integer): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Int64): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Cardinal): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: Word): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  function OrdinalToPtr(out Obj: IntPtr): IntPtr; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: ShortInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Byte); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: SmallInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Word); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Integer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Int64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Double); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: Cardinal); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure PtrToOrdinal(P: IntPtr; out Obj: IntPtr); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure FreeOrdinal(P: IntPtr); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ PChar and PWideChar routines }
{$IFDEF NEXTGEN}
  function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; inline;
  function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
  function StrComp(const Str1, Str2: PAnsiChar): Integer;
  function StrLen(const Str: PAnsiChar): Cardinal; inline;
  function CompareStr(const S1, S2: PAnsiChar): Integer; overload;
  function Copy(const S: TBytes; Index, Count: Integer): TBytes; inline; overload;
  function Copy(const S: string; Index, Count: Integer): string; inline; overload;
  function Copy(const S: PAnsiChar; Index, Count: Integer): string; inline; overload;
  function Copy(const S: AnsiString; Index, Count: Integer): AnsiString; overload;
  function TrimRight(const S: AnsiString): AnsiString; overload;
  procedure Delete(var s: string; Index, Count: Integer); inline; overload;
  procedure Delete(var s: AnsiString; Index, Count: Integer); overload;
  function AnsiUpperCase(const S: AnsiString): AnsiString; overload;

  procedure BinToHex(Buffer: PAnsiChar; Text: PWideChar; BufSize: Integer);
  function HexToBin(Text: PWideChar; Buffer: PAnsiChar; BufSize: Integer): Integer;
  function VarToWideStr(const Value: Variant): string; inline;
  function WideUpperCase(const S: string): string; inline;
{$ENDIF}
  function AnsiStrCompS(S1, S2: PAnsiChar): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF} // SORT_STRINGSORT
  function AnsiStrICompS(S1, S2: PAnsiChar): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF} // SORT_STRINGSORT
  function AnsiCompareTextS(const S1, S2: AnsiString): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF} // SORT_STRINGSORT
  function AnsiCompareStrS(const S1, S2: AnsiString): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF} // SORT_STRINGSORT

  procedure BinToHexA(const Buffer: TBytes; Text: PAnsiChar; BufSize: Integer);
  procedure BinToHexW(const Buffer: TBytes; Text: PWideChar; BufSize: Integer);
  function HexToBinA(Text: PAnsiChar; Buffer: IntPtr; BufSize: Integer): Integer; overload;
  function HexToBinW(Text: PWideChar; Buffer: IntPtr; BufSize: Integer): Integer; overload;

  function StrCopyW(Dest: PWChar; const Source: PWChar): IntPtr;
  procedure StrLCopyW(Dest: PWChar; const Source: PWChar; MaxLen{WideChars}: Integer);
  function StrLenW(const Str: PWChar): Integer;
  function StrTrim(const Str: PAChar; Len: Integer): Integer;
  function StrTrimmed(const Str: PAChar; Len: Integer = -1): AnsiString;
  function StrTrimW(const Str: PWChar; Len: Integer): Integer;
  function StrTrimmedW(const Str: PWChar; Len: Integer = -1): WideString;
  function AnsiStrLCompWS(const S1, S2: WideString; MaxLen: Integer): Integer; // SORT_STRINGSORT
  function AnsiStrLICompWS(const S1, S2: WideString; MaxLen: Integer): Integer; // SORT_STRINGSORT
  function AnsiStrCompWS(const S1, S2: WideString): Integer; // SORT_STRINGSORT
  function AnsiStrICompWS(const S1, S2: WideString): Integer; // SORT_STRINGSORT

{$IFDEF MSWINDOWS}
{$IFNDEF VER7P}
  function VarArrayAsPSafeArray(const A: Variant): PVarArray;
{$ENDIF}
{$IFDEF FPC}
const
  VAR_INVALIDARG    = HRESULT($80070057); // = Windows.E_INVALIDARG
{$ENDIF}
{$ENDIF}

  function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
  function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
  function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word; out AValue: TDateTime): Boolean;
  function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word): TDateTime;

  function Reverse2(Value: Word): Word;
  function Reverse4(Value: Cardinal): Cardinal;
  function Reverse8(Value: Int64): Int64;

{$IFDEF MSWINDOWS}
{$IFNDEF VER14P}
  procedure NameThreadForDebugging(AThreadName: AnsiString; AThreadID: Cardinal = $FFFFFFFF);
{$ENDIF}
{$ENDIF}

{$IFDEF HAVE_COMPRESS_INTERFACE}
const
  MIN_COMPRESS_LENGTH = 50; // Don't compress small bl.

  procedure CheckZLib;
  procedure DoCompress(dest: IntPtr; destLen: IntPtr; const source: IntPtr; sourceLen: Integer);
  procedure DoUncompress(dest: IntPtr; destlen: IntPtr; source: IntPtr; sourceLne: Integer);

type
  TCompressProc = function(dest: IntPtr; destLen: IntPtr; const source: IntPtr; sourceLen: Integer): Integer; {$IFNDEF MSWINDOWS}cdecl;{$ENDIF}
  TUncompressProc = function(dest: IntPtr; destlen: IntPtr; source: IntPtr; sourceLne: Integer): Integer; {$IFNDEF MSWINDOWS}cdecl;{$ENDIF}

{$IFDEF VER12P}
{$IFDEF HAVE_COMPRESS_INTERNAL}
  ECompressionError = EZCompressionError;
  EDecompressionError = EZDecompressionError;
{$ENDIF}
{$ENDIF}

var
  CompressProc: TCompressProc;
  UncompressProc: TUncompressProc;

{$ENDIF}

{$IFDEF MSWINDOWS}
var
  IsWin9x: boolean;
{$ENDIF}

{ Unicode build support for Delphi 10-11 }

implementation

uses
{$IFDEF VER6}
   SysConst, VarUtils,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.String_, Posix.Wctype,
{$ENDIF}
  DAConsts, CRFunctions;

{$IFDEF AUTOREFCOUNT}

function GCHandle.GetAddrOfPinnedObject: Pointer;
begin
  if FTarget <> nil then
    Result := Pointer(FTarget)
  else
    Result := FAddrOfPinnedObject;
end;

function GCHandle.GetTarget: TObject;
begin
  Result := FTarget;
end;

class function GCHandle.Alloc(Ptr: Pointer): Pointer;
var
  Handle: PGCHandle;
begin
  New(Handle);
  Handle.FAddrOfPinnedObject := Ptr;
  Handle.FTarget := nil;
  Result := Handle;
end;

class function GCHandle.Alloc(Obj: TObject): Pointer;
var
  Handle: PGCHandle;
begin
  New(Handle);
  Handle.FAddrOfPinnedObject := nil;
  Handle.FTarget := Obj;
  Result := Handle;
end;

class procedure GCHandle.Release(Ptr: pointer);
var
  Handle: PGCHandle;
begin
  if Ptr <> nil then begin
    Handle := Ptr;
    Handle.FAddrOfPinnedObject := nil;
    Handle.FTarget := nil;
    Dispose(Handle);
  end;
end;

{$ENDIF}

function CompareGuid(const g1, g2: TGuid): boolean;
begin
  Result := CompareMem(@g1, @g2, SizeOf(TGuid));
end;

function TimeStampToDateTime(const ATimeStamp: TTimeStamp): TDateTime;
begin
  if (ATimeStamp.Time < 0) or (ATimeStamp.Date <= 0) then
    raise EConvertError.Create(Format('''%d.%d'' is not a valid timestamp', [ATimeStamp.Date, ATimeStamp.Time]));

  Result := ATimeStamp.Date - DateDelta;
  if Result < 0 then
    Result := Result - (ATimeStamp.Time / MSecsPerDay)
  else
    Result := Result + (ATimeStamp.Time / MSecsPerDay);
end;

// bug in D8 in compare strings as variant type
function VarEqual(const Value1, Value2: variant): boolean;
var
  va_old, va_new: PVarArray;
  va_data_old, va_data_new: IntPtr;
begin
  // prevent comparing as AnsiString
  if (VarType(Value1) = varOleStr) and ((VarType(Value2) = varOleStr) or (VarType(Value2) = varString)) or
    (VarType(Value2) = varOleStr) and ((VarType(Value1) = varOleStr) or (VarType(Value1) = varString))
  then
    Result := WideString(Value1) = WideString(Value2)
  else
  if (VarType(Value1) = varNull) and (VarType(Value2) = varNull) then
    Result := True
  else
  if (VarType(Value1) = varNull) or (VarType(Value2) = varNull) then
    Result := False
  else
  if VarIsStr(Value1) and (Value1 = '') then
    Result := VarIsStr(Value2) and (Value2 = '')
  else
  if VarIsStr(Value2) and (Value2 = '') then
    Result := False
  else
  if (VarType(Value1) = varArray + varByte) or
    (VarType(Value2) = varArray + varByte) then begin
      va_old := TVarData(Value1).VArray;
      va_new := TVarData(Value2).VArray;
      if (va_old = nil) and (va_new = nil) then
        Result := True
      else
        if (va_old = nil) or (va_new = nil) or
          (va_old.Bounds[0].ElementCount <> va_new.Bounds[0].ElementCount) then
          Result := False
        else begin
          va_data_old := va_old.Data;
          va_data_new := va_new.Data;
          if (va_data_old = nil) and (va_data_new = nil) then
            Result := True
          else
            if (va_data_old = nil) or (va_data_new = nil) then
              Result := False
            else
              Result := CompareMem(va_data_old, va_data_new, va_old.Bounds[0].ElementCount);
        end;
  end
  else
    Result := Value1 = Value2;
end;

procedure CopyBuffer(Source, Dest: IntPtr; Count: Integer);
begin
  Move(Source^, Dest^, Count);
end;

procedure CopyBufferAnsi(const Source: AnsiString; Dest: IntPtr; Count{Bytes (#0 included)}: Integer);
begin
  CopyBuffer(PAnsiChar(Source), Dest, Count);
end;

procedure CopyBufferUni(const Source: WideString; Dest: IntPtr; Count{Bytes (#0#0 included)}: Integer);
begin
  CopyBuffer(PWideChar(Source), Dest, Count);
end;

{$IFNDEF VER17P}
function BytesOf(const Val: IntPtr; const Len: integer): TBytes;
begin
  SetLength(Result, Len);
  if Len > 0 then
    Move(PByte(Val)^, Result[0], Len);
end;
{$ENDIF}

procedure FillChar(X: IntPtr; Count: Integer; Value: byte);
begin
  System.FillChar(X^, Count, Value);
end;

function DynArrayCreate(const SourceArray: array of byte): TBytes; overload;
begin
  SetLength(Result, High(SourceArray) + 1);
  Move(SourceArray, Pointer(Result)^, High(SourceArray) + 1);
end;

{$IFNDEF NEXTGEN}
function DynArrayCreate(const SourceArray: array of AnsiChar): TBytes; overload;
begin
  Result := Encoding.Default.GetBytes(SourceArray);
end;
{$ENDIF}

procedure ArrayCopy(SourceArray: TBytes; SourceIndex: Integer; DestinationArray: TBytes; DestinationIndex: Integer; Length: Integer);
begin
  System.Move(SourceArray[sourceIndex], DestinationArray[destinationIndex], Length);
end;

{$IFDEF AUTOREFCOUNT}
function AllocGCHandle(Obj: TObject; Pinned: boolean = False): IntPtr;
begin
  Result := GCHandle.Alloc(Obj);
end;
{$ENDIF}

function AllocGCHandle(Obj: Pointer; Pinned: boolean = False): IntPtr;
begin
{$IFDEF AUTOREFCOUNT}
  Result := GCHandle.Alloc(Obj);
{$ELSE}
  Result := Obj;
{$ENDIF}
end;

function GetGCHandleTarget(Handle: IntPtr): TObject;
begin
{$IFDEF AUTOREFCOUNT}
  if Handle = nil then
    Result := nil
  else
    Result := PGCHandle(Handle).Target;
{$ELSE}
  Result := Handle;
{$ENDIF}
end;

function GetAddrOfPinnedObject(Handle: IntPtr): IntPtr;
begin
{$IFDEF AUTOREFCOUNT}
  if Handle = nil then
    Result := nil
  else
    Result := PGCHandle(Handle).AddrOfPinnedObject;
{$ELSE}
  Result := Handle;
{$ENDIF}
end;

procedure FreeGCHandle(Handle: IntPtr);
begin
{$IFDEF AUTOREFCOUNT}
  GCHandle.Release(Handle);
{$ENDIF}
end;

function AllocValueBuffer(Size: Integer): TValueBuffer;
begin
{$IFNDEF VER17P}
  Result := Marshal.AllocHGlobal(sizeof(IntPtr));
{$ELSE}
  SetLength(Result, Size);
{$ENDIF}
end;

procedure FreeValueBuffer(Buffer: TValueBuffer);
begin
{$IFNDEF VER17P}
  Marshal.FreeHGlobal(Buffer);
{$ELSE}
  // do nothing
{$ENDIF}
end;

procedure FreeString(P: IntPtr);
begin
end;

function AllocOrdinal(out Obj: ShortInt): IntPtr; overload;
begin
  Result := @Obj;
end;

function AllocOrdinal(out Obj: Byte): IntPtr; overload;
begin
  Result := @Obj;
end;

function AllocOrdinal(out Obj: Word): IntPtr; overload;
begin
  Result := @Obj;
end;

function AllocOrdinal(out Obj: Integer): IntPtr; overload;
begin
  Result := @Obj;
end;

function AllocOrdinal(out Obj: Cardinal): IntPtr; overload;
begin
  Result := @Obj;
end;

function AllocOrdinal(out Obj: IntPtr): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Double): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Byte): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: SmallInt): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Integer): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Int64): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Cardinal): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: Word): IntPtr; overload;
begin
  Result := @Obj;
end;

function OrdinalToPtr(out Obj: IntPtr): IntPtr; overload;
begin
  Result := @Obj;
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: ShortInt); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Byte); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: SmallInt); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Word); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Integer); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Int64); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Double); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: Cardinal); overload;
begin
end;

procedure PtrToOrdinal(P: IntPtr; out Obj: IntPtr); overload;
begin
end;

procedure FreeOrdinal(P: IntPtr);
begin
end;

function StrCopyW(Dest: PWChar; const Source: PWChar): IntPtr;
{$IFDEF PUREPASCAL}
var
  Src: PWChar;
begin
  Result := Dest;
  Src := Source;
  repeat
    Dest^ := Src^;
    Inc(Dest);
    Inc(Src);
  until Src^ = #0;
{$ELSE}
asm
{$IFDEF CPU64}
        PUSH    RDI
        PUSH    RSI
        MOV     RSI,Dest
        MOV     RDI,Source
        MOV     RCX,0FFFFFFFFFFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     RCX
        SHL     RCX, 1  // Size := Len * sizeof(WideChar)
        MOV     RDI,RSI
        MOV     RSI,Source
        MOV     RDX,RCX
        MOV     RAX,RDI
        SHR     RCX,2
        REP     MOVSD
        MOV     RCX,RDX
        AND     RCX,3
        REP     MOVSB
        POP     RSI
        POP     RDI
{$ELSE}
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,Dest
        MOV     EDI,Source
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        SHL     ECX, 1  // Size := Len * sizeof(WideChar)
        MOV     EDI,ESI
        MOV     ESI,Source
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
{$ENDIF CPU64}
{$ENDIF PUREPASCAL}
end;

procedure StrLCopyW(Dest: PWChar; const Source: PWChar; MaxLen{WideChars}: Integer);
var
  pwc: PWideChar;
begin
  pwc := Source;
  while (pwc^ <> #0) and (pwc < PWideChar(Source) + MaxLen) do begin
    PWideChar(Dest)^ := pwc^;
    Inc(PWideChar(Dest));
    Inc(pwc);
  end;
  PWideChar(Dest)^ := #0;
end;

function StrLenW(const Str: PWChar): Integer; {$IFNDEF PUREPASCAL}assembler;{$ENDIF}
{$IFDEF PUREPASCAL}
begin
  if Str = nil then
    Result := 0
  else
    Result := Length(WideString(Str));
{$ELSE}
asm
{$IFDEF CPU64}
        MOV     RDX,RDI
        MOV     RDI,Str
        MOV     RCX,0FFFFFFFFFFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     RAX,0FFFFFFFFFFFFFFFEH
        SUB     RAX,RCX
        MOV     RDI,RDX
{$ELSE}
        MOV     EDX,EDI
        MOV     EDI,Str
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
{$ENDIF}
{$ENDIF}
end;

function StrTrim(const Str: PAChar; Len: Integer): Integer;
var
  pc: PAnsiChar;
begin
  pc := PAnsiChar(Str) + Len - 1;

  while pc >= Str do
    if pc^ = #0 then
      Dec(pc)
    else if pc^ = ' ' then
      Dec(pc)
    else
      Break;

  Inc(pc);
  pc^ := #0;
  Result := pc - Str;
end;

function StrTrimmed(const Str: PAChar; Len: Integer = -1): AnsiString;
var
  pc: PAnsiChar;
begin
  if len = -1 then // Detect length
    len := StrLen(PAnsiChar(Str));

  pc := PAnsiChar(Str) + len - 1;

  while pc >= Str do
    if pc^ = #0 then
      Dec(pc)
    else if pc^ = ' ' then
      Dec(pc)
    else begin
      len := NativeUInt(pc) - NativeUint(Str) + 1;
      SetLengthA(Result, len);
      Move(Str^, PAnsiChar(Result)^, len);
      Break;
    end;
end;

function StrTrimW(const Str: PWChar; Len: Integer): Integer;
var
  pwc: PWideChar;
begin
  pwc := PWideChar(Str) + Len - 1;

  while pwc >= Str do
    if pwc^ = #0 then
      Dec(pwc)
    else if pwc^ = ' ' then
      Dec(pwc)
    else
      Break;

  Inc(pwc);
  pwc^ := #0;
  Result := pwc - Str; // length in chars
end;

function StrTrimmedW(const Str: PWChar; Len: Integer = -1): WideString;
var
  pwc: PWideChar;
begin
  if Len = -1 then // Detect length
    Len := StrLenW(Str);

  pwc := PWideChar(Str) + Len - 1;

  while pwc >= Str do
    if pwc^ = #0 then
      Dec(pwc)
    else if pwc^ = ' ' then
      Dec(pwc)
    else begin
      len := NativeUInt(pwc) - NativeUint(Str) + 2;
      SetLength(Result, len shr 1);
      Move(Str^, Result[1], len);
      Break;
    end;
end;

function AnsiStrLCompWS(const S1, S2: WideString; MaxLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Assert(not IsWin9x, 'Unicode support on Win9x');
  Result := CompareStringW(LOCALE_USER_DEFAULT, SORT_STRINGSORT, PWideChar(S1), MaxLen,
    PWideChar(S2), MaxLen) - 2;
{$ELSE}
  Result := AnsiStrLComp(PChar(S1), PChar(S2), MaxLen{$IFDEF FPC} * 2{$ENDIF});
{$ENDIF}
end;

function AnsiStrLICompWS(const S1, S2: WideString; MaxLen: Integer): Integer;
begin
{$IFDEF MSWINDOWS}
  Assert(not IsWin9x, 'Unicode support on Win9x');
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT,
    PWideChar(S1), MaxLen, PWideChar(S2), MaxLen) - 2;
{$ELSE}
  Result := AnsiStrLIComp(PChar(S1), PChar(S2), MaxLen{$IFDEF FPC} * 2{$ENDIF});
{$ENDIF}
end;

function AnsiStrCompWS(const S1, S2: WideString): Integer;
begin
{$IFDEF MSWINDOWS}
  Assert(not IsWin9x, 'Unicode support on Win9x');
  Result := CompareStringW(LOCALE_USER_DEFAULT, SORT_STRINGSORT, PWideChar(S1), -1,
    PWideChar(S2), -1) - 2;
{$ELSE}
{$IFDEF NEXTGEN}
  Result := AnsiCompareStr(S1, S2);
{$ELSE}
  Result := WideCompareStr(S1, S2);
{$ENDIF}
{$ENDIF}
end;

function AnsiStrICompWS(const S1, S2: WideString): Integer;
begin
{$IFDEF MSWINDOWS}
  Assert(not IsWin9x, 'Unicode support on Win9x');
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PWideChar(S1),
    -1, PWideChar(S2), -1) - 2;
{$ELSE}
{$IFDEF NEXTGEN}
  Result := AnsiCompareText(S1, S2);
{$ELSE}
  Result := WideCompareText(S1, S2);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF NEXTGEN}

function StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
begin
  Move(Source^, Dest^, StrLen(Source) + 1);
  Result := Dest;
end;

function StrLCopy(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
var
  Len: Cardinal;
begin
  Result := Dest;
  Len := StrLen(Source);
  if Len > MaxLen then
    Len := MaxLen;
  Move(Source^, Dest^, Len);
  Dest[Len] := #0;
end;

function StrComp(const Str1, Str2: PAnsiChar): Integer;
var
  P1, P2: PAnsiChar;
begin
  P1 := Str1;
  P2 := Str2;
  while True do
  begin
    if (P1^ <> P2^) or (P1^ = #0) then
      Exit(Ord(P1^) - Ord(P2^));
    Inc(P1);
    Inc(P2);
  end;
end;

function StrLen(const Str: PAnsiChar): Cardinal;
begin
  Result := Length(Str);
end;

function CompareStr(const S1, S2: PAnsiChar): Integer;
var
  P1, P2: PAnsiChar;
  I: Integer;
  L1, L2: Integer;
begin
  { Length and PChar of S1 }
  L1 := Length(S1);
  P1 := PAnsiChar(S1);

  { Length and PChar of S2 }
  L2 := Length(S2);
  P2 := PAnsiChar(S2);

  { Continue the loop until the end of one string is reached. }
  I := 0;
  while (I < L1) and (I < L2) do
  begin
    if (P1^ <> P2^) then
      Exit(Ord(P1^) - Ord(P2^));

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;

  { If chars were not different return the difference in length }
  Result := L1 - L2;
end;

function Copy(const S: TBytes; Index, Count: Integer): TBytes;
begin
  Result := System.Copy(S, Index, Count);
end;

function Copy(const S: string; Index, Count: Integer): string;
begin
  Result := System.Copy(S, Index, Count);
end;

function Copy(const S: PAnsiChar; Index, Count: Integer): string;
begin
  Result := string(System.Copy(S, Index, Count));
end;

function Copy(const S: AnsiString; Index, Count: Integer): AnsiString;
begin
  if (LengthA(S) = 0) or (Count <= 0) or (Index <= 0) then begin
    SetLengthA(Result, 0);
    Exit;
  end;

  SetLengthA(Result, Count);
  if Count > 0 then
    Move(S[Index]^, Result[1]^, Count);
end;

function TrimRight(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  if LengthA(S) = 0 then begin
    SetLengthA(Result, 0);
    Exit;
  end;

  I := LengthA(S);
  while (I >= 1) and (S[I]^ <= ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

procedure Delete(var s: string; Index, Count: Integer);
begin
  System.Delete(s, Index, Count);
end;

procedure Delete(var s: AnsiString; Index, Count: Integer);
var
  Len, TailLen: Integer;
begin
  Len := LengthA(s);
  if (Count > 0) and (Index >= 1) and (Index <= Len) then
  begin
    TailLen := Len - Index + 1;
    if Count > TailLen then Count := TailLen;
    Move(S[Index + Count]^, S[Index]^, TailLen - Count);
    S[Len - Count + 1]^ := #0;
  end;
end;

procedure AnsiStrUpper(Str: PAnsiChar);
{$IFDEF MSWINDOWS}
begin
  CharUpperA(Str);
end;
{$ENDIF}
{$IFDEF POSIX}
var
  Temp: string;
  Squish: AnsiString;
  I: Integer;
begin
  Temp := string(Str);    // expand and copy multibyte to widechar
  for I := Low(string) to High(Temp) do
    Temp[I] := WideChar(towupper(UCS4Char(Temp[I])));
  Squish := AnsiString(Temp);  // reduce and copy widechar to multibyte
  if Cardinal(LengthA(Squish)) > StrLen(Str) then
    raise ERangeError.CreateRes(@SRangeError);

  Move(Squish[1]^, Str^, LengthA(Squish));
end;
{$ENDIF}

function AnsiUpperCase(const S: AnsiString): AnsiString;
var
  Len: Integer;
begin
  Len := LengthA(S);
  Result := Copy(S, 1, Len);
  if Len > 0 then
    AnsiStrUpper(Result.Ptr);
end;

{$IFDEF POSIX}
function AnsiStrComp(S1, S2: PAnsiChar): Integer; inline;
begin
  Result := AnsiCompareStr(string(S1), string(S2));
end;

function AnsiStrIComp(S1, S2: PAnsiChar): Integer; inline;
begin
  Result := AnsiCompareText(string(S1), string(S2));
end;
{$ENDIF POSIX}

procedure BinToHex(Buffer: PAnsiChar; Text: PWideChar; BufSize: Integer);
const
  Convert: array[0..15] of WideChar = '0123456789ABCDEF';
var
  I: Integer;
begin
  for I := 0 to BufSize - 1 do
  begin
    Text[0] := Convert[Byte(Buffer[I]) shr 4];
    Text[1] := Convert[Byte(Buffer[I]) and $F];
    Inc(Text, 2);
  end;
end;

function HexToBin(Text: PWideChar; Buffer: PAnsiChar; BufSize: Integer): Integer;
const
  Convert: array['0'..'f'] of SmallInt =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15);
var
  I: Integer;
begin
  I := BufSize;
  while I > 0 do
  begin
    if CharInSet(Text[0], [':'..'@']) or CharInSet(Text[0], ['G'..#96]) or
       CharInSet(Text[1], [':'..'@']) or CharInSet(Text[1], ['G'..#96]) then
       Break;
    if not CharInSet(Text[0], ['0'..'f']) or not CharInSet(Text[1], ['0'..'f']) then Break;
    PByte(Buffer)[0] := AnsiChar((Convert[Text[0]] shl 4) + Convert[Text[1]]);
    Inc(Buffer);
    Inc(Text, 2);
    Dec(I);
  end;
  Result := BufSize - I;
end;

function VarToWideStr(const Value: Variant): string;
begin
  Result := VarToStr(Value);
end;

function WideUpperCase(const S: string): string;
begin
  Result := UpperCase(S);
end;

{$ENDIF NEXTGEN}

function AnsiStrCompS(S1, S2: PAnsiChar): Integer; // SORT_STRINGSORT
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, SORT_STRINGSORT, S1, -1, S2, -1) - 2;
{$ELSE}
  Result := AnsiStrComp(S1, S2);
{$ENDIF}
end;

function AnsiStrICompS(S1, S2: PAnsiChar): Integer; // SORT_STRINGSORT
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, S1, -1,
    S2, -1) - 2;
{$ELSE}
  Result := AnsiStrIComp(S1, S2);
{$ENDIF}
end;

function AnsiCompareTextS(const S1, S2: AnsiString): Integer; // SORT_STRINGSORT
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT, PAnsiChar(S1),
    Length(S1), PAnsiChar(S2), Length(S2)) - 2;
{$ELSE}
  Result := AnsiCompareText(string(S1), string(S2));
{$ENDIF}
end;

function AnsiCompareStrS(const S1, S2: AnsiString): Integer; // SORT_STRINGSORT
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, SORT_STRINGSORT, PAnsiChar(S1), Length(S1),
    PAnsiChar(S2), Length(S2)) - 2;
{$ELSE}
  Result := AnsiCompareStr(string(S1), string(S2));
{$ENDIF}
end;

procedure BinToHexA(const Buffer: TBytes; Text: PAnsiChar; BufSize: Integer);
const
{$IFNDEF NEXTGEN}
  Convert: array[0..15] of AnsiChar = AnsiString('0123456789ABCDEF');
{$ELSE}
  Convert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);
{$ENDIF}
var
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while i < BufSize do begin
  {$IFDEF NEXTGEN}
    PByte(Text)[J] := Convert[Byte(Buffer[I]) shr 4];
    Inc(J);
    PByte(Text)[J] := Convert[Byte(Buffer[I]) and $F];
    Inc(J);
  {$ELSE}
    Text[J] := Convert[Byte(Buffer[I]) shr 4];
    Inc(J);
    Text[J] := Convert[Byte(Buffer[I]) and $F];
    Inc(J);
  {$ENDIF}
    Inc(I);
  end;
end;

procedure BinToHexW(const Buffer: TBytes; Text: PWideChar; BufSize: Integer);
const
  Convert: array[0..15] of WideChar = WideString('0123456789ABCDEF');
var
  I, J: Integer;
begin
  I := 0;
  J := 0;
  while i < BufSize do begin
    Text[J] := WideChar(Convert[Byte(Buffer[I]) shr 4]);
    Inc(J);
    Text[J] := WideChar(Convert[Byte(Buffer[I]) and $F]);
    Inc(J);
    Inc(I);
  end;
end;

function HexToBinA(Text: PAnsiChar; Buffer: IntPtr; BufSize: Integer): Integer;
const
  Convert: array['0'..'f'] of Byte =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15);
var
  I, J: Integer;
  C1, C2: Char;
begin
  I := 0;
  J := 0;
  while I < BufSize do
  begin
    C1 := Char(Text[J]);
    Inc(J);
    C2 := Char(Text[J]);
    Inc(J);
    
    if not CharInSet(C1, ['0'..'f']) or
       not CharInSet(C2, ['0'..'f'])
    then
      Break;
      
    Marshal.WriteByte(Buffer, I, (Convert[C1] shl 4) + Convert[C2]);
    Inc(I);
  end;
  Result := I;
end;

function HexToBinW(Text: PWideChar; Buffer: IntPtr; BufSize: Integer): Integer;
const
  Convert: array['0'..'f'] of Byte =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15);
var
  I, J: Integer;
  C1, C2: Char;
begin
  I := 0;
  J := 0;
  while I < BufSize do
  begin
    C1 := Char(Text[J]);
    Inc(J);
    C2 := Char(Text[J]);
    Inc(J);

    if not CharInSet(C1, ['0'..'f']) or
       not CharInSet(C2, ['0'..'f'])
    then
      Break;

    Marshal.WriteByte(Buffer, I, (Convert[C1] shl 4) + Convert[C2]);
    Inc(I);
  end;
  Result := I;
end;

procedure OleVarClear(pValue: POleVariant);
begin
  pValue^ := Unassigned;
end;

function GetOleVariant(pValue: POleVariant): OleVariant;
begin
  Result := pValue^;
end;

procedure SetOleVariant(pValue: POleVariant; const Value: OleVariant);
begin
  pValue^ := Value;
end;

function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Integer(Hour * 3600000) + Integer(Min * 60000) + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function TryEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word; out AValue: TDateTime): Boolean;
var
  LTime: TDateTime;
begin
  Result := TryEncodeDate(AYear, AMonth, ADay, AValue);
  if Result then
  begin
    Result := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
    if Result then
      if AValue >= 0 then
        AValue := AValue + LTime
      else
        AValue := AValue - LTime;
  end;
end;

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: Word): TDateTime;
begin
  if not TryEncodeDateTime(AYear, AMonth, ADay,
                           AHour, AMinute, ASecond, AMilliSecond, Result) then
    raise EConvertError.Create(SDateEncodeError);
end;

function Reverse2(Value: Word): Word;
begin
  Result := Word((Value shl 8) or (Value shr 8));
end;

function Reverse4(Value: Cardinal): Cardinal;
begin
   Result := (Value shl 24) or
             (Value shr 24) or
             ((Value shl 8) and $00FF0000) or
             ((Value shr 8) and $0000FF00);
end;

function Reverse8(Value: Int64): Int64;
begin
  Int64Rec(Result).Hi := (Int64Rec(Value).Lo shl 24) or
                         (Int64Rec(Value).Lo shr 24) or
                         ((Int64Rec(Value).Lo shl 8) and $00FF0000) or
                         ((Int64Rec(Value).Lo shr 8) and $0000FF00);
  Int64Rec(Result).Lo := (Int64Rec(Value).Hi shl 24) or
                         (Int64Rec(Value).Hi shr 24) or
                         ((Int64Rec(Value).Hi shl 8) and $00FF0000) or
                         ((Int64Rec(Value).Hi shr 8) and $0000FF00);
end;

{$IFDEF MSWINDOWS}
{$IFNDEF VER14P}
procedure NameThreadForDebugging(AThreadName: AnsiString; AThreadID: Cardinal);
type
  TThreadNameInfo = record
    FType: Cardinal;     // must be 0x1000
    FName: PAnsiChar;    // pointer to name (in user address space)
    FThreadID: Cardinal; // thread ID (-1 indicates caller thread)
    FFlags: Cardinal;    // reserved for future use, must be zero
  end;
var
  ThreadNameInfo: TThreadNameInfo;
begin
  //if IsDebuggerPresent then
  begin
    ThreadNameInfo.FType := $1000;
    ThreadNameInfo.FName := PAnsiChar(AThreadName);
    ThreadNameInfo.FThreadID := AThreadID;
    ThreadNameInfo.FFlags := 0;

    try
      RaiseException($406D1388, 0, sizeof(ThreadNameInfo) div sizeof(Cardinal), @ThreadNameInfo);
    except
    end;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF VER7P}
function GetVarDataArrayInfo(const AVarData: TVarData; out AVarType: TVarType;
  out AVarArray: PVarArray): Boolean;
begin
  // variant that points to another variant?  lets go spelunking
  if AVarData.VType = varByRef or varVariant then
    Result := GetVarDataArrayInfo(PVarData(AVarData.VPointer)^, AVarType, AVarArray)
  else
  begin
    // make sure we are pointing to an array then
    AVarType := AVarData.VType;
    Result := (AVarType and varArray) <> 0;

    // figure out the array data pointer
    if Result then
      if (AVarType and varByRef) <> 0 then
        AVarArray := PVarArray(AVarData.VPointer^)
      else
        AVarArray := AVarData.VArray
    else
      AVarArray := nil;
  end;
end;
{$ENDIF}

{$IFNDEF VER7P}
function VarArrayAsPSafeArray(const A: Variant): PVarArray;
var
  LVarType: TVarType;
begin
  if not GetVarDataArrayInfo(TVarData(A), LVarType, Result) then
    VarResultCheck(VAR_INVALIDARG);
end;
{$ENDIF}
{$ENDIF}

{$IFDEF HAVE_COMPRESS_INTERFACE}
procedure CheckZLib;
begin
  if not Assigned(CompressProc) then
    raise Exception.Create(SCompressorNotLinked);
  if not Assigned(UncompressProc) then
    raise Exception.Create(SUncompressorNotLinked);
end;

procedure DoCompress(dest: IntPtr; destLen: IntPtr; const source: IntPtr; sourceLen: Integer);
begin
  Assert(Assigned(CompressProc), SCompressorNotLinked);
  CompressProc(dest, destLen, source, sourceLen)
end;

procedure DoUncompress(dest: IntPtr; destlen: IntPtr; source: IntPtr; sourceLne: Integer);
begin
  Assert(Assigned(UncompressProc), SUncompressorNotLinked);
  UncompressProc(dest, destLen, source, sourceLne)
end;

{$IFDEF HAVE_COMPRESS_INTERNAL}
function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create(sError);
end;

function DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise EDecompressionError.Create(sError);
end;

function compress(dest: IntPtr; destLen: IntPtr; const source: IntPtr; sourceLen: Integer): Integer; {$IFNDEF MSWINDOWS}cdecl;{$ENDIF}
var
  strm: TZStreamRec;
begin
  FillChar(@strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  strm.next_in := source;
  strm.avail_in := sourceLen;
  strm.next_out := dest;
  strm.avail_out := Integer(destLen^);
  CCheck(deflateInit_(strm, Z_DEFAULT_COMPRESSION{Z_BEST_COMPRESSION}, zlib_version, sizeof(strm)));
  try
    Result := CCheck(deflate(strm, Z_FINISH));
    if Result <> Z_STREAM_END then
      raise EZlibError.CreateRes(@sTargetBufferTooSmall);
  finally
    CCheck(deflateEnd(strm));
  end;
  Integer(destLen^) := strm.total_out;
end;

function uncompress(dest: IntPtr; destlen: IntPtr; source: IntPtr; sourceLne: Integer): Integer; {$IFNDEF MSWINDOWS}cdecl;{$ENDIF}
var
  strm: TZStreamRec;
begin
  FillChar(@strm, sizeof(strm), 0);
  strm.zalloc := zlibAllocMem;
  strm.zfree := zlibFreeMem;
  strm.next_in := source;
  strm.avail_in := sourcelne;
  strm.next_out := dest;
  strm.avail_out := Integer(destlen^);
  DCheck(inflateInit_(strm, zlib_version, sizeof(strm)));
  try
    Result := DCheck(inflate(strm, Z_FINISH));
    if Result <> Z_STREAM_END then
      raise EZlibError.CreateRes(@sTargetBufferTooSmall);
  finally
    DCheck(inflateEnd(strm));
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  lpVersionInformation: TOSVersionInfo;
{$ENDIF}

initialization
{$IFDEF MSWINDOWS}
  lpVersionInformation.dwOSVersionInfoSize := sizeof(lpVersionInformation);
  {$WARN SYMBOL_PLATFORM OFF}
  Win32Check(GetVersionEx(lpVersionInformation));
  IsWin9x := lpVersionInformation.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS;
{$ENDIF}

{$IFDEF HAVE_COMPRESS_INTERNAL}
  CompressProc := compress;
  UncompressProc := uncompress;
{$ENDIF}
end.
