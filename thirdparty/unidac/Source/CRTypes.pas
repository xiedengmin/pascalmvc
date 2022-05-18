
//////////////////////////////////////////////////
//  Devart Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRTypes
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRTypes;

interface

uses
  Types, Variants,
{$IFDEF AUTOREFCOUNT}
  Generics.Collections,
{$ENDIF}
{$IFDEF VER14}
  SqlTimSt,
{$ENDIF}
  Classes, SysUtils, SyncObjs;

const
  EMPTY_GUID = '{00000000-0000-0000-0000-000000000000}';

{$IFDEF VER16P}
  pidDevartWinPlatforms = pidWin32 or pidWin64;
  pidDevartAllPlatforms = pidWin32 or pidWin64
                    or pidOSX32{$IFDEF VER26P} or pidOSX64{$ENDIF}
    {$IFDEF VER18P} or pidiOSSimulator{$IFNDEF VER22P} or pidiOSDevice{$ELSE} or pidiOSDevice32 or pidiOSDevice64{$ENDIF}{$ENDIF}
    {$IFDEF VER25P} or pidLinux64 {$ENDIF}
    {$IFDEF VER26P} or pidAndroid32Arm or pidAndroid64Arm{$ELSE}{$IFDEF VER19P} or pidAndroid{$ENDIF}{$ENDIF};
{$ENDIF}

{$IFNDEF VER16P}
  varObject = $0049;
{$ENDIF}
  varSharedObject   = varByRef; // {$IFDEF FPC}or varVariant{$ENDIF}
  varValueArrayRef  = varByRef + varArray + varVariant;
  varObjectArrayRef = varByRef + varArray + varObject; // for ODAC

{$IFNDEF VER6P}
type
  EVariantInvalidOpError = class(EVariantError);
  EVariantTypeCastError = class(EVariantError);
  EVariantBadVarTypeError = class(EVariantError);
  EVariantOverflowError = class(EVariantError);
  EVariantBadIndexError = class(EVariantError);
  EVariantArrayLockedError = class(EVariantError);
  EVariantNotImplError = class(EVariantError);
  EVariantOutOfMemoryError = class(EVariantError);
  EVariantInvalidArgError = class(EVariantError);
  EVariantUnexpectedError = class(EVariantError);

const
  varShortInt = $0010; { vt_i1          }
  varWord     = $0012; { vt_ui2         }
  varLongWord = $0013; { vt_ui4         }
  varInt64    = $0014; { vt_i8          }

  MaxBCDPrecision = 18;
  MaxBCDScale = 4;

// These equate to Window's constants but are renamed to less OS dependent
  VAR_OK            = HRESULT($00000000); // = Windows.S_OK
  VAR_PARAMNOTFOUND = HRESULT($80020004); // = Windows.DISP_E_PARAMNOTFOUND
  VAR_TYPEMISMATCH  = HRESULT($80020005); // = Windows.DISP_E_TYPEMISMATCH
  VAR_BADVARTYPE    = HRESULT($80020008); // = Windows.DISP_E_BADVARTYPE
  VAR_EXCEPTION     = HRESULT($80020009); // = Windows.DISP_E_EXCEPTION
  VAR_OVERFLOW      = HRESULT($8002000A); // = Windows.DISP_E_OVERFLOW
  VAR_BADINDEX      = HRESULT($8002000B); // = Windows.DISP_E_BADINDEX
  VAR_ARRAYISLOCKED = HRESULT($8002000D); // = Windows.DISP_E_ARRAYISLOCKED
  VAR_NOTIMPL       = HRESULT($80004001); // = Windows.E_NOTIMPL
  VAR_OUTOFMEMORY   = HRESULT($8007000E); // = Windows.E_OUTOFMEMORY
  VAR_INVALIDARG    = HRESULT($80070057); // = Windows.E_INVALIDARG
  VAR_UNEXPECTED    = HRESULT($8000FFFF); // = Windows.E_UNEXPECTED

  SInvalidVarCast = 'Invalid variant type conversion';
  SVarBadType = 'Invalid variant type';
  SInvalidVarOp = 'Invalid variant operation';
  SVarOverflow = 'Variant overflow';
  SVarArrayBounds = 'Variant or safe array index out of bounds';
  SVarArrayLocked = 'Variant or safe array is locked';
  SVarNotImplemented = 'Operation not supported';
  SOutOfMemory = 'Out of memory';
  SVarInvalid = 'Invalid argument';
  SVarUnexpected = 'Unexpected variant error';
  SInvalidVarOpWithHResultWithPrefix = 'Invalid variant operation (%s%.8x)'#10'%s';
{$ENDIF}

{$IFNDEF VER7P}
const
{$IFDEF FPC}
  SVarArrayBounds = 'Variant or safe array index out of bounds';
  SVarArrayLocked = 'Variant or safe array is locked';
{$ENDIF}
  SVarArrayWithHResult = 'Unexpected variant or safe array error: %s%.8x';

type
  ESafeArrayError = class(Exception)
  private
    FErrorCode: HRESULT;
  public
    constructor CreateHResult(AResult: HRESULT; const AMessage: string = '');
    property ErrorCode: HRESULT read FErrorCode write FErrorCode;
  end;

  ESafeArrayCreateError = class(ESafeArrayError);
  ESafeArrayBoundsError = class(ESafeArrayError);
  ESafeArrayLockedError = class(ESafeArrayError);

{$IFNDEF FPC}
type
  TFormatSettings = record
    CurrencyFormat: Byte;
    NegCurrFormat: Byte;
    ThousandSeparator: Char;
    DecimalSeparator: Char;
    CurrencyDecimals: Byte;
    DateSeparator: Char;
    TimeSeparator: Char;
    ListSeparator: Char;
    CurrencyString: string;
    ShortDateFormat: string;
    LongDateFormat: string;
    TimeAMString: string;
    TimePMString: string;
    ShortTimeFormat: string;
    LongTimeFormat: string;
    ShortMonthNames: array[1..12] of string;
    LongMonthNames: array[1..12] of string;
    ShortDayNames: array[1..7] of string;
    LongDayNames: array[1..7] of string;
    TwoDigitYearCenturyWindow: Word;
  end;
{$ENDIF}

const
  SecsPerMin  = 60;
  MinsPerHour = 60;
  MSecsPerSec = 1000;
{$ENDIF}


{$IFDEF NODBACCESS}
type
  ECRError = class (Exception)
  protected
    FErrorCode: integer;
  public
    constructor Create(ErrorCode: integer; const Msg: string);

    property ErrorCode: integer read FErrorCode;
  end;
{$ENDIF}

type
  Int    = Integer;
  Int16  = SmallInt;
  Int32  = Integer;
  UInt16 = Word;
  UInt32 = Cardinal;
  SByte  = ShortInt;

  IntPtr = Pointer;
  PIntPtr = ^IntPtr;
  MulticastDelegate = Pointer;

{$IFNDEF VER16P}
  {$IFNDEF FPC}
  NativeInt   = Integer;
  NativeUInt  = Cardinal;
  PNativeInt  = ^NativeInt;
  PNativeUInt = ^NativeUInt;
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
{$IFNDEF VER7P}
  UInt64 = Int64;
{$ENDIF}
{$IFNDEF VER12P}
  PUInt64 = ^UInt64;
{$ENDIF}
{$IFNDEF VER15P}
  PLongBool = ^LongBool;
  TThreadID = Cardinal;
{$ENDIF}
{$ELSE}
  PUInt64 = ^UInt64;
{$ENDIF}

{$IFDEF VER17P}
  TValueBuffer = TArray<Byte>;
{$ELSE}
  TValueBuffer = IntPtr;
{$ENDIF}

{$IFDEF NEXTGEN}
  AnsiString = record
  private type
    TDisposer = class
    private
      // Use inline storage to avoid more allocation and indirection for few items.
      FInline: array[0..3] of Pointer;
      FOverflow: TArray<Pointer>;
      FCount: Integer;
      procedure AddDispose(P: Pointer);
      procedure Flush;
    public
      destructor Destroy; override;
    end;

  private
    FPtr: MarshaledAString;
    FLength: integer;
    FDisposer: TDisposer;

    procedure AddDispose(P: Pointer);
    function GetChars(Index: Integer): MarshaledAString; inline;
    procedure SetChars(Index: Integer; Value: MarshaledAString); inline;
    procedure SetPtr(Value: MarshaledAString);
  public
    procedure SetLength(NewLength: integer);
    function Length: integer; inline;

    class operator Equal(const Left, Right: AnsiString): Boolean;
    class operator NotEqual(const Left, Right: AnsiString): Boolean; inline;

    class operator Implicit(const Val: AnsiString): MarshaledAString;
    class operator Explicit(const Ptr: MarshaledAString): AnsiString;

    class operator Implicit(const Val: AnsiString): string;
    class operator Implicit(const Str: string): AnsiString;

    class operator Implicit(const Val: AnsiString): Variant;
    class operator Explicit(const v: Variant): AnsiString;

    class operator Implicit(const Val: AnsiString): TBytes;
    class operator Explicit(const b: TBytes): AnsiString;

    property Chars[index: Integer]: MarshaledAString read GetChars write SetChars; default;
    property Ptr: MarshaledAString read FPtr write SetPtr;
  end;

  WideString = string;

  AnsiChar = byte;
  PAnsiChar = MarshaledAString;
{$ENDIF NEXTGEN}

  TValueArr = PAnsiChar;

{$IFNDEF VER11P}
  TBytes = array of Byte;
{$ENDIF}
  PAChar = PAnsiChar;
  PWChar = PWideChar;

{$IFNDEF VER12P}
  PCardinal = ^Cardinal;
{$ENDIF}

  TStringArray = TStringDynArray;
  TVariantArray = array of Variant;
  PVariantArray = ^TVariantArray;

  TByteArr = TBytes;
  TIntArr = array of Integer;
  TWordArr = array of Word;
  TLongWordArr = array of Cardinal;

  TCardinalConstArray = array[0..1023 + 20] of Cardinal;
  TCardinalArray      = ^TCardinalConstArray;

  TUInt64ConstArray = array[0..1023 + 20] of UInt64;
  TUInt64Array      = ^TUInt64ConstArray;

{$IFNDEF MSWINDOWS}
  HWND = NativeUInt;
{$IFDEF NEXTGEN}
  LPSTR = IntPtr;
  LPWSTR = IntPtr;
{$ELSE}
  {$IFDEF FPC}
  BOOL = LongBool;
  {$ENDIF}
  LPSTR = PAnsiChar;
  {$EXTERNALSYM LPSTR}
  LPWSTR = PWideChar;
  {$EXTERNALSYM LPWSTR}
{$ENDIF}
{$ENDIF}

{$IFDEF UTF8}
type
  UTF8String = type AnsiString;
{$ENDIF}

type
  IScAsyncResult = interface
  ['{0426C822-A1FE-4760-AB89-60B6CB2FE1F3}']
    function get_AsyncState: TObject;
    function get_AsyncWaitHandle: TEvent;
    function get_IsCompleted: boolean;

    property AsyncState: TObject read get_AsyncState;
    property AsyncWaitHandle: TEvent read get_AsyncWaitHandle;
    property IsCompleted: boolean read get_IsCompleted;
  end;
  AsyncCallback = procedure(Result: IScAsyncResult) of object;

type
  // increment RefCount for objects in the List for AUTOREFCOUNT
  // but don't destroy objects on Removing and Deleting from List
{$IFDEF AUTOREFCOUNT}
  TCRList = TList<TObject>;
  TCRThreadList = TThreadList<TObject>;
{$ELSE}
  TCRList = TList;
  TCRThreadList = TThreadList;
{$ENDIF}

  // increment RefCount for objects in the List for AUTOREFCOUNT
  // and destroy objects on Removing and Deleting from List
{$IFDEF AUTOREFCOUNT}
  TCRObjectList = TObjectList<TObject>;
{$ELSE}
  TCRObjectList = class(TList)
  protected
    procedure Notify(Instance: Pointer; Action: TListNotification); override;
  end;
{$ENDIF}

  TMachineType = (mtUnknown, mtAM33, mtAMD64, mtARM, mtEBC, mtI386, mtIA64,
                  mtM32R, mtMIPS16, mtMIPSFPU, mtMIPSFPU16, mtPOWERPC, mtPOWERPCFP,
                  mtR4000, mtSH3, mtSH3DSP, mtSH4, mtSH5, mtTHUMB, mtWCEMIPSV2);

  TIntValueItem = record
    Key: string;
    Value: Integer;
  end;

  PIntValueItem = ^TIntValueItem;

  TIntValueStringList = class
  private
    FCodeStrings: TList;
    FSorted: Boolean;

    function GetKey(Index: Integer): string;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index: Integer; const Value: Integer);
    function GetCount: Integer;
  protected
    procedure ListChanged; virtual;
    function Find(const Key: string; out Index: Integer): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TIntValueStringList);

    function Add(const S: string; Code: Integer): integer;
    procedure Insert(Index: Integer; const Key: string; Value: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;

    function IndexOf(const Key: string): Integer;
    function TryGetValue(const Key: string; out Value: Integer): Boolean;

    procedure Sort;

    property Keys[Index: Integer]: string read GetKey; default;
    property Values[Index: Integer]: Integer read GetValue write SetValue;
    property Count: Integer read GetCount;
  end;

  TStrValueItem = record
    Key: string;
    Value: string;
  end;

  PStrValueItem = ^TStrValueItem;

  TStrValueStringList = class
  private
    FStrValueStrings: TList;
    FSorted: Boolean;

  protected
    function GetKey(Index: Integer): string;
    function GetValue(Index: Integer): string;
    procedure SetValue(Index: Integer; const Value: string);
    function GetName(const Key: string): string; virtual;
    procedure SetName(const Key: string; const Value: string); virtual;
    function GetCount: Integer;
    function Find(const Key: string; out Index: Integer): Boolean;
    procedure CheckNewValue(const Key, Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TStrValueStringList);

    procedure AddStrings(Strings: TStrings); overload;
    procedure AddStrings(Strings: TStrValueStringList); overload;
    function Add(const Key, Value: string): Integer;
    procedure Insert(Index: Integer; const Key, Value: string);
    procedure Delete(Index: Integer);
    procedure Clear;

    procedure Remove(const Key: string);
    function IndexOf(const Key: string): Integer;
    function TryGetValue(const Key: string; out Value: string): Boolean;

    procedure Sort;

    property Keys[Index: Integer]: string read GetKey;
    property Values[Index: Integer]: string read GetValue write SetValue;

    property Names[const Key: string]: string read GetName write SetName; default;
    property Count: Integer read GetCount;
  end;

{$IFNDEF VER24P}
  TBufferedFileStream = class(TFileStream)
  private
    FFilePos, FBufStartPos, FBufEndPos: Int64;
    FBuffer: PByte;
    FBufferSize: Integer;
    FModified: Boolean;
    FBuffered: Boolean;
  protected
    procedure SetSize(const NewSize: Int64); override;
    procedure SyncBuffer(ReRead: Boolean);
  public
    constructor Create(const AFileName: string; Mode: Word; BufferSize: Integer = 32768); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal; BufferSize: Integer = 32768); overload;
    destructor Destroy; override;
    procedure FlushBuffer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  {$IFDEF VER17P}
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
  {$ENDIF}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;
{$ENDIF}

  TScOnProgressEvent = procedure(Sender: TObject; Total, Current: Int64; var Cancel: boolean) of object;

{$IFNDEF USE_FORMATSETTINGS}
var
  FormatSettings: TFormatSettings;
{$ENDIF}

implementation

{$IFDEF NEXTGEN}
uses
  Math;
{$ENDIF}

{$IFDEF NEXTGEN}
{ AnsiString.TDisposer }

destructor AnsiString.TDisposer.Destroy;
begin
  Flush;
  inherited;
end;

procedure AnsiString.TDisposer.AddDispose(P: Pointer);
var
  c: Integer;
begin
  if FCount < System.Length(FInline) then
    FInline[FCount] := P
  else begin
    c := FCount - System.Length(FInline);
    if c = System.Length(FOverflow) then begin
      if System.Length(FOverflow) < 4 then
        System.SetLength(FOverflow, 4)
      else
        System.SetLength(FOverflow, System.Length(FOverflow) * 2);
    end;
    FOverflow[c] := P;
  end;

  Inc(FCount);
end;

procedure AnsiString.TDisposer.Flush;
var
  i: Integer;
begin
  if FCount <= System.Length(FInline) then begin
    for i := 0 to FCount - 1 do
      System.FreeMem(FInline[i]);
  end
  else begin
    for i := 0 to System.Length(FInline) - 1 do
      System.FreeMem(FInline[i]);
    for i := 0 to FCount - System.Length(FInline) - 1 do
      System.FreeMem(FOverflow[i]);
  end;
  FCount := 0;
  System.SetLength(FOverflow, 0);
end;

{ AnsiString }

procedure AnsiString.AddDispose(P: Pointer);
begin
  if FDisposer = nil then
    FDisposer := TDisposer.Create;
  FDisposer.AddDispose(P);
end;

procedure AnsiString.SetLength(NewLength: integer);
var
  NewPtr: Pointer;
begin
  if NewLength <= 0 then begin
    FPtr := nil;
    FLength := 0;
    Exit;
  end;

  NewPtr := System.AllocMem(NewLength + 1);
  if (FDisposer <> nil) and (FPtr <> nil) then
    Move(FPtr^, NewPtr^, Min(FLength, NewLength));

  AddDispose(NewPtr);
  FPtr := NewPtr;
  FPtr[NewLength] := #0;
  FLength := NewLength;
end;

function AnsiString.Length: integer;
begin
  if (FDisposer <> nil) and (FPtr <> nil) then
    Result := FLength
  else
    Result := 0;
end;

class operator AnsiString.Equal(const Left, Right: AnsiString): Boolean;
var
  Len: integer;
  P1, P2: PAnsiChar;
begin
  Len := Left.Length;
  Result := Len = Right.Length;
  if Result then begin
    P1 := Left.FPtr;
    P2 := Right.Ptr;
    while Len > 0 do begin
      if P1^ <> P2^ then
        Exit(False);
      Inc(P1);
      Inc(P2);
      Dec(Len);
    end;
  end;
end;

class operator AnsiString.NotEqual(const Left, Right: AnsiString): Boolean;
begin
  Result := not (Left = Right);
end;

class operator AnsiString.Implicit(const Val: AnsiString): MarshaledAString;
begin
  if Val.FPtr = nil then
    Result := #0
  else
    Result := Val.FPtr;
end;

class operator AnsiString.Explicit(const Ptr: MarshaledAString): AnsiString;
begin
  if Result.FPtr <> Ptr then begin
    Result.FLength := System.Length(Ptr);

    if Result.FLength = 0 then
      Result.FPtr := nil
    else begin
      Result.FPtr := System.AllocMem(Result.FLength + 1);
      Result.AddDispose(Result.FPtr);

      Move(Ptr^, Result.FPtr^, Result.FLength);
      Result.FPtr[Result.FLength] := #0;
    end;
  end;
end;

class operator AnsiString.Implicit(const Val: AnsiString): string;
begin
  Result := string(Val.FPtr);
end;

class operator AnsiString.Implicit(const Str: string): AnsiString;
begin
  Result.FLength := LocaleCharsFromUnicode(DefaultSystemCodePage, 0, PWideChar(Str), System.Length(Str) + 1, nil, 0, nil, nil);
  if Result.FLength > 0 then begin
    Result.FPtr := System.AllocMem(Result.FLength);
    Result.AddDispose(Result.FPtr);
    LocaleCharsFromUnicode(DefaultSystemCodePage, 0, PWideChar(Str), System.Length(Str) + 1,
      Result.FPtr, Result.FLength, nil, nil);
    Dec(Result.FLength);
  end
  else
    Result.FPtr := nil;
end;

class operator AnsiString.Implicit(const Val: AnsiString): Variant;
begin
  Result := string(Val.FPtr);
end;

class operator AnsiString.Explicit(const v: Variant): AnsiString;
begin
  Result := AnsiString(string(v));
end;

class operator AnsiString.Implicit(const Val: AnsiString): TBytes;
var
  Len: integer;
begin
  Len := Val.Length;
  System.SetLength(Result, Len);
  if Len > 0 then
    Move(Val.FPtr^, Result[0], Len);
end;

class operator AnsiString.Explicit(const b: TBytes): AnsiString;
begin
  Result.FLength := System.Length(b);

  if Result.FLength = 0 then
    Result.FPtr := nil
  else begin
    Result.FPtr := System.AllocMem(Result.FLength + 1);
    Result.AddDispose(Result.FPtr);

    Move(b[0], Result.FPtr^, Result.FLength);
    Result.FPtr[Result.FLength] := #0;
  end;
end;

function AnsiString.GetChars(Index: Integer): MarshaledAString;
begin
  Result := @FPtr[Index - 1];
end;

procedure AnsiString.SetChars(Index: Integer; Value: MarshaledAString);
begin
  FPtr[Index - 1] := Value[0];
end;

procedure AnsiString.SetPtr(Value: MarshaledAString);
begin
  if FDisposer = nil then
    FDisposer := TDisposer.Create;

  FPtr := Value;
  FLength := System.Length(Value);
end;

{$ENDIF}

{$IFNDEF VER7P}
constructor ESafeArrayError.CreateHResult(AResult: HRESULT; const AMessage: string);
var
  S: string;
begin
  S := AMessage;
  if S = '' then
    S := Format(SVarArrayWithHResult, [AResult]);
  Create(S);
  FErrorCode := AResult;
end;
{$ENDIF}

{$IFDEF NODBACCESS}

{ ECRError }

constructor ECRError.Create(ErrorCode: integer; const Msg: string);
begin
  inherited Create(Msg);

  FErrorCode := ErrorCode;
end;

{$ENDIF}

{$IFNDEF AUTOREFCOUNT}

{ TCRObjectList }

procedure TCRObjectList.Notify(Instance: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    TObject(Instance).Free;
  inherited Notify(Instance, Action);
end;
{$ENDIF}

{ TIntValueStringList }

function CmpIntValueStrings(Item1, Item2: IntPtr): Integer;
begin
  Result := CompareText(PIntValueItem(Item1).Key, PIntValueItem(Item2).Key);
end;

constructor TIntValueStringList.Create;
begin
  inherited;
  FCodeStrings := TList.Create;
  FSorted := False;
end;

destructor TIntValueStringList.Destroy;
begin
  Clear;
  FCodeStrings.Free;
  inherited;
end;

function TIntValueStringList.GetKey(Index: Integer): string;
begin
  Result := PIntValueItem(FCodeStrings[Index]).Key;
end;

function TIntValueStringList.GetValue(Index: Integer): Integer;
begin
  Result := PIntValueItem(FCodeStrings[Index]).Value;
end;

procedure TIntValueStringList.SetValue(Index: Integer; const Value: Integer);
begin
  PIntValueItem(FCodeStrings[Index]).Value := Value;
end;

function TIntValueStringList.GetCount: Integer;
begin
  Result := FCodeStrings.Count;
end;

procedure TIntValueStringList.ListChanged;
begin
  // Empty
end;

function TIntValueStringList.Find(const Key: string; out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  Index := -1;

  if not FSorted then begin
    for I := 0 to FCodeStrings.Count - 1 do
      if CompareText(Keys[I], Key) = 0 then begin
        Result := True;
        Index := I;
        Exit;
      end;
  end
  else begin
    L := 0;
    H := FCodeStrings.Count - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      C := CompareText(Keys[i], Key);
      if C < 0 then
        L := I + 1
      else begin
        H := I - 1;
        if C = 0 then begin
          Result := True;
          L := I;
        end;
      end;
    end;
    Index := L;
  end;
end;

procedure TIntValueStringList.Assign(Source: TIntValueStringList);
var
  i: integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source.Keys[i], Source.Values[i]);
end;

function TIntValueStringList.Add(const S: string; Code: Integer): Integer;
begin
  if not FSorted then
    Result := FCodeStrings.Count
  else
    if Find(S, Result) then
      raise Exception.Create('String "' + S + '" is already in the list');
  Insert(Result, S, Code);
end;

procedure TIntValueStringList.Insert(Index: Integer; const Key: string; Value: Integer);
var
  CodeString: PIntValueItem;
begin
  New(CodeString);
  CodeString.Key := Key;
  CodeString.Value := Value;
  FCodeStrings.Insert(Index, CodeString);

  ListChanged;
end;

procedure TIntValueStringList.Delete(Index: Integer);
begin
  Dispose(PIntValueItem(FCodeStrings[Index]));
  FCodeStrings.Delete(Index);

  ListChanged;
end;

procedure TIntValueStringList.Clear;
var
  i: integer;
begin
  for i := 0 to FCodeStrings.Count - 1 do
    Dispose(PIntValueItem(FCodeStrings[i]));
  FCodeStrings.Clear;

  ListChanged;
end;

procedure TIntValueStringList.Sort;
begin
  FCodeStrings.Sort(CmpIntValueStrings);
  FSorted := True;

  ListChanged;
end;

function TIntValueStringList.IndexOf(const Key: string): Integer;
begin
  if not Find(Key, Result) then
    Result := -1;
end;

function TIntValueStringList.TryGetValue(const Key: string; out Value: Integer): Boolean;
var
  Index: integer;
begin
  Result := Find(Key, Index);
  if Result then
    Value := Values[Index]
  else
    Value := 0;
end;

{ TStrValueStringList }

constructor TStrValueStringList.Create;
begin
  inherited;
  FStrValueStrings := TList.Create;
  FSorted := False;
end;

destructor TStrValueStringList.Destroy;
begin
  Clear;
  FStrValueStrings.Free;
  inherited;
end;

function TStrValueStringList.GetKey(Index: Integer): string;
begin
  Result := PStrValueItem(FStrValueStrings[Index]).Key;
end;

function TStrValueStringList.GetValue(Index: Integer): string;
begin
  Result := PStrValueItem(FStrValueStrings[Index]).Value;
end;

procedure TStrValueStringList.SetValue(Index: Integer; const Value: string);
begin
  PStrValueItem(FStrValueStrings[Index]).Value := Value;
end;

function TStrValueStringList.GetName(const Key: string): string;
var
  Index: Integer;
begin
  if Find(Key, Index) then
    Result := PStrValueItem(FStrValueStrings[Index]).Value
  else
    Result := '';
end;

procedure TStrValueStringList.SetName(const Key: string; const Value: string);
var
  Index: Integer;
begin
  if Find(Key, Index) then
    PStrValueItem(FStrValueStrings[Index]).Value := Value
  else
    Insert(FStrValueStrings.Count, Key, Value);
end;

function TStrValueStringList.GetCount: Integer;
begin
  Result := FStrValueStrings.Count;
end;

function TStrValueStringList.Find(const Key: string; out Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  Index := -1;

  if not FSorted then begin
    for I := 0 to FStrValueStrings.Count - 1 do
      if CompareText(Keys[I], Key) = 0 then begin
        Result := True;
        Index := I;
        Exit;
      end;
  end
  else begin
    L := 0;
    H := FStrValueStrings.Count - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      C := CompareText(Keys[i], Key);
      if C < 0 then
        L := I + 1
      else begin
        H := I - 1;
        if C = 0 then begin
          Result := True;
          L := I;
        end;
      end;
    end;
    Index := L;
  end;
end;

procedure TStrValueStringList.Assign(Source: TStrValueStringList);
var
  i: integer;
begin
  Clear;
  for i := 0 to Source.Count - 1 do
    Add(Source.Keys[i], Source.Values[i]);
end;

procedure TStrValueStringList.CheckNewValue(const Key, Value: string);
begin

end;

procedure TStrValueStringList.AddStrings(Strings: TStrings);
var
  Value: string;
  PosEq: integer;
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do begin
    Value := Strings.Strings[i];
    PosEq := Pos('=', Value);
    if PosEq > 0 then
      Value := Copy(Value, PosEq + 1, MaxInt)
    else
      Value := '';

    Add(Strings.Names[i], Value);
  end;
end;

procedure TStrValueStringList.AddStrings(Strings: TStrValueStringList);
var
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    Add(Strings.Keys[i], Strings.Values[i]);
end;

function TStrValueStringList.Add(const Key, Value: string): Integer;
begin
  if not FSorted then
    Result := FStrValueStrings.Count
  else
    if Find(Key, Result) then
      raise Exception.Create('String "' + Key + '" is already in the list');
  Insert(Result, Key, Value);
end;

procedure TStrValueStringList.Insert(Index: Integer; const Key, Value: string);
var
  StrValueItem: PStrValueItem;
begin
  CheckNewValue(Key, Value);
  New(StrValueItem);
  StrValueItem.Key := Key;
  StrValueItem.Value := Value;
  FStrValueStrings.Insert(Index, StrValueItem);
end;

procedure TStrValueStringList.Delete(Index: Integer);
begin
  Dispose(PStrValueItem(FStrValueStrings[Index]));
  FStrValueStrings.Delete(Index);
end;

procedure TStrValueStringList.Remove(const Key: string);
var
  Index: integer;
begin
  if Find(Key, Index) then
    Delete(Index);
end;

procedure TStrValueStringList.Clear;
var
  i: integer;
begin
  for i := 0 to FStrValueStrings.Count - 1 do
    Dispose(PStrValueItem(FStrValueStrings[i]));
  FStrValueStrings.Clear;
end;

function CmpStrValueStrings(Item1, Item2: IntPtr): Integer;
begin
  Result := CompareText(PStrValueItem(Item1).Key, PStrValueItem(Item2).Key);
end;

procedure TStrValueStringList.Sort;
begin
  FStrValueStrings.Sort(CmpStrValueStrings);
  FSorted := True;
end;

function TStrValueStringList.IndexOf(const Key: string): integer;
begin
  if not Find(Key, Result) then
    Result := -1;
end;

function TStrValueStringList.TryGetValue(const Key: string; out Value: string): Boolean;
var
  Index: integer;
begin
  Result := Find(Key, Index);
  if Result then
    Value := Values[Index]
  else
    Value := '';
end;

{$IFNDEF VER24P}

{ TBufferedFileStream }

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  BufferSize: Integer);
begin
{$IFDEF POSIX}
  Create(AFilename, Mode, FileAccessRights, BufferSize);
{$ELSE}
  Create(AFilename, Mode, 0, BufferSize);
{$ENDIF}
end;

constructor TBufferedFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal; BufferSize: Integer);
begin
  inherited Create(AFileName, Mode, Rights);
  FBufferSize := BufferSize;
  GetMem(FBuffer, FBufferSize);
  FBuffered := True;
  SyncBuffer(True);
end;

destructor TBufferedFileStream.Destroy;
begin
  SyncBuffer(False);
  FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

procedure TBufferedFileStream.SyncBuffer(ReRead: boolean);
begin
  if FModified then
  begin
    inherited Seek(FBufStartPos, soBeginning);
    inherited Write(FBuffer^, NativeInt(FBufEndPos - FBufStartPos));
    FModified := False;
  end;
  if ReRead then
  begin
    FBufStartPos := inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos + inherited Read(FBuffer^, FBufferSize);
  end
  else
  begin
    inherited Seek(FFilePos, soBeginning);
    FBufEndPos := FBufStartPos;
  end;
end;

procedure TBufferedFileStream.FlushBuffer;
begin
  SyncBuffer(False);
end;

function TBufferedFileStream.Read(var Buffer; Count: Longint): Longint;
var
  PSrc: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Read(Buffer, Count)
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufEndPos) then
      SyncBuffer(True);
    if Count < FBufEndPos - FFilePos then
      Result := Count
    else
      Result := FBufEndPos - FFilePos;
  {$IFDEF FPC}
    PSrc := FBuffer + FFilePos - FBufStartPos;
  {$ELSE}
    PSrc := IntPtr(NativeInt(FBuffer) + FFilePos - FBufStartPos);
  {$ENDIF}
{$IFDEF CPUARM32}
    Move(PSrc^, Buffer, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(@Buffer)^ := PByte(PSrc)^;
      SizeOf(Word):
        PWord(@Buffer)^ := PWord(PSrc)^;
      SizeOf(Cardinal):
        PCardinal(@Buffer)^ := PCardinal(PSrc)^;
      SizeOf(UInt64):
        PUInt64(@Buffer)^ := PUInt64(PSrc)^;
    else
      Move(PSrc^, Buffer, Result);
    end;
{$ENDIF}
  end;
  FFilePos := FFilePos + Result;
end;

function TBufferedFileStream.Write(const Buffer; Count: Longint): Longint;
var
  PDest: PByte;
begin
  if Count >= FBufferSize then
  begin
    SyncBuffer(False);
    Result := inherited Write(Buffer, Count);
    FFilePos := FFilePos + Result;
  end
  else
  begin
    if (FBufStartPos > FFilePos) or (FFilePos + Count > FBufStartPos + FBufferSize) then
      SyncBuffer(True);
    Result := Count;
  {$IFDEF FPC}
    PDest := FBuffer + FFilePos - FBufStartPos;
  {$ELSE}
    PDest := IntPtr(NativeInt(FBuffer) + FFilePos - FBufStartPos);
  {$ENDIF}
{$IFDEF CPUARM32}
    Move(Buffer, PDest^, Result);
{$ELSE}
    case Result of
      SizeOf(Byte):
        PByte(PDest)^ := PByte(@Buffer)^;
      SizeOf(Word):
        PWord(PDest)^ := PWord(@Buffer)^;
      SizeOf(Cardinal):
        PCardinal(PDest)^ := PCardinal(@Buffer)^;
      SizeOf(UInt64):
        PUInt64(PDest)^ := PUInt64(@Buffer)^;
    else
      Move(Buffer, PDest^, Result);
    end;
{$ENDIF}
    FModified := True;
    FFilePos := FFilePos + Result;
    if FFilePos > FBufEndPos then
      FBufEndPos := FFilePos;
  end;
end;

{$IFDEF VER17P}
function TBufferedFileStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

function TBufferedFileStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Write(Buffer[Offset], Count);
end;
{$ENDIF}

function TBufferedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if not FBuffered then
    FFilePos := inherited Seek(Offset, Origin)
  else
    case Origin of
      soBeginning:
        begin
          if (Offset < FBufStartPos) or (Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := Offset;
        end;
      soCurrent:
        begin
          if (FFilePos + Offset < FBufStartPos) or (FFilePos + Offset > FBufEndPos) then
            SyncBuffer(False);
          FFilePos := FFilePos + Offset;
        end;
      soEnd:
        begin
          SyncBuffer(False);
          FFilePos := inherited Seek(Offset, soEnd);
        end;
    end;
  Result := FFilePos;
end;

procedure TBufferedFileStream.SetSize(const NewSize: Int64);
begin
  if NewSize < FBufEndPos then
    SyncBuffer(False);
  FBuffered := False;
  try
    inherited SetSize(NewSize);
  finally
    FBuffered := True;
  end;
end;

{$ENDIF}

{$IFNDEF USE_FORMATSETTINGS}
var
  i: integer;
initialization
  FormatSettings.LongDateFormat := LongDateFormat;
  FormatSettings.ShortDateFormat := ShortDateFormat;
  FormatSettings.LongTimeFormat := LongTimeFormat;
  FormatSettings.ShortTimeFormat := ShortTimeFormat;
  FormatSettings.DateSeparator := DateSeparator;
  FormatSettings.TimeSeparator := TimeSeparator;
  FormatSettings.DecimalSeparator := DecimalSeparator;
  FormatSettings.TimeAMString := TimeAMString;
  FormatSettings.TimePMString := TimePMString;

  for i := Low(ShortMonthNames) to High(ShortMonthNames) do
    FormatSettings.ShortMonthNames[i] := ShortMonthNames[i];
  for i := Low(LongMonthNames) to High(LongMonthNames) do
    FormatSettings.LongMonthNames[i] := LongMonthNames[i];
  for i := Low(ShortDayNames) to High(ShortDayNames) do
    FormatSettings.ShortDayNames[i] := ShortDayNames[i];
  for i := Low(LongDayNames) to High(LongDayNames) do
    FormatSettings.LongDayNames[i] := LongDayNames[i];
{$ENDIF}

end.
