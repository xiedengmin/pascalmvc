
//////////////////////////////////////////////////
//  Devart Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CLRClasses
//////////////////////////////////////////////////

{$I Dac.inc}
unit CLRClasses;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  CRTypes, CRFunctions;

type
  TAnsiCharArray = array of AnsiChar;

  BitConverter = class
  public
    class function GetBytes(value: Word): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Cardinal): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Int64): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Double): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function GetBytes(value: Single): TBytes; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function Int64BitsToDouble(value: Int64): double; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function DoubleToInt64Bits(value: double): Int64; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToDouble(const value: TBytes; startIndex: Integer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToDouble(const value: PAnsiChar; startIndex: Integer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToSingle(const value: TBytes; startIndex: Integer): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt16(const value: TBytes; startIndex: Integer): Int16; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt16(const value: TBytes; startIndex: Integer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt16(const value: PAnsiChar; startIndex: Integer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt32(const value: TBytes; startIndex: Integer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt32(const value: PAnsiChar; startIndex: Integer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToUInt32(const value: TBytes; startIndex: Integer): UInt32; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt64(const value: TBytes; startIndex: Integer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ToInt64(const value: PAnsiChar; startIndex: Integer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function IsLittleEndian: Boolean;
  end;

  Marshal = class
  public
    class function AllocHGlobal(cb: NativeInt): Pointer;
    class function ReallocHGlobal(pv: Pointer; cb: NativeInt): Pointer;
    class procedure FreeHGlobal(hglobal: Pointer);
    class procedure FreeCoTaskMem(ptr: Pointer);

    class function ReadInt8(ptr: Pointer): Shortint; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadInt8(ptr: Pointer; ofs: Integer): Shortint; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt8(ptr: Pointer; val: Shortint); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt8(ptr: Pointer; ofs: Integer; val: Shortint); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadByte(ptr: Pointer): Byte; overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadByte(ptr: Pointer; ofs: Integer): Byte; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteByte(ptr: Pointer; val: Byte); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteByte(ptr: Pointer; ofs: Integer; val: Byte); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt16(ptr: Pointer): Int16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadInt16(ptr: Pointer; ofs: Integer): Int16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt16(ptr: Pointer; val: Int16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt16(ptr: Pointer; ofs: Integer; val: Int16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadUInt16(ptr: Pointer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadUInt16(ptr: Pointer; ofs: Integer): UInt16; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt16(ptr: Pointer; val: UInt16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt16(ptr: Pointer; ofs: Integer; val: UInt16); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt32(ptr: Pointer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadInt32(ptr: Pointer; ofs: Integer): Int32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt32(ptr: Pointer; val: Int32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt32(ptr: Pointer; ofs: Integer; val: Int32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadUInt32(ptr: Pointer): UInt32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadUInt32(ptr: Pointer; ofs: Integer): UInt32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt32(ptr: Pointer; val: UInt32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt32(ptr: Pointer; ofs: Integer; val: UInt32); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadInt64(ptr: Pointer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadInt64(ptr: Pointer; ofs: Integer): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt64(ptr: Pointer; val: Int64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteInt64(ptr: Pointer; ofs: Integer; val: Int64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadUInt64(ptr: Pointer): UInt64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadUInt64(ptr: Pointer; ofs: Integer): UInt64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt64(ptr: Pointer; val: UInt64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteUInt64(ptr: Pointer; ofs: Integer; val: UInt64); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadNativeInt(ptr: Pointer): NativeInt; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadNativeInt(ptr: Pointer; ofs: Integer): NativeInt; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteNativeInt(ptr: Pointer; val: NativeInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteNativeInt(ptr: Pointer; ofs: Integer; val: NativeInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadNativeUInt(ptr: Pointer): NativeUInt; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadNativeUInt(ptr: Pointer; ofs: Integer): NativeUInt; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteNativeUInt(ptr: Pointer; val: NativeUInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteNativeUInt(ptr: Pointer; ofs: Integer; val: NativeUInt); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadDouble(ptr: Pointer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadDouble(ptr: Pointer; ofs: Integer): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteDouble(ptr: Pointer; val: Double); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteDouble(ptr: Pointer; ofs: Integer; val: Double); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function ReadIntPtr(ptr: Pointer): Pointer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function ReadIntPtr(ptr: Pointer; ofs: Integer): Pointer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteIntPtr(ptr: Pointer; val: Pointer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class procedure WriteIntPtr(ptr: Pointer; ofs: Integer; val: Pointer); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class function PtrToStringAnsi(ptr: Pointer): AnsiString; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function PtrToStringAnsi(ptr: Pointer; len: Integer): AnsiString; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function PtrToStringUni(ptr: Pointer): WideString; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function PtrToStringUni(ptr: Pointer; len: Integer): WideString; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToHGlobalAnsi(const s: AnsiString): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    class function StringToHGlobalUni(const s: WideString): Pointer; {$IFDEF USE_INLINE}inline;{$ENDIF}

    class procedure Copy(const source: TBytes; startIndex: Integer; destination: Pointer; length: Integer); overload;
    class procedure Copy(source: Pointer; const destination: TBytes; startIndex: Integer; length: Integer); overload;

    class function GetIUnknownForObject(o: TInterfacedObject): Pointer;
    class function AddRef(pUnk: Pointer): Integer;
    class function Release(pUnk: Pointer): Integer;
  end;

{$IFDEF NEXTGEN}
  Encoding = class(TEncoding)
  private
    class function GetDefault: Encoding; static; inline;
    class function GetASCII: Encoding; static; inline;
    class function GetANSI: Encoding; static; inline;
    class function GetUnicode: Encoding; static; inline;
    class function GetBigEndianUnicode: Encoding; static; inline;
    class function GetUTF8: Encoding; static; inline;
  public
    class property Default: Encoding read GetDefault;
    class property ASCII: Encoding read GetASCII;
    class property ANSI: Encoding read GetAnsi;
    class property Unicode: Encoding read GetUnicode;
    class property BigEndianUnicode: Encoding read GetBigEndianUnicode;
    class property UTF8: Encoding read GetUTF8;
  end;
{$ELSE}
  Encoding = class
  private
    FIsSingleByte: Boolean;
  public
    class function Default: Encoding;
    class function ASCII: Encoding;
    class function ANSI: Encoding;
    class function Unicode: Encoding;
    class function BigEndianUnicode: Encoding;
    class function UTF8: Encoding;
    class function GetEncoding(codepage: Cardinal): Encoding;
    class function Convert(srcEncoding, dstEncoding: Encoding; const bytes: TBytes): TBytes; overload;
    class function Convert(srcEncoding, dstEncoding: Encoding; const bytes: TBytes; index, count: Integer): TBytes; overload;

    function GetBytes(const chars: AnsiString): TBytes; overload; virtual; abstract;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; virtual; abstract;
    function GetBytes(const chars: WideString): TBytes; overload; virtual; abstract;
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; virtual; abstract;

    function GetString(const bytes: TBytes): string; overload;
    function GetString(const bytes: TBytes; index: Integer; count: Integer): string; overload;
    function GetAnsiString(const bytes: TBytes): AnsiString; overload; virtual; abstract;
    function GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; virtual; abstract;
    function GetWideString(const bytes: TBytes): WideString; overload; virtual; abstract;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; virtual; abstract;

    function GetMaxByteCount(charCount: Integer): Integer; virtual;
    function GetMaxCharCount(byteCount: Integer): Integer; virtual;

    property IsSingleByte: Boolean read FIsSingleByte;
  end;

  ANSIEncoding = class (Encoding)
  {$IFDEF IS_UTF8}
  private
    function GetAnsiEncoding: TEncoding;
  {$ENDIF}
  public
    constructor Create;

    function GetBytes(const chars: AnsiString): TBytes; overload; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function GetBytes(const chars: WideString): TBytes; overload; override;
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetAnsiString(const bytes: TBytes): AnsiString; overload; override;
    function GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;
  end;

  UnicodeEncoding = class(Encoding)
  public
    function GetBytes(const chars: AnsiString): TBytes; overload; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function GetBytes(const chars: WideString): TBytes; overload; override;
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetAnsiString(const bytes: TBytes): AnsiString; overload; override;
    function GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;

    function GetMaxByteCount(charCount: Integer): Integer; override;
    function GetMaxCharCount(byteCount: Integer): Integer; override;
  end;

  BigEndianUnicodeEncoding = class (UnicodeEncoding)
  public
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function GetWideString(const bytes: TBytes; index: integer; count: integer): WideString; overload; override;
  end;

  UTF8Encoding = class(Encoding)
  public
    function GetBytes(const chars: AnsiString): TBytes; overload; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function GetBytes(const chars: WideString): TBytes; overload; override;
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetAnsiString(const bytes: TBytes): AnsiString; overload; override;
    function GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;

    function GetMaxByteCount(charCount: Integer): Integer; override;
  end;

{$IFDEF MSWINDOWS}
  CodePageEncoding = class(Encoding)
  private
    FCodePage: Cardinal;
  public
    constructor Create(CodePage: Cardinal);

    function GetBytes(const chars: AnsiString): TBytes; override;
    function GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;
    function GetBytes(const chars: WideString): TBytes; overload; override;
    function GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer; overload; override;

    function GetAnsiString(const bytes: TBytes): AnsiString; overload; override;
    function GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString; overload; override;
    function GetWideString(const bytes: TBytes): WideString; overload; override;
    function GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString; overload; override;
  end;
{$ENDIF}
{$ENDIF}

  AnsiStringBuilder = class
  private
    function GetChar(Index: integer): AnsiChar;
  protected
    FString: TAnsiCharArray;
    FActualLength: Integer;

    procedure SetActualLength(Value: Integer);
  public
    constructor Create(capacity: Integer); overload;
    constructor Create(const value: AnsiString; capacity: Integer); overload;
    procedure Append(const value: AnsiString); overload;
    procedure Append(const value: AnsiString; const startIndex: Integer; const count: Integer); overload;
    procedure Append(const value: TBytes; const startIndex: Integer; const count: Integer); overload;
    procedure Append(value: AnsiChar); overload;
    procedure Append(value: AnsiChar; repeatCount: Integer); overload;
    procedure Append(value: AnsiStringBuilder); overload;
    procedure Insert(index: Integer; const value: AnsiString); overload;
    procedure Replace(const OldValue: AnsiString; const NewValue: AnsiString);
    function ToString: AnsiString; reintroduce;

    property Length: Integer read FActualLength write SetActualLength;
    property Chars[Index: integer]: AnsiChar read GetChar; default;
  end;

  TWideCharArray = array of WideChar;

{$IFDEF VER12P}
  WString = string;
{$ELSE}
  WString = WideString;
{$ENDIF}

  WideStringBuilder = class
  private
    function GetChar(Index: integer): WideChar;
  protected
    FString: TWideCharArray;
    FActualLength: Integer;

    procedure SetActualLength(Value: Integer);
  public
    constructor Create(capacity: Integer); overload;
    constructor Create(const value: WString; capacity: Integer); overload;
    procedure Append(const value: WString); overload;
    procedure Append(const value: WString; const startIndex: Integer; const count: Integer); overload;
    procedure Append(value: WideChar); overload;
    procedure Append(value: WideChar; repeatCount: Integer); overload;
    procedure Append(value: WideStringBuilder); overload;
    procedure Insert(index: Integer; const value: WString); overload;
    procedure Replace(const OldValue: WString; const NewValue: WString);
    function ToString: WString; {$IFDEF VER12P} override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property Length: Integer read FActualLength write SetActualLength;
    property Chars[Index: integer]: WideChar read GetChar; default;
  end;

{$IFDEF VER12P}
  StringBuilder = WideStringBuilder;
{$ELSE}
  StringBuilder = AnsiStringBuilder;
{$ENDIF}

  Buffer = class
  public
    class procedure BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: array of Cardinal; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: TCardinalArray; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: TCardinalArray; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: TBytes; srcOffset: Integer; const dst: array of Cardinal; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer); overload;
    class procedure BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer); overload;
    class function GetByte(const src: Pointer; Index: Integer): Byte;
    class procedure SetByte(const src: Pointer; Index: Integer; Value: Byte);
  end;

{ MemoryStream }

  MemoryStream = class
  private
    FData: TBytes;
    FPosition: Integer;
    FLength: Integer;

  protected
    procedure SetPosition(const Pos: Integer);

  public
    constructor Create(Capacity: Integer);
    function Seek(Offset: Integer; Origin: Word): Integer;
    function Read(var Buffer: TBytes; Offset: Integer; Count: Integer): Integer; overload;
    function Read(Buffer: PAnsiChar; Offset: Integer; Count: Integer): Integer; overload;
    procedure Write(const Buffer: TBytes; Offset: Integer; Count: Integer); overload;
    procedure Write(Buffer: PAnsiChar; Offset: Integer; Count: Integer); overload;
    procedure WriteByte(Value: Byte);
    function ReadByte: Byte;
    function GetBuffer: TValueArr;
    function ToArray: TBytes;

    procedure Close;
    procedure SetLength(Value: Integer);

    property Length: Integer read FLength write SetLength;
    property Position: Integer read FPosition write SetPosition;
  end;

  TMethodArray = array of TMethod;

  TScCancellationTokenState = (ctsInited, ctsNotifying, ctsCanceled);

  TScCancellationToken = class
  private
    FState: TScCancellationTokenState;
    FWaiter: TEvent;
    FLock: TCriticalSection;
    FOnCancelEventList: TMethodArray;

    procedure NotifyOnCancelEvents;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ReInit;
    procedure Cancel;
    function IsCancellationRequested: boolean;
    procedure ThrowIfCancellationRequested;
    procedure Delay(Timeout: cardinal);

    function CanBeCanceled: boolean;

    procedure Register(Event: TThreadMethod);
    procedure Unregister(Event: TThreadMethod);
  end;

  ArgumentException = class(Exception)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
  end;

  NotSupportedException = class(Exception)
  public
    constructor Create; overload;
    constructor Create(const Msg: string); overload;
  end;

  ExceptionArray = array of Exception;

  AggregateException = class(Exception)
  private
    FExceptions: ExceptionArray;
  public
    constructor Create(const AExceptions: ExceptionArray); overload;
    constructor Create(const Msg: string); overload;
    destructor Destroy; override;

    property Exceptions: ExceptionArray read FExceptions;
  end;

  InvalidDataException = class(Exception);
  InvalidOperationException = class(Exception);
  JSONException = class(Exception);
  OperationCanceledException = class(Exception);

implementation

{ BitConverter }

class function BitConverter.GetBytes(value: Word): TBytes;
begin
  SetLength(Result, SizeOf(Word));
  Word(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Cardinal): TBytes;
begin
  SetLength(Result, SizeOf(Cardinal));
  Cardinal(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Int64): TBytes;
begin
  SetLength(Result, SizeOf(Int64));
  Int64(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Double): TBytes;
begin
  SetLength(Result, SizeOf(Double));
  Double(Pointer(Result)^) := value;
end;

class function BitConverter.GetBytes(value: Single): TBytes;
begin
  SetLength(Result, SizeOf(Single));
  Single(Pointer(Result)^) := value;
end;

class function BitConverter.Int64BitsToDouble(value: Int64): double;
begin
  Result := Double(Pointer(@value)^);
end;

class function BitConverter.DoubleToInt64Bits(value: double): Int64;
begin
  Result := PInt64(@value)^;
end;

class function BitConverter.ToDouble(const value: TBytes; startIndex: Integer): Double;
begin
  Result := Double(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToDouble(const value: PAnsiChar; startIndex: Integer): Double;
begin
  Result := Double(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToSingle(const value: TBytes; startIndex: Integer): Single;
begin
  Result := Single(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToInt16(const value: TBytes; startIndex: Integer): Int16;
begin
  Result := Int16(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToUInt16(const value: TBytes; startIndex: Integer): UInt16;
begin
  Result := UInt16(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToUInt16(const value: PAnsiChar; startIndex: Integer): UInt16;
begin
  Result := UInt16(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToInt32(const value: TBytes; startIndex: Integer): Int32;
begin
  Result := Int32(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToInt32(const value: PAnsiChar; startIndex: Integer): Int32;
begin
  Result := Int32(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToUInt32(const value: TBytes; startIndex: Integer): UInt32;
begin
  Result := UInt32(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToInt64(const value: TBytes; startIndex: Integer): Int64;
begin
  Result := Int64(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.ToInt64(const value: PAnsiChar; startIndex: Integer): Int64;
begin
  Result := Int64(PtrOffset(Pointer(value), startIndex)^);
end;

class function BitConverter.IsLittleEndian: Boolean;
begin
  Result := True;
end;

{ Marshal }

class function Marshal.AllocHGlobal(cb: NativeInt): Pointer;
begin
  GetMem(Result, cb);
end;

class function Marshal.ReallocHGlobal(pv: Pointer; cb: NativeInt): Pointer;
begin
  Result := pv;
  ReallocMem(Result, cb);
end;

class procedure Marshal.FreeHGlobal(hglobal: Pointer);
begin
  FreeMem(hglobal);
end;

class procedure Marshal.FreeCoTaskMem(ptr: Pointer);
begin
end;

class function Marshal.ReadInt8(ptr: Pointer): Shortint;
begin
  Result := Shortint(ptr^);
end;

class function Marshal.ReadInt8(ptr: Pointer; ofs: Integer): Shortint;
begin
  Result := Shortint(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteInt8(ptr: Pointer; val: Shortint);
begin
  Shortint(ptr^) := val;
end;

class procedure Marshal.WriteInt8(ptr: Pointer; ofs: Integer; val: Shortint);
begin
  Shortint(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadByte(ptr: Pointer): Byte;
begin
  Result := Byte(ptr^);
end;

class function Marshal.ReadByte(ptr: Pointer; ofs: Integer): Byte;
begin
  Result := Byte(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteByte(ptr: Pointer; val: Byte);
begin
  Byte(ptr^) := val;
end;

class procedure Marshal.WriteByte(ptr: Pointer; ofs: Integer; val: Byte);
begin
  Byte(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadInt16(ptr: Pointer): Int16;
begin
  Result := Int16(ptr^);
end;

class function Marshal.ReadInt16(ptr: Pointer; ofs: Integer): Int16;
begin
  Result := Int16(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteInt16(ptr: Pointer; val: Int16);
begin
  Int16(ptr^) := val;
end;

class procedure Marshal.WriteInt16(ptr: Pointer; ofs: Integer; val: Int16);
begin
  Int16(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadUInt16(ptr: Pointer): UInt16;
begin
  Result := UInt16(ptr^);
end;

class function Marshal.ReadUInt16(ptr: Pointer; ofs: Integer): UInt16;
begin
  Result := UInt16(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteUInt16(ptr: Pointer; val: UInt16);
begin
  UInt16(ptr^) := val;
end;

class procedure Marshal.WriteUInt16(ptr: Pointer; ofs: Integer; val: UInt16);
begin
  UInt16(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadInt32(ptr: Pointer): Int32;
begin
  Result := Int32(ptr^);
end;

class function Marshal.ReadInt32(ptr: Pointer; ofs: Integer): Int32;
begin
  Result := Int32(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteInt32(ptr: Pointer; val: Int32);
begin
  Int32(ptr^) := val;
end;

class procedure Marshal.WriteInt32(ptr: Pointer; ofs: Integer; val: Int32);
begin
  Int32(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadUInt32(ptr: Pointer): UInt32;
begin
  Result := UInt32(ptr^);
end;

class function Marshal.ReadUInt32(ptr: Pointer; ofs: Integer): UInt32;
begin
  Result := UInt32(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteUInt32(ptr: Pointer; val: UInt32);
begin
  UInt32(ptr^) := val;
end;

class procedure Marshal.WriteUInt32(ptr: Pointer; ofs: Integer; val: UInt32);
begin
  UInt32(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadInt64(ptr: Pointer): Int64;
begin
  Result := Int64(ptr^);
end;

class function Marshal.ReadInt64(ptr: Pointer; ofs: Integer): Int64;
begin
  Result := Int64(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteInt64(ptr: Pointer; val: Int64);
begin
  Int64(ptr^) := val;
end;

class procedure Marshal.WriteInt64(ptr: Pointer; ofs: Integer; val: Int64);
begin
  Int64(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadUInt64(ptr: Pointer): UInt64;
begin
  Result := UInt64(ptr^);
end;

class function Marshal.ReadUInt64(ptr: Pointer; ofs: Integer): UInt64;
begin
  Result := UInt64(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteUInt64(ptr: Pointer; val: UInt64);
begin
  UInt64(ptr^) := val;
end;

class procedure Marshal.WriteUInt64(ptr: Pointer; ofs: Integer; val: UInt64);
begin
  UInt64(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadNativeInt(ptr: Pointer): NativeInt;
begin
  Result := NativeInt(ptr^);
end;

class function Marshal.ReadNativeInt(ptr: Pointer; ofs: Integer): NativeInt;
begin
  Result := NativeInt(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteNativeInt(ptr: Pointer; val: NativeInt);
begin
  NativeInt(ptr^) := val;
end;

class procedure Marshal.WriteNativeInt(ptr: Pointer; ofs: Integer; val: NativeInt);
begin
  NativeInt(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadNativeUInt(ptr: Pointer): NativeUInt;
begin
  Result := NativeUInt(ptr^);
end;

class function Marshal.ReadNativeUInt(ptr: Pointer; ofs: Integer): NativeUInt;
begin
  Result := NativeUInt(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteNativeUInt(ptr: Pointer; val: NativeUInt);
begin
  NativeUInt(ptr^) := val;
end;

class procedure Marshal.WriteNativeUInt(ptr: Pointer; ofs: Integer; val: NativeUInt);
begin
  NativeUInt(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadDouble(ptr: Pointer): Double;
begin
  Result := Double(ptr^);
end;

class function Marshal.ReadDouble(ptr: Pointer; ofs: Integer): Double;
begin
  Result := Double(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteDouble(ptr: Pointer; val: Double);
begin
  Double(ptr^) := val;
end;

class procedure Marshal.WriteDouble(ptr: Pointer; ofs: Integer; val: Double);
begin
  Double(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.ReadIntPtr(ptr: Pointer): Pointer;
begin
  Result := Pointer(ptr^);
end;

class function Marshal.ReadIntPtr(ptr: Pointer; ofs: Integer): Pointer;
begin
  Result := Pointer(PtrOffset(ptr, ofs)^);
end;

class procedure Marshal.WriteIntPtr(ptr, val: Pointer);
begin
  Pointer(ptr^) := val;
end;

class procedure Marshal.WriteIntPtr(ptr: Pointer; ofs: Integer; val: Pointer);
begin
  Pointer(PtrOffset(ptr, ofs)^) := val;
end;

class function Marshal.PtrToStringAnsi(ptr: Pointer): AnsiString;
begin
{$IFDEF NEXTGEN}
  Result.Ptr := PAnsiChar(ptr);
{$ELSE}
  Result := PAnsiChar(ptr);
{$ENDIF}
end;

class function Marshal.PtrToStringAnsi(ptr: Pointer; len: Integer): AnsiString;
begin
  SetLengthA(Result, len);
  Move(ptr^, PAnsiChar(Result)^, len);
end;

class function Marshal.PtrToStringUni(ptr: Pointer): WideString;
begin
  Result := PWideChar(ptr);
end;

class function Marshal.PtrToStringUni(ptr: Pointer; len: Integer): WideString;
begin
  SetLength(Result, len);
  Move(ptr^, PWideChar(Result)^, len * SizeOf(WideChar));
end;

class function Marshal.StringToHGlobalAnsi(const s: AnsiString): Pointer;
begin
  Result := PAnsiChar(s);
end;

class function Marshal.StringToHGlobalUni(const s: WideString): Pointer;
begin
  Result := PWideChar(s);
end;

class procedure Marshal.Copy(const source: TBytes; startIndex: Integer;
  destination: Pointer; length: Integer);
begin
  if length > 0 then
    Move(source[StartIndex], destination^, length);
end;

class procedure Marshal.Copy(source: Pointer; const destination: TBytes;
  startIndex, length: Integer);
begin
  if length > 0 then
    Move(source^, destination[startIndex], length);
end;

class function Marshal.GetIUnknownForObject(o: TInterfacedObject): Pointer;
var
  iu: IUnknown;
begin
  iu := IUnknown(o);
  iu._AddRef;
  Result := Pointer(iu);
end;

class function Marshal.AddRef(pUnk: Pointer): Integer;
begin
  Result := IUnknown(pUnk)._AddRef;
end;

class function Marshal.Release(pUnk: Pointer): Integer;
begin
  Result := IUnknown(pUnk)._Release;
end;

{ Encoding }

{$IFDEF NEXTGEN}
class function Encoding.GetDefault: Encoding;
begin
{$IFDEF LINUX}
  Result := Encoding(TEncoding.UTF8);
{$ELSE}
  Result := Encoding(TEncoding.ANSI);
{$ENDIF}
end;

class function Encoding.GetASCII: Encoding;
begin
  Result := Encoding(TEncoding.ASCII);
end;

class function Encoding.GetANSI: Encoding;
begin
  Result := Encoding(TEncoding.ANSI);
end;

class function Encoding.GetUnicode: Encoding;
begin
  Result := Encoding(TEncoding.Unicode);
end;

class function Encoding.GetBigEndianUnicode: Encoding;
begin
  Result := Encoding(TEncoding.BigEndianUnicode);
end;

class function Encoding.GetUTF8: Encoding;
begin
  Result := Encoding(TEncoding.UTF8);
end;

{$ENDIF}

{$IFNDEF NEXTGEN}
var
  theANSIEncoding, theUnicodeEncoding, theBigEndianUnicode, theUTF8Encoding: Encoding;
{$IFDEF MSWINDOWS}
  codepageEncodings: array of CodePageEncoding;
{$ENDIF}

class function Encoding.Default: Encoding;
begin
{$IFDEF FPC}
  Result := theUTF8Encoding;
{$ELSE}
  Result := theANSIEncoding;
{$ENDIF}
end;

class function Encoding.ASCII: Encoding;
begin
  Result := theANSIEncoding;
end;

class function Encoding.ANSI: Encoding;
begin
  Result := theANSIEncoding;
end;

class function Encoding.Unicode: Encoding;
begin
  Result := theUnicodeEncoding;
end;

class function Encoding.BigEndianUnicode: Encoding;
begin
  Result := theBigEndianUnicode;
end;

class function Encoding.UTF8: Encoding;
begin
  Result := theUTF8Encoding;
end;

class function Encoding.GetEncoding(codepage: Cardinal): Encoding;
{$IFDEF MSWINDOWS}
var
  i: Integer;
{$ENDIF}
begin
  case codepage of
    65001: // CP_UTF8
      Result := theUTF8Encoding;
    1200: // UTF-16 LE
      Result := theUnicodeEncoding;
    1201: // UTF-16 BE
      Result := theBigEndianUnicode;
  else
  {$IFDEF MSWINDOWS}
    if codepage = GetACP then
  {$ENDIF}
      Result := theANSIEncoding
  {$IFDEF MSWINDOWS}
    else begin
      for i := 0 to Length(codepageEncodings) - 1 do
        if codepageEncodings[i].FCodePage = codepage then begin
          Result := codepageEncodings[i];
          exit;
        end;

      Result := CodePageEncoding.Create(codepage);
      SetLength(codepageEncodings, Length(codepageEncodings) + 1);
      codepageEncodings[Length(codepageEncodings) - 1] := CodePageEncoding(Result);
    end;
  {$ENDIF}
  end;
end;

class function Encoding.Convert(srcEncoding, dstEncoding: Encoding; const bytes: TBytes): TBytes;
begin
  Result := Convert(srcEncoding, dstEncoding, bytes, 0, Length(bytes));
end;

class function Encoding.Convert(srcEncoding, dstEncoding: Encoding; const bytes: TBytes; index, count: Integer): TBytes;
var
  ws: WideString;
begin
  if srcEncoding = dstEncoding then
    Result := Copy(bytes, index, count)
  else begin
    ws := srcEncoding.GetWideString(bytes, index, count);
    Result := dstEncoding.GetBytes(ws);
  end;
end;

function Encoding.GetString(const bytes: TBytes): string;
begin
{$IFDEF VER12P}
  Result := GetWideString(bytes);
{$ELSE}
  Result := GetAnsiString(bytes);
{$ENDIF}
end;

function Encoding.GetString(const bytes: TBytes; index: Integer; count: Integer): string;
begin
{$IFDEF VER12P}
  Result := GetWideString(bytes, index, count);
{$ELSE}
  Result := GetAnsiString(bytes, index, count);
{$ENDIF}
end;

function Encoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount;
end;

function Encoding.GetMaxCharCount(byteCount: Integer): Integer;
begin
  Result := byteCount;
end;

procedure InitEncodings;
begin
  theANSIEncoding := ANSIEncoding.Create;
  theUnicodeEncoding := UnicodeEncoding.Create;
  theBigEndianUnicode := BigEndianUnicodeEncoding.Create;
  theUTF8Encoding := UTF8Encoding.Create;
{$IFDEF MSWINDOWS}
  codepageEncodings := nil;
{$ENDIF}
end;

procedure FreeEncodings;
{$IFDEF MSWINDOWS}
var
  i: Integer;
{$ENDIF}
begin
  theANSIEncoding.Free;
  theUnicodeEncoding.Free;
  theBigEndianUnicode.Free;
  theUTF8Encoding.Free;

{$IFDEF MSWINDOWS}
  for i := 0 to Length(codepageEncodings) - 1 do
    codepageEncodings[i].Free;

  codepageEncodings := nil;
{$ENDIF}
end;


{ ANSIEncoding }

constructor ANSIEncoding.Create;
{$IFDEF MSWINDOWS}
var
  CodePage: Integer;
  LCPInfo: TCPInfo;
{$ENDIF}
begin
  inherited;

{$IFDEF MSWINDOWS}
  CodePage := GetACP;
  if GetCPInfo(CodePage, LCPInfo) then
    if LCPInfo.MaxCharSize = 1 then
      FIsSingleByte := True;
{$ELSE}{$IFDEF FPC}
  FIsSingleByte := DetectAnsiEncoding.IsSingleByte;
{$ELSE}
  FIsSingleByte := TEncoding.ANSI.IsSingleByte;
{$ENDIF}{$ENDIF}
end;

{$IFDEF IS_UTF8}
function ANSIEncoding.GetAnsiEncoding: TEncoding;
begin
{$IFDEF FPC}
  Result := DetectAnsiEncoding;
{$ELSE}
  Result := TEncoding.ANSI;
{$ENDIF}
end;
{$ENDIF}

function ANSIEncoding.GetBytes(const chars: AnsiString): TBytes;
{$IFNDEF IS_UTF8}
var
  charCount: Integer;
{$ENDIF}
begin
{$IFDEF IS_UTF8}
  Result := GetAnsiEncoding.GetBytes(Utf8Decode(chars));
{$ELSE}
  charCount := Length(chars);
  SetLength(Result, charCount);
  if charCount > 0 then
    Move(PAnsiChar(chars)^, Pointer(Result)^, charCount);
{$ENDIF}
end;

function ANSIEncoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
{$IFDEF IS_UTF8}
  if charCount > 0 then
    Result := GetAnsiEncoding.GetBytes(Utf8Decode(chars), charIndex, charCount, bytes, byteIndex)
  else
    Result := 0;
{$ELSE}
  if charCount > 0 then
    Move(chars[charIndex], bytes[byteIndex], charCount);
  Result := charCount;
{$ENDIF}
end;

function ANSIEncoding.GetBytes(const chars: WideString): TBytes;
begin
  Result := GetBytes(AnsiString(chars));
end;

function ANSIEncoding.GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := GetBytes(AnsiString(chars), charIndex, charCount, bytes, byteIndex);
end;

function ANSIEncoding.GetAnsiString(const bytes: TBytes): AnsiString;
begin
  Result := GetAnsiString(bytes, 0, Length(bytes));
end;

function ANSIEncoding.GetAnsiString(const bytes: TBytes; index, count: Integer): AnsiString;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;

{$IFDEF IS_UTF8}
  Result := Utf8Encode(GetAnsiEncoding.GetString(bytes, index, count));
{$ELSE}
  SetLength(Result, count);
  Move(bytes[index], PAnsiChar(Result)^, Length(Result));
{$ENDIF}
end;

function ANSIEncoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := WideString(GetAnsiString(bytes));
end;

function ANSIEncoding.GetWideString(const bytes: TBytes; index, count: Integer): WideString;
begin
  Result := WideString(GetAnsiString(bytes, index, count));
end;

{ UnicodeEncoding }

function UnicodeEncoding.GetBytes(const chars: AnsiString): TBytes;
begin
  Result := GetBytes(WideString(chars));
end;

function UnicodeEncoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := GetBytes(WideString(chars), charIndex, charCount, bytes, byteIndex);
end;

function UnicodeEncoding.GetBytes(const chars: WideString): TBytes;
begin
  SetLength(Result, Length(chars) shl 1);
  GetBytes(chars, 1, Length(chars), Result, 0);
end;

function UnicodeEncoding.GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  if charCount > 0 then
    Move(chars[charIndex], bytes[byteIndex], charCount shl 1);
  Result := charCount shl 1;
end;

function UnicodeEncoding.GetAnsiString(const bytes: TBytes): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes));
end;

function UnicodeEncoding.GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes, index, count));
end;

function UnicodeEncoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function UnicodeEncoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, count shr 1);
  Move(bytes[index], PWideChar(Result)^, count);
end;

function UnicodeEncoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount * 2;
end;

function UnicodeEncoding.GetMaxCharCount(byteCount: Integer): Integer;
begin
  Result := byteCount shr 1;
end;

{ BigEndianUnicodeEncoding }

function BigEndianUnicodeEncoding.GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := inherited GetBytes(chars, charIndex, charCount, bytes, byteIndex);
  ConvertBigEndianBuffer(IntPtr(bytes), Result);
end;

function BigEndianUnicodeEncoding.GetWideString(const bytes: TBytes; index, count: integer): WideString;
var
  Len: Integer;
begin
  Len := (count - index) shr 1;
  SetLength(Result, Len);
  ConvertBigEndianBuffer(IntPtr(bytes), PWideChar(Result), Len * 2);
end;

{ UTF8Encoding }

function UTF8Encoding.GetBytes(const chars: AnsiString): TBytes;
var
  charCount: Integer;
{$IFNDEF IS_UTF8}
  UTF8: AnsiString;
{$ENDIF}
begin
{$IFDEF IS_UTF8}
  charCount := Length(chars);
  SetLength(Result, charCount);
  if charCount > 0 then
    Move(PAnsiChar(chars)^, Pointer(Result)^, charCount);
{$ELSE}
  UTF8 := CRFunctions.AnsiToUtf8(chars);

  charCount := Length(UTF8);
  SetLength(Result, charCount);
  if charCount > 0 then
    Move(PAnsiChar(UTF8)^, Pointer(Result)^, charCount);
{$ENDIF}
end;

function UTF8Encoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
{$IFNDEF IS_UTF8}
var
  ws: WideString;
  UTF8: UTF8String;
{$ENDIF}
begin
{$IFDEF IS_UTF8}
  if charCount > 0 then
    Move(chars[charIndex], bytes[byteIndex], charCount);
{$ELSE}
  if charCount > 0 then begin
    ws := copy(WideString(chars), charIndex, charCount);
    UTF8 := CRFunctions.UTF8Encode(ws);
    Move(PAnsiChar(UTF8)^, bytes[byteIndex], Length(UTF8));
  end;
{$ENDIF}
  Result := charCount;
end;

function UTF8Encoding.GetBytes(const chars: WideString): TBytes;
var
  charCount: Integer;
  UTF8: UTF8String;
begin
  UTF8 := CRFunctions.UTF8Encode(chars);

  charCount := Length(UTF8);
  SetLength(Result, charCount);
  if charCount > 0 then
    Move(PAnsiChar(UTF8)^, Pointer(Result)^, charCount);
end;

function UTF8Encoding.GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
var
  SubChars: WideString;
  UTF8: UTF8String;
begin
  SubChars := Copy(chars, charIndex, charCount);
  UTF8 := CRFunctions.UTF8Encode(SubChars);

  Result := Length(UTF8);
  if byteIndex + Result > Length(bytes) then
    raise Exception.Create('Invalid buffer length');

  if Result > 0 then
    Move(PAnsiChar(UTF8)^, bytes[byteIndex], Result);
end;

function UTF8Encoding.GetAnsiString(const bytes: TBytes): AnsiString;
begin
  Result := GetAnsiString(bytes, 0, Length(bytes));
end;

function UTF8Encoding.GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
{$IFNDEF IS_UTF8}
var
  UTF8: UTF8String;
{$ENDIF}
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;

{$IFDEF IS_UTF8}
  SetLength(Result, count);
  Move(bytes[index], PAnsiChar(Result)^, count);
{$ELSE}
  SetLength(UTF8, count);
  Move(bytes[index], PAnsiChar(UTF8)^, count);
  Result := AnsiString(CRFunctions.Utf8ToAnsi(UTF8));
{$ENDIF}
end;

function UTF8Encoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function UTF8Encoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
var
  UTF8: UTF8String;
begin
  if count = 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(UTF8, count);
  Move(bytes[index], PAnsiChar(UTF8)^, count);
  Result := CRFunctions.UTF8Decode(UTF8);
end;

function UTF8Encoding.GetMaxByteCount(charCount: Integer): Integer;
begin
  Result := charCount * 3;
end;

{$IFDEF MSWINDOWS}
constructor CodePageEncoding.Create(CodePage: Cardinal);
var
  LCPInfo: TCPInfo;
begin
  inherited Create;

  FCodePage := CodePage;

  if GetCPInfo(FCodePage, LCPInfo) then
    if LCPInfo.MaxCharSize = 1 then
      FIsSingleByte := True;
end;

function CodePageEncoding.GetBytes(const chars: AnsiString): TBytes;
begin
  Result := GetBytes(WideString(chars));
end;

function CodePageEncoding.GetBytes(const chars: AnsiString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  Result := GetBytes(WideString(chars), charIndex, charCount, bytes, byteIndex);
end;

function CodePageEncoding.GetBytes(const chars: WideString): TBytes;
var
  byteCount: Integer;
begin
  if chars = '' then
    Result := nil
  else begin
    byteCount := WideCharToMultiByte(FCodePage, 0, PWideChar(chars), Length(chars),
      nil, 0, nil, nil);
    Win32Check(LongBool(byteCount));
    SetLength(Result, byteCount);
    GetBytes(chars, 1, Length(chars), Result, 0);
  end;
end;

function CodePageEncoding.GetBytes(const chars: WideString; charIndex, charCount: Integer; var bytes: TBytes; byteIndex: Integer): Integer;
begin
  if charCount <= 0 then
    Result := 0
  else begin
    Result := WideCharToMultiByte(FCodePage, 0, PWideChar(chars) + charIndex - 1, charCount,
      PAnsiChar(bytes) + byteIndex, Length(bytes) - byteIndex, nil, nil);
    Win32Check(LongBool(Result));
  end;
end;

function CodePageEncoding.GetAnsiString(const bytes: TBytes): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes));
end;

function CodePageEncoding.GetAnsiString(const bytes: TBytes; index: Integer; count: Integer): AnsiString;
begin
  Result := AnsiString(GetWideString(bytes, index, count));
end;

function CodePageEncoding.GetWideString(const bytes: TBytes): WideString;
begin
  Result := GetWideString(bytes, 0, Length(bytes));
end;

function CodePageEncoding.GetWideString(const bytes: TBytes; index: Integer; count: Integer): WideString;
var
  charCount: Integer;
begin
  if count <= 0 then
    Result := ''
  else begin
    charCount := MultiByteToWideChar(FCodePage, 0, PAnsiChar(bytes) + index, count, nil, 0);
    Win32Check(LongBool(charCount));
    SetLength(Result, charCount);
    charCount := MultiByteToWideChar(FCodePage, 0, PAnsiChar(bytes) + index, count,
      PWideChar(Result), charCount);
    Win32Check(LongBool(charCount));
  end;
end;
{$ENDIF}
{$ENDIF}

{ AnsiStringBuilder }

constructor AnsiStringBuilder.Create(capacity: Integer);
begin
  inherited Create;

  FActualLength := 0;
  SetLength(FString, capacity);
end;

constructor AnsiStringBuilder.Create(const value: AnsiString; capacity: Integer);
begin
  Create(capacity);
  Append(value);
end;

procedure AnsiStringBuilder.SetActualLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < Value then
    SetLength(FString, FActualLength + Value + l shr 1);
  FActualLength := Value;
end;

function AnsiStringBuilder.GetChar(Index: integer): AnsiChar;
begin
  if (Index >= 0) and (Index < System.Length(FString)) then
    Result := FString[Index]
  else
    Result := {$IFNDEF NEXTGEN}#0{$ELSE}0{$ENDIF};
end;

procedure AnsiStringBuilder.Append(const value: AnsiString);
var
  l, ls: Integer;
begin
  ls := LengthA(value);
  if ls = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);
  Move(Pointer(value)^, FString[FActualLength], ls);
  Inc(FActualLength, ls);
end;

procedure AnsiStringBuilder.Append(const value: AnsiString; const startIndex: Integer; const count: Integer);
var
  l: Integer;
begin
  if count = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < count then
    SetLength(FString, FActualLength + count + l shr 1);
  Move(value[startIndex + 1]{$IFDEF NEXTGEN}^{$ENDIF}, FString[FActualLength], count);
  Inc(FActualLength, count);
end;

procedure AnsiStringBuilder.Append(const value: TBytes; const startIndex: Integer; const count: Integer);
var
  l: Integer;
begin
  if count = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < count then
    SetLength(FString, FActualLength + count + l shr 1);
  Move(value[startIndex], FString[FActualLength], count);
  Inc(FActualLength, count);
end;

procedure AnsiStringBuilder.Append(value: AnsiChar);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < 1 then
    SetLength(FString, FActualLength + 1 + l shr 1);
  FString[FActualLength] := value;
  Inc(FActualLength);
end;

procedure AnsiStringBuilder.Append(value: AnsiChar; repeatCount: Integer);
var
  s: AnsiString;
begin
  SetLengthA(s, repeatCount);
  FillChar(s[1]{$IFDEF NEXTGEN}^{$ENDIF}, repeatCount, value);
  Append(s);
end;

procedure AnsiStringBuilder.Append(value: AnsiStringBuilder);
var
  l: Integer;
begin
  if (value = nil) or (value.Length = 0) then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < value.Length then
    SetLength(FString, FActualLength + value.Length + l shr 1);
  Move(Pointer(value.FString)^, FString[FActualLength], value.Length);
  Inc(FActualLength, value.Length);
end;

procedure AnsiStringBuilder.Insert(index: Integer; const value: AnsiString);
var
  l, ls: Integer;
begin
  l := System.Length(FString);
  ls := LengthA(value);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);

  Move(FString[Index], FString[Index + ls], FActualLength - Index);
  Move(Pointer(value)^, FString[Index], ls);

  Inc(FActualLength, ls);
end;

procedure AnsiStringBuilder.Replace(const OldValue: AnsiString; const NewValue: AnsiString);

  function PosEx(const SubStr: AnsiString; const S: TAnsiCharArray; StartPos, EndPos: Integer): Integer;
  var
    I,X: Integer;
    LenSubStr: Integer;
  begin
    I := StartPos;
    LenSubStr := LengthA(SubStr);
    EndPos := EndPos - LenSubStr + 1;
    while I <= EndPos do begin
      if S[I] = AnsiChar(SubStr[1]) then begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = AnsiChar(SubStr[X + 1])) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := -1;
  end;

  procedure InsertS(index: Integer; const value: AnsiString; offset: Integer);
  var
    l, ls: Integer;
  begin
    l := System.Length(FString);
    ls := LengthA(value) - offset + 1;
    if l - FActualLength < ls then
      SetLength(FString, FActualLength + ls + l shr 1);

    if FActualLength > Index then
      Move(FString[Index], FString[Index + ls], FActualLength - Index);
    Move(value[offset]{$IFDEF NEXTGEN}^{$ENDIF}, FString[Index], ls);
  end;

var
  lOld, lNew: Integer;
  Index: Integer;
begin
  lOld := LengthA(OldValue);
  lNew := LengthA(NewValue);
  Index := PosEx(OldValue, FString, 0, FActualLength - 1);

  while Index >= 0 do begin
    if lOld > lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lNew);
      Move(FString[Index + lOld], FString[Index + lNew],
        FActualLength - Index - lOld + 1);
    end
    else
    if lOld < lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lOld);
      InsertS(Index + lOld, NewValue, lOld + 1);
    end
    else
      Move(Pointer(NewValue)^, FString[Index], lNew);

    Inc(FActualLength, lNew - lOld);
    Index := PosEx(OldValue, FString, Index + lNew, FActualLength - 1);
  end;
end;

function AnsiStringBuilder.ToString: AnsiString;
begin
  SetLengthA(Result, FActualLength);
  if FActualLength > 0 then
    Move(FString[0], Result[1]{$IFDEF NEXTGEN}^{$ENDIF}, FActualLength);
end;

{ WideStringBuilder }

constructor WideStringBuilder.Create(capacity: Integer);
begin
  inherited Create;

  FActualLength := 0;
  SetLength(FString, capacity);
end;

constructor WideStringBuilder.Create(const value: WString; capacity: Integer);
begin
  Create(capacity);
  Append(value);
end;

function WideStringBuilder.GetChar(Index: integer): WideChar;
begin
  if (Index >= 0) and (Index < System.Length(FString)) then
    Result := FString[Index]
  else
    Result := #0;
end;

procedure WideStringBuilder.SetActualLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < Value then
    SetLength(FString, FActualLength + Value + l shr 1);
  FActualLength := Value;
end;

procedure WideStringBuilder.Append(const value: WString);
var
  l, ls: Integer;
begin
  ls := System.Length(value);
  if ls = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);
  Move(Pointer(value)^, FString[FActualLength], ls * SizeOf(WideChar));
  Inc(FActualLength, ls);
end;

procedure WideStringBuilder.Append(const value: WString; const startIndex: Integer; const count: integer);
var
  l: Integer;
begin
  if count = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < count then
    SetLength(FString, FActualLength + count + l shr 1);
  Move(value[startIndex + 1], FString[FActualLength], count * SizeOf(WideChar));
  Inc(FActualLength, count);
end;

procedure WideStringBuilder.Append(value: WideChar);
var
  l: Integer;
begin
  l := System.Length(FString);
  if l - FActualLength < 1 then
    SetLength(FString, FActualLength + 1 + l shr 1);
  FString[FActualLength] := value;
  Inc(FActualLength);
end;

procedure WideStringBuilder.Append(value: WideChar; repeatCount: Integer);
var
  s: WString;
begin
  s := StringOfChar(value, repeatCount);
  Append(s);
end;

procedure WideStringBuilder.Append(value: WideStringBuilder);
var
  l: Integer;
begin
  if (value = nil) or (value.Length = 0) then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < value.Length then
    SetLength(FString, FActualLength + value.Length + l shr 1);
  Move(Pointer(value.FString)^, FString[FActualLength], value.Length * SizeOf(WideChar));
  Inc(FActualLength, value.Length);
end;

procedure WideStringBuilder.Insert(index: Integer; const value: WString);
var
  l, ls: Integer;
begin
  l := System.Length(FString);
  ls := System.Length(value);
  if l - FActualLength < ls then
    SetLength(FString, FActualLength + ls + l shr 1);

  Move(FString[Index], FString[Index + ls], (FActualLength - Index) * SizeOf(WideChar));
  Move(Pointer(value)^, FString[Index], ls * SizeOf(WideChar));

  Inc(FActualLength, ls);
end;

procedure WideStringBuilder.Replace(const OldValue: WString; const NewValue: WString);

  function PosEx(const SubStr: WideString; const S: TWideCharArray; StartPos, EndPos: Integer): Integer;
  var
    I,X: Integer;
    LenSubStr: Integer;
  begin
    I := StartPos;
    LenSubStr := System.Length(SubStr);
    EndPos := EndPos - LenSubStr + 1;
    while I <= EndPos do begin
      if S[I] = SubStr[1] then begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := -1;
  end;

  procedure InsertS(index: Integer; const value: WideString; offset: Integer);
  var
    l, ls: Integer;
  begin
    l := System.Length(FString);
    ls := System.Length(value) - offset + 1;
    if l - FActualLength < ls then
      SetLength(FString, FActualLength + ls + l shr 1);

    if FActualLength > Index then
      Move(FString[Index], FString[Index + ls],
        (FActualLength - Index) * SizeOf(WideChar));
    Move(value[offset], FString[Index], ls * SizeOf(WideChar));
  end;

var
  lOld, lNew: Integer;
  Index: Integer;
begin
  lOld := System.Length(OldValue);
  lNew := System.Length(NewValue);
  Index := PosEx(OldValue, FString, 0, FActualLength - 1);

  while Index >= 0 do begin
    if lOld > lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lNew * SizeOf(WideChar));
      Move(FString[Index + lOld], FString[Index + lNew],
        (FActualLength - Index - lOld + 1) * SizeOf(WideChar));
    end else
    if lOld < lNew then begin
      Move(Pointer(NewValue)^, FString[Index], lOld * SizeOf(WideChar));
      InsertS(Index + lOld, NewValue, lOld + 1);
    end else
      Move(Pointer(NewValue)^, FString[Index], lNew * SizeOf(WideChar));

    Inc(FActualLength, lNew - lOld);
    Index := PosEx(OldValue, FString, Index + lNew, FActualLength - 1);
  end;
end;

function WideStringBuilder.ToString: WString;
begin
  SetLength(Result, FActualLength);
  if FActualLength > 0 then
    Move(FString[0], Result[1], FActualLength * SizeOf(WideChar));
end;

{ Buffer }

class procedure Buffer.BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: array of Cardinal; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(@src[0], srcOffset)^, PtrOffset(@dst[0], dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(@src[0], srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: array of Cardinal; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(@src[0], srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: TCardinalArray; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(src, srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: TCardinalArray; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(src, srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: TBytes; srcOffset: Integer; const dst: array of Cardinal; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(src, srcOffset)^, PtrOffset(@dst[0], dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TCardinalArray; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(src, srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class procedure Buffer.BlockCopy(const src: TBytes; srcOffset: Integer; const dst: TBytes; dstOffset: Integer; count: Integer);
begin
  Move(PtrOffset(src, srcOffset)^, PtrOffset(dst, dstOffset)^, count);
end;

class function Buffer.GetByte(const src: Pointer; Index: Integer): Byte;
begin
  Result := Byte(PtrOffset(src, Index)^);
end;

class procedure Buffer.SetByte(const src: Pointer; Index: Integer; Value: Byte);
begin
  Byte(PtrOffset(src, Index)^) := Value;
end;

{ MemoryStream }

constructor MemoryStream.Create(Capacity: Integer);
begin
  inherited Create;
  System.SetLength(FData, Capacity);
end;

procedure MemoryStream.Close;
begin
  System.SetLength(FData, 0);
end;

procedure MemoryStream.SetLength(Value: Integer);
var
  l: Integer;
begin
  l := System.Length(FData); // Performance opt
  if Value > l then begin
    l := l + l shr 1;
    if Value > l then
      System.SetLength(FData, Value)
    else
      System.SetLength(FData, l);
  end;
  FLength := Value;
end;

procedure MemoryStream.SetPosition(const Pos: Integer);
begin
  if Pos > Length then
    Length := Pos;
  FPosition := Pos;
end;

function MemoryStream.Read(var Buffer: TBytes; Offset: Integer; Count: Integer): Integer;
begin
  Result := Read(PAnsiChar(@Buffer[0]), Offset, Count);
end;

function MemoryStream.Read(Buffer: PAnsiChar; Offset: Integer; Count: Integer): Integer;
begin
  if (FPosition >= 0) and (Count > 0) then begin
    Result := System.Length(FData) - FPosition;
    if Result > Count then
      Result := Count;
    Move(FData[FPosition], PtrOffset(Buffer, Offset)^, Result);
    Inc(FPosition, Result);
  end
  else
    Result := 0;
end;

function MemoryStream.ReadByte: Byte;
begin
  Result := FData[FPosition];
  Inc(FPosition);
end;

function MemoryStream.GetBuffer: TValueArr;
begin
  Result := @FData[0];
end;

function MemoryStream.ToArray: TBytes;
begin
  System.SetLength(Result, FPosition);
  if FPosition > 0 then
    Move(FData[0], Result[0], FPosition);
end;

procedure MemoryStream.Write(const Buffer: TBytes; Offset: Integer; Count: Integer);
begin
  if Count > 0 then
    Write(PAnsiChar(@Buffer[0]), Offset, Count);
end;

procedure MemoryStream.Write(Buffer: PAnsiChar; Offset: Integer; Count: Integer);
var
  l: Integer;
begin
  if (FPosition >= 0) and (Count > 0) then begin
    l := FPosition + Count;
    if l > Length then
      Length := l;
    Move(PtrOffset(Buffer, Offset)^, FData[FPosition], Count);
    Inc(FPosition, Count);
  end;
end;

procedure MemoryStream.WriteByte(Value: Byte);
var
  l: Integer;
begin
  l := FPosition + 1;
  if l > Length then
    Length := l;
  FData[FPosition] := Value;
  Inc(FPosition);
end;

function MemoryStream.Seek(Offset: Integer; Origin: Word): Integer;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := System.Length(FData) - Offset;
  end;
  if FPosition > System.Length(FData) then
    FPosition := System.Length(FData)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function CloneException(E: Exception): Exception;
begin
  Result := nil;
  if E <> nil then
    if E.ClassName = ArgumentException.ClassName then
      Result := ArgumentException.Create(E.Message)
    else
    if E.ClassName = InvalidDataException.ClassName then
      Result := InvalidDataException.Create(E.Message)
    else
    if E.ClassName = InvalidOperationException.ClassName then
      Result := InvalidOperationException.Create(E.Message)
    else
    if E.ClassName = JSONException.ClassName then
      Result := JSONException.Create(E.Message)
    else
    if E.ClassName = OperationCanceledException.ClassName then
      Result := OperationCanceledException.Create(E.Message)
    else
      Result := Exception.Create(E.Message);
end;

{ TScCancellationToken }

constructor TScCancellationToken.Create;
begin
  inherited;

  FState := ctsInited;
  FWaiter := CreateEvent;
  FLock := TCriticalSection.Create;
end;

destructor TScCancellationToken.Destroy;
begin
  FWaiter.SetEvent;
  FWaiter.Free;
  FLock.Free;

  inherited;
end;

procedure TScCancellationToken.ReInit;
begin
  FLock.Enter;
  try
    FState := ctsInited;
    SetLength(FOnCancelEventList, 0);
  finally
    FLock.Leave;
  end;

  FWaiter.ResetEvent;
end;

function TScCancellationToken.CanBeCanceled: boolean;
begin
  Result := True;
end;

procedure TScCancellationToken.Cancel;
begin
  NotifyOnCancelEvents;
end;

function TScCancellationToken.IsCancellationRequested: boolean;
begin
  Result := FState > ctsInited;
end;

procedure TScCancellationToken.ThrowIfCancellationRequested;
begin
  if IsCancellationRequested then
    raise OperationCanceledException.Create('Operation canceled');
end;

procedure TScCancellationToken.Delay(Timeout: cardinal);
begin
  ThrowIfCancellationRequested;

  if FWaiter.WaitFor(Timeout) = wrSignaled then
    if IsCancellationRequested then
      ThrowIfCancellationRequested
    else
      FWaiter.ResetEvent;
end;

procedure TScCancellationToken.NotifyOnCancelEvents;
var
  TmpOnCancelEventList: TMethodArray;
  Exceptions: ExceptionArray;
  i: integer;
begin
  SetLength(TmpOnCancelEventList, 0);

  FLock.Enter;
  try
    if FState > ctsInited then
      Exit;

    FWaiter.SetEvent;

    FState := ctsNotifying;

    TmpOnCancelEventList := FOnCancelEventList;
    FOnCancelEventList := nil;
  finally
    FLock.Leave;
  end;

  SetLength(Exceptions, 0);
  for i := 0 to Length(TmpOnCancelEventList) - 1 do begin
    try
      TThreadMethod(TmpOnCancelEventList[i])();
    except
      on E: Exception do begin
        SetLength(Exceptions, Length(Exceptions) + 1);
        Exceptions[Length(Exceptions) - 1] := CloneException(E);
      end;
    end;
  end;

  FLock.Enter;
  try
    FState := ctsCanceled;
  finally
    FLock.Leave;
  end;

  if Length(Exceptions) > 0 then
    raise AggregateException.Create(Exceptions);
end;

procedure TScCancellationToken.Register(Event: TThreadMethod);
var
  SyncCall: boolean;
  Len: integer;
  i: integer;
begin
  if not Assigned(Event) then
    Exit;

  FLock.Enter;
  try
    if FState > ctsInited then
      SyncCall := True
    else begin
      SyncCall := False;

      Len := Length(FOnCancelEventList);
      for i := 0 to Len - 1 do
        if CompareMethods(FOnCancelEventList[i], TMethod(Event)) then
          Exit;

      SetLength(FOnCancelEventList, Len + 1);
      FOnCancelEventList[Len].Code := TMethod(Event).Code;
      FOnCancelEventList[Len].Data := TMethod(Event).Data;
    end;
  finally
    FLock.Leave;
  end;

  if SyncCall then
    Event();
end;

procedure TScCancellationToken.Unregister(Event: TThreadMethod);
var
  Len: integer;
  i: integer;
begin
  if not Assigned(Event) then
    Exit;

  FLock.Enter;
  try
    i := 0;
    Len := Length(FOnCancelEventList);

    while i < Len do begin
      if CompareMethods(FOnCancelEventList[i], TMethod(Event)) then begin
        Dec(Len);
        if i < Len then
          Move(FOnCancelEventList[i + 1], FOnCancelEventList[i], (Len - i) * SizeOf(TMethod));

        SetLength(FOnCancelEventList, Len);
        Exit;
      end
      else
        Inc(i);
    end;
  finally
    FLock.Leave;
  end;
end;

{ ArgumentException }

constructor ArgumentException.Create;
begin
  inherited Create('');
end;

constructor ArgumentException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{ NotSupportedException }

constructor NotSupportedException.Create;
begin
  inherited Create('');
end;

constructor NotSupportedException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{ AggregateException }

constructor AggregateException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

constructor AggregateException.Create(const AExceptions: ExceptionArray);
var
  AMessage: string;
  i: integer;
begin
  AMessage := '';
  for i := 0 to Length(AExceptions) - 1 do begin
    if AMessage <> '' then
      AMessage := AMessage + #13#10;
    AMessage := AMessage + AExceptions[i].Message;
  end;

  inherited Create(AMessage);

  FExceptions := AExceptions;
end;

destructor AggregateException.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FExceptions) - 1 do
    FExceptions[i].Free;

  inherited;
end;

{$IFNDEF NEXTGEN}
initialization
  InitEncodings;

finalization
  FreeEncodings;
{$ENDIF}

end.
