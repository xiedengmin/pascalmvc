
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgSQLNetUni;

interface

uses
  Classes, SysUtils, SyncObjs,
  CRTypes, CLRClasses, CRVio,
{$IFNDEF UNIDACPRO}
  PgError;
{$ELSE}
  PgErrorUni;
{$ENDIF}

const
  OffsetStackDelta = 25;
  ReadBufferSize = 1024 * 64;
  WriteBufferSize = 1024 * 8;
  TempBufferSize = 1024 * 64;

type
  TPgSQLNet = class
  private
    FVio: TCRVio;
    FReadBuffer: MemoryStream;
    FWriteBuffer: MemoryStream;
    FReadBufLen: integer;
    FTempBuffer: TBytes;

    FOffsetStack: array of integer;
    FOffsetStackPos: integer;

    FUseUnicode: boolean;

    procedure SetVio(const Value: TCRVio);
    procedure PushOffset(const Offset: integer);
    procedure PopOffset(var Offset: integer);

    procedure UpdateReadBuffer;
    procedure CheckReadBuffer;
    function VioRead(Buffer: TValueArr; Offset, Count: integer): integer;
    procedure VioWrite(const Buffer: TValueArr; Offset, Count: integer);
  public
    constructor Create;
    destructor Destroy; override;

    function FlushSend: Boolean;

    procedure WriteBytes(Buffer: TBytes); overload;
    procedure WriteBytes(Buffer: TValueArr; Offset, Count: integer); overload;
    procedure WriteByte(Value: Byte);
    procedure WriteWord(Value: Word);
    procedure WriteInt16(Value: Smallint);
    procedure WriteInt32(Value: integer); overload;
    procedure WriteInt32(Value: integer; Offset: integer); overload;
    procedure WriteInt64(Value: Int64);
    procedure WriteDouble(Value: Double);
    procedure WriteSingle(Value: Single);
    procedure WriteAnsiChar(Value: AnsiChar);
    procedure WriteAnsiString(const Value: AnsiString); overload;
    procedure WriteString(const Value: string);
    procedure WriteStringAsBytes(Buffer: TBytes); overload;

    procedure FlushReceive(Count: integer);

    procedure ReadBytes(Buffer: TValueArr; Offset, Count: integer);
    function ReadByte: Byte;
    function ReadWord: word;
    function ReadInt16: Smallint;
    function ReadInt32: integer;
    function ReadInt64: Int64;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadAnsiChar: AnsiChar;
    function ReadAnsiString: AnsiString; overload;
    function ReadAnsiString(Count: integer): AnsiString; overload;
    function ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload;
    function ReadWideString: WideString; overload;
    function ReadWideString(Count: integer): WideString; overload;
    function ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload;
    function ReadString(Count: integer): string;
    function ReadStringAsBytes(var Count: integer): TBytes;

    procedure EnterSizeBlock;
    procedure LeaveSizeBlock(AllowSizeRoom: boolean = False);
    procedure ClearWriteBuffer;

    function GetWriteBufferSize: integer;
    function GetReadBufferAvailable: integer;

    property Vio: TCRVio read FVio write SetVio;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode;
  end;

  TPgBufferProvider = class
  public
    procedure ReadBytes(Buffer: TValueArr; Offset, Count: integer); virtual; abstract;
    function ReadByte: Byte; virtual; abstract;
    function ReadWord: Word; virtual; abstract;
    function ReadInt16: Smallint; virtual; abstract;
    function ReadInt32: integer; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;
    function ReadSingle: Single; virtual; abstract;
    function ReadDouble: Double; virtual; abstract;
    function ReadAnsiChar: AnsiChar; virtual; abstract;
    function ReadAnsiString: AnsiString; overload; virtual; abstract;
    function ReadAnsiString(Count: integer): AnsiString; overload; virtual; abstract;
    function ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; virtual; abstract;
    function ReadWideString: WideString; overload; virtual; abstract;
    function ReadWideString(Count: integer): WideString; overload; virtual; abstract;
    function ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; virtual; abstract;
    function ReadString(Count: integer): string; virtual; abstract;
    function ReadStringAsBytes(var Count: integer): TBytes; virtual; abstract;
  end;

  TPgNetBufferProvider = class(TPgBufferProvider)
  private
    FNet: TPgSQLNet;
  public
    constructor Create(const Net: TPgSQLNet);

    procedure ReadBytes(Buffer: TValueArr; Offset, Count: integer); override;
    function ReadByte: Byte; override;
    function ReadWord: Word; override;
    function ReadInt16: Smallint; override;
    function ReadInt32: integer; override;
    function ReadInt64: Int64; override;
    function ReadSingle: Single; override;
    function ReadDouble: Double; override;
    function ReadAnsiChar: AnsiChar; override;
    function ReadAnsiString: AnsiString; overload; override;
    function ReadAnsiString(Count: integer): AnsiString; overload; override;
    function ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; override;
    function ReadWideString: WideString; overload; override;
    function ReadWideString(Count: integer): WideString; overload; override;
    function ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer; overload; override;
    function ReadString(Count: integer): string; override;
    function ReadStringAsBytes(var Count: integer): TBytes; override;
  end;

function FastSwap(Value: SmallInt): SmallInt; overload;
function FastSwap(Value: Word): Word; overload;
function FastSwap(Value: Integer): Integer; overload;
function FastSwap(Value: Cardinal): Cardinal; overload;
function FastSwap(Value: Int64): Int64; overload;{$IFDEF USE_INLINE} inline;{$ENDIF}
function FastSwap(Value: Double): Double; overload;{$IFDEF USE_INLINE} inline;{$ENDIF}
{$IFDEF VER7P}
function FastSwap(Value: UInt64): UInt64; overload;{$IFDEF USE_INLINE} inline;{$ENDIF}
{$ENDIF}

implementation

uses
  CRFunctions;

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

{ TPgSQLNet }

constructor TPgSQLNet.Create;
begin
  inherited;

  FReadBuffer := MemoryStream.Create(ReadBufferSize);
  FReadBuffer.SetLength(ReadBufferSize);
  FWriteBuffer := MemoryStream.Create(WriteBufferSize);

  SetLength(FTempBuffer, TempBufferSize);
end;

destructor TPgSQLNet.Destroy;
begin
  SetVio(nil);
  FReadBuffer.Free;
  FWriteBuffer.Free;

  inherited;
end;

procedure TPgSQLNet.SetVio(const Value: TCRVio);
begin
  if FVio <> nil then begin
    FVio.Close;
    FVio.Free;
  end;
  FVio := Value;
end;

function TPgSQLNet.VioRead(Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if FVio <> nil then begin
    Result := FVio.ReadNoWait(Buffer, Offset, Count);
    if Result = 0 then
      raise EPgError.Create(sFatal, FVio.LastErrorCode, FVio.LastError);
  end
  else
    Result := 0;
end;

procedure TPgSQLNet.VioWrite(const Buffer: TValueArr; Offset, Count: integer);
var
  Res: integer;
begin
  if FVio <> nil then begin
    Res := FVio.Write(Buffer, Offset, Count);
    if Res <> Count then
      raise EPgError.Create(sFatal, FVio.LastErrorCode, FVio.LastError);
  end;
end;

procedure TPgSQLNet.PushOffset(const Offset: integer);
begin
  Inc(FOffsetStackPos);
  if FOffsetStackPos >= Length(FOffsetStack) then
    SetLength(FOffsetStack, Length(FOffsetStack) + OffsetStackDelta);
  FOffsetStack[FOffsetStackPos] := Offset;
end;

procedure TPgSQLNet.PopOffset(var Offset: integer);
begin
  Offset := FOffsetStack[FOffsetStackPos];
  Dec(FOffsetStackPos);
end;

procedure TPgSQLNet.UpdateReadBuffer;
var
  Buf: TValueArr;
begin
  FReadBuffer.Position := 0;
  Buf := FReadBuffer.GetBuffer;
  FReadBufLen := VioRead(Buf, 0, ReadBufferSize);
end;

procedure TPgSQLNet.CheckReadBuffer;
begin
  if (FReadBufLen = 0) or (FReadBuffer.Position >= FReadBufLen) then
    UpdateReadBuffer;
end;

function TPgSQLNet.FlushSend: Boolean;
begin
  Result := False;
  if FWriteBuffer.Length > 0 then begin
    VioWrite(FWriteBuffer.GetBuffer, 0, FWriteBuffer.Length);
    FWriteBuffer.SetLength(0);
    FWriteBuffer.Position := 0;
    Result := True;
  end;
end;

procedure TPgSQLNet.WriteBytes(Buffer: TBytes);
begin
  FWriteBuffer.Write(Buffer, 0, Length(Buffer));
end;

procedure TPgSQLNet.WriteBytes(Buffer: TValueArr; Offset, Count: integer);
begin
  FWriteBuffer.Write(Buffer, Offset, Count);
end;

procedure TPgSQLNet.WriteByte(Value: Byte);
begin
  FWriteBuffer.WriteByte(Value);
end;

procedure TPgSQLNet.WriteWord(Value: Word);
begin
  WriteByte(Byte(Value shr 8));
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteInt16(Value: Smallint);
begin
  WriteByte(Byte(Value shr 8));
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteInt32(Value: integer);
begin
  Value := FastSwap(Value);
  FWriteBuffer.Write(TValueArr(@Value), 0, SizeOf(Value));
end;

procedure TPgSQLNet.WriteInt32(Value: integer; Offset: integer);
var
  OldPos: integer;
begin
  Oldpos := FWriteBuffer.Position;
  try
    FWriteBuffer.Position := Offset;
    WriteInt32(Value);
  finally
    FWriteBuffer.Position := OldPos;
  end;
end;

procedure TPgSQLNet.WriteInt64(Value: Int64);
begin
  Value := FastSwap(Value);
  FWriteBuffer.Write(TValueArr(@Value), 0, SizeOf(Value));
end;

procedure TPgSQLNet.WriteDouble(Value: Double);
var
  i: integer;
begin
  for i := 0 to 7 do
    WriteByte(PByte(PtrOffset(@Value, 7 - i))^);
end;

procedure TPgSQLNet.WriteSingle(Value: Single);
var
  i: integer;
begin
  i := BitConverter.ToInt32(BitConverter.GetBytes(Value), 0);
  WriteInt32(i);
end;

procedure TPgSQLNet.WriteAnsiChar(Value: AnsiChar);
begin
  WriteByte(Byte(Value));
end;

procedure TPgSQLNet.WriteAnsiString(const Value: AnsiString);
begin
  if Length(Value) > 0 then // AV in CLR if Value = ''
    WriteBytes(TValueArr(Value), 0, LengthA(Value));
  WriteByte(0);
end;

procedure TPgSQLNet.WriteString(const Value: string);
var
  Buffer: TBytes;
begin
  if FUseUnicode then
    Buffer := Encoding.UTF8.GetBytes(Value)
  else
    Buffer := Encoding.Default.GetBytes(Value);
  WriteStringAsBytes(Buffer);
end;

procedure TPgSQLNet.WriteStringAsBytes(Buffer: TBytes);
begin
  WriteBytes(Buffer);
  WriteByte(0);
end;

procedure TPgSQLNet.FlushReceive(Count: integer);
var
  ReadCount: integer;
  PieceLen: integer;
  Size: integer;
begin
  ReadCount := 0;
  while ReadCount < Count do begin
    CheckReadBuffer;

    PieceLen := FReadBufLen - FReadBuffer.Position;
    if PieceLen > Count - ReadCount then
      PieceLen := Count - ReadCount;

    if (FReadBuffer.Position >= 0) and (PieceLen > 0) then begin
      Size := System.Length(TBytes(FReadBuffer.GetBuffer));
      if Size > PieceLen then
        Size := PieceLen;
      FReadBuffer.Position := FReadBuffer.Position + Size;
    end;

    ReadCount := ReadCount + PieceLen;
  end;
end;

procedure TPgSQLNet.ReadBytes(Buffer: TValueArr; Offset, Count: integer);
var
  ReadCount: integer;
  PieceLen: integer;
begin
  ReadCount := 0;
  while ReadCount < Count do begin
    CheckReadBuffer;

    PieceLen := FReadBufLen - FReadBuffer.Position;
    if PieceLen > Count - ReadCount then
      PieceLen := Count - ReadCount;

    FReadBuffer.Read(Buffer, Offset + ReadCount, PieceLen);
    ReadCount := ReadCount + PieceLen;
  end;
end;

function TPgSQLNet.ReadByte: Byte;
begin
  CheckReadBuffer;
  Result := FReadBuffer.ReadByte;
end;

function TPgSQLNet.ReadWord: word;
begin
  Result := ReadByte shl 8 or ReadByte;
end;

function TPgSQLNet.ReadInt16: Smallint;
begin
  Result := Smallint(ReadByte shl 8 or ReadByte);
end;

function TPgSQLNet.ReadInt32: integer;
begin
  Result := ReadByte shl 24 or ReadByte shl 16 or ReadByte shl 8 or ReadByte;
end;

function TPgSQLNet.ReadInt64: Int64;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;  
  
  Result := BitConverter.ToInt64(Buf, 0);
end;

function TPgSQLNet.ReadSingle: Single;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 4);
  ReadBytes(TValueArr(Buf), 0, 4);

  for i := 0 to 1 do begin
     Buf[i] := Buf[i] xor Buf[3 - i];
     Buf[3 - i] := Buf[i] xor Buf[3 - i];
     Buf[i] := Buf[i] xor Buf[3 - i];
  end;

  Result := BitConverter.ToSingle(Buf, 0);
end;

function TPgSQLNet.ReadDouble: Double;
var
  i: integer;
  Buf: TBytes;
begin
  SetLength(Buf, 8);
  ReadBytes(TValueArr(Buf), 0, 8);

  for i := 0 to 3 do begin
     Buf[i] := Buf[i] xor Buf[7 - i];
     Buf[7 - i] := Buf[i] xor Buf[7 - i];
     Buf[i] := Buf[i] xor Buf[7 - i];
  end;

  Result := BitConverter.ToDouble(Buf, 0);
end;

function TPgSQLNet.ReadAnsiChar: AnsiChar;
begin
  Result := AnsiChar(ReadByte);
end;

function TPgSQLNet.ReadAnsiString: AnsiString;
begin
  Result := ReadAnsiString(-1);
end;

function TPgSQLNet.ReadAnsiString(Count: integer): AnsiString;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then
    if FUseUnicode then
    {$IFDEF NEXTGEN}
      Result := AnsiString(Encoding.UTF8.GetString(Buf, 0, Count))
    {$ELSE}
      Result := Encoding.UTF8.GetAnsiString(Buf, 0, Count)
    {$ENDIF}
    else
      Result := Marshal.PtrToStringAnsi(Buf, Count)
  else
    Result := '';
end;

function TPgSQLNet.ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer;
var
  sa: AnsiString;
begin
  if FUseUnicode then begin
    sa := ReadAnsiString(Count);
    Result := Length(sa);
    if Result > 0 then
      Move(sa[1]{$IFDEF NEXTGEN}^{$ENDIF}, Buffer^, Result);
  end
  else begin
    ReadBytes(Buffer, 0, Count);
    Result := Count;
  end;

  if AddNull then
    Marshal.WriteByte(Buffer, Result, 0);
end;

function TPgSQLNet.ReadWideString: WideString;
begin
  Result := ReadWideString(-1);
end;

function TPgSQLNet.ReadWideString(Count: integer): WideString;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then begin
    if FUseUnicode then
      Result := Encoding.UTF8.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(Buf, 0, Count)
    else
      Result := Encoding.Default.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(Buf, 0, Count);
  end
  else
    Result := '';
end;

function TPgSQLNet.ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean = True): integer;
var
  s: WideString;
begin
  s := ReadWideString(Count);
  Result := Length(s) * sizeof(WideChar);
  if s <> '' then
    Move(s[1], Buffer^, Result);

  if AddNull then
    Marshal.WriteInt16(Buffer, Result, 0);
end;

function TPgSQLNet.ReadStringAsBytes(var Count: integer): TBytes;
var
  i, p: Integer;
  ScrLen: Integer;
  DataLen: Integer;
  ResLen: integer;
  Buf: TValueArr;
begin
  CheckReadBuffer;

  Buf := FReadBuffer.GetBuffer;
  p := FReadBuffer.Position;
  i := p;

  ResLen := 0;
  if Count < 0 then
    ScrLen := 512
  else
    ScrLen := Count;
  if ScrLen > TempBufferSize then
    SetLength(Result, ScrLen)
  else
    Result := FTempBuffer;

  while (ScrLen > 0) and (Byte(Buf[i]) <> 0) do begin
    Inc(i);
    Dec(ScrLen);

    if i >= FReadBufLen then begin
      DataLen := i - p;
      Move(Buf[p], Result[ResLen], DataLen);
      ResLen := ResLen + DataLen;
      UpdateReadBuffer;
      p := 0;
      i := 0;
    end;
  end;

  DataLen := i - p;
  Move(Buf[p], Result[ResLen], DataLen);
  ResLen := ResLen + DataLen;

  if Count < 0 then
    FReadBuffer.Position := i + 1
  else
    FReadBuffer.Position := i;

  Count := ResLen;
end;

function TPgSQLNet.ReadString(Count: integer): string;
var
  Buf: TBytes;
begin
  Buf := ReadStringAsBytes(Count);
  if Count > 0 then
    if FUseUnicode then
      Result := Encoding.UTF8.GetString(Buf, 0, Count)
    else
      Result := Encoding.Default.GetString(Buf, 0, Count)
  else
    Result := '';
end;

procedure TPgSQLNet.EnterSizeBlock;
begin
  PushOffset(FWriteBuffer.Position);
  WriteInt32(0);
end;

procedure TPgSQLNet.LeaveSizeBlock(AllowSizeRoom: boolean = False);
var
  Offset: integer;
begin
  PopOffset(Offset);
  if AllowSizeRoom then
    WriteInt32(FWriteBuffer.Position - Offset, Offset)
  else
    WriteInt32(FWriteBuffer.Position - Offset - 4, Offset)
end;

procedure TPgSQLNet.ClearWriteBuffer;
begin
  FWriteBuffer.SetLength(0);
  FWriteBuffer.Position := 0;
end;

function TPgSQLNet.GetWriteBufferSize: integer;
begin
  Result := FWriteBuffer.Length;
end;

function TPgSQLNet.GetReadBufferAvailable: integer;
begin
  Result := FReadBufLen - FReadBuffer.Position;
end;

{ TPgNetBuferProvider }

constructor TPgNetBufferProvider.Create(const Net: TPgSQLNet);
begin
  inherited Create;

  FNet := Net;
end;

procedure TPgNetBufferProvider.ReadBytes(Buffer: TValueArr; Offset, Count: integer);
begin
  FNet.ReadBytes(Buffer, Offset, Count);
end;

function TPgNetBufferProvider.ReadByte: Byte;
begin
  Result := FNet.ReadByte;
end;

function TPgNetBufferProvider.ReadWord: Word;
begin
  Result := FNet.ReadWord;
end;

function TPgNetBufferProvider.ReadInt16: Smallint;
begin
  Result := FNet.ReadInt16;
end;

function TPgNetBufferProvider.ReadInt32: integer;
begin
  Result := FNet.ReadInt32;
end;

function TPgNetBufferProvider.ReadInt64: Int64;
begin
  Result := FNet.ReadInt64;
end;

function TPgNetBufferProvider.ReadSingle: Single;
begin
  Result := FNet.ReadSingle;
end;

function TPgNetBufferProvider.ReadDouble: Double;
begin
  Result := FNet.ReadDouble;
end;

function TPgNetBufferProvider.ReadAnsiChar: AnsiChar;
begin
  Result := FNet.ReadAnsiChar;
end;

function TPgNetBufferProvider.ReadAnsiString: AnsiString;
begin
  Result := FNet.ReadAnsiString;
end;

function TPgNetBufferProvider.ReadAnsiString(Count: integer): AnsiString;
begin
  Result := FNet.ReadAnsiString(Count);
end;

function TPgNetBufferProvider.ReadAnsiString(Buffer: IntPtr; Count: integer; AddNull: boolean): integer;
begin
  Result := FNet.ReadAnsiString(Buffer, Count, AddNull);
end;

function TPgNetBufferProvider.ReadWideString: WideString;
begin
  Result := FNet.ReadWideString;
end;

function TPgNetBufferProvider.ReadWideString(Count: integer): WideString;
begin
  Result := FNet.ReadWideString(Count);
end;

function TPgNetBufferProvider.ReadWideString(Buffer: IntPtr; Count: integer; AddNull: boolean): integer;
begin
  Result := FNet.ReadWideString(Buffer, Count, AddNull);
end;

function TPgNetBufferProvider.ReadString(Count: integer): string;
begin
  Result := FNet.ReadString(Count);
end;

function TPgNetBufferProvider.ReadStringAsBytes(var Count: integer): TBytes;
begin
  Result := FNet.ReadStringAsBytes(Count);
end;

end.
