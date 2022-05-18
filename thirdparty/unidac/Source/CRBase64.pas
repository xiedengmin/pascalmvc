
//////////////////////////////////////////////////
//  Devart Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRBase64
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRBase64;

interface

uses
  SysUtils, Classes,
  CRTypes;

type
  TBase64 = class
  public
    class procedure InternalEncode(const Data: TBytes; Offset, Count: integer;
      const OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
    class function Encode(const Data: TBytes): TBytes; overload;
    class function Encode(const Data: TBytes; Offset, Count: Integer): TBytes; overload;
    class procedure Encode(InStream, OutStream: TStream); overload;

    class procedure InternalDecode(const InBuf: TBytes; const InOffset, InCount: integer;
      const OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinised: boolean);
    class function Decode(const Data: TBytes): TBytes; overload;
    class function Decode(const Data: TBytes; Offset, Count: Integer): TBytes; overload;
    class procedure Decode(InStream, OutStream: TStream); overload;
  end;

implementation

const
  BUFFER_SIZE = 64 * 1024 - 1; // must be divisible by 3

  PAD: Integer = 64;

  _fromBase64: array[0..127] of Integer = (
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, -1, -1, -1, -1, -1,
                 -1, -1, -1, 62, -1, -1, -1, 63,
                 52, 53, 54, 55, 56, 57, 58, 59,
                 60, 61, -1, -1, -1, -1, -1, -1,
                 -1,  0,  1,  2,  3,  4,  5,  6,
                 7,  8,  9, 10, 11, 12, 13, 14,
                 15, 16, 17, 18, 19, 20, 21, 22,
                 23, 24, 25, -1, -1, -1, -1, -1,
                 -1, 26, 27, 28, 29, 30, 31, 32,
                 33, 34, 35, 36, 37, 38, 39, 40,
                 41, 42, 43, 44, 45, 46, 47, 48,
                 49, 50, 51, -1, -1, -1, -1, -1
                 );

  _toBase64: array[0..64] of byte = (
                 // 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
                 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
                 // 'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
                 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99,100,101,102,
                 // 'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
                 103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,
                 // 'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
                 119,120,121,122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47, Ord('=')
                 );

class function TBase64.Decode(const Data: TBytes): TBytes;
begin
  Result := Decode(Data, 0, Length(Data));
end;

class function TBase64.Decode(const Data: TBytes; Offset, Count: Integer): TBytes;
var
  IsFinised: boolean;
  ResCount, UnReadCount: Integer;
begin
  SetLength(Result, (Count * 3) div 4);
  IsFinised := True;

  InternalDecode(Data, Offset, Count, Result, 0, ResCount, UnReadCount, IsFinised);
  if Length(Result) > ResCount then
    SetLength(Result, ResCount);
end;

class procedure TBase64.Decode(InStream, OutStream: TStream);
var
  IsFinised: boolean;
  InStreamSize: Int64;
  InBuf, OutBuf: TBytes;
  InCount, OutCount, UnReadCount: Integer;
begin
  if (InStream = nil) or (OutStream = nil) or (InStream = OutStream) then
    raise Exception.Create('Invalid input arguments');

  SetLength(InBuf, BUFFER_SIZE);
  SetLength(OutBuf, BUFFER_SIZE);
  IsFinised := False;
  InCount := BUFFER_SIZE;
  InStreamSize := InStream.Size;

  while True do begin
    if (InStreamSize - InStream.Position) < BUFFER_SIZE then begin
      InCount := InStreamSize - InStream.Position;
      IsFinised := True;
      if InCount = 0 then
        break;
    end;

    InStream.ReadBuffer(InBuf, InCount);
    InternalDecode(InBuf, 0, InCount, OutBuf, 0, OutCount, UnReadCount, IsFinised);
    OutStream.WriteBuffer(OutBuf[0], OutCount);
    if IsFinised then
      break;

    if UnReadCount > 0 then
      InStream.Position := InStream.Position - UnReadCount;
  end;

  OutStream.Size := OutStream.Position;
end;

class procedure TBase64.InternalDecode(const InBuf: TBytes; const InOffset, InCount: integer;
  const OutBuf: TBytes; OutOffset: integer; out OutCount, UnReadCount: integer; var IsFinised: boolean);
var
  LastInOffset: integer;
  i, j, c, v: integer;
  bits: integer;
begin
  OutCount := 0;
  LastInOffset := InOffset;

  bits := 0;
  v := 0;
  j := OutOffset;
  for i := InOffset to InOffset + InCount - 1 do begin
    if InBuf[i] > 127 then
      continue;

    c := _fromBase64[InBuf[i]];
    if c < 0 then begin
      if InBuf[i] = _toBase64[PAD] then begin
        IsFinised := True;
        break;
      end;
      continue;
    end;

    v := (v shl 6) or c;
    bits := bits + 6;
    if bits >= 8 then begin
      bits := bits - 8;
      OutBuf[j] := byte(v shr bits);
      Inc(j);
      if bits = 0 then begin
        LastInOffset := i + 1;
        OutCount := j;
      end;
    end;
  end;

  if IsFinised then
    OutCount := j;
  if OutCount > 0 then
    OutCount := OutCount - OutOffset;

  UnReadCount := InCount - (LastInOffset - InOffset);
end;

class function TBase64.Encode(const Data: TBytes): TBytes;
begin
  Result := Encode(Data, 0, Length(Data));
end;

class procedure TBase64.Encode(InStream, OutStream: TStream);
var
  Count: Int64;
  InBuf, OutBuf: TBytes;
  InCount, OutCount: Integer;
begin
  if (InStream = nil) or (OutStream = nil) or (InStream = OutStream) then
    raise Exception.Create('Invalid input arguments');

  Assert((BUFFER_SIZE mod 3) = 0);
  SetLength(InBuf, BUFFER_SIZE);
  SetLength(OutBuf, (BUFFER_SIZE div 3) * 4);
  InCount := BUFFER_SIZE;
  Count := InStream.Size - InStream.Position;

  while Count > 0 do begin
    if Count < BUFFER_SIZE then
      InCount := Count;

    InStream.ReadBuffer(InBuf, InCount);
    InternalEncode(InBuf, 0, InCount, OutBuf, 0, OutCount);
    OutStream.WriteBuffer(OutBuf[0], OutCount);
    Dec(Count, InCount);
  end;
end;

class function TBase64.Encode(const Data: TBytes; Offset, Count: Integer): TBytes;
var
  OutCount: Integer;
begin
  if (Count mod 3) <> 0 then
    OutCount := (Count div 3) * 4 + 4
  else
    OutCount := (Count div 3) * 4;
  SetLength(Result, OutCount);

  InternalEncode(Data, Offset, Count, Result, 0, OutCount);
end;

class procedure TBase64.InternalEncode(const Data: TBytes; Offset, Count: integer;
  const OutBuf: TBytes; OutOffset: integer; out OutCount: integer);
var
  x1, x2, x3, x4: Byte;
  i, j, r, n: Integer;
begin
  r := Count mod 3;
  if r <> 0 then
    OutCount := (Count div 3) * 4 + 4
  else
    OutCount := (Count div 3) * 4;

  n := Offset + (Count - r);
  i := Offset;
  j := OutOffset;
  while i < n do begin
    x1 := (Data[i] {and $fc}) shr 2;
    x2 := ((Data[i    ] and $03) shl 4) or ((Data[i + 1] {and $f0}) shr 4);
    x3 := ((Data[i + 1] and $0f) shl 2) or ((Data[i + 2] {and $c0}) shr 6);
    x4 := (Data[i + 2] and $3f);
    OutBuf[j    ] := _toBase64[x1];
    OutBuf[j + 1] := _toBase64[x2];
    OutBuf[j + 2] := _toBase64[x3];
    OutBuf[j + 3] := _toBase64[x4];
    i := i + 3;
    j := j + 4;
  end;

  if r <> 0 then begin
    x1 := (Data[i] {and $fc}) shr 2;
    x2 := (Data[i] and $03) shl 4;
    if r = 2 then begin
      x2 := x2 or ((Data[i + 1] {and $f0}) shr 4);
      x3 := (Data[i + 1] and $0f) shl 2;
    end
    else
      x3 := PAD;
    x4 := PAD;
    OutBuf[j    ] := _toBase64[x1];
    OutBuf[j + 1] := _toBase64[x2];
    OutBuf[j + 2] := _toBase64[x3];
    OutBuf[j + 3] := _toBase64[x4];
  end;
end;

end.
