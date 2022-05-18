//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR Hash
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRHash;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

uses
  SysUtils,
  CLRClasses, CRTypes, CRFunctions, CRDECUtil, CRHashAlgorithm;

type
  THash_MD2 = class(THashAlgorithm)
  private
    FCount: integer;
    FBuffer: array[0..15] of byte;
    FCheckSum: array[0..15] of byte;
    X: array[0..47] of byte;

  protected
    procedure Transform(Buffer: PByteArray);
    procedure Done;
    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create; override;
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_MD4 = class(THashAlgorithm)
  private
    FCount: Cardinal;
    FBuffer: array[0..15] of Cardinal;
    FDigest: array[0..7] of Cardinal;

  protected
    procedure Transform(Buffer: TCardinalArray); virtual;
    procedure Done; virtual;
    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create; override;
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_MD5 = class(THash_MD4)
  protected
    procedure Transform(Buffer: TCardinalArray); override;
  end;

  THash_SHA = class(THash_MD4)
  protected
    procedure Done; override;
  end;

  THash_SHA1 = class(THash_SHA)
  protected
    FRotate: Boolean;
    procedure Transform(Buffer: TCardinalArray); override;
  public
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_SHA2_256 = class(THash_SHA)
  protected
    procedure Transform(Buffer: TCardinalArray); override;
  public
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_SHA2_224 = class(THash_SHA2_256)
  public
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_SHA2_512 = class(THashAlgorithm)
  private
    FCount: Int64;
    FBuffer: array[0..15] of UInt64;
    FDigest: array[0..7] of UInt64;

  protected
    procedure Transform(Buffer: TUInt64Array); virtual;
    procedure Done; virtual;

    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create; override;
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

  THash_SHA2_384 = class(THash_SHA2_512)
  public
    procedure Initialize; override;
    class function GetHashSize: Integer; override;
  end;

implementation

{ THash_MD2 }

constructor THash_MD2.Create;
begin
  inherited;

  Initialize;
end;

class function THash_MD2.GetHashSize: Integer;
begin
  Result := 16;
end;

procedure THash_MD2.Initialize;
begin
  FillChar(FCheckSum, SizeOf(FCheckSum), 0);
  FillChar(X, SizeOf(X), 0);
  FCount := 0;
end;

procedure THash_MD2.Done;
var
  Index: integer;
begin
  Index := FCount and $F;
  FillChar(FBuffer[Index], 16 - Index, 16 - Index);
  Transform(@FBuffer);
  Transform(@FCheckSum);
end;

procedure THash_MD2.HashCore(const Data: TValueArr; Offset, Count: Integer);
var
  Index: integer;
  Buf: PByteArray;
begin
  if (Data = nil) or (Count <= 0) then
    Exit;

  Index := FCount and $F;
  Inc(FCount, Count);
  if Index > 0 then begin
    if Count < 16 - Index then begin
      Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, Count);
      Exit;
    end;
    Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, 16 - Index);
    Transform(@FBuffer);
    Index := 16 - Index;
    Dec(Count, Index);
  end;

  if Count >= 16 then
    Buf := @Data[Index + Offset]
  else
    Buf := nil;

  Inc(Index, Count and not $F);
  while Count >= 16 do begin
    Transform(Buf);
    Inc(PAnsiChar(Buf), 16);
    Dec(Count, 16);
  end;
  Move(Data[Index + Offset], FBuffer[0], Count);
end;

function THash_MD2.HashFinal: TBytes;
begin
  Done;
  SetLength(Result, HashSize);
  Move(X[0], Result[0], HashSize);
end;

procedure THash_MD2.Transform(Buffer: PByteArray);
const
  PI_SUBST: array[0..255] of byte = (
    41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6, 19,
    98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188, 76, 130, 202,
    30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24, 138, 23, 229, 18,
    190, 78, 196, 214, 218, 158, 222, 73, 160, 251, 245, 142, 187, 47, 238, 122,
    169, 104, 121, 145, 21, 178, 7, 63, 148, 194, 16, 137, 11, 34, 95, 33,
    128, 127, 93, 154, 90, 144, 50, 39, 53, 62, 204, 231, 191, 247, 151, 3,
    255, 25, 48, 179, 72, 165, 181, 209, 215, 94, 146, 42, 172, 86, 170, 198,
    79, 184, 56, 210, 150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241,
    69, 157, 112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2,
    27, 96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
    85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197, 234, 38,
    44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65, 129, 77, 82,
    106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123, 8, 12, 189, 177, 74,
    120, 136, 149, 139, 227, 99, 232, 109, 233, 203, 213, 254, 59, 0, 29, 57,
    242, 239, 183, 14, 102, 88, 208, 228, 166, 119, 114, 248, 235, 117, 75, 10,
    49, 68, 80, 180, 143, 237, 31, 26, 219, 153, 141, 51, 159, 17, 131, 20
  );

var
  i, j: integer;
  t: byte;
begin
  Move(Buffer^, X[16], 16);
  for i := 0 to 15 do
    X[32 + i] := X[16 + i] xor X[i];

  t := 0;
  for i := 0 to 17 do begin
    X[0] := X[0] xor PI_SUBST[t];
    for j := 1 to 47 do
      X[j] := X[j] xor PI_SUBST[X[j - 1]];

    t := (X[47] + i) and $FF;
  end;

  FCheckSum[0] := FCheckSum[0] xor PI_SUBST[Buffer[0] xor FCheckSum[15]];
  for i := 1 to 15 do
    FCheckSum[i] := FCheckSum[i] xor PI_SUBST[Buffer[i] xor FCheckSum[i - 1]];
end;

{ THash_MD4 }

constructor THash_MD4.Create;
begin
  inherited;

  Initialize;
end;

class function THash_MD4.GetHashSize: Integer;
begin
  Result := 16;
end;

procedure THash_MD4.Initialize;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FCount := 0;

  /// MD4, MD5, SHA1 use this Init Key
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $C3D2E1F0;
end;

procedure THash_MD4.Done;
var
  Index: Integer;
  S: Int64;
begin
  Index := FCount and $3F;
  PByteArray(@FBuffer)[Index] := $80;
  Inc(Index);
  if Index > 64 - 8 then begin
    FillChar(PByteArray(@FBuffer)[Index], 64 - Index, 0);
    Transform(@FBuffer);
    Index := 0;
  end;

  FillChar(PByteArray(@FBuffer)[Index], 64 - Index, 0);
  S := UInt64(FCount) shl 3;
  FBuffer[16 - 2] := Cardinal(S);
  FBuffer[16 - 1] := Cardinal(S shr 32);
  Transform(@FBuffer);
end;

procedure THash_MD4.HashCore(const Data: TValueArr; Offset, Count: Integer);
var
  Index: Integer;
  Buf: TCardinalArray;
begin
  if (Data = nil) or (Count <= 0) then
    Exit;

  Index := FCount and $3F;
  Inc(FCount, Count);
  if Index > 0 then begin
    if Count < 64 - Index then begin
      Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, Count);
      Exit;
    end;
    Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, 64 - Index);
    Transform(@FBuffer);
    Index := 64 - Index;
    Dec(Count, Index);
  end;

  if Count >= 64 then
    Buf := @Data[Index + Offset]
  else
    Buf := nil;

  Inc(Index, Count and not $3F);
  while Count >= 64 do begin
    Transform(Buf);
    Inc(PAnsiChar(Buf), 64);
    Dec(Count, 64);
  end;
  Move(Data[Index + Offset], FBuffer[0], Count);
end;

function THash_MD4.HashFinal: TBytes;
begin
  Done;
  SetLength(Result, HashSize);
  Move(FDigest[0], Result[0], HashSize);
end;

procedure THash_MD4.Transform(Buffer: TCardinalArray);
var
  A, B, C, D: Cardinal;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 1] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[ 2] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[ 3] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[ 4] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 5] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[ 6] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[ 7] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[ 8] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 9] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[10] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[11] + (C and D or not C and A)); B := B shl 19 or B shr 13;
  Inc(A, Buffer[12] + (B and C or not B and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[13] + (A and B or not A and C)); D := D shl  7 or D shr 25;
  Inc(C, Buffer[14] + (D and A or not D and B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[15] + (C and D or not C and A)); B := B shl 19 or B shr 13;

  Inc(A, Buffer[ 0] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 4] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[ 8] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[12] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 1] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 5] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[ 9] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[13] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 2] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 6] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[10] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[14] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;
  Inc(A, Buffer[ 3] + $5A827999 + (B and C or B and D or C and D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 7] + $5A827999 + (A and B or A and C or B and C)); D := D shl  5 or D shr 27;
  Inc(C, Buffer[11] + $5A827999 + (D and A or D and B or A and B)); C := C shl  9 or C shr 23;
  Inc(B, Buffer[15] + $5A827999 + (C and D or C and A or D and A)); B := B shl 13 or B shr 19;

  Inc(A, Buffer[ 0] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 8] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 4] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[12] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 2] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[10] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 6] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[14] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 1] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[ 9] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 5] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[13] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;
  Inc(A, Buffer[ 3] + $6ED9EBA1 + (B xor C xor D)); A := A shl  3 or A shr 29;
  Inc(D, Buffer[11] + $6ED9EBA1 + (A xor B xor C)); D := D shl  9 or D shr 23;
  Inc(C, Buffer[ 7] + $6ED9EBA1 + (D xor A xor B)); C := C shl 11 or C shr 21;
  Inc(B, Buffer[15] + $6ED9EBA1 + (C xor D xor A)); B := B shl 15 or B shr 17;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;

{ THash_MD5 }

procedure THash_MD5.Transform(Buffer: TCardinalArray);
var
  A, B, C, D: Cardinal;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;

{ THash_SHA }

procedure THash_SHA.Done;
var
  Index: Integer;
  S: Int64;
begin
  Index := FCount and $3F;
  PByteArray(@FBuffer)[Index] := $80;
  Inc(Index);
  if Index > 64 - 8 then begin
    FillChar(PByteArray(@FBuffer)[Index], 64 - Index, 0);
    Transform(@FBuffer);
    Index := 0;
  end;

  FillChar(PByteArray(@FBuffer)[Index], 64 - Index, 0);
  S := UInt64(FCount) shl 3;
  FBuffer[16 - 1] := SwapInteger(Cardinal(S));
  FBuffer[16 - 2] := SwapInteger(Cardinal(S shr 32));
  Transform(@FBuffer);
  // the Endian conversion
  SwapIntegerBuffer(@FDigest, @FDigest, 8);
end;

{ THash_SHA1 }

class function THash_SHA1.GetHashSize: Integer;
begin
  Result := 20;
end;

procedure THash_SHA1.Initialize;
begin
  FRotate := True;
  inherited Initialize;
end;

procedure SHABuffer(const S, D: TCardinalArray; Rotate: Boolean); {$IFDEF UseASM}assembler;{$ENDIF}
{$IFNDEF UseASM}
var
  n: Cardinal;
  i: Integer;
begin
  for i := 0 to 15 do
    S[i] := SwapInteger(D[i]);

  for i := 16 to 79 do begin
    n := S[i - 3] xor S[i - 8] xor S[i - 14] xor S[i - 16];
    if Rotate then
      S[i] := (n shl 1) or (n shr 31)
    else
      S[i] := n;
  end;
{$ELSE}
asm
     PUSH  EBX
     PUSH  ECX
     MOV   EBX,EAX
     XOR   ECX,ECX
@@1: MOV   EAX,[EDX + ECX * 4]
     BSWAP EAX
     MOV   [EBX],EAX
     ADD   EBX,4
     INC   ECX
     CMP   ECX,16
     JNZ   @@1
     MOV   ECX,64
     POP   EDX
     CMP   DL,0
     JZ    @@3
@@2: MOV   EAX,[EBX -  3 * 4]
     XOR   EAX,[EBX -  8 * 4]
     XOR   EAX,[EBX - 14 * 4]
     XOR   EAX,[EBX - 16 * 4]
     ROL   EAX,1
     MOV   [EBX],EAX
     ADD   EBX,4
     DEC   ECX
     JNZ   @@2
     JMP   @@4

@@3: MOV   EAX,[EBX -  3 * 4]
     XOR   EAX,[EBX -  8 * 4]
     XOR   EAX,[EBX - 14 * 4]
     XOR   EAX,[EBX - 16 * 4]
     MOV   [EBX],EAX
     ADD   EBX,4
     DEC   ECX
     JNZ   @@3

@@4: POP   EBX
{$ENDIF}
end;

procedure THash_SHA1.Transform(Buffer: TCardinalArray);
var
  A, B, C, D, E: Cardinal;
  W: array[0..79] of Cardinal;
begin
  SHABuffer(@W, Buffer, FRotate);

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 0] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 1] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 2] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 3] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 4] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 5] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 6] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 7] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 8] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 9] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[10] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[11] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[12] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[13] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[14] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[15] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[16] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[17] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[18] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[19] + $5A827999); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[20] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[21] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[22] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[23] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[24] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[25] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[26] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[27] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[28] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[29] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[30] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[31] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[32] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[33] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[34] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[35] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[36] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[37] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[38] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[39] + $6ED9EBA1); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[40] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[41] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[42] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[43] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[44] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[45] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[46] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[47] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[48] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[49] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[50] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[51] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[52] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[53] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[54] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[55] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[56] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[57] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[58] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[59] + $8F1BBCDC); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[60] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[61] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[62] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[63] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[64] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[65] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[66] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[67] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[68] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[69] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[70] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[71] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[72] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[73] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[74] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[75] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[76] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[77] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[78] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[79] + $CA62C1D6); C := C shr 2 or C shl 30;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
end;

{ THash_SHA2_256 }

class function THash_SHA2_256.GetHashSize: Integer;
begin
  Result := 32;
end;

procedure THash_SHA2_256.Initialize;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FCount := 0;

  FDigest[0] := $6A09E667;
  FDigest[1] := $BB67AE85;
  FDigest[2] := $3C6EF372;
  FDigest[3] := $A54FF53A;
  FDigest[4] := $510E527F;
  FDigest[5] := $9B05688C;
  FDigest[6] := $1F83D9AB;
  FDigest[7] := $5BE0CD19;
end;

procedure THash_SHA2_256.Transform(Buffer: TCardinalArray);
const
  K: array[0..63] of Cardinal = (
    $428A2F98, $71374491, $B5C0FBCF, $E9B5DBA5, $3956C25B, $59F111F1, $923F82A4, $AB1C5ED5,
    $D807AA98, $12835B01, $243185BE, $550C7DC3, $72BE5D74, $80DEB1FE, $9BDC06A7, $C19BF174,
    $E49B69C1, $EFBE4786, $0FC19DC6, $240CA1CC, $2DE92C6F, $4A7484AA, $5CB0A9DC, $76F988DA,
    $983E5152, $A831C66D, $B00327C8, $BF597FC7, $C6E00BF3, $D5A79147, $06CA6351, $14292967,
    $27B70A85, $2E1B2138, $4D2C6DFC, $53380D13, $650A7354, $766A0ABB, $81C2C92E, $92722C85,
    $A2BFE8A1, $A81A664B, $C24B8B70, $C76C51A3, $D192E819, $D6990624, $F40E3585, $106AA070,
    $19A4C116, $1E376C08, $2748774C, $34B0BCB5, $391C0CB3, $4ED8AA4A, $5B9CCA4F, $682E6FF3,
    $748F82EE, $78A5636F, $84C87814, $8CC70208, $90BEFFFA, $A4506CEB, $BEF9A3F7, $C67178F2
  );

var
  A, B, C, D, E, F, G, H: Cardinal;
  W: array[0..63] of Cardinal;
  S0, S1: Cardinal;
  T1, T2: Cardinal;
  i: Integer;
begin
  for i := 0 to 15 do
    W[i] := SwapInteger(Buffer[i]);

  for i := 16 to 63 do begin
    S0 := W[i - 15];
    S0 := (S0 shr 7 or S0 shl 25) xor (S0 shr 18 or S0 shl 14) xor (S0 shr 3);
    S1 := W[i - 2];
    S1 := (S1 shr 17 or S1 shl 15) xor (S1 shr 19 or S1 shl 13) xor (S1 shr 10);
    W[i] := W[i - 16] + S0 + W[i - 7] + S1;
  end;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for i := 0 to 63 do begin
    S1 := (E shr 6 or E shl 26) xor (E shr 11 or E shl 21) xor (E shr 25 or E shl 7);
    T1 := H + S1 + ((E and F) xor ((not E) and G)) + K[i] + W[i];
    S0 := (A shr 2 or A shl 30) xor (A shr 13 or A shl 19) xor (A shr 22 or A shl 10);
    T2 := S0 + ((A and B) xor (A and C) xor (B and C));

    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

{ THash_SHA2_224 }

class function THash_SHA2_224.GetHashSize: Integer;
begin
  Result := 28;
end;

procedure THash_SHA2_224.Initialize;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FCount := 0;

  FDigest[0] := $C1059ED8;
  FDigest[1] := $367CD507;
  FDigest[2] := $3070DD17;
  FDigest[3] := $F70E5939;
  FDigest[4] := $FFC00B31;
  FDigest[5] := $68581511;
  FDigest[6] := $64F98FA7;
  FDigest[7] := $BEFA4FA4;
end;

{ THash_SHA2_512 }

constructor THash_SHA2_512.Create;
begin
  inherited;

  Initialize;
end;

class function THash_SHA2_512.GetHashSize: Integer;
begin
  Result := 64;
end;

procedure THash_SHA2_512.Initialize;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FCount := 0;

  FDigest[0] := UInt64($6a09e667f3bcc908);
  FDigest[1] := UInt64($bb67ae8584caa73b);
  FDigest[2] := UInt64($3c6ef372fe94f82b);
  FDigest[3] := UInt64($a54ff53a5f1d36f1);
  FDigest[4] := UInt64($510e527fade682d1);
  FDigest[5] := UInt64($9b05688c2b3e6c1f);
  FDigest[6] := UInt64($1f83d9abfb41bd6b);
  FDigest[7] := UInt64($5be0cd19137e2179);
end;

procedure THash_SHA2_512.Done;
var
  Index: Integer;
  S: UInt64;
begin
  Index := FCount and $7F;
  PByteArray(@FBuffer)[Index] := $80;
  Inc(Index);
  if Index > 128 - 16 then begin
    FillChar(PByteArray(@FBuffer)[Index], 128 - Index, 0);
    Transform(@FBuffer);
    Index := 0;
  end;

  FillChar(PByteArray(@FBuffer)[Index], 128 - Index, 0);
  S := UInt64(FCount) shl 3;
  FBuffer[16 - 1] := SwapInt64(S);
  FBuffer[16 - 2] := 0;
  Transform(@FBuffer);
  // the Endian conversion
  SwapInt64Buffer(@FDigest, @FDigest, 8);
end;

procedure THash_SHA2_512.HashCore(const Data: TValueArr; Offset, Count: Integer);
var
  Index: Integer;
  Buf: TUInt64Array;
begin
  if (Data = nil) or (Count <= 0) then
    Exit;

  Index := FCount and $7F;
  Inc(FCount, Count);
  if Index > 0 then begin
    if Count < 128 - Index then begin
      Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, Count);
      Exit;
    end;
    Move(Data[Offset], PtrOffset(@FBuffer[0], Index)^, 128 - Index);
    Transform(@FBuffer);
    Index := 128 - Index;
    Dec(Count, Index);
  end;

  if Count >= 128 then
    Buf := @Data[Index + Offset]
  else
    Buf := nil;

  Inc(Index, Count and not $7F);
  while Count >= 128 do begin
    Transform(Buf);
    Inc(PAnsiChar(Buf), 128);
    Dec(Count, 128);
  end;
  Move(Data[Index + Offset], FBuffer[0], Count);
end;

function THash_SHA2_512.HashFinal: TBytes;
begin
  Done;
  SetLength(Result, HashSize);
  Move(FDigest[0], Result[0], HashSize);
end;

procedure THash_SHA2_512.Transform(Buffer: TUInt64Array);
const
  K: array[0..79] of UInt64 = (
    UInt64($428a2f98d728ae22), UInt64($7137449123ef65cd), UInt64($b5c0fbcfec4d3b2f), UInt64($e9b5dba58189dbbc),
    UInt64($3956c25bf348b538), UInt64($59f111f1b605d019), UInt64($923f82a4af194f9b), UInt64($ab1c5ed5da6d8118),
    UInt64($d807aa98a3030242), UInt64($12835b0145706fbe), UInt64($243185be4ee4b28c), UInt64($550c7dc3d5ffb4e2),
    UInt64($72be5d74f27b896f), UInt64($80deb1fe3b1696b1), UInt64($9bdc06a725c71235), UInt64($c19bf174cf692694),
    UInt64($e49b69c19ef14ad2), UInt64($efbe4786384f25e3), UInt64($0fc19dc68b8cd5b5), UInt64($240ca1cc77ac9c65),
    UInt64($2de92c6f592b0275), UInt64($4a7484aa6ea6e483), UInt64($5cb0a9dcbd41fbd4), UInt64($76f988da831153b5),
    UInt64($983e5152ee66dfab), UInt64($a831c66d2db43210), UInt64($b00327c898fb213f), UInt64($bf597fc7beef0ee4),
    UInt64($c6e00bf33da88fc2), UInt64($d5a79147930aa725), UInt64($06ca6351e003826f), UInt64($142929670a0e6e70),
    UInt64($27b70a8546d22ffc), UInt64($2e1b21385c26c926), UInt64($4d2c6dfc5ac42aed), UInt64($53380d139d95b3df),
    UInt64($650a73548baf63de), UInt64($766a0abb3c77b2a8), UInt64($81c2c92e47edaee6), UInt64($92722c851482353b),
    UInt64($a2bfe8a14cf10364), UInt64($a81a664bbc423001), UInt64($c24b8b70d0f89791), UInt64($c76c51a30654be30),
    UInt64($d192e819d6ef5218), UInt64($d69906245565a910), UInt64($f40e35855771202a), UInt64($106aa07032bbd1b8),
    UInt64($19a4c116b8d2d0c8), UInt64($1e376c085141ab53), UInt64($2748774cdf8eeb99), UInt64($34b0bcb5e19b48a8),
    UInt64($391c0cb3c5c95a63), UInt64($4ed8aa4ae3418acb), UInt64($5b9cca4f7763e373), UInt64($682e6ff3d6b2b8a3),
    UInt64($748f82ee5defb2fc), UInt64($78a5636f43172f60), UInt64($84c87814a1f0ab72), UInt64($8cc702081a6439ec),
    UInt64($90befffa23631e28), UInt64($a4506cebde82bde9), UInt64($bef9a3f7b2c67915), UInt64($c67178f2e372532b),
    UInt64($ca273eceea26619c), UInt64($d186b8c721c0c207), UInt64($eada7dd6cde0eb1e), UInt64($f57d4f7fee6ed178),
    UInt64($06f067aa72176fba), UInt64($0a637dc5a2c898a6), UInt64($113f9804bef90dae), UInt64($1b710b35131c471b),
    UInt64($28db77f523047d84), UInt64($32caab7b40c72493), UInt64($3c9ebe0a15c9bebc), UInt64($431d67c49c100d4c),
    UInt64($4cc5d4becb3e42b6), UInt64($597f299cfc657e2a), UInt64($5fcb6fab3ad6faec), UInt64($6c44198c4a475817)
  );

var
  A, B, C, D, E, F, G, H: UInt64;
  W: array[0..79] of UInt64;
  S0, S1: UInt64;
  T1, T2: UInt64;
  i: Integer;
begin
  for i := 0 to 15 do
    W[i] := SwapInt64(Buffer[i]);

  for i := 16 to 79 do begin
    S0 := W[i - 15];
    S0 := (S0 shr 1 or S0 shl 63) xor (S0 shr 8 or S0 shl 56) xor (S0 shr 7);
    S1 := W[i - 2];
    S1 := (S1 shr 19 or S1 shl 45) xor (S1 shr 61 or S1 shl 3) xor (S1 shr 6);
    W[i] := W[i - 16] + S0 + W[i - 7] + S1;
  end;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for i := 0 to 79 do begin
    S1 := (E shr 14 or E shl 50) xor (E shr 18 or E shl 46) xor (E shr 41 or E shl 23);
    T1 := H + S1 + ((E and F) xor ((not E) and G)) + K[i] + W[i];
    S0 := (A shr 28 or A shl 36) xor (A shr 34 or A shl 30) xor (A shr 39 or A shl 25);
    T2 := S0 + ((A and B) xor (A and C) xor (B and C));

    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

{ THash_SHA2_384 }

class function THash_SHA2_384.GetHashSize: Integer;
begin
  Result := 48;
end;

procedure THash_SHA2_384.Initialize;
begin
  FillChar(FBuffer, SizeOf(FBuffer), 0);
  FCount := 0;

  FDigest[0] := UInt64($cbbb9d5dc1059ed8);
  FDigest[1] := UInt64($629a292a367cd507);
  FDigest[2] := UInt64($9159015a3070dd17);
  FDigest[3] := UInt64($152fecd8f70e5939);
  FDigest[4] := UInt64($67332667ffc00b31);
  FDigest[5] := UInt64($8eb44a8768581511);
  FDigest[6] := UInt64($db0c2e0d64f98fa7);
  FDigest[7] := UInt64($47b5481dbefa4fa4);
end;

end.
