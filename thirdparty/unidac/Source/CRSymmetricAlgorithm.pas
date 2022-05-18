
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  Include a Selection of various cipher's (Encryption Algo)
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRSymmetricAlgorithm;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

uses
  SysUtils,
  CLRClasses, CRTypes, CRDECUtil, CRCryptoTransformIntf;

type
  TKeySizes = class
  public
    MaxSize: Integer;
    MinSize: Integer;
    SkipSize: Integer;
  end;

  TCipherMode = (cmECB, cmCBC, cmCTR, cmGCM);
//  TCipherMode = (cmCTS, cmCBC, cmCFB, cmOFB, cmECB, cmCTSMAC, cmCBCMAC, cmCFBMAC);
{ the Cipher Modes:
  cmCTS     Cipher Text Stealing, a Variant from cmCBC, but relaxes
            the restriction that the DataSize must be a mulitply from BlockSize,
            this is the Defaultmode, fast and Bytewise
  cmCBC     Cipher Block Chaining
  cmCFB     K-bit Cipher Feedback, here is K = 8 -> 1 Byte
  cmOFB     K-bit Output Feedback, here is K = 8 -> 1 Byte
  cmECB *   Electronic Codebook, DataSize must be a multiply from BlockSize
  cmCTSMAC  Build a Message Authentication Code in cmCTS Mode
  cmCBCMAC  Build a CBC-MAC
  cmCFBMAC  Build a CFB-MAC
}

  TPaddingMode = (pmNone, pmPKCS7, pmZeros);

  TTransformBlockMethod = procedure(Source: TValueArr; SrcOffset: Integer; DataSize: Integer; Dest: TValueArr; DstOffset: Integer) of object;

  TSymmetricAlgorithmClass = class of TSymmetricAlgorithm;

  TSymmetricAlgorithm = class(TInterfacedObject, ICryptoTransform)
  private
    FTransformBlockMethod: TTransformBlockMethod;
    FMode: TCipherMode;
    FPadding: TPaddingMode;
    FLegalKeySizes: TKeySizes;

    FIntSize: Integer;
    FBuffer: PCardinal;
    FFeedback: PCardinal;
    FVector: PCardinal;
    FTag: PCardinal;
    FGHASH: PCardinal;
    FAAD: TBytes; // Additional authenticated data
    FReceivedTag: TBytes;
    FInitialized: Boolean;

    FGcmPreCalc: array[0..15, 0..3] of cardinal;
    procedure ProcessGcmAAD;
    procedure ProcessGcmLengths(DataSize: Integer);
    function GetTag: TBytes;

  protected
    FUser: TCardinalArray;
    FUserSize: Integer;
    FKeySize: Integer;
    FBlockSize: Integer;
    procedure SetMode(const Value: TCipherMode);
    procedure SetKey(const Value: TBytes);
    procedure Set_IV(const Value: TBytes);

    procedure IncVector; {$IFDEF USE_INLINE}inline;{$ENDIF}

    procedure GcmMulH(Value: TValueArr);
    procedure InitGCM;
    procedure Done; virtual;
    procedure InitBegin(KeySize: Integer);
    procedure InitEnd(const IVector: PCardinal);
    {must override}
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); virtual;

    {the en(de)code function, must override}
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); virtual;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); virtual;

    procedure InternalEncodeBuffer(Data: PCardinal; DataSize: Integer);
    procedure InternalDecodeBuffer(Data: PCardinal; DataSize: Integer);

    procedure Init(const Key: TBytes; const IVector: PCardinal); virtual;
    procedure Protect; virtual;

    function Get_OutputBlockSize: Integer; virtual;

    {the individual Userdata and Buffer}
    property User: TCardinalArray read FUser;
    property UserSize: Integer read FUserSize;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateEncryptor: ICryptoTransform;
    function CreateDecryptor: ICryptoTransform;

    procedure CodeBuffer(const Buffer: TValueArr; BufferSize: Integer);

    procedure GenerateIV; virtual;
    procedure GenerateKey; virtual;
    procedure ClearIV;
    procedure SetIV(Value: TValueArr; Offset, Count: integer);
    procedure SetReceivedTag(Value: TValueArr; Offset, Count: integer);

    procedure EncodeBuffer(Source, Dest: IntPtr; DataSize: Integer); overload;
    procedure EncodeBuffer(Source: TValueArr; SrcOffset: Integer; DataSize: Integer; Dest: TValueArr; DstOffset: Integer); overload;
    procedure DecodeBuffer(Source, Dest: IntPtr; DataSize: Integer); overload;
    procedure DecodeBuffer(Source: TValueArr; SrcOffset: Integer; DataSize: Integer; Dest: TValueArr; DstOffset: Integer); overload;
    function ValidKeySize(BitLength: Integer): Boolean;

    procedure TransformBlock(const Data: TBytes; Offset, Count: Integer); overload;
    function TransformBlock(const InputBuffer: TValueArr; InputOffset, InputCount: Integer; const OutputBuffer: TValueArr; OutputOffset: Integer): Integer; overload;
    procedure TransformFinalBlock(const InputBuffer: TBytes; InputOffset, InputCount: Integer);

    property Initialized: Boolean read FInitialized;
    property Mode: TCipherMode read FMode write SetMode;
    property Padding: TPaddingMode read FPadding write FPadding;
    property KeySize: Integer read FKeySize;
    property LegalKeySizes: TKeySizes read FLegalKeySizes;
    property BlockSize: Integer read FBlockSize;
    property Key: TBytes write SetKey;
    property IV: TBytes write Set_IV;
    property AAD: TBytes read FAAD write FAAD;
    property Tag: TBytes read GetTag;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  DAConsts, CRFunctions;

const
  GCM_Shift: array[0..15] of cardinal = (
    $00000000, $1C200000, $38400000, $24600000, $70800000, $6CA00000, $48C00000, $54E00000,
    $E1000000, $FD200000, $D9400000, $C5600000, $91800000, $8DA00000, $A9C00000, $B5E00000
  );

constructor TSymmetricAlgorithm.Create;
begin
  inherited Create;

  FLegalKeySizes := TKeySizes.Create;
  GetContext(FBlockSize, FUserSize, FLegalKeySizes);
  FKeySize := FLegalKeySizes.MaxSize div 8; // bits to bytes
  FIntSize := FBlockSize div SizeOf(Cardinal);

  GetMem(FVector, FBlockSize);
  GetMem(FFeedback, FBlockSize);
  GetMem(FBuffer, FBlockSize);
  GetMem(FTag, FBlockSize);
  GetMem(FGHASH, FBlockSize);
  GetMem(FUser, FUserSize);

  FMode := cmCBC;
  FPadding := pmNone;
end;

destructor TSymmetricAlgorithm.Destroy;
begin
  Protect;

  FreeMem(FVector);
  FreeMem(FFeedback);
  FreeMem(FBuffer);
  FreeMem(FTag);
  FreeMem(FGHASH);
  FreeMem(FUser);
  FLegalKeySizes.Free;

  inherited;
end;

procedure TSymmetricAlgorithm.InitBegin(KeySize: Integer);
begin
  FInitialized := False;
  Protect;
  if not ValidKeySize(KeySize * 8) then
    raise Exception.CreateFmt(SInvalidKeySize, [ClassName, FLegalKeySizes.MinSize, FLegalKeySizes.MaxSize]);
end;

procedure TSymmetricAlgorithm.InitEnd(const IVector: PCardinal);
begin
  if IVector = nil then
    Encode(FVector, 0)
  else
  if IVector <> FVector then
    Move(IVector^, FVector^, FBlockSize);

  Move(FVector^, FFeedback^, FBlockSize);
  FInitialized := True;

  if FMode = cmGCM then
    InitGCM;
end;

class procedure TSymmetricAlgorithm.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 0;
  KeySizes.MinSize := 0;
  KeySizes.MaxSize := 0;
  KeySizes.SkipSize := 0;
  UserSize := 0;
end;

procedure TSymmetricAlgorithm.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
end;

procedure TSymmetricAlgorithm.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
end;

function TSymmetricAlgorithm.ValidKeySize(BitLength: Integer): Boolean;
var
  Size: Integer;
begin
  Result := False;
  Size := LegalKeySizes.MinSize;
  repeat
    if BitLength < Size then
      Exit;
    if BitLength = Size then begin
      Result := True;
      Exit;
    end;
    Size := Size + LegalKeySizes.SkipSize;
  until Size >= LegalKeySizes.MaxSize + LegalKeySizes.SkipSize;
end;

procedure TSymmetricAlgorithm.GenerateIV;
var
  tmp: TBytes;
  i: Integer;
begin
  Randomize;
  SetLength(tmp, BlockSize);
  for i := 0 to BlockSize - 1 do
    tmp[i] := Random(256);

  Set_IV(tmp);
end;

procedure TSymmetricAlgorithm.GenerateKey;
var
  tmp: TBytes;
  i: Integer;
begin
  Randomize;
  SetLength(tmp, KeySize);
  for i := 0 to KeySize - 1 do
    tmp[i] := Random(256);

  SetKey(tmp);
end;

procedure TSymmetricAlgorithm.SetMode(const Value: TCipherMode);
begin
  if Value <> FMode then begin
    if Value = cmGCM then begin
      //GCM supports only symmetric block ciphers whose block size is 128 bits
      if BlockSize <> 16 then
        raise Exception.Create(SGCMCannotBeUsed);

      if FInitialized then
        InitGCM;
    end;

    FMode := Value;
  end;
end;

procedure TSymmetricAlgorithm.SetKey(const Value: TBytes);
begin
  FKeySize := Length(Value);
  Init(Value, FVector);
end;

procedure TSymmetricAlgorithm.Set_IV(const Value: TBytes);
begin
  SetIV(TValueArr(Value), 0, Length(Value));
end;

procedure TSymmetricAlgorithm.SetIV(Value: TValueArr; Offset, Count: integer);
var
  n: integer;
begin
  if (Get_OutputBlockSize = 1) and (Count = 0) then  // is stream cipher
    Exit;

  if (Count = 0) or (Count > BlockSize) then
    raise Exception.Create(SInvalidIV);

  if FMode = cmGCM then begin
    // Check whether the length of the IV is 96 bits
    if Count = 12 then begin
      // When the length of the IV is 96 bits, the padding string is
      // appended to the IV to form the pre-counter block
      Move(Value[Offset], FVector^, 12);
      PutIntBE(1, TValueArr(FVector), 12);
    end
    else begin
      // Initialize GHASH calculation
      FillChar(FVector^, 16, 0);

      // The IV processed in a block-by-block fashion
      n := Count;
      while n > 0 do begin
        // GHASH
        if n >= 16 then
          XORBuffers(FVector, 0, PCardinal(Value), 16)
        else
          XORBuffers(FVector, 0, PCardinal(Value), n);

        GcmMulH(TValueArr(FVector));

        Inc(Value, 16);
        Dec(n, 16);
      end;

      // The string is appended with 64 additional 0 bits, followed by the
      // 64-bit representation of the length of the IV
      FillChar(FBuffer^, 16, 0);
      PutIntBE(Count * 8, TValueArr(FBuffer), 12);
      // GHASH to the resulting string to form the pre-counter block
      XORBuffers(FVector, 0, FBuffer, 16);
      GcmMulH(TValueArr(FVector));
    end;
  end
  else begin
    Move(Value[Offset], FVector^, Count);
    Move(FVector^, FFeedback^, FBlockSize);
  end;
end;

function TSymmetricAlgorithm.GetTag: TBytes;
begin
  if FMode <> cmGCM then
    raise Exception.Create(SGCMModeMustBeUsed);

  SetLength(Result, FBlockSize);
  Move(FTag^, Result[0], FBlockSize);
end;

procedure TSymmetricAlgorithm.SetReceivedTag(Value: TValueArr; Offset, Count: integer);
begin
  SetLength(FReceivedTag, Count);
  if Count > 0 then
    Move(Value[Offset], FReceivedTag[0], Count);
end;

procedure TSymmetricAlgorithm.InitGCM;
var
  i, j, c: cardinal;
  h: array[0..3] of cardinal;
begin
  if not Initialized then
    raise Exception.CreateFmt(SNotInitialized, [ClassName]);

  //Let H = 0
  h[0] := 0;
  h[1] := 0;
  h[2] := 0;
  h[3] := 0;

  //Generate the hash subkey H
  Encode(PCardinal(@h[0]), 0);

  //Pre-compute M(0) = H * 0
  j := 0;
  FGcmPreCalc[j][0] := 0;
  FGcmPreCalc[j][1] := 0;
  FGcmPreCalc[j][2] := 0;
  FGcmPreCalc[j][3] := 0;

  //Pre-compute M(1) = H * 1
  j := ReverseInt4(1);
  FGcmPreCalc[j][0] := SwapInteger(h[3]);
  FGcmPreCalc[j][1] := SwapInteger(h[2]);
  FGcmPreCalc[j][2] := SwapInteger(h[1]);
  FGcmPreCalc[j][3] := SwapInteger(h[0]);

  //Pre-compute all 4-bit multiples of H
  for i := 2 to 15 do begin
    if (i and 1) = 1 then begin
      //Compute M(i) = M(i - 1) + H
      j := ReverseInt4(i - 1);
      h[0] := FGcmPreCalc[j][0];
      h[1] := FGcmPreCalc[j][1];
      h[2] := FGcmPreCalc[j][2];
      h[3] := FGcmPreCalc[j][3];

      //An addition in GF(2^128) is identical to a bitwise exclusive-OR operation
      j := ReverseInt4(1);
      h[0] := h[0] xor FGcmPreCalc[j][0];
      h[1] := h[1] xor FGcmPreCalc[j][1];
      h[2] := h[2] xor FGcmPreCalc[j][2];
      h[3] := h[3] xor FGcmPreCalc[j][3];
    end
    else begin
      //Compute M(i) = M(i / 2) * x
      j := ReverseInt4(i shr 1);
      h[0] := FGcmPreCalc[j][0];
      h[1] := FGcmPreCalc[j][1];
      h[2] := FGcmPreCalc[j][2];
      h[3] := FGcmPreCalc[j][3];

      //The multiplication of a polynomial by x in GF(2^128) corresponds to a shift of indices
      c := h[0] and 1;
      h[0] := (h[0] shr 1) or (h[1] shl 31);
      h[1] := (h[1] shr 1) or (h[2] shl 31);
      h[2] := (h[2] shr 1) or (h[3] shl 31);
      h[3] := h[3] shr 1;
      //If the highest term of the result is equal to one, then perform reduction
      if c <> 0 then
        h[3] := h[3] xor GCM_Shift[ReverseInt4(1)];
    end;

    //Save M(i)
    j := ReverseInt4(i);
    FGcmPreCalc[j][0] := h[0];
    FGcmPreCalc[j][1] := h[1];
    FGcmPreCalc[j][2] := h[2];
    FGcmPreCalc[j][3] := h[3];
  end;
end;

procedure TSymmetricAlgorithm.GcmMulH(Value: TValueArr);
var
  i: integer;
  b, c: byte;
  z: array[0..3] of cardinal;
begin
  //Let Z = 0
  z[0] := 0;
  z[1] := 0;
  z[2] := 0;
  z[3] := 0;

  //Fast table-driven implementation
  for i := 15 downto 0 do begin
    //Process the lower nibble
    b := PByteArray(Value)[i] and $0F;
    c := z[0] and $0F;
    z[0] := (z[0] shr 4) or (z[1] shl 28);
    z[1] := (z[1] shr 4) or (z[2] shl 28);
    z[2] := (z[2] shr 4) or (z[3] shl 28);
    z[3] := (z[3] shr 4) xor GCM_Shift[c];

    z[0] := z[0] xor FGcmPreCalc[b][0];
    z[1] := z[1] xor FGcmPreCalc[b][1];
    z[2] := z[2] xor FGcmPreCalc[b][2];
    z[3] := z[3] xor FGcmPreCalc[b][3];

    //Process the upper nibble
    b := (PByteArray(Value)[i] shr 4) and $0F;
    c := z[0] and $0F;
    z[0] := (z[0] shr 4) or (z[1] shl 28);
    z[1] := (z[1] shr 4) or (z[2] shl 28);
    z[2] := (z[2] shr 4) or (z[3] shl 28);
    z[3] := (z[3] shr 4) xor GCM_Shift[c];

    z[0] := z[0] xor FGcmPreCalc[b][0];
    z[1] := z[1] xor FGcmPreCalc[b][1];
    z[2] := z[2] xor FGcmPreCalc[b][2];
    z[3] := z[3] xor FGcmPreCalc[b][3];
  end;

  //Save the result
  PutIntBE(z[3], Value, 0);
  PutIntBE(z[2], Value, 4);
  PutIntBE(z[1], Value, 8);
  PutIntBE(z[0], Value, 12);
end;

procedure TSymmetricAlgorithm.Init(const Key: TBytes; const IVector: PCardinal);
begin
end;

procedure TSymmetricAlgorithm.Done;
begin
  if MemCompare(PByteArray(FVector), PByteArray(FFeedback), FBlockSize) = 0 then
    Exit;

  Move(FFeedback^, FBuffer^, FBlockSize);
  Move(FVector^, FFeedback^, FBlockSize);
end;

procedure TSymmetricAlgorithm.Protect;
begin
  FInitialized := False;

  FillChar(FVector^, FBlockSize, $00);
  FillChar(FFeedback^, FBlockSize, $00);
  FillChar(FBuffer^, FBlockSize, $00);
  FillChar(FTag^, FBlockSize, $00);
  FillChar(FGHASH^, FBlockSize, $00);
  FillChar(FUser^, FUserSize, $00);
end;

procedure TSymmetricAlgorithm.ClearIV;
begin
  FillChar(FVector^, FBlockSize, $00);
  FillChar(FFeedback^, FBlockSize, $00);
  FillChar(FTag^, FBlockSize, $00);
  FillChar(FGHASH^, FBlockSize, $00);
end;

procedure TSymmetricAlgorithm.CodeBuffer(const Buffer: TValueArr; BufferSize: Integer);
begin
  EncodeBuffer(Buffer, Buffer, BufferSize);
  Done;
end;

procedure TSymmetricAlgorithm.IncVector;
var
  i: integer;
begin
  for i := FBlockSize - 1 downto 1 do begin
    Inc(PByteArray(FVector)[i]);
    if PByteArray(FVector)[i] <> 0 then
      break;
  end;
end;

procedure TSymmetricAlgorithm.ProcessGcmAAD;
var
  PAAD: PCardinal;
  n: Integer;
begin
  // Compute MSB(CIPH(J(0)))
  Move(FVector^, FTag^, 16);
  Encode(FTag, 0);

  // Initialize GHASH calculation
  FillChar(FGHASH^, 16, 0);
  n := Length(FAAD);
  PAAD := @FAAD[0];

  // Process AAD - Additional authenticated data are processed in a block-by-block fashion
  while n >= 16 do begin
    // GHASH
    XORBuffers(FGHASH, 0, PAAD, 16);
    GcmMulH(TValueArr(FGHASH));

    Inc(PAAD, FIntSize);
    Dec(n, 16);
  end;

  if n > 0 then begin
    // GHASH
    XORBuffers(FGHASH, 0, PAAD, n);
    GcmMulH(TValueArr(FGHASH));
  end;
end;

procedure TSymmetricAlgorithm.ProcessGcmLengths(DataSize: Integer);
begin
  // Append the 64-bit representation of the length of the AAD and the ciphertext
  FillChar(FBuffer^, 16, 0);
  PutIntBE(Length(FAAD) * 8, TValueArr(FBuffer), 4);
  PutIntBE(DataSize * 8, TValueArr(FBuffer), 12);

  // GHASH to the result to produce a single output block S
  XORBuffers(FGHASH, 0, FBuffer, 16);
  GcmMulH(TValueArr(FGHASH));

  // Let R = MSB(GCTR(J(0), S)
  XORBuffers(FTag, 0, FGHASH, 16);
end;

procedure TSymmetricAlgorithm.EncodeBuffer(Source, Dest: IntPtr; DataSize: Integer);
var
  D: PCardinal;
begin
  D := Dest;
  if Source <> Dest then
    Move(Source^, Dest^, DataSize);

  InternalEncodeBuffer(D, DataSize);
end;

procedure TSymmetricAlgorithm.EncodeBuffer(Source: TValueArr; SrcOffset: Integer; DataSize: Integer;
  Dest: TValueArr; DstOffset: Integer);
var
  S, D: PCardinal;
begin
  S := PtrOffset(Source, SrcOffset);
  D := PtrOffset(Dest, DstOffset);
  if S <> D then
    Move(Source[SrcOffset], Dest[DstOffset], DataSize);

  InternalEncodeBuffer(D, DataSize);
end;

procedure TSymmetricAlgorithm.InternalEncodeBuffer(Data: PCardinal; DataSize: Integer);
var
  F: PCardinal;
  n, Pos: Integer;
begin
  if not Initialized then
    raise Exception.CreateFmt(SNotInitialized, [ClassName]);

  Pos := 0;
  case FMode of
    cmECB: begin
      while DataSize >= FBlockSize do begin
        Encode(Data, Pos);
        Inc(Pos, FIntSize);
        Dec(DataSize, FBlockSize);
      end;
      if DataSize > 0 then begin
        Move(TCardinalArray(Data)[Pos], FBuffer^, DataSize);
        Encode(FBuffer, 0, DataSize);
        Move(FBuffer^, TCardinalArray(Data)[Pos], DataSize);
      end;
    end;

    cmCBC: begin
      F := FFeedback;
      while DataSize >= FBlockSize do begin
        XORBuffers(Data, Pos, F, FBlockSize);
        Encode(Data, Pos);
        F := @TCardinalArray(Data)[Pos];

        Inc(Pos, FIntSize);
        Dec(DataSize, FBlockSize);
      end;

      if F <> FFeedback then
        Move(F^, FFeedback^, FBlockSize);

      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer, 0, DataSize);
        XORBuffers(Data, Pos, FBuffer, DataSize);
        XORBuffers(FFeedback, 0, FBuffer, FBlockSize);
      end;
    end;

    cmCTR: begin
      while DataSize >= FBlockSize do begin
        Move(FVector^, FBuffer^, FBlockSize);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, FBlockSize);
        IncVector;

        Inc(Pos, FIntSize);
        Dec(DataSize, FBlockSize);
      end;

      if DataSize > 0 then begin
        Move(FVector^, FBuffer^, FBlockSize);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, DataSize);
        IncVector;
      end;
    end;

    cmGCM: begin
      ProcessGcmAAD;

      n := DataSize;
      while n >= 16 do begin
        IncVector;
        Move(FVector^, FBuffer^, 16);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, 16);

        // GHASH
        XORBuffers(FGHASH, 0, @TCardinalArray(Data)[Pos], 16);
        GcmMulH(TValueArr(FGHASH));

        Inc(Pos, FIntSize);
        Dec(n, 16);
      end;

      if n > 0 then begin
        IncVector;
        Move(FVector^, FBuffer^, 16);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, n);

        // GHASH
        XORBuffers(FGHASH, 0, @TCardinalArray(Data)[Pos], n);
        GcmMulH(TValueArr(FGHASH));
      end;

      ProcessGcmLengths(DataSize);
    end;
{
    cmCTS: begin
      while DataSize >= FBlockSize do begin
        XORBuffers(S, FFeedback, FBlockSize, D);
        Encode(D);
        XORBuffers(D, FFeedback, FBlockSize, FFeedback);
        Inc(S, FBlockSize);
        Inc(D, FBlockSize);
        Dec(DataSize, FBlockSize);
      end;
      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        XORBuffers(S, FBuffer, DataSize, D);
        XORBuffers(FBuffer, FFeedback, FBlockSize, FFeedback);
      end;
    end;
    cmCFB: begin
      while DataSize > 0 do begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBlockSize-1);
        PByteArray(FFeedback)[FBlockSize-1] := D^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    end;
    cmOFB: begin
      while DataSize > 0 do begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBlockSize-1);
        PByteArray(FFeedback)[FBlockSize-1] := PByte(FBuffer)^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    end;
    cmCTSMAC: begin
      while DataSize >= FBlockSize do begin
        XORBuffers(S, FFeedback, FBlockSize, FBuffer);
        Encode(FBuffer);
        XORBuffers(FBuffer, FFeedback, FBlockSize, FFeedback);
        Inc(S, FBlockSize);
        Dec(DataSize, FBlockSize);
      end;
      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        XORBuffers(FBuffer, FFeedback, FBlockSize, FFeedback);
      end;
    end;
    cmCBCMAC: begin
      while DataSize >= FBlockSize do begin
        XORBuffers(S, FFeedback, FBlockSize, FBuffer);
        Encode(FBuffer);
        Move(FBuffer^, FFeedback^, FBlockSize);
        Inc(S, FBlockSize);
        Dec(DataSize, FBlockSize);
      end;
      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        XORBuffers(FBuffer, FFeedback, FBlockSize, FFeedback);
      end;
    end;
    cmCFBMAC: begin
      while DataSize > 0 do begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        Move(PByteArray(FFeedback)[1], FFeedback^, FBlockSize-1);
        PByteArray(FFeedback)[FBlockSize-1] := S^ xor PByte(FBuffer)^;
        Inc(S);
        Dec(DataSize);
      end;
    end;
}
  end;
end;

procedure TSymmetricAlgorithm.DecodeBuffer(Source, Dest: IntPtr; DataSize: Integer);
var
  D: PCardinal;
begin
  D := Dest;
  if Source <> Dest then
    Move(Source^, Dest^, DataSize);

  InternalDecodeBuffer(D, DataSize);
end;

procedure TSymmetricAlgorithm.DecodeBuffer(Source: TValueArr; SrcOffset: Integer; DataSize: Integer;
  Dest: TValueArr; DstOffset: Integer);
var
  S, D: PCardinal;
begin
  S := PtrOffset(Source, SrcOffset);
  D := PtrOffset(Dest, DstOffset);
  if S <> D then
    Move(Source[SrcOffset], Dest[DstOffset], DataSize);

  InternalDecodeBuffer(D, DataSize);
end;

procedure TSymmetricAlgorithm.InternalDecodeBuffer(Data: PCardinal; DataSize: Integer);
var
  S, F, B: PCardinal;
  n, Pos: Integer;
begin
  if not Initialized then
    raise Exception.CreateFmt(SNotInitialized, [ClassName]);

  Pos := 0;
  case FMode of
    cmECB: begin
      while DataSize >= FBlockSize do begin
        Decode(Data, Pos);
        Inc(Pos, FIntSize);
        Dec(DataSize, FBlockSize);
      end;
      if DataSize > 0 then begin
        Move(TCardinalArray(Data)[Pos], FBuffer^, DataSize);
        Encode(FBuffer, 0, DataSize);
        Move(FBuffer^, TCardinalArray(Data)[Pos], DataSize);
      end;
    end;

    cmCBC: begin
      F := FFeedback;
      B := FBuffer;
      while DataSize >= FBlockSize do begin
        Move(TCardinalArray(Data)[Pos], B^, FBlockSize);
        Decode(Data, Pos);
        XORBuffers(Data, Pos, F, FBlockSize);
        S := B;
        B := F;
        F := S;
        Inc(Pos, FIntSize);
        Dec(DataSize, FBlockSize);
      end;

      if F <> FFeedback then
        Move(F^, FFeedback^, FBlockSize);

      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer, 0, DataSize);
        XORBuffers(Data, Pos, FBuffer, DataSize);
        XORBuffers(FFeedback, 0, FBuffer, FBlockSize);
      end;
    end;

    cmCTR: begin
      InternalEncodeBuffer(Data, DataSize);
    end;

    cmGCM: begin
      ProcessGcmAAD;

      n := DataSize;
      while n >= 16 do begin
        // GHASH
        XORBuffers(FGHASH, 0, @TCardinalArray(Data)[Pos], 16);
        GcmMulH(TValueArr(FGHASH));

        IncVector;
        Move(FVector^, FBuffer^, 16);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, 16);

        Inc(Pos, FIntSize);
        Dec(n, 16);
      end;

      if n > 0 then begin
        // GHASH
        XORBuffers(FGHASH, 0, @TCardinalArray(Data)[Pos], n);
        GcmMulH(TValueArr(FGHASH));

        IncVector;
        Move(FVector^, FBuffer^, 16);
        Encode(FBuffer, 0);
        XORBuffers(Data, Pos, FBuffer, n);
      end;

      ProcessGcmLengths(DataSize);

      // The calculated tag is bitwise compared to the received tag.
      // The message is authenticated if and only if the tags match
      if (Length(FReceivedTag) <> 16) or (MemCompare(PByteArray(FTag), PByteArray(FReceivedTag), 16) <> 0) then
        raise Exception.Create(SInvalidEncData);
    end;
{
    cmCTS: begin
      if S <> D then
        Move(S^, D^, DataSize);
      F := FFeedback;
      B := FBuffer;
      while DataSize >= FBlockSize do begin
        XORBuffers(D, F, FBlockSize, B);
        Decode(D);
        XORBuffers(D, F, FBlockSize, D);
        S := B;
        B := F;
        F := S;
        Inc(D, FBlockSize);
        Dec(DataSize, FBlockSize);
      end;
      if F <> FFeedback then
        Move(F^, FFeedback^, FBlockSize);
      if DataSize > 0 then begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        XORBuffers(FBuffer, D, DataSize, D);
        XORBuffers(FBuffer, FFeedback, FBlockSize, FFeedback);
      end;
    end;
    cmCFB: begin
      while DataSize > 0 do begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        Move(PByteArray(FFeedback)[1], FFeedback^, FBlockSize-1);
        PByteArray(FFeedback)[FBlockSize-1] := S^;
        D^ := S^ xor PByte(FBuffer)^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    end;
    cmOFB: begin
      while DataSize > 0 do begin
        Move(FFeedback^, FBuffer^, FBlockSize);
        Encode(FBuffer);
        D^ := S^ xor PByte(FBuffer)^;
        Move(PByteArray(FFeedback)[1], FFeedback^, FBlockSize-1);
        PByteArray(FFeedback)[FBlockSize-1] := PByte(FBuffer)^;
        Inc(D);
        Inc(S);
        Dec(DataSize);
      end;
    end;
    cmCTSMAC, cmCBCMAC, cmCFBMAC:
      EncodeBuffer(Source, Dest, DataSize);
}
  end;
end;

procedure TSymmetricAlgorithm.TransformBlock(const Data: TBytes; Offset, Count: Integer);
begin
  FTransformBlockMethod(TValueArr(Data), Offset, Count, TValueArr(Data), Offset);
end;

function TSymmetricAlgorithm.TransformBlock(const InputBuffer: TValueArr; InputOffset, InputCount: Integer; const OutputBuffer: TValueArr; OutputOffset: Integer): Integer;
begin
  FTransformBlockMethod(InputBuffer, InputOffset, InputCount, OutputBuffer, OutputOffset);
  Result := InputCount;
end;

procedure TSymmetricAlgorithm.TransformFinalBlock(const InputBuffer: TBytes; InputOffset, InputCount: Integer);
begin
  FTransformBlockMethod(TValueArr(InputBuffer), InputOffset, InputCount, TValueArr(InputBuffer), InputOffset);
end;

function TSymmetricAlgorithm.CreateEncryptor: ICryptoTransform;
begin
  FTransformBlockMethod := EncodeBuffer;
  Result := Self;
end;

function TSymmetricAlgorithm.CreateDecryptor: ICryptoTransform;
begin
  FTransformBlockMethod := DecodeBuffer;
  Result := Self;
end;

function TSymmetricAlgorithm.Get_OutputBlockSize: Integer;
begin
  Result := BlockSize;
end;

end.
