
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  Include a Selection of various cipher's (Encryption Algo)
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRCipher;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

uses
  SysUtils,
  CLRClasses, CRTypes, DAConsts,
  CRDECUtil, CRSymmetricAlgorithm, CRCryptoTransformIntf;

type
  TCipher_Blowfish = class;
  TCipher_Rijndael = class;
  TCipher_1DES     = class;  {Single DES  8 byte Blocksize,  8 byte Keysize  56 bits relevant}
  TCipher_3DES     = class;  {Triple DES  8 byte Blocksize, 24 byte Keysize 168 bits relevant}
  TCipher_Cast128  = class;
  TCipher_RC4      = class;  {Streamcipher}

  TCipher_Blowfish = class(TSymmetricAlgorithm)
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  end;

  TCipher_BlowfishLE = class(TSymmetricAlgorithm)
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  public
    procedure InitEx(const Key: TBytes); overload;
    procedure InitEx(const Key, Salt: TBytes); overload;
  end;

  TCipher_Rijndael = class(TSymmetricAlgorithm)
  private
    FRounds: Integer;
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  end;

  TCipher_1DES = class(TSymmetricAlgorithm)
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure MakeKey(const Data: array of Byte; const DataOffset: Integer; Key: PCardinal; const KeyOffset: Integer; Reverse: Boolean);
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
    class procedure DES_Func(Data: PCardinal; DataOffset: Integer; Key: PCardinal);
  end;

  TCipher_3DES = class(TCipher_1DES)
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  end;

  TCipher_Cast128 = class(TSymmetricAlgorithm) {Carlisle Adams and Stafford Tavares }
  private
    FRounds: Byte;
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  end;

  TCipher_RC2 = class(TSymmetricAlgorithm)
  private
    FEffectiveKeySize: integer;
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
  public
    property EffectiveKeySize: integer read FEffectiveKeySize write FEffectiveKeySize;
  end;

  TCipher_RC4 = class(TSymmetricAlgorithm)
  private
    FI: Byte;
    FJ: Byte;
    FSI: Byte;
    FSJ: Byte;
  protected
    class procedure GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes); override;
    procedure Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0); override;
    procedure Init(const Key: TBytes; const IVector: PCardinal); override;
    procedure Done; override;

    function Get_OutputBlockSize: Integer; override;
  end;

implementation

{$I cipher.inc}

{ TCipher_Blowfish }

const
  SZ_Blowfish_SBox = SizeOf(Blowfish_SBox) div SizeOf(Integer);
  SZ_Blowfish_PArray = SizeOf(Blowfish_PArray) div SizeOf(Integer);

class procedure TCipher_Blowfish.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 32;
  KeySizes.MaxSize := 448;
  KeySizes.SkipSize := 8;
  UserSize := SizeOf(Blowfish_SBox) + SizeOf(Blowfish_PArray);
end;

procedure TCipher_Blowfish.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
{$IFDEF UseASM}
asm
        PUSH   EDI
        PUSH   ESI
        PUSH   EBX
        PUSH   EBP
        SHL    ECX,2
        ADD    EDX,ECX
        PUSH   EDX

        MOV    ESI,[EAX].TCipher_Blowfish.FUser
        MOV    EBX,[EDX]         // A
        MOV    EBP,[EDX + 4]     // B

        BSWAP  EBX
        BSWAP  EBP

        XOR    EDI,EDI
        XOR    EBX,[ESI + 4 * 256 * 4]
@@1:

        MOV    EAX,EBX
        SHR    EBX,16
        MOVZX  ECX,BH
        MOV    EDX,[ESI + ECX * 4 + 1024 * 0]
        MOVZX  ECX,BL
        ADD    EDX,[ESI + ECX * 4 + 1024 * 1]

        MOVZX  ECX,AH
        XOR    EDX,[ESI + ECX * 4 + 1024 * 2]
        MOVZX  ECX,AL
        ADD    EDX,[ESI + ECX * 4 + 1024 * 3]
        XOR    EBP,[ESI + 4 * 256 * 4 + 4 + EDI * 4]

        INC    EDI
        XOR    EDX,EBP
        TEST   EDI,010h
        MOV    EBP,EAX
        MOV    EBX,EDX
        JZ     @@1

        POP    EAX
        XOR    EBP,[ESI + 4 * 256 * 4 + 17 * 4]

        BSWAP  EBX
        BSWAP  EBP

        MOV    [EAX],EBP
        MOV    [EAX + 4],EBX

        POP    EBP
        POP    EBX
        POP    ESI
        POP    EDI
end;
{$ELSE}
var
  I, A, B: Cardinal;
  P: PCardinal;
begin
  P := PCardinal(User);
  Inc(P, SZ_Blowfish_SBox);
  Inc(Data, Offset);

  A := SwapInteger(Data^) xor P^;
  Inc(P);
  Inc(Data);
  B := SwapInteger(Data^);
  Dec(Data);

  for I := 0 to 7 do begin
    B := B xor P^ xor
      (User[          A shr 24        ] +
       User[1 * 256 + A shr 16 and $FF] xor
       User[2 * 256 + A shr  8 and $FF] +
       User[3 * 256 + A        and $FF]);
    Inc(P);

    A := A xor P^ xor
      (User[          B shr 24        ] +
       User[1 * 256 + B shr 16 and $FF] xor
       User[2 * 256 + B shr  8 and $FF] +
       User[3 * 256 + B        and $FF]);
    Inc(P);
  end;

  Data^ := SwapInteger(B xor P^);
  Inc(Data);
  Data^ := SwapInteger(A);
end;
{$ENDIF}

procedure TCipher_Blowfish.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
{$IFDEF UseASM}
asm
        PUSH   EDI
        PUSH   ESI
        PUSH   EBX
        PUSH   EBP
        SHL    ECX,2
        ADD    EDX,ECX
        PUSH   EDX

        MOV    ESI,[EAX].TCipher_Blowfish.FUser
        MOV    EBX,[EDX]         // A
        MOV    EBP,[EDX + 4]     // B

        BSWAP  EBX
        BSWAP  EBP

        XOR    EBX,[ESI + 4 * 256 * 4 + 17 * 4]
        MOV    EDI,16

@@1:    MOV    EAX,EBX
        SHR    EBX,16

        MOVZX  ECX,BH
        MOV    EDX,[ESI + ECX * 4 + 1024 * 0]
        MOVZX  ECX,BL
        ADD    EDX,[ESI + ECX * 4 + 1024 * 1]

        MOVZX  ECX,AH
        XOR    EDX,[ESI + ECX * 4 + 1024 * 2]
        MOVZX  ECX,AL
        ADD    EDX,[ESI + ECX * 4 + 1024 * 3]
        XOR    EBP,[ESI + 4 * 256 * 4 + EDI * 4]

        XOR    EDX,EBP
        DEC    EDI
        MOV    EBP,EAX
        MOV    EBX,EDX
        JNZ    @@1

        POP    EAX
        XOR    EBP,[ESI + 4 * 256 * 4]

        BSWAP  EBX
        BSWAP  EBP

        MOV    [EAX],EBP
        MOV    [EAX + 4],EBX

        POP    EBP
        POP    EBX
        POP    ESI
        POP    EDI
end;
{$ELSE}
var
  I, A, B: Cardinal;
  P: PCardinal;
begin
  P := PCardinal(User);
  Inc(P, SZ_Blowfish_SBox + SZ_Blowfish_PArray - 1);
  Inc(Data, Offset);

  A := SwapInteger(Data^) xor P^;
  Inc(Data);
  B := SwapInteger(Data^);
  Dec(Data);

  for I := 0 to 7 do begin
    Dec(P);
    B := B xor P^ xor
      (User[          A shr 24        ] +
       User[1 * 256 + A shr 16 and $FF] xor
       User[2 * 256 + A shr  8 and $FF] +
       User[3 * 256 + A        and $FF]);

    Dec(P);
    A := A xor P^ xor
      (User[          B shr 24        ] +
       User[1 * 256 + B shr 16 and $FF] xor
       User[2 * 256 + B shr  8 and $FF] +
       User[3 * 256 + B        and $FF]);
  end;

  Dec(P);
  Data^ := SwapInteger(B xor P^);
  Inc(Data);
  Data^ := SwapInteger(A);
end;
{$ENDIF}

procedure TCipher_Blowfish.Init(const Key: TBytes; const IVector: PCardinal);
var
  I, J: Integer;
  B: array[0..1] of Cardinal;
  KeyLength: Integer;
begin
  InitBegin(Length(Key));
  Buffer.BlockCopy(TCardinalArray(@Blowfish_SBox), 0, User, 0, SizeOf(Blowfish_SBox));
  Buffer.BlockCopy(Blowfish_PArray, 0, User, SizeOf(Blowfish_SBox), SizeOf(Blowfish_PArray));

  KeyLength := Length(Key);

  J := 0;
  for I := 0 to 17 do begin
    User[SZ_Blowfish_SBox + I] := User[SZ_Blowfish_SBox + I] xor
      (Key[(J + 0) mod KeyLength] shl 24 +
       Key[(J + 1) mod KeyLength] shl 16 +
       Key[(J + 2) mod KeyLength] shl  8 +
       Key[(J + 3) mod KeyLength]);
    Inc(J, 4);
  end;

  B[0] := 0;
  B[1] := 0;
  J := 0;
  for I := 0 to 8 do begin
    Encode(@B, 0);
    User[SZ_Blowfish_SBox + J] := SwapInteger(B[0]);
    User[SZ_Blowfish_SBox + J + 1] := SwapInteger(B[1]);
    Inc(J, 2);
  end;

  J := 0;
  for I := 0 to 511 do begin
    Encode(@B, 0);
    User[J] := SwapInteger(B[0]);
    User[J + 1] := SwapInteger(B[1]);
    Inc(J, 2);
  end;

  B[0] := 0;
  B[1] := 0;

  InitEnd(IVector);
end;

{ TCipher_BlowfishLE }

class procedure TCipher_BlowfishLE.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 32;
  KeySizes.MaxSize := 448;
  KeySizes.SkipSize := 8;
  UserSize := SizeOf(Blowfish_SBox) + SizeOf(Blowfish_PArray);
end;

procedure TCipher_BlowfishLE.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  I, A, B: Cardinal;
  P: PCardinal;
begin
  P := PCardinal(User);
  Inc(P, SZ_Blowfish_SBox);
  Inc(Data, Offset);

  A := Data^ xor P^;
  Inc(P);
  Inc(Data);
  B := Data^;
  Dec(Data);

  for I := 0 to 7 do begin
    B := B xor P^ xor
              (User[          A shr 24        ] +
               User[1 * 256 + A shr 16 and $FF] xor
               User[2 * 256 + A shr  8 and $FF] +
               User[3 * 256 + A        and $FF]);

    Inc(P);

    A := A xor P^ xor
              (User[          B shr 24        ] +
               User[1 * 256 + B shr 16 and $FF] xor
               User[2 * 256 + B shr  8 and $FF] +
               User[3 * 256 + B        and $FF]);
    Inc(P);
  end;

  Data^ := B xor P^;
  Inc(Data);
  Data^ := A;
end;

procedure TCipher_BlowfishLE.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  I, A, B: Cardinal;
  P: PCardinal;
begin
  P := PCardinal(User);
  Inc(P, SZ_Blowfish_SBox + SZ_Blowfish_PArray - 1);
  Inc(Data, Offset);

  A := Data^ xor P^;
  Inc(Data);
  B := Data^;
  Dec(Data);

  for I := 0 to 7 do begin
    Dec(P);
    B := B xor P^ xor
              (User[          A shr 24        ] +
               User[1 * 256 + A shr 16 and $FF] xor
               User[2 * 256 + A shr  8 and $FF] +
               User[3 * 256 + A        and $FF]);
    Dec(P);

    A := A xor P^ xor
              (User[          B shr 24        ] +
               User[1 * 256 + B shr 16 and $FF] xor
               User[2 * 256 + B shr  8 and $FF] +
               User[3 * 256 + B        and $FF]);
  end;

  Dec(P);
  Data^ := B xor P^;
  Inc(Data);
  Data^ := A;
end;

procedure TCipher_BlowfishLE.Init(const Key: TBytes; const IVector: PCardinal);
begin
  InitBegin(Length(Key));
  Buffer.BlockCopy(TCardinalArray(@Blowfish_SBox), 0, User, 0, SizeOf(Blowfish_SBox));
  Buffer.BlockCopy(Blowfish_PArray, 0, User, SizeOf(Blowfish_SBox), SizeOf(Blowfish_PArray));

  InitEx(Key);

  InitEnd(IVector);
end;

procedure TCipher_BlowfishLE.InitEx(const Key: TBytes);
var
  I, J: Integer;
  B: array[0..1] of Cardinal;
  KeyLength: Integer;
begin
  KeyLength := Length(Key);

  J := 0;
  for I := 0 to 17 do begin
    User[SZ_Blowfish_SBox + I] := User[SZ_Blowfish_SBox + I] xor
      (Key[(J + 0) mod KeyLength] shl 24 +
       Key[(J + 1) mod KeyLength] shl 16 +
       Key[(J + 2) mod KeyLength] shl  8 +
       Key[(J + 3) mod KeyLength]);
    Inc(J, 4);
  end;

  B[0] := 0;
  B[1] := 0;
  J := 0;
  for I := 0 to 8 do begin
    Encode(@B, 0);
    User[SZ_Blowfish_SBox + J] := B[0];
    User[SZ_Blowfish_SBox + J + 1] := B[1];
    Inc(J, 2);
  end;

  J := 0;
  for I := 0 to 511 do begin
    Encode(@B, 0);
    User[J] := B[0];
    User[J + 1] := B[1];
    Inc(J, 2);
  end;

  B[0] := 0;
  B[1] := 0;
end;

procedure TCipher_BlowfishLE.InitEx(const Key, Salt: TBytes);
var
  I, J: Integer;
  B: array[0..1] of Cardinal;
  SaltC: array of Cardinal;
  KeyLength, SaltLength: Integer;
begin
  if (Length(Salt) = 0) or ((Length(Salt) mod 4) <> 0) then
    raise Exception.Create(SInvalidIV);

  KeyLength := Length(Key);
  if not Initialized then begin
    Buffer.BlockCopy(TCardinalArray(@Blowfish_SBox), 0, User, 0, SizeOf(Blowfish_SBox));
    Buffer.BlockCopy(Blowfish_PArray, 0, User, SizeOf(Blowfish_SBox), SizeOf(Blowfish_PArray));
  end;

  J := 0;
  for I := 0 to 17 do begin
    User[SZ_Blowfish_SBox + I] := User[SZ_Blowfish_SBox + I] xor
      (Key[(J + 0) mod KeyLength] shl 24 +
       Key[(J + 1) mod KeyLength] shl 16 +
       Key[(J + 2) mod KeyLength] shl  8 +
       Key[(J + 3) mod KeyLength]);
    Inc(J, 4);
  end;

  SaltLength := Length(Salt) div 4;
  SetLength(SaltC, SaltLength);
  for I := 0 to SaltLength - 1 do
    SaltC[I] := (Salt[I * 4] shl 24) + (Salt[I * 4 + 1] shl 16) + (Salt[I * 4 + 2] shl 8) + Salt[I * 4 + 3];

  B[0] := 0;
  B[1] := 0;

  J := 0;
  for I := 0 to 8 do begin
    B[0] := B[0] xor SaltC[J mod SaltLength];
    B[1] := B[1] xor SaltC[(J + 1) mod SaltLength];

    Encode(@B, 0);

    User[SZ_Blowfish_SBox + J] := B[0];
    User[SZ_Blowfish_SBox + J + 1] := B[1];
    Inc(J, 2);
  end;

  for I := 0 to 511 do begin
    B[0] := B[0] xor SaltC[J mod SaltLength];
    B[1] := B[1] xor SaltC[(J + 1) mod SaltLength];
    Inc(J, 2);

    Encode(@B, 0);

    User[I * 2] := B[0];
    User[I * 2 + 1] := B[1];
  end;

  if not Initialized then
    InitEnd(nil);
end;

{ TCipher_Rijndael }

const
  Rijndael_Blocks =  4;
  Rijndael_Rounds = 14;

class procedure TCipher_Rijndael.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := Rijndael_Blocks * 4;
  KeySizes.MinSize := 128;
  KeySizes.MaxSize := 256;
  KeySizes.SkipSize := 64;
  UserSize := (Rijndael_Rounds + 1) * Rijndael_Blocks * SizeOf(Integer) * 2;
end;

procedure TCipher_Rijndael.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  P: PCardinal;
  I, A, B, C, D: Cardinal;
begin
  P := PCardinal(User);
  Inc(Data, Offset);

  for I := 2 to FRounds do begin
    A := Data^ xor P^;
    Inc(P);
    Inc(Data);
    B := Data^ xor P^;
    Inc(P);
    Inc(Data);
    C := Data^ xor P^;
    Inc(P);
    Inc(Data);
    D := Data^ xor P^;
    Inc(P);

    Data^ := Rijndael_T[0, D and $FF]        xor
             Rijndael_T[1, A shr  8 and $FF] xor
             Rijndael_T[2, B shr 16 and $FF] xor
             Rijndael_T[3, C shr 24];
    Dec(Data);

    Data^ := Rijndael_T[0, C and $FF]        xor
             Rijndael_T[1, D shr  8 and $FF] xor
             Rijndael_T[2, A shr 16 and $FF] xor
             Rijndael_T[3, B shr 24];
    Dec(Data);

    Data^ := Rijndael_T[0, B and $FF]        xor
             Rijndael_T[1, C shr  8 and $FF] xor
             Rijndael_T[2, D shr 16 and $FF] xor
             Rijndael_T[3, A shr 24];
    Dec(Data);

    Data^ := Rijndael_T[0, A and $FF]        xor
             Rijndael_T[1, B shr  8 and $FF] xor
             Rijndael_T[2, C shr 16 and $FF] xor
             Rijndael_T[3, D shr 24];
  end;

  A := Data^ xor P^;
  Inc(P);
  Inc(Data);
  B := Data^ xor P^;
  Inc(P);
  Inc(Data);
  C := Data^ xor P^;
  Inc(P);
  Inc(Data);
  D := Data^ xor P^;
  Inc(P);

  Data^ := Rijndael_S[0, D and $FF]               or
           Rijndael_S[0, A shr  8 and $FF] shl  8 or
           Rijndael_S[0, B shr 16 and $FF] shl 16 or
           Rijndael_S[0, C shr 24]         shl 24;
  Dec(Data);

  Data^ := Rijndael_S[0, C and $FF]               or
           Rijndael_S[0, D shr  8 and $FF] shl  8 or
           Rijndael_S[0, A shr 16 and $FF] shl 16 or
           Rijndael_S[0, B shr 24]         shl 24;
  Dec(Data);

  Data^ := Rijndael_S[0, B and $FF]               or
           Rijndael_S[0, C shr  8 and $FF] shl  8 or
           Rijndael_S[0, D shr 16 and $FF] shl 16 or
           Rijndael_S[0, A shr 24]         shl 24;
  Dec(Data);

  Data^ := Rijndael_S[0, A and $FF]               or
           Rijndael_S[0, B shr  8 and $FF] shl  8 or
           Rijndael_S[0, C shr 16 and $FF] shl 16 or
           Rijndael_S[0, D shr 24]         shl 24;

  for I := 1 to Rijndael_Blocks do begin
    Data^ := Data^ xor P^;
    Inc(P);
    Inc(Data);
  end;
end;

procedure TCipher_Rijndael.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  P: PCardinal;
  I, A, B, C, D: Cardinal;
begin
  P := PCardinal(User);
  Inc(P, (UserSize shr 1) div SizeOf(Integer) + FRounds * 4 + 3);
  Inc(Data, Offset + 3);

  for I := 2 to FRounds do begin
    D := Data^ xor P^;
    Dec(P); Dec(Data);
    C := Data^ xor P^;
    Dec(P); Dec(Data);
    B := Data^ xor P^;
    Dec(P); Dec(Data);
    A := Data^ xor P^;
    Dec(P);

    Data^ := Rijndael_T[4, A and $FF]        xor
             Rijndael_T[5, D shr  8 and $FF] xor
             Rijndael_T[6, C shr 16 and $FF] xor
             Rijndael_T[7, B shr 24];
    Inc(Data);

    Data^ := Rijndael_T[4, B and $FF]        xor
             Rijndael_T[5, A shr  8 and $FF] xor
             Rijndael_T[6, D shr 16 and $FF] xor
             Rijndael_T[7, C shr 24];
    Inc(Data);

    Data^ := Rijndael_T[4, C and $FF]        xor
             Rijndael_T[5, B shr  8 and $FF] xor
             Rijndael_T[6, A shr 16 and $FF] xor
             Rijndael_T[7, D shr 24];
    Inc(Data);

    Data^ := Rijndael_T[4, D and $FF]        xor
             Rijndael_T[5, C shr  8 and $FF] xor
             Rijndael_T[6, B shr 16 and $FF] xor
             Rijndael_T[7, A shr 24];
  end;

  D := Data^ xor P^;
  Dec(P);
  Dec(Data);
  C := Data^ xor P^;
  Dec(P);
  Dec(Data);
  B := Data^ xor P^;
  Dec(P);
  Dec(Data);
  A := Data^ xor P^;
  Dec(P);

  Data^ := Rijndael_S[1, A and $FF]               or
           Rijndael_S[1, D shr  8 and $FF] shl  8 or
           Rijndael_S[1, C shr 16 and $FF] shl 16 or
           Rijndael_S[1, B shr 24]         shl 24;
  Inc(Data);

  Data^ := Rijndael_S[1, B and $FF]               or
           Rijndael_S[1, A shr  8 and $FF] shl  8 or
           Rijndael_S[1, D shr 16 and $FF] shl 16 or
           Rijndael_S[1, C shr 24]         shl 24;
  Inc(Data);

  Data^ := Rijndael_S[1, C and $FF]               or
           Rijndael_S[1, B shr  8 and $FF] shl  8 or
           Rijndael_S[1, A shr 16 and $FF] shl 16 or
           Rijndael_S[1, D shr 24]         shl 24;
  Inc(Data);

  Data^ := Rijndael_S[1, D and $FF]               or
           Rijndael_S[1, C shr  8 and $FF] shl  8 or
           Rijndael_S[1, B shr 16 and $FF] shl 16 or
           Rijndael_S[1, A shr 24]         shl 24;

  for I := 0 to 3 do begin
    Data^ := Data^ xor P^;
    Dec(P);
    Dec(Data);
  end;
end;

procedure TCipher_Rijndael.Init(const Key: TBytes; const IVector: PCardinal);
var
  K: array[0..7] of Cardinal;

  procedure BuildEncodeKey;
  const
    RND_Data: array[0..29] of Byte = (
      $01,$02,$04,$08,$10,$20,$40,$80,$1B,$36,$6C,$D8,$AB,$4D,$9A,
      $2F,$5E,$BC,$63,$C6,$97,$35,$6A,$D4,$B3,$7D,$FA,$EF,$C5,$91);
  var
    T, R: Integer;

    procedure NextRounds;
    var
      J: Integer;
    begin
      J := 0;
      while (J < FRounds - 6) and (R <= FRounds) do begin
        while (J < FRounds - 6) and (T < Rijndael_Blocks) do begin
          User[R * Rijndael_Blocks + T] := K[J];
          Inc(J);
          Inc(T);
        end;

        if T = Rijndael_Blocks then begin
          T := 0;
          Inc(R);
        end;
      end;
    end;

  var
    RNDPos: Integer;
    B: PByte;
    I: Integer;
  begin
    R := 0;
    T := 0;
    RNDPos := 0;
    NextRounds;

    while R <= FRounds do begin
      B  := @K;
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr  8 and $FF] xor RND_Data[RNDPos];
      Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr 16 and $FF];
      Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] shr 24];
      Inc(B);
      B^ := B^ xor Rijndael_S[0, K[FRounds -7] and $FF];

      Inc(RNDPos);
      if FRounds = 14 then begin
        for I := 1 to 3 do
          K[I] := K[I] xor K[I -1];

        B := @K[4];
        B^ := B^ xor Rijndael_S[0, K[3] and $FF];
        Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr  8 and $FF];
        Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr 16 and $FF];
        Inc(B);
        B^ := B^ xor Rijndael_S[0, K[3] shr 24];

        for I := 5 to 7 do
          K[I] := K[I] xor K[I -1];
      end
      else
        for I := 1 to FRounds -7 do
          K[I] := K[I] xor K[I -1];

      NextRounds;
    end;
  end;

  procedure BuildDecodeKey;
  var
    I: Integer;
    D: PCardinal;
    USize: Integer;
  begin
    D := PCardinal(User);
    USize := UserSize shr 1;
    Buffer.BlockCopy(User, 0, TCardinalArray(D), USize, USize);

    Inc(D, USize div SizeOf(Integer) + 4);
    for I := 0 to FRounds * 4 - 5 do begin
      D^ := Rijndael_Key[D^ and $FF] xor
        ROL(Rijndael_Key[D^ shr  8 and $FF],  8) xor
        ROL(Rijndael_Key[D^ shr 16 and $FF], 16) xor
        ROL(Rijndael_Key[D^ shr 24],         24);
      Inc(D);
    end;
  end;

begin
  InitBegin(Length(Key));
  if Length(Key) <= 16 then
    FRounds := 10
  else
  if Length(Key) <= 24 then
    FRounds := 12
  else
    FRounds := 14;

  FillChar(K, SizeOf(K), 0);
  Assert(Length(Key) <= 32);
  Buffer.BlockCopy(Key, 0, K, 0, Length(Key));
  BuildEncodeKey;
  BuildDecodeKey;
  FillChar(K, SizeOf(K), 0);

  InitEnd(IVector);
end;

{ TCipher_1DES }

class procedure TCipher_1DES.DES_Func(Data: PCardinal; DataOffset: Integer; Key: PCardinal);
var
  L, R, X, Y, I: Cardinal;
begin
  Inc(Data, DataOffset);

  L := SwapInteger(Data^);
  Inc(Data);
  R := SwapInteger(Data^);
  Dec(Data);

  X := (L shr 4 xor R) and $0F0F0F0F;
  R := R xor X;
  L := L xor X shl 4;

  X := (L shr 16 xor R) and $0000FFFF;
  R := R xor X;
  L := L xor X shl 16;

  X := (R shr 2 xor L) and $33333333;
  L := L xor X;
  R := R xor X shl 2;

  X := (R shr 8 xor L) and $00FF00FF;
  L := L xor X;
  R := R xor X shl 8;

  R := R shl 1 or R shr 31;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 1 or L shr 31;

  for I := 0 to 7 do begin
    X := (R shl 28 or R shr 4) xor Key^;
    Inc(Key);
    Y := R xor Key^;
    Inc(Key);
    L := L xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);

    X := (L shl 28 or L shr 4) xor Key^;
    Inc(Key);
    Y := L xor Key^;
    Inc(Key);
    R := R xor (DES_Data[0, X        and $3F] or DES_Data[1, X shr  8 and $3F] or
                DES_Data[2, X shr 16 and $3F] or DES_Data[3, X shr 24 and $3F] or
                DES_Data[4, Y        and $3F] or DES_Data[5, Y shr  8 and $3F] or
                DES_Data[6, Y shr 16 and $3F] or DES_Data[7, Y shr 24 and $3F]);
  end;

  R := R shl 31 or R shr 1;
  X := (L xor R) and $AAAAAAAA;
  R := R xor X;
  L := L xor X;
  L := L shl 31 or L shr 1;

  X := (L shr 8 xor R) and $00FF00FF;
  R := R xor X;
  L := L xor X shl  8;
  X := (L shr 2 xor R) and $33333333;
  R := R xor X;
  L := L xor X shl  2;
  X := (R shr 16 xor L) and $0000FFFF;
  L := L xor X;
  R := R xor X shl 16;
  X := (R shr 4 xor L) and $0F0F0F0F;
  L := L xor X;
  R := R xor X shl  4;

  Data^ := SwapInteger(R);
  Inc(Data);
  Data^ := SwapInteger(L);
end;

class procedure TCipher_1DES.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 64;
  KeySizes.MaxSize := 64;
  KeySizes.SkipSize := 0;
  UserSize := 32 * 4 * 2;
end;

procedure TCipher_1DES.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
  DES_Func(Data, Offset, PCardinal(User));
end;

procedure TCipher_1DES.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
  DES_Func(Data, Offset, @User[32]);
end;

procedure TCipher_1DES.MakeKey(const Data: array of Byte; const DataOffset: Integer; Key: PCardinal; const KeyOffset: Integer; Reverse: Boolean);
const
  ROT: array[0..15] of Byte = (1,2,4,6,8,10,12,14,15,17,19,21,23,25,27,28);
var
  I, J, L, M, N: Cardinal;
  PC_M, PC_R: array[0..55] of Byte;
  K: array[0..31] of Cardinal;
begin
  FillChar(K, SizeOf(K), 0);

  for I := 0 to 55 do begin
    if Data[DataOffset + Integer(DES_PC1[I] shr 3)] and ($80 shr (DES_PC1[I] and $07)) <> 0 then
      PC_M[I] := 1
    else
      PC_M[I] := 0;
  end;

  for I := 0 to 15 do begin
    if Reverse then
      M := (15 - I) shl 1
    else
      M := I shl 1;
    N := M + 1;

    for J := 0 to 27 do begin
      L := J + ROT[I];
      if L < 28 then
        PC_R[J] := PC_M[L]
      else
        PC_R[J] := PC_M[L - 28];
    end;

    for J := 28 to 55 do begin
      L := J + ROT[I];
      if L < 56 then
        PC_R[J] := PC_M[L]
      else
        PC_R[J] := PC_M[L - 28];
    end;

    L := $1000000;
    for J := 0 to 23 do begin
      L := L shr 1;
      if PC_R[DES_PC2[J]] <> 0 then
        K[M] := K[M] or L;
      if PC_R[DES_PC2[J + 24]] <> 0 then
        K[N] := K[N] or L;
    end;
  end;

  Inc(Key, KeyOffset);
  for I := 0 to 15 do begin
    M := I shl 1;
    N := M + 1;
    Key^ := K[M] and $00FC0000 shl  6 or
            K[M] and $00000FC0 shl 10 or
            K[N] and $00FC0000 shr 10 or
            K[N] and $00000FC0 shr  6;
    Inc(Key);

    Key^ := K[M] and $0003F000 shl 12 or
            K[M] and $0000003F shl 16 or
            K[N] and $0003F000 shr  4 or
            K[N] and $0000003F;
    Inc(Key);
  end;
end;

procedure TCipher_1DES.Init(const Key: TBytes; const IVector: PCardinal);
begin
  InitBegin(Length(Key));
  Assert(Length(Key) = 8);
  MakeKey(Key, 0, PCardinal(User), 0,  False);
  MakeKey(Key, 0, PCardinal(User), 32, True);
  InitEnd(IVector);
end;

{ TCipher_3DES }

class procedure TCipher_3DES.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 128;
  KeySizes.MaxSize := 192;
  KeySizes.SkipSize := 64;
  UserSize := 32 * 4 * 2 * 3;
end;

procedure TCipher_3DES.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
  DES_Func(Data, Offset, @User[0]);
  DES_Func(Data, Offset, @User[32]);
  DES_Func(Data, Offset, @User[64]);
end;

procedure TCipher_3DES.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
  DES_Func(Data, Offset, @User[96]);
  DES_Func(Data, Offset, @User[128]);
  DES_Func(Data, Offset, @User[160]);
end;

procedure TCipher_3DES.Init(const Key: TBytes; const IVector: PCardinal);
var
  Pos: Integer;
begin
  InitBegin(Length(Key));

  Assert((Length(Key) = 16) or (Length(Key) = 24));
  Pos := 0;
  MakeKey(Key, 0, PCardinal(User), Pos, False);    Inc(Pos, 32);
  MakeKey(Key, 8, PCardinal(User), Pos, True);     Inc(Pos, 32);

  if Length(Key) = 24 then begin
    MakeKey(Key, 16, PCardinal(User), Pos, False); Inc(Pos, 32);
    MakeKey(Key, 16, PCardinal(User), Pos, True);  Inc(Pos, 32);
  end
  else begin
    MakeKey(Key, 0, PCardinal(User), Pos, False);  Inc(Pos, 32);
    MakeKey(Key, 0, PCardinal(User), Pos, True);   Inc(Pos, 32);
  end;

  MakeKey(Key, 8, PCardinal(User), Pos, False);    Inc(Pos, 32);
  MakeKey(Key, 0, PCardinal(User), Pos, True);

  InitEnd(IVector);
end;

{ TCipher_Cast128 }

class procedure TCipher_Cast128.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 40;
  KeySizes.MaxSize := 128;
  KeySizes.SkipSize := 8;
  UserSize := 128;
end;

procedure TCipher_Cast128.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  T, I, A, B: Cardinal;
  K1, K2: PCardinal;
begin
  K1 := PCardinal(User);
  K2 := PCardinal(User);
  Inc(K2, 16);
  Inc(Data, Offset);

  A := SwapInteger(Data^);
  Inc(Data);
  B := SwapInteger(Data^);

  for I := 0 to 2 do begin
    T := ROL(K1^ + B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Inc(K1);
    Inc(K2);

    T := ROL(K1^ xor A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Inc(K1);
    Inc(K2);

    T := ROL(K1^ - B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Inc(K1);
    Inc(K2);

    T := ROL(K1^ + A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);

    if I = 2 then
      Break;

    Inc(K1);
    Inc(K2);

    T := ROL(K1^ xor B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Inc(K1);
    Inc(K2);

    T := ROL(K1^ - A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Inc(K1);
    Inc(K2);

    if (I = 1) and (FRounds <= 12) then
      Break;
  end;

  Data^ := SwapInteger(A);
  Dec(Data);
  Data^ := SwapInteger(B);
end;

procedure TCipher_Cast128.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  T, I, A, B: Cardinal;
  K1, K2: PCardinal;
label
  Start;
begin
  K1 := PCardinal(User);
  K2 := PCardinal(User);
  Inc(K1, 15);
  Inc(K2, 15+16);
  Inc(Data, Offset);

  B := SwapInteger(Data^);
  Inc(Data);
  A := SwapInteger(Data^);
  Dec(Data);
  I := 2;
  if FRounds <= 12 then begin
    Dec(K1, 4);
    Dec(K2, 4);
  end
  else
    goto Start;

  while I > 0 do begin
    Dec(I);
    T := ROL(K1^ - A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);
    T := ROL(K1^ xor B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);

Start:
    T := ROL(K1^ + A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);
    T := ROL(K1^ - B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] +
                Cast128_Data[1, T shr 16 and $FF] xor
                Cast128_Data[2, T shr  8 and $FF] -
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);
    T := ROL(K1^ xor A, K2^);
    B := B xor (Cast128_Data[0, T shr 24] -
                Cast128_Data[1, T shr 16 and $FF] +
                Cast128_Data[2, T shr  8 and $FF] xor
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);
    T := ROL(K1^ + B, K2^);
    A := A xor (Cast128_Data[0, T shr 24] xor
                Cast128_Data[1, T shr 16 and $FF] -
                Cast128_Data[2, T shr  8 and $FF] +
                Cast128_Data[3, T and $FF]);
    Dec(K1);
    Dec(K2);
  end;

  Data^ := SwapInteger(A);
  Inc(Data);
  Data^ := SwapInteger(B);
end;

procedure TCipher_Cast128.Init(const Key: TBytes; const IVector: PCardinal);
var
  Z, X, T: array[0..3] of Cardinal;
  K: TCardinalArray;
  I: Cardinal;
begin
  InitBegin(Length(Key));
  if Length(Key) <= 10 then
    FRounds := 12
  else
    FRounds := 16;

  K := User;
  FillChar(X, SizeOf(X), 0);
  Assert(Length(Key) <= 16);
  Buffer.BlockCopy(Key, 0, X, 0, Length(Key));
  SwapIntegerBuffer(@X, @X, 4);
//  for I := 0 to 3 do X[I] := SwapInteger(X[I]);

  I := 0;
  while I < 32 do begin
    if I and 4 = 0 then begin
      Z[0] := X[0] xor Cast128_Key[0, X[3] shr 16 and $FF] xor
                       Cast128_Key[1, X[3] and $FF] xor
                       Cast128_Key[2, X[3] shr 24] xor
                       Cast128_Key[3, X[3] shr  8 and $FF] xor
                       Cast128_Key[2, X[2] shr 24];
      T[0] := Z[0];
      Z[1] := X[2] xor Cast128_Key[0, Z[0] shr 24] xor
                       Cast128_Key[1, Z[0] shr  8 and $FF] xor
                       Cast128_Key[2, Z[0] shr 16 and $FF] xor
                       Cast128_Key[3, Z[0] and $FF] xor
                       Cast128_Key[3, X[2] shr  8 and $FF];
      T[1] := Z[1];
      Z[2] := X[3] xor Cast128_Key[0, Z[1] and $FF] xor
                       Cast128_Key[1, Z[1] shr  8 and $FF] xor
                       Cast128_Key[2, Z[1] shr 16 and $FF] xor
                       Cast128_Key[3, Z[1] shr 24] xor
                       Cast128_Key[0, X[2] shr 16 and $FF];
      T[2] := Z[2];
      Z[3] := X[1] xor Cast128_Key[0, Z[2] shr  8 and $FF] xor
                       Cast128_Key[1, Z[2] shr 16 and $FF] xor
                       Cast128_Key[2, Z[2] and $FF] xor
                       Cast128_Key[3, Z[2] shr 24] xor
                       Cast128_Key[1, X[2] and $FF];
      T[3] := Z[3];
    end
    else begin
      X[0] := Z[2] xor Cast128_Key[0, Z[1] shr 16 and $FF] xor
                       Cast128_Key[1, Z[1] and $FF] xor
                       Cast128_Key[2, Z[1] shr 24] xor
                       Cast128_Key[3, Z[1] shr  8 and $FF] xor
                       Cast128_Key[2, Z[0] shr 24];
      T[0] := X[0];
      X[1] := Z[0] xor Cast128_Key[0, X[0] shr 24] xor
                       Cast128_Key[1, X[0] shr  8 and $FF] xor
                       Cast128_Key[2, X[0] shr 16 and $FF] xor
                       Cast128_Key[3, X[0] and $FF] xor
                       Cast128_Key[3, Z[0] shr  8 and $FF];
      T[1] := X[1];
      X[2] := Z[1] xor Cast128_Key[0, X[1] and $FF] xor
                       Cast128_Key[1, X[1] shr  8 and $FF] xor
                       Cast128_Key[2, X[1] shr 16 and $FF] xor
                       Cast128_Key[3, X[1] shr 24] xor
                       Cast128_Key[0, Z[0] shr 16 and $FF];
      T[2] := X[2];
      X[3] := Z[3] xor Cast128_Key[0, X[2] shr  8 and $FF] xor
                       Cast128_Key[1, X[2] shr 16 and $FF] xor
                       Cast128_Key[2, X[2] and $FF] xor
                       Cast128_Key[3, X[2] shr 24] xor
                       Cast128_Key[1, Z[0] and $FF];
      T[3] := X[3];
    end;

    case I and 12 of
      0, 12: begin
        K[I + 0] := Cast128_Key[0, T[2] shr 24] xor
                    Cast128_Key[1, T[2] shr 16 and $FF] xor
                    Cast128_Key[2, T[1] and $FF] xor
                    Cast128_Key[3, T[1] shr  8 and $FF];
        K[I + 1] := Cast128_Key[0, T[2] shr  8 and $FF] xor
                    Cast128_Key[1, T[2] and $FF] xor
                    Cast128_Key[2, T[1] shr 16 and $FF] xor
                    Cast128_Key[3, T[1] shr 24];
        K[I + 2] := Cast128_Key[0, T[3] shr 24] xor
                    Cast128_Key[1, T[3] shr 16 and $FF] xor
                    Cast128_Key[2, T[0] and $FF] xor
                    Cast128_Key[3, T[0] shr  8 and $FF];
        K[I + 3] := Cast128_Key[0, T[3] shr  8 and $FF] xor
                    Cast128_Key[1, T[3] and $FF] xor
                    Cast128_Key[2, T[0] shr 16 and $FF] xor
                    Cast128_Key[3, T[0] shr 24];
      end;
      4, 8: begin
        K[I + 0] := Cast128_Key[0, T[0] and $FF] xor
                    Cast128_Key[1, T[0] shr  8 and $FF] xor
                    Cast128_Key[2, T[3] shr 24] xor
                    Cast128_Key[3, T[3] shr 16 and $FF];
        K[I + 1] := Cast128_Key[0, T[0] shr 16 and $FF] xor
                    Cast128_Key[1, T[0] shr 24] xor
                    Cast128_Key[2, T[3] shr  8 and $FF] xor
                    Cast128_Key[3, T[3] and $FF];
        K[I + 2] := Cast128_Key[0, T[1] and $FF] xor
                    Cast128_Key[1, T[1] shr  8 and $FF] xor
                    Cast128_Key[2, T[2] shr 24] xor
                    Cast128_Key[3, T[2] shr 16 and $FF];
        K[I + 3] := Cast128_Key[0, T[1] shr 16 and $FF] xor
                    Cast128_Key[1, T[1] shr 24] xor
                    Cast128_Key[2, T[2] shr  8 and $FF] xor
                    Cast128_Key[3, T[2] and $FF];
      end;
    end;

    case I and 12 of
      0: begin
        K[I + 0] := K[I + 0] xor Cast128_Key[0, Z[0] shr 8 and $FF];
        K[I + 1] := K[I + 1] xor Cast128_Key[1, Z[1] shr 8 and $FF];
        K[I + 2] := K[I + 2] xor Cast128_Key[2, Z[2] shr 16 and $FF];
        K[I + 3] := K[I + 3] xor Cast128_Key[3, Z[3] shr 24];
      end;
      4: begin
        K[I + 0] := K[I + 0] xor Cast128_Key[0, X[2] shr 24];
        K[I + 1] := K[I + 1] xor Cast128_Key[1, X[3] shr 16 and $FF];
        K[I + 2] := K[I + 2] xor Cast128_Key[2, X[0] and $FF];
        K[I + 3] := K[I + 3] xor Cast128_Key[3, X[1] and $FF];
      end;
      8: begin
        K[I + 0] := K[I + 0] xor Cast128_Key[0, Z[2] shr 16 and $FF];
        K[I + 1] := K[I + 1] xor Cast128_Key[1, Z[3] shr 24];
        K[I + 2] := K[I + 2] xor Cast128_Key[2, Z[0] shr 8 and $FF];
        K[I + 3] := K[I + 3] xor Cast128_Key[3, Z[1] shr 8 and $FF];
      end;
      12: begin
        K[I + 0] := K[I + 0] xor Cast128_Key[0, X[0] and $FF];
        K[I + 1] := K[I + 1] xor Cast128_Key[1, X[1] and $FF];
        K[I + 2] := K[I + 2] xor Cast128_Key[2, X[2] shr 24];
        K[I + 3] := K[I + 3] xor Cast128_Key[3, X[3] shr 16 and $FF];
      end;
    end;

    if I >= 16 then begin
      K[I + 0] := K[I + 0] and $1F;
      K[I + 1] := K[I + 1] and $1F;
      K[I + 2] := K[I + 2] and $1F;
      K[I + 3] := K[I + 3] and $1F;
    end;

    Inc(I, 4);
  end;

  FillChar(X, SizeOf(X), 0);
  FillChar(Z, SizeOf(Z), 0);
  FillChar(T, SizeOf(T), 0);

  InitEnd(IVector);
end;

{ TCipher_RC2 }

class procedure TCipher_RC2.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := 8;
  KeySizes.MinSize := 8;
  KeySizes.MaxSize := 1024;
  KeySizes.SkipSize := 8;
  UserSize := 128;
end;

type
  PRC2Rec = ^TRC2Rec;
  TRC2Rec = packed record
    A, B, C, D: Word;
  end;

procedure TCipher_RC2.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);

{$IFDEF UseASM}
  function ROLADD16(Value, Add, Shift: Integer): Word; assembler;
  asm
      ADD  EAX,EDX
      ROL  AX,CL
  end;
{$ELSE}
  function ROLADD16(Value, Add, Shift: Integer): Word;
  begin
    Inc(Value, Add);
    Result := (Word(Value) shl Shift) or (Word(Value) shr (16 - Shift));
  end;
{$ENDIF}

var
  I: Integer;
  K: PWord;
  L: PWordArray;
begin
  K := Pointer(User);
  L := Pointer(User);

  Inc(Data, Offset);

  with PRC2Rec(Data)^ do begin
    for I := 0 to 4 do begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
    Inc(A, L[D and $3F]);
    Inc(B, L[A and $3F]);
    Inc(C, L[B and $3F]);
    Inc(D, L[C and $3F]);
    for I := 0 to 5 do begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
    Inc(A, L[D and $3F]);
    Inc(B, L[A and $3F]);
    Inc(C, L[B and $3F]);
    Inc(D, L[C and $3F]);
    for I := 0 to 4 do begin
      A := ROLADD16(A, (B and not D) + (C and D) + K^, 1); Inc(K);
      B := ROLADD16(B, (C and not A) + (D and A) + K^, 2); Inc(K);
      C := ROLADD16(C, (D and not B) + (A and B) + K^, 3); Inc(K);
      D := ROLADD16(D, (A and not C) + (B and C) + K^, 5); Inc(K);
    end;
  end;
end;

procedure TCipher_RC2.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);

{$IFDEF UseASM}
  function RORSUB16(Value, Sub, Shift: Integer): Word; assembler;
  asm
      ROR  AX,CL
      SUB  EAX,EDX
  end;
{$ELSE}
  function RORSUB16(Value, Sub, Shift: Integer): Word;
  begin
    Result := (Word(Value) shr Shift) or (Word(Value) shl (16 - Shift));
    Dec(Result, Sub);
  end;
{$ENDIF}

var
  I: Integer;
  K: PWord;
  L: PWordArray;
begin
  L := Pointer(User);
  K := Pointer(@L[63]);

  Inc(Data, Offset);

  with PRC2Rec(Data)^ do begin
    for I := 0 to 4 do begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
    Dec(D, L[C and $3F]);
    Dec(C, L[B and $3F]);
    Dec(B, L[A and $3F]);
    Dec(A, L[D and $3F]);
    for I := 0 to 5 do begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
    Dec(D, L[C and $3F]);
    Dec(C, L[B and $3F]);
    Dec(B, L[A and $3F]);
    Dec(A, L[D and $3F]);
    for I := 0 to 4 do begin
      D := RORSUB16(D, (A and not C) + (B and C) + K^, 5); Dec(K);
      C := RORSUB16(C, (D and not B) + (A and B) + K^, 3); Dec(K);
      B := RORSUB16(B, (C and not A) + (D and A) + K^, 2); Dec(K);
      A := RORSUB16(A, (B and not D) + (C and D) + K^, 1); Dec(K);
    end;
  end;
end;

procedure TCipher_RC2.Init(const Key: TBytes; const IVector: PCardinal);
var
  I: Integer;
  K: PByteArray;
  KeyLength, T8, TM: Integer;
begin
  KeyLength := Length(Key);
  InitBegin(KeyLength);

  if FEffectiveKeySize = 0 then
    FEffectiveKeySize := KeyLength * 8;

  K := Pointer(User);
  Move(Key[0], K^, KeyLength);

  for I := KeyLength to 127 do
    K[I] := RC2_Data[byte(K[I - 1] + K[I - KeyLength])];

  T8 := (FEffectiveKeySize + 7) div 8;
  TM := 255 mod (1 shl (8 + FEffectiveKeySize - 8 * T8));
  K[128 - T8] := RC2_Data[K[128 - T8] and TM];

  for I := 127 - T8 downto 0 do
    K[I] := RC2_Data[K[I + 1] xor K[I + T8]];

  InitEnd(IVector);
end;

{ TCipher_RC4 }

const
  RC4_BlockSize = 32;

class procedure TCipher_RC4.GetContext(var BlockSize, UserSize: Integer; KeySizes: TKeySizes);
begin
  BlockSize := RC4_BlockSize;
  KeySizes.MinSize := 40;
  KeySizes.MaxSize := 2048;
  KeySizes.SkipSize := 8;
  UserSize := 256 * 2;
end;

procedure TCipher_RC4.Encode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
var
  D: PByteArray;
  B: PByte;
  X, S: Byte;
begin
  D := Pointer(User);
  B := Pointer(Data);
  Inc(PCardinal(B), Offset);

  if DataSize = 0 then
    DataSize := RC4_BlockSize;

  for X := 0 to DataSize - 1 do begin
    Inc(FI);

    S := D[FI];
    Inc(FJ, S);
    D[FI] := D[FJ];
    D[FJ] := S;
    B^ := B^ xor D[(D[FI] + S) and $FF];
    Inc(B);
  end;
end;

procedure TCipher_RC4.Decode(Data: PCardinal; Offset: Integer; DataSize: Integer = 0);
begin
  Encode(Data, Offset, DataSize);
end;

procedure TCipher_RC4.Init(const Key: TBytes; const IVector: PCardinal);
var
  D: TBytes;
  I, J, S: Integer;
  KeyLength: Integer;
begin
  KeyLength := Length(Key);
  InitBegin(KeyLength);
  FI := 0;
  FJ := 0;
  J := 0;
  SetLength(D, 256);

  for I := 0 to 255 do
    D[I] := I;

  for I := 0 to 255 do begin
    J := (J + D[I] + Key[I mod KeyLength]) and $FF;
    S := D[I];
    D[I] := D[J];
    D[J] := S;
  end;

  InitEnd(IVector);

  Buffer.BlockCopy(D, 0, User, 0, 256);
  Buffer.BlockCopy(D, 0, User, 256, 256);
  FSI := FI;
  FSJ := FJ;
end;

procedure TCipher_RC4.Done;
begin
  inherited Done;

  FI := FSI;
  FJ := FSJ;
  Buffer.BlockCopy(User, 256, User, 0, 256);
end;

function TCipher_RC4.Get_OutputBlockSize: Integer;
begin
  Result := 1;
end;

end.
