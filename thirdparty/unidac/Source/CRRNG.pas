//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  Linear Feedback Shift Register (LFSR)
//  Random Number Generator with variable Period
//  from 2^32 -1 to 2^2032 -1, Standard is 2^128 -1
//  with .Seed('', -1) absolutly random
//  The Period have theoretical no effect on the Speed.
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRRNG;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  CRFunctions,
{$ENDIF}
  Classes, SyncObjs, SysUtils,
  CLRClasses, CRTypes, CRDECUtil, CRSymmetricAlgorithm;

type
  IScRandom = interface
    procedure Randomize(const Seed: TBytes);
    procedure Random(const buf: TBytes; const Offset, Count: integer); overload;
    procedure Random(buf: TValueArr; Count: integer); overload;
  end;

  TScRandom = class(TInterfacedObject, IScRandom)    // Basicly RNG, equal to Borland's Random()
  private
    FRegister: Integer;
    FProtection: TSymmetricAlgorithm;
  protected
// Count of Bytes that Int() or Buffer() has generated
    FCount: Integer;          // not as private Fields, easier access for descends
// the Size in Bits
    FSize: Integer;
    FBasicSeed: Integer;
    FLock: TCriticalSection;

    procedure SetSize(Value: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Randomize(const Seed: TBytes; const Offset, Count: Integer); overload; virtual;
    procedure Randomize(const Seed: TBytes); overload;
    procedure Randomize(Seed: TStream); overload;

    procedure Random(const buf: TBytes; const Offset, Count: integer); overload;
    procedure Random(buf: TValueArr; Count: integer); overload; virtual;
  end;

  TScRandomLFSR = class(TScRandom)   // Linear Feedback Shift Register
  private
    FPtr: Integer;                    // Current Position in FRegister
    FLast: Integer;                   // Highest Position in FRegister
    FTable: array[0..255] of Word;    // Lookup Table for FRegister
    FRegister: array[0..255] of Byte; // Linear Feedback Shift Register
    FFunc: procedure(Self: TObject; Buffer: TValueArr; Size: Integer);
  protected
    procedure SetSize(Value: Integer); override;
  public
    procedure Randomize(const Seed: TBytes; const Offset, Count: Integer); override;
    procedure Random(buf: TValueArr; Count: integer); override;
  end;

{   Follow the used polynomial's for TScRandomLFSR
     size in bytes of register, XORCode, Polynomial, Period

   4, $F5, x^32   + x^7 + x^5 + x^3 + x^2 + x + 1,   2^32   -1
   5, $9C, x^40   + x^5 + x^4 + x^3 + 1,             2^40   -1
   6, $ED, x^48   + x^7 + x^5 + x^4 + x^2 + x + 1,   2^48   -1
   7, $A9, x^56   + x^7 + x^4 + x^2 + 1,             2^56   -1
   8, $D8, x^64   + x^4 + x^3 + x + 1,               2^64   -1
   9, $FA, x^72   + x^6 + x^4 + x^3 + x^2 + x + 1,   2^72   -1
  10, $F5, x^80   + x^7 + x^5 + x^3 + x^2 + x + 1,   2^80   -1
  12, $BB, x^96   + x^7 + x^6 + x^4 + x^3 + x^2 + 1, 2^96   -1
  15, $E7, x^120  + x^7 + x^6 + x^5 + x^2 + x + 1,   2^120  -1
  16, $E1, x^128  + x^7 + x^2 + x + 1,               2^128  -1
  18, $A9, x^144  + x^7 + x^4 + x^2 + 1,             2^144  -1
  19, $B2, x^152  + x^6 + x^3 + x^2 + 1,             2^152  -1
  20, $B4, x^160  + x^5 + x^3 + x^2 + 1,             2^160  -1
  22, $BD, x^176  + x^7 + x^5 + x^4 + x^3 + x^2 + 1, 2^176  -1
  25, $B4, x^200  + x^5 + x^3 + x^2 + 1,             2^200  -1
  27, $D1, x^216  + x^7 + x^3 + x + 1,               2^216  -1
  38, $FC, x^304  + x^5 + x^4 + x^3 + x^2 + x + 1,   2^304  -1
  40, $D8, x^320  + x^4 + x^3 + x + 1,               2^320  -1
  42, $C9, x^336  + x^7 + x^4 + x + 1,               2^336  -1
  44, $BD, x^352  + x^7 + x^5 + x^4 + x^3 + x^2 + 1, 2^352  -1
  50, $B4, x^400  + x^5 + x^3 + x^2 + 1,             2^400  -1
  51, $FA, x^408  + x^6 + x^4 + x^3 + x^2 + x + 1,   2^408  -1
  55, $D8, x^440  + x^4 + x^3 + x + 1,               2^440  -1
  60, $BB, x^480  + x^7 + x^6 + x^4 + x^3 + x^2 + 1, 2^480  -1
  61, $D8, x^488  + x^4 + x^3 + x + 1,               2^488  -1
  63, $FA, x^504  + x^6 + x^4 + x^3 + x^2 + x + 1,   2^504  -1
  67, $95, x^536  + x^7 + x^5 + x^3 + 1,             2^536  -1
  84, $F6, x^672  + x^6 + x^5 + x^3 + x^2 + x + 1,   2^672  -1
  89, $9C, x^712  + x^5 + x^4 + x^3 + 1,             2^712  -1
  91, $B8, x^728  + x^4 + x^3 + x^2 + 1,             2^728  -1
 103, $FC, x^824  + x^5 + x^4 + x^3 + x^2 + x + 1,   2^824  -1
 141, $D1, x^1128 + x^7 + x^3 + x + 1,               2^1128 -1
 154, $F3, x^1232 + x^7 + x^6 + x^3 + x^2 + x + 1,   2^1232 -1
 254, $A3, x^2032 + x^7 + x^6 + x^2 + 1,             2^2032 -1

  follow various Periods
--------------------------------------------------------------------------------
  2^32-1   = 4294967295
  2^64-1   = 18446744073709551615
  2^128-1  = 340282366920938463463374607431768211455
  2^2032-1 = it's one Number
   49311837877366649323600580884811328064642490645928167773636391338386009428204
   17921935608125537553934278674005267623599165972833122328326583112816221076703
   35702985799671951234310153163915857728680359766210694390385082889078409114931
   66867209378778336289339669574030006474132653643098550122997363890264786354861
   31947843882498538312526670313197249581325688984118966381501107686008635362008
   71492771279798342546336760614070411100118371556871830774626226863061725361438
   46476937385117828689155818331492509954024778049592066494651864619855274961300
   9880449926596639031121858756000207590413184793166384097191709192063287295
--------------------------------------------------------------------------------
}

implementation

uses
  CRCipher;

const
// avaible Periods for the LFSR
  LFSRPeriod: array[0..33, 0..1] of Word =
   ((   32, $F5), (   40, $9C), (   48, $ED), (   56, $A9),
    (   64, $D8), (   72, $FA), (   80, $F5), (   96, $BB),
    (  120, $E7), (  128, $E1), (  144, $A9), (  152, $B2),
    (  160, $B4), (  176, $BD), (  200, $B4), (  216, $D1),
    (  304, $FC), (  320, $D8), (  336, $C9), (  352, $BD),
    (  400, $B4), (  408, $FA), (  440, $D8), (  480, $BB),
    (  488, $D8), (  504, $FA), (  536, $95), (  672, $F6),
    (  712, $9C), (  728, $B8), (  824, $FC), ( 1128, $D1),
    ( 1232, $F3), ( 2032, $A3));

// internal used for the random initialization of the Seed Initial Value
// change this to produce Application dependent Randomness
  DefaultSeed: Integer = 436375501;

constructor TScRandom.Create;
begin
  inherited;
  FProtection := TCipher_Blowfish.Create;
  TCipher_Blowfish(FProtection).GenerateKey;
  TCipher_Blowfish(FProtection).GenerateIV;

  FLock := TCriticalSection.Create;
  FBasicSeed := DefaultSeed;
  SetSize(0);
  Randomize(nil, 0, -1);
end;

destructor TScRandom.Destroy;
begin
  Randomize(nil, 0, 0);

{$IFNDEF NEXTGEN}
  FProtection.Free;
{$ENDIF}
  FLock.Free;
  inherited;
end;

procedure TScRandom.SetSize(Value: Integer);
begin
  FSize := 32;
end;

procedure TScRandom.Randomize(const Seed: TBytes; const Offset, Count: Integer);
var
  I: Integer;
begin
  FLock.Acquire;
  try
    if (Count > 0) and (Seed <> nil) then begin
      FRegister := 0;
      for I := 0 to Count - 1 do
        FRegister := FRegister + Integer(Seed[Offset + I] shl ((I and 3) * 8));
    end
    else
      if Count < 0 then
        FRegister := Integer({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF}) + (FCount + 1)
      else
        FRegister := FBasicSeed;

    if FProtection <> nil then
      FProtection.CodeBuffer(@FRegister, SizeOf(FRegister));
  finally
    FLock.Release;
  end;
end;

procedure TScRandom.Randomize(const Seed: TBytes);
begin
  Randomize(Seed, 0, Length(Seed));
end;

procedure TScRandom.Randomize(Seed: TStream);
var
  buf: TBytes;
  sz: integer;
begin
  Seed.Position := 0;
  sz := Seed.Size;
  SetLength(buf, sz);
  Seed.ReadBuffer(buf[0], sz);
  Randomize(buf, 0, Length(buf));
end;

procedure TScRandom.Random(const buf: TBytes; const Offset, Count: integer);
begin
  Random(@buf[Offset], Count);
end;

procedure TScRandom.Random(buf: TValueArr; Count: integer);
begin
  if Count <= 0 then
    Exit;

  FillChar(buf^, Count, 0);

  FLock.Acquire;
  try
    FRegister := RndXORBuffer(FRegister, buf, Count);
    Inc(FCount, Count);
    if FProtection <> nil then
      FProtection.CodeBuffer(buf, Count);
  finally
    FLock.Release;
  end;
end;

{$IFDEF UseASM}
// internal for TScRandomLFSR
procedure LFSRBuf(Self: TObject; Buffer: TValueArr; Size: Integer); assembler;
asm
      AND     EDX,EDX    // Buffer = nil ?
      JZ      @@9
      AND     ECX,ECX    // BufferSize <= 0 ?
      JLE     @@9

      PUSH    EDI
      PUSH    ESI
      PUSH    EBX
      PUSH    EBP
      PUSH    EAX

      MOV     EDI,[EAX].TScRandomLFSR.FPtr
      MOV     EBP,[EAX].TScRandomLFSR.FLast
      LEA     ESI,[EAX].TScRandomLFSR.FRegister
      LEA     EBX,[EAX].TScRandomLFSR.FTable
      DEC     EDX

@@1:  MOVZX   EAX,Byte Ptr [ESI + EDI]
      MOV     [EDX + ECX],AL
      MOV     AX,[EBX + EAX * 2]
      MOV     [ESI + EDI],AL
      DEC     EDI
      JS      @@2
      XOR     [ESI + EDI],AH
      ADD     EDI,2
      CMP     EDI,EBP
      JLE     @@3
      XOR     EDI,EDI
      JMP     @@3
@@2:  MOV     EDI,EBP
      XOR     [ESI + EDI],AH
      MOV     EDI,1
@@3:  DEC     ECX
      JNZ     @@1

      POP     EAX
      MOV     [EAX].TScRandomLFSR.FPtr,EDI

      POP     EBP
      POP     EBX
      POP     ESI
      POP     EDI
@@9:
end;
{$ENDIF}

procedure LFSRBuf128(Self: TObject; Buffer: TValueArr; Size: Integer); {$IFDEF UseASM}assembler;{$ENDIF}
{$IFNDEF UseASM}
var
  Rnd: Word;
  Reg: Byte;
  Ptr: Integer;
  i: Integer;
begin
  if Buffer = nil then
    Exit;

  Ptr := TScRandomLFSR(Self).FPtr;
  for i := Size - 1 downto 0 do begin
    Reg := TScRandomLFSR(Self).FRegister[Ptr];
    PByteArray(Buffer)[i] := Reg;
    Rnd := TScRandomLFSR(Self).FTable[Reg];
    TScRandomLFSR(Self).FRegister[Ptr] := Byte(Rnd);
    Dec(Ptr);
    Ptr := Ptr and $0f;
    TScRandomLFSR(Self).FRegister[Ptr] := TScRandomLFSR(Self).FRegister[Ptr] xor (Rnd shr 8);
    Ptr := (Ptr + 2) and $0f;
  end;
  TScRandomLFSR(Self).FPtr := Ptr;

{$ELSE}
asm
      AND     EDX,EDX    // Buffer = nil ?
      JZ      @@9
      AND     ECX,ECX    // BufferSize <= 0 ?
      JLE     @@9

      PUSH    EDI
      PUSH    ESI
      PUSH    EBX
      PUSH    EBP
      PUSH    EAX

      MOV     EDI,[EAX].TScRandomLFSR.FPtr
      LEA     EBP,[EAX].TScRandomLFSR.FTable
      LEA     ESI,[EAX].TScRandomLFSR.FRegister
      DEC     EDX
      XOR     EAX,EAX

@@1:  MOV     AL,[ESI + EDI]
      MOV     BX,[EBP + EAX * 2]
      MOV     [EDX + ECX],AL
      MOV     [ESI + EDI],BL
      DEC     EDI
      AND     EDI,0Fh
      XOR     [ESI + EDI],BH
      ADD     EDI,2
      AND     EDI,0Fh
      DEC     ECX
      JNZ     @@1

      POP     EAX
      MOV     [EAX].TScRandomLFSR.FPtr,EDI

      POP     EBP
      POP     EBX
      POP     ESI
      POP     EDI
@@9:
{$ENDIF}
end;

procedure TScRandomLFSR.SetSize(Value: Integer);

  procedure CalcLFSRTable(XORCode: Byte);
  var
    I,J,Z: Integer;
    {$IFNDEF UseASM}n: Byte;{$ENDIF}
  begin
  {$IFDEF UseASM}
    asm // Reverse the bitorder
      XOR   AX,AX
      MOV   AL,XORCode
      MOV   CL,8
@@1:  RCR   AL,1
      RCL   AH,1
      DEC   CL
      JNZ   @@1
      MOV   XORCode,AH
    end;

  {$ELSE}
    n := 0;
    for I := 1 to 8 do begin
      n := n shl $1;
      n := n or (XORCode and $1);
      XORCode := XORCode shr $1;
    end;
    XORCode := n;
  {$ENDIF}

    FillChar(FTable, SizeOf(FTable), 0);

    for I := 0 to 255 do begin
      Z := I;
      for J := 0 to 7 do begin
        FTable[I] := FTable[I] shl 1;
        if Z and $80 <> 0 then FTable[I] := FTable[I] xor XORCode;
        Z := Z shl 1;
      end;
    end;
  end;

  procedure DoSet(Index: Integer);
  begin
    FSize := LFSRPeriod[Index, 0];
    FLast := LFSRPeriod[Index, 0] shr 3 - 1;
  {$IFDEF UseASM}
    if FSize = 128 then
      FFunc := LFSRBuf128
    else
      FFunc := LFSRBuf;
  {$ELSE}
    FFunc := LFSRBuf128;
  {$ENDIF}

    CalcLFSRTable(LFSRPeriod[Index, 1]);
    Randomize(nil, 0, 0);
  end;

var
  I: Integer;
begin
//  if Value <= 0 then
  Value := 128;

  if Value <> FSize then begin
    for I := 33 downto 0 do
      if Value >= LFSRPeriod[I, 0] then begin
        DoSet(I);
        Exit;
      end;

    DoSet(9);  // The Standard fast 2^128-1 Period
  end;
end;

procedure TScRandomLFSR.Randomize(const Seed: TBytes; const Offset, Count: Integer);
var
  I, S: Integer;
begin
  FLock.Acquire;
  try
    FPtr := 0;
    if (Count > 0) and (Seed <> nil) then begin
      FillChar(FRegister, SizeOf(FRegister), 0);

      S := FSize div 8;
      for I := 0 to Count - 1 do
        FRegister[I mod S] := FRegister[I mod S] + Seed[Offset + I];
    end
    else
      if Count < 0 then
        RndXORBuffer(Integer({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF}) + (FCount + 1), @FRegister, Length(FRegister))
      else
        FillChar(FRegister, SizeOf(FRegister), 0);

    RndXORBuffer(FBasicSeed, @FRegister, Length(FRegister));
    if FProtection <> nil then
      FProtection.CodeBuffer(@FRegister, Length(FRegister));
  finally
    FLock.Release;
  end;
end;

procedure TScRandomLFSR.Random(buf: TValueArr; Count: integer);
begin
  if Count <= 0 then
    Exit;

  FLock.Acquire;
  try
    FFunc(Self, buf, Count);
    if FProtection <> nil then
      FProtection.CodeBuffer(buf, Count);
    Inc(FCount, Count);
  finally
    FLock.Release;
  end;
end;

end.
