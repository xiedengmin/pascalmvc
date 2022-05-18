
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  DEC Util
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRDECUtil;

interface
{$IFNDEF FPC}
  {$A+,Q-,R-,Y-}
{$ELSE}
  {$A+,Q-,R-}
{$ENDIF}

uses
  SysUtils,
  CLRClasses, CRTypes, CRFunctions;

// Result := Value shl Shift or Value shr (32 - Shift)
function ROL(Value: Cardinal; Shift: Integer): Cardinal;
// XOR's buffers Data and XORBlock ByteSize bytes to Data
procedure XORBuffers(Data: PCardinal; Offset: Integer; const XORBlock: PCardinal; ByteSize: Integer);
// XOR Buffer Size Bytes with Seed Randoms,
// the initial State from Buffer have effect on the Output
function RndXORBuffer(Seed: Integer; const Buf: TValueArr; Size: Integer): Integer;
function RndXORBufferL(Seed: Integer; const Buf: TCardinalArray; Size: Integer): Integer;
function ReverseInt4(Value: Cardinal): Cardinal;
function SwapInteger(Value: Cardinal): Cardinal; register;
function SwapInt64(Value: UInt64): UInt64;
procedure SwapIntegerBuffer(const Source: TCardinalArray; const Dest: TCardinalArray; Count: Integer); register;
procedure SwapInt64Buffer(const Source: TUInt64Array; const Dest: TUInt64Array; Count: Integer);
// Compare Memory, D2 have no CompareMem, Result can be -1, 0, 1
function MemCompare(P1, P2: PByteArray; Size: Integer): Integer; register;
function CompareBuf(const Buf1, Buf2: TBytes; Size: Integer): Integer;
procedure ArrayReverse(const Arr: TBytes; const Offset: Integer; const Length: Integer);

function GetIntLE(const Src: TBytes; Offset: Integer): cardinal;
procedure PutIntLE(const Value: cardinal; var Dest: TBytes; Offset: Integer);
function GetIntBE(const Src: TBytes; Offset: Integer): cardinal;
procedure PutIntBE(const Value: cardinal; Dest: TValueArr; Offset: Integer);
function GetInt64BE(const Src: TBytes; Offset: Integer): Int64;
procedure PutInt64BE(const Value: Int64; Dest: TValueArr; Offset: Integer);

implementation

uses
{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$ENDIF}
  DAConsts;

function ROL(Value: Cardinal; Shift: Integer): Cardinal; {$IFDEF UseASM}assembler;{$ENDIF}
{$IFNDEF UseASM}
begin
  Result := (Value shl Shift) or (Value shr (32 - Shift));

{$ELSE}
asm
       MOV   ECX,EDX
       ROL   EAX,CL
{$ENDIF}
end;

function ReverseInt4(Value: Cardinal): Cardinal;
begin
  Value := ((Value and $0C) shr 2) or (Value shl 2);
  Result := ((Value and $0A) shr 1) or ((Value and $05) shl 1);
end;

function SwapInteger(Value: Cardinal): Cardinal; {$IFDEF UseASM}assembler; register;{$ENDIF}
{$IFNDEF UseASM}
begin
  Result :=
    ((Value and $ff) shl 24) or
    ((Value and $ff00) shl 8) or
    ((Value and $ff0000) shr 8) or
    ((Value and $ff000000) shr 24);
{$ELSE}
asm
       BSWAP  EAX
{$ENDIF}
end;

function SwapInt64(Value: UInt64): UInt64;
begin
  Result := (UInt64(SwapInteger(Cardinal(Value and $ffffffff))) shl 32) or
             UInt64(SwapInteger(Cardinal(Value shr 32)));
end;

procedure SwapIntegerBuffer(const Source: TCardinalArray; const Dest: TCardinalArray; Count: Integer); {$IFDEF UseASM}assembler; register;{$ENDIF}
{$IFNDEF UseASM}
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dest[i] := SwapInteger(Source[i]);

{$ELSE}
asm
       TEST   ECX,ECX
       JLE    @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       BSWAP  EBX
       MOV    [EDX + ECX * 4],EBX
       DEC    ECX
       JNZ    @@1
       POP    EBX
@Exit:
{$ENDIF}
end;

procedure SwapInt64Buffer(const Source: TUInt64Array; const Dest: TUInt64Array; Count: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dest[i] := SwapInt64(Source[i]);
end;

procedure XORBuffers(Data: PCardinal; Offset: Integer; const XORBlock: PCardinal; ByteSize: Integer); {$IFDEF UseASM}assembler;{$ENDIF}
{$IFNDEF UseASM}
var
  LenInt, LenBytes: Integer;
  i: Integer;
begin
  if Data = nil then
    Exit;

  LenInt := ByteSize shr 2;
  LenBytes := ByteSize and $3;
  for i := 0 to LenInt - 1 do begin
    TCardinalArray(Data)[Offset] := TCardinalArray(Data)[Offset] xor TCardinalArray(XORBlock)[i];
    Inc(Offset);
  end;

  Offset := Offset shl 2;
  for i := 0 to LenBytes - 1 do
    PByteArray(Data)[Offset + i] := PByteArray(Data)[Offset + i] xor PByteArray(XORBlock)[LenInt shl 2 + i];
{$ELSE}
asm
       PUSH  ESI
       PUSH  EBX
       MOV   EBX,ByteSize

       AND   EBX,EBX
       JZ    @@4
       SHL   EDX,2
       ADD   EAX,EDX
       MOV   ESI,EAX
@@1:   TEST  EBX,3
       JNZ   @@3
@@2:   SUB   EBX,4
       JL    @@4
       MOV   EAX,[ESI + EBX]
       XOR   EAX,[ECX + EBX]
       MOV   [ESI + EBX],EAX
       JMP   @@2
@@3:   DEC   EBX
       MOV   AL,[ESI + EBX]
       XOR   AL,[ECX + EBX]
       MOV   [ESI + EBX],AL
       JMP   @@1
@@4:   POP   EBX
       POP   ESI
{$ENDIF}
end;

function RndXORBuffer(Seed: Integer; const Buf: TValueArr; Size: Integer): Integer; {$IFDEF UseASM}assembler;{$ENDIF}
{$IFNDEF UseASM}
var
  i: Integer;
begin
  Result := Seed;
  if Buf = nil then
    Exit;

  for i := 0 to Size - 1 do begin
    Result := (Result xor byte(Buf[i])) * 134775813;
    Inc(Result);
    PByte(Buf)[i] := byte(Result shr 24);
  end;

{$ELSE}
asm
      AND     EDX,EDX
      JZ      @@2
      AND     ECX,ECX
      JLE     @@2
      PUSH    EBX
@@1:  XOR     AL,[EDX]
      IMUL    EAX,EAX,134775813
      INC     EAX
      MOV     EBX,EAX
      SHR     EBX,24
      MOV     [EDX],BL
      INC     EDX
      DEC     ECX
      JNZ     @@1
      POP     EBX
@@2:
{$ENDIF}
end;

function RndXORBufferL(Seed: Integer; const Buf: TCardinalArray; Size: Integer): Integer; {$IFDEF UseASM}assembler;{$ENDIF}
begin
  Result := RndXORBuffer(Seed, Pointer(Buf), Size);
end;

function MemCompare(P1, P2: PByteArray; Size: Integer): Integer; {$IFDEF UseASM}assembler; register;{$ENDIF}
{$IFNDEF UseASM}
var
  i: Integer;
begin
  for i := 0 to Size - 1 do
    if P1[i] <> P2[i] then begin
      Result := Integer(P2[i] - P1[i]);
      Exit;
    end;

  Result := 0;
{$ELSE}
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI,P1
       MOV     EDI,P2
       XOR     EAX,EAX
       REPE    CMPSB
       JE      @@1
       MOVZX   EAX,BYTE PTR [ESI-1]
       MOVZX   EDX,BYTE PTR [EDI-1]
       SUB     EAX,EDX
@@1:   POP     EDI
       POP     ESI
{$ENDIF}
end;

function CompareBuf(const Buf1, Buf2: TBytes; Size: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to Size - 1 do
    if Buf1[i] <> Buf2[i] then begin
      Result := Integer(Buf2[i] - Buf1[i]);
      Exit;
    end;

  Result := 0;
end;

procedure ArrayReverse(const Arr: TBytes; const Offset: Integer; const Length: Integer);
var
  i, j: Integer;
  t: Byte;
begin
  j := Length + Offset - 1;
  for i := Offset to Length shr 1 + Offset - 1 do begin
    t := Arr[i];
    Arr[i] := Arr[j];
    Arr[j] := t;
    Dec(j);
  end;
end;

function GetIntLE(const Src: TBytes; Offset: Integer): cardinal;
begin
  Result := PCardinal(@Src[Offset])^;
//  Result := ((cardinal(Src[Offset])) or
//             (cardinal(Src[Offset + 1]) shl 8) or
//             (cardinal(Src[Offset + 2]) shl 16) or
//             (cardinal(Src[Offset + 3]) shl 24));
end;

procedure PutIntLE(const Value: cardinal; var Dest: TBytes; Offset: Integer);
begin
  PCardinal(@Dest[Offset])^ := Value;
//  Dest[Offset] := byte(Value);
//  Dest[Offset + 1] := byte(Value shr 8);
//  Dest[Offset + 2] := byte(Value shr 16);
//  Dest[Offset + 3] := byte(Value shr 24);
end;

function GetIntBE(const Src: TBytes; Offset: Integer): cardinal;
begin
  Result := ((cardinal(Src[Offset]) shl 24) or
             (cardinal(Src[Offset + 1]) shl 16) or
             (cardinal(Src[Offset + 2]) shl 8) or
             (cardinal(Src[Offset + 3])));
end;

procedure PutIntBE(const Value: cardinal; Dest: TValueArr; Offset: Integer);
begin
  PByteArray(Dest)[Offset] := byte(Value shr 24);
  PByteArray(Dest)[Offset + 1] := byte(Value shr 16);
  PByteArray(Dest)[Offset + 2] := byte(Value shr 8);
  PByteArray(Dest)[Offset + 3] := byte(Value);
end;

function GetInt64BE(const Src: TBytes; Offset: Integer): Int64;
begin
  Result := ((Int64(Src[Offset]) shl 56) or
             (Int64(Src[Offset + 1]) shl 48) or
             (Int64(Src[Offset + 2]) shl 40) or
             (Int64(Src[Offset + 3]) shl 32) or
             (Int64(Src[Offset + 4]) shl 24) or
             (Int64(Src[Offset + 5]) shl 16) or
             (Int64(Src[Offset + 6]) shl 8) or
             (Int64(Src[Offset + 7])));
end;

procedure PutInt64BE(const Value: Int64; Dest: TValueArr; Offset: Integer);
begin
  PByteArray(Dest)[Offset] := byte(Value shr 56);
  PByteArray(Dest)[Offset + 1] := byte(Value shr 48);
  PByteArray(Dest)[Offset + 2] := byte(Value shr 40);
  PByteArray(Dest)[Offset + 3] := byte(Value shr 32);
  PByteArray(Dest)[Offset + 4] := byte(Value shr 24);
  PByteArray(Dest)[Offset + 5] := byte(Value shr 16);
  PByteArray(Dest)[Offset + 6] := byte(Value shr 8);
  PByteArray(Dest)[Offset + 7] := byte(Value);
end;

end.
