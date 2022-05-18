
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyRSAKeyUni;

interface

uses
  Classes, SysUtils, StrUtils,
  CLRClasses, CRTypes, CRFunctions, CRBigInteger, CRBase64,
  CRDecUtil, CRHashAlgorithm, CRHash;

type
  TDataReader = class
  private
    FData: TBytes;
    FOffset: Integer;
    FCount: Integer;

  public
    constructor Create(const Image: TBytes);
    function ReadInt16: Word;
    function ReadByte: Byte;
    function PeekByte: Byte;
    function Read(Count: Integer): TBytes;
  end;

  TPaddingMode = (pmPKCS2, pmOAEP);
  THashAlgorithmType = (haNone, haSHA1, haSHA2_256, haSHA2_512, haSHA2_224, haSHA2_384, haMD5, haMD4, haMD2);

  TRSAKey = class
  private
    FHashAlgorithm: THashAlgorithmType;
    FMaskGenHashAlgorithm: THashAlgorithmType;
    FEmptyStrHash: TBytes;

    FRSAPublicExponent: TBigInteger;
    FRSAPublicModulus: TBigInteger;

    function GetEmptyStrHash: TBytes;
    procedure RaiseError;
    procedure DecodeKey(const KeyData: TBytes);
    function EncodeWithPad(const Input: TBytes; Padding: TPaddingMode): TBytes;
    function RSAPublicModPow(const Data: TBytes): TBytes;
    class function MGF1(const Seed: TBytes; MaskLen: integer; HashAlg: THashAlgorithmType): TBytes;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ImportFrom(KeyStrings: TStringList);
    function Encrypt(const Data: TBytes; Padding: TPaddingMode): TBytes;
  end;

implementation

resourcestring
  SUnexpectedEOP = 'Unexpected end of data packet';
  SBrokenKey = 'Key is broken';
  SErrorEncryptingData = 'An error occurred when encrypting data: Message too long';
  SInvalidInputArgs = 'Invalid input arguments';

{ TDataReader }

constructor TDataReader.Create(const Image: TBytes);
begin
  inherited Create;
  FData := Image;
  FCount := Length(FData);
  FOffset := 0;
end;

function TDataReader.ReadInt16: Word;
begin
  if (FOffset + 1) >= FCount then
    raise Exception.Create(SUnexpectedEOP);

  Result := (Word(FData[FOffset]) shl 8) +
            FData[FOffset + 1];
  FOffset := FOffset + 2;
end;

function TDataReader.PeekByte: Byte;
begin
  if FOffset >= FCount then
    raise Exception.Create(SUnexpectedEOP);

  Result := FData[FOffset];
end;

function TDataReader.ReadByte: Byte;
begin
  if FOffset >= FCount then
    raise Exception.Create(SUnexpectedEOP);

  Result := FData[FOffset];
  Inc(FOffset);
end;

function TDataReader.Read(Count: Integer): TBytes;
begin
  if ((FOffset + Count - 1) >= FCount) or (Count < 0) then
    raise Exception.Create(SUnexpectedEOP);

  SetLength(Result, Count);
  if Count > 0 then
    Buffer.BlockCopy(FData, FOffset, Result, 0, Count);
  FOffset := FOffset + Count;
end;

{ TRSAKey }

constructor TRSAKey.Create;
begin
  inherited Create;

  FHashAlgorithm := haSHA1;
  FMaskGenHashAlgorithm := haSHA1;
end;

destructor TRSAKey.Destroy;
begin
  FreeAndNil(FRSAPublicExponent);
  FreeAndNil(FRSAPublicModulus);

  inherited;
end;

procedure TRSAKey.RaiseError;
begin
  raise Exception.Create(SBrokenKey);
end;

function TRSAKey.GetEmptyStrHash: TBytes;
var
  csp: THashAlgorithm;
begin
  if FEmptyStrHash = nil then begin
    csp := THash_SHA1.Create;
    try
      FEmptyStrHash := csp.ComputeHash(nil);
    finally
      csp.Free;
    end;
  end;

  Result := FEmptyStrHash;
end;

procedure TRSAKey.DecodeKey(const KeyData: TBytes);
const
  SeqOID_RSA: array[0..14] of byte = (
    $30, $0D, $06, $09, $2A, $86, $48, $86, $F7, $0D, $01, $01, $01, $05, $00);

  procedure GetParamSize(Reader: TDataReader; out Size: Integer);
  var
    bt: byte;
  begin
    bt := Reader.ReadByte;
    if bt <> $02 then begin   //Expect integer
      if bt = $04 then
        Reader.ReadByte
      else
        RaiseError;

      Size := 0;
      Exit;
    end;

    bt := Reader.ReadByte;
    if bt = $81 then begin
      Size := Reader.ReadByte; // Data Size in next byte
    end else
    if bt = $82 then begin
      Size := Reader.ReadInt16; // Data Size in next 2 bytes
    end
    else
      Size := bt; // we already have the Data Size

    while Reader.PeekByte = $0 do begin
      //remove high Order zeros in Data
      Reader.ReadByte;
      Size := Size - 1;
    end;
  end;

var
  Reader: TDataReader;
  twoBytes: Word;
  bt: byte;
  Elems: Integer;
  seq: TBytes;
  i: integer;
begin
  Reader := TDataReader.Create(KeyData);
  try
    SetLength(seq, 0);
    twoBytes := Reader.ReadInt16;
    if twoBytes = $3081 then  // Data read as little endian Order (actual Data Order for Sequence is 30 81)
      Reader.ReadByte
    else
    if twoBytes = $3082 then
      Reader.ReadInt16
    else
      RaiseError;

    seq := Reader.Read(15); // read the Sequence OID
    for i := 0 to 14 do begin
      if seq[i] <> SeqOID_RSA[i] then
        RaiseError;
    end;

    Reader.ReadByte;
    bt := Reader.ReadByte;  // read next byte, or next 2 bytes is  $81 or $82; otherwise bt is the byte count
    if bt = $81 then
      Reader.ReadByte
    else
    if bt = $82 then
      Reader.ReadInt16
    else
      RaiseError;

    while Reader.PeekByte = $0 do
      Reader.ReadByte;

    twoBytes := Reader.ReadInt16;
    if twoBytes = $3081 then // Data read as little endian Order (actual Data Order for Sequence is 30 81)
      Reader.ReadByte //advance 1 byte
    else
    if twoBytes = $3082 then
      Reader.ReadInt16 //advance 2 bytes
    else
      RaiseError;

    // all private Key components are Integer sequences
    GetParamSize(Reader, Elems);
    FRSAPublicModulus := TBigInteger.Create(Reader.Read(Elems));
    GetParamSize(Reader, Elems);
    FRSAPublicExponent := TBigInteger.Create(Reader.Read(Elems));
  finally
    Reader.Free;
  end;
end;

procedure TRSAKey.ImportFrom(KeyStrings: TStringList);
const
  PKCS1_PUB_HEADER = '-----BEGIN PUBLIC KEY-----';
  PKCS1_PUB_FOOTER = '-----END PUBLIC KEY-----';
var
  sb: StringBuilder;
  KeyData: TBytes;
  i: integer;
begin
  if (KeyStrings[0] <> PKCS1_PUB_HEADER) or (KeyStrings[KeyStrings.Count - 1] <> PKCS1_PUB_FOOTER) then
    RaiseError;

  sb := StringBuilder.Create;
  try
    for i := 1 to KeyStrings.Count - 2 do
      sb.Append(Trim(KeyStrings[i]));

    KeyData := TBase64.Decode(Encoding.ASCII.GetBytes(sb.ToString));
  finally
    sb.Free;
  end;

  DecodeKey(KeyData);
end;

function TRSAKey.Encrypt(const Data: TBytes; Padding: TPaddingMode): TBytes;
var
  PaddedData: TBytes;
begin
  PaddedData := EncodeWithPad(Data, Padding);
  Result := RSAPublicModPow(PaddedData);
end;

class function TRSAKey.MGF1(const Seed: TBytes; MaskLen: integer; HashAlg: THashAlgorithmType): TBytes;
var
  csp: THashAlgorithm;
  ResLen, HashLen, SeedLen, Counter: integer;
  Buf, Hash: TBytes;
begin
  Result := nil;

  csp := THash_SHA1.Create;
  try
    SeedLen := Length(Seed);
    SetLength(Buf, SeedLen + 4);
    Move(Seed[0], Buf[0], SeedLen);
    SetLength(Hash, 0);
    HashLen := csp.HashSize;

    ResLen := 0;
    Counter := 0;
    while ResLen < MaskLen do begin
      PutIntBE(Counter, TValueArr(Buf), SeedLen);
      Hash := csp.ComputeHash(Buf);
      if Result = nil then
        Result := Hash
      else begin
        SetLength(Result, ResLen + HashLen);
        Move(Hash[0], Result[ResLen], HashLen);
      end;
      Inc(ResLen, HashLen);
      Inc(Counter);
    end;

    SetLength(Result, MaskLen);
  finally
    csp.Free;
  end;
end;

function TRSAKey.EncodeWithPad(const Input: TBytes; Padding: TPaddingMode): TBytes;
var
  MLen, PaddingLen: integer;
  KeySize, HashLen, DBLen: integer;
  EmptyStrHash: TBytes;
  MaskedSeed, MaskedDB: TBytes;
  SeedMask, DBMask, Seed, DB: TBytes;
  i: integer;
begin
  Randomize;
  KeySize := (FRSAPublicModulus.BitCount + 7) shr 3;

  case Padding of
    pmPKCS2: begin
      MLen := Length(Input);
      PaddingLen := KeySize - MLen;
      if PaddingLen < 11 then
        raise Exception.Create(SErrorEncryptingData);

      SetLength(Result, KeySize);
      Result[0] := 0;
      Result[1] := 2;
      Result[PaddingLen - 1] := 0;
      Move(Input[0], Result[PaddingLen], MLen);

      for i := 0 to PaddingLen - 4 do begin
        while Result[2 + i] = 0 do
          Result[2 + i] := Random(255);
      end;
    end;

    pmOAEP: begin
      MLen := Length(Input);
      PaddingLen := KeySize - MLen;
      if PaddingLen < (2 + 2 * 20{CipherFactory.GetHashSize(HashAlgorithm)}) then
        raise Exception.Create(SErrorEncryptingData);

      SetLength(EmptyStrHash, 0);
      EmptyStrHash := GetEmptyStrHash;
      HashLen := Length(EmptyStrHash);

      DBLen := KeySize - HashLen - 1;
      SetLength(DB, DBLen);
      Move(EmptyStrHash[0], DB[0], HashLen);
      FillChar(DB[HashLen], DBLen - MLen - HashLen - 1, 0);
      DB[DBLen - MLen - 1] := 1;
      Move(Input[0], DB[DBLen - MLen], MLen);

      SetLength(Seed, HashLen);
      for i := 0 to HashLen - 1 do
        Seed[i] := Random(255);

      SetLength(DBMask, 0);
      DBMask := MGF1(Seed, DBLen, FMaskGenHashAlgorithm);

      SetLength(MaskedDB, DBLen);
      for i := 0 to DBLen - 1 do
        MaskedDB[i] := DB[i] xor DBMask[i];

      SetLength(SeedMask, 0);
      SeedMask := MGF1(MaskedDB, HashLen, FMaskGenHashAlgorithm);

      SetLength(MaskedSeed, HashLen);
      for i := 0 to HashLen - 1 do
        MaskedSeed[i] := Seed[i] xor SeedMask[i];

      SetLength(Result, KeySize);
      Result[0] := 0;
      Move(MaskedSeed[0], Result[1], HashLen);
      Move(MaskedDB[0], Result[1 + HashLen], DBLen);
    end;
  else
    raise Exception.Create(SInvalidInputArgs);
  end;
end;

function TRSAKey.RSAPublicModPow(const Data: TBytes): TBytes;
var
  Input, Res: TBigInteger;
begin
  Res := nil;
  Input := TBigInteger.Create(Data);
  try
    Res := Input.ModPow(FRSAPublicExponent, FRSAPublicModulus);
    Result := Res.GetBytes((FRSAPublicModulus.BitCount + 7) shr 3);
  finally
    Input.Free;
    Res.Free;
  end;
end;

end.

