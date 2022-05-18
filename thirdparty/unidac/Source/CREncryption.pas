//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR Encryption
//////////////////////////////////////////////////

{$I Dac.inc}

unit CREncryption;

interface

uses
  SysUtils, Classes, Variants,
  CLRClasses, CRTypes, CRFunctions,
  CRSymmetricAlgorithm, CRHashAlgorithm, CRRNG;

type
  TCRDecryptedDataType = (ddtDecrypted, ddtNonEncrypted, ddtError);
  TBytesEncryptionMethod = function (const Data: TValueArr; var Length: cardinal): TBytes of object;
  TEncryptionMethod = function (const Data: TValueArr; DataType: Word; var Length: cardinal): Variant of object;
  TDecryptionMethod = procedure (Data: IntPtr; DataType: Word; var Length: cardinal; out DecryptedDataType: TCRDecryptedDataType) of object;

  TCREncryptionAlgorithm =
    (eaTripleDES, eaBlowfish, eaAES128, eaAES192, eaAES256, eaCast128, eaRC4);
  TCRHashAlgorithm = (haSHA1, haMD5);
  TCREncDataHeader = (ehTagAndHash, ehTag, ehNone);
  TCRInvalidHashAction = (ihFail, ihSkipData, ihIgnoreError);

  TCREncryptor = class(TComponent)
  private
    FDataHeader: TCREncDataHeader;
    FEncryptionAlgorithm: TCREncryptionAlgorithm;
    FHashAlgorithm: TCRHashAlgorithm;
    FInvalidHashAction: TCRInvalidHashAction;
    FEncryptor: TSymmetricAlgorithm;
    FHashSHA1: THashAlgorithm;
    FHashMD5: THashAlgorithm;
    FHash: THashAlgorithm;
    FPassword: string;
    FKeyFromPassword: TBytes;
    FKey: TBytes;
    FTmpBuffer: IntPtr;

    procedure SetEncryptionAlgorithm(const Value: TCREncryptionAlgorithm);
    procedure SetPassword(const Value: string);
    procedure Password2Key;
    procedure CheckEncryptor;

  protected
    function EncryptBytes(const Data: TValueArr; var Length: cardinal): TBytes;
    function Encrypt(const Data: TValueArr; DataType: Word; var Length: cardinal): Variant;
    procedure Decrypt(Data: IntPtr; DataType: Word; var Length: cardinal; out DecryptedDataType: TCRDecryptedDataType);

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadEncryptedPassword(Reader: TReader);
    procedure ReadPassword(Reader: TReader);
    procedure WriteEncryptedPassword(Writer: TWriter);
    function EncryptToHex(const Value: string): string;
    function DecryptFromHex(const Value: string): string;
    function EncryptPassword(const Value: TBytes): TBytes; virtual;
    function DecryptPassword(const Value: TBytes): TBytes; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetKey(const Key; Count: Integer); overload;
    procedure SetKey(const Key: TBytes; Offset, Count: Integer); overload;

  published
    property DataHeader: TCREncDataHeader read FDataHeader write FDataHeader default ehTagAndHash;
    property EncryptionAlgorithm: TCREncryptionAlgorithm read FEncryptionAlgorithm write SetEncryptionAlgorithm default eaBlowfish;
    property HashAlgorithm: TCRHashAlgorithm read FHashAlgorithm write FHashAlgorithm default haSHA1;
    property InvalidHashAction: TCRInvalidHashAction read FInvalidHashAction write FInvalidHashAction default ihFail;
    property Password: string read FPassword write SetPassword stored False;
  end;

  EInvalidEncData = class (Exception)
  end;

  EInvalidHash = class (Exception)
  end;

  TCREncryptorUtils = class
  public
    class function BytesEncryptor(Obj: TCREncryptor): TBytesEncryptionMethod;
    class function Encryptor(Obj: TCREncryptor): TEncryptionMethod;
    class function Decryptor(Obj: TCREncryptor): TDecryptionMethod;
  end;

function _GetRandomGenerator: TScRandom;

var
  IgnoreInvalidHashLength: boolean = False;

implementation

uses
  Math, CRCipher, CRHash, MemUtils, MemData, DAConsts;

var
  Random: TScRandom;
  PKEY_GUID: IntPtr;

const
  INTERNAL_HEADER_LEN = 8; {sizeof($BAADF00D) + sizeof(CipherDataLength)}
  IV_LEN = 8;
  KEY_GUID_LEN = 16;
  KEY_GUID: array[0..15] of byte =
    ($C5, $44, $E5, $29, $2C, $9C, $42, $A5, $B9, $4F, $E2, $79, $12, $70, $29, $01);

{ functions }

function _GetRandomGenerator: TScRandom;
begin
  Result := Random;
end;

{ TCREncryptor }

constructor TCREncryptor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEncryptionAlgorithm := eaBlowfish;
  FHashAlgorithm := haSHA1;
  FDataHeader := ehTagAndHash;
  FInvalidHashAction := ihFail;
  FHashSHA1 := THash_SHA1.Create;
  FHashMD5 := THash_MD5.Create;
  FTmpBuffer := Marshal.AllocHGlobal(32);
{$IFDEF WIN64}
  if not Assigned(Random) then
    Random := TScRandomLFSR.Create;
{$ENDIF}
end;

destructor TCREncryptor.Destroy;
begin
  Marshal.FreeHGlobal(FTmpBuffer);
  FHashSHA1.Free;
  FHashMD5.Free;
  FEncryptor.Free;

  inherited;
end;

procedure TCREncryptor.Password2Key;
var
  PassBuf: TBytes;
begin
  if FPassword <> '' then begin
    FHashSHA1.Initialize;
    FHashMD5.Initialize;
    SetLength(PassBuf, 0);
    PassBuf := Encoding.Unicode.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(WideString(FPassword));
    FHashSHA1.ComputeHash(PassBuf);
    FHashMD5.ComputeHash(PassBuf);
    SetLength(FKeyFromPassword, Length(FHashSHA1.Hash) + Length(FHashMD5.Hash));
    Buffer.BlockCopy(FHashSHA1.Hash, 0, FKeyFromPassword, 0, Length(FHashSHA1.Hash));
    Buffer.BlockCopy(FHashMD5.Hash, 0, FKeyFromPassword, Length(FHashSHA1.Hash), Length(FHashMD5.Hash));
  end;
end;

procedure TCREncryptor.CheckEncryptor;
var
  KeySize: integer;
  KeyBuf: TBytes;
begin
  case FHashAlgorithm of
    haSHA1:
      FHash := FHashSHA1;
    haMD5:
      FHash := FHashMD5;
  else
    Assert(False);
  end;

  if FEncryptor = nil then begin
    case FEncryptionAlgorithm of
      eaTripleDES:
        FEncryptor := TCipher_3DES.Create;
      eaBlowfish:
        FEncryptor := TCipher_Blowfish.Create;
      eaAES128, eaAES192, eaAES256:
        FEncryptor := TCipher_Rijndael.Create;
      eaCast128:
        FEncryptor := TCipher_Cast128.Create;
      eaRC4:
        FEncryptor := TCipher_RC4.Create;
    else
      Assert(False);
    end;

    FEncryptor.Mode := cmCBC;
  end;

  if FEncryptor.Initialized and (FEncryptionAlgorithm <> eaRC4) then
    Exit;

  if Length(FKey) > 0 then begin
    FEncryptor.Key := FKey;
    Exit;
  end;

  if FPassword = '' then
    raise Exception.Create(SInvalidKey);

  case FEncryptionAlgorithm of
    eaAES128:
      KeySize := 16;
    eaAES192:
      KeySize := 24;
    eaAES256:
      KeySize := 32;
  else
    KeySize := FEncryptor.KeySize;
  end;

  Assert(Length(FKeyFromPassword) >= 32);
  SetLength(KeyBuf, Min(KeySize, 32));
  Buffer.BlockCopy(FKeyFromPassword, 0, KeyBuf, 0, Length(KeyBuf));
  FEncryptor.Key := KeyBuf;
end;

function TCREncryptor.EncryptBytes(const Data: TValueArr; var Length: cardinal): TBytes;
var
  CipherLen, DataLen, HashLen: cardinal;
  Hash: TBytes;
begin
  CheckEncryptor;

  Assert((Data <> nil) or (Length = 0));

  if FDataHeader = ehTagAndHash then
    HashLen := FHash.HashSize
  else
    HashLen := 0;

  case FDataHeader of
    ehTagAndHash, ehTag: begin
      CipherLen := (((INTERNAL_HEADER_LEN + Length + HashLen) div cardinal(FEncryptor.BlockSize)) + 1) * cardinal(FEncryptor.BlockSize);
      DataLen := KEY_GUID_LEN + IV_LEN + CipherLen;
      SetLength(Result, DataLen);
      Move(KEY_GUID[0], Result[0], KEY_GUID_LEN);
      Random.Random(Result, KEY_GUID_LEN, IV_LEN);
      FEncryptor.SetIV(@Result[0], KEY_GUID_LEN, IV_LEN);

      Marshal.WriteInt32(Result, KEY_GUID_LEN + IV_LEN, integer($BAADF00D));
      Marshal.WriteInt32(Result, KEY_GUID_LEN + IV_LEN + 4, integer(Length));

      if Data <> nil then
        Move(Data[0], Result[KEY_GUID_LEN + IV_LEN + INTERNAL_HEADER_LEN], Length);
      Random.Random(Result, KEY_GUID_LEN + IV_LEN + INTERNAL_HEADER_LEN + Length, CipherLen - {$IFDEF FPC}Integer{$ENDIF}(INTERNAL_HEADER_LEN + Length + HashLen));

      if HashLen > 0 then begin
        SetLength(Hash, 0);
        FHash.Initialize;
        Hash := FHash.ComputeHash(Data, 0, Length);
        Buffer.BlockCopy(Hash, 0, Result, KEY_GUID_LEN + IV_LEN + Integer(CipherLen - HashLen), HashLen);
      end;

      FEncryptor.EncodeBuffer(TValueArr(Result), KEY_GUID_LEN + IV_LEN, CipherLen - HashLen, TValueArr(Result), KEY_GUID_LEN + IV_LEN);
      Length := DataLen;
    end;
    ehNone: begin
      if Length > 0 then begin
        SetLength(Result, Length);
        Move(Data[0], Result[0], Length);
        FEncryptor.ClearIV;
        FEncryptor.EncodeBuffer(TValueArr(Result), 0, Length, TValueArr(Result), 0);
      end
      else begin
        SetLength(Result, 0);
        Exit;
      end;
    end;
  else
    Assert(False);
  end;
end;

function TCREncryptor.Encrypt(const Data: TValueArr; DataType: Word; var Length: cardinal): Variant;
var
  s: string;
  ws: WideString;
  TempArr: TBytes;
  Output: TBytes;
begin
  Output := EncryptBytes(Data, Length);

  case DataType of
    dtBytes: begin
      Result := Output;
    end;
    dtString: begin
      SetLength(s, Length * 2);
    {$IFDEF IS_UNICODE}
      BinToHexW(Output, PWideChar(s), Length);
    {$ELSE}
      BinToHexA(Output, PAnsiChar(s), Length);
    {$ENDIF}
      Length := Length * 2;
      Result := s;
    end;
    dtWideString: begin
      SetLength(ws, Length * 2);
      BinToHexW(Output, PWideChar(ws), Length);
      Length := Length * 2;
      Result := ws;
    end;
    dtMemo: begin
      SetLength(TempArr, Length * 2);
      BinToHexA(Output, PAnsiChar(TempArr), Length);
      Length := Length * 2;
      Result := TempArr;
    end;
    dtWideMemo: begin
      SetLength(TempArr, Length * 4);
      BinToHexW(Output, PWideChar(TempArr), Length);
      Length := Length * 4;
      Result := TempArr;
    end;
  else
    raise Exception.CreateFmt(SEncryptionNotSupported, [IntToStr(DataType)]);
  end;
end;

procedure TCREncryptor.Decrypt(Data: IntPtr; DataType: Word; var Length: cardinal; out DecryptedDataType: TCRDecryptedDataType);
const
  BUG_HASH_LENGTH = 2;
var
  HashLen: integer;

  procedure DecryptBlob(Blob: TBlob; RealDataLen: cardinal; var Length: cardinal);
  var
    Offset: integer;
    FirstBlockLen, BufLen, NextLen: integer;
    Rest: integer;
    Piece: PPieceHeader;
    Hash: TBytes;
  begin
    Piece := Blob.FirstPiece;
    Data := PieceData(Piece);
    Rest := Piece.Used;

    if FDataHeader in [ehTagAndHash, ehTag] then
      Offset := KEY_GUID_LEN + IV_LEN
    else
      Offset := 0;

    while Length > 0 do begin
      BufLen := Min(cardinal(((Piece.Used - Offset) div FEncryptor.BlockSize) * FEncryptor.BlockSize), Length);
      if BufLen = 0 then
        BufLen := Length;

      if (Piece = Blob.FirstPiece) and (FDataHeader in [ehTagAndHash, ehTag]) then begin
        /// First Block was encrypted and copied
        FirstBlockLen := Min(Length, FEncryptor.BlockSize) - INTERNAL_HEADER_LEN;
        Piece.Used := Piece.Used - (KEY_GUID_LEN + IV_LEN + INTERNAL_HEADER_LEN);

        if RealDataLen > cardinal(FirstBlockLen) then begin
          Offset := KEY_GUID_LEN + IV_LEN + FEncryptor.BlockSize;
          FEncryptor.DecodeBuffer(PtrOffset(Data, Offset), PtrOffset(Data, Offset), BufLen - FEncryptor.BlockSize);
          CopyBuffer(PtrOffset(Data, Offset), PtrOffset(Data, FirstBlockLen), Piece.Used - FirstBlockLen);
        end
        else
          CopyBuffer(PtrOffset(Data, KEY_GUID_LEN + IV_LEN + Min(Length, FEncryptor.BlockSize)), PtrOffset(Data, FirstBlockLen), Piece.Used - FirstBlockLen);

        Dec(Length, cardinal(BufLen));
        Dec(BufLen, INTERNAL_HEADER_LEN);
        Offset := 0;
      end
      else begin
        FEncryptor.DecodeBuffer(PtrOffset(Data, Offset), PtrOffset(Data, Offset), BufLen);
        Dec(Length, cardinal(BufLen));
      end;

      Rest := Min(cardinal(Piece.Used - (Offset + BufLen)), Length);

      if Rest > 0 then begin
        CopyBuffer(PtrOffset(Data, Offset + BufLen), FTmpBuffer, Rest);

        NextLen := Min(FEncryptor.BlockSize, Length) - Rest;
        if NextLen > 0 then
          CopyBuffer(PieceData(Piece.Next), PtrOffset(FTmpBuffer, Rest), NextLen);
        FEncryptor.DecodeBuffer(FTmpBuffer, FTmpBuffer, Rest + NextLen);
        Length := Length - cardinal(Rest + NextLen);

        CopyBuffer(FTmpBuffer, PtrOffset(Data, Offset + BufLen), Rest);
        Inc(BufLen, Rest);

        if NextLen > 0 then
          CopyBuffer(PtrOffset(FTmpBuffer, Rest), PieceData(Piece.Next), NextLen);
      end
      else
        NextLen := 0;

      Inc(Offset, BufLen);
      Rest := Piece.Used - Offset;
      Piece.Used := Min(Offset, RealDataLen);
      if HashLen > 0 then
        FHash.TransformBlock(Data, 0, Piece.Used, Data, 0);
      RealDataLen := RealDataLen - Min(RealDataLen, cardinal(Offset));

      if Rest = 0 then begin
        Piece := Piece.Next;
        if IntPtr(Piece) = nil then
          Break;

        Data := PieceData(Piece);
        Offset := NextLen;
        Rest := Piece.Used - Offset;

        if RealDataLen = 0 then
          Piece.Used := 0;
      end;
    end;

    if HashLen > 0 then begin
      FHash.TransformFinalBlock(nil, 0, 0);
      SetLength(Hash, 0);
      Hash := FHash.Hash;

      Assert(Rest > 0);
      CopyBuffer(PtrOffset(Data, Offset), FTmpBuffer, Rest);
      if Rest < HashLen then begin
        Piece := Piece.Next;
        if IntPtr(Piece) <> nil then begin
          Data := PieceData(Piece);
          Piece.Used := 0;
          CopyBuffer(Data, PtrOffset(FTmpBuffer, Rest), HashLen - Rest);
        end;
      end;

      if not CompareMem(@Hash[0], FTmpBuffer, HashLen) then begin
        case FInvalidHashAction of
          ihFail:
            raise EInvalidHash.Create(SInvalidHash);
          ihSkipData: begin
            Length := 0;
            DecryptedDataType := ddtError;
            Exit;
          end;
          ihIgnoreError: ;
        else
          Assert(False);
        end;
      end;
    end;

    Length := Blob.Size;
    DecryptedDataType := ddtDecrypted;
  end;

var
  Hash: TBytes;
  RealDataLen, FirstBlockLen: cardinal;
  Blob: TBlob;
  Len, Offset: integer;
begin
  CheckEncryptor;

  Assert(Data <> nil);
  Blob := nil;
  if FDataHeader = ehTagAndHash then
    HashLen := FHash.HashSize
  else
    HashLen := 0;

  case FDataHeader of
    ehTagAndHash, ehTag: begin
      DecryptedDataType := ddtNonEncrypted;

      case DataType of
        dtBytes: begin
          if (Length < KEY_GUID_LEN) or not CompareMem(Data, PKEY_GUID, KEY_GUID_LEN) then
            Exit;
        end;
        dtString, dtMemo: begin
          if DataType = dtString then
            Length := StrLen(PAChar(Data));
          if Length < KEY_GUID_LEN * 2 then
            Exit;

          Len := HexToBinA(Data, FTmpBuffer, KEY_GUID_LEN);
          if (Len < KEY_GUID_LEN) or not CompareMem(FTmpBuffer, PKEY_GUID, KEY_GUID_LEN) then
            Exit;

          Length := HexToBinA(Data, Data, Length shr 1);
        end;
        dtWideString, dtWideMemo: begin
          if DataType = dtWideString then
            Length := StrLenW(Data);
          if Length < KEY_GUID_LEN * 2 then
            Exit;

          Len := HexToBinW(PWChar(Data), FTmpBuffer, KEY_GUID_LEN);
          if (Len < KEY_GUID_LEN) or not CompareMem(FTmpBuffer, PKEY_GUID, KEY_GUID_LEN) then
            Exit;

          Length := HexToBinW(PWChar(Data), Data, Length shr 1);
        end;
        dtBlob: begin
          Blob := TBlob(GetGCHandleTarget(Data));
          // workaround for call CheckValue and load deferred Lobs
          if not Blob.IsEmpty then
            Data := PieceData(Blob.FirstPiece)
          else
            Data := nil;
          Length := Blob.Size;

          if (Length < KEY_GUID_LEN) or not CompareMem(Data, PKEY_GUID, KEY_GUID_LEN) then
            Exit;
        end;
      else
        raise Exception.CreateFmt(SEncryptionNotSupported, [IntToStr(DataType)]);
      end;

      if Length < cardinal(KEY_GUID_LEN + IV_LEN + INTERNAL_HEADER_LEN + HashLen) then begin
        if not IgnoreInvalidHashLength or (Length < cardinal(KEY_GUID_LEN + IV_LEN + INTERNAL_HEADER_LEN + BUG_HASH_LENGTH)) then
          raise Exception.Create(SInvalidEncDataSize);
        HashLen := BUG_HASH_LENGTH;
      end;

      Length := Length - (KEY_GUID_LEN + IV_LEN);
      FEncryptor.SetIV(Data, KEY_GUID_LEN, IV_LEN);

      if DataType = dtBytes then begin
        while (Length > cardinal(FEncryptor.BlockSize)) and (Marshal.ReadByte(Data, KEY_GUID_LEN + IV_LEN + integer(Length) - 1) = 0) do
          Dec(Length);
        if (Length mod cardinal(FEncryptor.BlockSize)) <> 0 then
          Length := ((Length div cardinal(FEncryptor.BlockSize)) + 1) * cardinal(FEncryptor.BlockSize);
      end;

      if (Length mod cardinal(FEncryptor.BlockSize)) <> 0 then
        raise Exception.Create(SInvalidEncDataSize);

      if HashLen > 0 then
        FHash.Initialize;
      Length := Length - cardinal(HashLen);

      FirstBlockLen := Min(Length, FEncryptor.BlockSize);
      FEncryptor.DecodeBuffer(PtrOffset(Data, KEY_GUID_LEN + IV_LEN), FTmpBuffer, FirstBlockLen);
      RealDataLen := cardinal(Marshal.ReadInt32(FTmpBuffer, 4));
      if RealDataLen > Length - INTERNAL_HEADER_LEN then begin
        if not IgnoreInvalidHashLength then
          raise EInvalidEncData.Create(SInvalidEncData)
        else begin
          Length := Length + cardinal(HashLen) - BUG_HASH_LENGTH;
          HashLen := BUG_HASH_LENGTH;
          FEncryptor.SetIV(Data, KEY_GUID_LEN, IV_LEN);
          FirstBlockLen := Min(Length, FEncryptor.BlockSize);
          FEncryptor.DecodeBuffer(PtrOffset(Data, KEY_GUID_LEN + IV_LEN), FTmpBuffer, FirstBlockLen);
          RealDataLen := cardinal(Marshal.ReadInt32(FTmpBuffer, 4));
          if RealDataLen > Length - INTERNAL_HEADER_LEN then
            raise EInvalidEncData.Create(SInvalidEncData);
        end;
      end;
      Dec(FirstBlockLen, INTERNAL_HEADER_LEN);
      CopyBuffer(PtrOffset(FTmpBuffer, INTERNAL_HEADER_LEN), Data, FirstBlockLen);

      if DataType <> dtBlob then begin
        if RealDataLen > FirstBlockLen then begin
          Offset := KEY_GUID_LEN + IV_LEN + FEncryptor.BlockSize;
          FEncryptor.DecodeBuffer(PtrOffset(Data, Offset), PtrOffset(Data, Offset), Length - cardinal(FEncryptor.BlockSize));
          CopyBuffer(PtrOffset(Data, Offset), PtrOffset(Data, FirstBlockLen), RealDataLen - FirstBlockLen);
        end;

        DecryptedDataType := ddtDecrypted;
        if HashLen > 0 then begin
          FHash.TransformBlock(Data, 0, RealDataLen, Data, 0);
          FHash.TransformFinalBlock(nil, 0, 0);
          SetLength(Hash, 0);
          Hash := FHash.Hash;
          if not CompareMem(@Hash[0], PtrOffset(Data, KEY_GUID_LEN + IV_LEN + Length), HashLen) then begin
            if IgnoreInvalidHashLength and
              CompareMem(@Hash[0], PtrOffset(Data, KEY_GUID_LEN + IV_LEN + Length + cardinal(HashLen) - BUG_HASH_LENGTH), BUG_HASH_LENGTH)
            then begin
              Length := RealDataLen;
              Exit;
            end;

            case FInvalidHashAction of
              ihFail:
                raise EInvalidHash.Create(SInvalidHash);
              ihSkipData: begin
                Length := 0;
                DecryptedDataType := ddtError;
              end;
              ihIgnoreError:
                Length := RealDataLen;
            else
              Assert(False);
            end;
          end
          else
            Length := RealDataLen;
        end
        else
          Length := RealDataLen;
      end
      else
        DecryptBlob(Blob, RealDataLen, Length);
    end;

    ehNone: begin
      DecryptedDataType := ddtDecrypted;
      FEncryptor.ClearIV;

      if DataType = dtBlob then begin
        Blob := TBlob(GetGCHandleTarget(Data));
        Length := Blob.Size;
        DecryptBlob(Blob, Length, Length);
      end
      else begin
        case DataType of
          dtString:
            Length := HexToBinA(Data, Data, StrLen(PAChar(Data)) shr 1);
          dtWideString:
            Length := HexToBinW(PWChar(Data), Data, StrLenW(Data) shr 1);
          dtMemo:
            Length := HexToBinA(Data, Data, Length shr 1);
          dtWideMemo:
            Length := HexToBinW(PWChar(Data), Data, Length shr 1);
        end;

        FEncryptor.DecodeBuffer(Data, Data, Length);
      end;
    end;
  else
    Assert(False);
  end;
end;

procedure TCREncryptor.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Password', ReadPassword, nil, True);
  Filer.DefineProperty('EncryptedPassword', ReadEncryptedPassword, WriteEncryptedPassword, (Password <> ''));
end;

procedure TCREncryptor.ReadEncryptedPassword(Reader: TReader);
begin
  Password := DecryptFromHex(Reader.ReadString);
end;

procedure TCREncryptor.ReadPassword(Reader: TReader);
begin
  Password := Reader.ReadString;
end;

procedure TCREncryptor.WriteEncryptedPassword(Writer: TWriter);
begin
  Writer.WriteString(EncryptToHex(Password));
end;

function TCREncryptor.EncryptToHex(const Value: string ): string;
var
  Buf: TBytes;
  Len: Integer;
begin
  Buf := Encoding.Unicode.GetBytes(WideString(Value));
  Buf := EncryptPassword(Buf);
  Len := Length(Buf);
  SetLength(Result, Len * 2);
{$IFDEF IS_UNICODE}
  BinToHexW(Buf, PWideChar(Result), Len);
{$ELSE}
  BinToHexA(Buf, PAnsiChar(Result), Len);
{$ENDIF}
end;

function TCREncryptor.DecryptFromHex(const Value: string): string;
var
{$IFDEF VER12P}
  HexStr: WideString;
{$ELSE}
  HexStr: AnsiString;
{$ENDIF}
  Buf: TBytes;
  Len: Integer;
begin
  HexStr := Value;
  Len := Length(HexStr) shr 1;
  SetLength(Buf, Len);
{$IFDEF IS_UNICODE}
  Len := HexToBinW(PWideChar(HexStr), Buf, Len);
{$ELSE}
  Len := HexToBinA(PAnsiChar(HexStr), Buf, Len);
{$ENDIF}
  SetLength(Buf, Len);
  Buf := DecryptPassword(Buf);
  result := string(Encoding.Unicode.GetString(Buf));
end;

function TCREncryptor.EncryptPassword(const Value: TBytes): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := 0 to Length(Value) - 1 do
    Result[i] := not Value[i];
end;

function TCREncryptor.DecryptPassword(const Value: TBytes): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Length(Value));
  for i := 0 to Length(Value) - 1 do
    Result[i] := not Value[i];
end;

procedure TCREncryptor.SetKey(const Key; Count: Integer);
begin
  SetKey(TBytes(@Key), 0, Count);
end;

procedure TCREncryptor.SetKey(const Key: TBytes; Offset, Count: Integer);
begin
  SetLength(FKey, Count);
  Buffer.BlockCopy(Key, Offset, FKey, 0, Count);
  FPassword := '';

  FEncryptor.Free;
  FEncryptor := nil;
  CheckEncryptor;
end;

procedure TCREncryptor.SetPassword(const Value: string);
begin
  if Value <> FPassword then begin
    FPassword := Value;
    SetLength(FKey, 0);
    Password2Key;

    FEncryptor.Free;
    FEncryptor := nil;
    CheckEncryptor;
  end;
end;

procedure TCREncryptor.SetEncryptionAlgorithm(const Value: TCREncryptionAlgorithm);
begin
  if Value <> FEncryptionAlgorithm then begin
    FEncryptionAlgorithm := Value;
    FEncryptor.Free;
    FEncryptor := nil;
  end;
end;

{ TCREncryptorUtils }

class function TCREncryptorUtils.BytesEncryptor(Obj: TCREncryptor): TBytesEncryptionMethod;
begin
  Result := Obj.EncryptBytes;
end;

class function TCREncryptorUtils.Encryptor(Obj: TCREncryptor): TEncryptionMethod;
begin
  Result := Obj.Encrypt;
end;

class function TCREncryptorUtils.Decryptor(Obj: TCREncryptor): TDecryptionMethod;
begin
  Result := Obj.Decrypt;
end;

initialization
{$IFNDEF WIN64}
  Random := TScRandomLFSR.Create;
{$ENDIF}
  PKEY_GUID := @KEY_GUID;

finalization
  Random.Free;

end.

