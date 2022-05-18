//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR HMAC
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRHMAC;

interface

uses
  SysUtils,
  CLRClasses, CRTypes, CRHashAlgorithm, CRCryptoTransformIntf;

type
  TKeyedHashAlgorithm = class(THashAlgorithm)
  protected
    FKeyValue: TBytes;
  end;

  /// http://www.ietf.org/rfc/rfc2104.txt
  /// Implements the HMAC keyed message authentication code algorithm.
  THMAC = class(TKeyedHashAlgorithm)
  private
    /// Holds the internal hash algorithm
    FHashAlgorithm: THashAlgorithm;
    /// true, if a hash operation is in prograss, false - otherwise.
    FIsHashing: boolean;
    FPadded36: TBytes;
    FPadded5C: TBytes;
    FBlockSize: integer;

  protected
    function Get_HashSize: integer; override;
    procedure HashCore(const Data: TValueArr; Offset, Count: integer); override;
    function HashFinal: TBytes; override;

  public
    constructor Create(const HashAlgorithmClass: THashAlgorithmClass; const rgbKey: TBytes); reintroduce;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses
  DAConsts, CRDECUtil;

{ THMAC }

constructor THMAC.Create(const HashAlgorithmClass: THashAlgorithmClass; const rgbKey: TBytes);
var
  KeyBuffer: TBytes;
  i: integer;
begin
  inherited Create;

  if HashAlgorithmClass = nil then
    raise Exception.Create(SInvalidInputArgs);

//  if rgbKey = nil then begin
//    SetLength(rgbKey, Hash.HashSize div 8);
//    rand.Random(rgbKey, 0, Length(rgbKey));
//  end;

  FHashAlgorithm := HashAlgorithmClass.Create;
  if FHashAlgorithm.HashSize <= 32 then
    FBlockSize := 64
  else
    FBlockSize := 128; // sha384, sha512

  FKeyValue := rgbKey;

  SetLength(FPadded36, FBlockSize);
  SetLength(FPadded5C, FBlockSize);

  SetLength(KeyBuffer, 0);
  if Length(FKeyValue) > FBlockSize then begin
    FHashAlgorithm.Initialize;
    KeyBuffer := FHashAlgorithm.ComputeHash(FKeyValue);
  end
  else
    KeyBuffer := FKeyValue;
  SetLength(KeyBuffer, FBlockSize);

  for i := 0 to FBlockSize - 1 do begin
    FPadded36[i] := byte(KeyBuffer[i] xor $36);
    FPadded5C[i] := byte(KeyBuffer[i] xor $5C);
  end;

  Initialize;
end;

destructor THMAC.Destroy;
begin
  FHashAlgorithm.Free;

  inherited;
end;

procedure THMAC.Initialize;
begin
  FHashAlgorithm.Initialize;
  FIsHashing := False;
  FState := 0;
end;

/// Routes data written to the object into the hash algorithm for computing the hash.
procedure THMAC.HashCore(const Data: TValueArr; Offset, Count: integer);
begin
  if not FIsHashing then begin
    FHashAlgorithm.TransformBlock(FPadded36, 0, Length(FPadded36));
    FIsHashing := True;
  end;

  FHashAlgorithm.TransformBlock(Data, Offset, Count, Data, Offset);
end;

/// Finalizes the hash computation after the last data is processed by the cryptographic stream object.
function THMAC.HashFinal: TBytes;
begin
  FHashAlgorithm.TransformFinalBlock(nil, 0, 0);

  FHashAlgorithm.Initialize;
  FHashAlgorithm.TransformBlock(FPadded5C, 0, Length(FPadded5C));
  FHashAlgorithm.TransformFinalBlock(FHashAlgorithm.Hash, 0, Length(FHashAlgorithm.Hash));

  Result := FHashAlgorithm.Hash;
  FIsHashing := False;
end;

function THMAC.get_HashSize: integer;
begin
  Result := FHashAlgorithm.HashSize;
end;

end.

