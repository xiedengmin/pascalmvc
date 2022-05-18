
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  HashAlgorithm
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRHashAlgorithm;

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
  THashAlgorithmClass = class of THashAlgorithm;

  THashAlgorithm = class(TInterfacedObject, IHashTransform)
  protected
    FHashBlock: TBytes;
    FState: Integer;

    procedure HashCore(const Data: TValueArr; Offset, Count: Integer); virtual; abstract;
    function HashFinal: TBytes; virtual; abstract;
    function Get_HashSize: Integer; virtual;

  public
    constructor Create; virtual;

    procedure Initialize; virtual; abstract;
    procedure TransformBlock(const Data: TBytes; Offset, Count: Integer); overload;
    function TransformBlock(const InputBuffer: TValueArr; InputOffset, InputCount: Integer; const OutputBuffer: TValueArr; OutputOffset: Integer): Integer; overload;
    procedure TransformFinalBlock(const InputBuffer: TBytes; InputOffset, InputCount: Integer); overload;
    procedure TransformFinalBlock(const InputBuffer: TValueArr; InputCount: Integer); overload;
    function ComputeHash(const Buffer: TValueArr; Offset, Count: Integer): TBytes; overload;
    function ComputeHash(const Buffer: TBytes): TBytes; overload;
    class function GetHashSize: Integer; virtual;

    property Hash: TBytes read FHashBlock;
    property HashSize: Integer read Get_HashSize;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows;
{$ELSE}
  CRFunctions;
{$ENDIF}

constructor THashAlgorithm.Create;
begin
  inherited;
end;

function THashAlgorithm.Get_HashSize: Integer;
begin
  Result := GetHashSize;
end;

class function THashAlgorithm.GetHashSize: Integer;
begin
  Result := 0;
end;

procedure THashAlgorithm.TransformBlock(const Data: TBytes; Offset, Count: Integer);
begin
  FState := 1;
  HashCore(TValueArr(Data), Offset, Count);
end;

function THashAlgorithm.TransformBlock(const InputBuffer: TValueArr; InputOffset, InputCount: Integer;
  const OutputBuffer: TValueArr; OutputOffset: Integer): Integer;
begin
  FState := 1;
  HashCore(InputBuffer, InputOffset, InputCount);
  if (InputBuffer <> OutputBuffer) or (InputOffset <> OutputOffset) then
    Move(InputBuffer[InputOffset], OutputBuffer[OutputOffset], InputCount);
  Result := InputCount;
end;

procedure THashAlgorithm.TransformFinalBlock(const InputBuffer: TBytes; InputOffset, InputCount: Integer);
begin
  HashCore(TValueArr(InputBuffer), InputOffset, InputCount);
  FHashBlock := HashFinal;
  FState := 0;
end;

procedure THashAlgorithm.TransformFinalBlock(const InputBuffer: TValueArr; InputCount: Integer);
begin
  HashCore(InputBuffer, 0, InputCount);
  FHashBlock := HashFinal;
  FState := 0;
end;

function THashAlgorithm.ComputeHash(const Buffer: TBytes): TBytes;
begin
  HashCore(TValueArr(Buffer), 0, Length(Buffer));
  FHashBlock := HashFinal;
  Initialize;
  Result := FHashBlock;
end;

function THashAlgorithm.ComputeHash(const Buffer: TValueArr; Offset, Count: Integer): TBytes;
begin
  HashCore(Buffer, Offset, Count);
  FHashBlock := HashFinal;
  Initialize;
  Result := FHashBlock;
end;

end.
