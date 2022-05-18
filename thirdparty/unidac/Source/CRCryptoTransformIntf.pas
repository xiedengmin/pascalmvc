
{$I Dac.inc}
unit CRCryptoTransformIntf;

interface

uses
  SysUtils,
  CRTypes;

{$M-}
type
  ICryptoTransform = interface
  ['{AE5FFE08-1D09-4452-91C8-122B05513FEB}']
    procedure TransformBlock(const Data: TBytes; Offset, Count: Integer); overload;
    function TransformBlock(const InputBuffer: TValueArr; InputOffset, InputCount: Integer; const OutputBuffer: TValueArr; OutputOffset: Integer): Integer; overload;
    procedure TransformFinalBlock(const InputBuffer: TBytes; InputOffset, InputCount: Integer);
    procedure SetIV(Value: TValueArr; Offset, Count: integer);

    function Get_OutputBlockSize: Integer;
    property OutputBlockSize: Integer read get_OutputBlockSize;
  end;

  IHashTransform = interface
  ['{2FA7F01B-FBAF-4357-A302-596DA966CD45}']
    function ComputeHash(const Data: TBytes): TBytes; overload;
    function ComputeHash(const Data: TValueArr; Offset, Count: Integer): TBytes; overload;

    function Get_HashSize: Integer;
    property HashSize: Integer read Get_HashSize;
  end;

implementation

end.
