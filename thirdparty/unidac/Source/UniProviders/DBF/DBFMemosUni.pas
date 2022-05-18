
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFMemosUni;

interface

{$IFDEF DBFENGINE}
uses
  SysUtils, Classes,
  CRTypes, CRFunctions, MemData, CRVirtualData,
  CLRClasses,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs, DBFUtils;
{$ELSE}
  DBFConstsUni, DBFStructsUni, DBFUtilsUni;
{$ENDIF}

type
  TDBFMemoClass = class of TDBFMemoFile;

  TDBFMemoFile = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDBF: TObject; // TDBFDBase;
    FHeader: TBytes;
    FBuffer: TBytes;
    FBufferSize: Cardinal;
    FBlockSize: Word;
    FNextBlock: Cardinal;
    FFileName: string;
    FFileStream: TFileStream;
  private
    procedure InternalWrite(const Value: TVirtualValue; AsBinary: boolean);
    procedure InternalRead(const Blob: TBlob; Source: TBytes; Index, Count: Cardinal; AsBinary: boolean);
  protected
    procedure CreateMemo; virtual;
    procedure FillBlockFooter(Len: integer; var Blocks: integer);
  public
    constructor Create(const Parent: TObject; const FileName: string; New: boolean = False);
    destructor Destroy; override;

    procedure Open; virtual;
    procedure Close;
    procedure ZapMemo; virtual;
    procedure Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False); virtual;
    function Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer; virtual;

    property FileName: string read FFileName;
  end;

  TDBFClipperMemo = class(TDBFMemoFile)
  public
    procedure Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False); override;
    function Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer; override;
  end;

  TDBFIVMemo = class(TDBFMemoFile)
  protected
    procedure CreateMemo; override;
  public
    procedure Open; override;
    procedure ZapMemo; override;
    procedure Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False); override;
    function Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer; override;
  end;

  TDBFFoxProMemo = class(TDBFMemoFile)
  protected
    procedure CreateMemo; override;
  public
    procedure Open; override;
    procedure ZapMemo; override;
    procedure Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False); override;
    function Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer; override;
  end;

  TDBFHiPerSixMemo = class(TDBFMemoFile)
  protected
    procedure CreateMemo; override;
  public
    procedure Open; override;
    procedure ZapMemo; override;
    procedure Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False); override;
    function Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer; override;
  end;

{$ENDIF}

implementation

{$IFDEF DBFENGINE}

{$IFNDEF UNIDACPRO}
uses
  DBFDBase;
{$ELSE}
uses
  DBFDBaseUni;
{$ENDIF}
{ TMemoFile }

constructor TDBFMemoFile.Create(const Parent: TObject; const FileName: string; New: boolean = False);
begin
  inherited Create;

  FDBF := Parent;
  FFileName := FileName;

  if not New then
    FFileStream := TFileStream.Create(FFileName, TDBFDBase(FDBF).GetFileMode(TDBFDBase(FDBF).ForceReadOnly, TDBFDBase(FDBF).ForceExclusive))
  else
    CreateMemo;
end;

destructor TDBFMemoFile.Destroy;
begin
  Close;

  inherited;
end;

procedure TDBFMemoFile.InternalWrite(const Value: TVirtualValue; AsBinary: boolean);

  procedure WriteBlob(Blob: TBlob);
  var
    BlobData: TCRBlobData;
    buf: TBytes;
    bufPos, bufLen: Cardinal;
  begin
    SetLength(buf, 65536); // todo ? static array instead
    bufPos := 0;
    BlobData := Blob.GetData;
    while bufPos < Blob.Size do begin
      bufLen := BlobData.Read(bufPos, System.Length(buf), @buf[0]);
      FFileStream.Write(buf[0], bufLen);
      Inc(bufPos, bufLen);
    end;
  end;

var
  Blob: TBlob;
  v_type: Word;
  aStr: AnsiString;
  TmpBuf: TBytes;
begin
{$IFNDEF VER9P}
  TmpBuf := nil;
{$ENDIF}
  case Value.ValueType of
    vrString,
    vrAnsiString,
    vrWideString: begin
      aStr := AnsiString(Value.Value);
      if Length(aStr) > 0 then begin
        if not AsBinary and (FDBF <> nil) and (TDBFDBase(FDBF).FileEncoding <> nil) then begin
          TmpBuf := TDBFDBase(FDBF).FileEncoding.GetBytes(aStr);
          FFileStream.Write(TmpBuf[0], Length(TmpBuf));
        end
        else
          FFileStream.Write(PAnsiChar(aStr)^, Length(aStr));
      end;
    end;
    vrBlob: begin
      v_type := TVarData(Value.Value).VType;
      if (v_type and varByRef) <> 0 then begin
        Blob := TVarData(Value.Value).VPointer;
        WriteBlob(Blob);
      end
      else if v_type = varArray + varByte then
        FFileStream.Write(TVarData(Value.Value).VArray.Data^, TVarData(Value.Value).VArray.Bounds[0].ElementCount)
      else
        raise Exception.CreateFmt('Unknown v_type %d', [v_type]);
    end;
  else
    raise Exception.CreateFmt('Unknown ValueType %d', [integer(Value.ValueType)]);
  end;
end;

procedure TDBFMemoFile.InternalRead(const Blob: TBlob; Source: TBytes; Index, Count: Cardinal; AsBinary: boolean);
var
  TmpBuf: TBytes;
begin
{$IFNDEF VER9P}
  TmpBuf := nil;
{$ENDIF}
  if (FDBF <> nil) and (TDBFDBase(FDBF).FileEncoding <> nil) and (TDBFDBase(FDBF).FileEncoding <> Encoding.Default) and not AsBinary then begin
    TmpBuf := TDBFDBase(FDBF).FileEncoding.Convert(TDBFDBase(FDBF).FileEncoding, Encoding.Default, Source, Index, Count);
    Blob.Write(Blob.Size, Length(TmpBuf), @TmpBuf[0]);
  end
  else
    Blob.Write(Blob.Size, Count, @Source[Index]);
end;

procedure TDBFMemoFile.CreateMemo;
var
  pDBT3Hdr: PDBT3Header;
  written: integer;
begin
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FBlockSize := 512;
  SetLength(FHeader, SizeOf(TDBT3Header));
  SetLength(FBuffer, FBlockSize);
  pDBT3Hdr := @FBuffer[0];
  FNextBlock := 1;
  pDBT3Hdr.NextBlock := FNextBlock;

  Move(FBuffer[0], FHeader[0], SizeOf(TDBT3Header)); // copy to FHeader

  written := FFileStream.Write(FHeader[0], Length(FBuffer));
  if written <> Length(FBuffer) then
    raise Exception.CreateFmt('Write error, written %d instead of %d', [written, Length(FBuffer)]);
end;

procedure TDBFMemoFile.FillBlockFooter(Len: integer; var Blocks: integer);
var
  buf: TBytes;
  left: integer;
begin
  Blocks := Len div FBlockSize;
  if (Len mod FBlockSize) > 0 then
    Inc(Blocks);
  left := Blocks * FBlockSize - Len;
  if left > 0 then begin
    SetLength(buf, left);
    FillChar(buf[0], left, 0);
    FFileStream.Write(buf[0], Length(buf));
  end;
end;

procedure TDBFMemoFile.Open;
var
  readed: integer;
begin
  FFileStream.Seek(LongInt(0), soFromBeginning);

  // size of empty file is 4
  SetLength(FHeader, SizeOf(TDBT3Header));
  readed := FFileStream.Read(FHeader[0], SizeOf(TDBT3Header));
  if readed <> SizeOf(TDBT3Header) then
    raise Exception.CreateFmt(SMemoOpenError, [FFileName]);
  FBlockSize := 512;
  SetLength(FBuffer, FBlockSize);
  FNextBlock := PDBT3Header(@FHeader[0]).NextBlock;
end;

procedure TDBFMemoFile.Close;
begin
  FreeAndNil(FFileStream);

  SetLength(FBuffer, 0);
  SetLength(FHeader, 0);
end;

procedure TDBFMemoFile.ZapMemo;
begin
  FNextBlock := 1;
  PDBT3Header(@FHeader[0]).NextBlock := FNextBlock;
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
  FFileStream.Size := Int64(FBlockSize);
end;

procedure TDBFMemoFile.Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False);
var
  i, readed: integer;
  count: Cardinal;
  done: boolean;
begin
  done := False;
  FFileStream.Seek(LongInt(StartIndex * FBlockSize), soFromBeginning);
  Blob.RollbackEnabled := False;
  try
    while not done do begin
      readed := FFileStream.Read(FBuffer[0], FBlockSize);
      if readed <> FBlockSize then
        raise Exception.CreateFmt('Error when reading BLOB data: %d bytes instead of %d', [readed, FBlockSize]);

      count := 0;
      for i := 0 to FBlockSize - 1 do
        if FBuffer[i] = {$IFNDEF NEXTGEN}Ord{$ENDIF}(DBF_RECORD_EOF) then begin
          done := True;
          Break;
        end
        else
          Inc(count);

      InternalRead(Blob, FBuffer, 0, count, AsBinary);
    end;
  finally
    Blob.RollbackEnabled := True;
  end;
end;

function TDBFMemoFile.Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer;
var
  blocks: integer;
begin
  // result is MemoIdx
  Result := PDBT3Header(@FHeader[0]).NextBlock;
  FFileStream.Seek(LongInt(Result * FBlockSize), soFromBeginning);
  InternalWrite(Value, AsBinary);
  FFileStream.Write(DBF_RECORD_EOF, 1);
  FFileStream.Write(DBF_RECORD_EOF, 1);

  Inc(Size, 2);
  FillBlockFooter(Size, blocks);

  // adjust NextBlock and store header
  PDBT3Header(@FHeader[0]).NextBlock := Result + blocks;
  Assert(FFileStream.Position = PDBT3Header(@FHeader[0]).NextBlock * FBlockSize);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
end;

{ TDBFClipperMemo }

procedure TDBFClipperMemo.Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False);
var
  readed: integer;
  i, count: Cardinal;
begin
  FFileStream.Seek(LongInt(StartIndex * FBlockSize), soFromBeginning);
  Blob.RollbackEnabled := False;
  try
    readed := FFileStream.Read(FBuffer[0], FBlockSize);
    if readed <> FBlockSize then
      raise Exception.CreateFmt('DBT read error, %d bytes instead of %d', [readed, FBlockSize]);

    if (FBuffer[0] <> $FF) and (FBuffer[1] <> $FF) then
      raise Exception.Create('Invalid DBT format');

    count := Cardinal((@FBuffer[4])^) - 8;

    if count > 0 then begin
      i := count;
      if i > Cardinal(FBlockSize - 8) then
        i := FBlockSize - 8;
      InternalRead(Blob, FBuffer, 8, i, AsBinary);
      count := count - i;

      while count > 0 do begin
        if count > FBlockSize then
          i := FBlockSize
        else
          i := count;

        readed := FFileStream.Read(FBuffer[0], i);
        if Cardinal(readed) <> i then
          raise Exception.CreateFmt('DBT read error, %d bytes instead of %d', [readed, FBlockSize]);
        InternalRead(Blob, FBuffer, 0, i, AsBinary);
        count := count - i;
      end;
    end;
  finally
    Blob.RollbackEnabled := True;
  end;
end;

function TDBFClipperMemo.Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer;
var
  len, blocks: integer;
begin
  // result is MemoIdx
  Result := PDBT3Header(@FHeader[0]).NextBlock;
  FFileStream.Seek(LongInt(Result * FBlockSize), soFromBeginning);
  len := GetVirtualTypeSize(Value);
  Inc(len, 8);
  FBuffer[0] := $FF;
  FBuffer[1] := $FF;
  FBuffer[2] := $80;
  FBuffer[3] := $0;
  Cardinal((@FBuffer[4])^) := len;
  FFileStream.Write(FBuffer[0], 8);
  InternalWrite(Value, AsBinary);

  FillBlockFooter(len, blocks);

  // adjust NextBlock and store header
  PDBT3Header(@FHeader[0]).NextBlock := Result + blocks;
  Assert(FFileStream.Position = PDBT3Header(@FHeader[0]).NextBlock * FBlockSize);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
end;

{ TDBFIVMemo }

procedure TDBFIVMemo.CreateMemo;
var
  pDBT4Hdr: PDBT4Header;
  s: string;
  aStr: AnsiString;
  len, written: integer;
begin
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FBlockSize := $400;
  SetLength(FHeader, SizeOf(TDBT4Header));
  SetLength(FBuffer, FBlockSize);
  pDBT4Hdr := @FBuffer[0];
  FNextBlock := 1;
  pDBT4Hdr.NextBlock := FNextBlock;
  pDBT4Hdr.Ver := $01020000;
  pDBT4Hdr.BlockSize := FBlockSize;

  s := ExtractFileName(FFileName);
  s := Copy(s, 1, Length(s) - 4);
  aStr := AnsiString(s);
  len := Length(aStr);
  if len > 8 then
    len := 8;
  Move(PAnsiChar(aStr)^, pDBT4Hdr.TableName[0], len);

  Move(FBuffer[0], FHeader[0], SizeOf(TDBT4Header)); // copy to FHeader

  written := FFileStream.Write(FBuffer[0], Length(FBuffer));
  if written <> Length(FBuffer) then
    raise Exception.CreateFmt('Write error, written %d instead of %d', [written, Length(FBuffer)]);
end;

procedure TDBFIVMemo.Open;
var
  readed: integer;
begin
  FFileStream.Seek(LongInt(0), soFromBeginning);

  // size of empty file is BlockSize
  SetLength(FHeader, SizeOf(TDBT4Header));
  readed := FFileStream.Read(FHeader[0], SizeOf(TDBT4Header));
  if readed <> SizeOf(TDBT4Header) then
    raise Exception.CreateFmt(SMemoOpenError, [FFileName]);
  FBlockSize := PDBT4Header(@FHeader[0]).BlockSize;
  FBufferSize := FBlockSize * 64;
  SetLength(FBuffer, FBufferSize);
  FNextBlock := PDBT4Header(@FHeader[0]).NextBlock;
end;

procedure TDBFIVMemo.Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False);
var
  readed: integer;
  count, bytesLeft: Cardinal;
begin
  FFileStream.Seek(LongInt(StartIndex * FBlockSize), soFromBeginning);
  Blob.RollbackEnabled := False;
  try
    readed := FFileStream.Read(FBuffer[0], SizeOf(TDBT4BlockHeader));
    if readed <> SizeOf(TDBT4BlockHeader) then
      raise Exception.CreateFmt('DBT read error, %d bytes instead of %d', [readed, SizeOf(TDBT4BlockHeader)]);

    Assert(PDBT4BlockHeader(@FBuffer[0]).Sign = DBT4_BLOCK_SIGN, Format('Wrong DBT BlockHeader Sign %.8X', [PDBT4BlockHeader(@FBuffer[0]).Sign]));
    bytesLeft := PDBT4BlockHeader(@FBuffer[0]).Size;
    Assert(bytesLeft >= SizeOf(TDBT4BlockHeader), Format('Wrong DBT BlockHeader Size %.8X', [bytesLeft]));

    Dec(bytesLeft, SizeOf(TDBT4BlockHeader));
    while bytesLeft > 0 do begin
      count := bytesLeft;
      if count > FBufferSize then
        count := FBufferSize;
      readed := FFileStream.Read(FBuffer[0], count);
      InternalRead(Blob, FBuffer, 0, readed, AsBinary);
      Dec(bytesLeft, readed);
    end;
  finally
    Blob.RollbackEnabled := True;
  end;
end;

function TDBFIVMemo.Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer;
var
  len, blocks: integer;
  bh4: TDBT4BlockHeader;
begin
  // result is MemoIdx
  Result := PDBT4Header(@FHeader[0]).NextBlock;
  FFileStream.Seek(LongInt(Result * FBlockSize), soFromBeginning);
  len := GetVirtualTypeSize(Value);
  bh4.Sign := DBT4_BLOCK_SIGN;
  bh4.Size := len + SizeOf(TDBT4BlockHeader);
  FFileStream.Write(bh4, SizeOf(TDBT4BlockHeader));
  InternalWrite(Value, AsBinary);

  Inc(len, SizeOf(TDBT4BlockHeader));
  FillBlockFooter(len, blocks);

  // adjust NextBlock and store header
  PDBT4Header(@FHeader[0]).NextBlock := Result + blocks;
  Assert(FFileStream.Position = PDBT4Header(@FHeader[0]).NextBlock * FBlockSize);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
end;

procedure TDBFIVMemo.ZapMemo;
begin
  FNextBlock := 1;
  PDBT3Header(@FHeader[0]).NextBlock := FNextBlock;
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
  FFileStream.Size := Int64(FBlockSize);
end;

{ TDBFFoxProMemo }

procedure TDBFFoxProMemo.CreateMemo;
var
  pFPTHdr: PFPTHeader;
  written: integer;
begin
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FBlockSize := $40;
  FNextBlock := 8;
  SetLength(FHeader, SizeOf(TFPTHeader));
  SetLength(FBuffer, FBlockSize * FNextBlock);
  pFPTHdr := @FBuffer[0];
  pFPTHdr.NextBlock := FastSwap(FNextBlock);
  pFPTHdr.BlockSize := FastSwap(FBlockSize);

  Move(FBuffer[0], FHeader[0], SizeOf(TFPTHeader)); // copy to FHeader

  written := FFileStream.Write(FBuffer[0], Length(FBuffer));
  if written <> Length(FBuffer) then
    raise Exception.CreateFmt('Write error, written %d instead of %d', [written, Length(FBuffer)]);
end;

procedure TDBFFoxProMemo.Open;
var
  readed: integer;
begin
  FFileStream.Seek(LongInt(0), soFromBeginning);

  SetLength(FHeader, SizeOf(TFPTHeader));
  readed := FFileStream.Read(FHeader[0], SizeOf(TFPTHeader));
  if readed <> SizeOf(TFPTHeader) then
    raise Exception.CreateFmt(SMemoOpenError, [FFileName]);
  FBlockSize := FastSwap(PFPTHeader(@FHeader[0]).BlockSize);
  FBufferSize := FBlockSize * 1024;
  SetLength(FBuffer, FBufferSize);
  FNextBlock := FastSwap(PFPTHeader(@FHeader[0]).NextBlock);
end;

procedure TDBFFoxProMemo.Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False);
var
  readed: integer;
  count, bytesLeft: Cardinal;
begin
  FFileStream.Seek(LongInt(StartIndex * FBlockSize), soFromBeginning);
  Blob.RollbackEnabled := False;
  try
    readed := FFileStream.Read(FBuffer[0], SizeOf(TFPTBlockHeader));
    if readed <> SizeOf(TFPTBlockHeader) then
      raise Exception.CreateFmt('BLOB read error, %d bytes instead of %d', [readed, SizeOf(TFPTBlockHeader)]);

    bytesLeft := FastSwap(PFPTBlockHeader(@FBuffer[0]).Size);

    while bytesLeft > 0 do begin
      count := bytesLeft;
      if count > FBufferSize then
        count := FBufferSize;
      readed := FFileStream.Read(FBuffer[0], count);
      InternalRead(Blob, FBuffer, 0, readed, AsBinary);
      Dec(bytesLeft, readed);
    end;
  finally
    Blob.RollbackEnabled := True;
  end;
end;

function TDBFFoxProMemo.Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer;
var
  len, blocks: integer;
  bhfp: TFPTBlockHeader;
begin
  // result is MemoIdx
  Result := FastSwap(PFPTHeader(@FHeader[0]).NextBlock);
  FFileStream.Seek(LongInt(Result * FBlockSize), soFromBeginning);
  len := Size;
  bhfp.Sign := FastSwap(integer(1)); // text (memo field type)
  bhfp.Size := FastSwap(len);
  FFileStream.Write(bhfp, SizeOf(TFPTBlockHeader));
  InternalWrite(Value, AsBinary);

  Inc(len, SizeOf(TFPTBlockHeader));
  FillBlockFooter(len, blocks);

  // adjust NextBlock and store header
  PFPTHeader(@FHeader[0]).NextBlock := FastSwap(Result + blocks);
  Assert(FFileStream.Position = FastSwap(PFPTHeader(@FHeader[0]).NextBlock) * FBlockSize);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
end;

procedure TDBFFoxProMemo.ZapMemo;
begin
  FNextBlock := 8;
  PDBT3Header(@FHeader[0]).NextBlock := FastSwap(FNextBlock); // 512/64
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
  FFileStream.Size := Int64(FNextBlock * FBlockSize); // FFileStream.Size := 512;
end;

{ TDBFHiPerSixMemo }

procedure TDBFHiPerSixMemo.CreateMemo;
var
  pSMTHdr: PSMTHeader;
  len, written: integer;
  aStr: AnsiString;
begin
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FBlockSize := $40;
  FNextBlock := 8;
  SetLength(FHeader, SizeOf(TSMTHeader));
  SetLength(FBuffer, FBlockSize * FNextBlock);
  pSMTHdr := @FBuffer[0];
  pSMTHdr.NextBlock := FNextBlock;
  pSMTHdr.BlockSize := FBlockSize;

  aStr := 'SIxMemo';
  len := Length(aStr);
  if len > 8 then
    len := 8;
  Move(PAnsiChar(aStr)^, pSMTHdr.TableName[0], len);

  Move(FBuffer[0], FHeader[0], SizeOf(TSMTHeader)); // copy to FHeader

  written := FFileStream.Write(FBuffer[0], Length(FBuffer));
  if written <> Length(FBuffer) then
    raise Exception.CreateFmt('Write error, written %d instead of %d', [written, Length(FBuffer)]);
end;

procedure TDBFHiPerSixMemo.Open;
var
  readed: integer;
begin
  FFileStream.Seek(LongInt(0), soFromBeginning);

  SetLength(FHeader, SizeOf(TSMTHeader));
  readed := FFileStream.Read(FHeader[0], SizeOf(TSMTHeader));
  if readed <> SizeOf(TSMTHeader) then
    raise Exception.CreateFmt(SMemoOpenError, [FFileName]);
  FBlockSize := PSMTHeader(@FHeader[0]).BlockSize;
  SetLength(FBuffer, FBlockSize);
  FNextBlock := PSMTHeader(@FHeader[0]).NextBlock;
end;

procedure TDBFHiPerSixMemo.Read(Blob: TBlob; StartIndex: integer; Len: integer = 0; AsBinary: boolean = False);
var
  readed, bytesLeft: integer;
begin
  FFileStream.Seek(LongInt(StartIndex * FBlockSize), soFromBeginning);
  Blob.RollbackEnabled := False;
  try
    bytesLeft := Len;
    while bytesLeft > 0 do begin
      readed := FFileStream.Read(FBuffer[0], Length(FBuffer));
      InternalRead(Blob, FBuffer, 0, readed, AsBinary);
      Dec(bytesLeft, readed);
    end;
  finally
    Blob.RollbackEnabled := True;
  end;
end;

function TDBFHiPerSixMemo.Write(const Value: TVirtualValue; Size: integer; AsBinary: boolean): integer;
var
  blocks: integer;
begin
  // result is offset to Memo in block sizes
  // pad file
  FFileStream.Seek(LongInt(0), soFromEnd);
  FillBlockFooter(FFileStream.Size, Result);

  InternalWrite(Value, AsBinary);

  // adjust NextBlock and store header
  blocks := FFileStream.Size div FBlockSize;
  if (FFileStream.Size mod FBlockSize) > 0 then
    Inc(blocks);
  PSMTHeader(@FHeader[0]).NextBlock := blocks;
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
end;

procedure TDBFHiPerSixMemo.ZapMemo;
begin
  FNextBlock := 8;
  PSMTHeader(@FHeader[0]).NextBlock := FNextBlock;
  FFileStream.Seek(LongInt(0), soFromBeginning);
  FFileStream.Write(FHeader[0], Length(FHeader));
  FFileStream.Size := Int64(FNextBlock * FBlockSize);
end;

{$ENDIF}

end.
