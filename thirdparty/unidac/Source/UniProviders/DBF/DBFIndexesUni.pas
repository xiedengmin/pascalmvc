
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFIndexesUni;

interface

{$IFDEF DBFENGINE}
uses
{$IFDEF LOG_PACKETS}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$ENDIF}
  SysUtils, Classes, DateUtils,
{$IFDEF LOG_PACKETS}
  LogHandler,
{$ENDIF}
  FmtBcd, CRTypes, CRFunctions, CRParser,
{$IFNDEF UNIDACPRO}
  DBFConsts, DBFStructs, DBFUtils, DBFParser;
{$ELSE}
  DBFConstsUni, DBFStructsUni, DBFUtilsUni, DBFParserUni;
{$ENDIF}

type
  TIndexFileType = (itNDX, itMDX, itCDX, itNTX, itNSX);
  TMDXKeyFormat = (kfMdxCalculated, kfMdxData);

  PDBFLinkedListEntry = ^TDBFLinkedListEntry;
  TDBFLinkedListEntry = record
    Parent, Child: PDBFLinkedListEntry;
    BlockNo: integer; // MDX: offset = BlockNo * PageSize; CDX: pure offset
    EntryNo: integer;
    IsLeaf: boolean;
  end;

  TDBFExpressionArray = TList;

  TIndexFieldValue = record
    FieldNo: integer;
    IndexName: string;
  end;
  TIndexFieldArray = array of TIndexFieldValue;

  TDBFIndexClass = class of TDBFIndexFile;

  // index files RecNo 1 based
  TDBFIndexFile = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FDBF: TObject; // TDBFDBase;
    FPageSize: integer;
    FFileName: string;
    FFileStream: TFileStream;
    FExpressions: TDBFExpressionArray;

    function Compare(K1, K2: PAnsiChar; KeyType: AnsiChar; KeyLen: integer): integer; virtual; abstract;
    procedure SeekBlock(BlockNo: integer); virtual; abstract;
    procedure ReadBlock(BlockNo: integer; var Block: TBytes); virtual; abstract;
    procedure WriteBlock(BlockNo: integer; const Block: TBytes); virtual; abstract;
    function ParseExpression(const Str: string): TDBFExpression;
    function GetIndexNameExists(const IndexName: string): boolean;
    procedure ClearExpressions;
    procedure CreateNew(IndexFields: TIndexFieldArray); virtual; abstract;
  public
    constructor Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil); virtual;
    destructor Destroy; override;

    class function BcdToDBaseNumeric(B: PBcd): TDBaseNumeric;
    class function DBaseNumericToBcd(N: PDBaseNumeric): TBcd;

    procedure Open; virtual; abstract;
    procedure Close; virtual;
    procedure ZapIndex; virtual; abstract;
    function GetIndexNo(const IndexName: string): integer; virtual; abstract;
    function GetIndexCount: integer; virtual;
    function GetIndexName(IdxNo: integer): string; virtual; abstract;
    function GetIndexExpressionFieldName(IdxNo: integer): string;
    function GetIndexExpression(IdxNo: integer): TDBFExpression;
    function GetIndexIsValid(IdxNo: integer): boolean; virtual;
    function GetIndexIsDescending(IdxNo: integer): boolean; virtual;
    function GetIndexIsUnique(IdxNo: integer): boolean; virtual;
    function GetIndexKeyType(IdxNo: integer): AnsiChar; virtual; abstract;
    procedure SetIndexKeyType(IdxNo: integer; const KeyType: AnsiChar); virtual; abstract;
    function GetIndexKeyLength(IdxNo: integer): integer; virtual; abstract;
    function GetIndexNullable(IdxNo: integer): boolean; virtual;
    procedure GetIndexFields(List: TStringList); virtual; abstract;
    procedure AddIndex(FieldNo: integer; const IndexName, Expression: string); virtual; abstract;
    procedure DeleteIndex(const IndexName: string); virtual; abstract;

    class function LinkedListRoot(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
    class function LinkedListLeaf(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
    class function LinkedListDepth(Entry: PDBFLinkedListEntry): integer;
    class function CloneLinkedList(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
    class procedure FreeLinkedList(var Entry: PDBFLinkedListEntry);

    function IncEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; virtual; abstract;
    function DecEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; virtual; abstract;

    { Search }
    function GetLeftmostEntry(IdxNo: integer): PDBFLinkedListEntry; virtual; abstract;
    function GetRightmostEntry(IdxNo: integer): PDBFLinkedListEntry; virtual; abstract;
    function FindFirstEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; virtual; abstract;
    function FindLastEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; virtual; abstract;
    function GetEqualRightEntry(FirstEntry: PDBFLinkedListEntry; IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; virtual; abstract;
    procedure FillListByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList); virtual; abstract;
    procedure CheckPathByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList); virtual;
    function Find(Parent: PDBFLinkedListEntry; BlockNo, IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; virtual; abstract;

    { Tree manipulation }
    // (even deleted records still indexed)
    procedure Add(IdxNo, RecNo: integer; Value: PAnsiChar); virtual; abstract;
    procedure Delete(IdxNo, RecNo: integer; Value: PAnsiChar); virtual; abstract;
  {$IFDEF LOG_PACKETS}
    procedure Dump(BlockNo, IdxNo: integer); virtual;
  {$ENDIF}

    property FileName: string read FFileName;
  end;

  TMDXFile = class(TDBFIndexFile)
  private
    FHeader: TMDXHeader;
    FTags: array of TMDXTag;
    FIndexHeaders: array of TIndexHeader;
    FKeyBuffers: array of TBytes; // to reduce memory allocation for each key copy

    function GetEntry(const Block: TBytes; IdxNo, EntryNo: integer; out Valid: boolean): PMDXEntry;
    function GetEntryValue(Entry: PMDXEntry; IdxNo: integer): PAnsiChar;
    function CompareEntryValue(Entry: PMDXEntry; IdxNo: integer; Value: PAnsiChar): integer;
    procedure SetIsLeaf(var Block: TBytes; IdxNo: integer);
    function CreateBlock(IdxNo: integer; out BlockNo: integer): TBytes;
    procedure WriteHeader;
    procedure WriteIndexHeader(IdxNo: integer);

    procedure ClearEmptySpace(Entry: PDBFLinkedListEntry; IdxNo: integer; var Block: TBytes; EntryNo: integer);
    procedure RemoveEntry(Entry: PDBFLinkedListEntry; IdxNo: integer; var Block: TBytes; EntryNo: integer);
    procedure InsertIntoNode(var Entry: PDBFLinkedListEntry; IdxNo, LeftBlockNo, RightBlockNo: integer; Value: PAnsiChar);
  protected
    procedure CreateNew(IndexFields: TIndexFieldArray); override;

    function Compare(K1, K2: PAnsiChar; KeyType: AnsiChar; KeyLen: integer): integer; override;
    procedure SeekBlock(BlockNo: integer); override;
    procedure ReadBlock(BlockNo: integer; var Block: TBytes); override;
    procedure WriteBlock(BlockNo: integer; const Block: TBytes); override;
    function GetIsLeaf(const Block: TBytes; IdxNo: integer): boolean;
  public
    constructor Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil); override;
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    procedure ZapIndex; override;
    procedure GetIndexFields(List: TStringList); override;
    function GetIndexNo(const IndexName: string): integer; override;
    function GetIndexCount: integer; override;
    function GetIndexName(IdxNo: integer): string; override;
    function GetIndexIsValid(IdxNo: integer): boolean; override;
    function GetIndexIsDescending(IdxNo: integer): boolean; override;
    function GetIndexIsUnique(IdxNo: integer): boolean; override;
    function GetIndexKeyType(IdxNo: integer): AnsiChar; override;
    procedure SetIndexKeyType(IdxNo: integer; const KeyType: AnsiChar); override;
    function GetIndexKeyLength(IdxNo: integer): integer; override;
    procedure AddIndex(FieldNo: integer; const IndexName, Expression: string); override;
    procedure DeleteIndex(const IndexName: string); override;

    function IncEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; override;
    function DecEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; override;

    { Search }
    function GetLeftmostEntry(IdxNo: integer): PDBFLinkedListEntry; override;
    function GetRightmostEntry(IdxNo: integer): PDBFLinkedListEntry; override;
    function FindFirstEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    function FindLastEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    function GetEqualRightEntry(FirstEntry: PDBFLinkedListEntry; IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    procedure FillListByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList); override;
    procedure CheckPathByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList); override;
    function Find(Parent: PDBFLinkedListEntry; BlockNo, IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;

    { Tree manipulation }
    procedure Add(IdxNo, RecNo: integer; Value: PAnsiChar); override;
    procedure Delete(IdxNo, RecNo: integer; Value: PAnsiChar); override;
  end;

  PCDXCompKeyParams = ^TCDXCompKeyParams;
  TCDXCompKeyParams = record
    RecNo: integer;
    DupCount: integer;
    TrailCount: integer;
  end;

  TCDXIndexInfo = record
    Name: AnsiString;
    Expression: AnsiString;
    Filter: AnsiString;
    KeyType: AnsiChar;
    Valid: boolean; // we can't use expressions yet
    IsDesc: boolean;
    RootNode: integer;
    RecNo: integer;
    KeyLength: Word;
    KeyRecLen: integer;
    Options: Byte;
    Nullable: boolean;
  end;

  TCDXIndexInfoArray = array of TCDXIndexInfo;

  TCDXFile = class(TDBFIndexFile)
  private
    FHeader: TCDXHeader;
    FIndexInfos: TCDXIndexInfoArray;

    procedure GetIndexKey(PNode: PCDXCompactIndexNode; KeyLen: integer; KeyType: AnsiChar; var Offs: integer;
      const CompKeyParams: TCDXCompKeyParams; var PrevKey: PAnsiChar; Key: PAnsiChar);
    procedure ExtractIdxInfo(PNode: PCDXCompactIndexNode; Idx: integer; var CompKeyParams: TCDXCompKeyParams);
    procedure CompactIdxInfo(PNode: PCDXCompactIndexNode; Idx: integer; const CompKeyParams: TCDXCompKeyParams);
    function ExtractCdxNode(PNode: PCDXCompactIndexNode; IdxNo: integer): TBytes;
    function CompressCdxNode(IdxNo: integer; Attributes: Word; LeftNode, RightNode: Integer;
      const KeyInfos: TBytes; var NewBlock: TBytes): boolean;

    procedure UpdateNodeEntry(Entry: PDBFLinkedListEntry; IdxNo, UpdateEntryNo: integer; Value: PAnsiChar);
    procedure UpdateNodeOffsets(Offset, LeftBlockNo, RightBlockNo: integer);
    procedure RemoveEntry(Entry: PDBFLinkedListEntry; IdxNo: integer; Node: PCDXCompactIndexNode; var Block: TBytes; EntryNo: integer);
    procedure InsertIntoNode(var Entry: PDBFLinkedListEntry; IdxNo, LeftBlockNo, RightBlockNo: integer; Value: PAnsiChar);
  protected
    function Compare(K1, K2: PAnsiChar; KeyType: AnsiChar; KeyLen: integer): integer; override;
    procedure SeekBlock(BlockNo: integer); override;
    procedure ReadBlock(BlockNo: integer; var Block: TBytes); override;
    procedure WriteBlock(BlockNo: integer; const Block: TBytes); override;
    procedure AppendBlock(const Block: TBytes);
    function GetRootNodeBlockNo(IdxNo, BlockNo: integer): integer;
    function GetIsLeaf(PNode: PCDXCompactIndexNode; IdxNo: integer): boolean;
    procedure CreateNew(IndexFields: TIndexFieldArray); override;
  public
    constructor Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil); override;
    destructor Destroy; override;

    procedure Open; override;
    procedure Close; override;
    procedure ZapIndex; override;
    procedure GetIndexFields(List: TStringList); override;
    function GetIndexNo(const IndexName: string): integer; override;
    function GetIndexCount: integer; override;
    function GetIndexName(IdxNo: integer): string; override;
    function GetIndexIsValid(IdxNo: integer): boolean; override;
    function GetIndexIsDescending(IdxNo: integer): boolean; override;
    function GetIndexIsUnique(IdxNo: integer): boolean; override;
    function GetIndexKeyType(IdxNo: integer): AnsiChar; override;
    procedure SetIndexKeyType(IdxNo: integer; const KeyType: AnsiChar); override;
    function GetIndexKeyLength(IdxNo: integer): integer; override;
    function GetIndexKeyRecLength(IdxNo: integer): integer;
    function GetIndexNullable(IdxNo: integer): boolean; override;
    procedure AddIndex(FieldNo: integer; const IndexName, Expression: string); override;
    procedure DeleteIndex(const IndexName: string); override;

    function IncEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; override;
    function DecEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean; override;

    { Search }
    function GetLeftmostEntry(IdxNo: integer): PDBFLinkedListEntry; override;
    function GetRightmostEntry(IdxNo: integer): PDBFLinkedListEntry; override;
    function FindFirstEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    function FindLastEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    function GetEqualRightEntry(FirstEntry: PDBFLinkedListEntry; IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;
    procedure FillListByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList); override;
    function Find(Parent: PDBFLinkedListEntry; BlockNo, IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry; override;

    { Tree manipulation }
    procedure Add(IdxNo, RecNo: integer; Value: PAnsiChar); override;
    procedure Delete(IdxNo, RecNo: integer; Value: PAnsiChar); override;
  {$IFDEF LOG_PACKETS}
    procedure Dump(BlockNo, IdxNo: integer); override;
  {$ENDIF}
  end;

  TNSXFile = class(TDBFIndexFile)

  end;

{$IFDEF LOG_PACKETS}
var
  AddIndexTickCount, AddIndexStartTick, CompareTickCount, CompareStartTick: Cardinal;
{$ENDIF}

{$ENDIF}

implementation

{$IFDEF DBFENGINE}
uses
  Math,
{$IFNDEF UNIDACPRO}
  DBFDBase;
{$ELSE}
  DBFDBaseUni;
{$ENDIF}

  // All column names must be valid.
  // All columns must be in the same ascending or descending order.
  // The length of any single text column must be less than 100 bytes.
  // If more than one column exists, all of the columns must be text columns and the sum of the column sizes must be less than 100 bytes.
  // Memo fields cannot be indexed.
  // An index must not be specified for the current set of fields (that is, duplicate indexes are not allowed).
  // The index name must match the dBASE index naming convention. dBASE III requires that each index be in a separate file, each having an .ndx extension. In dBASE IV, indexes are created as tag names that are stored in a single .mdx file. The .mdx file has the same base name as the database file (for example, Emp.mdx is the index file for the Emp.dbf database).
  // dBASE defines a unique index as one where only one record from a set with identical key values is added to the index.

  // Field types that can be indexed
  // DBase 7: Autoincrement, Character, Numeric, Date, Float, Long, Timestamp, Double

  // index record's padding 4?

function CompareBytes(K1, K2: PAnsiChar; Count: integer): integer;
var
  i: integer;
  pb1, pb2: PByte;
begin
  Result := 0;
  pb1 := PByte(K1);
  pb2 := PByte(K2);
  for i := 0 to Count - 1 do
    if pb1^ = pb2^ then begin
      Inc(pb1);
      Inc(pb2);
    end
    else begin
      if pb1^ < pb2^ then
        Result := -1
      else
        Result := 1;
      Break;
    end;
end;

function CompareChars(K1, K2: PAnsiChar; Count: integer): integer;
var
  i: integer;
  pb1, pb2: PByte;
begin
  Result := 0;
  pb1 := PByte(K1);
  pb2 := PByte(K2);
  for i := 0 to Count - 1 do begin
    if pb1^ < pb2^ then begin
      Result := -1;
      Break;
    end
    else
    if pb1^ > pb2^ then begin
      Result := 1;
      Break;
    end;
    if (pb1^ = 0) or (pb2^ = 0) then
      Break;
    Inc(pb1);
    Inc(pb2);
  end;
end;

{ TDBFIndex }

constructor TDBFIndexFile.Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil);
begin
  inherited Create;

  FExpressions := TDBFExpressionArray.Create;

  FDBF := Parent;
  FPageSize := 512;
  FFileName := FileName;
  if not New then
    FFileStream := TFileStream.Create(FFileName, TDBFDBase(FDBF).GetFileMode(TDBFDBase(FDBF).ForceReadOnly, TDBFDBase(FDBF).ForceExclusive));
end;

destructor TDBFIndexFile.Destroy;
begin
  Close;

  ClearExpressions;
  FExpressions.Free;

  inherited;
end;

procedure TDBFIndexFile.Close;
begin
  FreeAndNil(FFileStream);
  ClearExpressions;
end;

function TDBFIndexFile.GetIndexCount: integer;
begin
  Result := 0;
end;

function TDBFIndexFile.GetIndexNullable(IdxNo: integer): boolean;
begin
  Result := False;
end;

function TDBFIndexFile.GetIndexIsValid(IdxNo: integer): boolean;
begin
  Result := False;
end;

function TDBFIndexFile.GetIndexIsDescending(IdxNo: integer): boolean;
begin
  Result := False;
end;

function TDBFIndexFile.GetIndexIsUnique(IdxNo: integer): boolean;
begin
  Result := False;
end;

function TDBFIndexFile.GetIndexNameExists(const IndexName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to GetIndexCount - 1 do begin
    Result := SameText(IndexName, GetIndexName(i));
    if Result then
      Break;
  end;
end;

procedure TDBFIndexFile.ClearExpressions;
var
  i: integer;
begin
  if (FExpressions <> nil) and (FExpressions.Count > 0) then
    for i := 0 to FExpressions.Count - 1 do
      TDBFExpression(FExpressions[i]){$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
  FExpressions.Clear;
end;

function TDBFIndexFile.GetIndexExpressionFieldName(IdxNo: integer): string;
begin
  if (TDBFExpression(FExpressions[IdxNo]).ExpressionType = etField) then
    Result := TDBFExpression(FExpressions[IdxNo]).Fields[0]
  else
    Result := '';
end;

function TDBFIndexFile.GetIndexExpression(IdxNo: integer): TDBFExpression;
begin
  Result := FExpressions[IdxNo];
end;

function TDBFIndexFile.ParseExpression(const Str: string): TDBFExpression;
var
  Parser: TDBFExpressionParserExt;
  Normalizer: TDBFExpressionNormalizer;
begin
  Parser := TDBFExpressionParserExt.Create(Str);
  try
    Result := Parser.Parse(Str);

    if (Result <> nil) and (Result.ExpressionType = etField) then begin
      Result.FieldNo := TDBFDBase(FDBF).GetFieldNo(Result.Fields[0]);
      Exit;
    end;

    Normalizer := TDBFExpressionNormalizer.Create;
    try
      Result := Normalizer.VisitExpression(Result);
    finally
      Normalizer.Free;
    end;
  finally
    Parser.Free;
  end;
end;

class function TDBFIndexFile.DBaseNumericToBcd(N: PDBaseNumeric): TBcd;
var
  IntLen, Precision, Scale, Sign: integer;
begin
  FillChar(Result, SizeOf(TBcd), 0);

  IntLen := N.IntLen - $34;
  Sign := N.SignScaleUnk and $80; // as is
  Precision := (N.SignScaleUnk and $7C) shr 2;
  if IntLen > Precision then
  	Precision := IntLen;
  Scale := Precision - IntLen;

  Result.Precision := Precision;
  Result.SignSpecialPlaces := Sign or (Scale and $3F);
  Move(N.Fraction, Result.Fraction, SizeOf(N.Fraction));
end;

class function TDBFIndexFile.BcdToDBaseNumeric(B: PBcd): TDBaseNumeric;
var
  IntLen, Scale, Sign: integer;
begin
  FillChar(Result, SizeOf(TDBaseNumeric), 0);

  Sign := B.SignSpecialPlaces and $80; // as is
  Scale := B.SignSpecialPlaces and $3F;
  IntLen := B.Precision - Scale + $34;

  Result.IntLen := IntLen;
  Result.SignScaleUnk := Sign or ((Scale and $1F) shl 2) or 1;
  Move(B.Fraction, Result.Fraction, SizeOf(Result.Fraction));
end;

class function TDBFIndexFile.LinkedListRoot(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
begin
  Result := Entry;
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

class function TDBFIndexFile.LinkedListLeaf(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
begin
  Result := Entry;
  while Result.Child <> nil do
    Result := Result.Child;
end;

class function TDBFIndexFile.LinkedListDepth(Entry: PDBFLinkedListEntry): integer;
var
  temp: PDBFLinkedListEntry;
begin
  temp := Entry;
  if temp.Child <> nil then
    temp := LinkedListLeaf(temp);

  Result := 1;
  while temp.Parent <> nil do begin
    temp := temp.Parent;
    Inc(Result);
  end;
end;

class function TDBFIndexFile.CloneLinkedList(Entry: PDBFLinkedListEntry): PDBFLinkedListEntry;
var
  parent, temp: PDBFLinkedListEntry;
begin
  Result := nil;
  temp := LinkedListRoot(Entry);
  while True do begin
    parent := Result;
    Result := AllocMem(SizeOf(TDBFLinkedListEntry));
    Move(temp^, Result^, SizeOf(TDBFLinkedListEntry));
    Result.Parent := parent;
    if Result.Parent <> nil then
      Result.Parent.Child := Result;
    if temp.Child = nil then
      Break;
    temp := temp.Child;
  end;
end;

class procedure TDBFIndexFile.FreeLinkedList(var Entry: PDBFLinkedListEntry);
var
  curEntry, parentEntry: PDBFLinkedListEntry;
begin
  parentEntry := LinkedListLeaf(Entry);
  repeat
    curEntry := parentEntry;
    parentEntry := parentEntry.Parent;
    FreeMem(curEntry);
  until parentEntry = nil;

  Entry := nil;
end;

procedure TDBFIndexFile.CheckPathByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList);
begin
  // nothing
end;

{$IFDEF LOG_PACKETS}
procedure TDBFIndexFile.Dump(BlockNo, IdxNo: integer);
begin
  // nothing
end;
{$ENDIF}

{ TMDXFile }

constructor TMDXFile.Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil);
begin
  inherited Create(Parent, FileName, New);

  if New then
    CreateNew(IndexFields);
end;

destructor TMDXFile.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FKeyBuffers) - 1 do
    FKeyBuffers[i] := nil;

  inherited;
end;

procedure TMDXFile.AddIndex(FieldNo: integer; const IndexName, Expression: string);
var
  idx, len, written: integer;
  fieldPtr: PDBFField;
  tag: TMDXTag;
  pIndexHdr: PIndexHeader;
  s: string;
  aStr: AnsiString;
  year, month, day: Word;
  buf: TBytes;
begin
  fieldPtr := TDBFDBase(FDBF).GetField(FieldNo);

  s := IndexName;
  if s = '' then
    s := TDBFDBase(FDBF).GetFieldName(FieldNo);
  if GetIndexNameExists(s) then
    raise Exception.CreateFmt('Index %s already exists', [IndexName]);
  aStr := AnsiString(UpperCase(s));
  len := LengthA(aStr);

  Inc(FHeader.TagsUsed);
  if FHeader.TagsUsed > MDX_MAX_INDEX_COUNT then
    raise Exception.Create('Too much indexes');

  idx := Length(FTags);

  FillChar(tag, SizeOf(tag), 0);
  tag.HeaderPageNo := FHeader.NumPages;
  Inc(FHeader.NumPages, FHeader.BlockSize);

  if FHeader.MdxVersion = 3 then begin
    if len > Length(tag.Ver3TagName) then
      len := Length(tag.Ver3TagName);
    Move(PAnsiChar(aStr)^, tag.Ver3TagName[0], len);

    tag.Ver3TagInfo.KeyFormat := $10; // Data Field
    tag.Ver3TagInfo.ForwardTagLess := 0;
    tag.Ver3TagInfo.ForwardTagMore := 0;
    tag.Ver3TagInfo.BackwardTag := 0;
    tag.Ver3TagInfo.Reserved := 2;
    tag.Ver3TagInfo.KeyType := Dbf7FieldTypeToIndexType[Byte(fieldPtr.DB7FType)];
    if Byte(tag.Ver3TagInfo.KeyType) = 0 then
      raise Exception.CreateFmt('Unknown index type for field type %s', [Char(fieldPtr.DB7FType)]);
  end
  else begin
    if len > Length(tag.Ver2TagName) then
      len := Length(tag.Ver2TagName);
    Move(PAnsiChar(aStr)^, tag.Ver2TagName[0], len);

    tag.Ver2TagInfo.KeyFormat := $10; // Data Field
    tag.Ver2TagInfo.ForwardTagLess := 0;
    tag.Ver2TagInfo.ForwardTagMore := 0;
    tag.Ver2TagInfo.BackwardTag := 0;
    tag.Ver2TagInfo.Reserved := 2;
    tag.Ver2TagInfo.KeyType := Dbf4FieldTypeToIndexType[Byte(fieldPtr.DB3FType)];
    if Byte(tag.Ver2TagInfo.KeyType) = 0 then
      raise Exception.CreateFmt('Unknown index type for field type %s', [Char(fieldPtr.DB3FType)]);
  end;

  // store tag
  SetLength(FTags, idx + 1);
  Move(tag, FTags[idx], FHeader.TagSize);
  FFileStream.Seek(LongInt($200 + (idx + 1) * FHeader.TagSize), soFromBeginning);
  written := FFileStream.Write(tag, FHeader.TagSize);
  if written <> FHeader.TagSize then
    raise Exception.Create('Write MDX tag exception');

  // create index header
  SetLength(FIndexHeaders, idx + 1);
  pIndexHdr := @FIndexHeaders[idx];
  pIndexHdr.RootPage := FHeader.NumPages;
  pIndexHdr.NumPages := FHeader.BlockSize;
  Inc(FHeader.NumPages, pIndexHdr.NumPages);
  pIndexHdr.KeyFormat := $10; // todo Desc, Unique, another dBaseVII has F0 here for Int/Primary
  if TDBFDBase(FDBF).GetFieldIsKey(FieldNo) then
    pIndexHdr.KeyFormat := pIndexHdr.KeyFormat or $40;
  if FHeader.MdxVersion = 3 then begin
    pIndexHdr.KeyType := tag.Ver3TagInfo.KeyType; // C, N, D, I, +, @
    pIndexHdr.Dummy := $0000;
    pIndexHdr.KeyLen := DbfIndexTypeToLen[Byte(pIndexHdr.KeyType)];
    if pIndexHdr.KeyLen = 0 then
      pIndexHdr.KeyLen := fieldPtr.DB7Len;
  end
  else begin
    pIndexHdr.KeyType := tag.Ver2TagInfo.KeyType; // C, N, D
    pIndexHdr.Dummy := $0000;
    PByte(@pIndexHdr.Dummy)^ := LanguageDriverID[TDBFDBase(FDBF).CodePage];
    pIndexHdr.KeyLen := DbfIndexTypeToLen[Byte(pIndexHdr.KeyType)];
    if pIndexHdr.KeyLen = 0 then
      pIndexHdr.KeyLen := fieldPtr.DB3Len;
  end;

  pIndexHdr.KeyRecLen := pIndexHdr.KeyLen + 4;
  pIndexHdr.MaxNumKeys := (FPageSize * FHeader.BlockSize - SizeOf(TMDXBlock) - 4) div pIndexHdr.KeyRecLen;
  pIndexHdr.Version := 1;

  FExpressions.Add(ParseExpression(Expression));
  s := Expression;
  if s = '' then
    s := TDBFDBase(FDBF).GetFieldName(FieldNo);
  aStr := AnsiString(UpperCase(s));
  Move(PAnsiChar(aStr)^, pIndexHdr.Expression[0], LengthA(aStr));

  pIndexHdr.FirstBlock := pIndexHdr.RootPage;
  pIndexHdr.LastBlock := pIndexHdr.RootPage;
  DecodeDate(Now, year, month, day);
  pIndexHdr.UpdYear := Byte(year - 1900);
  pIndexHdr.UpdMonth := month;
  pIndexHdr.UpdDay := day;

  SetLength(FKeyBuffers, idx + 1);
  SetLength(FKeyBuffers[idx], FIndexHeaders[idx].KeyLen);

  SetLength(buf, FPageSize * FHeader.BlockSize);
  // store IndexHeader
  FillChar(buf[0], Length(buf), 0);
  Move(FIndexHeaders[idx], buf[0], SizeOf(TIndexHeader));
  WriteBlock(tag.HeaderPageNo, buf);

  // store empty Root
  FillChar(buf[0], Length(buf), 0);
  WriteBlock(pIndexHdr.RootPage, buf);

  // save Header changes
  WriteHeader;
end;

procedure TMDXFile.DeleteIndex(const IndexName: string);
var
  s: string;
  i, TagOffset, written: integer;
  temp: array of TMDXTag;
begin
  s := UpperCase(IndexName);
  if FHeader.MdxVersion = 3 then begin
    if Length(s) > 33 then // Length(TMDXTag.Ver3TagName)
      SetLength(s, 33);
  end
  else
    if Length(s) > 11 then // Length(TMDXTag.Ver2TagName)
      SetLength(s, 11);

  if not GetIndexNameExists(s) then
    raise Exception.CreateFmt('Index %s not found', [IndexName]);

  // remove from tags
  // there is no reason to drop TDBFDBase(FDBF).SetFieldConstraint by FExpressions[i].FieldNo
  // due to field can be used in several indexes
  temp := nil;
  for i := 0 to Length(FTags) - 1 do
    if GetIndexName(i) <> IndexName then begin
      SetLength(temp, Length(temp) + 1);
      temp[High(temp)] := FTags[i];
    end;

  // store new tags
  TagOffset := FPageSize + FHeader.TagSize;
  for i := 0 to Length(temp) - 1 do begin
    FTags[i] := temp[i];
    FFileStream.Seek(LongInt(TagOffset + i * FHeader.TagSize), soFromBeginning);
    written := FFileStream.Write(temp[i], FHeader.TagSize);
    if written <> FHeader.TagSize then
      raise Exception.Create('TMDXFile.Open tag exception');
  end;
  SetLength(FTags, Length(temp));
  // save new tag count
  FHeader.TagsUsed := Length(temp);
  WriteHeader;
end;

procedure TMDXFile.CreateNew(IndexFields: TIndexFieldArray);
var
  i, len, written: integer;
  hdrBuf, buf: TBytes;
  year, month, day: Word;
  s: string;
  aStr: AnsiString;
begin
  if Length(IndexFields) = 0 then
    Exit;

  FreeAndNil(FFileStream);
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FTags := nil;
  FIndexHeaders := nil;
  FKeyBuffers := nil;
  SetLength(hdrBuf, FPageSize);

  if TDBFDBase(FDBF).HeaderFormat = dfdBaseVII then
    FHeader.MdxVersion := 3
  else
    FHeader.MdxVersion := 2;
  DecodeDate(Now, year, month, day);
  FHeader.Year := Byte(year - 1900);
  FHeader.Month := month;
  FHeader.Day := day;

  s := ExtractFileName(FFileName);
  s := Copy(s, 1, Length(s) - 4);
  aStr := AnsiString(s);
  len := LengthA(aStr);
  if len > 8 then
    len := 8;
  Move(PAnsiChar(aStr)^, FHeader.FileName[0], len);

  FHeader.BlockSize := 2;
  FHeader.BlockAdder := FHeader.BlockSize * FPageSize; // $400
  FHeader.ProdFlag := DBF_FLAG_HAS_MDX;
  FHeader.NumTags := $30;
  if FHeader.MdxVersion = 3 then
    FHeader.TagSize := $2B
  else
    FHeader.TagSize := $20;
  FHeader.TagsUsed := 0;
  FHeader.Dummy2 := 0;
  FHeader.Language := LanguageDriverID[TDBFDBase(FDBF).CodePage];

  FHeader.FreePage := 0;
  FHeader.BlockFree := 0;
  FHeader.UpdYear := Byte(year - 1900);
  FHeader.UpdMonth := month;
  FHeader.UpdDay := day;

  if FHeader.MdxVersion = 3 then
    SetLength(buf, $C00 - FPageSize) // index headers started at C00, buf from $200 to $C00
  else
    SetLength(buf, $800 - FPageSize); // index headers started at 800, buf from $200 to $800

  if FHeader.MdxVersion = 3 then begin
    buf[$27] := 1; // ForwardTagMore ?
    FHeader.NumPages := 6; // inc by BlockSize
  end
  else begin
    buf[$11] := 1;
    FHeader.NumPages := 4;
  end;

  // write to file so we can use size to add new indexes
  Move(FHeader, hdrBuf[0], SizeOf(FHeader));
  written := FFileStream.Write(hdrBuf[0], Length(hdrBuf));
  if written <> Length(hdrBuf) then
    raise Exception.Create('Write MDX header exception');

  written := FFileStream.Write(buf[0], Length(buf));
  if written <> Length(buf) then
    raise Exception.Create('Write MDX index tags exception');

  for i := 0 to Length(IndexFields) - 1 do
    AddIndex(IndexFields[i].FieldNo, IndexFields[i].IndexName, TDBFDBase(FDBF).GetFieldName(IndexFields[i].FieldNo));
end;

function TMDXFile.Compare(K1, K2: PAnsiChar; KeyType: AnsiChar; KeyLen: integer): integer;
var
  //as1, as2: AnsiString;
  i1, i2: integer;
  d1, d2: double;
  bcd1, bcd2: TBcd;
  dt1, dt2: TDateTime;
begin
{$IFDEF LOG_PACKETS}
  CompareStartTick := GetTickCount;
{$ENDIF}
  case KeyType of
    DBF_TYPE_INTEGER:
      Result := CompareBytes(K1, K2, SizeOf(integer));
    DBF_TYPE_AUTOINC: begin
      Assert(TDBFDBase(FDBF).HeaderFormat = dfdBaseVII, Format('dfdBaseVII only type %s', [string(AnsiString(KeyType))]));
      // big-endian
      i1 := DBase7DataToInteger(PInteger(K1)^);
      i2 := DBase7DataToInteger(PInteger(K2)^);
      Result := Math.CompareValue(i1, i2);
    end;
    DBF_TYPE_CHAR: begin
      {SetLengthA(as1, KeyLen);
      SetLengthA(as2, KeyLen);
      Move(K1^, PAnsiChar(as1)^, KeyLen);
      Move(K2^, PAnsiChar(as2)^, KeyLen);
      Result := CompareChars(PAnsiChar(as1), PAnsiChar(as2), KeyLen);}
      Result := CompareChars(K1, K2, KeyLen);
    end;
    DBF_TYPE_NUMERIC: begin
      bcd1 := DBaseNumericToBcd(PDBaseNumeric(K1));
      bcd2 := DBaseNumericToBcd(PDBaseNumeric(K2));
      Result := BcdCompare(bcd1, bcd2);
    end;
    DBF_TYPE_DATE: begin // string YYYYMMDD like 20021201
      // 12:00:00 of day as dBase index value
      d1 := PDouble(K1)^;
      d2 := PDouble(K2)^;
      Result := Math.CompareValue(d1, d2);
    end;
    DBF_TYPE_TIMESTAMP: begin
      dt1 := DBaseTimeStampToDateTime(PInt64(K1)^);
      dt2 := DBaseTimeStampToDateTime(PInt64(K2)^); // RecodeMilliSecond(, 0): test data filled with milliseconds
      Result := Math.CompareValue(dt1, dt2);
    end;
    DBF_TYPE_OLEBLOB: begin
      Assert(TDBFDBase(FDBF).HeaderFormat = dfdBaseVII, Format('dfdBaseVII only type %s', [string(AnsiString(KeyType))]));
      d1 := DBase7DataToDouble(PDouble(K1)^);
      d2 := DBase7DataToDouble(PDouble(K2)^);
      Result := Math.CompareValue(d1, d2);
    end;
  else
    raise Exception.CreateFmt('Unknown type %s or cannot be indexed', [KeyType]);
  end;
{$IFDEF LOG_PACKETS}
  CompareTickCount := CompareTickCount + GetTickCount - CompareStartTick;
{$ENDIF}
end;

procedure TMDXFile.GetIndexFields(List: TStringList);
var
  i: integer;
  temp: AnsiString;
begin
  List.Clear;
  List.Sorted := True;
  List.Duplicates := dupIgnore;

  for i := 0 to Length(FIndexHeaders) - 1 do begin
  {$IFNDEF NEXTGEN}
    temp := FIndexHeaders[i].Expression;
  {$ELSE}
    temp.SetLength(Length(FIndexHeaders[i].Expression));
    Move(FIndexHeaders[i].Expression[0], temp.Ptr^, temp.Length);
  {$ENDIF}

    // todo: separate field names from expressions
    if temp <> '' then
      List.Add(UpperCase(string(temp)));
  end;
end;

function TMDXFile.GetIndexNo(const IndexName: string): integer;
var
  i: integer;
  upStr: string;
begin
  Result := -1;
  upStr := UpperCase(IndexName);

  for i := 0 to Length(FIndexHeaders) - 1 do begin
  //{$IFNDEF NEXTGEN}
  //  temp := FIndexHeaders[i].Expression;
  //{$ELSE}
  //  temp.SetLength(Length(FIndexHeaders[i].Expression));
  //  Move(FIndexHeaders[i].Expression[0], temp.Ptr^, temp.Length);
  //{$ENDIF}

    if upStr = UpperCase(GetIndexName(i)) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TMDXFile.GetIndexCount: integer;
begin
  Result := Length(FIndexHeaders);
end;

function TMDXFile.GetIndexName(IdxNo: integer): string;
begin
  // Get and Set can be Alter related
  if TDBFDBase(FDBF).HeaderFormat = dfdBaseVII then
    Result := Ver3TagNameToString(FTags[IdxNo])
  else
    Result := Ver2TagNameToString(FTags[IdxNo]);
end;

function TMDXFile.GetIndexIsValid(IdxNo: integer): boolean;
begin
  Result := True; // todo if not complex expression
end;

function TMDXFile.GetIndexIsDescending(IdxNo: integer): boolean;
begin
  Result := (FIndexHeaders[IdxNo].KeyFormat and 8) <> 0;
end;

function TMDXFile.GetIndexIsUnique(IdxNo: integer): boolean;
begin
  Result := (FIndexHeaders[IdxNo].KeyFormat and $40) <> 0;
end;

function TMDXFile.GetIndexKeyType(IdxNo: integer): AnsiChar;
begin
  if TDBFDBase(FDBF).HeaderFormat = dfdBaseVII then
    Result := FTags[IdxNo].Ver3TagInfo.KeyType
  else
    Result := FTags[IdxNo].Ver2TagInfo.KeyType;
end;

procedure TMDXFile.SetIndexKeyType(IdxNo: integer; const KeyType: AnsiChar);
begin
  if TDBFDBase(FDBF).HeaderFormat = dfdBaseVII then
    FTags[IdxNo].Ver3TagInfo.KeyType := KeyType
  else
    FTags[IdxNo].Ver2TagInfo.KeyType := KeyType;
end;

function TMDXFile.GetIndexKeyLength(IdxNo: integer): integer;
begin
  Result := FIndexHeaders[IdxNo].KeyLen;
end;

procedure TMDXFile.Open;
var
  i, readed, TagOffset: integer;
  temp: AnsiString;
  Mode: Word;
  Expr: string;
begin
  Mode := fmOpenReadWrite;
  case TDBFDBase(FDBF).ConnectMode of
    cmShared:
      Mode := Mode or fmShareDenyWrite;
    cmUnsafe:
      Mode := Mode or fmShareDenyNone;
  end;

  if FFileStream = nil then
    FFileStream := TFileStream.Create(FFileName, Mode);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  readed := FFileStream.Read(FHeader, SizeOf(TMDXHeader));
  if readed <> SizeOf(TMDXHeader) then
    raise Exception.Create('TMDXFile.Open exception');

  // todo get index field names and attributes
  Assert(FHeader.TagsUsed <= MDX_MAX_INDEX_COUNT);
  SetLength(FTags, FHeader.TagsUsed);
  SetLength(FIndexHeaders, FHeader.TagsUsed);
  SetLength(FKeyBuffers, FHeader.TagsUsed);
  ClearExpressions;
  TagOffset := FPageSize + FHeader.TagSize;
  for i := 0 to FHeader.TagsUsed - 1 do begin
    // read page FHeader
    FFileStream.Seek(LongInt(TagOffset + i * FHeader.TagSize), soFromBeginning);
    readed := FFileStream.Read(FTags[i], FHeader.TagSize);
    if readed <> FHeader.TagSize then
      raise Exception.Create('TMDXFile.Open tag exception');

    FFileStream.Seek(LongInt(FTags[i].HeaderPageNo * FPageSize), soFromBeginning);
    readed := FFileStream.Read(FIndexHeaders[i], SizeOf(TIndexHeader));
    if readed <> SizeOf(TIndexHeader) then
      raise Exception.Create('TIndexHeader exception');

    SetLength(FKeyBuffers[i], FIndexHeaders[i].KeyLen);

  {$IFNDEF NEXTGEN}
    temp := FIndexHeaders[i].Expression;
  {$ELSE}
    temp.SetLength(Length(FIndexHeaders[i].Expression));
    Move(FIndexHeaders[i].Expression[0], temp.Ptr^, temp.Length);
  {$ENDIF}
    Expr := Trim(string(temp));
    if Expr = '' then
      if SuppressIndexOpenErrors then
        Exit
      else
        raise Exception.Create(SEmptyIndexExpression);
    FExpressions.Add(ParseExpression(Expr));
  end;
end;

procedure TMDXFile.Close;
begin
  FTags := nil;
  FIndexHeaders := nil;
  FKeyBuffers := nil;

  inherited;
end;

procedure TMDXFile.ZapIndex;
var
  i, idxHdrPageNo, idxRootPageNo, readed, written: integer; //TagOffset,
  buf: TBytes;
  year, month, day: Word;
  pTag: PMDXTag;
  pIndexHdr: PIndexHeader;
begin
  DecodeDate(Now, year, month, day);

  if FHeader.MdxVersion = 3 then begin
    // HeaderPage + TagsPages (5) + IndexHeaders (BlockSize * idxCount) + RootNodes (BlockSize * idxCount)
    idxRootPageNo := 1 + 5 + FHeader.BlockSize * FHeader.TagsUsed;
    FHeader.NumPages := idxRootPageNo + FHeader.BlockSize * FHeader.TagsUsed;
  end
  else begin
    // HeaderPage + TagsPages (3) + IndexHeaders (BlockSize * idxCount) + RootNodes (BlockSize * idxCount)
    idxRootPageNo := 1 + 3 + FHeader.BlockSize * FHeader.TagsUsed;
    FHeader.NumPages := idxRootPageNo + FHeader.BlockSize * FHeader.TagsUsed;
  end;
  FHeader.FreePage := 0;
  FHeader.BlockFree := 0;
  FHeader.UpdYear := Byte(year - 1900);
  FHeader.UpdMonth := month;
  FHeader.UpdDay := day;

  if FHeader.MdxVersion = 3 then
    SetLength(buf, $C00 - FPageSize) // index headers started at C00
  else
    SetLength(buf, $800 - FPageSize); // index headers started at 800

  //TagOffset := FPageSize;
  FFileStream.Seek(LongInt(FPageSize), soFromBeginning);
  readed := FFileStream.Read(buf[0], Length(buf));
  if readed <> Length(buf) then
    raise Exception.Create('File read exception');

  if FHeader.MdxVersion = 3 then
    idxHdrPageNo := 6 // inc by BlockSize
  else
    idxHdrPageNo := 4;

  for i := 0 to FHeader.TagsUsed - 1 do begin
    pTag := @buf[(i + 1) * FHeader.TagSize];
    pTag.headerPageNo := idxHdrPageNo;
    FTags[i].headerPageNo := idxHdrPageNo;

    pIndexHdr := @FIndexHeaders[i];
    pIndexHdr.RootPage := idxRootPageNo;
    pIndexHdr.NumPages := 2;

    pIndexHdr.Version := 1;
    pIndexHdr.FirstBlock := idxRootPageNo;
    pIndexHdr.LastBlock := idxRootPageNo;
    pIndexHdr.UpdYear := Byte(year - 1900);
    pIndexHdr.UpdMonth := month;
    pIndexHdr.UpdDay := day;

    Inc(idxHdrPageNo, FHeader.BlockSize);
    Inc(idxRootPageNo, FHeader.BlockSize);
  end;

  FFileStream.Seek(LongInt(0), soFromBeginning);
  written := FFileStream.Write(FHeader, SizeOf(FHeader));
  if written <> SizeOf(FHeader) then
    raise Exception.Create('Write MDX header exception');

  FFileStream.Seek(LongInt(FPageSize), soFromBeginning);
  written := FFileStream.Write(buf[0], Length(buf));
  if written <> Length(buf) then
    raise Exception.Create('Write MDX index tags exception');

  SetLength(buf, FPageSize * FHeader.BlockSize);
  // write IndexHeaders
  for i := 0 to Length(FIndexHeaders) - 1 do begin
    FillChar(buf[0], Length(buf), 0);
    Move(FIndexHeaders[i], buf[0], SizeOf(TIndexHeader));
    written := FFileStream.Write(buf[0], Length(buf));
    if written <> Length(buf) then
      raise Exception.Create('Write MDX index headers exception');
  end;
  // write empty root pages
  FillChar(buf[0], Length(buf), 0);
  for i := 0 to Length(FIndexHeaders) - 1 do begin
    written := FFileStream.Write(buf[0], Length(buf));
    if written <> Length(buf) then
      raise Exception.Create('Write MDX root pages exception');
  end;
  FFileStream.Size := FFileStream.Position;
end;

procedure TMDXFile.SeekBlock(BlockNo: integer);
var
  sp, fp: int64;
begin
  if ((FHeader.MdxVersion = 3) and (BlockNo < 6))
    or ((FHeader.MdxVersion < 3) and (BlockNo < 4)) then
    raise Exception.CreateFmt('SeekBlock error: BlockNo %d', [BlockNo]);

  sp := BlockNo * FPageSize;
  fp := FFileStream.Seek(LongInt(sp), soFromBeginning);
  if fp <> sp then
    raise Exception.CreateFmt('Seek error: %d instead of %d', [fp, sp]);
end;

procedure TMDXFile.ReadBlock(BlockNo: integer; var Block: TBytes);
var
  readed: integer;
begin
  SetLength(Block, FHeader.BlockSize * FPageSize);
  SeekBlock(BlockNo);
  readed := FFileStream.Read(Block[0], FHeader.BlockSize * FPageSize);
  if readed <> FHeader.BlockSize * FPageSize then
    raise Exception.Create('Read FPageSize exception');
end;

procedure TMDXFile.WriteBlock(BlockNo: integer; const Block: TBytes);
var
  written: integer;
begin
  Assert(Length(Block) >= FHeader.BlockSize * FPageSize, Format('Wrong block size %d, expected %d', [Length(Block), FHeader.BlockSize * FPageSize]));
  SeekBlock(BlockNo);
  written := FFileStream.Write(Block[0], FHeader.BlockSize * FPageSize);
  if written <> FHeader.BlockSize * FPageSize then
    raise Exception.Create('Write FPageSize exception');
end;

procedure TMDXFile.WriteHeader;
var
  written: integer;
begin
  // todo upd year, month, day
  FFileStream.Seek(LongInt(0), soFromBeginning);
  written := FFileStream.Write(FHeader, SizeOf(TMDXHeader));
  if written <> SizeOf(TMDXHeader) then
    raise Exception.Create('Write header exception');
end;

procedure TMDXFile.WriteIndexHeader(IdxNo: integer);
var
  written: integer;
begin
  FFileStream.Seek(LongInt(FTags[IdxNo].HeaderPageNo * FPageSize), soFromBeginning);
  written := FFileStream.Write(FIndexHeaders[IdxNo], SizeOf(TIndexHeader));
  if written <> SizeOf(TIndexHeader) then
    raise Exception.Create('TIndexHeader size exception');
end;

function TMDXFile.CreateBlock(IdxNo: integer; out BlockNo: integer): TBytes;
var
  pBlock: PMDXBlock;
begin
  // todo if FHeader.FreePage ??? reuse
  SetLength(Result, FHeader.BlockSize * FPageSize);
  pBlock := PMDXBlock(@Result[0]);
  pBlock.PrevBlock := FIndexHeaders[IdxNo].LastBlock;
  BlockNo := FHeader.NumPages;
  // write block here just in case of errors after?

  Inc(FIndexHeaders[IdxNo].NumPages, FHeader.BlockSize);
  FIndexHeaders[IdxNo].LastBlock := BlockNo;
  WriteIndexHeader(IdxNo);

  Inc(FHeader.NumPages, FHeader.BlockSize);
  WriteHeader;
end;

function TMDXFile.GetEntry(const Block: TBytes; IdxNo, EntryNo: integer; out Valid: boolean): PMDXEntry;
var
  pBlock: PMDXBlock;
begin
  pBlock := PMDXBlock(@Block[0]);
  Result := @Block[SizeOf(TMDXBlock) + EntryNo * FIndexHeaders[IdxNo].KeyRecLen];
  Valid := ((Result.RecBlockNo <> 0) and (EntryNo <= pBlock.NumEntries)) or (EntryNo < pBlock.NumEntries);
end;

function TMDXFile.GetEntryValue(Entry: PMDXEntry; IdxNo: integer): PAnsiChar;
begin
  FillChar(FKeyBuffers[IdxNo][0], Length(FKeyBuffers[IdxNo]), 0); // null terminated for AnsiString, suppose to be always same size and 0 at the same position
  Move(Entry.KeyData, FKeyBuffers[IdxNo][0], FIndexHeaders[IdxNo].KeyLen);
  Result := @FKeyBuffers[IdxNo][0];
end;

function TMDXFile.CompareEntryValue(Entry: PMDXEntry; IdxNo: integer; Value: PAnsiChar): integer;
begin
  Result := Compare(GetEntryValue(Entry, IdxNo), Value, FIndexHeaders[IdxNo].KeyType, FIndexHeaders[IdxNo].KeyLen);
end;

function TMDXFile.GetIsLeaf(const Block: TBytes; IdxNo: integer): boolean;
var
  pEntry: PMDXEntry;
  valid: boolean;
begin
  pEntry := GetEntry(block, IdxNo, PMDXBlock(@block[0]).NumEntries, valid);
  Result := pEntry.RecBlockNo = 0; // is leaf = 0, else right block
end;

procedure TMDXFile.SetIsLeaf(var Block: TBytes; IdxNo: integer);
var
  //len, left: integer;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  pEntry := GetEntry(block, IdxNo, PMDXBlock(@block[0]).NumEntries, valid);
  {len := RecLen;
  left := FHeader.BlockSize * FPageSize - (SizeOf(TMDXBlock) + PMDXBlock(@block[0]).NumEntries * RecLen);
  if len > left then
    len := left;
  FillChar(pEntry^, len, 0);}
  pEntry.RecBlockNo := 0;
end;

function TMDXFile.IncEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean;
var
  block: TBytes;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  Result := True;
  Inc(Entry.EntryNo);
  ReadBlock(Entry.BlockNo, block);
  if ((Entry.EntryNo >= PMDXBlock(@block[0]).NumEntries) and (Entry.IsLeaf)) // leaf has extra node as zeros
    or ((Entry.EntryNo > PMDXBlock(@block[0]).NumEntries) and not Entry.IsLeaf) // other nodes as right pointer
    then begin
    // update higher node
    if Entry.Parent = nil then
      Result := False
    else
      Result := IncEntryNo(Entry.Parent, IdxNo);
  end;

  // reassign child
  if not Result then
    Dec(Entry.EntryNo)
  else begin
    ReadBlock(Entry.BlockNo, block);
    pEntry := GetEntry(block, IdxNo, Entry.EntryNo, valid);
    if not valid then begin
      Dec(Entry.EntryNo);
      Result := False;
    end
    else if Entry.Child <> nil then begin
      Entry.Child.BlockNo := pEntry.RecBlockNo;
      // start from 0 of next node
      Entry.Child.EntryNo := 0;
    end;
  end;
end;

function TMDXFile.DecEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean;
var
  block: TBytes;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  Result := True;
  Dec(Entry.EntryNo);
  if Entry.EntryNo < 0 then begin
    // update higher node
    if Entry.Parent = nil then
      Result := False
    else
      Result := DecEntryNo(Entry.Parent, IdxNo);
  end;

  // reassign child
  if not Result then
    Dec(Entry.EntryNo)
  else begin
    ReadBlock(Entry.BlockNo, block);
    pEntry := GetEntry(block, IdxNo, Entry.EntryNo, valid);
    if not valid then begin
      Inc(Entry.EntryNo); // Dec(Entry.EntryNo);
      Result := False;
    end
    else if Entry.Child <> nil then begin
      Entry.Child.BlockNo := pEntry.RecBlockNo;
      // start from end of next node
      ReadBlock(Entry.Child.BlockNo, block);
      if Entry.Child.IsLeaf then
        Entry.Child.EntryNo := PMDXBlock(@block[0]).NumEntries - 1
      else
        Entry.Child.EntryNo := PMDXBlock(@block[0]).NumEntries;
    end;
  end;
end;

{ Search }

function TMDXFile.GetLeftmostEntry(IdxNo: integer): PDBFLinkedListEntry;
var
  block: TBytes;
  blockNo: integer;
  parent: PDBFLinkedListEntry;
  valid: boolean;
  pEntry: PMDXEntry;
begin
  Result := nil;
  blockNo := 0;
  while True do begin
    if blockNo = 0 then // read root page
      blockNo := FIndexHeaders[IdxNo].RootPage;
    parent := Result;
    Result := AllocMem(SizeOf(TDBFLinkedListEntry));
    Result.Parent := parent;
    // Result.Child := nil;
    if parent <> nil then
      parent.Child := Result;
    ReadBlock(blockNo, block);
    Result.BlockNo := blockNo;
    Result.EntryNo := 0;
    Result.IsLeaf := GetIsLeaf(block, IdxNo);
    if Result.IsLeaf then
      Break;

    Assert(PMDXBlock(@block[0]).NumEntries > 0);
    pEntry := GetEntry(block, IdxNo, 0, valid);
    if not valid then
      Break;
    blockNo := pEntry.RecBlockNo;
  end;
end;

function TMDXFile.GetRightmostEntry(IdxNo: integer): PDBFLinkedListEntry;
var
  block: TBytes;
  pBlock: PMDXBlock;
  blockNo: integer;
  parent: PDBFLinkedListEntry;
  valid: boolean;
  pEntry: PMDXEntry;
begin
  Result := nil;
  blockNo := 0;
  while True do begin
    if blockNo = 0 then // read root page
      blockNo := FIndexHeaders[IdxNo].RootPage;
    parent := Result;
    Result := AllocMem(SizeOf(TDBFLinkedListEntry));
    Result.Parent := parent;
    // Result.Child := nil;
    if parent <> nil then
      parent.Child := Result;
    ReadBlock(blockNo, block);
    pBlock := PMDXBlock(@block[0]);
    Result.BlockNo := blockNo;
    Result.IsLeaf := GetIsLeaf(block, IdxNo);
    if not Result.IsLeaf then
      Result.EntryNo := pBlock.NumEntries
    else
      Result.EntryNo := pBlock.NumEntries - 1;
    if Result.IsLeaf then
      Break;

    Assert(PMDXBlock(@block[0]).NumEntries > 0);
    pEntry := GetEntry(block, IdxNo, Result.EntryNo, valid);
    if not valid then
      Break;
    blockNo := pEntry.RecBlockNo;
  end;
end;

function TMDXFile.FindFirstEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
begin
  Result := Find(nil, 0, IdxNo, Value, IsFound);
end;

function TMDXFile.FindLastEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  FirstEntry: PDBFLinkedListEntry;
begin
  FirstEntry := Find(nil, 0, idxNo, Value, IsFound);
  if IsFound then begin
    Result := GetEqualRightEntry(FirstEntry, IdxNo, Value, IsFound);
    FreeLinkedList(FirstEntry);
  end
  else
    Result := FirstEntry;
end;

function TMDXFile.GetEqualRightEntry(FirstEntry: PDBFLinkedListEntry; IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  block: TBytes;
  wasFound: boolean;
  i, cmpResult: integer;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  // returns rightmost equal
  Result := CloneLinkedList(FirstEntry);
  Result := LinkedListLeaf(Result);
  IsFound := False;
  wasFound := False;
  while True do begin
    ReadBlock(Result.BlockNo, block);
    // current EntryNo suppose to be from Find, start from it to have found True and try IncEntryNo
    for i := Result.EntryNo to PMDXBlock(@block[0]).NumEntries - 1 do begin
      pEntry := GetEntry(block, IdxNo, i, valid);
      Assert(valid);
      cmpResult := CompareEntryValue(pEntry, IdxNo, Value);
      if cmpResult <> 0 then begin
        if wasFound and (i = 0) then // we incremented before, but new node has no equal
          DecEntryNo(Result, IdxNo);
        Exit;
      end;
      IsFound := True;
      Result.EntryNo := i;
    end;

    if IsFound then begin // try next node, current contains equal at last entry
      if not IncEntryNo(Result, IdxNo) then // eof
        Break;
      wasFound := True;
    end;
  end;
end;

function TMDXFile.Find(Parent: PDBFLinkedListEntry; BlockNo, IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  block: TBytes;
  pBlock: PMDXBlock;
  i, cmpResult: integer;
  pEntry: PMDXEntry;
  found, isDescending, valid: boolean;
begin
  // finds leftmost equal or entryNo above (ASC) or below (DESC)
  // fills Nodes with left position of equal (or where it suppose to be if not found)
  IsFound := False;
  Result := AllocMem(SizeOf(TDBFLinkedListEntry));
  Result.Parent := Parent;
  // Result.Child := nil;
  if Parent <> nil then
    Parent.Child := Result;

  if BlockNo = 0 then
    Result.BlockNo := FIndexHeaders[IdxNo].RootPage
  else
    Result.BlockNo := BlockNo;
  ReadBlock(Result.BlockNo, block);
  pBlock := PMDXBlock(@block[0]);
  Result.IsLeaf := GetIsLeaf(block, IdxNo);

  isDescending := GetIndexIsDescending(IdxNo);

  if Result.IsLeaf then begin
    found := False;
    for i := 0 to pBlock.NumEntries - 1 do begin
      pEntry := GetEntry(block, IdxNo, i, valid);
      Assert(valid);
      cmpResult := CompareEntryValue(pEntry, IdxNo, Value);
      if (not isDescending and (cmpResult >= 0)) or (isDescending and (cmpResult <= 0)) then begin
        // duplicates possible to the right
        Result.EntryNo := i;
        IsFound := cmpResult = 0;
        found := True;
        Break;
      end;
    end;
    if not found then // set position
      if not isDescending then begin // to rigth from current
        //Result.EntryNo := pBlock.NumEntries - 1;
        //IncEntryNo(Result, IdxNo);
        Result.EntryNo := pBlock.NumEntries;
      end
      else begin // to left from current
        //Result.EntryNo := 0;
        //DecEntryNo(Result, IdxNo);
        Result.EntryNo := 0; //-1;
      end;
  end
  else begin
    Assert(pBlock.NumEntries > 0);
    // get proper block
    found := False;
    for i := 0 to pBlock.NumEntries - 1 do begin
      pEntry := GetEntry(block, IdxNo, i, valid);
      Assert(valid);
      cmpResult := CompareEntryValue(pEntry, IdxNo, Value);
      if (not isDescending and (cmpResult >= 0)) or (isDescending and (cmpResult <= 0)) then begin // goto left block pointer
        Result.EntryNo := i;
        Result := Find(Result, pEntry.RecBlockNo, IdxNo, Value, IsFound);
        found := True;
        Break;
      end;
    end;
    if not found then begin // goto right block
      Result.EntryNo := pBlock.NumEntries;
      pEntry := GetEntry(block, IdxNo, pBlock.NumEntries, valid);
      Assert(valid);
      Result := Find(Result, pEntry.RecBlockNo, IdxNo, Value, IsFound);
    end;
  end;
end;

procedure TMDXFile.FillListByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList);
var
  lastEntryNo: integer;
  block: TBytes;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  FirstEntry := LinkedListLeaf(FirstEntry);
  LastEntry := LinkedListLeaf(LastEntry);

  while True do begin
    ReadBlock(FirstEntry.BlockNo, block);
    if FirstEntry.BlockNo = LastEntry.BlockNo then
      lastEntryNo := LastEntry.EntryNo
    else
      lastEntryNo := PMDXBlock(@block[0]).NumEntries - 1;

    while FirstEntry.EntryNo <= lastEntryNo do begin
      pEntry := GetEntry(block, IdxNo, FirstEntry.EntryNo, valid);
      if valid then begin
        List.Add(Pointer(NativeUInt(pEntry.RecBlockNo)));
        Inc(FirstEntry.EntryNo);
      end
      else
        Break;
    end;

    if FirstEntry.BlockNo = LastEntry.BlockNo then
      Break;
    if not IncEntryNo(FirstEntry, IdxNo) then
      Break;
  end;
end;

procedure TMDXFile.CheckPathByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList);
var
  i: integer;
  tempList: TList;
  block: TBytes;
  found: boolean;
  temp: PDBFLinkedListEntry;
begin
  FirstEntry := LinkedListLeaf(FirstEntry);
  LastEntry := LinkedListLeaf(LastEntry);

  tempList := TList.Create;
  try
    while True do begin
      found := False;
      for i := 0 to tempList.Count - 1 do
        if FirstEntry.BlockNo = PDBFLinkedListEntry(tempList[i]).BlockNo then begin
          List.Add(CloneLinkedList(tempList[i]));
          List.Add(CloneLinkedList(FirstEntry));
          found := True;
          Break;
        end;

      if not found then
        tempList.Add(CloneLinkedList(FirstEntry));

      if FirstEntry.BlockNo = LastEntry.BlockNo then
        Exit;

      ReadBlock(FirstEntry.BlockNo, block);
      FirstEntry.EntryNo := PMDXBlock(@block[0]).NumEntries - 1;
      if not IncEntryNo(FirstEntry, IdxNo) then
        raise Exception.Create('IncEntryNo Error');
    end;
  finally
    for i := 0 to tempList.Count - 1 do begin
      temp := tempList[i];
      FreeLinkedList(PDBFLinkedListEntry(temp));
    end;
    tempList.Free;
  end;
end;

{ Tree manipulation }

procedure TMDXFile.ClearEmptySpace(Entry: PDBFLinkedListEntry; IdxNo: integer; var Block: TBytes; EntryNo: integer);
var
  from, len: integer;
begin
  if EntryNo < FIndexHeaders[IdxNo].MaxNumKeys then begin
    from := SizeOf(TMDXBlock) + EntryNo * FIndexHeaders[IdxNo].KeyRecLen;
    if not Entry.IsLeaf then
      Inc(from, SizeOf(integer));
    len := FHeader.BlockSize * FPageSize - from;
    FillChar(PtrOffset(@Block[0], from)^, len, 0);
  end;
end;

procedure TMDXFile.RemoveEntry(Entry: PDBFLinkedListEntry; IdxNo: integer; var Block: TBytes; EntryNo: integer);
var
  i, recLen: integer;
  pBlock: PMDXBlock;
  validFrom, validTo: boolean;
begin
  recLen := FIndexHeaders[IdxNo].KeyRecLen;

  pBlock := PMDXBlock(@Block[0]);
  if EntryNo < pBlock.NumEntries - 1 then begin
    // move entries
    for i := EntryNo to pBlock.NumEntries - 1 do
      Move(PtrOffset(pBlock, SizeOf(TMDXBlock) + (i + 1) * recLen)^, PtrOffset(pBlock, SizeOf(TMDXBlock) + i * recLen)^, recLen);
    // copy right offset
    if not Entry.IsLeaf then
      GetEntry(Block, IdxNo, pBlock.NumEntries, validTo).RecBlockNo := GetEntry(Block, IdxNo, pBlock.NumEntries + 1, validFrom).RecBlockNo;
  end;

  Dec(pBlock.NumEntries);
  if Entry.IsLeaf then
    SetIsLeaf(Block, IdxNo);
  ClearEmptySpace(Entry, IdxNo, Block, pBlock.NumEntries);
end;

procedure TMDXFile.InsertIntoNode(var Entry: PDBFLinkedListEntry; IdxNo, LeftBlockNo, RightBlockNo: integer; Value: PAnsiChar);
var
  recLen: integer;

  procedure InsertEntry(var Block: TBytes; EntryNo: integer);
  var
    i: integer;
    pBlock: PMDXBlock;
    validFrom, validTo: boolean;
  begin
    pBlock := PMDXBlock(@Block[0]);
    // copy right offset
    if not Entry.IsLeaf then
      GetEntry(Block, IdxNo, pBlock.NumEntries + 1, validTo).RecBlockNo := GetEntry(Block, IdxNo, pBlock.NumEntries, validFrom).RecBlockNo;
    // move entries
    for i := pBlock.NumEntries - 1 downto EntryNo do
      Move(PtrOffset(pBlock, SizeOf(TMDXBlock) + i * recLen)^, PtrOffset(pBlock, SizeOf(TMDXBlock) + (i + 1) * recLen)^, recLen);

    Inc(PMDXBlock(@block[0]).NumEntries);
  end;

  procedure SetEntry(var Block: TBytes; EntryNo: integer);
  var
    pEntry: PMDXEntry;
    valid: boolean;
  begin
    // set new entry RecBlockNo and Key
    pEntry := GetEntry(Block, IdxNo, EntryNo, valid);
    pEntry.RecBlockNo := LeftBlockNo;
    Move(Value^, pEntry.KeyData, FIndexHeaders[IdxNo].KeyLen);
    if not Entry.IsLeaf then
      GetEntry(Block, IdxNo, EntryNo + 1, valid).RecBlockNo := RightBlockNo;

    if FIndexHeaders[IdxNo].KeyExist = 0 then begin
      FIndexHeaders[IdxNo].Version := 2;
      FIndexHeaders[IdxNo].KeyExist := 1;
      WriteIndexHeader(IdxNo);
    end;
  end;

var
  count, insertInto, blockNo, newBlockNo: integer;
  block, newBlock: TBytes;
  pBlock, pNewBlock: PMDXBlock;
  valid, validFrom, validTo: boolean;
  pEntry: PMDXEntry;
begin
  recLen := FIndexHeaders[IdxNo].KeyRecLen;

  if Entry = nil then begin
    // make new root
    Entry := AllocMem(SizeOf(TDBFLinkedListEntry));
    // Entry.IsLeaf := False;

    // make new node
    block := CreateBlock(IdxNo, blockNo);

    Entry.BlockNo := blockNo;
    Entry.EntryNo := 0;

    // update TIndexHeader and root page
    FIndexHeaders[IdxNo].RootPage := blockNo;
    //FIndexHeaders[IdxNo].Version := 3; // ??
    WriteIndexHeader(IdxNo);

    SetEntry(block, Entry.EntryNo);
    PMDXBlock(@block[0]).NumEntries := 1;
    WriteBlock(blockNo, block);
  end
  else begin
    blockNo := Entry.BlockNo;
    ReadBlock(blockNo, block);
    pBlock := PMDXBlock(@block[0]);
    insertInto := Entry.EntryNo;

    // if room in node insert
    if pBlock.NumEntries < FIndexHeaders[IdxNo].MaxNumKeys then begin
      InsertEntry(block, insertInto);
      SetEntry(block, insertInto);

      if Entry.IsLeaf then
        SetIsLeaf(block, IdxNo);
      WriteBlock(blockNo, block);
    end
    else begin
      // split and promote
      if insertInto >= FIndexHeaders[IdxNo].MaxNumKeys then
        count := insertInto
      else
        count := FIndexHeaders[IdxNo].MaxNumKeys div 2 + 1;

      // make new node
      newBlock := CreateBlock(IdxNo, newBlockNo);
      pNewBlock := PMDXBlock(@newBlock[0]);
      // move rigth part to new node
      if insertInto < FIndexHeaders[IdxNo].MaxNumKeys then
        Move(PtrOffset(pBlock, SizeOf(TMDXBlock) + count * recLen)^, PtrOffset(pNewBlock, SizeOf(TMDXBlock))^, (pBlock.NumEntries - count) * recLen);
      // set entryNo
      pNewBlock.NumEntries := pBlock.NumEntries - count;
      // copy offset to right block
      if not Entry.IsLeaf then
        GetEntry(newBlock, IdxNo, pNewBlock.NumEntries, validTo).RecBlockNo := GetEntry(Block, IdxNo, pBlock.NumEntries, validFrom).RecBlockNo;
      // clear the rest, temporary
      ClearEmptySpace(Entry, IdxNo, block, count);
      // set entryNo
      pBlock.NumEntries := count;

      if insertInto < count then begin
        InsertEntry(block, insertInto);
        SetEntry(block, insertInto);
      end
      else begin
        insertInto := insertInto - count;
        InsertEntry(newBlock, insertInto);
        SetEntry(newBlock, insertInto);
      end;

      // set isLeaf sign or rightBlockNo
      if Entry.IsLeaf then begin
        SetIsLeaf(block, IdxNo);
        SetIsLeaf(newBlock, IdxNo);
      end;
      WriteBlock(newBlockNo, newBlock);

      // update TIndexHeader
      //FIndexHeaders[IdxNo].Version := 3;
      //WriteIndexHeader(IdxNo);

      // add current node last entry key to higher node (promote)
      pEntry := GetEntry(block, IdxNo, pBlock.NumEntries - 1, valid);
      Assert(valid);
      InsertIntoNode(Entry.Parent, IdxNo, blockNo, newBlockNo, PAnsiChar(PtrOffset(pEntry, SizeOf(integer))));
      Entry.Parent.Child := Entry;
      if not Entry.IsLeaf then begin
        RemoveEntry(Entry, IdxNo, block, pBlock.NumEntries - 1);
        ClearEmptySpace(Entry, IdxNo, block, pBlock.NumEntries);
      end;
      WriteBlock(blockNo, block);
    end;
  end;
end;

procedure TMDXFile.Add(IdxNo, RecNo: integer; Value: PAnsiChar);
var
  entry: PDBFLinkedListEntry;
  found: boolean;
begin
{$IFDEF LOG_PACKETS}
  AddIndexStartTick := GetTickCount;
{$ENDIF}
  if not GetIndexIsDescending(IdxNo) then begin
    entry := FindLastEntry(IdxNo, Value, found);
    if found then
      IncEntryNo(entry, IdxNo);
  end
  else begin
    entry := FindFirstEntry(IdxNo, Value, found);
    if found then
      DecEntryNo(entry, IdxNo);
  end;

  if GetIndexIsUnique(IdxNo) and found then
    raise Exception.Create(SDuplicateIndexValue)
  else begin
    Assert(entry.BlockNo > 0);
    InsertIntoNode(entry, IdxNo, RecNo, 0, Value);
  end;
  FreeLinkedList(entry);
{$IFDEF LOG_PACKETS}
  AddIndexTickCount := AddIndexTickCount + GetTickCount - AddIndexStartTick;
{$ENDIF}
end;

procedure TMDXFile.Delete(IdxNo, RecNo: integer; Value: PAnsiChar);
var
  FirstEntry, LastEntry: PDBFLinkedListEntry;
  found, recNoFound: boolean;
  lastEntryNo: integer;
  block: TBytes;
  pEntry: PMDXEntry;
  valid: boolean;
begin
  FirstEntry := FindFirstEntry(IdxNo, Value, found);
  if found then begin
    LastEntry := GetEqualRightEntry(FirstEntry, idxNo, Value, found);

    recNoFound := False;
    while True do begin
      ReadBlock(FirstEntry.BlockNo, block);
      if FirstEntry.BlockNo = LastEntry.BlockNo then
        lastEntryNo := LastEntry.EntryNo
      else
        lastEntryNo := PMDXBlock(@block[0]).NumEntries - 1;

      while FirstEntry.EntryNo <= lastEntryNo do begin
        pEntry := GetEntry(block, IdxNo, FirstEntry.EntryNo, valid);
        Assert(valid);

        if pEntry.RecBlockNo = RecNo then begin
          recNoFound := True;
          RemoveEntry(FirstEntry, IdxNo, block, FirstEntry.EntryNo);
          WriteBlock(FirstEntry.BlockNo, block);
          Break;
        end;
        Inc(FirstEntry.EntryNo);
      end;

      if recNoFound or (FirstEntry.BlockNo = LastEntry.BlockNo) then
        Break;
      if not IncEntryNo(FirstEntry, IdxNo) then
        Break;
    end;
    FreeLinkedList(LastEntry);
  end;
  FreeLinkedList(FirstEntry);
end;

{ TCDXFile }

constructor TCDXFile.Create(const Parent: TObject; const FileName: string; New: boolean = False; IndexFields: TIndexFieldArray = nil);
begin
  inherited Create(Parent, FileName, New);

  FPageSize := 512;
  if New then
    CreateNew(IndexFields);
end;

destructor TCDXFile.Destroy;
begin

  inherited;
end;

procedure TCDXFile.AddIndex(FieldNo: integer; const IndexName, Expression: string);
var
  i, len, written: integer;
  v, newBlock: TBytes;
  fType: AnsiChar;
  fieldPtr: PDBFField;
  s: string;
  aStr: AnsiString;
  idxHdr: TCDXHeader;
begin
  i := Length(FIndexInfos);
  SetLength(FIndexInfos, Length(FIndexInfos) + 1);
  SetLength(v, FHeader.KeyLength);

  fieldPtr := TDBFDBase(FDBF).GetField(FieldNo);

  // set expression as field name
  s := Expression;
  if s = '' then
    s := TDBFDBase(FDBF).GetFieldName(FieldNo);
  if GetIndexNameExists(s) then
    raise Exception.CreateFmt('Index %s already exists', [IndexName]);
  aStr := AnsiString(UpperCase(s));
  len := LengthA(aStr);

  FExpressions.Add(ParseExpression(s));

  FillChar(idxHdr, SizeOf(idxHdr), 0);
  idxHdr.RootNode := FFileStream.Size + SizeOf(idxHdr);

  fType := VfpFieldTypeToIndexType[Byte(fieldPtr.DB3FType)];
  if Byte(fType) = 0 then
    raise Exception.CreateFmt('Unknown index type for field type %s', [string(AnsiString(fType))]);
  idxHdr.KeyLength := DbfIndexTypeToLen[Byte(fType)];
  if fType = DBF_TYPE_NUMERIC then
    idxHdr.KeyLength := 8; // stored as double
  if idxHdr.KeyLength = 0 then
    idxHdr.KeyLength := fieldPtr.DB3Len;
  if TDBFDBase(FDBF).GetFieldNullable(FieldNo) then
    Inc(idxHdr.KeyLength);

  idxHdr.Options := $60; // $62 if nullable
  if TDBFDBase(FDBF).GetFieldIsKey(FieldNo) then
    idxHdr.Options := idxHdr.Options or 1;
  idxHdr.Signature := 1;
  idxHdr.ExpressionLen := len + 1;
  idxHdr.FilterLen := 1;
  idxHdr.KeyExprLen := len + 1;
  Move(PAnsiChar(aStr)^, idxHdr.KeyPool, len);
  FFileStream.Seek(LongInt(0), soFromEnd);
  written := FFileStream.Write(idxHdr, SizeOf(idxHdr));
  if written <> SizeOf(idxHdr) then
    raise Exception.Create('Write CDX header exception');

  FIndexInfos[i].Name := AnsiString(UpperCase(s));
  FIndexInfos[i].Expression := aStr;
  FIndexInfos[i].KeyType := fType;
  FIndexInfos[i].Valid := True;
  FIndexInfos[i].IsDesc := False;
  FIndexInfos[i].RootNode := idxHdr.RootNode;
  FIndexInfos[i].KeyLength := idxHdr.KeyLength; //fieldPtr.DB3Len;
  FIndexInfos[i].KeyRecLen := idxHdr.KeyLength + SizeOf(TCDXKeyInfo); //fieldPtr.DB3Len + SizeOf(TCDXKeyInfo);
  FIndexInfos[i].Options := 0;
  FIndexInfos[i].Nullable := False;

  // write empty root node page
  CompressCdxNode(i, CDX_NODETYPE_ROOT or CDX_NODETYPE_LEAF, -1, -1, nil, newBlock);
  WriteBlock(idxHdr.RootNode, newBlock);
  // add aStr to FHeader.RootNode
  s := UpperCase(IndexName);
  if s = '' then
    s := UpperCase(TDBFDBase(FDBF).GetFieldName(FieldNo));
  if Length(s) > FHeader.KeyLength then
    SetLength(s, FHeader.KeyLength);
  aStr := AnsiString(s);
  len := LengthA(aStr);
  if len > Length(v) then
    len := Length(v);
  FillChar(v[0], Length(v), 0);
  Move(PAnsiChar(aStr)^, v[0], len);
  // as PAnsiChar contains garbage after len + 1, array used instead
  Add(-1, idxHdr.RootNode - SizeOf(idxHdr), @v[0]);
end;

procedure TCDXFile.DeleteIndex(const IndexName: string);
var
  s: string;
  aStr: AnsiString;
  idxNo, len: integer;
  temp: TBytes;
begin
  s := UpperCase(IndexName);
  if Length(s) > FHeader.KeyLength then
    SetLength(s, FHeader.KeyLength);
  idxNo := GetIndexNo(s);
  if idxNo < 0 then
    raise Exception.CreateFmt('Index %s not found', [IndexName]);

  SetLength(temp, FHeader.KeyLength);
  aStr := AnsiString(s);
  len := LengthA(aStr);
  if len > Length(temp) then
    len := Length(temp);
  FillChar(temp[0], Length(temp), $20);
  Move(PAnsiChar(aStr)^, temp[0], len);
  Delete(-1, FIndexInfos[idxNo].RecNo, @temp[0]);
end;

procedure TCDXFile.CreateNew(IndexFields: TIndexFieldArray);
var
  i, written: integer;
  newBlock: TBytes;
begin
  if Length(IndexFields) = 0 then
    Exit;

  FreeAndNil(FFileStream);
  FFileStream := TFileStream.Create(FFileName, fmCreate);

  FIndexInfos := nil;

  FillChar(FHeader, SizeOf(FHeader), 0);
  FHeader.RootNode := $400;
  FHeader.KeyLength := $A;
  FHeader.Options := $E0;
  FHeader.Signature := 1;
  FHeader.ExpressionLen := 1;
  FHeader.FilterLen := 1;
  FHeader.KeyExprLen := 1;

  written := FFileStream.Write(FHeader, SizeOf(FHeader));
  if written <> SizeOf(FHeader) then
    raise Exception.Create('Write CDX header exception');

  CompressCdxNode(-1, CDX_NODETYPE_ROOT or CDX_NODETYPE_LEAF, -1, -1, nil, newBlock);
  WriteBlock(FHeader.RootNode, newBlock);

  // Add indexes as keys
  for i := 0 to Length(IndexFields) - 1 do
    AddIndex(IndexFields[i].FieldNo, IndexFields[i].IndexName, TDBFDBase(FDBF).GetFieldName(IndexFields[i].FieldNo));
end;

function TCDXFile.Compare(K1, K2: PAnsiChar; KeyType: AnsiChar; KeyLen: integer): integer;
begin
{$IFDEF LOG_PACKETS}
  CompareStartTick := GetTickCount;
{$ENDIF}
  case KeyType of
    DBF_TYPE_INTEGER,
    DBF_TYPE_LOGICAL,
    DBF_TYPE_CURRENCY,
    DBF_TYPE_BINARY, DBF_TYPE_FLOAT,
    DBF_TYPE_NUMERIC,
    DBF_TYPE_DATE,
    DBF_TYPE_TIME:
      Result := CompareBytes(K1, K2, KeyLen);
    DBF_TYPE_CHAR,
    DBF_TYPE_VARCHAR,
    DBF_TYPE_VARBINARY:
      Result := CompareChars(K1, K2, KeyLen);
  else
    raise Exception.CreateFmt('Unknown type %s or cannot be indexed', [KeyType]);
  end;
{$IFDEF LOG_PACKETS}
  CompareTickCount := CompareTickCount + GetTickCount - CompareStartTick;
{$ENDIF}
end;

procedure TCDXFile.SeekBlock(BlockNo: integer);
var
  fp: int64;
begin
  //Assert(BlockNo >= 6);
  fp := FFileStream.Seek(LongInt(BlockNo), soFromBeginning);
  if fp <> BlockNo then
    raise Exception.CreateFmt('Seek error: %d instead of %d', [fp, BlockNo]);
end;

procedure TCDXFile.ReadBlock(BlockNo: integer; var Block: TBytes);
var
  readed: integer;
begin
  SetLength(Block, FPageSize);
  SeekBlock(BlockNo);
  readed := FFileStream.Read(Block[0], FPageSize);
  if readed <> FPageSize then
    raise Exception.Create('Read FPageSize exception');
end;

procedure TCDXFile.WriteBlock(BlockNo: integer; const Block: TBytes);
var
  written: integer;
begin
  SeekBlock(BlockNo);
  written := FFileStream.Write(Block[0], FPageSize);
  if written <> FPageSize then
    raise Exception.Create('Write FPageSize exception');
end;

procedure TCDXFile.AppendBlock(const Block: TBytes);
var
  written: integer;
begin
  FFileStream.Seek(LongInt(0), soFromEnd);
  written := FFileStream.Write(Block[0], FPageSize);
  if written <> FPageSize then
    raise Exception.Create('Write FPageSize exception');
end;

function TCDXFile.GetRootNodeBlockNo(IdxNo, BlockNo: integer): integer;
begin
  if BlockNo <> 0 then
    Result := BlockNo
  else if IdxNo >= 0 then
    Result := FIndexInfos[IdxNo].RootNode
  else
    Result := FHeader.RootNode; // Header block
end;

function TCDXFile.GetIsLeaf(PNode: PCDXCompactIndexNode; IdxNo: integer): boolean;
begin
  Result := (PNode.Attributes and CDX_NODETYPE_LEAF) <> 0;
end;

procedure TCDXFile.GetIndexFields(List: TStringList);
var
  i: integer;
begin
  List.Clear;
  List.Sorted := True;
  List.Duplicates := dupIgnore;
  // todo filter by usable expressions ?
  for i := 0 to Length(FIndexInfos) - 1 do
    if FIndexInfos[i].Expression <> '' then
      List.Add(UpperCase(string(FIndexInfos[i].Expression)));
end;

procedure TCDXFile.GetIndexKey(PNode: PCDXCompactIndexNode; KeyLen: integer; KeyType: AnsiChar; var Offs: integer;
  const CompKeyParams: TCDXCompKeyParams; var PrevKey: PAnsiChar; Key: PAnsiChar);
var
  start, len, into: integer;
begin
  start := Offs - (KeyLen - CompKeyParams.TrailCount - CompKeyParams.DupCount);
  len := Offs - start;

  into := 0;
  if CompKeyParams.DupCount > 0 then begin
    Assert(PrevKey <> nil);
    Move(PrevKey^, Key^, CompKeyParams.DupCount);
    into := CompKeyParams.DupCount;
  end;

  if len > 0 then
    Move(PtrOffset(PNode, start)^, PtrOffset(Key, into)^, len);

  // fill trail
  if KeyType = DBF_TYPE_CHAR then  // DBF_TYPE_VARCHAR ?
    FillChar(PtrOffset(Key, KeyLen - CompKeyParams.TrailCount)^, CompKeyParams.TrailCount, $20)
  else
    FillChar(PtrOffset(Key, KeyLen - CompKeyParams.TrailCount)^, CompKeyParams.TrailCount, 0);

  Offs := start;
  PrevKey := Key;
end;

procedure TCDXFile.ExtractIdxInfo(PNode: PCDXCompactIndexNode; Idx: integer; var CompKeyParams: TCDXCompKeyParams);
var
  i: Int64;
begin
  i := PInt64(@PNode.IndexKeys[Idx * PNode.NumBytesRecNo])^;

  CompKeyParams.RecNo := i and PNode.RecNumMask;
  i := i shr PNode.NumBitsRecNo;

  CompKeyParams.DupCount := i and PNode.DupCountMask;
  i := i shr PNode.NumBitsDupCount;

  CompKeyParams.TrailCount := i and PNode.TrailCountMask;
end;

procedure TCDXFile.CompactIdxInfo(PNode: PCDXCompactIndexNode; Idx: integer; const CompKeyParams: TCDXCompKeyParams);
var
  i: Int64;
begin
  i := CompKeyParams.TrailCount and PNode.TrailCountMask;

  i := i shl PNode.NumBitsDupCount;
  i := i or (CompKeyParams.DupCount and PNode.DupCountMask);

  i := i shl PNode.NumBitsRecNo;
  i := i or (CompKeyParams.RecNo and PNode.RecNumMask);

  Move(i, PNode.IndexKeys[Idx * PNode.NumBytesRecNo], PNode.NumBytesRecNo);
end;

function TCDXFile.ExtractCdxNode(PNode: PCDXCompactIndexNode; IdxNo: integer): TBytes;
var
  i, keyLen, keyRecLen, offs: integer;
  keyType: AnsiChar;
  compKeyParams: TCDXCompKeyParams;
  key, prevKey: PAnsiChar;
begin
  Result := nil; // reused as param

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);
  keyType := GetIndexKeyType(IdxNo);
  SetLength(Result, PNode.NumOfKeys * keyRecLen);

  if (PNode.Attributes and CDX_NODETYPE_LEAF) <> 0 then begin // root the same + CDX_NODETYPE_LEAF
    offs := SizeOf(TCDXCompactIndexNode); // keys at the end of block

    prevKey := nil;
    for i := 0 to PNode.NumOfKeys - 1 do begin
      ExtractIdxInfo(PNode, i, compKeyParams);
      // "Key + RecNo + Offset"  as in Index nodes
      key := @Result[i * keyRecLen];
      GetIndexKey(PNode, keyLen, keyType, offs, compKeyParams, prevKey, key);
      PInteger(PtrOffset(key, keyLen))^ := FastSwap(compKeyParams.RecNo);
    end;
  end
  else begin
    // interior
    // key of KeyLength
    // RecNo bigendian
    // file offset bigendian
    // there is no need to do something if RecNo/Offset bigendian, swap later
    Move(PtrOffset(PNode, 12)^, Result[0], PNode.NumOfKeys * keyRecLen); // 12 is start of data_pool
  end;
end;

{$IFDEF LOG_PACKETS}
procedure TCDXFile.Dump(BlockNo, IdxNo: integer);
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  i, j, keyLen, keyRecLen, bNo: integer;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
  isLeaf: Boolean;
  temp: string;
begin
  if BlockNo = 0 then begin
    AddToLog('');
    AddToLog(Format('Dump of index %d %s', [IdxNo, FIndexInfos[IdxNo].Name]));
  end;
  bNo := GetRootNodeBlockNo(IdxNo, BlockNo);

  ReadBlock(bNo, block);
  pNode := PCDXCompactIndexNode(@block[0]);

  isLeaf := GetIsLeaf(pNode, IdxNo);
  keyInfos := ExtractCdxNode(pNode, IdxNo);

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);

  AddToLog(Format('BlockNo %.4X', [bNo]));
  AddToLog(Format('Attributes %.2X', [pNode.Attributes]));
  AddToLog(Format('NumOfKeys %d', [pNode.NumOfKeys]));
  AddToLog(Format('LeftNode %.4X', [pNode.LeftNode]));
  AddToLog(Format('RightNode %.4X', [pNode.RightNode]));
  AddToLog('');
  for i := 0 to pNode.NumOfKeys - 1 do begin
    pKeyInfo := PCDXKeyInfo(@keyInfos[i * keyRecLen + keyLen]);
    temp := '';
    for j := 0 to keyLen - 1 do
      temp := temp + Format('%.2X ', [keyInfos[i * keyRecLen + j]]);
    if keyLen = 4 then
      temp := Format('%s (%d)', [temp, VfpDataToInteger(PInteger(@keyInfos[i * keyRecLen])^)]);
    AddToLog(temp);
    AddToLog(Format('%.4X', [FastSwap(pKeyInfo.RecNo)]));
    AddToLog(Format('%.4X', [FastSwap(pKeyInfo.Offset)]));
  end;
  AddToLog('');
  for i := 0 to pNode.NumOfKeys - 1 do begin
    if not isLeaf then begin
      pKeyInfo := PCDXKeyInfo(@keyInfos[i * keyRecLen + keyLen]);
      Dump(FastSwap(pKeyInfo.Offset), IdxNo);
    end;
  end;
  AddToLog('');
end;
{$ENDIF}

function TCDXFile.CompressCdxNode(IdxNo: integer; Attributes: Word; LeftNode, RightNode: Integer;
  const KeyInfos: TBytes; var NewBlock: TBytes): boolean;

  function RecNoLenToNumBits(RecNo: integer): integer;
  var
    i, temp: integer;
  begin
    Result := 0;
    temp := RecNo;
    for i := 0 to 31 do
      if temp = 0 then
        Break
      else begin
        Inc(Result);
        temp := temp shr 1;
      end;
  end;

const
  LenToNumBits: array [0 .. 255] of Byte = (
    0, 1, 2, 2, 3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,
    5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5,
    6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8
  );
  NumBitsToMask: array [0 .. 31] of integer = (
    0, 1, 3, 7,
    $F, $1F, $3F, $7F,
    $FF, $1FF, $3FF, $7FF,
    $FFF, $1FFF, $3FFF, $7FFF,
    $FFFF, $1FFFF, $3FFFF, $7FFFF,
    $FFFFF, $1FFFFF, $3FFFFF, $7FFFFF,
    $FFFFFF, $1FFFFFF, $3FFFFFF, $7FFFFFF,
    $FFFFFFF, $1FFFFFFF, $3FFFFFFF, $7FFFFFFF
  );
var
  i, j, offs, len, keyLen, keyRecLen, bitsTotal: integer;
  keyType: AnsiChar;
  pNode: PCDXCompactIndexNode;
  pKeyInfo: PCDXKeyInfo;
  compKeyParamsArray: array of TCDXCompKeyParams;
  pCompKeyParams: PCDXCompKeyParams;
  maxCompKeyParams: TCDXCompKeyParams;
  pKey: PByte;
  prevKey: TBytes;
begin
  Result := True;
  SetLength(NewBlock, SizeOf(TCDXCompactIndexNode));
  FillChar(NewBlock[0], Length(NewBlock), 0);
  pNode := @NewBlock[0];

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);
  keyType := GetIndexKeyType(IdxNo);

  pNode.Attributes := Attributes;
  pNode.NumOfKeys := Length(KeyInfos) div keyRecLen;
  pNode.LeftNode := LeftNode;
  pNode.RightNode := RightNode;
  if (Attributes and CDX_NODETYPE_LEAF) <> 0 then begin // root the same + CDX_NODETYPE_LEAF
    pNode.FreeSpace := SizeOf(pNode.IndexKeys);
    pNode.NumBitsDupCount := LenToNumBits[keyLen];
    pNode.NumBitsTrailCount := LenToNumBits[keyLen];
    pNode.DupCountMask := NumBitsToMask[pNode.NumBitsDupCount];
    pNode.TrailCountMask := NumBitsToMask[pNode.NumBitsTrailCount];

    SetLength(compKeyParamsArray, pNode.NumOfKeys);
    FillChar(maxCompKeyParams, SizeOf(maxCompKeyParams), 0);
    offs := SizeOf(TCDXCompactIndexNode); // keys at the end of block
    SetLength(prevKey, keyLen);

    for i := 0 to pNode.NumOfKeys - 1 do begin
      pKeyInfo := @KeyInfos[i * keyRecLen + keyLen];
      pCompKeyParams := @compKeyParamsArray[i];
      // suppose to be all empty but somehow DupCount > 0 occurs at the start
      pCompKeyParams.TrailCount := 0;
      pCompKeyParams.DupCount := 0;
      pCompKeyParams.RecNo := 0;

      pKey := @KeyInfos[i * keyRecLen + keyLen - 1];
      for j := keyLen - 1 downto 0 do
        if (pKey^ = 0) or ((keyType = DBF_TYPE_CHAR) and (pKey^ = $20)) then begin
          Inc(pCompKeyParams.TrailCount);
          Dec(pKey);
        end
        else
          Break;

      pKey := @KeyInfos[i * keyRecLen];
      if i <> 0 then
        for j := 0 to keyLen - 1 - pCompKeyParams.TrailCount do
          if prevKey[j] = pKey^ then begin
            Inc(pCompKeyParams.DupCount);
            Inc(pKey);
          end
          else
            Break;

      len := keyLen - pCompKeyParams.DupCount - pCompKeyParams.TrailCount;
      if len > pNode.FreeSpace then begin
        Result := False;
        Exit;
      end;
      Move(pKey^, PtrOffset(@pNode.Attributes, offs - len)^, len);
      Dec(offs, len);
      Dec(pNode.FreeSpace, len);

      pCompKeyParams.RecNo := FastSwap(pKeyInfo.RecNo);

      Move(KeyInfos[i * keyRecLen], prevKey[0], keyLen);

      maxCompKeyParams.RecNo := Max(maxCompKeyParams.RecNo, compKeyParamsArray[i].RecNo);
    end;
    // calculate width of TrailCount, DupCount, RecNo, Masks
    maxCompKeyParams.RecNo := Max(maxCompKeyParams.RecNo, $FFFF);
    pNode.NumBitsRecNo := RecNoLenToNumBits(maxCompKeyParams.RecNo);
    bitsTotal := pNode.NumBitsRecNo + pNode.NumBitsDupCount + pNode.NumBitsTrailCount;
    pNode.NumBytesRecNo := bitsTotal shr 3;
    if (bitsTotal and 7) <> 0 then
      Inc(pNode.NumBytesRecNo);
    pNode.NumBitsRecNo := pNode.NumBytesRecNo shl 3 - pNode.NumBitsDupCount - pNode.NumBitsTrailCount; // * 8
    pNode.RecNumMask := NumBitsToMask[pNode.NumBitsRecNo];
    // try to store
    for i := 0 to Length(compKeyParamsArray) - 1 do begin
      if pNode.NumBytesRecNo > pNode.FreeSpace then begin
        Result := False;
        Exit;
      end;

      CompactIdxInfo(pNode, i, compKeyParamsArray[i]);
      Dec(pNode.FreeSpace, pNode.NumBytesRecNo);
    end;
  end
  else begin
    // interior
    if Length(KeyInfos) > SizeOf(pNode.data_pool) then begin
      Result := False;
      Exit;
    end;
    Move(KeyInfos[0], pNode.data_pool[0], Length(KeyInfos));
  end;
end;

procedure TCDXFile.Open;
var
  i, readed, len, keyLen, keyRecLen: integer;
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
  hdr: TCDXHeader;
  fieldNo: integer;
  Mode: Word;
  Expr: string;
begin
  Mode := fmOpenReadWrite;
  case TDBFDBase(FDBF).ConnectMode of
    cmShared:
      Mode := Mode or fmShareDenyWrite;
    cmUnsafe:
      Mode := Mode or fmShareDenyNone;
  end;

  if FFileStream = nil then
    FFileStream := TFileStream.Create(FFileName, Mode);
  FFileStream.Seek(LongInt(0), soFromBeginning);
  readed := FFileStream.Read(FHeader, SizeOf(TCDXHeader));
  if readed <> SizeOf(TCDXHeader) then
    raise Exception.Create('TCDXFile.Open exception');

  // get Root pages as RecNo
  ReadBlock(FHeader.RootNode, block);
  pNode := PCDXCompactIndexNode(@block[0]);

  keyLen := FHeader.KeyLength;
  keyRecLen := keyLen + SizeOf(TCDXKeyInfo);
  keyInfos := ExtractCdxNode(pNode, -1);

  SetLength(FIndexInfos, Length(keyInfos) div keyRecLen);
  ClearExpressions;
  for i := 0 to pNode.NumOfKeys - 1 do begin
    pKeyInfo := PCDXKeyInfo(@keyInfos[i * keyRecLen + keyLen]);
    FIndexInfos[i].RecNo := FastSwap(pKeyInfo.RecNo);
    FFileStream.Seek(LongInt(FIndexInfos[i].RecNo), soFromBeginning); // RecNo used as offset
    readed := FFileStream.Read(hdr, SizeOf(TCDXHeader));
    if readed <> SizeOf(TCDXHeader) then
      raise Exception.Create('TCDXFile node exception');

    len := keyLen;
    if len > 0 then begin
      SetLengthA(FIndexInfos[i].Name, len);
      Move(keyInfos[i * keyRecLen], PAnsiChar(FIndexInfos[i].Name)^, len);
      FIndexInfos[i].Name := AnsiString(Trim(string(FIndexInfos[i].Name)));
    end;

    len := hdr.ExpressionLen - 1;
    if len > 0 then begin
      SetLengthA(FIndexInfos[i].Expression, len);
      Move(hdr.KeyPool[0], PAnsiChar(FIndexInfos[i].Expression)^, len);
    end;

    len := hdr.FilterLen - 1;
    if len > 0 then begin
      SetLengthA(FIndexInfos[i].Filter, len);
      Move(hdr.KeyPool[0], PAnsiChar(FIndexInfos[i].Filter)^, len);
    end;

    FIndexInfos[i].IsDesc := hdr.Desc <> 0;
    FIndexInfos[i].RootNode := hdr.RootNode;
    FIndexInfos[i].KeyLength := hdr.KeyLength;
    FIndexInfos[i].KeyRecLen := hdr.KeyLength + SizeOf(TCDXKeyInfo);
    FIndexInfos[i].Options := hdr.Options;

    Expr := Trim(string(FIndexInfos[i].Expression));
    if Expr = '' then
      if SuppressIndexOpenErrors then
        Exit
      else
        raise Exception.Create(SEmptyIndexExpression);
    FExpressions.Add(ParseExpression(Expr));
    fieldNo := TDBFExpression(FExpressions[i]).FieldNo;
    if fieldNo >= 0 then begin
      FIndexInfos[i].KeyType := TDBFDBase(FDBF).GetFieldType(fieldNo);
      FIndexInfos[i].Valid := True; // FExpressions[i].DbfExpression = deNONE; ?
      FIndexInfos[i].Nullable := TDBFDBase(FDBF).GetFieldNullable(fieldNo);
    end;
  end;
end;

procedure TCDXFile.Close;
begin
  FIndexInfos := nil;

  inherited;
end;

procedure TCDXFile.ZapIndex;
var
  i, readed, written, keyLen, keyRecLen, maxLen: integer;
  blockHdr, newBlock: TBytes;
  pNodeHdr: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
  hdr: TCDXHeader;
begin
  FHeader.FreeList := 0;
  FHeader.Version := 0;
  FFileStream.Seek(LongInt(0), soFromBeginning);
  written := FFileStream.Read(FHeader, SizeOf(TCDXHeader));
  if written <> SizeOf(TCDXHeader) then
    raise Exception.Create('TCDXFile.Open exception');

  maxLen := FHeader.RootNode + SizeOf(TCDXCompactIndexNode);
  ReadBlock(FHeader.RootNode, blockHdr);
  pNodeHdr := PCDXCompactIndexNode(@blockHdr[0]);

  keyLen := FHeader.KeyLength;
  keyRecLen := keyLen + SizeOf(TCDXKeyInfo);
  keyInfos := ExtractCdxNode(pNodeHdr, -1);

  for i := 0 to pNodeHdr.NumOfKeys - 1 do begin
    pKeyInfo := PCDXKeyInfo(@keyInfos[i * keyRecLen + keyLen]);
    FFileStream.Seek(LongInt(FastSwap(pKeyInfo.RecNo)), soFromBeginning); // RecNo used as offset
    readed := FFileStream.Read(hdr, SizeOf(TCDXHeader));
    if readed <> SizeOf(TCDXHeader) then
      raise Exception.Create('TCDXFile node exception');
    maxLen := Max(maxLen, FastSwap(pKeyInfo.RecNo) + SizeOf(TCDXHeader));

    // clear root page and save
    CompressCdxNode(i, CDX_NODETYPE_ROOT or CDX_NODETYPE_LEAF, -1, -1, nil, newBlock);
    WriteBlock(hdr.RootNode, newBlock);

    maxLen := Max(maxLen, hdr.RootNode + FPageSize);
  end;
  FFileStream.Size := maxLen;
end;

function TCDXFile.GetIndexNo(const IndexName: string): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to Length(FIndexInfos) - 1 do begin
    if SameText(IndexName, GetIndexName(i)) then begin
      Result := i;
      Exit;
    end;
  end;

  for i := 0 to Length(FIndexInfos) - 1 do begin
    if SameText(IndexName, string(FIndexInfos[i].Expression)) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TCDXFile.GetIndexCount: integer;
begin
  Result := Length(FIndexInfos);
end;

function TCDXFile.GetIndexName(IdxNo: integer): string;
begin
  // Get and Set can be Alter related
  Result := string(FIndexInfos[IdxNo].Name);
end;

function TCDXFile.GetIndexIsValid(IdxNo: integer): boolean;
begin
  Result := FIndexInfos[IdxNo].Valid and (FIndexInfos[IdxNo].KeyType <> DBF_TYPE_TIME); // there is no way to convert TDateTime into same value as FoxPro;
end;

function TCDXFile.GetIndexIsDescending(IdxNo: integer): boolean;
begin
  Result := FIndexInfos[IdxNo].IsDesc;
end;

function TCDXFile.GetIndexIsUnique(IdxNo: integer): boolean;
begin
  if IdxNo < 0 then
    Result := True // False?
  else
    Result := (FIndexInfos[IdxNo].Options and 1) <> 0;
end;

function TCDXFile.GetIndexKeyType(IdxNo: integer): AnsiChar;
begin
  if IdxNo < 0 then
    Result := DBF_TYPE_CHAR
  else
    Result := FIndexInfos[IdxNo].KeyType;
end;

procedure TCDXFile.SetIndexKeyType(IdxNo: integer; const KeyType: AnsiChar);
begin
  if (IdxNo >= 0) and (IdxNo <= High(FIndexInfos)) then
    FIndexInfos[IdxNo].KeyType := KeyType;
end;

function TCDXFile.GetIndexKeyLength(IdxNo: integer): integer;
begin
  if IdxNo < 0 then
    Result := FHeader.KeyLength
  else
    Result := FIndexInfos[IdxNo].KeyLength; // nullable already have 1 extra byte in KeyLength
end;

function TCDXFile.GetIndexKeyRecLength(IdxNo: integer): integer;
begin
  Result := GetIndexKeyLength(IdxNo) + SizeOf(TCDXKeyInfo);
end;

function TCDXFile.GetIndexNullable(IdxNo: integer): boolean;
begin
  Result := FIndexInfos[IdxNo].Nullable;
end;

function TCDXFile.IncEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean;
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
begin
  Result := True;
  Inc(Entry.EntryNo);
  ReadBlock(Entry.BlockNo, block);
  pNode := PCDXCompactIndexNode(@block[0]);
  if Entry.EntryNo >= pNode.NumOfKeys then begin
    // update higher node
    if (Entry.Parent = nil) or (pNode.RightNode = -1) then begin
      Result := False;
      Dec(Entry.EntryNo);
    end
    else begin
      Result := IncEntryNo(Entry.Parent, IdxNo);
      if Result then begin
        Entry.BlockNo := pNode.RightNode;
        Entry.EntryNo := 0; // start from 0 of next node
      end
      else
        Dec(Entry.EntryNo);
    end;
  end;
end;

function TCDXFile.DecEntryNo(Entry: PDBFLinkedListEntry; IdxNo: integer): boolean;
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
begin
  Result := True;
  Dec(Entry.EntryNo);
  if Entry.EntryNo < 0 then begin
    ReadBlock(Entry.BlockNo, block);
    pNode := PCDXCompactIndexNode(@block[0]);
    // update higher node
    if (Entry.Parent = nil) or (pNode.LeftNode = -1) then begin
      Result := False;
      Inc(Entry.EntryNo);
    end
    else begin
      Result := DecEntryNo(Entry.Parent, IdxNo);
      if Result then begin
        Entry.BlockNo := pNode.LeftNode;
        // start from end of next node
        ReadBlock(Entry.BlockNo, block);
        Entry.EntryNo := PCDXCompactIndexNode(@block[0]).NumOfKeys - 1;
      end
      else
        Inc(Entry.EntryNo);
    end;
  end;
end;

{ Search }

function TCDXFile.GetLeftmostEntry(IdxNo: integer): PDBFLinkedListEntry;
var
  block: TBytes;
  blockNo, keyLen: integer;
  parent: PDBFLinkedListEntry;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
  {$IFNDEF VER9P}keyInfos := nil;{$ENDIF}
  Result := nil;
  blockNo := 0;
  keyLen := GetIndexKeyLength(IdxNo);
  while True do begin
    blockNo := GetRootNodeBlockNo(IdxNo, blockNo);
    parent := Result;
    Result := AllocMem(SizeOf(TDBFLinkedListEntry));
    Result.Parent := parent;
    // Result.Child := nil;
    if parent <> nil then
      parent.Child := Result;
    ReadBlock(blockNo, block);
    pNode := PCDXCompactIndexNode(@block[0]);
    Result.BlockNo := blockNo;
    Result.EntryNo := 0;
    Result.IsLeaf := GetIsLeaf(pNode, IdxNo);
    if Result.IsLeaf then
      Break;

    Assert(pNode.NumOfKeys > 0);
    keyInfos := ExtractCdxNode(pNode, IdxNo);
    pKeyInfo := PCDXKeyInfo(@keyInfos[keyLen]);
    blockNo := FastSwap(pKeyInfo.Offset);
  end;
end;

function TCDXFile.GetRightmostEntry(IdxNo: integer): PDBFLinkedListEntry;
var
  block: TBytes;
  blockNo, keyLen: integer;
  parent: PDBFLinkedListEntry;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
  {$IFNDEF VER9P}keyInfos := nil;{$ENDIF}
  Result := nil;
  blockNo := 0;
  keyLen := GetIndexKeyLength(IdxNo);
  while True do begin
    blockNo := GetRootNodeBlockNo(IdxNo, blockNo);
    parent := Result;
    Result := AllocMem(SizeOf(TDBFLinkedListEntry));
    Result.Parent := parent;
    // Result.Child := nil;
    if parent <> nil then
      parent.Child := Result;
    ReadBlock(blockNo, block);
    pNode := PCDXCompactIndexNode(@block[0]);
    Result.BlockNo := blockNo;
    Result.IsLeaf := GetIsLeaf(pNode, IdxNo);
    Result.EntryNo := pNode.NumOfKeys - 1; // if 0?
    if Result.IsLeaf then
      Break;

    Assert(pNode.NumOfKeys > 0);
    keyInfos := ExtractCdxNode(pNode, IdxNo);
    pKeyInfo := PCDXKeyInfo(@keyInfos[keyLen]);
    blockNo := FastSwap(pKeyInfo.Offset);
  end;
end;

function TCDXFile.FindFirstEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
begin
  Result := Find(nil, 0, IdxNo, Value, IsFound);
end;

function TCDXFile.FindLastEntry(IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  FirstEntry: PDBFLinkedListEntry;
begin
  FirstEntry := Find(nil, 0, idxNo, Value, IsFound);
  if IsFound then begin
    Result := GetEqualRightEntry(FirstEntry, IdxNo, Value, IsFound);
    FreeLinkedList(FirstEntry);
  end
  else
    Result := FirstEntry;
end;

function TCDXFile.GetEqualRightEntry(FirstEntry: PDBFLinkedListEntry; IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  wasFound: boolean;
  i, cmpResult, keyLen, keyRecLen: integer;
  keyInfos: TBytes;
  keyType: AnsiChar;
begin
  {$IFNDEF VER9P}keyInfos := nil;{$ENDIF}
  // returns rightmost equal
  Result := CloneLinkedList(FirstEntry);
  Result := LinkedListLeaf(Result);
  IsFound := False;
  wasFound := False;

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);
  keyType := GetIndexKeyType(IdxNo);
  while True do begin
    ReadBlock(Result.BlockNo, block);
    pNode := PCDXCompactIndexNode(@block[0]);
    keyInfos := ExtractCdxNode(pNode, IdxNo);
    // current EntryNo suppose to be from Find, start from it to have found True and try IncEntryNo
    for i := Result.EntryNo to pNode.NumOfKeys - 1 do begin
      cmpResult := Compare(Value,  @keyInfos[i * keyRecLen], keyType, keyLen);
      if cmpResult <> 0 then begin
        if wasFound and (i = 0) then // we incremented before, but new node has no equal
          DecEntryNo(Result, IdxNo);
        Exit;
      end;
      IsFound := True;
      Result.EntryNo := i;
    end;

    if IsFound then begin // try next node, current contains equal at last entry
      if not IncEntryNo(Result, IdxNo) then // eof
        Break;
      wasFound := True;
    end;
  end;
end;

function TCDXFile.Find(Parent: PDBFLinkedListEntry; BlockNo, IdxNo: integer; Value: PAnsiChar; out IsFound: boolean): PDBFLinkedListEntry;
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  i, keyLen, keyRecLen, cmpResult: integer;
  found: boolean;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
  keyType: AnsiChar;
begin
  // finds leftmost equal
  // fills Nodes with left position of equal (or where it suppose to be if not found)
  IsFound := False;
  Result := AllocMem(SizeOf(TDBFLinkedListEntry));
  Result.Parent := Parent;
  // Result.Child := nil;
  if Parent <> nil then
    Parent.Child := Result;

  Result.BlockNo := GetRootNodeBlockNo(IdxNo, BlockNo);
  ReadBlock(Result.BlockNo, block);
  pNode := PCDXCompactIndexNode(@block[0]);

  Result.IsLeaf := GetIsLeaf(pNode, IdxNo);
  // it is possible to make it faster by reusing Key instead of list of extracted keys
  keyInfos := ExtractCdxNode(pNode, IdxNo);

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);
  keyType := GetIndexKeyType(IdxNo);

  found := False;
  for i := 0 to pNode.NumOfKeys - 1 do begin
    cmpResult := Compare(Value,  @keyInfos[i * keyRecLen], keyType, keyLen);
    if cmpResult <= 0 then begin // always ASC in CDX
      Result.EntryNo := i;
      IsFound := cmpResult = 0;
      found := True;
      if not Result.IsLeaf then begin
        pKeyInfo := PCDXKeyInfo(@keyInfos[i * keyRecLen + keyLen]);
        Result := Find(Result, FastSwap(pKeyInfo.Offset), IdxNo, Value, IsFound);
      end;
      Break;
    end;
  end;
  if not found then begin
    // set position
    if not Result.IsLeaf then begin
      Result.EntryNo := pNode.NumOfKeys - 1;
      pKeyInfo := PCDXKeyInfo(@keyInfos[Result.EntryNo * keyRecLen + keyLen]);
      Result := Find(Result, FastSwap(pKeyInfo.Offset), IdxNo, Value, IsFound);
    end
    else
      Result.EntryNo := pNode.NumOfKeys;
  end;
end;

procedure TCDXFile.FillListByEntries(FirstEntry, LastEntry: PDBFLinkedListEntry; IdxNo: integer; List: TList);
var
  lastEntryNo, keyLen, keyRecLen: integer;
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
  {$IFNDEF VER9P}keyInfos := nil;{$ENDIF}
  FirstEntry := LinkedListLeaf(FirstEntry);
  LastEntry := LinkedListLeaf(LastEntry);

  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);
  // todo?: maybe reverse for DESC order
  while True do begin
    ReadBlock(FirstEntry.BlockNo, block);
    pNode := PCDXCompactIndexNode(@block[0]);
    keyInfos := ExtractCdxNode(pNode, IdxNo);
    if FirstEntry.BlockNo = LastEntry.BlockNo then
      lastEntryNo := LastEntry.EntryNo
    else
      lastEntryNo := pNode.NumOfKeys - 1;

    while FirstEntry.EntryNo <= lastEntryNo do begin
      pKeyInfo := PCDXKeyInfo(@keyInfos[FirstEntry.EntryNo * keyRecLen + keyLen]);;
      List.Add(Pointer(NativeUInt(FastSwap(pKeyInfo.RecNo))));
      Inc(FirstEntry.EntryNo);
    end;

    if FirstEntry.BlockNo = LastEntry.BlockNo then
      Break;
    if not IncEntryNo(FirstEntry, IdxNo) then
      Break;
  end;
end;

procedure TCDXFile.UpdateNodeEntry(Entry: PDBFLinkedListEntry; IdxNo, UpdateEntryNo: integer; Value: PAnsiChar);
var
  offset, keyRecLen: integer;
  block, newBlock: TBytes;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
begin
  keyRecLen := GetIndexKeyRecLength(IdxNo);

  offset := GetRootNodeBlockNo(IdxNo, Entry.BlockNo);

  ReadBlock(offset, block);
  pNode := PCDXCompactIndexNode(@block[0]);
  Assert((pNode.NumOfKeys > 0) and (UpdateEntryNo < pNode.NumOfKeys));

  keyInfos := ExtractCdxNode(pNode, IdxNo);
  Move(Value^, keyInfos[UpdateEntryNo * keyRecLen], keyRecLen);
  if not CompressCdxNode(IdxNo, pNode.Attributes, pNode.LeftNode, pNode.RightNode, keyInfos, newBlock) then
    raise Exception.Create('Error compressing node');
  WriteBlock(offset, newBlock);
  if (Entry.Parent <> nil) and (UpdateEntryNo >= pNode.NumOfKeys) then // update parent key right offset
    UpdateNodeEntry(Entry.Parent, IdxNo, Entry.Parent.EntryNo, @keyInfos[(pNode.NumOfKeys - 1) * keyRecLen]);
end;

procedure TCDXFile.UpdateNodeOffsets(Offset, LeftBlockNo, RightBlockNo: integer);
var
  block: TBytes;
  pNode: PCDXCompactIndexNode;
begin
  Assert(Offset > 0);
  ReadBlock(Offset, block);
  pNode := PCDXCompactIndexNode(@block[0]);
  if LeftBlockNo > 0 then
    pNode.LeftNode := LeftBlockNo;
  if RightBlockNo > 0 then
    pNode.RightNode := RightBlockNo;
  WriteBlock(offset, block);
end;

procedure TCDXFile.InsertIntoNode(var Entry: PDBFLinkedListEntry; IdxNo, LeftBlockNo, RightBlockNo: integer; Value: PAnsiChar);
var
  keyLen, keyRecLen, count, len, leftBlockOffset, rightBlockOffset,
  offset, insertInto, splitAt: integer;
  block, newBlock: TBytes;
  pNode: PCDXCompactIndexNode;
  keyInfos, temp, temp2: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);

  offset := GetRootNodeBlockNo(IdxNo, Entry.BlockNo);

  ReadBlock(offset, block);
  pNode := PCDXCompactIndexNode(@block[0]);

  keyInfos := ExtractCdxNode(pNode, IdxNo);
  insertInto := Entry.EntryNo;
  count := pNode.NumOfKeys - insertInto;
  SetLength(temp, Length(keyInfos) + keyRecLen);
  len := Length(temp) div keyRecLen;
  if insertInto > 0 then
    Move(keyInfos[0], temp[0], insertInto * keyRecLen);
  Move(Value^, temp[insertInto * keyRecLen], keyRecLen);
  if count > 0 then
    Move(keyInfos[insertInto * keyRecLen], temp[(insertInto + 1) * keyRecLen], count * keyRecLen);
  if not CompressCdxNode(IdxNo, pNode.Attributes, pNode.LeftNode, pNode.RightNode, temp, newBlock) then begin
  {$IFDEF LOG_PACKETS}
    AddToLog(sLineBreak + 'Splitting ' + FFileName);
    Dump(0, IdxNo);
  {$ENDIF}
    // split, if last entry then previous to left, added to right, else split by newsize/2 or size/2 + 1
    // add right block, link to left, second parent key/RecNo
    // add left block, link to right ^, first parent key/RecNo, copy keys here
    rightBlockOffset := FFileStream.Size;
    if Entry.Parent = nil then // root
      leftBlockOffset := rightBlockOffset + SizeOf(TCDXCompactIndexNode)
    else
      leftBlockOffset := offset;
    if insertInto >= pNode.NumOfKeys then
      splitAt := pNode.NumOfKeys
    else
      splitAt := pNode.NumOfKeys div 2 + 1;

    // write right block
    count := len - splitAt;
    SetLength(temp2, keyRecLen * count);
    Move(temp[keyRecLen * splitAt], temp2[0], keyRecLen * count);
    CompressCdxNode(IdxNo, pNode.Attributes and not(CDX_NODETYPE_ROOT), leftBlockOffset, pNode.RightNode, temp2, newBlock);
    AppendBlock(newBlock);

    // write left block
    SetLength(temp2, keyRecLen * splitAt);
    Move(temp[0], temp2[0], keyRecLen * splitAt);
    CompressCdxNode(IdxNo, pNode.Attributes and not(CDX_NODETYPE_ROOT), pNode.LeftNode, rightBlockOffset, temp2, newBlock);
    if Entry.Parent = nil then
      AppendBlock(newBlock)
    else
      WriteBlock(offset, newBlock);

    if Entry.Parent = nil then begin
      // rewrite root
      // set keys, InsertIntoNode right and left
      SetLength(temp2, keyRecLen shl 1);
      // left
      Move(temp[(splitAt - 1) * keyRecLen], temp2[0], keyRecLen);
      pKeyInfo := @temp2[keyLen];
      pKeyInfo.Offset := FastSwap(leftBlockOffset);
      // right
      Move(temp[(len - 1) * keyRecLen], temp2[keyRecLen], keyRecLen);
      pKeyInfo := @temp2[keyRecLen + keyLen];
      pKeyInfo.Offset := FastSwap(rightBlockOffset);
      CompressCdxNode(IdxNo, pNode.Attributes and not(CDX_NODETYPE_LEAF), pNode.LeftNode, pNode.RightNode, temp2, newBlock);
      WriteBlock(offset, newBlock);
    end
    else begin
      SetLength(temp2, keyRecLen);
      pKeyInfo := @temp2[keyLen];
      // update right offset
      Move(temp[(len - 1) * keyRecLen], temp2[0], keyRecLen);
      pKeyInfo.Offset := FastSwap(rightBlockOffset);
      UpdateNodeEntry(Entry.Parent, IdxNo, Entry.Parent.EntryNo, @temp2[0]);
      // insert left key + offset into parent
      Move(temp[(splitAt - 1) * keyRecLen], temp2[0], keyRecLen);
      pKeyInfo.Offset := FastSwap(leftBlockOffset);
      InsertIntoNode(Entry.Parent, IdxNo, -1, -1, @temp2[0]);
    end;

    // update offset of nodes on the left and right
    if pNode.LeftNode > 0 then
      UpdateNodeOffsets(pNode.LeftNode, -1, leftBlockOffset);
    if pNode.RightNode > 0 then
      UpdateNodeOffsets(pNode.RightNode, rightBlockOffset, -1);
  {$IFDEF LOG_PACKETS}
    AddToLog(sLineBreak + 'After Splitting');
    Dump(0, IdxNo);
  {$ENDIF}
  end
  else begin
    WriteBlock(offset, newBlock);
    if (Entry.Parent <> nil) and (insertInto >= pNode.NumOfKeys) then begin
      // update parent key right offset
      pKeyInfo := @temp[(len - 1) * keyRecLen + keyLen];
      pKeyInfo.Offset := FastSwap(offset);
      UpdateNodeEntry(Entry.Parent, IdxNo, Entry.Parent.EntryNo, @temp[(len - 1) * keyRecLen]);
    end;
  end;
end;

procedure TCDXFile.Add(IdxNo, RecNo: integer; Value: PAnsiChar);
var
  entry: PDBFLinkedListEntry;
  found: boolean;
  keyLen, keyRecLen: integer;
  temp: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
{$IFDEF LOG_PACKETS}
  AddIndexStartTick := GetTickCount;
{$ENDIF}
  {$IFDEF VER9P}temp := nil;{$ENDIF}
  entry := FindLastEntry(IdxNo, Value, found);
  if found then
    Inc(entry.EntryNo); // just increment, will be put on compress

  if GetIndexIsUnique(IdxNo) and found then
    raise Exception.Create(SDuplicateIndexValue)
  else begin
    Assert(entry.BlockNo > 0);
    keyLen := GetIndexKeyLength(IdxNo);
    keyRecLen := GetIndexKeyRecLength(IdxNo);
    SetLength(temp, keyRecLen);
    Move(Value^, temp[0], keyLen);
    pKeyInfo := @temp[keyLen];
    pKeyInfo.RecNo := FastSwap(RecNo);
    pKeyInfo.Offset := 0;
    InsertIntoNode(entry, IdxNo, -1, -1, @temp[0]);
  end;
  FreeLinkedList(entry);
{$IFDEF LOG_PACKETS}
  AddIndexTickCount := AddIndexTickCount + GetTickCount - AddIndexStartTick;
{$ENDIF}
end;

procedure TCDXFile.RemoveEntry(Entry: PDBFLinkedListEntry; IdxNo: integer; Node: PCDXCompactIndexNode; var Block: TBytes; EntryNo: integer);
var
  i, keyRecLen: integer;
  temp, newBlock: TBytes;
  pN: PCDXCompactIndexNode;
  keyInfos: TBytes;
begin
  // Block is keyInfos
  keyRecLen := GetIndexKeyRecLength(IdxNo);

  // move keys
  if EntryNo < Node.NumOfKeys - 1 then
    for i := EntryNo to Node.NumOfKeys - 2 do
      Move(Block[(i + 1) * keyRecLen], Block[i * keyRecLen], keyRecLen);

  SetLength(Block, Length(Block) - keyRecLen);
  if (Length(Block) = 0) and ((Node.Attributes and CDX_NODETYPE_ROOT) <> 0) then
    // removed up to empty root, make it leaf again
    Node.Attributes := Node.Attributes or CDX_NODETYPE_LEAF;
  CompressCdxNode(IdxNo, Node.Attributes, Node.LeftNode, Node.RightNode, Block, newBlock);
  WriteBlock(Entry.BlockNo, newBlock);
  if (Length(Block) = 0) and (Entry.Parent <> nil) then begin
    // set left and right to nearest blocks
    if Node.LeftNode <> -1 then begin
      ReadBlock(Node.LeftNode, temp);
      pN := PCDXCompactIndexNode(@temp[0]);
      pN.RightNode := Node.RightNode;
      WriteBlock(Node.LeftNode, temp);
    end;
    if Node.RightNode <> -1 then begin
      ReadBlock(Node.RightNode, temp);
      pN := PCDXCompactIndexNode(@temp[0]);
      pN.LeftNode := Node.LeftNode;
      WriteBlock(Node.RightNode, temp);
    end;

    ReadBlock(Entry.Parent.BlockNo, temp);
    pN := PCDXCompactIndexNode(@temp[0]);
    keyInfos := ExtractCdxNode(pN, IdxNo);
    RemoveEntry(Entry.Parent, IdxNo, pN, keyInfos, Entry.Parent.EntryNo);
  end;
end;

procedure TCDXFile.Delete(IdxNo, RecNo: integer; Value: PAnsiChar);
var
  FirstEntry, LastEntry: PDBFLinkedListEntry;
  found, recNoFound: boolean;
  keyLen, keyRecLen, lastEntryNo: integer;
  block: TBytes;
  pNode: PCDXCompactIndexNode;
  keyInfos: TBytes;
  pKeyInfo: PCDXKeyInfo;
begin
  keyLen := GetIndexKeyLength(IdxNo);
  keyRecLen := GetIndexKeyRecLength(IdxNo);

  FirstEntry := FindFirstEntry(IdxNo, Value, found);
  if found then begin
    LastEntry := GetEqualRightEntry(FirstEntry, idxNo, Value, found);

    recNoFound := False;
    while True do begin
      ReadBlock(FirstEntry.BlockNo, block);
      pNode := PCDXCompactIndexNode(@block[0]);
      keyInfos := ExtractCdxNode(pNode, IdxNo);
      if FirstEntry.BlockNo = LastEntry.BlockNo then
        lastEntryNo := LastEntry.EntryNo
      else
        lastEntryNo := pNode.NumOfKeys - 1;

      while FirstEntry.EntryNo <= lastEntryNo do begin
        pKeyInfo := @keyInfos[FirstEntry.EntryNo * keyRecLen + keyLen];
        if FastSwap(pKeyInfo.RecNo) = RecNo then begin
          recNoFound := True;

          RemoveEntry(FirstEntry, IdxNo, pNode, keyInfos, FirstEntry.EntryNo);
          Break;
        end;
        Inc(FirstEntry.EntryNo);
      end;

      if recNoFound or (FirstEntry.BlockNo = LastEntry.BlockNo) then
        Break;
      if not IncEntryNo(FirstEntry, IdxNo) then
        Break;
    end;
    FreeLinkedList(LastEntry);
  end;
  FreeLinkedList(FirstEntry);
end;

{$ENDIF}

end.
