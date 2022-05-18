
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRParser
//  Created:            27.03.98
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRParser;

interface

{$IFNDEF IS_UNICODE}
{$IFNDEF PUREPASCAL}
  {$DEFINE USEANSIQUOTE}
{$ENDIF}
{$ENDIF}

uses
  SysUtils, Classes, SyncObjs,
{$IFDEF PERF_COUNTER}
  Debug,
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
  Generics.Collections,
{$ENDIF}
  CRTypes, CLRClasses;

const
  lcEnd     =  0;
  lcLexem   = -100;
  lcSymbol  = -102;
  lcIdent   = -103;  // identificator
  lcNumber  = -105;
  lcString  = -106;
  lcBlank   = -107;
  lcComment = -108;

  lxExclamation   = 1;
  lxDoubleQuotes  = 2;
  lxOctothorp     = 3;
  lxDollar        = 4;
  lxPercent       = 5;
  lxAmp           = 6;
  lxQuote         = 7;
  lxLeftBracket   = 8;
  lxRightBracket  = 9;
  lxAsterisk      = 10;
  lxPlus          = 11;
  lxComma         = 12;
  lxDash          = 13;
  lxPoint         = 14;
  lxSlash         = 15;
  lxColon         = 16;
  lxSemicolon     = 17;
  lxLess          = 18;
  lxEqual         = 19;
  lxMore          = 20;
  lxQuestion      = 21;
  lxAt            = 22;
  lxLeftSqBracket = 23;
  lxBackSlash     = 24;
  lxRightSqBracket = 25;
  lxCircumflex    = 26;
  lxUnderline     = 27;
  lxGrave         = 28;
  lxMaxSymbolValue = lxGrave;

  lxSQLFirst  = 100;
  lxALL       = lxSQLFirst;
  lxAND       = lxALL + 1;
  lxAS        = lxAND + 1;
  lxBEGIN     = lxAS + 1;
  lxBY        = lxBEGIN + 1;
  lxCASE      = lxBY + 1;
  lxCOMMIT    = lxCASE + 1;
  lxDECLARE   = lxCOMMIT + 1;
  lxDELETE    = lxDECLARE + 1;
  lxDESC      = lxDELETE + 1;
  lxDISTINCT  = lxDESC + 1;
  lxELSE      = lxDISTINCT + 1;
  lxEND       = lxELSE + 1;
  lxEXECUTE   = lxEND + 1;
  lxFETCH     = lxEXECUTE + 1;
  lxFOR       = lxFETCH + 1;
  lxFROM      = lxFOR + 1;
  lxFULL      = lxFROM + 1;
  lxGROUP     = lxFULL + 1;
  lxHAVING    = lxGROUP + 1;
  lxINNER     = lxHAVING + 1;
  lxINSERT    = lxINNER + 1;
  lxINTERSECT = lxINSERT + 1;
  lxINTO      = lxINTERSECT + 1;
  lxIS        = lxINTO + 1;
  lxJOIN      = lxIS + 1;
  lxLEFT      = lxJOIN + 1;
  lxLIMIT     = lxLEFT + 1;
  lxLOCK      = lxLIMIT + 1;
  lxMINUS     = lxLOCK + 1;
  lxNOT       = lxMINUS + 1;
  lxOFFSET    = lxNOT + 1;
  lxON        = lxOFFSET + 1;
  lxONLY      = lxON + 1; // PostgreSQL lexeme in the FROM statement
  lxOR        = lxONLY + 1;
  lxORDER     = lxOR + 1;
  lxOUT       = lxORDER + 1;
  lxOUTER     = lxOUT + 1;
  lxOUTPUT    = lxOUTER + 4;
  lxRELEASE   = lxOUTPUT + 1;
  lxRETURNING = lxRELEASE + 1;
  lxRIGHT     = lxRETURNING + 1;
  lxROLLBACK  = lxRIGHT + 1;
  lxSAVEPOINT = lxROLLBACK + 1;
  lxSELECT    = lxSAVEPOINT + 1;
  lxSET       = lxSELECT + 1;
  lxTHEN      = lxSET + 1;
  lxTO        = lxTHEN + 1;
  lxTRANSACTION = lxTO + 1;
  lxUNION     = lxTRANSACTION + 1;
  lxUPDATE    = lxUNION + 1;
  lxWHEN      = lxUPDATE + 1;
  lxWHERE     = lxWHEN + 1;
  lxWITH      = lxWHERE + 1;
  lxVALUES    = lxWITH + 1;

  // Common for SQL Server, ASE, MySQL
  lxCALL      = lxVALUES + 1;
  lxCREATE    = lxCALL + 1;
  lxEXEC      = lxCREATE + 1;

  BLOCK_SIZE = 64 * 1024;

type
  TParserClass = class of TParser;
  TSQLParserClass = class of TSQLParser;
  TCharSet = set of AnsiChar;

  TLexemList = class(TIntValueStringList)
  private
    FIndexes: TCRObjectList;
    FMaxLength: Integer;

    function GetIndex(Len: Integer): TList;
  protected
    FLock: TCriticalSection;

    procedure ListChanged; override;
    procedure CreateIndexes;
    procedure FreeIndexes;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Indexes[Len: Integer]: TList read GetIndex;
    property MaxLength: Integer read FMaxLength;
  end;

{$IFDEF AUTOREFCOUNT}
  TClauseList = TList<Integer>;
{$ELSE}
  TClauseList = class(TList)
  protected
    function Get(Index: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure Put(Index: Integer; Item: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}
  public
    function IndexOf(Item: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function Add(Item: Integer): Integer; {$IFDEF USE_INLINE}inline;{$ENDIF}

    property Items[Index: Integer]: Integer read Get write Put; default;
  end;
{$ENDIF}

  TParser = class
  protected
    FOmitBlank: boolean;
    FOmitComment: boolean;
    FOmitInlineComment: boolean;
    FUppered: boolean;
    FQuotedString: boolean;
    FAdvancedStringParsing: boolean;
    FDecSeparator: char;
    FCommentBegin: string;
    FCommentEnd: string;

    OldPos: integer;
    OldOldPos: Int64;
    FCurrLine: Int64;
    FPrevLine: Int64;
    FCurrBegLine: integer;
    FPrevBegLine: integer;

    FSavedPos: integer; // local var buf in previous version
    FLexemPos: integer; // start Lexem pos in Text
    FLexemLength: integer; // actual length of FLexemArray;
    FToken: integer;
    FLexem: string;

    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FEncoding: Encoding;
    FStream: TStream;
    FStartStreamPosition: Int64; // bytes
    FStreamSize: Int64; // bytes
    FStoredBlocks: TIntValueStringList;
    FCurrentBlock: string;
    FBlockOffset: integer;
    FBlockSize: integer;
    FTmpBuffer: TBytes;
    FAlternativeQuoting: boolean; // supported in Oracle 10
    FDollarQuoting: boolean; // supported in PostgreSQL

    function GetCharSize(ByteSize: Int64): Int64;
    procedure ReadNextBlock;
    function GetChar(Index: integer): char; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetStoredChar(Index: integer): char;
    procedure DecreaseBlockParameters;

  protected
    TextLength: integer;
    Pos: integer;
    StreamLength: Int64;
    Offset: Int64;

    FSymbolLexems: TLexemList;
    FKeywordLexems: TLexemList;
    FOmitKeywords: boolean;
    FDesiredLexem: string;
    FDesiredLexemFound: boolean;

    procedure InitParser; virtual;

    function IsSymbol(Ch: char): boolean; virtual;
    function IsAlpha(Ch: char): boolean; virtual;
    function IsNumber(Ch: char): boolean; virtual;
    function IsStringQuote(Ch: char): boolean; virtual;
    procedure ToRightQuoteP(RightQuote: char); virtual;
  {$IFDEF USEANSIQUOTE}
    procedure ToRightQuoteA(RightQuote: AnsiChar); virtual;
  {$ENDIF}
    procedure ToRightQuote(RightQuote: char); virtual; // Search right quote of quoted string value or quoted identifier
    function IsIdentQuote(Ch: char): boolean; virtual;
    function IsInlineComment(Ch: char; Pos: integer): boolean; virtual;
    function FindLexemIndex(const LexemPos, LexemLength: integer; Lexems: TLexemList): integer;
    function InternalGetNext: integer;

    function CopyText(Pos, Count: integer): string;
    procedure AddToLexemArray(Index: integer; const Len: integer);

    property Text[Index: integer]: char read GetChar;
    property AlternativeQuoting: boolean read FAlternativeQuoting write FAlternativeQuoting;
    property DollarQuoting: boolean read FDollarQuoting write FDollarQuoting;

  public
    constructor Create(const Text: string); overload; virtual;
    constructor Create(const Stream: TStream; AEncoding: Encoding = nil); overload;
    constructor Create(const Stream: TStream; ASize: Int64; AEncoding: Encoding = nil); overload; virtual;
    destructor Destroy; override;

    function GetStream: TStream;
    procedure SetText(const Text: string);

    procedure ToBegin; virtual;
    procedure Back;
    function GetNext(out Lexem: string): integer; virtual;
    function GetNextToken: integer;

    function ToLexem(const Code: integer; const SkipSubQueries: boolean = False): integer; overload;
    function ToLexem(const Codes: array of integer; const IsNestedQuery: boolean = False): integer; overload;
    function ToLexem(const Lexem: string): boolean; overload;

    function IsSymbolCode(Code: integer): boolean;
    function GetSymbolByCode(Code: integer): string;

    function CurrPos: Int64;
    function PrevPos: Int64;
    function PrevPrevPos: Int64;
    function CurrLine: Int64;
    function PrevLine: Int64;
    function CurrCol: Int64;
    function PrevCol: Int64;

    property StartStreamPosition: Int64 read FStartStreamPosition;

    property Token: integer read FToken;
    property Lexem: string read FLexem;

    property Encoding: Encoding read FEncoding;
    property OmitBlank: boolean read FOmitBlank write FOmitBlank;
    property OmitComment: boolean read FOmitComment write FOmitComment;
    property OmitInlineComment: boolean read FOmitInlineComment write FOmitInlineComment;
    property OmitKeywords: boolean read FOmitKeywords write FOmitKeywords;
    property Uppered: boolean read FUppered write FUppered;
    property QuotedString: boolean read FQuotedString write FQuotedString;
    property AdvancedStringParsing: boolean read FAdvancedStringParsing write FAdvancedStringParsing;
    property DecSeparator: char read FDecSeparator write FDecSeparator;
    property CommentBegin: string read FCommentBegin write FCommentBegin;
    property CommentEnd: string read FCommentEnd write FCommentEnd;

    property SymbolLexems: TLexemList read FSymbolLexems;
    property KeywordLexems: TLexemList read FKeywordLexems;
  end;

  TSQLParser = class(TParser)
  protected
    FClauses: TClauseList; // lxWHERE, lxLIMIT, lxORDER etc. Must be filled on Create

    procedure InitParser; override;
    function IsAlpha(Ch: char): boolean; override;
    function IsStringQuote(Ch: char): boolean; override;
    function IsIdentQuote(Ch: char): boolean; override;
    function IsInlineComment(Ch: char; Pos: integer): boolean; override;

  public
    function IsClauseLexem(Code: integer): boolean; // Is SELECT statement clause
//    function PosClauseLexem(Code: integer): integer; // Code position in list of SELECT statement clauses. -1 if not found
    function CompareClauseLexems(const Code1, Code2: integer): integer;
    function IsMacroAllowed(Code: integer): boolean; virtual;
    function IsSelectModifier(Code: integer): boolean; virtual;
    class function IsNumericMacroNameAllowed: boolean; virtual;
    class function IsFunctionOrConst(const UpperedName: string): boolean; virtual;
    class function IsQuasiColumn(const UpperedName: string): boolean; virtual;
  end;

var
  CommonSymbolLexems, CommonKeywordLexems: TLexemList;
  SQLSymbolLexems, SQLKeywordLexems: TLexemList;
  SQLClauses: TClauseList;

  function _GetFrom(
    const SQL: string;
    ParserClass: TSQLParserClass;
    OmitComment: boolean;
    const MacroChar: string
  ): string;
  function _GetWhere(
    const SQL: string;
    ParserClass: TSQLParserClass;
    OmitComment: boolean;
    const MacroChar: string
  ): string;
  function _GetOrderBy(
    const SQL: string;
    ParserClass: TSQLParserClass
  ): string;
  procedure _FindWherePosition(
    const SQL: string; var Condition: string;
    ParserClass: TSQLParserClass;
    OmitComment: boolean;
    const MacroChar: string;
    out StartPos, EndPos: integer
  );
  function _AddWhere(
    const SQL: string; Condition: string;
    ParserClass: TSQLParserClass;
    OmitComment: boolean;
    const MacroChar: string
  ): string;
  function _SetWhere(
    const SQL: string; Condition: string;
    ParserClass: TSQLParserClass;
    OmitComment: boolean
  ): string;
  function _SetOrderBy(
    const SQL: string; Fields: string;
    ParserClass: TSQLParserClass
  ): string;

implementation

uses
  CRFunctions, DAConsts;

{ TLexemList }

constructor TLexemList.Create;
begin
  inherited;

  FLock := TCriticalSection.Create;
end;

destructor TLexemList.Destroy;
begin
  inherited;

  FLock.Free;
end;

function TLexemList.GetIndex(Len: Integer): TList;
begin
  if FIndexes = nil then
    CreateIndexes;

  if FIndexes.Count > Len then
    Result := TList(FIndexes[Len])
  else
    Result := nil
end;

procedure TLexemList.ListChanged;
begin
  inherited;
  FreeIndexes;
end;

procedure TLexemList.CreateIndexes;
var
  i: Integer;
  SLen: Integer;
  Index: TList;
  Items: TCRObjectList;
begin
  if FIndexes <> nil then
    Exit;

  FLock.Enter;
  try
    if FIndexes <> nil then
      Exit;

    Items := TCRObjectList.Create;
    Items.Add(nil);

    for i := 0 to Count - 1 do begin
      SLen := Length(Keys[i]);
      if SLen = 0 then
        Continue;
      if Items.Count <= SLen then
        Items.Count := SLen + 1;
      Index := TList(Items[SLen]);
      if Index = nil then begin
        Index := TList.Create;
        Items[SLen] := Index;
      end;
      Index.Add(Pointer(NativeUInt(i)));
    end;

    FMaxLength := Items.Count - 1;
    FIndexes := Items;
  finally
    FLock.Leave;
  end;
end;

procedure TLexemList.FreeIndexes;
begin
  if FIndexes = nil then
    Exit;

  FLock.Enter;
  try
    FreeAndNil(FIndexes);
  finally
    FLock.Leave;
  end;
end;

{$IFNDEF AUTOREFCOUNT}

{ TClauseList }


function TClauseList.Get(Index: Integer): Integer;
begin
  Result := Integer(NativeUInt(inherited Get(Index)));
end;

procedure TClauseList.Put(Index: Integer; Item: Integer);
begin
  inherited Put(Index, Pointer(NativeInt(Item)));
end;

function TClauseList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(Pointer(NativeInt(Item)));
end;

function TClauseList.IndexOf(Item: Integer): Integer;
begin
  Result := inherited IndexOf(Pointer(NativeInt(Item)));
end;

{$ENDIF}

{ TParser }

constructor TParser.Create(const Text: string);
begin
  inherited Create;

  FCurrentBlock := Text;
  TextLength := Length(Text);
  FBlockSize := TextLength;

  InitParser;
end;

constructor TParser.Create(const Stream: TStream; AEncoding: Encoding = nil);
begin
  Create(Stream, -1, AEncoding);
end;

constructor TParser.Create(const Stream: TStream; ASize: Int64; AEncoding: Encoding = nil);
begin
  inherited Create;

  FStoredBlocks := TIntValueStringList.Create;
  FStream := Stream;
  FEncoding := AEncoding;
  if FEncoding = nil then
    FEncoding := Encoding.Default;

  if ASize = -1 then
    ASize := Stream.Size - Stream.Position;
  FStartStreamPosition := Stream.Position;
  FStreamSize := ASize;
  StreamLength := GetCharSize(FStreamSize);

  SetLength(FTmpBuffer, BLOCK_SIZE);

  if StreamLength > MaxInt then
    TextLength := MaxInt
  else
    TextLength := StreamLength;

  InitParser;
end;

destructor TParser.Destroy;
begin
  FStoredBlocks.Free;

  inherited;
end;

procedure TParser.InitParser;
begin
  FSymbolLexems := CommonSymbolLexems;
  FKeywordLexems := CommonKeywordLexems;
  FCommentBegin := '/*';
  FCommentEnd := '*/';
  // Performance optimization
  FDecSeparator := char({$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);

  FOmitBlank := True;
  FUppered := True;
  FQuotedString := False;

  ToBegin;
end;

function TParser.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TParser.SetText(const Text: string);
begin
  FCurrentBlock := Text;
  TextLength := Length(Text);
  FBlockSize := TextLength;

  ToBegin;
end;

function TParser.IsSymbol(Ch: char): boolean;
begin
  case Ch of
    '!'..'/':
      Result := True;
    ':'..'@':
      Result := True;
    '['..'^':
      Result := True;
    '{'..'~':
      Result := True;
    '`':
      Result := True;
    #127:
      Result := True;
    else
      Result := False;
  end;
end;

function TParser.IsAlpha(Ch: char): boolean;
begin
  case Ord(Ch) of
    Ord('a')..Ord('z'):
      Result := True;
    Ord('A')..Ord('Z'):
      Result := True;
    Ord('_'):
      Result := True;
    Ord(#128)..Ord(High(char)):
      Result := True;
    else
      Result := False;
  end;
end;

function TParser.IsNumber(Ch: char): boolean;
begin
  case Ch of
    '0'..'9':
      Result := True;
    else
      Result := False;
  end;
end;

function TParser.IsStringQuote(Ch: char): boolean;
begin
  case Ch of
     '''':
      Result := True;
     '"':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TParser.ToRightQuote(RightQuote: char);
begin
  if FAdvancedStringParsing or (FStream <> nil) then
    ToRightQuoteP(RightQuote)
  else
  {$IFDEF USEANSIQUOTE}
    ToRightQuoteA(RightQuote);
  {$ELSE}
    while (Pos <= TextLength) and (Text[Pos] <> RightQuote) do
      Inc(Pos);
  {$ENDIF}
end;

procedure TParser.ToRightQuoteP(RightQuote: char);
var
  c: char;
begin
  while (Pos <= TextLength) do begin
    c := Text[Pos];
    if (c = #13) or (c = #10) then begin
      if (Pos < TextLength) and (Text[Pos + 1] = #10) then
        inc(Pos);
      Inc(FCurrLine);
      FCurrBegLine := Pos + 1;
    end
    else
    if c = RightQuote then
      Break;
    inc(Pos);
  end;
end;

{$IFDEF USEANSIQUOTE}
procedure TParser.ToRightQuoteA(RightQuote: AnsiChar);
var
  i: NativeInt;
begin
  i := TextLength - Pos + 1 {Pos is counted from 1} + 1 {#0};
  asm
{$IFDEF CPU64}
    PUSH RAX
    PUSH RDI
    PUSH RCX

    MOV RAX, Self
    MOV RDI, RAX

    ADD RAX, Pos
    MOV RAX, [RAX] // RAX := Pos

    ADD RDI, FCurrentBlock
    MOV RDI, [RDI]
    ADD RDI, RAX
    DEC RDI // RDI := PChar(Text) + Pos {Pos is counted from 1};

    MOV RCX, i
    MOV AL, RightQuote
    REPNE   SCASB
    MOV i, RCX

    POP RCX
    POP RDI
    POP RAX
{$ELSE}
    PUSH EAX
    PUSH EDI
    PUSH ECX

    MOV EAX, Self
    MOV EDI, EAX

    ADD EAX, Pos
    MOV EAX, [EAX] // EAX := Pos

    ADD EDI, FCurrentBlock
    MOV EDI, [EDI]
    ADD EDI, EAX
    DEC EDI // EDI := PChar(Text) + Pos {Pos is counted from 1};

    MOV ECX, i
    MOV AL, RightQuote
    REPNE   SCASB
    MOV i, ECX

    POP ECX
    POP EDI
    POP EAX
{$ENDIF}
  end;
  Pos := TextLength - Integer(i) + 1;
end;
{$ENDIF}

function TParser.IsIdentQuote(Ch: char): boolean;
begin
  Result := False;
end;

function TParser.IsInlineComment(Ch: char; Pos: integer): boolean;
begin
  Result := (Ch = '/') and (TextLength > Pos) and (Text[Pos + 1] = '/');
end;

function TParser.FindLexemIndex(const LexemPos, LexemLength: integer; Lexems: TLexemList): integer;
var
  L, H, I, C, K: integer;
  S: string;
  Lexem: string;
  Index: TList;
  Ch: Char;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[9].Start;
{$ENDIF}

  // Lexems.Indexes[0] must always return nil !!!
  Index := Lexems.Indexes[LexemLength];
  if Index = nil then begin
    Result := -1;
    Exit;
  end;

  SetLength(Lexem, LexemLength);
  K := LexemPos;
  for I := 1 to LexemLength do begin
    Ch := Text[K];
    if CharInSet(Ch, ['a'..'z']) then
      Dec(Ch, 32);
    Lexem[I] := Ch;
    Inc(K);
  end;

  L := 0;
  H := Index.Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    Result := NativeInt(Index[I]);
    S := Lexems[Result];

    K := 1;
    repeat
      C := Integer(S[K]) - Integer(Lexem[K]);
      if C <> 0 then
        Break;
      Inc(K);
    until K > LexemLength;

    if C < 0 then
      L := I + 1
    else if C > 0 then
      H := I - 1
    else
      Exit;
  end;

  Result := -1;

{$IFDEF PERF_COUNTER}
  PerfCounters[9].Stop;
{$ENDIF}
end;

procedure TParser.ToBegin;
begin
  Pos := 1;
  OldPos := 1;
  OldOldPos := 1;
  FCurrLine := 0;
  FPrevLine := 0;
  FCurrBegLine := 1;
  FPrevBegLine := 1;

  FBlockOffset := 1;
  Offset := 0;
  if FStream <> nil then
    FBlockSize := 0;

  FToken := lcBlank;
  FLexem := '';
end;

procedure TParser.Back;
begin
  Pos := OldPos;
  FCurrLine := FPrevLine;
  FCurrBegLine := FPrevBegLine;
end;

function TParser.InternalGetNext: integer;

  function IsCommentBegin(const Ch: char): boolean;
  var
    i, l: integer;
  begin
    if Ch <> FCommentBegin[1] then begin
      Result := False;
      Exit;
    end;

    l := Length(FCommentBegin);
    Result := Pos <= TextLength - l + 1;
    if not Result then
      Exit;

    for i := 2 to l do
      if Text[Pos + i - 1] <> FCommentBegin[i] then begin
        Result := False;
        Exit;
      end;
  end;

var
  j, CommentLen: integer;
  IsComment: boolean;
  Quote: char;
  c: char;
  DL: boolean;
  i: integer;
  St: integer;
  SymbolPos: integer;
  QuoteDelimiter: char;
  QuoteDelimiterFound: boolean;
  DollarTag, Tag: string;
  DollarPos: integer;
begin
  DL := FDesiredLexem <> '';
  OldOldPos := OldPos + Offset;
  OldPos := Pos;
  FPrevLine := FCurrLine;
  FPrevBegLine := FCurrBegLine;

  if FStream <> nil then
    DecreaseBlockParameters;

  FSavedPos := -1;
  FLexemPos := -1;
  FLexemLength := 0;

  repeat
    if Pos > TextLength then begin
      Result := lcEnd;
      Exit;
      c := #0; // To Prevent compiler warning
    end
    else
    if FStream = nil then // Performance optimization
      c := FCurrentBlock[Pos]
    else
      c := Text[Pos];

    // Blanks
    FLexemPos := -1;
    FLexemLength := 0;
    FSavedPos := Pos;

    // Skip blanks
    while true do begin
      case c of
        #32: begin
          Inc(Pos);
          c := Text[Pos];
        end;
        #13: begin
          Inc(Pos);
          c := Text[Pos];

          if c = #10 then begin
            Inc(Pos);
            c := Text[Pos];
          end;

          Inc(FCurrLine);
          FCurrBegLine := Pos;
        end;
        #10: begin
          Inc(Pos);
          c := Text[Pos];

          Inc(FCurrLine);
          FCurrBegLine := Pos;
        end;
        #9: begin
          Inc(Pos);
          c := Text[Pos];
        end;
        else
          break;
      end;
    end;

    if Pos > FSavedPos then
      AddToLexemArray(FSavedPos, Pos - FSavedPos)
    else
      FSavedPos := -1;

    if not OmitBlank and (FLexemLength <> 0) then begin
      Result := lcBlank;
      Exit;
    end;

    // End
    if Pos > TextLength then begin
      Result := lcEnd;
      Exit;
    end;

    // Comment
    FLexemPos := -1;
    FLexemLength := 0;
    IsComment := False;
    if not OmitInlineComment and IsInlineComment(c, Pos) then begin
      IsComment := True;
      FSavedPos := Pos;
      while (Pos <= TextLength) and (c <> #13) and (c <> #10) do begin
        Inc(Pos);
        c := Text[Pos];
      end;
      // optimize expression StrLexem := StrLexem + Copy(Buf, 1, Pos - Buf);
      AddToLexemArray(FSavedPos, Pos - FSavedPos);
    end;

    if IsCommentBegin(c) then begin
      IsComment := True;
      if Pos <= TextLength then begin
        FSavedPos := Pos;
        while Pos <= TextLength do begin
          case c of
            #13: begin
              Inc(Pos);
              c := Text[Pos];

              if c = #10 then begin
                Inc(Pos);
                c := Text[Pos];
              end;

              Inc(FCurrLine);
              FCurrBegLine := Pos;
            end;
            #10: begin
              Inc(Pos);
              c := Text[Pos];

              Inc(FCurrLine);
              FCurrBegLine := Pos;
            end;
            else begin
              j := 0;
              CommentLen := Length(FCommentEnd);
              while (j < CommentLen) and (Text[Pos + j] = FCommentEnd[j + 1]) do
                inc(j);
              if j = CommentLen then begin
                Inc(Pos, CommentLen);
                Break;
              end;

              Inc(Pos);
              c := Text[Pos];
            end;
          end;
        end;
        // optimize expression StrLexem := StrLexem + Copy(Buf, 1, Pos - Buf);
        AddToLexemArray(FSavedPos, Pos - FSavedPos);
      end;
    end;

    if not OmitComment and IsComment then begin
      Result := lcComment;
      Exit;
    end;
  until not IsComment;

  FLexemPos := -1;
  FLexemLength := 0;
  if IsAlpha(c) then begin
    Result := lcIdent;
    FSavedPos := Pos;
    QuoteDelimiter := c;
    repeat
      if DL then begin
        if CharInSet(c, ['a'..'z']) then
          Dec(c, 32);
        DL := (Pos - FSavedPos + 1 <= Length(FDesiredLexem)) and (FDesiredLexem[Pos - FSavedPos + 1] = c);
      end;

      Inc(Pos);
      c := Text[Pos];
    until not IsAlpha(c) and not IsNumber(c) and (c <> '$');

    if FAlternativeQuoting and
       (Pos - FSavedPos = 1) and
       ((QuoteDelimiter = 'q') or (QuoteDelimiter = 'Q')) and
       IsStringQuote(c)
    then begin
      Result := lcString;
      Quote := c;
      Inc(Pos);
      if Pos <= TextLength then begin
        QuoteDelimiter := Text[Pos];
        case QuoteDelimiter of
          '[': QuoteDelimiter := ']';
          '{': QuoteDelimiter := '}';
          '<': QuoteDelimiter := '>';
          '(': QuoteDelimiter := ')';
        end;
        QuoteDelimiterFound := False;
        Inc(Pos);
        while Pos <= TextLength do begin
          c := Text[Pos];
          Inc(Pos);

          case c of
            #13: begin
              Inc(Pos);
              c := Text[Pos];

              if c = #10 then begin
                Inc(Pos);
                c := Text[Pos];
              end;

              Inc(FCurrLine);
              FCurrBegLine := Pos;
            end;
            #10: begin
              Inc(Pos);
              c := Text[Pos];

              Inc(FCurrLine);
              FCurrBegLine := Pos;
            end;
          end;

          if QuoteDelimiterFound and (c = Quote) then
            break;

          QuoteDelimiterFound := (c = QuoteDelimiter);
        end;
      end;
      if FQuotedString then
        AddToLexemArray(FSavedPos, Pos - FSavedPos)
      else
        if Pos - FSavedPos > 5 then
          AddToLexemArray(FSavedPos + 3, Pos - FSavedPos - 5)
    end
    else begin
      FDesiredLexemFound := DL;
      AddToLexemArray(FSavedPos, Pos - FSavedPos);
    end;
  end
  else if IsNumber(c) then begin
    Result := lcNumber;
    SymbolPos := Pos;
    FSavedPos := Pos;
    repeat
      if DL then
        DL := (Pos - SymbolPos + 1 <= Length(FDesiredLexem)) and (FDesiredLexem[Pos - SymbolPos + 1] = c);

      Inc(Pos);
      c := Text[Pos];
    until not IsNumber(c);
    AddToLexemArray(FSavedPos, Pos - FSavedPos);

    FDesiredLexemFound := DL;
    if (c = FDecSeparator) and (Pos + 1 <= TextLength) and IsNumber(Text[Pos + 1]) then begin
      FSavedPos := Pos;
      Inc(Pos);
      c := Text[Pos];
      while IsNumber(c) do begin
        Inc(Pos);
        c := Text[Pos];
      end;
      AddToLexemArray(FSavedPos, Pos - FSavedPos);
    end;
  end
  else if IsSymbol(c) then begin
    if IsStringQuote(c) then begin
      Result := lcString;
      Quote := c;
      if FQuotedString then
        AddToLexemArray(Pos, 1);
      Inc(Pos);
      FSavedPos := Pos;
      ToRightQuote(Quote);
      // optimize expression StrLexem := StrLexem + Copy(Buf, 1, Pos - Buf);
      if Pos > FSavedPos then
        AddToLexemArray(FSavedPos, Pos - FSavedPos);
      if Pos <= TextLength then begin
        if FQuotedString then
          AddToLexemArray(Pos, 1);
        Inc(Pos);
      end;
    end
    else
    if IsIdentQuote(c) then begin
      Result := lcIdent;
      Quote := c;
      AddToLexemArray(Pos, 1);
      Inc(Pos);
      FSavedPos := Pos;
      ToRightQuote(Quote);
      if Pos > FSavedPos then
        AddToLexemArray(FSavedPos, Pos - FSavedPos);
      if Pos <= TextLength then begin
        AddToLexemArray(Pos, 1);
        Inc(Pos);
      end;
    end
    else
    if FDollarQuoting and (c = '$') and
       (Pos < TextLength) and not IsNumber(Text[Pos + 1])
    then begin
      Result := lcString;
      if FQuotedString then
        AddToLexemArray(Pos, 1);
      Inc(Pos);
      FSavedPos := Pos;
      ToRightQuote('$');
      DollarTag := CopyText(FSavedPos, Pos - FSavedPos);
      if FQuotedString and (Pos > FSavedPos) then
        AddToLexemArray(FSavedPos, Pos - FSavedPos);
      if Pos <= TextLength then begin
        if FQuotedString then
          AddToLexemArray(Pos, 1);
        Inc(Pos);
        repeat
          FSavedPos := Pos;
          ToRightQuote('$');
          if Pos > FSavedPos then
            AddToLexemArray(FSavedPos, Pos - FSavedPos);
          DollarPos := Pos;
          if Pos <= TextLength then begin
            AddToLexemArray(Pos, 1);
            Inc(Pos);
            FSavedPos := Pos;
            ToRightQuote('$');
            Tag := CopyText(FSavedPos, Pos - FSavedPos);
            if Pos > FSavedPos then
              AddToLexemArray(FSavedPos, Pos - FSavedPos);
          end;
        until (Tag = DollarTag) or (Pos > TextLength);
        if Pos <= TextLength then begin
          if FQuotedString then
            AddToLexemArray(Pos, 1)
          else
            Dec(FLexemLength, Pos - DollarPos);
          Inc(Pos);
        end;
      end;
    end
    else begin
      Result := lcSymbol;
      SymbolPos := Pos + 1;   // WAR
      FLexemPos := Pos;
      FLexemLength := 0;
      St := Pos;

      repeat
        AddToLexemArray(Pos, 1);
        Inc(Pos);
        c := Text[Pos];
        // Find
        i := FindLexemIndex(FLexemPos, FLexemLength, FSymbolLexems);
        if i <> - 1 then begin
          FLexem := FSymbolLexems[i];

          SymbolPos := Pos;
          Result := FSymbolLexems.Values[i];
          Assert(Result > 0);
        end;
      until (FLexemLength = FSymbolLexems.MaxLength) or not IsSymbol(c);

      if Result = lcSymbol then begin
        FLexemPos := St;
        FLexemLength := 1;
        if DL then begin
          // Assert(Length(FDesiredLexem) <= 1);
          if Length(FDesiredLexem) = 1 then
            FDesiredLexemFound := (FDesiredLexem[1] = Text[FLexemPos]);
        end;
      end
      else
      if DL then
        FDesiredLexemFound := (FDesiredLexem = FLexem);

      Pos := SymbolPos;
    end;
  end
  else
    raise Exception.Create('Parser: The unknown symbol $' + IntToHex(Word(Text[Pos]), sizeof(char) * 2) + '  ''' + Text[Pos] + '''');

  Assert(Result <> - MaxInt);
end;

function TParser.GetNext(out Lexem: string): integer;
begin
  Result := GetNextToken;
  Lexem := FLexem;
end;

function TParser.GetNextToken: integer;
var
  i: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[2].Start;//}
  try
{$ENDIF}

  FToken := InternalGetNext; // FToken is not in KeywordLexems
  if FToken <= 0 then
    if FLexemLength = 0 then
      FLexem := ''
    else begin
      Assert(FLexemPos > 0);
      FLexem := CopyText(FLexemPos, FLexemLength);
    end
  else
    Assert(FLexem <> '');

  if not FOmitKeywords and (FToken = lcIdent) then begin
    Assert(FLexemPos > 0);
    Assert(FLexemLength > 0);
    i := FindLexemIndex(FLexemPos, FLexemLength, FKeywordLexems);
    if i <> -1 then begin
      FToken := FKeywordLexems.Values[i];
      Assert(FToken > 0);
      if Uppered then
        FLexem := AnsiUpperCase(FLexem);  //WAR problem with macros as key words
    end;
  end;

  Assert(FToken <> - MaxInt);

  Result := FToken;

{$IFDEF PERF_COUNTER}
  finally
    PerfCounters[2].Stop;
  end;
{$ENDIF}
end;

function TParser.ToLexem(const Code: integer; const SkipSubQueries: boolean = False): integer;
var
  BracketCount: integer;
begin
  if SkipSubQueries then begin
    BracketCount := 0;
    repeat
      Result := GetNextToken;
      if Result = lxLeftBracket then
        Inc(BracketCount)
      else if Result = lxRightBracket then
        Dec(BracketCount);
    until ((Result = Code) and (BracketCount = 0)) or (Result = lcEnd);
  end
  else
    repeat
      Result := GetNextToken;
    until (Result = Code) or (Result = lcEnd);
end;

function TParser.ToLexem(const Codes: array of integer; const IsNestedQuery: boolean = False): integer;
var
  BracketCount: integer;
  i: integer;
begin
  BracketCount := 0;
  repeat
    Result := GetNextToken;
    if Result = lxLeftBracket then
      Inc(BracketCount)
    else if Result = lxRightBracket then begin
      Dec(BracketCount);
      if IsNestedQuery and (BracketCount < 0) then
        Break;
    end
    else if BracketCount = 0 then
      for i := Low(Codes) to High(Codes) do
        if Result = Codes[i] then
          Exit;
  until Result = lcEnd;
end;

function TParser.ToLexem(const Lexem: string): boolean;
begin
  try
    FDesiredLexem := UpperCase(Lexem);
    FDesiredLexemFound := False;

    while (InternalGetNext <> lcEnd) and not FDesiredLexemFound do;

    Result := FDesiredLexemFound;
  finally
    FDesiredLexem := '';
    FDesiredLexemFound := False;
  end;
end;

function TParser.IsSymbolCode(Code: integer): boolean;
begin
  Result := (Code > 0) and (Code <= lxMaxSymbolValue);
end;

function TParser.GetSymbolByCode(Code: integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to SymbolLexems.Count - 1 do
    if Code = SymbolLexems.Values[i] then begin
      Result := SymbolLexems[i];
      exit;
    end;
end;

function TParser.CurrPos: Int64;
begin
  Result := {$IFDEF FPC}Int64{$ENDIF}(Pos) - 1 + Offset;
end;

function TParser.PrevPos: Int64;
begin
  Result := {$IFDEF FPC}Int64{$ENDIF}(OldPos) - 1 + Offset;
end;

function TParser.PrevPrevPos: Int64;
begin
  Result := OldOldPos - 1;
end;

function TParser.CurrLine: Int64;
begin
  Result := FCurrLine;
end;

function TParser.PrevLine: Int64;
begin
  Result := FPrevLine;
end;

function TParser.CurrCol: Int64;
begin
  Result := {$IFDEF FPC}Int64{$ENDIF}(Pos) - FCurrBegLine;
end;

function TParser.PrevCol: Int64;
begin
  Result := {$IFDEF FPC}Int64{$ENDIF}(OldPos) - FPrevBegLine;
end;

function TParser.GetCharSize(ByteSize: Int64): Int64;
begin
  if FEncoding = Encoding.Unicode then
    Result := ByteSize shr 1
  else
    Result := ByteSize;
end;

procedure TParser.ReadNextBlock;
var
  ReadSize, Size, BrokenLen: integer;
begin
  if FBlockSize > 0 then begin
    Inc(FBlockOffset, FBlockSize);
    FStoredBlocks.Add(FCurrentBlock, FBlockSize);
  end;

  if FEncoding = Encoding.{$IFDEF IS_UNICODE}Unicode{$ELSE}Default{$ENDIF} then begin
    SetLength(FCurrentBlock, BLOCK_SIZE div sizeof(char));

    if FStreamSize >= BLOCK_SIZE then
      ReadSize := BLOCK_SIZE
    else
      ReadSize := FStreamSize;
    Size := FStream.Read(FCurrentBlock[1], ReadSize);
    FBlockSize := Size div sizeof(char);

    SetLength(FCurrentBlock, FBlockSize);
  end
  else begin
    SetLength(FTmpBuffer, BLOCK_SIZE);

    if FStreamSize >= BLOCK_SIZE then
      ReadSize := BLOCK_SIZE
    else
      ReadSize := FStreamSize;
    Size := FStream.Read(FTmpBuffer[0], ReadSize);

    if FEncoding = Encoding.UTF8 then begin
      BrokenLen := DetectUtf8LastBrockenChar(@FTmpBuffer[0], Size);
      if BrokenLen > 0 then begin
        Dec(Size, BrokenLen);
        FStream.Position := FStream.Position - BrokenLen;
      end;
    end;

    FCurrentBlock := FEncoding.GetString(FTmpBuffer, 0, Size);
    FBlockSize := Length(FCurrentBlock);

    if (Size <> FBlockSize) and (FEncoding <> Encoding.Unicode) then begin // Variable char length included
      StreamLength := StreamLength - ({$IFDEF FPC}Int64{$ENDIF}(Size) - FBlockSize);
      TextLength := TextLength - (Size - FBlockSize);
    end;
  end;
end;

function TParser.GetStoredChar(Index: integer): char;
var
  Len, StoredBlockPos: integer;
  i: Integer;
begin
  StoredBlockPos := 0;
  for i := 0 to FStoredBlocks.Count - 1 do begin
    Len := FStoredBlocks.Values[i];
    if Index <= StoredBlockPos + Len then begin
      Result := FStoredBlocks[i][Index - StoredBlockPos];
      Exit;
    end;
    Inc(StoredBlockPos, Len);
  end;

  Result := #0;
  Assert(False);
end;

function TParser.GetChar(Index: integer): char;
begin
  if Index > TextLength then
    Result := #0
  else
  if FStream <> nil then begin // Performance optimization
    if Index >= FBlockOffset + FBlockSize then
      ReadNextBlock;

    if Index >= FBlockOffset then
      Result := FCurrentBlock[Index - FBlockOffset + 1]
    else
      Result := GetStoredChar(Index);
  end
  else
    Result := FCurrentBlock[Index];
end;

procedure TParser.DecreaseBlockParameters;
var
  StoredBlockPos: integer;
begin
  if OldPos > -1 then begin
    StoredBlockPos := 0;
    while FStoredBlocks.Count > 0 do begin
      Inc(StoredBlockPos, FStoredBlocks.Values[0]);
      if OldPos > StoredBlockPos then
        FStoredBlocks.Delete(0)
      else begin
        Dec(StoredBlockPos, FStoredBlocks.Values[0]);
        break;
      end;
    end;

    if StoredBlockPos > 0 then begin
      FPrevBegLine := FPrevBegLine - StoredBlockPos;
      FCurrBegLine := FCurrBegLine - StoredBlockPos;
      OldPos := OldPos - StoredBlockPos;
      Pos := Pos - StoredBlockPos;
      FBlockOffset := FBlockOffset - StoredBlockPos;
      Offset := Offset + StoredBlockPos;

      if StreamLength - Offset > MaxInt then
        TextLength := MaxInt
      else
        TextLength := StreamLength - Offset;
    end;
  end;
end;

procedure TParser.AddToLexemArray(Index: integer; const Len: integer);
begin
  FSavedPos := -1;
  if FLexemPos = -1 then
    FLexemPos := Index;
  Inc(FLexemLength, Len);
end;

function TParser.CopyText(Pos, Count: integer): string;
var
  sb: StringBuilder;
  Len, StoredBlockPos: integer;
  i: integer;
begin
  if (FStream <> nil) and (Pos < FBlockOffset) then begin
    sb := StringBuilder.Create(Count);
    try
      StoredBlockPos := 0;
      i := 0;
      while i < FStoredBlocks.Count do begin
        Len := FStoredBlocks.Values[i];
        Inc(StoredBlockPos, Len);
        if Pos <= StoredBlockPos then begin
          sb.Append(Copy(FStoredBlocks[i], Pos - (StoredBlockPos - Len), Count));
          Dec(Count, StoredBlockPos - Pos + 1);
          Inc(i);
          break;
        end;
        Inc(i);
      end;

      while (Count > 0) and (i < FStoredBlocks.Count) do begin
        Len := FStoredBlocks.Values[i];
        if Count >= Len then
          sb.Append(FStoredBlocks[i])
        else
          sb.Append(Copy(FStoredBlocks[i], 1, Count));
        Dec(Count, Len);
        Inc(i);
      end;

      if Count > 0 then
        sb.Append(Copy(FCurrentBlock, 1, Count));

      Result := sb.ToString;
    finally
      sb.Free;
    end;
  end
  else
    Result := Copy(FCurrentBlock, Pos - FBlockOffset + 1, Count);
end;

{ TSQLParser }

procedure TSQLParser.InitParser;
begin
  inherited;

  FKeywordLexems := SQLKeywordLexems;
  FSymbolLexems := SQLSymbolLexems;
  FClauses := SQLClauses;

  FDecSeparator := '.'; // To avoid 'INSERT INTO t(f1, f2) VALUES(:1,2)'
end;

function TSQLParser.IsAlpha(Ch: char): boolean;
begin
  case Ord(Ch) of
    Ord('a')..Ord('z'):
      Result := True;
    Ord('A')..Ord('Z'):
      Result := True;
    Ord('_'):
      Result := True;
    Ord('$'):
      Result := True;
    Ord('#'):
      Result := True;
    Ord(#128)..Ord(High(char)):
      Result := True;
    else
      Result := False;
  end;
end;

function TSQLParser.IsStringQuote(Ch: char): boolean;
begin
  case Ch of
    '''':
      Result := True;
    else
      Result := False;
  end;
end;

function TSQLParser.IsIdentQuote(Ch: char): boolean;
begin
  case Ch of
    '"':
      Result := True;
    else
      Result := False;
  end;
end;

function TSQLParser.IsInlineComment(Ch: char; Pos: integer): boolean;
begin
  Result := (Ch = '-') and (TextLength >= Pos + 1) and (Text[Pos + 1] = '-');
end;

function TSQLParser.IsClauseLexem(Code: integer): boolean;
begin
  Result := FClauses.IndexOf(Code) >= 0;
end;

//function TSQLParser.PosClauseLexem(Code: integer): integer;
//var
//  i: integer;
//begin
//  Result := FClauses.IndexOf(Code);
//end;

function TSQLParser.CompareClauseLexems(const Code1, Code2: integer): integer;
var
  Pos1, Pos2: Integer;
begin
  Pos1 := FClauses.IndexOf(Code1);
  Pos2 := FClauses.IndexOf(Code2);

  if (Pos1 = -1) or (Pos2 = -1) then
    Result := -2
  else
  if Pos1 > Pos2 then
    Result := 1
  else
  if Pos1 < Pos2 then
    Result := -1
  else
    Result := 0;
end;

function TSQLParser.IsMacroAllowed(Code: integer): boolean;
begin
  Result := Code <> lcString;
end;

function TSQLParser.IsSelectModifier(Code: integer): boolean;
begin
  case Code of
    lxALL, lxDISTINCT:
      Result := True;
  else
    Result := False;
  end;
end;

class function TSQLParser.IsNumericMacroNameAllowed: boolean;
begin
  Result := False;
end;

class function TSQLParser.IsFunctionOrConst(const UpperedName: string): boolean;
begin
  if UpperedName = 'NULL' then
    Result := True
  else
    Result := False;
end;

class function TSQLParser.IsQuasiColumn(const UpperedName: string): boolean;
begin
  Result := False;
end;

{ Functions }

function _GetFromOrWhere(
  const SQL: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean;
  NeedCode: integer; // From or Where code
  const MacroChar: string
): string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  StLex: string;
  Code, BracketCount: integer;
  IsMacro: boolean;
begin
  Result := '';
  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := OmitComment;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      FirstPos := 0;
      BracketCount := 0;
      IsMacro := False;
      repeat
        Code := Parser.GetNext(StLex);
        if (Code = NeedCode) and (not IsMacro) and (BracketCount = 0) then
          FirstPos := Parser.CurrPos + 2
        else if Code = lxLeftBracket then
          Inc(BracketCount)
        else if Code = lxRightBracket then
          Dec(BracketCount);

        IsMacro := (Code <> lcString) and (StLex = MacroChar);
        if (BracketCount = 0) and Parser.IsClauseLexem(Code) and (Code <> NeedCode) then begin
          Parser.Back;
          Break;
        end;
      until Code = lcEnd;

      if FirstPos > 0 then
        Result := Copy(SQL, FirstPos, Parser.CurrPos - FirstPos + 1);
    end;
  finally
    Parser.Free;
  end;
end;

function _GetFrom(
  const SQL: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean;
  const MacroChar: string
): string;
begin
  Result := _GetFromOrWhere(SQL, ParserClass, OmitComment, lxFROM, MacroChar);
end;

function _GetWhere(
  const SQL: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean;
  const MacroChar: string
): string;
begin
  Result := _GetFromOrWhere(SQL, ParserClass, OmitComment, lxWHERE, MacroChar);
end;

function _GetOrderBy(
  const SQL: string;
  ParserClass: TSQLParserClass
): string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  Code, BracketCount: integer;
begin
  Result := '';
  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      Code := Parser.ToLexem(lxORDER, True);
      if Code = lxORDER then
        if Parser.GetNextToken = lxBY then begin
          FirstPos := Parser.CurrPos + 2;
          BracketCount := 0;
          repeat
            Code := Parser.GetNextToken;
            if Code = lxLeftBracket then
              Inc(BracketCount)
            else if Code = lxRightBracket then
              Dec(BracketCount)
            else if Parser.IsClauseLexem(Code) and (BracketCount = 0) then begin
              Parser.Back;
              Break;
            end;
          until Code = lcEnd;

          Result := Trim(Copy(SQL, FirstPos, Parser.CurrPos - FirstPos + 1));
        end;
    end;
  finally
    Parser.Free;
  end;
end;

procedure _FindWherePosition(
  const SQL: string; var Condition: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean;
  const MacroChar: string;
  out StartPos, EndPos: integer
);
var
  Parser: TSQLParser;
  HasFrom: boolean;
  HasWhere: boolean;
  Code: integer;
  PrevCode: integer;
  StLex: string;
  BracketCount: integer;
  WherePos: integer;
  IsMacro: boolean;
begin
  Parser := ParserClass.Create(SQL);
  Parser.OmitComment := OmitComment;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      IsMacro := False;
      HasFrom := False;
      HasWhere := False;
      WherePos := 0;
      Code := 0;
      BracketCount := 0;
      repeat
        PrevCode := Code;
        Code := Parser.GetNext(StLex);
        if (Code = lxWHERE) and (not IsMacro) and (BracketCount = 0) then begin
          HasWhere := True;
          WherePos := Parser.CurrPos + 2;
        end
        else if (Code = lxFROM) and (not IsMacro) and (BracketCount = 0) then
          HasFrom := True
        else if Code = lxLeftBracket then
          Inc(BracketCount)
        else if Code = lxRightBracket then
          Dec(BracketCount);

        IsMacro := (Code <> lcString) and (StLex = MacroChar);
        if HasFrom and (BracketCount = 0) and Parser.IsClauseLexem(Code) and (Code <> lxWHERE) then
          Break;
      until Code = lcEnd;

      if (Code = lcEnd) and (PrevCode = lxSemicolon) then
        EndPos := Parser.PrevPrevPos
      else
        EndPos := Parser.PrevPos;

      if HasWhere then begin
        StartPos := WherePos;
        if PrevCode = lcComment then
          Condition := SLLineSeparator + ') AND ' + Condition + ' '
        else
          if StartPos > EndPos then begin // WHERE is empty
            Condition := ' ' + Condition;
            StartPos := EndPos + 1;
            end
          else
            Condition := ') AND ' + Condition + ' ';
      end
      else begin
        Condition := SLLineSeparator + 'WHERE ' + Condition + ' ';
        StartPos := EndPos + 1;
      end;
    end
    else begin
      StartPos := -1;
      EndPos := -1
    end;
  finally
    Parser.Free;
  end;
end;

function _AddWhere(
  const SQL: string; Condition: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean;
  const MacroChar: string
): string;
var
  StartPos, EndPos: integer;
begin
  Result := SQL;

  if Trim(Condition) = '' then
    Exit;

  _FindWherePosition(SQL, Condition, ParserClass, OmitComment, MacroChar, StartPos, EndPos);

  if StartPos <> -1 then begin
    Insert(Condition, Result, EndPos + 1);
    if StartPos <= EndPos then
      Insert('(', Result, StartPos);
  end;
end;

function _SetWhere(
  const SQL: string; Condition: string;
  ParserClass: TSQLParserClass;
  OmitComment: boolean
): string;
var
  Parser: TSQLParser;
  FirstPos: integer;
  LastPos: integer;
  Code, PrevCode: integer;
  StLex: string;
  BracketCount: integer;
begin
  Result := SQL;
  Parser := ParserClass.Create(SQL);
  Parser.OmitBlank := False;
  Parser.OmitComment := OmitComment;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      FirstPos := 0;
      LastPos := 0;
      BracketCount := 0;
      PrevCode := lcEnd;
      Code := Parser.GetNext(StLex);
      repeat
        if Code = lcBlank then begin
          if LastPos = 0 then
            LastPos := Parser.PrevPos;
        end
        else begin
          LastPos := 0;

          if Code = lxWHERE then begin
            if BracketCount = 0 then begin
              if Condition = '' then begin
                if PrevCode = lcBlank then
                  FirstPos := Parser.PrevPrevPos + 1
                else
                  FirstPos := Parser.PrevPos + 1;
              end
              else begin
                Parser.GetNext(StLex); // blank
                FirstPos := Parser.CurrPos + 1;
              end;
            end;
          end
          else if Code = lxLeftBracket then
            Inc(BracketCount)
          else if Code = lxRightBracket then
            Dec(BracketCount);
        end;

        PrevCode := Code;
        Code := Parser.GetNext(StLex);
      until (Code = lcEnd) or (Parser.IsClauseLexem(Code) and (Code <> lxWHERE)) and (BracketCount = 0);

      if LastPos = 0 then
        LastPos := Length(Result);

      if FirstPos > 0 then
        Delete(Result, FirstPos, LastPos - FirstPos + 1);

      if Condition <> '' then begin
        if FirstPos = 0 then begin
          FirstPos := LastPos + 1;
          Condition := ' WHERE ' + Condition;
          if Pos(#13, Copy(Result, 1, FirstPos)) > 0 then
            Condition := SLLineSeparator + ' ' + Condition;
        end;
        Insert(Condition, Result, FirstPos);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

function _SetOrderBy(
  const SQL: string; Fields: string;
  ParserClass: TSQLParserClass
): string;
var
  i: Integer;
  Parser: TSQLParser;
  FirstPos: integer;
  LastPos: integer;
  Code, PrevCode: integer;
  StLex: string;
  BracketCount: integer;
  InCase: boolean;
begin
  for i := 1 to Length(Fields) do
    if Fields[i] = ';' then
      Fields[i] := ',';

  Result := SQL;
  Parser := ParserClass.Create(Result);
  Parser.OmitBlank := False;
  Parser.OmitComment := True;
  try
    Code := Parser.ToLexem([lxWITH, lxSELECT]);
    if Code = lxWITH then
      Code := Parser.ToLexem(lxSELECT, True);

    if Code <> lcEnd then begin
      FirstPos := 0;
      LastPos := 0;
      BracketCount := 0;
      PrevCode := lcEnd;
      Code := Parser.GetNext(StLex);
      repeat
        if Code = lcBlank then begin
          if LastPos = 0 then
            LastPos := Parser.PrevPos;
        end
        else begin
          LastPos := 0;

          if Code = lxORDER then begin
            if BracketCount = 0 then begin
              if Fields = '' then
                if PrevCode = lcBlank then
                  FirstPos := Parser.PrevPrevPos + 1
                else
                  FirstPos := Parser.PrevPos + 1;
              Parser.GetNext(StLex);  // blank
              if Parser.GetNext(StLex) = lxBY then begin
                if Fields <> '' then begin
                  Parser.GetNext(StLex); // blank
                  FirstPos := Parser.CurrPos + 1;
                end;
                InCase := False;
                while Code <> lcEnd do begin
                  PrevCode := Code;
                  Code := Parser.GetNext(StLex);
                  // To ommit SELECT and etc in brackets
                  case Code of
                    lxDESC:
                      continue;
                    lxCASE:
                      InCase := True;
                    lxEND:
                      if InCase then begin
                        InCase := False;
                        continue;
                      end;
                    lxLeftBracket:
                      Inc(BracketCount);
                    lxRightBracket:
                      Dec(BracketCount);
                    else
                      if not InCase and (BracketCount = 0) and (Code >= lxSQLFirst) then
                        break;
                  end;
                end;
                if Code <> lcEnd then
                  LastPos := Parser.PrevPos - 1;
                Break;
              end
              else
                FirstPos := 0;
            end;
          end
          else if Code = lxLeftBracket then
            Inc(BracketCount)
          else if (Code = lxRightBracket) and (BracketCount > 0) then
            Dec(BracketCount);
        end;

        PrevCode := Code;
        Code := Parser.GetNext(StLex);
      until (Code = lcEnd) or ((Parser.CompareClauseLexems(Code, lxORDER) > 0) and (BracketCount = 0));

      if (Code = lcEnd) and (PrevCode = lxSemicolon) then
        LastPos := Parser.PrevPrevPos;

      if LastPos = 0 then
        LastPos := Length(Result);

      if FirstPos > 0 then
        Delete(Result, FirstPos, LastPos - FirstPos + 1);

      if Fields <> '' then begin
        if FirstPos = 0 then begin
          FirstPos := LastPos + 1;
          Fields := ' ORDER BY ' + Fields;
          if Pos(#13, Copy(Result, 1, FirstPos)) > 0 then
            Fields := SLLineSeparator + ' ' + Fields;
        end;
        Insert(Fields, Result, FirstPos);
      end;
    end;
  finally
    Parser.Free;
  end;
end;

initialization
  CommonSymbolLexems := TLexemList.Create;

  CommonSymbolLexems.Add('!', lxExclamation);
  CommonSymbolLexems.Add('"', lxDoubleQuotes);
  CommonSymbolLexems.Add('#', lxOctothorp);
  CommonSymbolLexems.Add('$', lxDollar);
  CommonSymbolLexems.Add('%', lxPercent);
  CommonSymbolLexems.Add('&', lxAmp);
  CommonSymbolLexems.Add('''', lxQuote);
  CommonSymbolLexems.Add('(', lxLeftBracket);
  CommonSymbolLexems.Add(')', lxRightBracket);
  CommonSymbolLexems.Add('*', lxAsterisk);
  CommonSymbolLexems.Add('+', lxPlus);
  CommonSymbolLexems.Add(',', lxComma);
  CommonSymbolLexems.Add('-', lxDash);
  CommonSymbolLexems.Add('.', lxPoint);
  CommonSymbolLexems.Add('/', lxSlash);
  CommonSymbolLexems.Add(':', lxColon);
  CommonSymbolLexems.Add(';', lxSemicolon);
  CommonSymbolLexems.Add('<', lxLess);
  CommonSymbolLexems.Add('=', lxEqual);
  CommonSymbolLexems.Add('>', lxMore);
  CommonSymbolLexems.Add('?', lxQuestion);
  CommonSymbolLexems.Add('@', lxAt);
  CommonSymbolLexems.Add('[', lxLeftSqBracket);
  CommonSymbolLexems.Add('\', lxBackSlash);
  CommonSymbolLexems.Add(']', lxRightSqBracket);
  CommonSymbolLexems.Add('^', lxCircumflex);
  CommonSymbolLexems.Add('_', lxUnderline);
  CommonSymbolLexems.Add('`', lxGrave);
  CommonSymbolLexems.Sort;

  CommonKeywordLexems := TLexemList.Create;

  CommonKeywordLexems.Add('AND', 29);
  CommonKeywordLexems.Add('NOT', 30);
  CommonKeywordLexems.Add('OR', 31);
  CommonKeywordLexems.Sort;

  SQLSymbolLexems := TLexemList.Create;

  SQLSymbolLexems.Add('*', lxAsterisk);
  SQLSymbolLexems.Add(',', lxComma);
  SQLSymbolLexems.Add('.', lxPoint);
  SQLSymbolLexems.Add('/', lxSlash);
  SQLSymbolLexems.Add(':', lxColon);
  SQLSymbolLexems.Add(';', lxSemicolon);
  SQLSymbolLexems.Add('=', lxEqual);
  SQLSymbolLexems.Add('(', lxLeftBracket);
  SQLSymbolLexems.Add(')', lxRightBracket);
  SQLSymbolLexems.Add('@', lxAt);
  SQLSymbolLexems.Add('?', lxQuestion);
  SQLSymbolLexems.Sort;

  SQLKeywordLexems := TLexemList.Create;

  SQLKeywordLexems.Add('ALL',       lxALL      );
  SQLKeywordLexems.Add('AND',       lxAND      );
  SQLKeywordLexems.Add('AS',        lxAS       );
  SQLKeywordLexems.Add('BEGIN',     lxBEGIN    );
  SQLKeywordLexems.Add('BY',        lxBY       );
  SQLKeywordLexems.Add('CASE',      lxCASE     );
  SQLKeywordLexems.Add('COMMIT',    lxCOMMIT   );
  SQLKeywordLexems.Add('DELETE',    lxDELETE   );
  SQLKeywordLexems.Add('DESC',      lxDESC     );
  SQLKeywordLexems.Add('DISTINCT',  lxDISTINCT );
  SQLKeywordLexems.Add('ELSE',      lxELSE     );
  SQLKeywordLexems.Add('END',       lxEND      );
  SQLKeywordLexems.Add('EXECUTE',   lxEXECUTE  );
  SQLKeywordLexems.Add('FETCH',     lxFETCH    );
  SQLKeywordLexems.Add('FOR',       lxFOR      );
  SQLKeywordLexems.Add('FROM',      lxFROM     );
  SQLKeywordLexems.Add('FULL',      lxFULL     );
  SQLKeywordLexems.Add('GROUP',     lxGROUP    );
  SQLKeywordLexems.Add('HAVING',    lxHAVING   );
  SQLKeywordLexems.Add('INNER',     lxINNER    );
  SQLKeywordLexems.Add('INSERT',    lxINSERT   );
  SQLKeywordLexems.Add('INTO',      lxINTO     );
  SQLKeywordLexems.Add('IS',        lxIS       );
  SQLKeywordLexems.Add('JOIN',      lxJOIN     );
  SQLKeywordLexems.Add('LEFT',      lxLEFT     );
  SQLKeywordLexems.Add('LIMIT',     lxLIMIT    );
  SQLKeywordLexems.Add('LOCK',      lxLOCK     );
  SQLKeywordLexems.Add('MINUS',     lxMINUS    );
  SQLKeywordLexems.Add('NOT',       lxNOT      );
  SQLKeywordLexems.Add('OFFSET',    lxOFFSET   );
  SQLKeywordLexems.Add('ON',        lxON       );
  SQLKeywordLexems.Add('OR',        lxOR       );
  SQLKeywordLexems.Add('ORDER',     lxORDER    );
  SQLKeywordLexems.Add('OUTER',     lxOUTER    );
  SQLKeywordLexems.Add('RELEASE',   lxRELEASE  );
  SQLKeywordLexems.Add('RETURNING', lxRETURNING);
  SQLKeywordLexems.Add('RIGHT',     lxRIGHT    );
  SQLKeywordLexems.Add('ROLLBACK',  lxROLLBACK );
  SQLKeywordLexems.Add('SAVEPOINT', lxSAVEPOINT);
  SQLKeywordLexems.Add('SELECT',    lxSELECT   );
  SQLKeywordLexems.Add('SET',       lxSET      );
  SQLKeywordLexems.Add('THEN',      lxTHEN     );
  SQLKeywordLexems.Add('TO',        lxTO       );
  SQLKeywordLexems.Add('TRANSACTION', lxTRANSACTION);
  SQLKeywordLexems.Add('UNION',     lxUNION    );
  SQLKeywordLexems.Add('UPDATE',    lxUPDATE   );
  SQLKeywordLexems.Add('WHEN',      lxWHEN     );
  SQLKeywordLexems.Add('WHERE',     lxWHERE    );
  SQLKeywordLexems.Add('WITH',      lxWITH     );
  SQLKeywordLexems.Add('VALUES',    lxVALUES   );
  SQLKeywordLexems.Sort;

  SQLClauses := TClauseList.Create;
  SQLClauses.Add(lxWHERE);
  SQLClauses.Add(lxGROUP);
  SQLClauses.Add(lxHAVING);
  SQLClauses.Add(lxUNION);
  SQLClauses.Add(lxINTERSECT);
  SQLClauses.Add(lxMINUS);
  SQLClauses.Add(lxORDER);

{$IFDEF PERF_COUNTER}
  PerfCounters[2].Name := 'Parser.Next';
  PerfCounters[8].Name := 'Parser.Code';
  PerfCounters[9].Name := 'Parser.FindLexemIndex';
{$ENDIF}

finalization
  CommonSymbolLexems.Free;
  CommonKeywordLexems.Free;
  SQLSymbolLexems.Free;
  SQLKeywordLexems.Free;
  SQLClauses.Free;

end.
