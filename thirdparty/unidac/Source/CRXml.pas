
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRXml
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRXml;

interface

uses
  Classes, SysUtils,
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
  Generics.Collections,
{$ELSE}
  Contnrs,
{$ENDIF}
  CRTypes, CLRClasses;

type
  StreamWriter = class
  private
    FStream: TStream;
    FReleaseStream: Boolean;
    FEncoding: Encoding;
  public
    constructor Create(const path: string; Append: Boolean); overload;
    constructor Create(output: TStream; aEncoding: Encoding); overload;
    destructor Destroy; override;

    procedure Close;
    procedure Flush;
    procedure Write(const value: WideString);
    procedure WriteLine(const value: WideString);
  end;

  XmlException = class(Exception);

  XmlNodeType = (ntNone, ntStartElement, ntEndElement, ntClosedElement, ntAttribute, ntComment, ntDeclaration, ntDocumentType, ntText);
  XmlReadState = (Initial, Interactive, Error, EndOfFile, Closed);

  TBytesArray = array of TBytes;

  TAttribute = record
    Prefix: TBytes;
    Name: TBytes;
    Value: TBytes;
  end;
  TAttributeArray = array of TAttribute;

  XmlTextReader = class
  private
    FBlocks: TBytesArray;
    FStream: TStream;
    FTmpBlock: TBytes;
    FStreamPosition: int64;// When this class created through constructor Create(str: string), FStreamPosition = FFullSize
    FBlockSize: integer;
    FFullSize: int64;
    FMaxNumBlock: integer;
    FBlockCount: integer;
    FCurPosition: integer;
    FActualPosition: integer;
    FLastBlockSize: integer;
    FPrefix: TBytes;
    FValue: TBytes;
    FName: TBytes;
    FCurrElementName: TBytes;
    FNodeType: XmlNodeType;
    FAttributes: TAttributeArray;
    FAttributeCount: Integer;
    FState: XmlReadState;
    FDecodeHTMLEntities: boolean;

    function GetName: string;
    function GetFullName: string;
    function GetPrefix: string;
    function GetValue: string;
  {$IFNDEF NEXTGEN}
    function GetAnsiValue: AnsiString;
    function GetWideValue: WideString;
  {$ENDIF}
    function GetDepth: integer;
    function GetHasAttributes: boolean;
    function GetAttributeName(Index: Integer): string;
    function GetAttributePrefix(Index: Integer): string;
    function GetAttributeValue(Index: Integer): string;
    function GetAttributeCount: integer;
    procedure InitInstance;
    function GetEof: boolean;
    function LoadNextBlock(Count: integer): boolean;
    procedure FreeLastBlocks(Count: integer);
    function ReadTo(const SubStr: AnsiString; out ResultValue: TBytes; const AdvLenth: integer = 0): Boolean;
    function IsToken(const SubStr: AnsiString): Boolean;
    function GetNextSymbol: AnsiChar;
    function MoveTo(const Lexem: AnsiString): Boolean;
  protected
    function GetAttributeIndex(const Name: string): Integer;
    procedure ParseXMLNodeAttributes(const Node: TBytes);
  public
    constructor Create(Stream: TStream; DecodeHTMLEntities: boolean = True); overload;
    constructor Create(const Str: string {UTF8}; DecodeHTMLEntities: boolean = True); overload; // Parameter "Str" mast content only XML Text, Url for XML not supported
    constructor Create(const Binary: TBytes; DecodeHTMLEntities: boolean = True); overload;
    destructor Destroy; override;

    function Read: boolean;

    procedure MoveToAttribute(Index: integer); overload;
    function MoveToAttribute(const Name: string): Boolean; overload;
    function Items(const AttrIndex: Integer): string; overload; virtual;
    function Items(const AttrName: string): string; overload;

    property Name: string read GetName;
    property Prefix: string read GetPrefix;
    property FullName: string read GetFullName;
    property Value: string read GetValue;
  {$IFNDEF NEXTGEN}
    property AnsiValue: AnsiString read GetAnsiValue;
    property WideValue: WideString read GetWideValue;
  {$ENDIF}

    property NodeType: XmlNodeType read FNodeType;
    property Depth: integer read GetDepth;
    property ReadState: XmlReadState read FState;
    property Eof: boolean read GetEof;

    property HasAttributes: boolean read GetHasAttributes;
    property AttributeNames[Index: integer]: string read GetAttributeName;
    property AttributePrefixes[Index: integer]: string read GetAttributePrefix;
    property AttributeValues[Index: integer]: string read GetAttributeValue;
    property AttributeCount: integer read GetAttributeCount;

    property Blocks: TBytesArray read FBlocks;
  end;

  XmlFormatting = (fmtNone, fmtIndented);
  XmlWriteState = (wsAttribute, wsClosed, wsContent, wsElement, wsStart);

  XmlTextWriter = class
  private
    FText: WideStringBuilder;
    FFormatting: XmlFormatting;
    FIndentation: Integer;
    FIndentChar: WideChar;
    FQuoteChar: WideChar;
    FWriteState: XmlWriteState;
    FDepth: Integer;
    FPosStack: TStack{$IFDEF POSIX}<Pointer>{$ENDIF};
    FTagStack: TStringList;
    FWriter: StreamWriter;

    function IndentStr: WideString;
    function PopTagName: string;
    procedure PushTagName(const TagName: string);
    procedure InternalCloseStartTag;
  protected
    procedure InternalWriteStartElement(const Prefix, LocalName, ns: string);
    procedure InternalWriteElementString(const LocalName, ns: string; const Value: WideString);
    procedure InternalWriteAttributeString(const Prefix, LocalName, ns: string; const Value: WideString);
    procedure InternalWriteEndElement;
    procedure FlushData;
  public
    constructor Create(w: StreamWriter);
    destructor Destroy; override;

    procedure WriteStartElement(const LocalName: string); overload;
    procedure WriteStartElement(const Prefix, LocalName, ns: string); overload;
    procedure WriteStartElement(const LocalName, ns: string); overload;
    procedure WriteEndElement;
    procedure WriteFullEndElement;
    procedure WriteString(const Text: WideString); overload;
    procedure WriteElementString(const LocalName: string; const Value: WideString); overload;
    procedure WriteElementString(const LocalName, ns: string; const Value: WideString); overload;
    procedure WriteAttributeString(const LocalName: string; const Value: WideString); overload;
    procedure WriteAttributeString(const Prefix, LocalName, ns: string; const Value: WideString); overload;
  {$IFDEF FPC}
    procedure WriteString(const Text: AnsiString); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WriteElementString(const LocalName: string; const Value: AnsiString); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WriteElementString(const LocalName, ns: string; const Value: AnsiString); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WriteAttributeString(const LocalName: string; const Value: AnsiString); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure WriteAttributeString(const Prefix, LocalName, ns: string; const Value: AnsiString); overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
  {$ENDIF}

    procedure Close;

    property Formatting: XmlFormatting read FFormatting write FFormatting;
    property Indentation: Integer read FIndentation write FIndentation;
    property IndentChar: WideChar read FIndentChar write FIndentChar;
    property QuoteChar: WideChar read FQuoteChar write FQuoteChar;
    property WriteState: XmlWriteState read FWriteState;
  end;

  function XMLDecode(const Value: TBytes; DecodeHTMLEntities: boolean = True): TBytes;

implementation

uses
  Math,
  CRFunctions, CRParser, MemUtils;

const
  XMLLineSeparator = #13#10;
  SInvalidXML = 'Invalid XML';

type
  TXmlSequence = record
    Sequence: string;
    Symbol: Char;
  end;

const
  XmlSequences: array[0..13] of TXmlSequence = (
    (Sequence: '&#x27;'; Symbol: ''''),
    (Sequence: '&#x39;'; Symbol: ''''),
    (Sequence: '&#x92;'; Symbol: ''''),
    (Sequence: '&#x96;'; Symbol: ''''),
    (Sequence: '&#x22;'; Symbol: '"'),
    (Sequence: '&#x3c;'; Symbol: '<'),
    (Sequence: '&#x3e;'; Symbol: '>'),
    (Sequence: '&#x26;'; Symbol: '&'),
    (Sequence: '&#13;'; Symbol: #13),
    (Sequence: '&#10;'; Symbol: #10),
    (Sequence: '&amp;'; Symbol: '&'),
    (Sequence: '&quot;'; Symbol: '"'),
    (Sequence: '&gt;'; Symbol: '>'),
    (Sequence: '&lt;'; Symbol: '<')
  );

type
  TXmlParser = class(TParser)
  public
    constructor Create(const Text: string); override;
    function GetNextIdent(out Lexem: string): integer;
  end;

var
  XmlSymbolLexems, XmlKeywordLexems: TLexemList;

{ Functions }

procedure DeleteInvisibleSymbol(var s: string);
var
  i: Integer;
begin
  for i := Length(s) downto 1 do
    if not CharInSet(s[i], ['a'..'z', 'A'..'Z']) and (s[i] <> '.') and (s[i] <> ':') then
      Delete(s, i, 1);
end;

function XMLEncode(const AStr: WideString): WideString;
var
  sb: WideStringBuilder;
begin
  sb := WideStringBuilder.Create(AStr, Length(AStr));
  try
    sb.Replace('&', '&#x26;');
    sb.Replace('''', '&#x27;');
    sb.Replace('"', '&#x22;');
    sb.Replace('<', '&#x3c;');
    sb.Replace('>', '&#x3e;');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function XMLDecode(const Value: TBytes; DecodeHTMLEntities: boolean = True): TBytes;

  function CheckSequences(Index: Integer): Integer;
  var
    i, cnt: Integer;
  begin
    if DecodeHTMLEntities then
      cnt := Length(XmlSequences)
    else
      cnt := Length(XmlSequences) - 4;

    for Result := 0 to cnt - 1 do begin
      if Index + Length(XmlSequences[Result].Sequence) > Length(Value) then
        continue;
      for i := 2 to Length(XmlSequences[Result].Sequence) do
        if Value[Index + i - 1] <> Byte(XmlSequences[Result].Sequence[i]) then
          Break
        else if i = Length(XmlSequences[Result].Sequence) then
          Exit;
    end;

    Result := -1;
  end;

var
  i, j, k: Integer;
begin
  SetLength(Result, Length(Value));

  i := 0;
  j := 0;
  while i < Length(Value) do begin
    if Value[i] <> Byte('&') then begin
      Result[j] := Value[i];
      Inc(i);
    end
    else begin
      k := CheckSequences(i);
      if k >= 0 then begin
        Result[j] := Byte(XmlSequences[k].Symbol);
        i := i + Length(XmlSequences[k].Sequence);
      end
      else begin
        Result[j] := Value[i];
        Inc(i);
      end;
    end;
    Inc(j);
  end;

  SetLength(Result, j);
end;

{ XmlTextReader }

constructor XmlTextReader.Create(Stream: TStream; DecodeHTMLEntities: boolean = True);
begin
  inherited Create;

  FDecodeHTMLEntities := DecodeHTMLEntities;

  FStream := Stream;
  FStream.Position := 0;
  FStreamPosition := 0;

  FFullSize := FStream.Size;
  FBlockSize := 8192;
  SetLength(FTmpBlock, FBlockSize);
  FMaxNumBlock := (FFullSize div FBlockSize) + 1;
  SetLength(FBlocks, FMaxNumBlock);

  FLastBlockSize := 0;
  FBlockCount := 0;
  FActualPosition := 0;
  FCurPosition := 0;

  LoadNextBlock(1);
  InitInstance;
end;

constructor XmlTextReader.Create(const Str: string; DecodeHTMLEntities: boolean = True);
begin
  inherited Create;

  FDecodeHTMLEntities := DecodeHTMLEntities;

  FMaxNumBlock := 1;
  SetLength(FBlocks, 1);
  FBlocks[0] := Encoding.UTF8.GetBytes(Str);
  FBlockSize := Length(FBlocks[0]);
  FFullSize := FBlockSize;

  FStream := nil; // When  FStream = nil  procedure NormalizeBuffer not working
  FStreamPosition := FFullSize;

  FLastBlockSize := FFullSize;
  FBlockCount := 1;
  FActualPosition := 0;
  FCurPosition := 0;

  InitInstance;
end;

constructor XmlTextReader.Create(const Binary: TBytes; DecodeHTMLEntities: boolean = True);
begin
  inherited Create;

  FDecodeHTMLEntities := DecodeHTMLEntities;

  FMaxNumBlock := 1;
  SetLength(FBlocks, 1);
  FBlocks[0] := Binary;
  FBlockSize := Length(FBlocks[0]);
  FFullSize := FBlockSize;

  FStream := nil; // When  FStream = nil  procedure NormalizeBuffer not working
  FStreamPosition := FFullSize;

  FLastBlockSize := FFullSize;
  FBlockCount := 1;
  FActualPosition := 0;
  FCurPosition := 0;

  InitInstance;
end;

destructor XmlTextReader.Destroy;
begin
  SetLength(FBlocks, 0);
  SetLength(FAttributes, 0);

  inherited;
end;

function XmlTextReader.GetName: string;
begin
  Result := Trim(Encoding.UTF8.GetString(FName));
end;

function XmlTextReader.GetPrefix: string;
begin
  Result := Trim(Encoding.UTF8.GetString(FPrefix));
end;

function XmlTextReader.GetFullName: string;
var
  Prefix: string;
begin
  Result := GetName;

  Prefix := GetPrefix;
  if Prefix <> '' then
    Result := Prefix + ':' + Result;
end;

function XmlTextReader.GetValue: string;
begin
  Result := Trim(Encoding.UTF8.GetString(FValue));
end;

{$IFNDEF NEXTGEN}

function XmlTextReader.GetAnsiValue: AnsiString;
begin
  Result := Trim(Encoding.UTF8.GetAnsiString(FValue));
end;

function XmlTextReader.GetWideValue: WideString;
begin
  Result := Trim(Encoding.UTF8.GetWideString(FValue));
end;

{$ENDIF}

function XmlTextReader.GetDepth: integer;
begin
  Result := 0;
end;

function XmlTextReader.GetHasAttributes: Boolean;
begin
  Result := FAttributeCount > 0;
end;

function XmlTextReader.GetAttributeName(Index: Integer): string;
begin
  Result := Encoding.UTF8.GetString(FAttributes[Index].Name);
end;

function XmlTextReader.GetAttributeValue(Index: Integer): string;
begin
  Result := Encoding.UTF8.GetString(FAttributes[Index].Value);
end;

function XmlTextReader.GetAttributePrefix(Index: Integer): string;
begin
  Result := Encoding.UTF8.GetString(FAttributes[Index].Prefix);
end;

function XmlTextReader.GetAttributeCount: integer;
begin
  Result := FAttributeCount;
end;

function XmlTextReader.LoadNextBlock(Count: integer): boolean;
begin
  Result := False;
  if FStream = nil then
    Exit;

  if FBlockCount >= Count then begin
    Result := True;
    Exit;
  end;

  if Length(FTmpBlock) < FBlockSize then
    SetLength(FTmpBlock, FBlockSize);
  FLastBlockSize := FStream.Read(FTmpBlock[0], FBlockSize);
  FStreamPosition := FStream.Position;
  Result := FLastBlockSize <> 0;
  if Result then begin
    Inc(FBlockCount);
    SetLength(FBlocks, FBlockCount);
    SetLength(FBlocks[FBlockCount - 1], FLastBlockSize);
    Move(FTmpBlock[0], FBlocks[FBlockCount - 1][0], FLastBlockSize);
    FCurPosition := 0;
  end;
end;

procedure XmlTextReader.FreeLastBlocks(Count: integer);
var
  i: integer;
begin
  if FBlockCount > Count then begin
    for i := 0 to Count - 1 do
      FBlocks[i] := FBlocks[FBlockCount - Count];
    FBlockCount := Count;
    SetLength(FBlocks, FBlockCount);
  end;
end;

function XmlTextReader.GetEof: Boolean;
begin
  Result := (((FStreamPosition - FLastBlockSize) + FActualPosition) >= FFullSize) or
            (FCurPosition > FLastBlockSize);
end;

function XmlTextReader.IsToken(const SubStr: AnsiString): boolean;
var
  i, BlockPos, BlockNo: integer;
begin
  BlockPos := FActualPosition;
  BlockNo := 0;

  for i := 1 to LengthA(SubStr) do begin
    if BlockPos > Length(FBlocks[BlockNo]) then begin
      Inc(BlockNo);
      if not LoadNextBlock(BlockNo + 1) then begin
        Result := False;
        Exit;
      end;
      BlockPos := 1;
    end;

    if FBlocks[BlockNo][BlockPos] <> Ord(SubStr[i]{$IFDEF NEXTGEN}^{$ENDIF}) then begin
      Result := False;
      Exit;
    end;
    Inc(BlockPos);
  end;

  FActualPosition := BlockPos;
  FCurPosition := BlockPos;

  FreeLastBlocks(1);
  if (FActualPosition >= Length(FBlocks[0])) and (FStream <> nil) then begin
    FreeLastBlocks(0);
    LoadNextBlock(1);
    FActualPosition := 0;
  end;

  Result := True;
end;

function XmlTextReader.MoveTo(const Lexem: AnsiString): boolean;
begin
  Result := False;
  while (FActualPosition < FFullSize) and not Eof do begin
    if IsToken(Lexem) then begin
      Result := True;
      Exit;
    end;

    Inc(FActualPosition);

    if (FActualPosition >= Length(FBlocks[0])) and (FStream <> nil) then begin
      FreeLastBlocks(0);
      LoadNextBlock(1);
      FActualPosition := 0;
    end;
  end;
end;

function XmlTextReader.GetNextSymbol: AnsiChar;
begin
  if (FCurPosition >= Length(FBlocks[0])) and (FStream <> nil) then begin
    FreeLastBlocks(0);
    LoadNextBlock(1);
    FActualPosition := 0;
  end;
  Result := AnsiChar(FBlocks[0][FCurPosition]);
  Inc(FCurPosition);
end;

function XmlTextReader.ReadTo(const SubStr: AnsiString; out ResultValue: TBytes; const AdvLenth: integer): boolean;
var
  i, BlockNo, FoundBlockNo, SubStrIndex, SubStrLen,
  CurPos, StartPos, FoundPos, FullLen, Len, Offset, l: integer;
begin
  BlockNo := 0;
  FoundBlockNo := 0;
  SubStrIndex := 1;
  StartPos := FActualPosition;
  FoundPos := -1;
  SubStrLen := LengthA(SubStr);
  Result := False;
  CurPos := StartPos;

  while BlockNo <= FMaxNumBlock do begin
    while CurPos < Length(FBlocks[BlockNo]) do begin

      if FBlocks[BlockNo][CurPos] <> Ord(SubStr[SubStrIndex]{$IFDEF NEXTGEN}^{$ENDIF}) then begin
        SubStrIndex := 1;
        FoundPos := -1;
        Inc(CurPos);
        Continue;
      end;

      if FoundPos = -1 then begin
        FoundPos := CurPos;
        FoundBlockNo := BlockNo;
      end;

      if SubStrIndex = SubStrLen then begin
        FullLen := FoundPos - StartPos + FBlockSize * FoundBlockNo + AdvLenth;
        if Length(FTmpBlock) < FullLen then
          SetLength(FTmpBlock, FullLen);

        Len := FullLen;
        if Len > FBlockSize - StartPos then
          Len := FBlockSize - StartPos;
        Move(FBlocks[0][StartPos], FTmpBlock[0], Len);
        Offset := Len;

        i := 1;
        Len := FullLen - Offset;
        while Len > 0 do begin
          if i >= FBlockCount then
            LoadNextBlock(FBlockCount + 1);

          l := Min(Len, FBlockSize);
          Move(FBlocks[i][0], FTmpBlock[Offset], l);
          Inc(Offset, l);
          Dec(Len, l);
          Inc(i);
        end;

        ResultValue := copy(FTmpBlock, 0, FullLen);
        FreeLastBlocks(FBlockCount - FoundBlockNo);

        FCurPosition := FoundPos;
        FActualPosition := FoundPos;
        Result := True;
        Exit;
      end;

      Inc(CurPos);
      Inc(SubStrIndex);
    end;

    Inc(BlockNo);
    CurPos := 0;
    if not LoadNextBlock(BlockNo + 1) then
      Exit;
  end;
end;

procedure XmlTextReader.InitInstance;
begin
  FState := Initial;
  FNodeType := ntNone;
  SetLength(FAttributes, 0);
  FAttributeCount := 0;
  SetLength(FCurrElementName, 0);
end;

function XmlTextReader.Read: boolean;

  function Pos(B: Byte; const Data: TBytes): Integer;
  begin
    for Result := 0 to Length(Data) - 1 do
      if Data[Result] = B then
        Exit;

    Result := -1;
  end;

var
  Ind, Len: Integer;
  Value: TBytes;
  EndTagName: string;
  IsTextFound: Boolean;
  a: AnsiChar;
begin
  Result := False;

  FAttributeCount := 0;
  SetLength(FAttributes, 0);

  if FState in [Initial, Interactive] then begin
    IsTextFound := False;
    if NodeType in [ntStartElement, ntClosedElement] then begin
      a := GetNextSymbol;
      while (byte(a) <> Ord('<')) and not Eof do begin
        if CharInSet(a, ['a'..'z', 'A'..'Z', '0'..'9', '.', ':']) then begin
          IsTextFound := True;
          break;
        end;
        a := GetNextSymbol;
      end;
      if IsTextFound then
        FNodeType := ntText;
    end;

    if not IsTextFound then begin
      if not MoveTo('<') then begin
        if Eof then
          Exit;
        FState := Error;
        XmlException.Create('Root element missing');
      end;

      if IsToken('?') then
        FNodeType := ntDeclaration
      else
      if IsToken('!--') then
        FNodeType := ntComment
      else if IsToken('!DOCTYPE') then
        FNodeType := ntDocumentType
      else if IsToken('/') then begin
        FNodeType := ntEndElement;
      end
      else
        FNodeType := ntStartElement;
    end;
    FState := Interactive;
  end;

  SetLength(FName, 0);
  SetLength(FValue, 0);
  SetLength(FPrefix, 0);

  case FNodeType of
    ntDeclaration: begin
      ReadTo(' ', Value);
      FName := Value;
      if not ReadTo('?>', Value) then
      begin
        FState := Error;
        raise XmlException.Create('Invalid declaration tag');
      end;
      FValue := Value;
      Result := True;
    end;

    ntComment: begin
      if not ReadTo('-->', Value) then
      begin
        FState := Error;
        raise XmlException.Create('Invalid comment tag');
      end;
      FValue := Value;
      Result := True;
    end;

    ntDocumentType: begin
      if not ReadTo('[<', Value) then begin
        FState := Error;
        raise XmlException.Create('Invalid Document type tag');
      end;
      FName := Value;
      ReadTo('>]', Value);
      FValue := Value;
      Result := True;
    end;

    ntEndElement: begin
      Result := ReadTo('>', Value);
      Ind := Pos(Byte(':'), Value);
      if Ind < 0 then
        FName := Value
      else begin
        FPrefix := Copy(Value, 0, Ind);
        Inc(Ind);
        FName := Copy(Value, Ind, Length(Value) - Ind);
      end;
    end;

    ntStartElement: begin
      if ReadTo('>', Value, 1) then begin
        Ind := Pos(Byte(' '), Value);
        if Ind < 0 then begin
          Len := Length(Value);
          if (Len >= 2) and (Value[Len - 2] = Byte('/')) then begin
            FNodeType := ntClosedElement;
            FName := Copy(Value, 0, Len - 2);
          end
          else
            FName := Copy(Value, 0, Len - 1);
        end
        else begin
          FName := Copy(Value, 0, Ind);

          Len := Length(Value);
          if (Len >= 2) and (Value[Len - 2] = Byte('/')) then
            FNodeType := ntClosedElement;

          Ind := Length(FName);
          Value := Copy(Value, Ind, Length(Value) - Ind);
          ParseXMLNodeAttributes(Value);
        end;

        FCurrElementName := FName;

        Ind := Pos(Byte(':'), FName);
        if Ind >= 0 then begin
          FPrefix := Copy(FName, 0, Ind);
          Inc(Ind);
          FName := Copy(FName, Ind, Length(FName) - Ind);
        end;

        Result := True;
      end;
    end;

    ntText: begin
      EndTagName := Encoding.UTF8.GetString(FCurrElementName);
      DeleteInvisibleSymbol(EndTagName);
      EndTagname := '</' + EndTagName + '>';

      if FCurrElementName[Length(FCurrElementName) - 1] <> Byte('/') then begin
        Inc(FActualPosition);
        ReadTo(AnsiString(EndTagName), Value);
        FValue := XMLDecode(Value, FDecodeHTMLEntities);
      end;
      Result := True;
    end;
  else
    begin
     FState := Error;
     Assert(False);
    end;
  end;
end;

function XmlTextReader.GetAttributeIndex(const Name: string): Integer;
var
  NameUpper: string;
  AttrName: string;
  AttrPrefix: string;
begin
  NameUpper := UpperCase(Name);

  for Result := 0 to FAttributeCount - 1 do begin
    AttrPrefix := UpperCase(Encoding.UTF8.GetString(FAttributes[Result].Prefix));
    AttrName := UpperCase(Encoding.UTF8.GetString(FAttributes[Result].Name));
    if (AttrName = NameUpper) or (AttrPrefix + ':' + AttrName = NameUpper) then
      Exit;
  end;

  Result := -1;
end;

procedure XmlTextReader.ParseXMLNodeAttributes(const Node: TBytes);

  function ReadNext(var CurPos: Integer; out Value: TBytes): Byte;
  var
    ValuePos: Integer;
    InQuotes: boolean;
  begin
    Result := 0;
    if CurPos >= Length(Node) then
      Exit;

    InQuotes := False;
    ValuePos := -1;
    while True do begin
      Result := Node[CurPos];

      if Result in [$22, $27] then // '"', ''''
        InQuotes := not InQuotes
      else if not InQuotes then
        if Result in [$3A, $3C, $3D, $3E, $2F] then // ':', '<', '=', '>', '/'
          Break
        else if Result <= $20 then
          if (ValuePos < 0) or (ValuePos = CurPos) then
            ValuePos := CurPos + 1
          else
            Break;

      if ValuePos < 0 then
        ValuePos := CurPos;

      Inc(CurPos);
      if CurPos >= Length(Node) then begin
        Result := 0;
        Break;
      end;
    end;

    if ValuePos > 0 then
      if ((Node[ValuePos] = $22) and (Node[CurPos - 1] = $22)) or // '"'
         ((Node[ValuePos] = $27) and (Node[CurPos - 1] = $27))    // ''''
      then
        Value := Copy(Node, ValuePos + 1, CurPos - ValuePos - 2)
      else
        Value := Copy(Node, ValuePos, CurPos - ValuePos)
    else
      SetLength(Value, 0);

    Inc(CurPos);
  end;

var
  CurPos: Integer;
  Code: integer;
  Lexem, AttrName, AttrValue, AttrPrefix: TBytes;
begin
  CurPos := 0;
  while CurPos < Length(Node) do begin
    Code := ReadNext(CurPos, Lexem);
    if Code = $3A then // ':'
      AttrPrefix := Lexem
    else if Code = $3D then // '='
      AttrName := Lexem
    else if (Code <= $20) or
            (Code in [$3C, $3E, $2F]) // '<', '=', '>', '/'
    then begin
      AttrValue := XMLDecode(Lexem, FDecodeHTMLEntities);

      if Length(FAttributes) = FAttributeCount then
        if Length(FAttributes) = 0 then
          SetLength(FAttributes, 16)
        else
          SetLength(FAttributes, Length(FAttributes) * 2);

      if Length(AttrName) > 0 then begin
        FAttributes[FAttributeCount].Prefix := AttrPrefix;
        FAttributes[FAttributeCount].Name := AttrName;
        FAttributes[FAttributeCount].Value := AttrValue;
        Inc(FAttributeCount);
      end;

      if Code in [$3C, $3E, $2F] then
        Exit
      else begin
        SetLength(AttrPrefix, 0);
        SetLength(AttrName, 0);
        SetLength(AttrValue, 0);
      end;
    end;
  end;
end;

function XmlTextReader.Items(const AttrIndex: Integer): string;
begin
  Result := GetAttributeValue(AttrIndex);
end;

function XmlTextReader.Items(const AttrName: string): string;
var
  Index: Integer;
begin
  Index := GetAttributeIndex(AttrName);
  Result := Items(Index);
end;

procedure XmlTextReader.MoveToAttribute(Index: integer);
begin
  try
    FPrefix := FAttributes[Index].Prefix;
    FName := FAttributes[Index].Name;
    FValue := FAttributes[Index].Value;
  except
    raise XmlException.Create(Format('Attribute not found (%d)', [Index]));
  end;
end;

function XmlTextReader.MoveToAttribute(const Name: string): Boolean;
var
  AttrIndex: Integer;
begin
  try
    AttrIndex := GetAttributeIndex(Name);
    if AttrIndex >= 0 then begin
      MoveToAttribute(AttrIndex);
      Result := True;
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

{ XmlTextWriter }

constructor XmlTextWriter.Create(w: StreamWriter);
begin
  inherited Create;

  FFormatting := fmtNone;
  FIndentation := 2;
  FIndentChar := ' ';
  FQuoteChar := '"';
  FWriteState := wsStart;
  FDepth := 0;
  FPosStack := TStack{$IFDEF POSIX}<Pointer>{$ENDIF}.Create;
  FTagStack := TStringList.Create;
  FText := WideStringBuilder.Create(8192);
  FWriter := w;
end;

destructor XmlTextWriter.Destroy;
begin
  FPosStack.Free;
  FTagStack.Free;
  FText.Free;

  inherited;
end;

procedure XmlTextWriter.InternalWriteStartElement(const Prefix, LocalName, ns: string);
var
  EndTagPos: Integer;
begin
  InternalCloseStartTag;
  if FWriteState = wsContent then
    FlushData;

  if FDepth > 0 then
    FText.Append(XMLLineSeparator);

  if Prefix <> '' then
    FText.Append(IndentStr + '<' + WideString(Prefix) + ':' + WideString(LocalName))
  else
    FText.Append(IndentStr + '<' + WideString(LocalName));

  if ns <> '' then
    if Prefix <> '' then
      FText.Append(' ' + 'xmlns:' + WideString(Prefix) + '=' + FQuoteChar + WideString(ns) + FQuoteChar)
    else
      FText.Append(' ' + 'xmlns=' + FQuoteChar + WideString(ns) + FQuoteChar);

  EndTagPos := FText.Length;

  inc(FDepth);

  FPosStack.Push(PtrOffset(nil, EndTagPos));
  if Prefix <> '' then
    PushTagName(Prefix + ':' + LocalName)
  else
    PushTagName(LocalName);

  FWriteState := wsElement;
end;

procedure XmlTextWriter.InternalWriteElementString(const LocalName, ns: string; const Value: WideString);
begin
  InternalCloseStartTag;
  if FWriteState = wsElement then
    inc(FDepth);
  FText.Append(XMLLineSeparator);
  FText.Append(IndentStr + '<' + WideString(LocalName));

  if ns <> '' then
    FText.Append(' xmlns=' + FQuoteChar + WideString(ns) + FQuoteChar + '>')
  else
    FText.Append('>');

  if Value <> '' then
    FText.Append(XMLEncode(Value));
  FText.Append('</' + WideString(LocalName) + '>');
  if FWriteState = wsElement then
    dec(FDepth);
  FlushData;
  FWriteState := wsContent;
end;

procedure XmlTextWriter.InternalWriteAttributeString(const Prefix, LocalName, ns: string; const Value: WideString);
var
  AttrPos: Integer;
  AttrStr: WideString;
begin
  if FWriteState in [wsElement, wsAttribute] then
    AttrPos := Integer(FPosStack.Pop {$IFDEF FPC}- nil{$ENDIF}) + 1
  else
    raise XmlException.Create('Token WriteAttributeString in state Content would result in an invalid XML document');

  if Prefix <> '' then
    AttrStr := ' ' + WideString(Prefix) + ':'
  else
    AttrStr := ' ';

  AttrStr := AttrStr + WideString(LocalName) + '=' + FQuoteChar + XMLEncode(Value) + FQuoteChar;// + ' ';

  FText.Insert(AttrPos - 1, AttrStr);

  AttrPos := AttrPos + Length(AttrStr);

  if ns <> '' then begin
    AttrStr := 'xmlns:' + WideString(Prefix) + '=' + FQuoteChar + WideString(ns) + FQuoteChar;// + ' ';
    FText.Insert(AttrPos - 1, AttrStr);
    AttrPos := AttrPos + Length(AttrStr);
  end;

  FPosStack.Push(PtrOffset(nil, AttrPos - 1));
  FWriteState := wsAttribute;
end;

procedure XmlTextWriter.FlushData;
begin
  FWriter.Write(FText.ToString);
  FText.Length := 0;
end;

procedure XmlTextWriter.Close;
begin
  FlushData;
end;

procedure XmlTextWriter.WriteStartElement(const LocalName: string);
begin
  InternalWriteStartElement('', LocalName, '');
end;

procedure XmlTextWriter.WriteStartElement(const Prefix, LocalName, ns: string);
begin
  if (Prefix <> '') and (ns = '') then
    raise XmlException.Create('Cannot use a prefix with an empty namespace');

  InternalWriteStartElement(Prefix, LocalName, ns);
end;

procedure XmlTextWriter.WriteStartElement(const LocalName, ns: string);
begin
  InternalWriteStartElement('', LocalName, ns);
end;

procedure XmlTextWriter.WriteEndElement;
begin
  dec(FDepth);
  if (FWriteState = wsAttribute) then begin
    FText.Append(' />');
    PopTagName;
    FWriteState := wsContent;
  end
  else
    InternalWriteEndElement;
  FlushData;
end;

procedure XmlTextWriter.InternalWriteEndElement;
var
  Len: Integer;
begin
  InternalCloseStartTag;
  Len := FText.Length;
  if (Len = 0) or ((Len >= 2) and (FText[Len - 1] <> #10) and (FText[Len - 2] <> #13)) then
    FText.Append(XMLLineSeparator + IndentStr + '</' + WideString(PopTagName) + '>')
  else
    FText.Append('</' + WideString(PopTagName) + '>');
  FWriteState := wsContent;
end;

procedure XmlTextWriter.WriteFullEndElement;
begin
  dec(FDepth);
  InternalWriteEndElement;
  FlushData;
end;

procedure XmlTextWriter.WriteString(const Text: WideString);
begin
  InternalCloseStartTag;
  FText.Append(Text);
  FWriteState := wsContent;
end;

procedure XmlTextWriter.WriteElementString(const LocalName, ns: string; const Value: WideString);
begin
  InternalWriteElementString(LocalName, ns, Value);
end;

procedure XmlTextWriter.WriteElementString(const LocalName: string; const Value: WideString);
begin
  InternalWriteElementString(LocalName, '', Value);
end;

procedure XmlTextWriter.WriteAttributeString(const LocalName: string; const Value: WideString);
begin
  InternalWriteAttributeString('', LocalName, '', Value);
end;

procedure XmlTextWriter.WriteAttributeString(const Prefix, LocalName, ns: string; const Value: WideString);
begin
  if (Prefix <> '') and (ns = '') then
    raise XmlException.Create('Cannot use a prefix with an empty namespace');

  InternalWriteAttributeString(Prefix, LocalName, ns, Value);
end;

{$IFDEF FPC}

procedure XmlTextWriter.WriteString(const Text: AnsiString);
begin
  WriteString(WideString(Text));
end;

procedure XmlTextWriter.WriteElementString(const LocalName, ns: string; const Value: AnsiString);
begin
  WriteElementString(LocalName, ns, WideString(Value));
end;

procedure XmlTextWriter.WriteElementString(const LocalName: string; const Value: AnsiString);
begin
  WriteElementString(LocalName, WideString(Value));
end;

procedure XmlTextWriter.WriteAttributeString(const LocalName: string; const Value: AnsiString);
begin
  WriteAttributeString(LocalName, WideString(Value));
end;

procedure XmlTextWriter.WriteAttributeString(const Prefix, LocalName, ns: string; const Value: AnsiString);
begin
  WriteAttributeString(Prefix, LocalName, ns, WideString(Value));
end;

{$ENDIF}

function XmlTextWriter.IndentStr: WideString;
var
  i: Integer;
begin
  Result := '';
  if (FFormatting = fmtIndented) and (FDepth <> 0) then
    for i := 1 to FDepth * FIndentation do
      Result := Result + FIndentChar;
end;

function XmlTextWriter.PopTagName: string;
begin
  if FTagStack.Count = 0 then
    raise XmlException.Create('There was no XML start tag open');
  Result := FTagStack[FTagStack.Count-1];
  FTagStack.Delete(FTagStack.Count-1);
end;

procedure XmlTextWriter.PushTagName(const TagName: string);
begin
  FTagStack.Add(TagName);
end;

procedure XmlTextWriter.InternalCloseStartTag;
var
  n, i: integer;
begin
  if FWriteState in [wsElement, wsAttribute] then begin
    FPosStack.Pop;
    n := FText.Length - 1;
    i := n;
    while (i >= 0) and (FText[i] <= #32) do
      Dec(i);
    if i = n then
      FText.Append('>')
    else begin
      FText.Insert(i + 1, '>');
      FText.Length := i + 2;
    end;
  end;
end;

{ StreamWriter }

constructor StreamWriter.Create(const path: string; Append: Boolean);
begin
  inherited Create;

  if FileExists(path) and Append then begin
    FStream := TFileStream.Create(path, fmOpenReadWrite);
    FStream.Seek(LongInt(0), soFromEnd);
  end
  else
    FStream := TFileStream.Create(path, fmCreate);
  FReleaseStream := True;
  FEncoding := Encoding.Default;
end;

constructor StreamWriter.Create(output: TStream; aEncoding: Encoding);
begin
  inherited Create;

  FStream := output;
  FReleaseStream := False;
  FEncoding := aEncoding;
end;

destructor StreamWriter.Destroy;
begin
  if FReleaseStream then
    FStream.Free;

  inherited;
end;

procedure StreamWriter.Close;
begin
end;

procedure StreamWriter.Flush;
begin
end;

procedure StreamWriter.Write(const value: WideString);
var
  bytes: TBytes;
begin
  bytes := FEncoding.{$IFNDEF VER5}GetBytes{$ELSE}GetBytesWide{$ENDIF}(value);
  FStream.Write(Pointer(bytes)^, Length(bytes));
end;

procedure StreamWriter.WriteLine(const value: WideString);
begin
  Write(value + XMLLineSeparator);
end;

{ TXmlParser }

constructor TXmlParser.Create(const Text: string);
begin
  inherited;

  FSymbolLexems := XmlSymbolLexems;
  FKeywordLexems := XmlKeywordLexems;

  FOmitKeywords := True; // XmlKeywordLexems.Count = 0
end;

function TXmlParser.GetNextIdent(out Lexem: string): integer;
var
  i: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[2].Start;//}
  try
{$ENDIF}
  Result := InternalGetNext; // Result is not in KeywordLexems

  if Result = lcIdent then begin// Optimize
    if Result > 0 then begin
      Assert(FLexem <> '');
      Lexem := FLexem
    end
    else begin
      if FLexemLength = 0 then
        Lexem := ''
      else
      begin
        Assert(FLexemPos > 0);
        Lexem := CopyText(FLexemPos, FLexemLength);
      end;
    end;
  end;

  if not FOmitKeywords and (Result = lcIdent) then begin
    Assert(FLexemPos > 0);
    Assert(FLexemLength > 0);
    i := FindLexemIndex(FLexemPos, FLexemLength, FKeywordLexems);
    if i <> -1 then begin
      Result := FKeywordLexems.Values[i];
      Assert(Result > 0);
      if Uppered then
        Lexem := AnsiUpperCase(Lexem);  //WAR problem with macros as key words
    end;
  end;

  Assert(Result <> - MaxInt);
{$IFDEF PERF_COUNTER}
  finally
    PerfCounters[2].Stop;
  end;
{$ENDIF}
end;

initialization
  XmlSymbolLexems := TLexemList.Create;
  XmlKeywordLexems := TLexemList.Create;

  XmlSymbolLexems.Add(':', lxColon);
  XmlSymbolLexems.Add('<', lxLess);
  XmlSymbolLexems.Add('=', lxEqual);
  XmlSymbolLexems.Add('>', lxMore);
  XmlSymbolLexems.Sort;

finalization
  XmlSymbolLexems.Free;
  XmlKeywordLexems.Free;

end.
