
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  My Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MyParserUni;
{$ENDIF}

interface
uses
{$IFNDEF CLR}
  CLRClasses,
{$ENDIF}
  CRTypes, CRParser;

const
  lxMyFirst     = 1000;
  lxALTER       = lxMyFirst;
  lxCHARSET     = lxALTER + 1;
  lxCOLUMNS     = lxCHARSET + 1;
  lxCREATE      = lxCOLUMNS + 1;
  lxCROSS       = lxCREATE + 1;
  lxDELIMITER   = lxCROSS + 1;
  lxDESCRIBE    = lxDELIMITER + 1;
  lxDISTINCTROW = lxDESCRIBE + 1;
  lxENGINES     = lxDISTINCTROW + 1;
  lxEXPLAIN     = lxENGINES + 1;
  lxFORCE       = lxEXPLAIN + 1;
  lxFUNCTION    = lxFORCE + 1;
  lxHANDLER     = lxFUNCTION + 1;
  lxIGNORE      = lxHANDLER + 1;
  lxNATURAL     = lxIGNORE + 1;
  lxPRIVILEGES  = lxNATURAL + 1;
  lxPROCEDURE   = lxPRIVILEGES + 1;
  lxPROCESSLIST = lxPROCEDURE + 1;
  lxREPLACE     = lxPROCESSLIST + 1;
  lxSHOW        = lxREPLACE + 1;
  lxSLAVE       = lxSHOW + 1;
  lxSTATUS      = lxSLAVE + 1;
  lxSTRAIGHT_JOIN = lxSTATUS + 1;
  lxTRIGGER     = lxSTRAIGHT_JOIN + 1;
  lxUSE         = lxTRIGGER + 1;
  lxLOOP         = lxUSE + 1;
  lxREPEAT       = lxLOOP + 1;
  LxWHILE       = lxREPEAT + 1;

type
  TMyParser = class (TSQLParser)
  protected
    function IsStringQuote(Ch: Char): boolean; override;
    procedure ToRightQuote(RightQuote: Char); override;
  {$IFNDEF CLR}
  {$IFNDEF IS_UNICODE}
  {$IFNDEF PUREPASCAL}
    procedure ToRightQuoteA(RightQuote: AnsiChar); override;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
    function IsIdentQuote(Ch: Char): boolean; override;
    function IsInlineComment(Ch: Char; Pos: integer): boolean; override;

    procedure InitParser; override;
  public
  end;

  TMyUniParser = class (TMyParser)
  protected
    function IsStringQuote(Ch: Char): boolean; override;
    function IsIdentQuote(Ch: Char): boolean; override;
  end;

implementation

uses
  Classes, SysUtils, CRFunctions;

var
  MyKeywordLexems: TLexemList;
  MyClauses: TClauseList;

{ TMyParser }

procedure TMyParser.InitParser;
begin
  inherited;

  FKeywordLexems := MyKeywordLexems;
  FClauses := MyClauses;

  CommentBegin := '/*';
  CommentEnd := '*/';
end;

function TMyParser.IsStringQuote(Ch: Char): boolean;
begin
  Result := (Ch = '''')
    or (Ch = '"' {WAR if not MySQL ANSI mode});
end;

{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
{$IFNDEF PUREPASCAL}
procedure TMyParser.ToRightQuoteA(RightQuote: AnsiChar);
var
  i: integer;
  p: IntPtr;
  Ready: boolean;
begin
  repeat

    i := TextLength - Pos + 1 {Pos is counted from 1} + 1 {#0};
    p := PtrOffset(PChar(FCurrentBlock), Pos - 1);
    asm
    {$IFDEF CPU64}
      PUSH RAX
      PUSH RCX
      PUSH RDI

      MOV RDI, p;
      MOV ECX, i
      MOV AL, RightQuote
      REPNE   SCASB
      MOV i, ECX

      POP RDI
      POP RCX
      POP RAX
    {$ELSE}
      PUSHAD

      MOV EDI, p;
      MOV ECX, i
      MOV AL, RightQuote
      REPNE   SCASB
      MOV i, ECX

      POPAD
    {$ENDIF}
    end;
    Pos := TextLength - i + 1;
    // Assert(Text[Pos] <> #0);

    Ready := True;
    i := Pos;
    while i > 2 do begin
      Dec(i);
      if Text[i] <> '\' then begin
        // ???.'
        Ready := True;
        Break;
      end;

      // ???\'
      Dec(i);
      if Text[i] <> '\' then begin
        // ??.\'
        Ready := False; // Continue scanning
        Break;
      end;

      // ??\\'
    end;
    if not Ready then
      Inc(Pos);
  until Ready;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TMyParser.ToRightQuote(RightQuote: Char);
var
  c: Char;
begin
{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
{$IFNDEF PUREPASCAL}
  if not FAdvancedStringParsing and (FStream = nil) then begin
    ToRightQuoteA(RightQuote);
    Exit;
  end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

  while Pos <= TextLength do begin
    c := Text[Pos];
    if (c = #13) or (c = #10) then begin
      if (Pos < TextLength) and (Text[Pos + 1] = #10) then
        Inc(Pos);
      Inc(FCurrLine);
      FCurrBegLine := Pos + 1;
    end
    else
    if c = '\' then // Escape character
      Inc(Pos)
    else
    if c = RightQuote then
      Break;
    Inc(Pos);
  end;
end;

function TMyParser.IsIdentQuote(Ch: Char): boolean;
begin
  case Ch of
    '`' {, '"' WAR if MySQL ANSI mode}:
      Result := True;
    else
      Result := False;
  end;
end;

function TMyParser.IsInlineComment(Ch: Char; Pos: integer): boolean;
begin
  Result := Ch = '#';

  if not Result then
    Result := (TextLength >= Pos + 3) and (Ch = '-') and (Text[Pos + 1] = '-') and (Text[Pos + 2] = ' ');
    // Must be '-- ', see http://dev.mysql.com/doc/mysql/en/ansi-diff-comments.html for details
end;

function TMyUniParser.IsStringQuote(Ch: Char): boolean;
begin
  Result := Ch = '''';
end;

function TMyUniParser.IsIdentQuote(Ch: Char): boolean;
begin
  case Ch of
    '`', '"':
      Result := True;
    else
      Result := False;
  end;
end;

initialization
  MyKeywordLexems := TLexemList.Create;
  MyKeywordLexems.Assign(SQLKeywordLexems);

  MyKeywordLexems.Add('ALTER', lxALTER);
  MyKeywordLexems.Add('CHARSET', lxCHARSET);
  MyKeywordLexems.Add('COLUMNS', lxCOLUMNS);
  MyKeywordLexems.Add('CREATE', lxCREATE);
  MyKeywordLexems.Add('CROSS', lxCROSS);
  MyKeywordLexems.Add('DELIMITER', lxDELIMITER);
  MyKeywordLexems.Add('DESCRIBE', lxDESCRIBE);
  MyKeywordLexems.Add('DISTINCTROW', lxDISTINCTROW);
  MyKeywordLexems.Add('ENGINES', lxENGINES);
  MyKeywordLexems.Add('EXPLAIN', lxEXPLAIN);
  MyKeywordLexems.Add('FORCE', lxFORCE);
  MyKeywordLexems.Add('FUNCTION', lxFUNCTION);
  MyKeywordLexems.Add('HANDLER', lxHANDLER);
  MyKeywordLexems.Add('IGNORE', lxIGNORE);
  MyKeywordLexems.Add('NATURAL', lxNATURAL);
  MyKeywordLexems.Add('PRIVILEGES', lxPRIVILEGES);
  MyKeywordLexems.Add('PROCEDURE', lxPROCEDURE);
  MyKeywordLexems.Add('PROCESSLIST', lxPROCESSLIST);
  MyKeywordLexems.Add('REPLACE', lxREPLACE);
  MyKeywordLexems.Add('SHOW', lxSHOW);
  MyKeywordLexems.Add('SLAVE', lxSLAVE);
  MyKeywordLexems.Add('STATUS', lxSTATUS);
  MyKeywordLexems.Add('STRAIGHT_JOIN', lxSTRAIGHT_JOIN);
  MyKeywordLexems.Add('TRIGGER', lxTRIGGER);
  MyKeywordLexems.Add('USE', lxUSE);
  MyKeywordLexems.Add('LOOP', lxLOOP);
  MyKeywordLexems.Add('REPEAT', lxREPEAT);
  MyKeywordLexems.Add('WHILE', lxWHILE);
  MyKeywordLexems.Sort;

(*
SELECT
    [ALL | DISTINCT | DISTINCTROW ]
      [HIGH_PRIORITY]
      [STRAIGHT_JOIN]
      [SQL_SMALL_RESULT] [SQL_BIG_RESULT] [SQL_BUFFER_RESULT]
      [SQL_CACHE | SQL_NO_CACHE] [SQL_CALC_FOUND_ROWS]
    select_expr, ...
    [INTO OUTFILE 'file_name' export_options
      | INTO DUMPFILE 'file_name']
    [FROM table_references
      [WHERE where_definition]
      [GROUP BY {col_name | expr | position}
        [ASC | DESC], ... [WITH ROLLUP]]
      [HAVING where_definition]
      [ORDER BY {col_name | expr | position}
        [ASC | DESC] , ...]
      [LIMIT {[offset,] row_count | row_count OFFSET offset}]
      [PROCEDURE procedure_name(argument_list)]
      [FOR UPDATE | LOCK IN SHARE MODE]]
*)

  MyClauses := TClauseList.Create;
  MyClauses.Add(lxWHERE);
  MyClauses.Add(lxGROUP);
  MyClauses.Add(lxHAVING);
  MyClauses.Add(lxORDER);
  MyClauses.Add(lxLIMIT);
  MyClauses.Add(lxPROCEDURE);
  MyClauses.Add(lxFOR);
  MyClauses.Add(lxLOCK);

finalization
  MyKeywordLexems.Free;
  MyClauses.Free;

end.
