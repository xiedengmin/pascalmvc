
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Ora Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Odac.inc}
unit OraParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRTypes, CRParser;

const
  lxOraFirst       = 1000;
  lxALTER          = lxOraFirst;
  lxBODY           = lxALTER + 1;
  lxCONNECT        = lxBODY + 1;
  lxCREATE         = lxCONNECT + 1;
  lxDROP           = lxCREATE + 1;
  lxDEFINE         = lxDROP + 1;
  lxDISCONNECT     = lxDEFINE + 1;
  lxEDITIONABLE    = lxDISCONNECT + 1;
  lxEXEC           = lxEDITIONABLE + 1;
  lxEXIT           = lxEXEC + 1;
  lxEXPLAIN        = lxEXIT + 1;
  lxFUNCTION       = lxEXPLAIN + 1;
  lxJAVA           = lxFUNCTION + 1;
  lxMERGE          = lxJAVA + 1;
  lxNONEDITIONABLE = lxMERGE + 1;
  lxPACKAGE        = lxNONEDITIONABLE + 1;
  lxPARTITION      = lxPACKAGE + 1;
  lxPAUSE          = lxPARTITION + 1;
  lxPROCEDURE      = lxPAUSE + 1;
  lxPROMPT         = lxPROCEDURE + 1;
  lxQUIT           = lxPROMPT + 1;
  lxREM            = lxQUIT + 1;
  lxREMARK         = lxREM + 1;
  lxREPLACE        = lxREMARK + 1;
  lxSTART          = lxREPLACE + 1;
  lxTRIGGER        = lxSTART + 1;
  lxTYPE           = lxTRIGGER + 1;
  lxUNDEFINE       = lxTYPE + 1;
  lxUNIQUE         = lxUNDEFINE + 1;
  lxWRAPPED        = lxUNIQUE + 1;
  lxACCEPT         = lxWRAPPED + 1;
  lxGRANT          = lxACCEPT + 1;
  lxREVOKE         = lxGRANT + 1;
  lxSOURCE         = lxREVOKE +  1;
  lxRESOURCE       = lxSOURCE +  1;
  lxCLASS          = lxRESOURCE +  1;

type
  TOraParser = class (TSQLParser)
  private
    FInWrapped: boolean;
  protected
    procedure InitParser; override;
  public
    function GetNext(out Lexem: string): integer; override;
    function IsMacroAllowed(Code: integer): boolean; override;
    function IsSelectModifier(Code: integer): boolean; override;
    class function IsNumericMacroNameAllowed: boolean; override;
    class function IsFunctionOrConst(const UpperedName: string): boolean; override;
    class function IsQuasiColumn(const UpperedName: string): boolean; override;
  end;

  TTNSParser = class (TParser)
  protected
    function IsInlineComment(Ch: Char; Pos: integer): boolean; override;
  public
    function GetText: string;
  end;

implementation

var
  OraKeywordLexems: TLexemList;
  OraClauses: TClauseList;

{ TOraParser }

procedure TOraParser.InitParser;
begin
  inherited;

  FKeywordLexems := OraKeywordLexems;
  FClauses := OraClauses;

  FInWrapped := False;
  CommentBegin := '/*';
  CommentEnd := '*/';
  AlternativeQuoting := True;
end;

function TOraParser.GetNext(out Lexem: string): integer;
var
  Code: Integer;
  Stm: string;
  StartPos: Integer;
  BlankLine: boolean;
  InWrappedDelimiter: boolean;
begin
  if not FInWrapped then begin
    Result := inherited GetNext(Lexem);
    if Result = lxWRAPPED then
      FInWrapped := True;
  end
  else begin
    // return all wrapped code as single string
    InWrappedDelimiter := False;
    BlankLine := False;
    StartPos := CurrPos + 1;
    while True do begin
      Code := inherited GetNext(Stm);
      if Code = lcEnd then
        break
      else if BlankLine and (Stm = '/') then
        InWrappedDelimiter := True
      else if (Code <> lcBlank) and (Code <> lcComment) then begin
        InWrappedDelimiter := False;
        BlankLine := False;
      end
      else if (PrevLine < CurrLine) then begin
        if InWrappedDelimiter then
          break
        else
          BlankLine := True;
      end;
    end;

    FInWrapped := False;
    Result := lcString;
    Lexem := CopyText(StartPos, CurrPos - StartPos + 1);
  end
end;

function TOraParser.IsMacroAllowed(Code: integer): boolean;
begin
  if FInWrapped then
    Result := False
  else
    Result := inherited IsMacroAllowed(Code);
end;

function TOraParser.IsSelectModifier(Code: integer): boolean;
begin
  Result := inherited IsSelectModifier(Code) or (Code = lxUNIQUE);
end;

class function TOraParser.IsNumericMacroNameAllowed: boolean;
begin
  Result := True;
end;

class function TOraParser.IsFunctionOrConst(const UpperedName: string): boolean;
begin
  if inherited IsFunctionOrConst(UpperedName) or
    (UpperedName = 'SYSDATE') or
    (UpperedName = 'USER') or
    (UpperedName = 'UID')
  then
    Result := True
  else
    Result := False;
end;

class function TOraParser.IsQuasiColumn(const UpperedName: string): boolean;
begin
  if UpperedName = 'ROWID' then
    Result := True
  else
    Result := False;
end;

{ TTNSParser }

function TTNSParser.IsInlineComment(Ch: Char; Pos: integer): boolean;
begin
  Result := (TextLength > Pos) and (Text[Pos] = '#');
end;

function TTNSParser.GetText: string;
begin
  Result := FCurrentBlock;
end;

initialization
  OraKeywordLexems := TLexemList.Create;
  OraKeywordLexems.Assign(SQLKeywordLexems);

  OraKeywordLexems.Add('ALTER',          lxALTER    );
  OraKeywordLexems.Add('BODY',           lxBODY     );
  OraKeywordLexems.Add('CONNECT',        lxCONNECT  );
  OraKeywordLexems.Add('CREATE',         lxCREATE   );
  OraKeywordLexems.Add('DECLARE',        lxDECLARE  );
  OraKeywordLexems.Add('DEFINE',         lxDEFINE   );
  OraKeywordLexems.Add('DISCONNECT',     lxDISCONNECT);
  OraKeywordLexems.Add('EDITIONABLE',    lxEDITIONABLE);
  OraKeywordLexems.Add('EXEC',           lxEXEC     );
  OraKeywordLexems.Add('EXIT',           lxEXIT     );
  OraKeywordLexems.Add('EXPLAIN',        lxEXPLAIN  );
  OraKeywordLexems.Add('FUNCTION',       lxFUNCTION );
  OraKeywordLexems.Add('JAVA',           lxJAVA     );
  OraKeywordLexems.Add('MERGE',          lxMERGE    );
  OraKeywordLexems.Add('NONEDITIONABLE', lxNONEDITIONABLE);
  OraKeywordLexems.Add('PACKAGE',        lxPACKAGE  );
  OraKeywordLexems.Add('PAUSE',          lxPAUSE    );
  OraKeywordLexems.Add('PARTITION',      lxPARTITION);
  OraKeywordLexems.Add('PROCEDURE',      lxPROCEDURE);
  OraKeywordLexems.Add('PROMPT',         lxPROMPT   );
  OraKeywordLexems.Add('QUIT',           lxQUIT     );
  OraKeywordLexems.Add('REM',            lxREM      );
  OraKeywordLexems.Add('REMARK',         lxREMARK   );
  OraKeywordLexems.Add('REPLACE',        lxREPLACE  );
  OraKeywordLexems.Add('START',          lxSTART    );
  OraKeywordLexems.Add('TRIGGER',        lxTRIGGER  );
  OraKeywordLexems.Add('TYPE',           lxTYPE     );
  OraKeywordLexems.Add('UNDEFINE',       lxUNDEFINE );
  OraKeywordLexems.Add('WRAPPED',        lxWRAPPED  );
  OraKeywordLexems.Add('ACCEPT',         lxACCEPT   );
  OraKeywordLexems.Add('GRANT',          lxGRANT    );
  OraKeywordLexems.Add('REVOKE',         lxREVOKE   );
  OraKeywordLexems.Add('SOURCE',         lxSOURCE   );
  OraKeywordLexems.Add('RESOURCE',       lxRESOURCE );
  OraKeywordLexems.Add('CLASS',          lxCLASS    );
  OraKeywordLexems.Sort;

  OraClauses := TClauseList.Create;
  OraClauses.Add(lxWHERE);
  OraClauses.Add(lxSTART);
  OraClauses.Add(lxCONNECT);
  OraClauses.Add(lxGROUP);
  OraClauses.Add(lxHAVING);
  OraClauses.Add(lxUNION);
  OraClauses.Add(lxINTERSECT);
  OraClauses.Add(lxMINUS);
  OraClauses.Add(lxORDER);
  OraClauses.Add(lxFOR);
  OraClauses.Add(lxEXEC);

finalization
  OraKeywordLexems.Free;
  OraClauses.Free;

end.
