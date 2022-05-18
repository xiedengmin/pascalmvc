
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF VIRTUAL_QUERY}
{$I LiteDac.inc}
unit LiteParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRTypes, CRParser;

const
  lxLiteFirst  = 1000;
  lxALTER      = lxLiteFirst;
  lxCREATE     = lxALTER + 1;
  lxPRAGMA     = lxCREATE + 1;
  lxTRIGGER    = lxPRAGMA + 1;
  lxDROP       = lxTRIGGER + 1;
  lxDEFAULT    = lxDROP + 1;
  lxINDEX      = lxDEFAULT + 1;
  lxFOREIGN    = lxINDEX + 1;
  lxTABLE      = lxFOREIGN + 1;
  lxFIELD      = lxTABLE + 1;
  lxADD        = lxFIELD + 1;
  lxUNIQUE     = lxADD + 1;
  lxDATE       = lxUNIQUE + 1;
  lxTYPE       = lxDATE + 1;
  lxCHECK      = lxTYPE + 1;
  lxUSER       = lxCHECK + 1;

type
  TLiteParser = class (TSQLParser)
  protected
    procedure ToRightQuote(RightQuote: char); override;
    function IsIdentQuote(Ch: char): boolean; override;

    procedure InitParser; override;
  end;

var
  LiteKeywordLexems: TLexemList;
  LiteClauses: TClauseList;

implementation

procedure TLiteParser.ToRightQuote(RightQuote: char);
begin
  if RightQuote = '[' then
    RightQuote := ']';

  inherited;
end;

function TLiteParser.IsIdentQuote(Ch: char): boolean;
begin
  Result := False;
  case Ch of
    '`', '"', '[':
      Result := True;
  end;
end;

procedure TLiteParser.InitParser;
begin
  inherited;

  FKeywordLexems := LiteKeywordLexems;
  FClauses := LiteClauses;
end;

var
  i: Integer;

initialization
  LiteKeywordLexems := TLexemList.Create;
  LiteKeywordLexems.Assign(SQLKeywordLexems);

  LiteKeywordLexems.Add('ALTER',   lxALTER);
  LiteKeywordLexems.Add('CREATE',  lxCREATE);
  LiteKeywordLexems.Add('PRAGMA',  lxPRAGMA);
  LiteKeywordLexems.Add('TRIGGER', lxTRIGGER);
  LiteKeywordLexems.Add('DROP',    lxDROP);
  LiteKeywordLexems.Add('DEFAULT', lxDEFAULT);
  LiteKeywordLexems.Add('INDEX',   lxDEFAULT);
  LiteKeywordLexems.Add('FOREIGN', lxFOREIGN);
  LiteKeywordLexems.Add('TABLE',   lxTABLE);
  LiteKeywordLexems.Add('FIELD',   lxFIELD);
  LiteKeywordLexems.Add('ADD',     lxADD);
  LiteKeywordLexems.Add('UNIQUE',  lxUNIQUE);
  LiteKeywordLexems.Add('ORDER',   lxORDER);
  LiteKeywordLexems.Add('DATE',    lxDATE);
  LiteKeywordLexems.Add('TYPE',    lxTYPE);
  LiteKeywordLexems.Add('CHECK',   lxCHECK);
  LiteKeywordLexems.Add('USER',    lxUSER);
  LiteKeywordLexems.Sort;

  LiteClauses := TClauseList.Create;
  for i := 0 to SQLClauses.Count - 1 do
    LiteClauses.Add(SQLClauses[i]);
  LiteClauses.Add(lxLIMIT);

finalization
  LiteKeywordLexems.Free;
  LiteClauses.Free;

end.
