
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  MS Parser
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSParserUni;
{$ENDIF}

interface

uses
  CRParser, CRTypes;

const
  lxMSFirst   = 1000;
  lxALTER     = lxMSFirst;
  lxCOMPUTE   = lxMSFirst + 1;
  lxCROSS     = lxMSFirst + 2;
  lxDEFAULT   = lxMSFirst + 3;
  lxFUNCTION  = lxMSFirst + 4;
  lxGO        = lxMSFirst + 5;
  lxOPTION    = lxMSFirst + 6;
  lxPROC      = lxMSFirst + 7;
  lxPROCEDURE = lxMSFirst + 8;
  lxREPLACE   = lxMSFirst + 9;
  lxRULE      = lxMSFirst + 10;
  lxTABLE     = lxMSFirst + 11;
  lxTOP       = lxMSFirst + 12;
  lxTRIGGER   = lxMSFirst + 13;
  lxVIEW      = lxMSFirst + 14;

type
  TMSParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: Char): boolean; override;
    procedure ToRightQuote(LeftQuote: Char); override; // Search right quote of quoted string value or quoted identifier

    procedure InitParser; override;
  public
    function IsSelectModifier(Code: integer): boolean; override;
  end;

implementation

uses
  Classes, SysUtils;

var
  MSKeywordLexems: TLexemList;
  MSClauses: TClauseList;

{ TMSParser }

procedure TMSParser.InitParser;
begin
  inherited;

  FKeywordLexems := MSKeywordLexems;
  FClauses := MSClauses;

  CommentBegin := '/*';
  CommentEnd := '*/';
end;

function TMSParser.IsIdentQuote(Ch: Char): boolean;
begin
  case Ch of
    '"', '[', ']':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TMSParser.ToRightQuote(LeftQuote: Char);
begin
  if LeftQuote = '[' then
    inherited ToRightQuote(']')
  else
    inherited;
end;

function TMSParser.IsSelectModifier(Code: integer): boolean;
begin
  case Code of
    lxTOP: begin
      Code := GetNextToken;
      if Code <> lcNumber then
        Back;

      Result := True;
    end;
  else
    Result := inherited IsSelectModifier(Code);
  end;
end;

initialization
  MSKeywordLexems := TLexemList.Create;
  MSKeywordLexems.Assign(SQLKeywordLexems);

  MSKeywordLexems.Add('ALTER', lxALTER);
  MSKeywordLexems.Add('CALL', lxCALL);
  MSKeywordLexems.Add('COMPUTE', lxCOMPUTE);
  MSKeywordLexems.Add('CREATE', lxCREATE);
  MSKeywordLexems.Add('CROSS', lxCROSS);
  MSKeywordLexems.Add('DECLARE', lxDECLARE);
  MSKeywordLexems.Add('DEFAULT', lxDEFAULT);
  MSKeywordLexems.Add('EXEC', lxEXEC);
  MSKeywordLexems.Add('FUNCTION', lxFUNCTION);
  MSKeywordLexems.Add('GO', lxGO);
  MSKeywordLexems.Add('OPTION', lxOPTION);
  MSKeywordLexems.Add('PROC', lxPROC);
  MSKeywordLexems.Add('PROCEDURE', lxPROCEDURE);
  MSKeywordLexems.Add('REPLACE', lxREPLACE);
  MSKeywordLexems.Add('RULE', lxRULE);
  MSKeywordLexems.Add('TABLE', lxTABLE);
  MSKeywordLexems.Add('TOP', lxTOP);
  MSKeywordLexems.Add('TRIGGER', lxTRIGGER);
  MSKeywordLexems.Add('VIEW', lxVIEW);
  MSKeywordLexems.Sort;

(*
SELECT statement ::=
    < query_expression >
    [ ORDER BY { order_by_expression | column_position [ ASC | DESC ] }
        [ ,...n ]    ]
    [ COMPUTE
        { { AVG | COUNT | MAX | MIN | SUM } ( expression ) } [ ,...n ]
        [ BY expression [ ,...n ] ]
    ]
    [ FOR { BROWSE | XML { RAW | AUTO | EXPLICIT }
            [ , XMLDATA ]
            [ , ELEMENTS ]
            [ , BINARY base64 ]
        }
]
    [ OPTION ( < query_hint > [ ,...n ]) ]

< query expression > ::=
    { < query specification > | ( < query expression > ) }
    [ UNION [ ALL ] < query specification | ( < query expression > ) [...n ] ]

< query specification > ::=
    SELECT [ ALL | DISTINCT ]
        [ { TOP integer | TOP integer PERCENT } [ WITH TIES ] ]
        < select_list >
    [ INTO new_table ]
    [ FROM { < table_source > } [ ,...n ] ]
    [ WHERE < search_condition > ]
    [ GROUP BY [ ALL ] group_by_expression [ ,...n ]
        [ WITH { CUBE | ROLLUP } ]
    ]
    [ HAVING < search_condition > ]

*)

  MSClauses := TClauseList.Create;
  MSClauses.Add(lxWHERE);
  MSClauses.Add(lxGROUP);
  MSClauses.Add(lxHAVING);
  MSClauses.Add(lxORDER);
  MSClauses.Add(lxCOMPUTE);
  MSClauses.Add(lxFOR);
  MSClauses.Add(lxOPTION);

finalization
  MSKeywordLexems.Free;
  MSClauses.Free;

end.
