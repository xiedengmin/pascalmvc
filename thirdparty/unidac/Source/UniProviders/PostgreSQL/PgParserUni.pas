//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I PgDac.inc}
unit PgParserUni;

{$ENDIF}

interface

uses
  Classes,
  CRTypes, CRParser;

const
  lxPgFirst  = 1000;
  lxALTER    = lxPgFirst;
  lxBETWEEN  = lxALTER + 1;
  lxCREATE   = lxBETWEEN + 1;
  lxEXCEPT   = lxCREATE + 1;
  lxILIKE    = lxEXCEPT + 1;
  lxLIKE     = lxILIKE + 1;

type
  TPgParser = class(TSQLParser)
  protected
    procedure InitParser; override;
    function IsAlpha(Ch: char): boolean; override;
    procedure ToRightQuoteP(RightQuote: char); override;
{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
{$IFNDEF PUREPASCAL}
    procedure ToRightQuoteA(RightQuote: AnsiChar); override;
{$ENDIF}
{$ENDIF}
{$ENDIF}
  end;

implementation

var
  PgKeywordLexems: TLexemList;
  PgClauses: TClauseList;

{ TPgParser }

procedure TPgParser.InitParser;
begin
  inherited;

  FKeywordLexems := PgKeywordLexems;
  FClauses := PgClauses;

  CommentBegin := '/*';
  CommentEnd := '*/';
  DollarQuoting := True;
end;

function TPgParser.IsAlpha(Ch: char): boolean;
begin
  case Ord(Ch) of
    Ord('a')..Ord('z'):
      Result := True;
    Ord('A')..Ord('Z'):
      Result := True;
    Ord('_'):
      Result := True;
    Ord('#'):
      Result := True;
    Ord(#128)..Ord(High(char)):
      Result := True;
    else
      Result := False;
  end;
end;

procedure TPgParser.ToRightQuoteP(RightQuote: char);
begin
  // for strings with escape symbols: E'STEPHEN O\'KEEFE'
  if (Pos >= 3) and (Text[Pos - 2] = 'E') then begin
    while Pos <= TextLength do begin
      inherited ToRightQuoteP(RightQuote);
      if (Text[Pos - 1] = '\') and (Text[Pos - 2] <> '\') then
        Inc(Pos)
      else
        break;
    end;
  end
  else
    inherited ToRightQuoteP(RightQuote);
end;

{$IFNDEF CLR}
{$IFNDEF IS_UNICODE}
{$IFNDEF PUREPASCAL}
procedure TPgParser.ToRightQuoteA(RightQuote: AnsiChar);
begin
  // for strings with escape symbols: E'STEPHEN O\'KEEFE'
  if (Pos >= 3) and (Text[Pos - 2] = 'E') then begin
    while Pos <= TextLength do begin
      inherited ToRightQuoteA(RightQuote);
      if (Text[Pos - 1] = '\') and (Text[Pos - 2] <> '\') then
        Inc(Pos)
      else
        break;
    end;
  end
  else
    inherited ToRightQuoteA(RightQuote);
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

initialization
  PgKeywordLexems := TLexemList.Create;
  PgKeywordLexems.Assign(SQLKeywordLexems);

  PgKeywordLexems.Add('ALTER', lxALTER);
  PgKeywordLexems.Add('BETWEEN', lxBETWEEN);
  PgKeywordLexems.Add('CREATE', lxCREATE);
  PgKeywordLexems.Add('EXCEPT', lxEXCEPT);
  PgKeywordLexems.Add('ONLY',   lxONLY);
  PgKeywordLexems.Add('ILIKE', lxILIKE);
  PgKeywordLexems.Add('LIKE', lxLIKE);
  PgKeywordLexems.Sort;

(*
    SELECT [ ALL | DISTINCT [ ON ( expression [, ...] ) ] ]
    * | expression [ AS output_name ] [, ...]
    [ FROM from_item [, ...] ]
    [ WHERE condition ]
    [ GROUP BY expression [, ...] ]
    [ HAVING condition [, ...] ]
    [ { UNION | INTERSECT | EXCEPT } [ ALL ] select ]
    [ ORDER BY expression [ ASC | DESC | USING operator ] [, ...] ]
    [ LIMIT { count | ALL } ]
    [ OFFSET start ]
    [ FOR { UPDATE | SHARE } [ OF table_name [, ...] ] [ NOWAIT ] [...] ]


    where from_item can be one of:

    [ ONLY ] table_name [ * ] [ [ AS ] alias [ ( column_alias [, ...] ) ] ]
    ( select ) [ AS ] alias [ ( column_alias [, ...] ) ]
    function_name ( [ argument [, ...] ] ) [ AS ] alias [ ( column_alias [, ...] | column_definition [, ...] ) ]
    function_name ( [ argument [, ...] ] ) AS ( column_definition [, ...] )
    from_item [ NATURAL ] join_type from_item [ ON join_condition | USING ( join_column [, ...] ) ]
*)

  PgClauses := TClauseList.Create;
  PgClauses.Add(lxWHERE);
  PgClauses.Add(lxGROUP);
  PgClauses.Add(lxHAVING);
  PgClauses.Add(lxUNION);     // UNION/INTERSECT/EXCEPT
  PgClauses.Add(lxINTERSECT); // UNION/INTERSECT/EXCEPT
  PgClauses.Add(lxEXCEPT);    // UNION/INTERSECT/EXCEPT
  PgClauses.Add(lxORDER);
  PgClauses.Add(lxLIMIT);
  PgClauses.Add(lxOFFSET);
  PgClauses.Add(lxFOR);

finalization
  PgKeywordLexems.Free;
  PgClauses.Free;

end.
