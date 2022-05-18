
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  IBC Parser
//////////////////////////////////////////////////


{$IFNDEF CLR}
unit IBCParserUni;
{$ENDIF}

interface
uses
  Classes, CRTypes, CRParser;

var
  IBCKeywordLexems: TLexemList;
  IBCClauses: TClauseList;

const
  lxIBCFirst  = 1000;
  lxALTER     = lxIBCFirst;
  lxAUTODDL   = lxALTER + 1;
  lxBATCH     = lxAUTODDL + 1;
  lxBLOCK     = lxBATCH + 1;
  lxCONNECT   = lxBLOCK + 1;
  lxCREATE    = lxCONNECT + 1;
  lxDATABASE  = lxCREATE + 1;
  lxDIALECT   = lxDATABASE + 1;
  lxDROP      = lxDIALECT + 1;
  lxFUNCTION  = lxDROP + 1;
  lxGENERATOR = lxFUNCTION + 1;
  lxGRANT     = lxGENERATOR + 1;
  lxNAMES     = lxGRANT + 1;
  lxPLAN      = lxNAMES + 1;
  lxPROCEDURE = lxPLAN + 1;
  lxRECONNECT = lxPROCEDURE + 1;
  lxRETAIN    = lxRECONNECT + 1;
  lxREVOKE    = lxRETAIN + 1;
  lxROWS      = lxREVOKE + 1;
  lxSCHEMA    = lxROWS + 1;
  lxSQL       = lxSCHEMA + 1;
  lxSTART     = lxSQL + 1;
  lxTERM      = lxSTART + 1;
  lxTRIGGER   = lxTERM + 1;
  lxTYPE      = lxTRIGGER + 1;
  lxFIRST     = lxTYPE + 1;

type
  TIBCParser = class (TSQLParser)
  protected
    procedure InitParser; override;

  public
  end;

implementation
uses
  SysUtils;

{ TIBCParser }

procedure TIBCParser.InitParser;
begin
  inherited;

  FKeywordLexems := IBCKeywordLexems;
  FClauses := IBCClauses;

  CommentBegin := '/*';
  CommentEnd := '*/';
end;

initialization
  IBCKeywordLexems := TLexemList.Create;
  IBCKeywordLexems.Assign(SQLKeywordLexems);

  IBCKeywordLexems.Add('ALTER',     lxALTER    );
  IBCKeywordLexems.Add('AUTODDL',   lxAUTODDL  );
  IBCKeywordLexems.Add('BATCH',     lxBATCH    );
  IBCKeywordLexems.Add('BLOCK',     lxBLOCK    );
  IBCKeywordLexems.Add('CONNECT',   lxCONNECT  );
  IBCKeywordLexems.Add('CREATE',    lxCREATE   );
  IBCKeywordLexems.Add('DATABASE',  lxDATABASE );
  IBCKeywordLexems.Add('DECLARE',   lxDECLARE  );
  IBCKeywordLexems.Add('DIALECT',   lxDIALECT  );
  IBCKeywordLexems.Add('DROP',      lxDROP     );
  IBCKeywordLexems.Add('FUNCTION',  lxFUNCTION );
  IBCKeywordLexems.Add('GENERATOR', lxGENERATOR);
  IBCKeywordLexems.Add('GRANT',     lxGRANT    );
  IBCKeywordLexems.Add('NAMES',     lxNAMES    );
  IBCKeywordLexems.Add('PLAN',      lxPLAN     );
  IBCKeywordLexems.Add('PROCEDURE', lxPROCEDURE);
  IBCKeywordLexems.Add('RECONNECT', lxRECONNECT);
  IBCKeywordLexems.Add('RETAIN',    lxRETAIN   );
  IBCKeywordLexems.Add('REVOKE',    lxREVOKE   );
  IBCKeywordLexems.Add('ROWS',      lxROWS     );
  IBCKeywordLexems.Add('SCHEMA',    lxSCHEMA   );
  IBCKeywordLexems.Add('SQL',       lxSQL      );
  IBCKeywordLexems.Add('START',     lxSTART    );
  IBCKeywordLexems.Add('TERM',      lxTERM     );
  IBCKeywordLexems.Add('TRIGGER',   lxTRIGGER  );
  IBCKeywordLexems.Add('TYPE',      lxTYPE     );
  IBCKeywordLexems.Add('FIRST',     lxFIRST    );
  IBCKeywordLexems.Sort;

(*  SELECT [TRANSACTION transaction]
    [DISTINCT | ALL]
    {* | val [, val …]}
    [INTO :var [, :var …]]
    FROM tableref [, tableref …]
    [WHERE search_condition]
    [GROUP BY col [COLLATE collation] [, col [COLLATE collation] …]
    [HAVING search_condition]
    [UNION [ALL] select_expr]
    [PLAN plan_expr]
    [ORDER BY order_list]
    [ROWS value [TO upper_value] [BY step_value][PERCENT][WITH TIES]]
    [FOR UPDATE [OF col [, col …]]];*)

  IBCClauses := TClauseList.Create;
  IBCClauses.Add(lxWHERE);
  IBCClauses.Add(lxGROUP);
  IBCClauses.Add(lxHAVING);
  IBCClauses.Add(lxUNION);
  IBCClauses.Add(lxPLAN);
  IBCClauses.Add(lxORDER);
  IBCClauses.Add(lxROWS);
  IBCClauses.Add(lxFOR);

finalization
  IBCKeywordLexems.Free;
  IBCClauses.Free;

end.
