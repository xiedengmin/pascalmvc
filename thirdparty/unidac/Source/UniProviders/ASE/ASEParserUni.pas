
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEParserUni;

interface

uses
  Classes, CRTypes, CRParser;

const
  lxASEFirst = 1000;
  lxALTER    = lxASEFirst;
  lxASC      = lxASEFirst + 1;
  lxGO       = lxASEFirst + 2;
  lxTOP      = lxASEFirst + 3;

type
  TASEParser = class(TSQLParser)
  protected
    procedure InitParser; override;
  end;

implementation

var
  ASEKeywordLexems: TLexemList;

{ TASEParser }

procedure TASEParser.InitParser;
begin
  inherited;

  FKeywordLexems := ASEKeywordLexems;
end;

initialization
  ASEKeywordLexems := TLexemList.Create;
  ASEKeywordLexems.Assign(SQLKeywordLexems);

  ASEKeywordLexems.Add('ALTER', lxALTER);
  ASEKeywordLexems.Add('ASC', lxASC);
  ASEKeywordLexems.Add('CALL', lxCALL);
  ASEKeywordLexems.Add('CREATE', lxCREATE);
  ASEKeywordLexems.Add('DECLARE', lxDECLARE);
  ASEKeywordLexems.Add('EXEC', lxEXEC);
  ASEKeywordLexems.Add('GO', lxGO);
  ASEKeywordLexems.Add('OUT', lxOUT);
  ASEKeywordLexems.Add('OUTPUT', lxOUTPUT);
  ASEKeywordLexems.Add('TOP', lxTOP);
  ASEKeywordLexems.Sort;

finalization
  ASEKeywordLexems.Free;

end.
