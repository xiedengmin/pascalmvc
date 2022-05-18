{$IFNDEF CLR}

{$I AccessDac.inc}
unit AccessParserUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, CRParser, CRTypes;

const
  lxAccessFirst  = 1000;
  lxTOP         = lxAccessFirst;
  lxPERCENT     = lxTOP + 1;
  lxDISTINCTROW = lxPERCENT + 1;

type
  TAccessParser = class (TSQLParser)
  protected
    function IsIdentQuote(Ch: char): boolean; override;
    procedure ToRightQuote(LeftQuote: char); override;
    function IsAlpha(Ch: char): boolean; override;
    function IsStringQuote(Ch: char): boolean; override;
    procedure InitParser; override;
  public
    function IsSelectModifier(Code: integer): boolean; override;
  end;

implementation

var
  AccessKeywordLexems: TLexemList;

function TAccessParser.IsIdentQuote(Ch: char): boolean;
begin
  case Ch of
    '"', '[', ']','`':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TAccessParser.ToRightQuote(LeftQuote: char);
begin
  if LeftQuote = '[' then
    inherited ToRightQuote(']')
  else
    inherited;
end;

function TAccessParser.IsAlpha(Ch: char): boolean;
var
  c: char;
  p: integer;
begin
  Result := (Ch <> '#') and Inherited IsAlpha(Ch);
  if not Result and IsNumber(Ch) then begin
    p := Pos;
    repeat
      inc(p);
      c := Text[p];
    until not IsNumber(c);
    if IsAlpha(c) or (c = '_') then
      Result := True;
  end;
end;

function TAccessParser.IsStringQuote(Ch: char): boolean;
begin
  Result := (Ch = '#') or inherited IsStringQuote(Ch);
end;

procedure TAccessParser.InitParser;
begin
  inherited;

  FKeywordLexems := AccessKeywordLexems;
end;

function TAccessParser.IsSelectModifier(Code: integer): boolean;
var
  OldPos: integer;
begin
  case Code of
    lxTOP: begin
      ToLexem(lcNumber);
      OldPos := Pos;
      if ToLexem(lxPERCENT) <> lxPERCENT then
        Pos := OldPos;
      Result := True;
    end;
    else
      Result := inherited IsSelectModifier(Code) or (Code = lxDISTINCTROW)
  end;
end;

initialization
  AccessKeywordLexems := TLexemList.Create;
  AccessKeywordLexems.Assign(SQLKeywordLexems);

  AccessKeywordLexems.Add('TOP', lxTOP);
  AccessKeywordLexems.Add('PERCENT', lxPERCENT);
  AccessKeywordLexems.Add('DISTINCTROW', lxDISTINCTROW);

  AccessKeywordLexems.Sort;

finalization
  AccessKeywordLexems.Free;

end.
