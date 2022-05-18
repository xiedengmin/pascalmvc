
//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Sdac.inc}

unit MSScriptProcessorUni;

{$ENDIF}

interface

uses
  Classes, SysUtils, DAScript, CRParser, CRTypes;

type
  TMSScriptProcessor = class(TDAScriptProcessor)
  private
    FFirstCode: integer;
    FLexemNo: integer;
    FLastSpecificLexem: integer;
    FBeginCount: integer;
  protected
    function GetParserClass: TSQLParserClass; override;
    procedure Reset; override;
    procedure CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean); override;
    function IsSpecificSQL(StatementType: integer): boolean; override;
    function GetReady(Code: integer): boolean; override;
    function CanOptimize(const SQL: string; const StatementType: integer): boolean; override;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  MSParser;
{$ELSE}
  MSParserUni;
{$ENDIF}

{ TMSScriptProcessor }

function TMSScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TMSParser;
end;

procedure TMSScriptProcessor.Reset;
begin
  inherited;

  FLexemNo := 1;
  FLastSpecificLexem := 0;
  FBeginCount := 0;
end;

procedure TMSScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
begin
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    if (FCurrDelimiter = ';') and ((Code = lxBEGIN) or (Code = lxDECLARE) or (Code = lxAS)) then
      StatementType := ST_SPECIFIC_SQL;

    case FLexemNo of
      1: begin
        FFirstCode := Code;
        Inc(FLexemNo);
      end;
      2: begin
        if (FFirstCode = lxCREATE) or (FFirstCode = lxALTER) then begin
          if Code >= lxMSFirst then
            Inc(FLexemNo);

          if (FCurrDelimiter = ';') and
            ((Code = lxPROC) or (Code = lxPROCEDURE) or (Code = lxFUNCTION) or (Code = lxTRIGGER)) then
            StatementType := ST_SPECIFIC_SQL;
        end
        else
          Inc(FLexemNo);
      end;
      else begin
        if StatementType = ST_SPECIFIC_SQL then
          case Code of
            lxAS: begin
              if FLastSpecificLexem = 0 then begin
                Inc(FBeginCount);
                FLastSpecificLexem := Code;
              end;
            end;
            lxBEGIN: begin
              if FLastSpecificLexem <> lxAS then
                Inc(FBeginCount);
              FLastSpecificLexem := Code;
            end;
            lxCASE:
              Inc(FBeginCount);
            lxEND:
              Dec(FBeginCount);
          end;
      end;
    end;
  end;
end;

function TMSScriptProcessor.IsSpecificSQL(StatementType: integer): boolean;
begin
  Result := (StatementType = ST_SPECIFIC_SQL) and (FBeginCount > 0);
end;

function TMSScriptProcessor.GetReady(Code: integer): boolean;
begin
  Result := Code = lxGO;
end;

function TMSScriptProcessor.CanOptimize(const SQL: string; const StatementType: integer): boolean;
begin
  Result := (StatementType <> ST_SPECIFIC_SQL) and inherited CanOptimize(SQL, StatementType);
end;

end.
