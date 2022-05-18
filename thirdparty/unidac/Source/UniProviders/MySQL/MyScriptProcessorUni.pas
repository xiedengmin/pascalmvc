{$IFNDEF CLR}
{$I MyDac.inc}
unit MyScriptProcessorUni;
{$ENDIF}

interface

uses
  Classes, SysUtils, DBAccess, DAScript, CRParser, CRTypes;

type
  TMyScriptProcessor = class (TDAScriptProcessor)
  private
    FFirstCode: integer;
    FPrevCode: integer;
    FLexemNo: integer;
    FBeginCount: integer;
    FOldUseUnicode: boolean;
    FOldisUTF8: boolean;
  protected
    function GetParserClass: TSQLParserClass; override;
    procedure Reset; override;
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
    function IsSpecificSQL(StatementType: integer): boolean; override;
    function CanOptimize(const SQL: string; const StatementType: integer): boolean; override;
    function IsBlankEndsDelimeter: boolean; override;
    procedure DoBeforeStatementExecute(var SQL: string; StatementType: integer; var Omit: boolean); override;
    procedure DoAfterStatementExecute(var SQL: string; StatementType: integer); override;
  end;

implementation

uses
  CLRClasses,
{$IFNDEF UNIDACPRO}
  MyParser, MyClasses;
{$ELSE}
  MyParserUni, MyClassesUni;
{$ENDIF}

{ TMyScriptProcessor }

function TMyScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TMyParser;
end;

procedure TMyScriptProcessor.DoBeforeStatementExecute(var SQL: string; StatementType: integer; var Omit: boolean);
var
  IConnection: TMySQLConnection;
begin
  inherited;

  if not Omit and (FParser <> nil) and (FParser.GetStream <> nil) then begin
    IConnection := TDBAccessUtils.GetIConnection(UsedConnection) as TMySQLConnection;
    if IConnection <> nil then begin
      FOldUseUnicode := IConnection.UseUnicode;
      FOldisUTF8 := IConnection.IsUtf8;
      IConnection.UseUnicode := FParser.Encoding <> Encoding.Default;
      IConnection.IsUtf8 := FParser.Encoding <> Encoding.Default;
    end;
  end;
end;

procedure TMyScriptProcessor.DoAfterStatementExecute(var SQL: string; StatementType: integer);
var
  IConnection: TMySQLConnection;
begin
  inherited;

  if (FParser <> nil) and (FParser.GetStream <> nil) then begin
    IConnection := TDBAccessUtils.GetIConnection(UsedConnection) as TMySQLConnection;
    IConnection.UseUnicode := FOldUseUnicode;
    IConnection.IsUtf8 := FOldUseUnicode;
    FOldUseUnicode := False;
    FOldisUTF8 := False;
  end;
end;

procedure TMyScriptProcessor.Reset;
begin
  inherited;

  FLexemNo := 1;
  FBeginCount := 0;
  FPrevCode := 0;
end;

procedure TMyScriptProcessor.CheckLexem(Code: integer; var StatementType: integer; var Omit: boolean);
begin
  if (Code <> lcBlank) and (Code <> lcComment) then begin
    // Set statement types
    if (Code = lxDELIMITER) and (StatementType <> ST_SPECIFIC_SQL) then begin
      FDelimiterState := dsDelimiter;
      StatementType := ST_DELIMETER;
    end;

    case FLexemNo of
      1: begin
        FFirstCode := Code;
        Inc(FLexemNo);
      end;
      2: begin
        if (FFirstCode = lxCREATE) or (FFirstCode = lxALTER) then begin
          if Code >= lxMyFirst then
            Inc(FLexemNo);

          if (Code = lxPROCEDURE) or (Code = lxFUNCTION) or (Code = lxTRIGGER) then
            StatementType := ST_SPECIFIC_SQL;
        end
        else
          Inc(FLexemNo);
      end;
      else begin
        if StatementType = ST_SPECIFIC_SQL then begin
          case Code of
            lxBEGIN:
              Inc(FBeginCount);
          end;

          if (FPrevCode = lxEND) and (Code = lxSemicolon) then
            Dec(FBeginCount);
          if (Code <> lcSymbol) and not FParser.IsSymbolCode(Code) then
            FPrevCode := Code;
        end;
      end;
    end;
  end;
end;

function TMyScriptProcessor.IsSpecificSQL(StatementType: integer): boolean;
begin
  if FPrevCode = lxEND then
    Dec(FBeginCount);

  Result := (StatementType = ST_SPECIFIC_SQL) and (FBeginCount > 0);
end;

function TMyScriptProcessor.CanOptimize(const SQL: string; const StatementType: integer): boolean;
var
  IConnection: TMySQLConnection;
begin
  IConnection := TDBAccessUtils.GetIConnection(UsedConnection) as TMySQLConnection;
  Result := IConnection.IsClient41 and IConnection.IsServer41 and inherited CanOptimize(SQL, StatementType);
end;

function TMyScriptProcessor.IsBlankEndsDelimeter: boolean;
begin
  Result := True;
end;

end.
