unit RegExprEx;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, RegExpr;

type
  { TRegExprEx }
  TRegExprEx = class(TRegExpr)
  private

  public
    function Matches(InputString, Expression: RegExprString): TStringList;
    function MatchEx(InputString, Expression: RegExprString): RegExprString;
    function Exec(InputString, Expression: RegExprString): boolean; overload;
    function ReplaceExEx(InputString, Expression, Replacement: RegExprString)
      : RegExprString;

  end;

implementation

{ TRegExprEx }

function TRegExprEx.Matches(InputString, Expression: RegExprString): TStringList;
var
  vStringList: TStringList;

begin
  Result := TStringList.Create;
  vStringList := TStringList.Create;
  try
    Self.Expression := Expression;
    Self.InputString := InputString;
    if Self.Exec then
    begin
      repeat
        vStringList.Add(Self.Match[0]);
      until not Self.ExecNext;
      Result.Assign(vStringList);
    end;
  finally
    FreeAndNil(vStringList);
  end;
end;

function TRegExprEx.MatchEx(InputString, Expression: RegExprString): RegExprString;
begin
  Self.Expression := Expression;
  if Self.Exec(InputString) then
    Result := Self.Match[0];
end;

function TRegExprEx.Exec(InputString, Expression: RegExprString): boolean;
begin
  Result := False;
  Self.Expression := Expression;
  Result := Self.Exec(InputString);
end;

function TRegExprEx.ReplaceExEx(InputString, Expression, Replacement:
  RegExprString): RegExprString;
begin
  Self.Expression := Expression;
  Result := Self.replace(InputString, Replacement, True);
end;

end.
