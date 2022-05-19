unit mvc.Query;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, DB, Variants;

type

  { TQuery }

  TQuery = class(TSQLQuery)
  private
    function GetB(V: string): boolean;
    function GetD(V: string): TDateTime;
    function GetF(V: string): double;
    function GetI(V: string): integer;
    function GetS(V: string): string;
    procedure SetB(V: string; AValue: boolean);
    procedure SetD(V: string; AValue: TDateTime);
    procedure SetF(V: string; AValue: double);
    procedure SetI(V: string; AValue: integer);
    procedure SetS(V: string; AValue: string);




  public
    property S[V: string]: string read GetS write SetS;
    property I[V: string]: integer read GetI write SetI;
    property B[V: string]: boolean read GetB write SetB;
    property D[V: string]: TDateTime read GetD write SetD;
    property F[V: string]: double read GetF write SetF;
  end;

implementation

{ TQuery }

function TQuery.GetB(V: string): boolean;
begin
  Result := FieldByName(V).Value;
end;

function TQuery.GetD(V: string): TDateTime;
begin
  Result := FieldByName(V).Value;
end;


function TQuery.GetF(V: string): double;
begin
  Result := FieldByName(V).Value;
end;

function TQuery.GetI(V: string): integer;
begin
  Result := FieldByName(V).Value;
end;

function TQuery.GetS(V: string): string;
begin
  Result := FieldByName(V).Value;
end;


procedure TQuery.SetB(V: string; AValue: boolean);
begin
  FieldByName(V).Value := AValue;
end;

procedure TQuery.SetD(V: string; AValue: TDateTime);
begin
  FieldByName(V).Value := AValue;
end;


procedure TQuery.SetF(V: string; AValue: double);
begin
  FieldByName(V).Value := AValue;
end;

procedure TQuery.SetI(V: string; AValue: integer);
begin
  FieldByName(V).Value := AValue;
end;

procedure TQuery.SetS(V: string; AValue: string);
begin
  FieldByName(V).Value := Utf8ToAnsi(AValue);
end;


end.
