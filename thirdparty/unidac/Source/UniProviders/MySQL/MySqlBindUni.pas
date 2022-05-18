
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I MyDac.inc}
unit MySqlBindUni;
{$ENDIF}

interface

uses
  CRTypes;

type
  TMySqlBind = class
  public
    IsNull: boolean;
    Offset: integer;
    Length: integer;
  end;

  TMySqlBinds = class
  public
    Items: array of TMySqlBind;
    Buffer: TValueArr;

    constructor Create(Len: integer);
    destructor Destroy; override;
  end;

implementation

{ TMySqlBinds }

constructor TMySqlBinds.Create(Len: integer);
var
  i: integer;
begin
  inherited Create;

  SetLength(Items, Len);
  for i := 0 to Length(Items) - 1 do
    Items[i] := TMySqlBind.Create;
end;

destructor TMySqlBinds.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(Items) - 1 do
    Items[i].Free;

  inherited;
end;

end.
