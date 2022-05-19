unit MVC.JSON;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fpjson,

  jsonparser;

type
  IJObject = interface
    procedure SetS(key: string; Value: string); overload;
    procedure SetI(key: string; Value: integer); overload;
    procedure SetD(key: string; Value: double); overload;
    procedure SetB(key: string; Value: boolean); overload;
    function GetI(key: string): integer;
    function GetD(key: string): double;
    function GetS(key: string): string;
    function GetB(key: string): boolean;
    procedure Remove(key: string);
    function ParseJSON(Value: string): TJSONObject;
    function O: TJSONObject;
    function toJSON: string;
  end;


  { TJObject }

  TJObject = class(TInterfacedObject, IJObject)
  private
    FJSON: TJSONObject;
  public

    procedure SetS(key: string; Value: string); overload;
    procedure SetI(key: string; Value: integer); overload;
    procedure SetD(key: string; Value: double); overload;
    procedure SetB(key: string; Value: boolean); overload;
    function GetI(key: string): integer;
    function GetD(key: string): double;
    function GetS(key: string): string;
    function GetB(key: string): boolean;
    procedure Remove(key: string);
    function O: TJSONObject;
    function toJSON: string;
    function ParseJSON(Value: string): TJSONObject;
    class function I(json: string = ''): IJObject;
    constructor Create(json: string = '');
    destructor Destroy; override;

  end;

  IJArray = interface
    function A: TJSONArray;
    function toJSON: string;
  end;

  { TJArray }

  TJArray = class(TInterfacedObject, IJArray)
  private
    FJSON: TJSONArray;
  public
    function A: TJSONArray;
    function toJSON: string;

    class function I(json: string = ''): IJArray;
    constructor Create(json: string = '');
    destructor Destroy; override;
  end;

function IIJObject(json: string = ''): IJObject;

function IIJArray(json: string = ''): IJArray;

implementation

{ TJArray }
function IIJObject(json: string): IJObject;
begin
  Result := TJObject.I(json);
end;

function IIJArray(json: string): IJArray;
begin
  Result := TJArray.I(json);
end;

function TJArray.A: TJSONArray;
begin
  Result := FJSON;
end;

function TJArray.toJSON: string;
begin
  Result := FJSON.AsJSON;
end;

class function TJArray.I(json: string): IJArray;
var
  jsonA: IJArray;
begin
  jsonA := TJArray.Create(json);
  Result := jsonA;
end;

constructor TJArray.Create(json: string);
begin

  FJSON := TJSONArray.Create();
  if json.Trim <> '' then
    FJSON := TJSONArray(GetJSON(json));
end;

destructor TJArray.Destroy;
begin

  FJSON.Free;
  inherited Destroy;
end;

{ TJObject }



procedure TJObject.SetS(key: string; Value: string);
var
  outvlaue: TJSONString;
begin

  if FJSON.Find(key, outvlaue) then
  begin
    if outvlaue.Value <> Value then
      FJSON.Elements[key].Value := Value;
  end
  else
    FJSON.Add(key,( Value));

end;

procedure TJObject.SetD(key: string; Value: double);
var
  outvlaue: TJSONNumber;
begin

  if FJSON.Find(key, outvlaue) then
  begin
    if outvlaue.Value <> Value then
      FJSON.Elements[key].Value := Value;
  end
  else
    FJSON.Add(key, Value);

end;


procedure TJObject.SetI(key: string; Value: integer);
var
  outvlaue: TJSONNumber;
begin

  if FJSON.Find(key, outvlaue) then
  begin
    if outvlaue.Value <> Value then
      FJSON.Elements[key].Value := Value;
  end
  else
    FJSON.Add(key, Value);

end;

procedure TJObject.SetB(key: string; Value: boolean);
var
  outvlaue: TJSONBoolean;
begin

  if FJSON.Find(key, outvlaue) then
  begin
    if outvlaue.Value <> Value then
      FJSON.Elements[key].Value := Value;
  end
  else
    FJSON.Add(key, Value);

end;

function TJObject.GetB(key: string): boolean;
var
  outvlaue: TJSONBoolean;
begin
  if FJSON.Find(key, outvlaue) then
    Result := outvlaue.Value
  else
    Result := False;
end;

procedure TJObject.Remove(key: string);
begin
  FJSON.Delete(key);
end;

function TJObject.GetD(key: string): double;
var
  outvlaue: TJSONNumber;
begin
  if FJSON.Find(key, outvlaue) then
    Result := outvlaue.Value
  else
    Result := 0;
end;

function TJObject.GetI(key: string): integer;
var
  outvlaue: TJSONNumber;
begin
  if FJSON.Find(key, outvlaue) then
    Result := outvlaue.Value
  else
    Result := 0;
end;

function TJObject.GetS(key: string): string;
var
  outvlaue: TJSONString;
begin
  if FJSON.Find(key, outvlaue) then
    Result := outvlaue.Value
  else
    Result := '';
end;

function TJObject.O: TJSONObject;
begin
  Result := FJSON;
end;

function TJObject.toJSON: string;
begin
  Result := FJSON.AsJSON;
end;

function TJObject.ParseJSON(Value: string): TJSONObject;
begin
  FJSON := TJSONObject(GetJSON(Value));
  Result := FJSON;
end;

class function TJObject.I(json: string): IJObject;
var
  jsonO: IJObject;
begin
  jsonO := TJObject.Create(json);
  Result := jsonO;
end;

constructor TJObject.Create(json: string);
begin
  if json.Trim <> '' then
  begin
    FJSON := TJSONObject(GetJSON(json));
  end
  else
  begin
    FJSON := TJSONObject.Create();
  end;
end;

destructor TJObject.Destroy;
begin
  FJSON.Free;
  inherited Destroy;
end;

end.
