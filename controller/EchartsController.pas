unit EchartsController;

interface

uses
  System.SysUtils, System.Classes, BaseController, MVC.JSON, System.JSON;

type

  [MURL('echarts', 'echarts')]
  TEchartsController = class(TBaseController)
  public
    procedure index;
    procedure getdata;
  end;

implementation

{ TEchartsController }

procedure TEchartsController.getdata;
var
  year: string;
  jarr: IJArray;
  jo: TJSONObject;
  i: Integer;

begin
  year := Input('year');
  jarr := IIJArray();
  for i := 1 to 12 do
  begin
    jo := TJSONObject.Create;
    jo.AddPair('con', TJSONNumber.Create(10 + Random(500)));
    jo.AddPair('month', i.ToString + 'ÔÂ');

    jarr.A.AddElement(jo);
  end;

  ShowJSON(jarr);

end;

procedure TEchartsController.index;
var
  year: string;
begin
  year := FormatDateTime('yyyy', now);
  SetAttr('year', year);
end;

end.
