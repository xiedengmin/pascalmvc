unit BaseController;

{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, mvc.Route, MVC.controller, mvc.JSON, fpjson, superobject,
  mvc.Session, MVC.DataSet, MVC.TOOL, ServiceMap;

type

  { TBaseController }

  TBaseController = class(TController)
  published
    function getVCode: string;
    function Intercept: boolean; override;
    procedure CreateController; override;
    procedure ShowPage(ds: IDataSet);
    {封装分页参数，可根据自己的需要修改}
  public

    destructor Destroy; override;
  end;

procedure SetRoute(routeUrl: string; Action: TClass; tplPath: string = '');

implementation

{ TBaseController }
function TBaseController.getVCode: string;
var
  code: string;
begin
  Result := IITool.getVCode(code);
  Session.setValue('vcode', code);
end;

procedure SetRoute(routeUrl: string; Action: TClass; tplPath: string);
begin
  Route.SetRoute(routeUrl, Action, tplPath);
end;

function TBaseController.Intercept: boolean;
var
  username: string;
  json: IJObject;
begin
  json := Session.getJSON_('user');
  username := Session.getValue('name');
  if json <> nil then
    Result := False
  else
    Result := True;
end;

procedure TBaseController.ShowPage(ds: IDataSet);
var
  json: string;
  sjson: ISuperObject;
begin
  json := ds.toJSONArray;
  sjson := SO('{}');
  sjson.I['code'] := 0;
  sjson.S['msg'] := '获取成功';
  sjson.I['count'] := ds.Count;
  sjson.O['data'] := so(json);
  ShowJSON(sjson);
end;

procedure TBaseController.CreateController;
begin
  inherited CreateController;

end;

destructor TBaseController.Destroy;
begin

end;

end.
