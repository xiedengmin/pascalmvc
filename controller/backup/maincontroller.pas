unit MainController;

{$mode ObjFPC}{$H+}
{$M+}
interface

uses
  Classes, SysUtils, BaseController,MVC.DataSet;

type

  { TMainController }

  TMainController = class(TBaseController)
    private
        mainservice:Tmainservice;
  published
    procedure index;
    procedure main;
     procedure menu;
    function Intercept: boolean; override;
  end;

implementation
  uses
     MainService;
{ TMainController }

procedure TMainController.index;
var
  ds: Idataset;
  mainservice:Tmainservice;
begin

  SetAttr('realname', Session.getValue('username'));
  ds := mainservice.getmenu;
  SetAttr('menuls', ds);
  Show('main/index');

end;

procedure TMainController.main;
var
  sess: string;
begin
  // ShowText('main');

  sess := Session.getValue('name');
  SetAttr('message', sess);
  Show('main');
end;
procedure TMainController.menu;
var
  ds: Idataset;

begin
  ds := mainservice.getmenu;
  ShowJSON(ds);
end;

function TMainController.Intercept: boolean;
begin
  Result := False;
end;

initialization
  SetRoute('main', TMainController, 'main');
end.
