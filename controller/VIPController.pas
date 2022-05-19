unit VIPController;

interface

uses
  System.SysUtils, System.Classes, MVC.JSON, MVC.DataSet, BaseController;

type

  [MURL('VIP', 'VIP')]
  TVIPController = class(TBaseController)
  public
    procedure index;
  end;

implementation

{ TRoleController }

procedure TVIPController.index;
begin
  ShowText('hello');
end;

end.
