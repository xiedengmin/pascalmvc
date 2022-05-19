unit RoleController;

interface

uses
  SysUtils,Classes, MVC.JSON, MVC.DataSet, BaseController;

type

  //[MURL('Role', 'Role')]
  TRoleController = class(TBaseController)
  public
    procedure index;
  end;

implementation

{ TRoleController }

procedure TRoleController.index;
begin
  ShowText('hello');
end;
initialization
  SetRoute('role', TRoleController, 'role');
end.
