unit UserController;

interface

uses
  SysUtils, Classes,MVC.HttpServer, MVC.DataSet, MVC.JSON,MVC.Config, BaseController,
  UserService, RoleService, superobject,libsagui;

type
   TFakeResponse = class
  public
    ResponseAlreadySent: Boolean;
    WasCompressed: Boolean;
    LastStatus: Word;
    ErrorCode: Integer;
    Empty: Boolean;
  end;
  { TUserController }
  TUserController = class(TBaseController)
  private
    roleservice: TRoleService;
    userservice: Tuserservice;
  published
    procedure index;

    procedure getData;
    procedure getrole;
    procedure add;
    procedure edit;
    // [MURL('del/:id', TMethod.sGET)]
    procedure del(id: string);
    procedure save;
    procedure print;
    function Intercept: boolean; override;
  end;

implementation

{ TUserController }

procedure TUserController.add;
begin

  SetAttr('role', roleservice.getData);
end;

procedure TUserController.del(id: string);
var
  idjo: IsuperObject;
begin
  idjo := InputtoJsonso;
  if userservice.del(idjo.GetS('id')) then
    Success(0, '删除成功')
  else
    Fail(-1, '删除失败');
end;

procedure TUserController.edit;
begin
  SetAttr('role', roleservice.getData);
  Show('user/edit');
end;

procedure TUserController.getData;
var
  ds: Idataset;
begin
  ds := userservice.getData(InputToJSON);
  ShowPage(ds);
end;

procedure TUserController.getrole;
begin
  ShowJSON(roleservice.getData);
end;

procedure TUserController.index;
begin
  SetAttr('role', roleservice.getData);
  Show('user/index');
end;

procedure TUserController.print;
var
  nowdate: string;
begin
  SetAttr('list', userservice.getAllData(InputToJSON));
  nowdate := FormatDateTime('yyyy年MM月dd日', Now);
  SetAttr('nowdate', nowdate);
end;

procedure TUserController.save;
begin
  if userservice.save(InputToJSON) then
    Success(0, '保存成功')
  else
    Fail(-1, '保存失败');
end;

function TUserController.Intercept: boolean;
   var
     msg,content:string;
begin
  if Session.getValue('username') = '' then
  begin
     {如果Session中user没有数据返回到登陆界面}


  try


  finally
 //   Response.Free;
  end;
    Result := true;
  end
  else
    Result := false;
end;

initialization
  SetRoute('user', TUserController, 'user');
  //SetRoute('user', TUserController);
end.
