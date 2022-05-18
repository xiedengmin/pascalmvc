program pascalMVC;

uses
  mvc.app,
  mvc.dbMySql57,
  MVC.TplParser, MVC.Service,
  RegExpr,
  IndexController,
  MainController,
  UserController   ,
  RoleController   ,
  JwtController,
  //mvc.tool,
  UpImageController in '..\Controller\UpImageController.pas';

{$R *.res}
 //  var
 //    i:integer;
begin
   MVCApp.Run();
end.
