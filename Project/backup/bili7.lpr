program bili7;

uses
  RegExpr,
  IndexController,
  MainController,
  UserController   ,
  RoleController   ,
  JwtController,
  mvc.app,
  UpImageController in '..\Controller\UpImageController.pas', MVCModel;
{$I mormot.defines.inc}

{$ifdef OSWINDOWS}
  {$apptype console}
  {$R mormot.win.default.manifest.res}
{$endif OSWINDOWS}

{$R *.res}
 //  var
 //    i:integer;
begin
   MVCApp.Run();
end.
