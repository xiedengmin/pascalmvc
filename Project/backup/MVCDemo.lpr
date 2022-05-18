program MVCDemo;

uses
  mvc.app,
  mvc.dbMySql57,
  IndexController,
  MainController;

{$R *.res}

begin
  MVCApp.Run();
end.
          