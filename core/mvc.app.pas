unit MVC.App;

{$mode ObjFPC}{$H+}
interface
uses
{$I mormot.uses.inc}
{$IFDEF UNIX}
//cthreads,
cmem,
{$ENDIF}
  Classes, SysUtils, ExtCtrls, superobject, mvc.HttpServer, mvc.Config,
  mvc.DB, mvc.dbMySql57, mvc.dbSQLite, MVCModel,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.os,
mormot.core.log,
  mormot.DB.sql.zeos,
  mormot.DB.raw.sqlite3,
  mormot.DB.raw.sqlite3.static,
  mormot.orm.core,
  mormot.orm.sql,
  mormot.rest.http.server,
  mormot.rest.sqlite3,
mormot.rest.core,

  zdbc;

type

  TCmd = class(TThread)
  private
    function Command: boolean;
  protected
    procedure Execute; override;
  public
  end;

  TMVCApp = class
  public
    procedure Run();
    procedure Close();
  end;

var
  MVCApp: TMVCApp;
  sqlite: TSQLiteDB;  {链接sqlite数据库}
  mysql: TMySQL57DB;  {链接mysql数据库}
  aModel: TOrmModel;
  myzeos: TSqlDBZeosConnectionProperties;
  Dbserver: TRestServerDB;
  aRestModel: TRest;

implementation

{ TCmd }

function TCmd.Command: boolean;
var
  LResponse: string;
begin
  Writeln('input ''q'' Close Server');
  readln(LResponse);
  if LResponse.ToLower = 'q' then
    Result := False
  else
    Result := True;
end;

procedure TCmd.Execute;
begin

  Writeln('Server Start');
  Writeln('http://localhost:' + Config.Port);
  while True do
  begin
    if not Command then
      Break;
  end;
  MVCApp.Close();
end;



procedure TMVCApp.Run;
var
  dbconfig, db1: isuperobject;
  dbstr: string;
begin
  mysql := TMySQL57DB.Create('db1');
  sqlite := TSQLiteDB.Create('db2');
  aModel := CreateModel;
  dbconfig := so(Config.DBConfig);
  db1 := dbconfig.GetO('db1');
  try
    {$IFDEF UNIX}
    myzeos := TSQLDBZEOSConnectionProperties.Create(
      'zdbc:mysql://127.0.0.1:3306/pascalmvc?username='+db1.GetS('User_Name')+';' +
      'password='+db1.GetS('Password')+';LibLocation=./libmysqlclient.so.20', '', '', '');
    {$ELSE}
    myzeos := TSQLDBZEOSConnectionProperties.Create(
      'zdbc:mysql://127.0.0.1:3306/pascalmvc?username=' + db1.GetS('User_Name') + ';' +
      // 'password=Xdm96600;LibLocation=e:\soft\mysql-5.7.31-win32\lib\libmySQL.dll',
      'password=' + db1.GetS('Password') +
      ';LibLocation=e:\soft\mysql-5.7.31-winx64\lib\libmySQL.dll', '', '', '');
    {$ENDIF}
    myzeos.ExecuteInlined('SET AUTOCOMMIT=0', False);
    try
      VirtualTableExternalRegisterAll(aModel, myzeos, [regMapAutoKeywordFields]);
      Dbserver := TRestServerDB.Create(aModel, False);
      Dbserver.Server.CreateMissingTables;
    finally

    end;

  finally
  end;
  TCmd.Create(False).Start;
  TMVCServer.Start;

end;

procedure TMVCApp.Close;
begin
  TMVCServer.Stop;
end;

initialization
  MVCApp := TMVCApp.Create;

finalization
  mysql.Free;
  sqlite.Free;
  MVCApp.Free;
  Dbserver.Free;
  myzeos.Free;
end.
