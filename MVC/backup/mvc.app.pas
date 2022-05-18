unit MVC.App;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF UNIX} cthreads, cmem, {$ENDIF}
  Classes, SysUtils, ExtCtrls, mmsystem, mvc.HttpServer, mvc.Config,
  mvc.DB, mvc.dbMySql57, mvc.dbSQLite;

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
  mysqltimer: Ttimer;

  MMTimerID: integer; // 定时器ID



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

 { procedure TimerProc(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD); stdcall;
begin
   Writeln('Server Start');
  // 业务代码
end;       }
{ TMVC }

procedure TMVCApp.Run;
begin
  mysql := TMySQL57DB.Create('db1');
  sqlite := TSQLiteDB.Create('db2');
  TCmd.Create(False).Start;
  TMVCServer.Start;

end;

procedure TMVCApp.Close;
begin
  TMVCServer.Stop;
end;

initialization
  //MMTimerID := timeSetEvent(1000, 0, TimerProc, 0, TIME_PERIODIC);
  MVCApp := TMVCApp.Create;

finalization
  mysql.Free;
  sqlite.Free;
  MVCApp.Free;
end.
