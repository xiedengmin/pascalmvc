unit MVC.LogUnit;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, MVC.Tool, lzxcompressthread;

type
  TLogThread = class(TThread)
  private
    procedure Write(msg: string);
  protected
    procedure Execute; override;
  public
    LogList: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

var
  logThread: TLogThread;


procedure Log(msg: string);

procedure LogDebug(msg: string);

procedure WriteLog(msg: string);

implementation

uses
  MVC.Config;

procedure WriteLog(msg: string);
begin
  Lock();
  logThread.Write(msg);
  UnLock();
end;

procedure log(msg: string);
begin
  if config.open_log then
    logThread.LogList.Add(msg);
end;

procedure LogDebug(msg: string);
begin
  if Config.open_debug then
    logThread.LogList.Add(msg);
end;

{ TLogTh }

constructor TLogThread.Create;
begin
  inherited Create(False);
  LogList := TStringList.Create;
end;

destructor TLogThread.Destroy;
begin
  LogList.Free;
  inherited;
end;

procedure TLogThread.Execute;
var
  k: integer;
begin
  k := 0;
  while not Terminated do
  begin
    Sleep(10);
    Inc(k);
    if k >= 100 then
    begin
      k := 0;
      if LogList.Count > 0 then
      begin
        Lock();
        Write(LogList.Strings[0]);
        LogList.Delete(0);
        UnLock();

      end;
    end;
  end;
end;

procedure TLogThread.Write(msg: string);
var
  log: string;
  logfile: string;
  tf: TextFile;
  fi: THandle;
begin

  try
    log := FormatDateTime('yyyy-MM-dd hh:mm:ss', Now) + '  ' + msg;
    logfile := Config.BasePath + 'Log/';
    //logfile := IITool.PathFmt(logfile);
    if not DirectoryExists(logfile) then
    begin
      CreateDir(logfile);
    end;
    logfile := logfile + 'Log_' + FormatDateTime('yyyyMMdd', Now) + '.txt';

    AssignFile(tf, logfile);
    if FileExists(logfile) then
    begin
      Append(tf);
    end
    else
    begin
      fi := FileCreate(logfile);
      FileClose(fi);
      Rewrite(tf);
    end;
    Writeln(tf, log);
    Flush(tf);
  finally
    CloseFile(tf);
  end;
end;

initialization
  logThread := TLogThread.Create;


finalization
  logThread.Free;

end.
