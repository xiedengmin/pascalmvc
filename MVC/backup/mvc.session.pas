unit MVC.Session;

{$mode ObjFPC}{$H+}
interface

uses
  SysUtils, Classes, Generics.Collections, mvc.HttpServer, mvc.Config,
  mvc.json, MVC.LogUnit,
  fpjson, jsonparser;

type
  TSession = class
  private
    sessionid: string;
  public
    function getValue(key: string): string;
    function getJSON_(key: string): IJObject;
    procedure setValue(key: string; Value: string);
    procedure setJSON_(key: string; Value: IJObject);
    procedure remove(key: string);
    procedure removeAll();
    function getSessionID: string;
    function Text: string;
    constructor Create(Request: TWebRequest; Response: TWebResponse);
    destructor Destroy; override;
  end;

  TSessionPool = class(TThread)
  private
    SessionLs_vlue: specialize TDictionary<string, string>;
    SessionLs_timerout: specialize TDictionary<string, string>;
    procedure clearMap;
  protected
    procedure Execute; override;
  public
    function getValueByKey(sessionid: string): string;
    function getTimeroutByKey(sessionid: string): string;
    function setValueByKey(sessionid: string; Value: string): boolean;
    function setTimeroutByKey(sessionid: string; timerout: string): boolean;
    function deleteSession(key: string): boolean;
    procedure delAllSessioin();
    function editTimerOut(sessionid: string; Value: string): boolean;
    procedure getAllSession(var list: TStringList);
    constructor Create();
    destructor Destroy; override;
  end;

var
  SessionPool: TSessionPool;


implementation

{ TSession }

constructor TSession.Create(Request: TWebRequest; Response: TWebResponse);
var
  timerout: TDateTime;
begin
  if (not Config.session_start) then
    exit;

  timerout := Now + (1 / 24 / 60) * Config.session_timer;
  sessionid := Request.Cookies.Values[Config.sessoin_name];
  if sessionid = '' then
    sessionid := GetGUID();

  with Response.Cookies.Add do
  begin
    Path := '/';
    Name := Config.sessoin_name;
    Value := sessionid;
    Expires := timerout;
  end;
  SessionPool.editTimerOut(sessionid, DateTimeToStr(timerout));
  SessionPool.setTimeroutByKey(sessionid, DateTimeToStr(timerout));

end;

destructor TSession.Destroy;
begin

  inherited;
end;

function TSession.getJSON_(key: string): IJObject;
var
  sessionValue: string;
  json, retjson: IJObject;
  jo: TJSONObject;
begin
  retjson := IIJObject;
  json := IIJObject;

  sessionValue := SessionPool.getValueByKey(sessionid);
  if sessionValue.Trim = '' then
    Result := nil
  else
  begin
    try
      json.ParseJSON(sessionValue);
      jo := TJSONObject(json.O.FindPath(key));
      if jo = nil then
      begin
        Result := nil;
      end
      else
      begin
        retjson.ParseJSON(jo.AsJSON);
        Result := retjson;
      end;
    except
      Result := nil;
    end;
  end;
end;

function TSession.getSessionID: string;
begin
  Result := sessionid;
end;

function TSession.getValue(key: string): string;
var
  sessionValue: string;
  json: IJObject;
begin
  try
    sessionValue := SessionPool.getValueByKey(sessionid);
    if sessionValue.Trim = '' then
      Result := ''
    else
    begin
      json := IIJObject;
      json.ParseJSON(sessionValue);
      Result := json.GetS(key);
    end;
  except
    Result := '';
  end;
end;

procedure TSession.remove(key: string);
var
  sessionValue, s: string;
  json: IJObject;
begin
  sessionValue := SessionPool.getValueByKey(sessionid);
  if sessionValue.Trim <> '' then
  begin
    json := IIJObject(sessionValue);
    s := json.ToJSON;
    SessionPool.setValueByKey(sessionid, s);
  end;
end;

procedure TSession.removeAll;
begin
  SessionPool.setValueByKey(sessionid, '');
end;

procedure TSession.setJSON_(key: string; Value: IJObject);
var
  sessionValue: string;
  json: IJObject;
  jo: TJSONObject;
  outvalue: TJSONObject;
  s: string;
begin
  jo := TJSONObject(GetJSON(Value.ToJSON));
  sessionValue := SessionPool.getValueByKey(sessionid);
  json := IIJObject;
  if sessionValue.Trim <> '' then
    json.ParseJSON(sessionValue);

  json.Remove(key);
  json.O.add(key, jo);

  s := json.ToJSON;
  SessionPool.setValueByKey(sessionid, s);
end;

procedure TSession.setValue(key, Value: string);
var
  sessionValue, s: string;
  jo: TJSONObject;
  outvalue: TJSONString;
begin

  sessionValue := SessionPool.getValueByKey(sessionid);
  if sessionValue.Trim = '' then
    jo := TJSONObject.Create
  else
    jo := TJSONObject(GetJSON(sessionValue));
  try
    if jo.Find(key, outvalue) then
    begin
      if outvalue.Value <> Value then
        jo.Elements[key].Value := Value;
    end
    else
    begin
      jo.add(key, Value);
    end;
    s := jo.AsJSON;
  finally
    jo.Free;
  end;
  SessionPool.setValueByKey(sessionid, s);
end;

function TSession.Text: string;
begin
  Result := SessionPool.getValueByKey(sessionid);
end;


{ TSessionPool }

constructor TSessionPool.Create;
begin
  inherited Create(False);
  SessionLs_vlue := specialize TDictionary<string, string>.Create();
  SessionLs_timerout := specialize TDictionary<string, string>.Create;
end;

procedure TSessionPool.delAllSessioin;
begin
  Lock(SessionLs_vlue);
  Lock(SessionLs_timerout);
  try
    SessionLs_vlue.Clear;
    SessionLs_timerout.Clear;
  finally
    UnLock(SessionLs_vlue);
    UnLock(SessionLs_timerout);
  end;
end;

function TSessionPool.deleteSession(key: string): boolean;
begin
  Lock(SessionLs_vlue);
  Lock(SessionLs_timerout);
  try
    Result := False;
    if SessionLs_timerout.Count > 0 then
    begin
      try
        SessionLs_timerout.Remove(key);
        if SessionLs_vlue.Count > 0 then
        begin
          SessionLs_vlue.Remove(key);
        end;
        Result := True;
      except
        Result := False;
      end;
    end;
  finally
    UnLock(SessionLs_vlue);
    UnLock(SessionLs_timerout);
  end;
end;

destructor TSessionPool.Destroy;
begin
  inherited;
  SessionLs_vlue.Clear;
  SessionLs_timerout.Clear;
  SessionLs_vlue.Free;
  SessionLs_timerout.Free;

end;

function TSessionPool.editTimerOut(sessionid, Value: string): boolean;
begin
  if SessionLs_timerout.ContainsKey(sessionid) then
  begin
    Lock(SessionLs_timerout);
    try

      SessionLs_timerout.AddOrSetValue(sessionid, Value);
    finally
      UnLock(SessionLs_timerout);
    end;
  end;
  Result := True;
end;

procedure TSessionPool.Execute;
var
  k: integer;
begin
  k := 0;
  while not Terminated do
  begin
    Sleep(10);
    Inc(k);
    if k >= 1000 then
    begin
      k := 0;
      clearMap;

    end;

  end;
end;

procedure TSessionPool.getAllSession(var list: TStringList);
var
  key: string;
  i: integer;
  tmp_list: specialize TDictionary<string, string>;
begin
  Lock(SessionLs_timerout);
  tmp_list := specialize TDictionary<string, string>.Create;
  for key in SessionLs_timerout.keys do
  begin
    tmp_list.AddOrSetValue(key, SessionLs_timerout[key]);
  end;
  UnLock(SessionLs_timerout);
  i := 0;
  try
    for key in tmp_list.Keys do
    begin
      list.Add('[' + i.ToString + '] KEY:' + key + ' TimeOut:' + tmp_list[key]);
      Inc(i);
    end;
  finally
    tmp_list.Clear;
    tmp_list.Free;
  end;

end;

function TSessionPool.getTimeroutByKey(sessionid: string): string;
var
  s: string;
begin
  Lock(SessionLs_timerout);
  try
    try
      SessionLs_timerout.TryGetValue(sessionid, s);
      Result := s;
    except
      log('getTimeroutByKey error');
    end;
  finally
    UnLock(SessionLs_timerout);
  end;

end;

function TSessionPool.getValueByKey(sessionid: string): string;
var
  s: string;
begin

  Lock(SessionLs_vlue);
  try
    try

      SessionLs_vlue.TryGetValue(sessionid, s);
    except
      log('getValueByKey error');
    end;
  finally
    UnLock(SessionLs_vlue);
  end;
  Result := s;
end;

function TSessionPool.setTimeroutByKey(sessionid, timerout: string): boolean;
begin
  Lock(SessionLs_timerout);
  try
    try
      SessionLs_timerout.AddOrSetValue(sessionid, timerout);
    except
      log('session error1');
    end;
  finally
    UnLock(SessionLs_timerout);
  end;
  Result := True;
end;

function TSessionPool.setValueByKey(sessionid, Value: string): boolean;
begin
  Lock(SessionLs_vlue);
  try
    try
      SessionLs_vlue.AddOrSetValue(sessionid, Value);
    except
      log('session error2');
    end;
  finally
    UnLock(SessionLs_vlue);
  end;
  Result := True;
end;

procedure TSessionPool.clearMap();
var
  key, s: string;
begin
  try
    for key in SessionLs_timerout.Keys do
    begin
      s := SessionLs_timerout.Items[key];
      if s.Trim <> '' then
      begin
        if Now() >= StrToDateTime(s) then
        begin
          if deleteSession(key) then
          begin
            break;
          end;

        end;
      end;
    end;
  except
    Exit;
  end;
end;

initialization
  sessionPool := TSessionPool.Create();


finalization
  sessionPool.Free;

end.
