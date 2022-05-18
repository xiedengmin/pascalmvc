unit MVC.Route;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, DateUtils, Rtti, mvc.HttpServer,
  mvc.Config, MVC.LogUnit;

type
  TActionMethod = procedure of object;
  TInterceptMethod = function: boolean of object;
  { TRouteItem }

  TRouteItem = class
  private
    FAction: TClass;
    FrouteUrl: string;
    FtplPath: string;
    procedure SetAction(AValue: TClass);
    procedure SetrouteUrl(AValue: string);
    procedure SettplPath(AValue: string);
  public
    ActoinClass: TRttiType;
    CreateController, Intercept, Show: TRttiMethod;
    Response, Request, routeUrl_pro, tplPath_pro: TRttiProperty;
    property routeUrl: string read FrouteUrl write SetrouteUrl;
    property Action: TClass read FAction write SetAction;
    property tplPath: string read FtplPath write SettplPath;
  end;

  TRoute = class
  private
    _RTTIContext: TRttiContext;
    list: specialize TObjectList<TRouteItem>;
    listkey: TStringList;
    function DateTimeToGMT(const ADate: TDateTime): string;
    function finditem(Name: string): boolean;
    function GetItem(Route: string): TRouteItem;
    function getMimeType(extension: string): string;
    function MethodInvoke(action: TObject; Name: string; out msg: string): boolean;
    function InterceptInvoke(action: TObject; out msg: string): integer;
  public

    procedure loadAssets(_Request: TWebRequest; _Response: TWebResponse;
      url: string);
    function GetRoute(url: string; var Route: string;
      var method: string): TRouteItem;

    procedure SetRoute(routeUrl: string; Action: TClass; tplPath: string);
    procedure OpenRoute(_Request: TWebRequest; _Response: TWebResponse);
    procedure Error500(Response: TWebResponse; msg: string = '');
    procedure Error404(Response: TWebResponse; msg: string);
    procedure Error302(Response: TWebResponse; msg: string);
    constructor Create;
    destructor Destroy; override;
  end;

var
  Route: TRoute;

implementation

{ TRoute }
function DescCompareStrings(sList: TStringList; Index1, Index2: integer): integer;
begin
  Result := -AnsiCompareText(sList[Index1], sList[Index2]);
end;

procedure TRoute.SetRoute(routeUrl: string; Action: TClass; tplPath: string);
var
  item: TRouteItem;
begin
  if (routeUrl.Trim <> '') and (routeUrl.Trim <> '/') then
    routeUrl := '/' + routeUrl + '/'
  else
    routeUrl := '/';
  if (tplPath.Trim <> '') and (tplPath.Trim <> '/') then
    tplPath := '/' + tplPath + '/'
  else
    tplPath := '/';
  if not finditem(routeUrl) then
  begin
    item := TRouteItem.Create;
    item.Action := Action;
    item.tplPath := tplPath;
    item.routeUrl := routeUrl;
    list.Add(item);
    with item do
    begin
      item.ActoinClass := _RTTIContext.GetType(item.Action);
      Request := ActoinClass.GetProperty('Request');
      Response := ActoinClass.GetProperty('Response');

    end;
    listkey.Add(routeUrl);
    listkey.CustomSort(@DescCompareStrings);

  end
  else
  begin
    item := GetItem(routeUrl);
  end;

end;

procedure TRoute.OpenRoute(_Request: TWebRequest; _Response: TWebResponse);
var
  Action: TObject;
  item: TRouteItem;
  InterceptRet: boolean;
  InterceptRetState: integer;
  httpMethod, errmsg: string;
  urlt, url, RoutePath, method_name, assetsfile: string;
  k: integer;
  Response, Request: TRttiProperty;

begin
  InterceptRet := False;
  // _Response.ContentEncoding := Config.document_charset;
  //  _Response.Date := DateTimeToStr(Now);
  httpMethod := _Request.Method;
  url := _Request.Path;
  // if url.IsEmpty then url := '/';
  k := Pos('.', url);
  if k <= 0 then
  begin
    item := Route.GetRoute(url, RoutePath, method_name);
    if item <> nil then
    begin
      Action := item.Action.Create;
      try
        try
          Request := item.Request;
          Response := item.Response;
          Request.SetValue(Action, _Request);
          Response.SetValue(Action, _Response);
          ///下面是控制器创建方法
          if not MethodInvoke(Action, 'CreateController', errmsg) then
          begin
            Route.Error500(_Response, errmsg);
            exit;
          end;
          /////下面进行拦截处理
          InterceptRetState := InterceptInvoke(Action, errmsg);
          if InterceptRetState = 0 then
            InterceptRet := False
          else if InterceptRetState > 1 then
          begin
            Route.Error500(_Response, errmsg);
            exit;
          end
          else if InterceptRetState = 1 then
          begin
            Route.Error302(_Response, url);
            exit;
          end;
          /////下面开始执行具体方法
          if not MethodInvoke(Action, method_name, errmsg) then
          begin
            Route.Error404(_Response, url);
            exit;
          end;
        except
          on e: Exception do
          begin
            errmsg := method_name + '方法执行异常:' + e.Message;
            Log(errmsg);
            Route.Error500(_Response, errmsg);
          end;

        end;
      finally
        //     _Response.Free;
        Action.Free;
      end;
    end
    else
    begin
      Route.Error404(_Response, url);
    end;
  end
  else
  begin
    Route.loadAssets(_Request, _Response, url); //加载资源文件
  end;
end;

procedure TRoute.Error500(Response: TWebResponse; msg: string);
var
  Content, tplFile: string;
begin
  if msg = '' then
    msg := '系统异常,请查看日志';
  Content := '<html><body><div style="text-align: left;">';
  Content := Content + '<div><h1>Error 500</h1></div>';
  Content := Content + '<hr><div>[ ' + msg + ' ]' + '</div></div></body></html>';
  Response.send(Content, 'text/html; charset=' + Config.document_charset, 500);

end;

procedure TRoute.Error404(Response: TWebResponse; msg: string);
var
  Content, tplFile: string;
begin
  if msg = '' then
    msg := '系统异常,请查看日志';
  Content := '<html><body><div style="text-align: left;">';
  Content := Content + '<div><h1>Error 404</h1></div>';
  Content := Content + '<hr><div>[ ' + msg + ' ] Not Find Page' +
    '</div></div></body></html>';
  Response.send(Content, 'text/html; charset=' + Config.document_charset, 404);
end;
procedure TRoute.Error302(Response: TWebResponse; msg: string);
var
  Content, tplFile: string;
begin
  if msg = '' then
    msg := '系统异常,请查看日志';
  Content := '<html><body><div style="text-align: left;">';
  Content := Content + '<div><h1>Error 404</h1></div>';
  Content := Content + '<hr><div>[ ' + msg + ' ] Not Find Page' +
    '</div></div></body></html>';
//  Response.send(Content, 'text/html; charset=' + Config.document_charset, 302);

Response.Headers['Location'].IsEmpty;
   Response.Headers['Location'] := '/login'  ;
       Response.SendAndRedirect('/login', '/login', 'text/html; charset=' + Config.document_charset, 302);

 //  Response.Clear;
 {          msg := '系统异常,请查看日志';
  Content := '<html><body><div style="text-align: left;">';
  Content := Content + '<div><h1>Error 404</h1></div>';
  Content := Content + '<hr><div>[ ' + msg + ' ] Not Find Page' +
    '</div></div></body></html>';
  Response.send(Content, 'text/html; charset=' + Config.document_charset,302);      }

end;

constructor TRoute.Create;
begin
  list := specialize TObjectList<TRouteItem>.Create;
  listkey := TStringList.Create;
end;

destructor TRoute.Destroy;
begin
  listkey.Clear;
  listkey.Free;
  list.Clear;
  list.Free;
  inherited Destroy;
end;

function TRoute.GetItem(Route: string): TRouteItem;
var
  I, j: integer;
  item, defitem: TRouteItem;
  key: string;
  isFind: boolean;
begin
  Result := nil;
  defitem := nil;
  isFind := False;
  for j := 0 to listkey.Count - 1 do
  begin
    key := listkey.Strings[j];
    if (UpperCase(key) = LeftStr(UpperCase(Route), Length(key))) or
      (UpperCase(key) = LeftStr(UpperCase(Route + '/'), Length(key))) then
    begin
      isFind := True;
      break;
    end;
  end;
  if isFind then
  begin
    for I := 0 to list.Count - 1 do
    begin
      item := list.Items[I];
      if ((key = item.routeUrl) or ((key + '/') = item.routeUrl)) and
        (Length(item.routeUrl) > 1) then
      begin
        Result := item;
        break;
      end
      else if ((key = item.routeUrl) or ((key + '/') = item.routeUrl)) and
        (item.routeUrl = '/') then
      begin
        defitem := item;
      end;
    end;
  end;
  if Result = nil then
    Result := defitem;
end;

function TRoute.GetRoute(url: string; var Route: string;
  var method: string): TRouteItem;
var
  item: TRouteItem;
  url1: string;
  tmp: string;
begin
  if url.IsEmpty then
    url := '';
  url1 := Trim(url);
  item := GetItem(url1);
  if item <> nil then
  begin
    Route := url1;
    tmp := Copy(Route, Length(item.routeUrl) + 1, Length(Route) - Length(item.routeUrl));
    method := Copy(tmp, 1, Pos('/', tmp) - 1);
    if method = '' then
    begin
      if tmp <> '' then
        method := tmp
      else
        method := 'index';
    end;
  end;
  Result := item;
end;

function TRoute.finditem(Name: string): boolean;
var
  j: integer;
  key: string;
begin
  Result := False;
  for j := 0 to listkey.Count - 1 do
  begin
    key := listkey.Strings[j];
    if UpperCase(key) = UpperCase(Name) then
    begin
      Result := True;
      break;
    end;
  end;
end;

function TRoute.getMimeType(extension: string): string;
var
  MimeType, key: string;
begin
 // Lock();
  try
    Result := '';
    for key in Config.MIME.Keys do
    begin
      if (Pos(extension, key) > 0) and (Config.MIME.TryGetValue(key, MimeType)) then
      begin
        Result := MimeType + '; charset=' + Config.document_charset;
        Break;
      end;
    end;
  finally
 //  UnLock();
  end;
end;

function TRoute.MethodInvoke(action: TObject; Name: string; out msg: string): boolean;
var
  ActionMethod: TActionMethod;
  ActionMethodOp: TMethod;
begin
  // try
  ActionMethodOp.Data := Pointer(action);
  ActionMethodOp.Code := action.MethodAddress(Name);
  if ActionMethodOp.Code <> nil then
  begin
    ActionMethod := TActionMethod(ActionMethodOp);
    ActionMethod();
    msg := '执行成功';
    Result := True;
  end
  else
  begin
    msg := Name + '方法不存在';
    Result := False;
  end;

  //except
  //  on e: Exception do
  //  begin
  //    msg := Name + '方法执行异常:' + e.Message;
  //    Result := False;
  //  end;
  //end;
end;

function TRoute.InterceptInvoke(action: TObject; out msg: string): integer;
var
  ActionMethod: TInterceptMethod;
  ActionMethodOp: TMethod;
  ret: boolean;
  Name: string;
begin
  try
    Name := 'Intercept';
    ActionMethodOp.Data := Pointer(action);
    ActionMethodOp.Code := action.MethodAddress(Name);
    if ActionMethodOp.Code <> nil then
    begin
      ActionMethod := TInterceptMethod(ActionMethodOp);
      ret := ActionMethod();
      if ret then
        Result := 1
      else
        Result := 0;
      msg := '执行成功';
    end
    else
    begin
      msg := Name + '方法不存在';
      Result := 3;
    end;

  except
    on e: Exception do
    begin
      msg := Name + '方法执行异常:' + e.Message;
      Result := 4;
    end;
  end;
end;

function TRoute.DateTimeToGMT(const ADate: TDateTime): string;
const
  WEEK: array[1..7] of PChar = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthDig: array[1..12] of PChar =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  wWeek, wYear, wMonth, wDay, wHour, wMin, wSec, wMilliSec: word;
  sWeek, sMonth: string;
begin
  DecodeDateTime(ADate, wYear, wMonth, wDay, wHour, wMin, wSec, wMilliSec);
  wWeek := DayOfWeek(ADate);
  sWeek := WEEK[wWeek];
  sMonth := MonthDig[wMonth];
  Result := Format('%s, %.2d %s %d %.2d:%.2d:%.2d GMT',
    [sWeek, wDay, sMonth, wYear, wHour, wMin, wSec]);
end;

procedure TRoute.loadAssets(_Request: TWebRequest; _Response: TWebResponse; url: string);
var
  tmp, assetsfile, extension, webpath, ContentType: string;
  assets: TMemoryStream;
begin
  if (not Config.open_debug) and Config.open_cache then
  begin
    _Response.Headers.Add('Cache-Control', 'max-age=' + Config.cache_max_age);
    _Response.Headers.Add('Pragma', 'Pragma');
    //TTimeZone.local.ToUniversalTime(now())
    tmp := DateTimeToGMT(now);
    _Response.Headers.Add('Last-Modified', tmp);
    tmp := DateTimeToGMT(IncHour(now(), 24));
    _Response.Headers.Add('Expires', tmp);
  end
  else
  begin
    _Response.Headers.Add('Cache-Control', 'no-cache,no-store');
  end;
  webpath := Config.BasePath;
  assetsfile := webpath + Config.WebRoot + url;
  if FileExists(assetsfile) then
  begin
    if Pos('?', assetsfile) > 0 then
    begin
      assetsfile := assetsfile.Substring(0, Pos('?', assetsfile));
    end;
    assets := TMemoryStream.Create;
    try
      try
        extension := ExtractFileExt(assetsfile);
        extension := extension.Replace('.', '');
        // assets.LoadFromFile(assetsfile);
        ContentType := getMimeType(extension);
        if ContentType <> '' then
        begin
          //_Response.ContentStream := assets;
          //  _Response.SendStream(assets,true,200);
          _Response.Render(assetsfile);
        end
        else
        begin
          Error404(_Response, url);
        end;
      except
        Error404(_Response, url);
      end;
    finally
      assets.Free;
    end;
  end
  else
  begin
    Error404(_Response, url);
  end;
end;

{ TRouteItem }

procedure TRouteItem.SetAction(AValue: TClass);
begin
  if FAction = AValue then Exit;
  FAction := AValue;
end;

procedure TRouteItem.SetrouteUrl(AValue: string);
begin
  if FrouteUrl = AValue then Exit;
  FrouteUrl := AValue;
end;

procedure TRouteItem.SettplPath(AValue: string);
begin
  if FtplPath = AValue then Exit;
  FtplPath := AValue;
end;


initialization
  Route := TRoute.Create;

finalization
  Route.Free;
end.
