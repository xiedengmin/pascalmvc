unit mvc.controller;

{$mode ObjFPC}{$H+}
{$M+}

interface

uses
  Classes, SysUtils, mvc.HttpServer, BrookStringMap, BrookHTTPUploads,
  mvc.Config, mvc.JSON, MVC.TplUnit, mvc.tool,
  MVC.TplParser, mvc.Session, MVC.DataSet, superobject;



////////////////
type
  { TController }

  TController = class
  private
    FRouteUrl: string;
    FtplPath: string;
    FWebPath: string;
    PageParams: TStringList;
    FRequest: TWebRequest;
    FResponse: TWebResponse;
    FSession: TSession;

    procedure SetRequest(AValue: TWebRequest);
    procedure SetResponse(AValue: TWebResponse);
    procedure SetRouteUrl(AValue: string);
    procedure SettplPath(AValue: string);

  published
    property Response: TWebResponse read FResponse write SetResponse;
    property Request: TWebRequest read FRequest write SetRequest;
    //控制器创建
    procedure CreateController; virtual;
  public
    procedure Corss_Origin;
    property RouteUrl: string read FRouteUrl write SetRouteUrl;
    property tplPath: string read FtplPath write SettplPath;
    property WebPath: string read FWebPath; //系统物理根目录
    function Session: TSession;
    function Input(param: string): string;
    function InputBody: string;
    function InputToJSON: IJObject;
    function InputToJSONSO: Isuperobject;
    function InputToJSONArray: IJArray;
    function UpFiles(filedir: string = ''; filename: string = ''): string;
    procedure SetAttr(key: string; ds: IDataSet); overload;
    procedure SetAttr(key, Value: string); overload;
    procedure SetAttr(key: string; json: IJObject); overload;
    procedure SetAttr(key: string; JsonArray: IJArray); overload;
    procedure ShowText(Text: string);
    procedure ShowJSON(json: string); overload;
    procedure ShowJSON(json: IJObject); overload;
    procedure ShowJSON(json: ISuperObject); overload;
    procedure ShowJSON(json: IJArray); overload;
    procedure ShowJSON(json: IDataSet); overload;
    procedure Show(htmlTpl: string);
    //访问拦截处理方法需子类继承使用
    function Intercept(): boolean; virtual;
    procedure Success(code: integer = 0; msg: string = '');
    procedure Fail(code: integer = -1; msg: string = '');
    destructor Destroy; override;
  end;

implementation

{ TController }
procedure TController.Corss_Origin;
var
  s: string;
begin
  s := Config.Corss_Origin.Allow_Origin;
  if s <> '' then
  begin
    Response.Headers.Add('Access-Control-Allow-Origin', s);
    s := Config.Corss_Origin.Allow_Headers;
    if s <> '' then
      Response.Headers.Add('Access-Control-Allow-Headers', s);
    s := Config.Corss_Origin.Allow_Method;
    if s <> '' then
      Response.Headers.Add('Access-Control-Allow-Method', s);
    if Config.Corss_Origin.Allow_Credentials then
      Response.Headers.Add('Access-Control-Allow-Method', 'true')
    else
      Response.Headers.Add('Access-Control-Allow-Method', 'false');
  end;
end;

function TController.Input(param: string): string;

begin

  if (Request.Method.ToLower = 'get') then
  begin
    Result := Request.Fields.Values[param];
    if Trim(Result) = '' then
      Result := Request.Fields.Values[param];
  end
  else if (Request.Method.ToLower = 'post') then
  begin
    Result := Request.Fields.Values[param];
  end;
end;

function TController.InputBody: string;
  //var
begin
  //Result := Request.Content;
  Result := Request.Fields.ToString;
end;

function TController.InputToJSON: IJObject;
var
  jo: IJObject;
  i: integer;
  isok: boolean;
  key, Value: string;
  body: string;
  fieldsmap: TBrookStringPair;
  testr: string;
begin
  isok := False;
  body := InputBody;
  try
    if body.Trim <> '' then
    begin
      if (body.Substring(0, 1) = '[') and (body.Substring(body.Length - 1, 1) = ']') then
        exit;   //不处理json数组由InputToJSONArray处理
      if (body.Substring(0, 1) = '{') and (body.Substring(body.Length - 1, 1) = '}') then
      begin
        try
          jo := IIJObject(body);
          isok := True;
        except
          jo := nil;
        end;
      end
      else if Request.Fields.Count > 0 then
      begin
        jo := IIJObject();
        Request.Fields.First(fieldsmap);
        for i := 0 to Request.Fields.Count - 1 do
        begin

          key := fieldsmap.Name;
          Value := fieldsmap.Value;
          if (key.Trim <> '') and (Value.Trim <> '') then
          begin
            isok := True;
            jo.O.Add(key, Utf8ToAnsi(Value));
          end;
          Request.Fields.Next(fieldsmap);
        end;
      end;
    end
    else
    begin
      jo := IIJObject();
      //testr:=pchar(request.Params.ToString);
      Request.Params.First(fieldsmap);
      for i := 0 to Request.Params.Count - 1 do
      begin

        key := fieldsmap.Name;
        Value := fieldsmap.Value;
        if (key.Trim <> '') and (Value.Trim <> '') then
        begin
          jo.O.Add(key, Utf8ToAnsi(Value));                                   //有问题
          isok := True;
        end;
        Request.Params.Next(fieldsmap);
      end;
    end;
  finally
    if not isok then
      jo := nil;
    Result := jo;
  end;

end;

function TController.InputToJSONSO: Isuperobject;
var
  jo: Isuperobject;
  i: integer;
  isok: boolean;
  key, Value: string;
  body: string;
  fieldsmap: TBrookStringPair;
  testr: string;
begin
  jo := SO('{}');
  isok := False;
  body := InputBody;
  try
    if body.Trim <> '' then
    begin
      if (body.Substring(0, 1) = '[') and (body.Substring(body.Length - 1, 1) = ']') then
        exit;   //不处理json数组由InputToJSONArray处理
      if (body.Substring(0, 1) = '{') and (body.Substring(body.Length - 1, 1) = '}') then
      begin
        try
          jo := so(body);
          isok := True;
        except
          jo := nil;
        end;
      end
      else if Request.Fields.Count > 0 then
      begin
        // jo := IIJObject();
        Request.Fields.First(fieldsmap);
        for i := 0 to Request.Fields.Count - 1 do
        begin

          key := fieldsmap.Name;
          Value := fieldsmap.Value;
          if (key.Trim <> '') and (Value.Trim <> '') then
          begin
            isok := True;
            jo.PutS(key, Value);
          end;
          Request.Fields.Next(fieldsmap);
        end;
      end;
    end
    else
    begin
      //testr:=pchar(request.Params.ToString);
      Request.Params.First(fieldsmap);
      for i := 0 to Request.Params.Count - 1 do
      begin

        key := fieldsmap.Name;
        Value := fieldsmap.Value;
        if (key.Trim <> '') and (Value.Trim <> '') then
        begin
          jo.s[key] := Utf8ToAnsi(Value);
          isok := True;
        end;
        Request.Params.Next(fieldsmap);
      end;
    end;
  finally
    if not isok then
      jo := nil;
    Result := jo;
  end;

end;

function TController.InputToJSONArray: IJArray;
var
  ja: IJArray;
  body: string;
begin
  body := InputBody;
  if body.Trim <> '' then
  begin
    if (body.Substring(0, 1) = '[') and (body.Substring(body.Length - 1, 1) = ']') then
    begin
      try
        ja := IIJArray(body);
      except
        ja := nil;
      end;
    end;
  end;
  Result := ja;
end;

procedure TController.SetAttr(key, Value: string);
begin
  if Value.Trim = '' then
    Value := ' ';
  PageParams.Values[key] := Value;
end;

procedure TController.SetAttr(key: string; ds: IDataSet);
begin
  setAttr(key, ds.toJSONArray);
end;

procedure TController.SetAttr(key: string; json: IJObject);
begin
  if json <> nil then
  begin
    setAttr(key, json.toJSON);
  end;
end;

procedure TController.SetAttr(key: string; JsonArray: IJArray);
begin
  if JsonArray <> nil then
  begin
    setAttr(key, JsonArray.toJSON);
  end;
end;


procedure TController.SetRequest(AValue: TWebRequest);
begin
  if FRequest = AValue then Exit;
  FRequest := AValue;
end;

procedure TController.SetResponse(AValue: TWebResponse);
begin
  if FResponse = AValue then Exit;
  FResponse := AValue;
end;

procedure TController.SetRouteUrl(AValue: string);
begin
  if FRouteUrl = AValue then Exit;
  FRouteUrl := AValue;
end;

procedure TController.SettplPath(AValue: string);
begin
  if FtplPath = AValue then Exit;
  FtplPath := AValue;
end;

procedure TController.ShowText(Text: string);
begin
  Corss_Origin;
  Response.Send(Text, 'text/html; charset=' + Config.document_charset, 200);
end;

procedure TController.ShowJSON(json: string);
begin
  Corss_Origin;
  Response.Send(json, 'application/json; charset=' + Config.document_charset, 200);
end;

procedure TController.ShowJSON(json: IJObject);
begin
  if json = nil then
    ShowJSON('{}')
  else
    ShowJSON(json.toJSon);
end;

procedure TController.ShowJSON(json: ISuperObject);
begin
  if json = nil then
    ShowJSON('{}')
  else
    ShowJSON(json.AsJSon());
end;

procedure TController.ShowJSON(json: IJArray);
begin
  if json = nil then
    ShowJSON('[]')
  else
    ShowJSON(json.toJSON);
end;

procedure TController.ShowJSON(json: IDataSet);
begin
  ShowJSON(json.toJSONArray);
end;

procedure TController.Show(htmlTpl: string);
var
  key: string;
  htmlcontent: string;
  suff: string;
  pageParser: TTplParser;
begin
  if htmlTpl.Trim = '' then
    exit;
  suff := '';
  if Pos('.', htmlTpl) < 1 then
  begin
    suff := Config.template_type;
  end;

  htmlTpl := htmlTpl.Replace('/', '\');
  if htmlTpl.Substring(0, 1) = '\' then
  begin
    key := htmlTpl + suff;
  end
  else
  begin
    if tplPath <> '' then
      tplPath := tplPath + '\';
    key := tplPath + htmlTpl + suff;
  end;
  htmlcontent := PageCache.LoadPage(key);
  if htmlcontent = '' then
  begin
    htmlcontent := '<h1>模板文件不存在</h1><hr>' + key;
  end
  else
  begin
    try
      pageParser := TTplParser.Create;
      pageParser.Parser(htmlcontent, PageParams, '');
    finally
      pageParser.Free;
    end;
  end;

  Response.Send(htmlcontent, 'text/html; charset=' + Config.document_charset, 200);
end;

function TController.Intercept: boolean;
begin
  Result := False;
end;

procedure TController.CreateController;
begin
  PageParams := TStringList.Create;
  FSession := TSession.Create(Request, Response);
end;



function TController.UpFiles(filedir: string = ''; filename: string = ''): string;

var
  VUpload: TBrookHTTPUpload;
  VFile, vfpath, VList, VError, ret: string;
begin
  ret := '';
  try
    if Request.IsUploading then
    begin
      VList := '<ol>';
      for VUpload in Request.Uploads do
      begin
        vfpath := serv.UploadsDir + PathDelim + DateToStr(Now).Replace('/', '');
        if not DirectoryExists(vfpath) then
        begin
          ForceDirectories(vfpath);
        end;
        VFile := Concat(serv.UploadsDir, PathDelim,
          DateToStr(Now).Replace('/', '') + PathDelim + GetGUID + VUpload.Name);
        vupload.SaveAs(VFile);
        if VUpload.Save(False, VError) then
        begin
          VList := Concat(VList, '<li><a href="?file=', VUpload.Name,
            '">', VUpload.Name, '</a></li>');

        end
        else
        begin
          VList := Concat(VList, '<li><font color="red">', VUpload.Name,
            ' - failed - ', VError, '</font></li>');
          VList := Concat(VList, '</ol>');
          //    Response.SendFmt(PAGE_DONE, [VList], CONTENT_TYPE, 200);
        end;

      end;
    end
    else
    begin
      if Request.Params.TryValue('file', VFile) then
        Response.Download(Concat(serv.UploadsDir, PathDelim, VFile))
      else
      begin
        // Response.Send('', CONTENT_TYPE, 200);
      end;

    end;
    ret := ret + VFile + ',';
  finally
  end;
  ret := ret.Substring(0, ret.Length - 1);
  Result := ret;
end;

  {
  var
  k: integer;
  path, s, FFileName: string;
  Afile: TFileStream;
  i: integer;
  p, ret, filetmp: string;
begin
  k := Request.Files.Count - 1;
  if k = -1 then
  begin
    ret := '';
    Result := ret;
    exit;
  end;
  for i := 0 to k do
  begin
    if filedir.Trim <> '' then
      path := WebPath + filedir.Trim
    else
      path := WebPath + 'upfile';
    if not DirectoryExists(path) then
    begin
      CreateDir(path);
    end;
    s := ExtractFileName(Request.Files.Next.Name);
    if filename.Trim <> '' then
    begin
      p := '';
      if i > 0 then
        p := i.ToString;
      filetmp := filename.Trim + p + copy(s, Pos('.', s), s.Length - pos('.', s) + 1);
    end
    else
    begin
      filetmp := GetGUID + copy(s, Pos('.', s), s.Length - pos('.', s) + 1);
    end;
    FFileName := path + '\' + filetmp;
    Afile := TFileStream.Create(FFileName, fmCreate);
    try
      Request.Files[i].Stream.Position := 0;
      Afile.CopyFrom(Request.Files[i].Stream, Request.Files[i].Stream.Size);
    finally
      Afile.Free;
    end;
    ret := ret + filetmp + ',';

  end;
  ret := ret.Substring(0, ret.Length - 1);
  Result := ret;
end;     }

function TController.Session: TSession;
begin
  Result := FSession;
end;

procedure TController.Fail(code: integer; msg: string);
var
  jo: ISuperObject;
begin
  jo := SO('{}');
  if Trim(msg) = '' then
    msg := '操作失败';
  jo.puti('code', code);
  jo.puts('message', msg);
  ShowJSON(jo);
end;

{
procedure TController.Success(code: integer; msg: string);
var
  jo: IJObject;
begin
  jo := IIJObject();
  if Trim(msg) = '' then
    msg := '操作成功';
  jo.O.Add('code', code);
  jo.O.Add('message', msg);
  ShowJSON(jo);
end;     }
procedure TController.Success(code: integer; msg: string);
var
  jo: ISuperObject;
begin
  jo := SO('{}');
  if Trim(msg) = '' then
    msg := '操作成功';
  jo.puti('code', code);
  jo.puts('message', msg);
  ShowJSON(jo);
end;

destructor TController.Destroy;
begin
  PageParams.Free;
  FSession.Free;
  inherited Destroy;
end;

end.
