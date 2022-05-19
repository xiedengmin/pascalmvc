unit MVC.TplUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, mvc.Config, MVC.Tool, uregexpr;

type

  { TPage }

  TPage = class
  private
    PageContent: TStringList;
  public
    function Text(msg: string = ''): string;
    function Error404(msg: string): string;
    constructor Create(htmlfile: string);
    destructor Destroy; override;
  end;

  TPageCache = class
  public
    PageList: specialize TDictionary<string, string>;
    function LoadPage(key: string): string;
    constructor Create();
    destructor Destroy; override;
  end;
    TSQLCache = class
  public
    SQLList: specialize TDictionary<string, string>;
    function LoadPage(key: string): string;
    constructor Create();
    destructor Destroy; override;
  end;
var
  PageCache: TPageCache;
    SQLCache: TSQLCache;
implementation

{ TPage }

function TPage.Text(msg: string): string;
var
  content, s: string;
  regex: TRegExpr;
  k: integer;
begin
  content := PageContent.Text;
  if msg <> '' then
  begin
    regex := TRegExpr.Create;
    try
      regex.Expression := '{#message}';
      while regex.Exec(Utf8ToAnsi(content)) do
      begin
        content := AnsiToUtf8(regex.Replace(Utf8ToAnsi(content),
          Utf8ToAnsi(msg), True));
      end;
    finally
      regex.Free;
    end;
    Result := content;
  end
  else
    Result := content;
end;

function TPage.Error404(msg: string): string;
var
  htmlcontent: string;
begin
  htmlcontent := Text;
  if Trim(htmlcontent) = '' then
  begin
    htmlcontent := '<html><body><div style="text-align: left;">';
    htmlcontent := htmlcontent + '<div><h1>Error 404</h1></div>';
    htmlcontent := htmlcontent + '<hr><div>[ ' + msg + ' ] Not Find Page';
    htmlcontent := htmlcontent + '</div></div></body></html>';
  end;
  Result := htmlcontent;
end;

constructor TPage.Create(htmlfile: string);
begin
  //htmlfile := htmlfile.Replace('/', '\');
  PageContent := TStringList.Create;
  if htmlfile.Trim = '' then
    exit;
  if not FileExists(htmlfile) then
    exit;
  //if UpperCase(Config.document_charset) = 'UTF-8' then
  // begin
  //   PageContent.LoadFromFile(htmlfile, TEncoding.UTF8);
  // end
  // else
  begin
    PageContent.LoadFromFile(htmlfile);
  end;
end;

destructor TPage.Destroy;
begin
  PageContent.Free;
  inherited Destroy;
end;

constructor TPageCache.Create;
begin
  PageList := specialize TDictionary<string, string>.Create;
end;

destructor TPageCache.Destroy;
begin
  PageList.Free;
  inherited;
end;

function TPageCache.LoadPage(key: string): string;
var
  page: TPage;
  htmlcontent, pagefile: string;
begin
  if PageCache.PageList.ContainsKey(key) and not Config.open_debug then
  begin
    Lock();
    PageCache.PageList.TryGetValue(key, htmlcontent);
    UnLock();
  end
  else
  begin
    pagefile := Config.BasePath + config.WebRoot + '/' + Config.template + '/' + key;
    pagefile := IITool.PathFmt(pagefile);
    if FileExists(pagefile) then
    begin
      page := TPage.Create(pagefile);
      try
        htmlcontent := page.Text;
        Lock();
        PageCache.PageList.AddOrSetValue(key, htmlcontent);
        UnLock();
      finally
        page.Free;
      end;
    end
    else
    begin
      htmlcontent := '';
    end;
  end;
  Result := htmlcontent;
end;
 { TSQLCache }

constructor TSQLCache.Create;
begin
  SQLList := specialize TDictionary<string, string>.Create;
end;

destructor TSQLCache.Destroy;
begin
  SQLList.Free;
  inherited;
end;

function TSQLCache.LoadPage(key: string): string;
var
  page: TPage;
  sqlcontent, pagefile: string;
begin
  if SQLCache.SQLList.ContainsKey(key) and not Config.open_debug then
  begin
    Lock();
    SQLCache.SQLList.TryGetValue(key, sqlcontent);
    UnLock();
  end
  else
  begin
    pagefile := Config.BasePath + key;
    pagefile := IITool.PathFmt(pagefile);
    if FileExists(pagefile) then
    begin
      page := TPage.Create(pagefile);
      try
        sqlcontent := page.text;
        Lock();
        SQLCache.SQLList.AddOrSetValue(key, sqlcontent);
        UnLock();
      finally
        page.Free;
      end;
    end
    else
    begin
      sqlcontent := '';
    end;
  end;
  Result := sqlcontent;
end;
initialization
  PageCache := TPageCache.Create;
   SQLCache := TSQLCache.Create;

finalization
  PageCache.Free;
    SQLCache.Free;
end.
