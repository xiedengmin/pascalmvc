unit MVC.TplParser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mvc.Config, MVC.TplUnit, uregexpr;

type

  { TTplParser }

  TTplParser = class
  private
    function foreach(Text: string; param: TStringList): string;
    procedure foreachinclude(var Text: string; param: TStringList; url: string);
    function foreachvalue(Text, key, Value: string; var isok: boolean): string;
  public
    procedure Parser(var Text: string; param: TStringList; url: string);
  end;

implementation

{ TTplParser }

function TTplParser.foreach(Text: string; param: TStringList): string;
var
  isok: boolean;
  key, Value: string;
  i: integer;
begin
  for i := 0 to param.Count - 1 do
  begin
    key := param.Names[i];
    Value := param.ValueFromIndex[i];
    Text := foreachvalue(Text, key, Value, isok);
  end;
  Result := Text;
end;

procedure TTplParser.foreachinclude(var Text: string; param: TStringList; url: string);
var
  regex, reg2: TRegExpr;
  s: string;
  content: ansistring;
  htmlfile, root: string;
  page: TPage;
begin
  regex := TRegExpr.Create;
  regex.Expression := '<#include.*file=[\s\S]*?\>';
  reg2 := TRegExpr.Create;
  reg2.Expression := 'file=.*?>';
 // content := Utf8ToAnsi(Text);
  content := Text;
  while regex.Exec(content) do
  begin

    s := regex.Match[0];
    reg2.Exec(s);
    htmlfile := Trim(reg2.Match[0]);
    htmlfile := Copy(htmlfile, Pos('=', htmlfile) + 1, Pos('>', htmlfile) -
      Pos('=', htmlfile) - 1);
    htmlfile := Trim(htmlfile);
    if htmlfile[htmlfile.Length] = '/' then
    begin
      htmlfile := Copy(htmlfile, 0, htmlfile.Length - 1);
      htmlfile := Trim(htmlfile);
    end;
    if Config.WebRoot.Trim <> '' then
    begin
      root := Config.WebRoot + '/';
    end;
    htmlfile := htmlfile.Replace('''', '').Replace('"', '');
    if (htmlfile.IndexOf('/') = 0) then
      htmlfile := Config.BasePath + root + Config.template + htmlfile
    else
      htmlfile := Config.BasePath + root + Config.template + '/' + url + htmlfile;
    if (Trim(htmlfile) <> '') then
    begin
      if (not FileExists(htmlfile)) then
      begin
        Text := '';
      end
      else
      begin
        page := TPage.Create(htmlfile);
        try
          content := regex.Replace(content, page.Text, True);
        finally
          page.Free;
        end;
        //  foreachinclude(text, param, url);
      end;
    end;
  end;
  Text := content;
  regex.Free;
end;

{procedure TTplParser.foreachinclude(var Text: string; param: TStringList; url: string);
var
  regex, reg2: TRegExpr;
  s: string;
  content: ansistring;
  htmlfile, root: string;
  page: TPage;
begin
  regex := TRegExpr.Create;
  regex.Expression := '<#include.*file=[\s\S]*?\>';
  reg2 := TRegExpr.Create;
  reg2.Expression := 'file=.*?>';
  content := Utf8ToAnsi(Text);
  while regex.Exec(content) do
  begin

    s := regex.Match[0];
    reg2.Exec(Utf8ToAnsi(s));
    htmlfile := Trim(reg2.Match[0]);
    htmlfile := Copy(htmlfile, Pos('=', htmlfile) + 1, Pos('>', htmlfile) -
      Pos('=', htmlfile) - 1);
    htmlfile := Trim(htmlfile);
    if htmlfile[htmlfile.Length] = '/' then
    begin
      htmlfile := Copy(htmlfile, 0, htmlfile.Length - 1);
      htmlfile := Trim(htmlfile);
    end;
    if Config.WebRoot.Trim <> '' then
    begin
      root := Config.WebRoot + '/';
    end;
    htmlfile := htmlfile.Replace('''', '').Replace('"', '');
    if (htmlfile.IndexOf('/') = 0) then
      htmlfile := Config.BasePath + root + Config.template + htmlfile
    else
      htmlfile := Config.BasePath + root + Config.template + '/' + url + htmlfile;
    if (Trim(htmlfile) <> '') then
    begin
      if (not FileExists(htmlfile)) then
      begin
        Text := '';
      end
      else
      begin
        page := TPage.Create(htmlfile);
        try
          content := regex.Replace(content, Utf8ToAnsi(page.Text()), True);
        finally
          page.Free;
        end;
        //  foreachinclude(text, param, url);
      end;
    end;
  end;
  Text := AnsiToUtf8(content);
  regex.Free;
end; }
function TTplParser.foreachvalue(Text, key, Value: string; var isok: boolean): string;
var
  regex, reg2: TRegExpr;
  content: ansistring;
begin
  regex := TRegExpr.Create;
  regex.Expression := '{#' + key + '}';
  content := Utf8ToAnsi(Text);
  while regex.Exec(content) do
  begin
    content := regex.Replace(content, Utf8ToAnsi(Value), True);
  end;
  Text := AnsiToUtf8(content);
  Result := Text;
end;

procedure TTplParser.Parser(var Text: string; param: TStringList; url: string);
begin
  foreachinclude(Text, param, url);
  Text := foreach(Text, param);
end;

end.
