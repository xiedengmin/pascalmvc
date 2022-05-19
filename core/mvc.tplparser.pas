{*******************************************************}

{       pascalMVC                                    }



{*******************************************************}
unit MVC.TplParser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  MVC.TplUnit,
  MVC.Config,
  {$IFDEF FPC}
  //CommUnit,
  superobject,
  RegExprEx ,
  {$ELSE}
  Web.HTTPApp, FireDAC.comp.Client,
  XSuperObject,
  System.RegularExpressions,
  {$ENDIF}
  //uDBConfig
  uregexpr;

type
  TTplParser = class(TPersistent)
  private
    // FDb: TDBConfig;
    leftFmt: string; //左边字符
    rightFmt: string; //右边字符
    procedure foreachother(var Text: string);
    procedure foreachinclude(var Text: string; param: TStringList; url: string);
    procedure foreachclear(var Text: string);
    function foreachvalue(Text: string; key: string; Value: string;
      var isok: boolean): string;
    function foreach(Text: string; param: TStringList): string;
    function ForeachJson(Text: string; key: string; json: ISuperObject;
      var isok: boolean): string;
    function foreachsubjson(Text: string; key: string; json: ISuperObject;
      var isok: boolean): string;
    function foreachlist(Text: string; key: string; json: ISuperObject;
      var isok: boolean): string;
    function foreachsublist(Text: string; key: string; json: ISuperObject;
      var isok: boolean): string;
    function foreachif(Text: string; key: string; Value: string;
      var isok: boolean): string;
    function foreachsetif(Text: string): string;
    function foreachelseif(Text: string): string;
    function checkifwhere(where: string): boolean;
    // procedure SetDb(const Value: TDBConfig);
  public
    //property Db: TDBConfig read FDb write SetDb;
    procedure Parser(var Text: string; param: TStringList; url: string);
    constructor Create();
  end;

implementation

function TTplParser.foreachvalue(Text, key, Value: string; var isok: boolean): string;
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s: string;
  regex: TRegExpr;
  content: ansistring;
begin
  {$IFDEF FPC}
    regex := TRegExpr.Create;
    regex.Expression := '{#' + key + '}';
    {$IFDEF UNIX}
         content := Text;
     {$else}
    content := Utf8ToAnsi(Text);
     {$ENDIF}
    while regex.Exec(content) do
    begin
      {$IFDEF UNIX}
       content := regex.Replace(content, Value, True);
     {$else}
      content := regex.Replace(content, Utf8ToAnsi(Value), True);
     {$ENDIF}

    end;
    {$IFDEF UNIX}
        Text := content;
     {$else}
    Text := ansitoutf8(content);
     {$ENDIF}
    regex.Free;
    Result := Text;

  {$ELSE}
  matchs := TRegEx.Matches(Text, '#\{(([\s\S])*?)\}');
  isok := False;
  s := '#{' + key + '}';
  matchs := TRegEx.Matches(Text, s);
  for match in matchs do
  begin
    if match.Value = s then
    begin
      Text := Text.Replace(match.Value, Value);
      isok := True;
    end;
  end;
  Result := Text;
  {$ENDIF}
end;


  {
procedure TTplParser.SetDb(const Value: TDBConfig);
begin
  FDb := Value;
end;          }

function TTplParser.checkifwhere(where: string): boolean;
var
  sn: integer;
begin
  where := where.Replace('neq', ' != ');
  where := where.Replace('eq', ' = ');
  where := where.Replace('and', ' and ');
  where := where.Replace('or', ' or ');
  where := where.Replace('gte', ' >= ').Replace('ge', ' >= ');
  where := where.Replace('gt', ' >= ');
  where := where.Replace('lte', ' <= ').Replace('le', ' <= ');
  where := where.Replace('lt', ' < ');
  where := where.Replace('==', ' = ');
  try
    // Db.Default.TryConnDB;
    {$IFDEF FPC}
  //  Db.Default.TMP_CDS.Sql.Clear;
  //  Db.Default.TMP_CDS.Sql.Add('select ' + where + ' as sn');
   // Db.Default.TMP_CDS.Open;
    {$ELSE}
    // Db.Default.TMP_CDS.Open('select ' + where + ' as sn');
    {$ENDIF}
    // sn := Db.Default.TMP_CDS.FieldByName('sn').AsInteger;
    Result := sn = 1;
  except
    Result := False;
  end;
end;

constructor TTplParser.Create();
begin
end;

function TTplParser.foreach(Text: string; param: TStringList): string;
var
  i: integer;
  key, Value: string;
  jo: ISuperObject;
  html: string;
  isok: boolean;
  tmpstr: TStringList;
begin
  html := Text;
  tmpstr := TStringList.Create;
  try
    for i := 0 to param.Count - 1 do
    begin
      key := param.Names[i];
      Value := param.ValueFromIndex[i];
      try
        //     jo := SO(value);
        if (pos('{', Value) > 0) and (pos('}', Value) > 0) then
          isok := True
        else
          isok := False;
      except
        isok := False;
      end;
      if not isok then
      begin
        html := foreachvalue(html, key, Value, isok);
        if isok then
        begin
          tmpstr.Add(key);
        end;
      end;
    end;
    for i := 0 to tmpstr.Count - 1 do
    begin
      param.Delete(param.IndexOfName(tmpstr.Strings[i]));
    end;
    tmpstr.Clear;
    for i := 0 to param.Count - 1 do
    begin
      key := param.Names[i];
      Value := param.ValueFromIndex[i];
      try
        jo := SO(Value);
      except
        jo := nil;
      end;
      if jo <> nil then
      begin
        {$IFDEF FPC}
        if jo.DataType = TSuperType.stObject then
        begin
          html := ForeachJson(html, key, jo, isok);
          if isok then
          begin
            tmpstr.Add(key);
          end;
        end
        else if jo.DataType = TSuperType.stArray then
        begin
          html := foreachlist(html, key, jo, isok);
          if isok then
          begin
            tmpstr.Add(key);
          end;
        end
        else
        begin
          html := foreachif(html, key, value, isok);
        end;
        {$ELSE}
        if jo.DataType = TDataType.dtObject then
        begin
          html := foreachjson(html, key, jo, isok);
          if isok then
          begin
            tmpstr.Add(key);
          end;
        end
        else if jo.DataType = TDataType.dtArray then
        begin
          html := foreachlist(html, key, jo, isok);
          if isok then
          begin
            tmpstr.Add(key);
          end;
        end
        else
        begin
          html := foreachif(html, key, Value, isok);
        end;
        {$ENDIF}
      end;
    end;
    for i := 0 to tmpstr.Count - 1 do
    begin
      param.Delete(param.IndexOfName(tmpstr.Strings[i]));
    end;
  finally
    tmpstr.Clear;
    tmpstr.Free;
  end;
  param.Clear;

  html := foreachsetif(html);
  Result := html;
end;

function TTplParser.foreachlist(Text, key: string; json: ISuperObject;
  var isok: boolean): string;
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  arr: ISuperObject;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  arr: ISuperArray;
  {$ENDIF}
  s, datavalue, itemvalue: string;
  strls: TStringList;
  html, html1: string;
  I: integer;
begin
  strls := TStringList.Create;

  {$IFDEF FPC}
  arr := json;
  {$ELSE}
  arr := json.AsArray;
  {$ENDIF}
  isok := False;
  {$IFDEF FPC}
    RegExpr := TRegExprEx.Create;
  matchs := RegExpr.Matches(text, '<#list.*data=' + key + ' [\s\S]*?</#list>');

  try
    for match in matchs do
    begin
      strls.Text := match;

      s := RegExpr.ReplaceExEx(strls.Text, '<#list.*?>', '');
      s := RegExpr.ReplaceExEx(s, '</#list>', '');
      html1 := s;
      s := Trim(RegExpr.MatchEx(strls.Text, '<#list.*?>'));
      datavalue := Trim(RegExpr.MatchEx(s, 'data=.*? '));
      itemvalue := Trim(RegExpr.MatchEx(s, 'item=.*?>')) ;
      itemvalue :=itemvalue.Replace('>', '');
      datavalue := Copy(datavalue, 6, Length(datavalue) - 5);
      itemvalue := Copy(itemvalue, 6, Length(itemvalue) - 5);
      if datavalue = key then
      begin
        for I := 0 to arr.AsArray.Length - 1 do
        begin
          html := html + ForeachJson(html1, itemvalue, arr.AsArray[I], isok);
        end;
        html := foreachsetif(html);
        text := text.Replace(match, html);
        isok := true;
      end;
    end;

    Result := text;
  finally
    FreeAndNil(RegExpr);
    strls.Clear;
    strls.Free;
  end;
  {$ELSE}
  matchs := TRegEx.Matches(Text, '<#list.*data=' + key + ' [\s\S]*?</#list>');
  try
    for match in matchs do
    begin
      strls.Text := match.Value;

      s := TRegEx.Replace(strls.Text, '<#list.*?>', '');
      s := TRegEx.Replace(s, '</#list>', '');
      html1 := s;
      s := Trim(TRegEx.Match(strls.Text, '<#list.*?>').Value);
      datavalue := Trim(TRegEx.Match(s, 'data=.*? ').Value);
      itemvalue := Trim(TRegEx.Match(s, 'item=.*?>').Value.Replace('>', ''));
      datavalue := Copy(datavalue, 6, Length(datavalue) - 5);
      itemvalue := Copy(itemvalue, 6, Length(itemvalue) - 5);
      if datavalue = key then
      begin
        for I := 0 to arr.Length - 1 do
        begin
          html := html + foreachjson(html1, itemvalue, arr.O[I].AsObject, isok);
        end;
        html := foreachsetif(html);
        Text := Text.Replace(match.Value, html);
        isok := True;
      end;
    end;

    Result := Text;
  finally
    strls.Clear;
    strls.Free;
  end;
  {$ENDIF}
end;

function TTplParser.foreachjson(Text, key: string; json: ISuperObject;
  var isok: boolean): string;
var
  {$IFDEF FPC}
  SuperObjectIter : TSuperObjectIter;
  jarr:    Tsuperarray;
  matchs: TStringList;
  match: string;
  RegExpr : TRegExprEx;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s, html: string;
  reg2: Tregexpr;
begin
  html := Text;
  isok := False;
  if json.IsType(TSuperType.stObject) then
  begin
    {$IFDEF FPC}
    RegExpr := TRegExprEx.Create;
    try
      jarr:=json.AsArray;
      if ObjectFindFirst(json, SuperObjectIter) then
      begin
        try
          repeat
            if json.DataType <> stArray then
            begin
            //  html := foreachif(html, key + '.' + SuperObjectIter.Key, SuperObjectIter.Val.AsString, isok);
             // s := '<' + key + '.' + SuperObjectIter.Key+ '>';
               s := self.leftFmt + key + '.' + SuperObjectIter.Key+  self.rightFmt;
         //  reg2:=Tregexpr.create;
        //  reg2.Expression :=('#{' + 'i.sex' + '}');
        //   Reg2.Exec((html));
         //   reg2.free;
               try
              matchs := RegExpr.Matches(html, s);
              for match in matchs do
              begin
                if match = s then
                begin
                  html := html.Replace(match, ansitoutf8(SuperObjectIter.val.AsString));
                  isok := true;
                end;
              end;
              finally
                FreeAndNil(matchs);
              end;
            end
            else
            begin
              html := foreachsublist(html, key + '.' + SuperObjectIter.Key, so(SuperObjectIter.Val.AsString), isok);
            end;
          until not ObjectFindNext(SuperObjectIter);
        finally
          ObjectFindClose(SuperObjectIter);
        end;
      end;
    finally
      FreeAndNil(RegExpr);
    end;
    {$ELSE}
    json.First;
    while not json.EOF do
    begin
      if json.DataType <> TDataType.dtArray then
      begin
        html := foreachif(html, key + '.' + json.CurrentKey,
          json.CurrentValue.AsVariant, isok);
        //s := '#{' + key + '.' + json.CurrentKey + '}';
        matchs := TRegEx.Matches(html, s);
        for match in matchs do
        begin
          if match.Value = s then
          begin
            html := html.Replace(match.Value, json.CurrentValue.AsVariant);
            isok := True;
          end;
        end;
      end
      else
      begin
        html := foreachsublist(html, key + '.' + json.CurrentKey,
          so(json.CurrentValue.AsVariant), isok);
      end;
      json.Next;
    end;
    {$ENDIF}
  end;

  Result := html;
end;

procedure TTplParser.foreachclear(var Text: string);
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
begin
  {$IFDEF FPC}
  Matchs := TStringList.Create;
  try
    RegExpr := TRegExprEx.Create;
    try
      matchs := RegExpr.Matches(text, '#\{[\s\S]*?\}');
      for match in matchs do
      begin
        text := RegExpr.ReplaceExEx(text, match, '');
      end;
      matchs := RegExpr.Matches(text, '<#list[\s\S]*?</#list>');
      for match in matchs do
      begin
        text := RegExpr.ReplaceExEx(text, match, '');
      end;
      matchs := RegExpr.Matches(text, '<#if[\s\S]*?</#if>');
      for match in matchs do
      begin
        text := RegExpr.ReplaceExEx(text, match, '');
      end;
    finally
      RegExpr.Free;
    end;
  finally
    FreeAndNil(Matchs);
  end;

  {$ELSE}
  matchs := TRegEx.Matches(Text, '#\{[\s\S]*?\}');
  for match in matchs do
  begin
    Text := TRegEx.Replace(Text, match.Value, '');
  end;
  matchs := TRegEx.Matches(Text, '<#list[\s\S]*?</#list>');
  for match in matchs do
  begin
    Text := TRegEx.Replace(Text, match.Value, '');
  end;
  matchs := TRegEx.Matches(Text, '<#if[\s\S]*?</#if>');
  for match in matchs do
  begin
    Text := TRegEx.Replace(Text, match.Value, '');
  end;
  {$ENDIF}
end;

function TTplParser.foreachelseif(Text: string): string;
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  match: string;
  matchs : TStringList;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s, datavalue: string;
  strls: TStringList;
  html: string;
  isok: boolean;
begin
  isok := False;
  strls := TStringList.Create;
  RegExpr := TRegExprEx.Create;
  try
    {$IFDEF FPC}
      matchs := RegExpr.Matches(text, 'if[\s\S]*?<#else');
      for match in matchs do
      begin
        strls.Text := match;
        s := RegExpr.ReplaceExEx(strls.Text, 'if.*?>', '');
        s := RegExpr.ReplaceExEx(s, '<#else', '');
        html := s;
        RegExpr.Exec(strls.Text, 'if.*?>');
        datavalue := Trim(RegExpr.Match[0]);
        datavalue := datavalue.Replace('if', '');
        datavalue := datavalue.Replace('>', '');
        datavalue := datavalue.Replace(' ', '');
        datavalue := datavalue.Replace('''', '');
        if checkifwhere(datavalue) then
        begin
          text := html;
          isok := true;
          break;
        end;
      end;
   {$ELSE}
    matchs := TRegEx.Matches(Text, 'if[\s\S]*?<#else');
    for match in matchs do
    begin
      strls.Text := match.Value;
      s := TRegEx.Replace(strls.Text, 'if.*?>', '');
      s := TRegEx.Replace(s, '<#else', '');
      html := s;
      datavalue := Trim(TRegEx.Match(strls.Text, 'if.*?>').Value);
      datavalue := datavalue.Replace('if', '');
      datavalue := datavalue.Replace('>', '');
      datavalue := datavalue.Replace(' ', '');
      datavalue := datavalue.Replace('''', '');
      if checkifwhere(datavalue) then
      begin
        Text := html;
        isok := True;
        break;
      end;
    end;
    {$ENDIF}

    if not isok then
    begin
      {$IFDEF FPC}
      matchs := RegExpr.Matches(text, 'if[\s\S]*?</#if>');
      for match in matchs do
      begin
        if match.IndexOf('else') < 0 then
        begin
          strls.Text := match;
          s := RegExpr.ReplaceExEx(strls.Text, 'if.*?>', '');
          s := RegExpr.ReplaceExEx(s, '</#', '');
          html := s;
          datavalue := Trim(RegExpr.MatchEx(strls.Text, 'if.*?>'));
          datavalue := datavalue.Replace('if', '');
          datavalue := datavalue.Replace('>', '');
          datavalue := datavalue.Replace(' ', '');
          datavalue := datavalue.Replace('''', '');
          if checkifwhere(datavalue) then
          begin
            text := html;
            isok := true;
            break;
          end;
        end;
      end;
      {$ELSE}
      matchs := TRegEx.Matches(Text, 'if[\s\S]*?</#if>');
      for match in matchs do
      begin
        if match.Value.IndexOf('else') < 0 then
        begin
          strls.Text := match.Value;
          s := TRegEx.Replace(strls.Text, 'if.*?>', '');
          s := TRegEx.Replace(s, '</#', '');
          html := s;
          datavalue := Trim(TRegEx.Match(strls.Text, 'if.*?>').Value);
          datavalue := datavalue.Replace('if', '');
          datavalue := datavalue.Replace('>', '');
          datavalue := datavalue.Replace(' ', '');
          datavalue := datavalue.Replace('''', '');
          if checkifwhere(datavalue) then
          begin
            Text := html;
            isok := True;
            break;
          end;
        end;
      end;
      {$ENDIF}
    end;

    if not isok then
    begin
      {$IFDEF FPC}
      matchs := RegExpr.Matches(text, '<#else>[\s\S]*?</#if>');
      for match in matchs do
      begin
        strls.Text := match;
        s := RegExpr.ReplaceExEx(strls.Text, '<#else>', '');
        s := RegExpr.ReplaceExEx(s, '</#if>', '');
        text := s;
        isok := true;
        Break;
      end;
      {$ELSE}
      matchs := TRegEx.Matches(Text, '<#else>[\s\S]*?</#if>');
      for match in matchs do
      begin
        strls.Text := match.Value;
        s := TRegEx.Replace(strls.Text, '<#else>', '');
        s := TRegEx.Replace(s, '</#if>', '');
        Text := s;
        isok := True;
        Break;
      end;
      {$ENDIF}
    end;

    if not isok then
    begin
      Text := '';
    end;
    Result := Text;
  finally
    FreeAndNil(RegExpr);
    strls.Free;
  end;
end;

function TTplParser.foreachif(Text, key, Value: string; var isok: boolean): string;
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s: string;
begin
  s := Text;
  isok := False;

  {$IFDEF FPC}
  RegExpr := TRegExprEx.Create;
  try
    try
      matchs := RegExpr.Matches(text, '<#if ' + key + '[\s\S]*?>');
      for match in matchs do
      begin
        s := RegExpr.ReplaceExEx(match, key, value);
        text := RegExpr.ReplaceExEx(text, match, s);
        isok := true;
      end;
    finally
      Matchs.Free;
    end;
  finally
    FreeAndNil(RegExpr);
  end;
  {$ELSE}
  matchs := TRegEx.Matches(Text, '<#if ' + key + '[\s\S]*?>');
  for match in matchs do
  begin
    s := TRegEx.Replace(match.Value, key, Value);
    Text := TRegEx.Replace(Text, match.Value, s);
    isok := True;
  end;
  {$ENDIF}

  Result := Text;
end;

procedure TTplParser.foreachinclude(var Text: string; param: TStringList; url: string);
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  I, nCount : Integer;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s: string;
  regex, reg2: TRegExpr;
  content: ansistring;
  htmlfile, root: string;
  page: TPage;
begin
  {$IFDEF FPC}
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
  reg2.free;
  {$ELSE}
  matchs := TRegEx.Matches(Text, '<#include.*file=[\s\S]*?\>');

  for match in matchs do
  begin
    s := match.Value;
    s := s.Replace('__APP__', '');
    begin
      htmlfile := Trim(TRegEx.Match(s, 'file=.*?>').Value);
      htmlfile := Copy(htmlfile, Pos('=', htmlfile) + 1, Pos('>', htmlfile) -
        Pos('=', htmlfile) - 1);
      htmlfile := Trim(htmlfile);
      if htmlfile[htmlfile.Length] = '/' then
      begin
        htmlfile := Copy(htmlfile, 0, htmlfile.Length - 1);
        htmlfile := Trim(htmlfile);
      end;
      if Config.__WebRoot__.Trim <> '' then
      begin
        root := Config.__WebRoot__ + '/';
      end;
      htmlfile := htmlfile.Replace('''', '').Replace('"', '');
      if (htmlfile.IndexOf('/') = 0) then
        htmlfile := WebApplicationDirectory + root + Config.template + htmlfile
      else
        htmlfile := url + htmlfile;
      if (Trim(htmlfile) <> '') then
      begin
        if (not FileExists(htmlfile)) then
        begin
          Text := '';
        end
        else
        begin
          try
            page := TPage.Create(htmlfile, param, url);
            Text := TRegEx.Replace(Text, match.Value, page.HTML);
          finally
            FreeAndNil(page);
          end;
          foreachinclude(Text, param, url);
        end;
      end;
    end;
  end;
  {$ENDIF}
end;



procedure TTplParser.foreachother(var Text: string);
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
begin
  {$IFDEF FPC}
  RegExpr := TRegExprEx.Create;
  try
    matchs := RegExpr.Matches(text, 'APP');
    try
      for match in matchs do
      begin
        if  Config.app.Trim = '' then
          text := RegExpr.ReplaceExEx(text, match, '')
        else
          text := RegExpr.ReplaceExEx(text, match, '/' + Config.app);
      end;
    finally
      FreeAndNil(matchs);
    end;
  finally
    FreeAndNil(RegExpr);
  end;
  {$ELSE}
  matchs := TRegEx.Matches(Text, 'APP');
  for match in matchs do
  begin
    if Config.APP.Trim = '' then
      Text := TRegEx.Replace(Text, match.Value, '')
    else
      Text := TRegEx.Replace(Text, match.Value, '/' + Config.APP);

  end;
  {$ENDIF}
end;

function TTplParser.foreachsetif(Text: string): string;
var
  {$IFDEF FPC}
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s: string;
  html: string;
begin
  {$IFDEF FPC}
  RegExpr := TRegExprEx.Create;
  try
    matchs := RegExpr.Matches(text, '<#if[\s\S]*?</#if>');
    try
      for match in matchs do
      begin
        html := foreachelseif(match);
        s := text.Replace(match, html);
        text := s;
      end;
    finally
      FreeAndNil(matchs);
    end;
  finally
    FreeAndNil(RegExpr);
  end;
  {$ELSE}
  matchs := TRegEx.Matches(Text, '<#if[\s\S]*?</#if>');
  for match in matchs do
  begin
    html := foreachelseif(match.Value);
    s := Text.Replace(match.Value, html);
    Text := s;
  end;
  {$ENDIF}

  Result := Text;
end;

function TTplParser.foreachsubjson(Text, key: string; json: ISuperObject;
  var isok: boolean): string;
var
  {$IFDEF FPC}
  SuperObjectIter : TSuperObjectIter;
  matchs: TStringList;
  match: string;
  RegExpr : TRegExprEx;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  {$ENDIF}
  s, html: string;
begin
  html := Text;
  isok := False;
  {$IFDEF FPC}
  RegExpr := TRegExprEx.Create;
  try
    if json.IsType(TSuperType.stObject) then
    begin
      if ObjectFindFirst(json, SuperObjectIter) then
      begin
        try
          repeat
            html := foreachif(html, key + '.' + SuperObjectIter.Key, SuperObjectIter.Val.AsString, isok);
            matchs := RegExpr.Matches(html, s);
            for match in matchs do
            begin
              if match = s then
              begin
                html := html.Replace(match, SuperObjectIter.Val.AsString);
                isok := true;
              end;
            end;
          until not ObjectFindNext(SuperObjectIter);
        finally
          ObjectFindClose(SuperObjectIter);
        end;
      end;
    end;
  finally
    FreeAndNil(RegExpr);
  end;
  {$ELSE}
  try
    if json.IsType(TSuperType.stObject) then
    begin
      json.First;
      while not json.EOF do
      begin

        html := foreachif(html, key + '.' + json.CurrentKey,
          json.CurrentValue.AsVariant, isok);
        //s := '#{' + key + '.' + json.CurrentKey + '}';
        matchs := TRegEx.Matches(html, s);
        for match in matchs do
        begin
          if match.Value = s then
          begin
            html := html.Replace(match.Value, json.CurrentValue.AsVariant);
            isok := True;
          end;
        end;
      end;
    end;
  finally
  end;
  {$ENDIF}

  Result := html;
end;

function TTplParser.foreachsublist(Text, key: string; json: ISuperObject;
  var isok: boolean): string;
var
  {$IFDEF FPC}
  arr: TSuperArray;
  RegExpr : TRegExprEx;
  matchs: TStringList;
  match: string;
  {$ELSE}
  matchs: TMatchCollection;
  match: TMatch;
  arr: ISuperArray;
  {$ENDIF}
  s, datavalue, itemvalue: string;
  strls: TStringList;
  html, html1: string;
  I: integer;
begin
  {$IFDEF FPC}
  strls := TStringList.Create;
  try
    arr := json.AsArray;
    isok := false;
    RegExpr := TRegExprEx.Create;
    try
      matchs := RegExpr.Matches(text, '<#sublist.*data=' + key + ' [\s\S]*?</#sublist>');
      try
        for match in matchs do
        begin
          strls.Text := match;

          s := RegExpr.ReplaceExEx(strls.Text, '<#sublist.*?>', '');
          s := RegExpr.ReplaceExEx(s, '</#sublist>', '');
          html1 := s;
          s := Trim(RegExpr.MatchEx(strls.Text, '<#sublist.*?>'));
          datavalue := Trim(RegExpr.MatchEx(s, 'data=.*? '));
          itemvalue := Trim(RegExpr.MatchEx(s, 'item=.*?>'));
           itemvalue := itemvalue.Replace('>', '');
          datavalue := Copy(datavalue, 6, Length(datavalue) - 5);
          itemvalue := Copy(itemvalue, 6, Length(itemvalue) - 5);
          if datavalue = key then
          begin
            for I := 0 to arr.Length - 1 do
            begin
              html := html + foreachsubjson(html1, itemvalue, arr.O[I], isok);
            end;
            html := foreachsetif(html);
            text := text.Replace(match, html);
            isok := true;
          end;
        end;
        Result := text;
      finally
        FreeAndNil(matchs);
      end;
    finally
      FreeAndNil(RegExpr);
    end;
  finally
    strls.Clear;
    strls.Free;
  end;
  {$ELSE}
  strls := TStringList.Create;

  arr := json.AsArray;
  isok := False;
  matchs := TRegEx.Matches(Text, '<#sublist.*data=' + key + ' [\s\S]*?</#sublist>');
  try
    for match in matchs do
    begin
      strls.Text := match.Value;

      s := TRegEx.Replace(strls.Text, '<#sublist.*?>', '');
      s := TRegEx.Replace(s, '</#sublist>', '');
      html1 := s;
      s := Trim(TRegEx.Match(strls.Text, '<#sublist.*?>').Value);
      datavalue := Trim(TRegEx.Match(s, 'data=.*? ').Value);
      itemvalue := Trim(TRegEx.Match(s, 'item=.*?>').Value.Replace('>', ''));
      datavalue := Copy(datavalue, 6, Length(datavalue) - 5);
      itemvalue := Copy(itemvalue, 6, Length(itemvalue) - 5);
      if datavalue = key then
      begin
        for I := 0 to arr.Length - 1 do
        begin
          html := html + foreachsubjson(html1, itemvalue, arr.O[I], isok);
        end;
        html := foreachsetif(html);
        Text := Text.Replace(match.Value, html);
        isok := True;
      end;
    end;

    Result := Text;
  finally
    strls.Clear;
    strls.Free;
  end;
  {$ENDIF}
end;

procedure TTplParser.Parser(var Text: string; param: TStringList; url: string);
begin
  if Text = '' then
    Exit;
  self.leftFmt := Config.leftFmt;
  self.rightFmt := Config.rightFmt;
  foreachinclude(Text, param, url);
  Text := foreach(Text, param);
  // foreachclear(text);
  //  foreachother(text);
end;

end.
