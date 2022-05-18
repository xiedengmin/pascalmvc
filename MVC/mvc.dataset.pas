unit MVC.DataSet;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mvc.Config, MVC.Tool, MVC.LogUnit,MVC.JSON,MVC.TplParser,SQLDB, DB,RegExprEx;

type
        TResData = class
    Code: integer;
    Msg: string;
    procedure Value(sCode: Integer; sMsg: string);
  end;
  ISQL = interface
    procedure Select(fields: string);
    procedure From(tables: string);
    procedure And_(value: string);
    procedure OR_(value: string);
    procedure Order(value: string);
    procedure Insert(tables: string);
    procedure Edit(tables: string);
    procedure Del(tables: string);
    procedure Value(value: string);
    procedure Set_(value_set: string);
    function getSelect: string;
    function getFrom: string;
    function getOrder: string;
    function getInsert: string;
    function getEdit: string;
    function getDel: string;
    function getValue: string;
    function getSet_: string;
    function Text: string;
    function getWhere: string;
    function SQL: TStringList;
    procedure Clear;
    //不带引号
    procedure AndEq(key, value_andeq: string); //等于
    procedure AndNe(key, value_andne: string); //不等于
    procedure AndLt(key, value_andlt: string); //小于
    procedure AndLte(key, value_andlte: string); //小于等于
    procedure AndGt(key, value_andgt: string); //大于
    procedure AndGte(key, value_andgte: string); //大于等于
     //带引号
    procedure AndEqF(key, value_andeqf: string); //等于
    procedure AndNeF(key, value_andnef: string); //不等于
    procedure AndLtF(key, value_andltf: string); //小于
    procedure AndLteF(key, value_andltef: string); //小于等于
    procedure AndGtF(key, value_andgtf: string); //大于
    procedure AndGteF(key, value_andgtef: string); //大于等于
  end;

  TSQL = class(TInterfacedObject, ISQL)
  private
    FSelect: string;
    FFrom: string;
    FWhere: string;
    FOrder: string;
    FInsert: string;
    FEdit: string;
    FDelete: string;
    FValue: string;
    FSet: string;
    SQL_V: TStringList;
  public
    function SQL: TStringList;
    procedure Select(fields: string);
    procedure From(tables: string);
    procedure And_(value: string);

    procedure Insert(tables: string);
    procedure Edit(tables: string);
    procedure Del(tables: string);
    procedure Value(value: string);
    procedure Set_(value_set: string);

    function getSelect: string;
    function getFrom: string;
    function getOrder: string;

    function getInsert: string;
    function getEdit: string;
    function getDel: string;
    function getValue: string;
    function getSet_: string;
    function getWhere: string;
    function Text: string;
    //不带引号
    //不带引号
    procedure AndEq(key, value_andeq: string); //等于
    procedure AndNe(key, value_andne: string); //不等于
    procedure AndLt(key, value_andlt: string); //小于
    procedure AndLte(key, value_andlte: string); //小于等于
    procedure AndGt(key, value_andgt: string); //大于
    procedure AndGte(key, value_andgte: string); //大于等于
     //带引号
    procedure AndEqF(key, value_andeqf: string); //等于
    procedure AndNeF(key, value_andnef: string); //不等于
    procedure AndLtF(key, value_andltf: string); //小于
    procedure AndLteF(key, value_andltef: string); //小于等于
    procedure AndGtF(key, value_andgtf: string); //大于
    procedure AndGteF(key, value_andgtef: string); //大于等于

    procedure OR_(value_or: string);
    procedure Order(value_order: string);

    procedure Clear;
    //
    constructor Create(table: string);
    destructor Destroy; override;
  end;

  ISQLTpl = interface
    procedure SetKey(key: string; sParam: IJObject = nil);
    procedure SetParam(sParam: IJObject);
    procedure SetTpl(tpl: string);
    function AsISQL: ISQL;
  end;

  TSQLTpl = class(TInterfacedObject, ISQLTpl)
  private
    FTpl: string;
    Fkey: string;
    FParam: IJObject;
    function getSQL(sql: string; sType: string): ISQL;
    function ClearNotes(txt: string): string; //清理注释
    function getSQLKey(sql: string; sKey: string; _T: Boolean = False): string;
  public
    function AsISQL: ISQL;
    procedure SetKey(key: string; sParam: IJObject = nil);
    procedure SetParam(sParam: IJObject);
    procedure SetTpl(tpl: string);
    constructor Create(tpl: string; key: string; sParam: IJObject); overload;
    constructor Create(tpl: string); overload;

    destructor Destroy; override;
  end;
  IDataSet = interface

    procedure setCount(n: Integer);
    function toJSONArray: string;
    function toJSONObject: string;
        function DS: TSQLQuery;
    function Count: Integer;
    function isEmpty: Boolean;
        procedure Next;
    procedure Post;
   // procedure Append;
   // procedure Edit;
     function Eof: Boolean;
  end;

  { TDataSet }

  TDataSet = class(TInterfacedObject, IDataSet)
  private
     FCount: integer;
    dataset: TSQLQuery;
    function checkType(dbtype: TFieldType): boolean;
  public

    function toJSONArray: string;
    function toJSONObject: string;
     function S(fieldname: string): string;
    function Int(fieldname: string): Integer;
    function D(fieldname: string): Double;
         function DS: TSQLQuery;
    function Count: Integer;
    function isEmpty: Boolean;
    procedure Next;
    procedure Post;
   // procedure Append;
   // procedure Edit;
     function Eof: Boolean;
    procedure setCount(n: Integer);

    constructor Create();
    destructor Destroy; override;
  end;

function IIDataSet: IDataSet;
  function IISQL(table: string = ''): ISQL;

function IISQLTpl(tpl: string; key: string; sParam: IJObject = nil): ISQLTpl; overload;

function IISQLTpl(tpl: string): ISQLTpl; overload;
implementation
uses  MVC.TplUnit;
function IISQLTpl(tpl: string; key: string; sParam: IJObject = nil): ISQLTpl;
begin
  Result := TSQLTpl.Create(tpl, key, sParam) ;
end;

function IISQLTpl(tpl: string): ISQLTpl;
begin
  Result := TSQLTpl.Create(tpl) ;
end;

function IISQL(table: string): ISQL;
begin
  Result := Tsql.Create(table);
end;
{ TDataSet }
function IIDataSet: IDataSet;
var
  ds: IDataSet;
begin
  ds := TDataSet.Create;
  Result := ds;
end;

function TDataSet.DS: TSQLQuery;
begin
  Result := dataset;
end;

function TDataSet.checkType(dbtype: TFieldType): boolean;
begin
  if dbtype in [ftString, ftWideString, ftUnknown, ftWideMemo, ftMemo,
    ftDate, ftDateTime, ftTime, ftFmtMemo, ftTimeStamp, ftFixedChar,
    ftFixedWideChar] then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;
function TDataSet.Count: Integer;
begin
  if FCount <> 0 then
    Result := FCount
  else
    Result := DS.RecordCount;
end;
procedure TDataSet.Next;
begin
  ds.Next;
end;

procedure TDataSet.Post;
begin
  DS.Post;
end;

procedure TDataSet.setCount(n: integer);
begin
  FCount := n;
end;
function TDataSet.toJSONArray: string;
var
  i:integer;
  ftype: TFieldType;
  json, item, key, Value: string;
begin
  try
    if dataset = nil then
    begin
      Result := '[]';
      exit;
    end;
    json := '[';
    with dataset do
    begin
      First;

      while not EOF do
      begin
        item := '{';
        for i := 0 to Fields.Count - 1 do
        begin
          ftype := Fields[i].DataType;
          if Config.JsonToLower then
            key := Fields[i].DisplayLabel.ToLower
          else
            key := Fields[i].DisplayLabel;
          if checkType(ftype) then
            Value := '"' + IITool.UnicodeEncode(Fields[i].AsString) + '"'
          else if ftype = ftBoolean then
            Value := Fields[i].AsString.ToLower
          else
            Value := Fields[i].AsString;

          if Value = '' then
            Value := '""';
          item := item + '"' + key + '"' + ':' + Value + ',';
        end;
        item := copy(item, 1, item.Length - 1);
        item := item + '},';
        json := json + item;
        Next;
      end;
    end;
    if json.Length > 1 then
      json := copy(json, 1, json.Length - 1);
    json := json + ']';
    Result := json;
  except
    on e: Exception do
    begin
      log(e.Message);
    end;
  end;
end;

function TDataSet.toJSONObject: string;
var
  i: integer;
  ftype: TFieldType;
  json, item, key, Value: string;
begin
  json := '';
  try
    if dataset = nil then
    begin
      Result := '{}';
      exit;
    end;
    with dataset do
    begin

      if not IsEmpty then
      begin
        item := '{';
        for i := 0 to Fields.Count - 1 do
        begin
          ftype := Fields[i].DataType;
          if Config.JsonToLower then
            key := Fields[i].DisplayLabel.ToLower
          else
            key := Fields[i].DisplayLabel;

          if checkType(ftype) then
            Value := '"' + IITool.UnicodeEncode(Fields[i].AsString) + '"'
          else if ftype = ftBoolean then
            Value := Fields[i].AsString.ToLower
          else
            Value := Fields[i].AsString;

          if Value = '' then
            Value := '0';
          item := item + '"' + key + '"' + ':' + Value + ',';
        end;
        item := copy(item, 1, item.Length - 1);
        item := item + '},';
        json := json + item;
        Next;
      end;
    end;
    if json.Length > 1 then
      json := copy(json, 1, json.Length - 1);
    Result := json;
  except
    on e: Exception do
    begin
      log(e.Message);
    end;
  end;
end;
{procedure TDataSet.Edit;
begin
  ds.Edit;
end;
           }
function TDataSet.Eof: Boolean;
begin
  Result := ds.Eof;
end;

function TDataSet.D(fieldname: string): Double;
begin
  Result := ds.FieldByName(fieldname).AsFloat;
end;

function TDataSet.Int(fieldname: string): Integer;
begin
  Result := ds.FieldByName(fieldname).AsInteger;
end;

function TDataSet.S(fieldname: string): string;
begin
  Result := ds.FieldByName(fieldname).AsString;
end;

function TDataSet.isEmpty: boolean;
begin
  Result := dataset.IsEmpty;
end;
constructor TDataSet.Create;
begin
  dataset := TSQLQuery.Create(nil);
end;

destructor TDataSet.Destroy;
begin
  dataset.Free;
  inherited Destroy;
end;
 
procedure TSQL.Clear;
begin
  FFrom := '';
  FWhere := '';
  FSelect := '';
  FOrder := '';
  SQL_V.Clear;
end;

constructor TSQL.Create(table: string);
begin
  SQL_V := TStringList.Create;
  SQL_V.Clear;
  if table.Trim <> '' then
    FFrom := ' from ' + table;
end;

procedure TSQL.Del(tables: string);
begin
  if Trim(tables) = '' then
    exit;
  FDelete := tables;
  SQL_V.Clear;
end;

destructor TSQL.Destroy;
begin
  SQL_V.Clear;
  SQL_V.Free;
  inherited;
end;

procedure TSQL.Edit(tables: string);
begin
  if Trim(tables) = '' then
    exit;
  FEdit := tables;
  SQL_V.Clear;
end;

procedure TSQL.AndEq(key, value_andeq: string);
begin
  if (Trim(value_andeq) = '') or (Trim(key) = '') then
    exit;
  And_(key + '=' + value_andeq);
end;

procedure TSQL.AndEqF(key, value_andeqf: string);
begin
  if (Trim(value_andeqf) = '') or (Trim(key) = '') then
    exit;
  And_(key + '=' + QuotedStr(value_andeqf));
end;

procedure TSQL.AndGt(key, value_andgt: string);
begin
  if (Trim(value_andgt) = '') or (Trim(key) = '') then
    exit;
  And_(key + '>' + value_andgt);
end;

procedure TSQL.AndGte(key, value_andgte: string);
begin
  if (Trim(value_andgte) = '') or (Trim(key) = '') then
    exit;
  And_(key + '>=' + value_andgte);
end;

procedure TSQL.AndGteF(key, value_andgtef: string);
begin
  if (Trim(value_andgtef) = '') or (Trim(key) = '') then
    exit;
  And_(key + '>=' + QuotedStr(value_andgtef));
end;

procedure TSQL.AndGtF(key, value_andgtf: string);
begin
  if (Trim(value_andgtf) = '') or (Trim(key) = '') then
    exit;
  And_(key + '>' + QuotedStr(value_andgtf))
end;

procedure TSQL.AndLt(key, value_andlt: string);
begin
  if (Trim(value_andlt) = '') or (Trim(key) = '') then
    exit;
  And_(key + '>' + value_andlt);
end;

procedure TSQL.AndLte(key, value_andlte: string);
begin
  if (Trim(value_andlte) = '') or (Trim(key) = '') then
    exit;
  And_(key + '<=' + value_andlte);
end;

procedure TSQL.AndLteF(key, value_andltef: string);
begin
  if (Trim(value_andltef) = '') or (Trim(key) = '') then
    exit;
  And_(key + '<=' + QuotedStr(value_andltef));
end;

procedure TSQL.AndLtF(key, value_andltf: string);
begin
  if (Trim(value_andltf) = '') or (Trim(key) = '') then
    exit;
  And_(key + '<' + QuotedStr(value_andltf));
end;

procedure TSQL.AndNe(key, value_andne: string);
begin
  if (Trim(value_andne) = '') or (Trim(key) = '') then
    exit;
  And_(key + '<>' + value_andne);
end;

procedure TSQL.AndNeF(key, value_andnef: string);
begin
  if (Trim(value_andnef) = '') or (Trim(key) = '') then
    exit;
  And_(key + '<>' + QuotedStr(value_andnef));
end;

procedure TSQL.And_(value: string);
begin
  if Trim(value) = '' then
    exit;
  if FWhere = '' then
    FWhere := ' where 1=1 ';
  FWhere := FWhere + ' and ' + value;
  SQL_V.Clear;
end;

procedure TSQL.From(tables: string);
begin
  if Trim(tables) = '' then
    exit;
  if FFrom = '' then
    FFrom := ' from ';
  FFrom := FFrom + tables;
  SQL_V.Clear;
end;

function TSQL.getDel: string;
begin
  Result := FDelete;
end;

function TSQL.getEdit: string;
begin
  Result := FEdit;
end;

function TSQL.getFrom: string;
begin
  Result := FFrom + FWhere;
end;

function TSQL.getInsert: string;
begin
  Result := FInsert;
end;

function TSQL.getOrder: string;
begin
  Result := FOrder;
end;

function TSQL.getSelect: string;
begin

  if FSelect = '' then
    FSelect := 'select * ';
  Result := FSelect;
end;

function TSQL.getSet_: string;
begin
  Result := FSet;
end;

function TSQL.getValue: string;
begin
  Result := FValue;
end;

function TSQL.getWhere: string;
begin
  Result := FWhere;
end;

procedure TSQL.Insert(tables: string);
begin
  if Trim(tables) = '' then
    exit;
  FInsert := tables;
  SQL_V.Clear;
end;

procedure TSQL.OR_(value_or: string);
begin
  if Trim(value_or) = '' then
    exit;
  if FWhere = '' then
    FWhere := ' where 1=1 ';
  FWhere := FWhere + ' or ' + value_or;
  SQL_V.Clear;
end;

procedure TSQL.Order(value_order: string);
begin
  if Trim(value_order) = '' then
    exit;
  if FOrder = '' then
    FOrder := ' order by ';
  FOrder := FOrder + value_order;
  SQL_V.Clear;

end;

procedure TSQL.Select(fields: string);
begin
  if Trim(fields) = '' then
    exit;
  if FSelect = '' then
    FSelect := 'select ';
  FSelect := FSelect + fields;
  SQL_V.Clear;

end;

procedure TSQL.Set_(value_set: string);
begin
  if Trim(value_set) = '' then
    exit;
  FSet := value_set;
  SQL_V.Clear;
end;

function TSQL.SQL: TStringList;
begin
  Result := SQL_V;
end;

function TSQL.Text: string;
begin
  if SQL_V.Text.Trim <> '' then
  begin
    Result := SQL_V.Text;
    Exit;
  end;
  if (FSelect = '') and (FFrom <> '') then
    FSelect := 'select * ';
  SQL_V.Text := FSelect + FFrom + FWhere + FOrder;
  Result := SQL_V.Text;

end;

procedure TSQL.value(value: string);
begin
  if Trim(value) = '' then
    exit;
  FValue := value;
  SQL_V.Clear;
end;

{ TSQLTpl }

constructor TSQLTpl.Create(tpl: string; key: string; sParam: IJObject);
begin
  FTpl := tpl;
  Fkey := key;
  FParam := sParam;
end;

function TSQLTpl.ClearNotes(txt: string): string; //清理注释;
var
    matchs: TStringList;
  match: string;
  text: string;
  RegExpr:tregexprex;
begin
  text := txt;
    RegExpr := TRegExprEx.Create;
  matchs := RegExpr.Matches(text,  '<!--[\s\S]*?-->');

  for match in matchs do
  begin
    text := RegExpr.Replaceexex(text, match,'');
  end;
  RegExpr.free;
  Result := text;
end;

constructor TSQLTpl.Create(tpl: string);
begin
  FTpl := tpl;
  Fkey := '';
  FParam := nil;
end;

destructor TSQLTpl.Destroy;
begin
  inherited;
end;

function TSQLTpl.getSQLKey(sql: string; sKey: string; _T: Boolean = False): string;
var
  key, s: string;
   matchs: TStringList;
  match: string;
  RegExpr:tregexprex;
begin
  s := '';
  if not _T then
    key := '<' + sKey + ' key="' + Fkey + '"[\s\S]*?</' + sKey + '>'
  else
    key := '(?<=<' + sKey + ' key="' + Fkey + '">)[\s\S]*(?=</' + sKey + '>)';
      RegExpr := TRegExprEx.Create;
  matchs := RegExpr.Matches(sql, key);

  for match in matchs do
  begin
    s := match;
    break;
  end;
    RegExpr.free;
  Result := s.Trim;
end;

procedure TSQLTpl.SetKey(key: string; sParam: IJObject);
begin
  Fkey := key;
  FParam := sParam;
end;

procedure TSQLTpl.SetParam(sParam: IJObject);
begin
  FParam := sParam;
end;

procedure TSQLTpl.SetTpl(tpl: string);
begin
  FTpl := tpl;
end;

function TSQLTpl.getSQL(sql: string; sType: string): ISQL;
var
  retsql: string;
  select, from, where, order, insert, edit, del, value, set_: string;
  FSQL: ISQL;
  arr: specialize  TArray<string>;
  fieldname, fieldvalue: string;

  function getkeyvalue(sKey: string): string;
  var
    key, s: string;
     matchs: TStringList;
  match: string;
  RegExpr:tregexprex;
  begin
    s := '';
    key := '(?<=<' + sKey + '>)[\s\S]*(?=</' + sKey + '>)';
     RegExpr := TRegExprEx.Create;
  matchs := RegExpr.Matches(sql, key);
    for match in matchs do
    begin
      s := match;
     // s := sKey + ' ' + s;
      break;
    end;
     RegExpr.free;
    Result := s;
  end;

begin
  FSQL := IISQL;
  select := getkeyvalue('select');
  from := getkeyvalue('from');
  where := getkeyvalue('where');
  order := getkeyvalue('order');
  insert := getkeyvalue('insert');
  edit := getkeyvalue('update');
  value := getkeyvalue('value');
  set_ := getkeyvalue('set');
  del := getkeyvalue('delete');

  FSQL.Select(select);
  FSQL.From(from);
  FSQL.And_(where);
  FSQL.Order(order);
  FSQL.Insert(insert);
  FSQL.Edit(edit);
  FSQL.Del(del);
  FSQL.Value(value);
  FSQL.Set_(set_);

  //
  if Trim(del) <> '' then
    del := 'delete from ' + del;
  if Trim(edit) <> '' then
    edit := 'update ' + edit;
  if Trim(insert) <> '' then
  begin
    insert := 'insert into ' + insert;
    fieldname := '(';
    fieldvalue := '(';
    arr := value.Split([',']);
    for retsql in arr do
    begin
      fieldname := fieldname + retsql.Split(['='])[0] + ',';
      fieldvalue := fieldvalue + retsql.Split(['='])[1] + ',';
    end;
    fieldname := fieldname.Substring(0, fieldname.Length - 1) + ')';
    fieldvalue := fieldvalue.Substring(0, fieldvalue.Length - 1) + ')';
    insert := insert + fieldname + ' value ' + fieldvalue;
  end;

  if Trim(set_) <> '' then
    set_ := 'set ' + set_;
  if Trim(where) <> '' then
    where := 'where ' + where;
  //
  if sType.ToLower = 'querysql' then
  begin
    retsql := FSQL.Text;
    if Trim(retsql) = '' then
      FSQL.SQL.Text := getSQLKey(sql, 'querysql', True)
    else
      FSQL.sql.Text := retsql;
  end;
  if sType.ToLower = 'insertsql' then
  begin
    FSQL.SQL.Text := insert;
  end;
  if sType.ToLower = 'updatesql' then
  begin
    FSQL.SQL.Text := edit + ' ' + set_ + ' ' + where;
  end;
  if sType.ToLower = 'deletesql' then
  begin
    FSQL.SQL.Text := del + ' ' + where;
  end;
  if sType.ToLower = 'procsql' then
  begin
    FSQL.SQL.Text := getSQLKey(sql, 'procsql', True)
  end;
  Result := FSQL;
end;

function TSQLTpl.AsISQL: ISQL;
var
  TplContent, params: TStringList;
 Parser: TTplParser;
 sql: string;
begin
  Result := nil;
  if (FTpl.Trim = '') or (Fkey.Trim = '') then
    exit;
  TplContent := TStringList.Create;
  params := TStringList.Create;
  Parser := TTplParser.create;
  if FParam <> nil then
    params.Values['_Param'] := FParam.toJSON;

  try
    TplContent.Text := SQLCache.LoadPage(FTpl);
    TplContent.Text := ClearNotes(TplContent.Text);
    if TplContent.Text = '' then
    begin
      Log(FTpl + '文件不存在');
      exit;
    end;
    sql := getSQLKey(TplContent.Text, 'QuerySQL');
    if sql <> '' then
    begin
      Parser.Parser(sql, params, '');
      Result := getSQL(sql, 'querysql');  //解析成功该进行sql拼接组装了。
      exit;
    end;
    sql := getSQLKey(TplContent.Text, 'InsertSQL');
    if sql <> '' then
    begin
      Parser.Parser(sql, params, '');
      Result := getSQL(sql, 'InsertSQL');  //解析成功该进行sql拼接组装了。
      exit;
    end;
    sql := getSQLKey(TplContent.Text, 'UpdateSQL');
    if sql <> '' then
    begin
      Parser.Parser(sql, params, '');
      Result := getSQL(sql, 'UpdateSQL');  //解析成功该进行sql拼接组装了。
      exit;
    end;
    sql := getSQLKey(TplContent.Text, 'DeleteSQL');
    if sql <> '' then
    begin
      Parser.Parser(sql, params, '');
      Result := getSQL(sql, 'DeleteSQL');  //解析成功该进行sql拼接组装了。
      exit;
    end;
    sql := getSQLKey(TplContent.Text, 'ProcSQL');
    if sql <> '' then
    begin
      Parser.Parser(sql, params, '');
      Result := getSQL(sql, 'ProcSQL');  //解析成功该进行sql拼接组装了。
      exit;
    end;
  finally
    params.Free;
    TplContent.Free;
    Parser.free;
  end;

end;

{ TResData }

procedure TResData.value(sCode: Integer; sMsg: string);
begin
  Self.Code := sCode;
  Self.Msg := sMsg;
end;

end.
