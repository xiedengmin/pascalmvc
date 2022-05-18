unit mvc.dbSQLite;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MVC.Tool, MVC.LogUnit, mvc.Config, mvc.JSON, MVC.DataSet,
  Generics.Collections, SQLDB, DB, fpjson, SQLite3Conn;

type

  { TSQLiteDbItem }

  TSQLiteDbItem = class
  private
    Fdbname: string;
    FID: string;
    FOverTime: TDateTime;
    FState: integer;
    function filterSQL(sql: string): string;
    procedure Setdbname(AValue: string);
    procedure SetID(AValue: string);
    procedure SetOverTime(AValue: TDateTime);
    procedure SetState(AValue: integer);
    procedure setParams();
    function TryConn: boolean;
  public
    TMP_CDS: TSQLQuery;
    SQLTran: TSQLTransaction;
    Conn: TSQLite3Connection;
    property dbname: string read Fdbname write Setdbname;
    property ID: string read FID write SetID;
    property OverTime: TDateTime read FOverTime write SetOverTime;
    property State: integer read FState write SetState;
    function Find(sql: string): IDataSet;
    constructor Create;
    destructor Destroy; override;
  end;

  { TSQLiteDbPool }

  TSQLiteDbPool = class(TThread)
  private

    DBList: specialize TDictionary<string, TSQLiteDbItem>;
    procedure ClearAction;
  protected
    procedure Execute; override;
  public
    procedure setParams(dbname: string);
    function getDbItem(dbname: string): TSQLiteDbItem;
    procedure freeDbItem(dbitem: TSQLiteDbItem);
    constructor Create;
    destructor Destroy; override;
  end;

  { TSQLiteDB }

  TSQLiteDB = class(TSQLiteDbItem)
  private
    dbitem: TSQLiteDbItem;
  public
    constructor Create(sDbName: string);
    destructor Destroy; override;
  end;

var
  SQLitePool: TSQLiteDbPool;

implementation

{ TSQLiteDB }

constructor TSQLiteDB.Create(sDbName: string);
begin
  lock();
  if not Assigned(SQLitePool) then
    SQLitePool := TSQLiteDbPool.Create;
  UnLock();
  dbitem := SQLitePool.getDbItem(sDbName);
  Conn := dbitem.Conn;
end;

destructor TSQLiteDB.Destroy;
begin
  if Assigned(dbitem) then
    SQLitePool.freeDbItem(dbitem);
end;

{ TSQLiteDbPool }

procedure TSQLiteDbPool.Execute;
var
  k: integer;
begin
  k := 0;
  while not Terminated do
  begin
    try
      Inc(k);
      if k >= 1000 then
      begin
        k := 0;
        try
          self.ClearAction;
        except
          on e: Exception do
            log(e.Message);
        end;
      end;
    finally
      Sleep(10);
    end;
  end;
end;

procedure TSQLiteDbPool.setParams(dbname: string);
begin

end;

procedure TSQLiteDbPool.ClearAction;
var
  item: TSQLiteDbItem;
  key: string;
  tmp_dblist: specialize TDictionary<string, TSQLiteDbItem>;
begin
  if DBList.Count < 2 then
    exit;
  Lock();
  tmp_dblist := specialize TDictionary<string, TSQLiteDbItem>.Create();
  for key in DBList.keys do
  begin
    tmp_dblist.AddOrSetValue(key, Dblist[key]);
  end;
  UnLock();
  try
    for key in tmp_dblist.Keys do
    begin

      DBlist.TryGetValue(key, item);
      if Assigned(item) then
      begin
        if (Now() > item.OverTime) and (item.State = 0) then
        begin
          Lock();
          item.State := 2;
          DBList.AddOrSetValue(item.ID, item);
          UnLock();
          Break;
        end
        else if item.State = 2 then
        begin
          Lock();
          DBList.Remove(item.ID);
          item.Free;
          UnLock();
          Break;
        end;
      end;
      Sleep(100);
    end;
  finally
    tmp_dblist.Clear;
    tmp_dblist.Free;
  end;
end;

function TSQLiteDbPool.getDbItem(dbname: string): TSQLiteDbItem;
var
  key: string;
  item: TSQLiteDbItem;
  findDb: boolean;
begin
  findDb := False;
  Lock();
  for key in DBList.Keys do
  begin
    if dblist.TryGetValue(key, item) then
    begin
      if (item.State = 0) and (item.dbname = dbname) then
      begin
        findDb := True;
        Break;
      end;
    end;
  end;
  if not findDb then
  begin
    item := TSQLiteDbItem.Create();
    item.ID := IITool.GetGUID;
    item.dbname := dbname;
    item.setParams;
    // item.Conn := item.Conn;
  end;

  item.State := 1; //修改为使用中状态
  item.OverTime := Now + (1 / 24 / 60) * 1;
  DBList.AddOrSetValue(item.ID, item);
  UnLock();
  Result := item;

end;

procedure TSQLiteDbPool.freeDbItem(dbitem: TSQLiteDbItem);
begin
  Lock();
  if dbitem <> nil then
  begin
    dbitem.State := 0;
    DBList.AddOrSetValue(dbitem.ID, dbitem);
  end;
  UnLock();
end;

constructor TSQLiteDbPool.Create;
begin
  DBList := specialize TDictionary<string, TSQLiteDbItem>.Create();
  inherited Create(False);
end;

destructor TSQLiteDbPool.Destroy;
begin

  DBList.Clear;
  DBList.Free;
  inherited Destroy;
end;

{ TSQLiteDbItem }

procedure TSQLiteDbItem.SetState(AValue: integer);
begin
  if FState = AValue then Exit;
  FState := AValue;
end;

procedure TSQLiteDbItem.setParams;
var
  joConfig: IJObject;
  jo, jo1: TJSONObject;
  db: TJSONData;
begin
  joConfig := IIJObject(Config.DBConfig);
  jo1 := joConfig.O;
  db := jo1.Find(dbname);
  if db <> nil then
  begin
    jo := TJSONObject(jo1.Find(dbname));
    if jo <> nil then
    begin
      with conn do
      begin
        DatabaseName := jo.get('Database');
        UserName := jo.get('User_Name');
        Password := jo.get('Password');
        CharSet := jo.get('CharacterSet');
      end;
    end;
  end;
end;

function TSQLiteDbItem.filterSQL(sql: string): string;
begin
  if Config.show_sql then
    log(sql);
  // Result := sql.Replace(';', '').Replace('-', '');
  Result := sql;
end;

function TSQLiteDbItem.TryConn: boolean;
begin
  if Conn = nil then
  begin
    Result := False;
    exit;
  end;
  try
    if not Conn.Connected then
      Conn.Connected := True;
    if TMP_CDS = nil then
    begin
      TMP_CDS := TSQLQuery.Create(nil);
      TMP_CDS.DataBase := Conn;
    end;
    if Conn.Connected then
    begin
      LogDebug('数据库链接成功');
      Result := True;
    end
    else
    begin
      Result := False;
      LogDebug('数据库链接失败');
    end;

  except
    on e: Exception do
    begin
      log(e.Message);
      Result := False;

    end;
  end;
end;

function TSQLiteDbItem.Find(sql: string): IDataSet;
var
  cds: IDataSet;
  s: string;
begin
  Result := nil;

  if TryConn then
  begin
    try
      cds := IIDataSet;
      cds.DS.DataBase := Conn;
      s := filterSQL(sql);
      cds.DS.SQL.Text := s;
      cds.DS.Open;
      Result := cds;
    except
      on e: Exception do
      begin
        log('SQL执行异常:' + e.Message);
        Result := nil;
      end;

    end;
  end;

end;

procedure TSQLiteDbItem.SetOverTime(AValue: TDateTime);
begin
  if FOverTime = AValue then Exit;
  FOverTime := AValue;
end;

procedure TSQLiteDbItem.SetID(AValue: string);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TSQLiteDbItem.Setdbname(AValue: string);
begin
  if Fdbname = AValue then Exit;
  Fdbname := AValue;
end;

constructor TSQLiteDbItem.Create;
begin
  Conn := TSQLite3Connection.Create(nil);
  SQLTran := TSQLTransaction.Create(nil);
  SQLTran.DataBase := Conn;
end;

destructor TSQLiteDbItem.Destroy;
begin
  if conn.Connected then
    Conn.Connected := False;
  conn.Free;
  SQLTran.Free;
  inherited Destroy;
end;

initialization


finalization
  if Assigned(SQLitePool) then
    SQLitePool.Free;
end.
