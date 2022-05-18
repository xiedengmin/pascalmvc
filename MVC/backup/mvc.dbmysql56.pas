unit mvc.dbMySql56;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MVC.Tool, MVC.LogUnit, mvc.Config, mvc.JSON, MVC.DataSet,
  Generics.Collections, SQLDB, DB, fpjson, mysql56conn;

type

  { TMySQL56DbItem }

  TMySQL56DbItem = class
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
    Conn: TMySQL57Connection;
    property dbname: string read Fdbname write Setdbname;
    property ID: string read FID write SetID;
    property OverTime: TDateTime read FOverTime write SetOverTime;
    property State: integer read FState write SetState;
    // 工作状态0未工作1工作中2待回收
    function Find(sql: string): IDataSet;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMySQL56DbPool }

  TMySQL56DbPool = class(TThread)
  private

    DBList: specialize TDictionary<string, TMySQL56DbItem>;
    procedure ClearAction;
  protected
    procedure Execute; override;
  public
    procedure setParams(dbname: string);
    function getDbItem(dbname: string): TMySQL56DbItem;
    procedure freeDbItem(dbitem: TMySQL56DbItem);
    constructor Create;
    destructor Destroy; override;
  end;

  { TMySQL56DB }

  TMySQL56DB = class(TMySQL56DbItem)
  private
    dbitem: TMySQL56DbItem;
  public
    constructor Create(sDbName: string);
    destructor Destroy; override;
  end;

var
  MySQL65Pool: TMySQL56DbPool;

implementation

{ TMySQL56DB }

constructor TMySQL56DB.Create(sDbName: string);
begin
  dbitem := MySQL65Pool.getDbItem(sDbName);
  Conn := dbitem.Conn;
end;

destructor TMySQL56DB.Destroy;
begin
  if Assigned(dbitem) then
    MySQL65Pool.freeDbItem(dbitem);
  // inherited Destroy;
end;

{ TMySQL56DbPool }

procedure TMySQL56DbPool.Execute;
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

procedure TMySQL56DbPool.setParams(dbname: string);
begin

end;

procedure TMySQL56DbPool.ClearAction;
var
  item: TMySQL56DbItem;
  key: string;
  tmp_dblist: specialize TDictionary<string, TMySQL56DbItem>;
begin
  if DBList.Count < 2 then
    exit;
  Lock(DBList);
  tmp_dblist := specialize TDictionary<string, TMySQL56DbItem>.Create();
  for key in DBList.keys do
  begin
    tmp_dblist.AddOrSetValue(key, Dblist[key]);
  end;
  UnLock(DBList);
  try
    for key in tmp_dblist.Keys do
    begin

      DBlist.TryGetValue(key, item);
      if Assigned(item) then
      begin
        if (Now() > item.OverTime) and (item.State = 0) then
        begin
          Lock(DBList);
          item.State := 2;
          DBList.AddOrSetValue(item.ID, item);
          UnLock(DBList);
          Break;
        end
        else if item.State = 2 then
        begin
          Lock(DBList);
          DBList.Remove(item.ID);
          item.Free;
          UnLock(DBList);
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

function TMySQL56DbPool.getDbItem(dbname: string): TMySQL56DbItem;
var
  key: string;
  item: TMySQL56DbItem;
  findDb: boolean;
begin
  findDb := False;
  Lock(DBList);
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
    item := TMySQL56DbItem.Create();
    item.ID := IITool.GetGUID;
    item.dbname := dbname;
    item.setParams;
    // item.Conn := item.Conn;
  end;

  item.State := 1; //修改为使用中状态
  item.OverTime := Now + (1 / 24 / 60) * 1;
  DBList.AddOrSetValue(item.ID, item);
  UnLock(DBList);
  Result := item;

end;

procedure TMySQL56DbPool.freeDbItem(dbitem: TMySQL56DbItem);
begin
  Lock(DBList);
  if dbitem <> nil then
  begin
    dbitem.State := 0;
    DBList.AddOrSetValue(dbitem.ID, dbitem);
  end;
  UnLock(DBList);
end;

constructor TMySQL56DbPool.Create;
begin
  DBList := specialize TDictionary<string, TMySQL56DbItem>.Create();
  inherited Create(False);
end;

destructor TMySQL56DbPool.Destroy;
begin

  DBList.Clear;
  DBList.Free;
  inherited Destroy;
end;

{ TMySQL56DbItem }

procedure TMySQL56DbItem.SetState(AValue: integer);
begin
  if FState = AValue then Exit;
  FState := AValue;
end;

procedure TMySQL56DbItem.setParams;
var
  joConfig: IJObject;
  jo, jo1: TJSONObject;
  db: TJSONData;
  sPort: integer;
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
        HostName := jo.get('Server');
        sPort := jo.Get('Port');
        Port := sPort;
        DatabaseName := jo.get('Database');
        UserName := jo.get('User_Name');
        Password := jo.get('Password');
        CharSet := jo.get('CharacterSet');
      end;
    end;
  end;
end;

function TMySQL56DbItem.filterSQL(sql: string): string;
begin
  if Config.show_sql then
    log(sql);
  // Result := sql.Replace(';', '').Replace('-', '');
  Result := sql;
end;

function TMySQL56DbItem.TryConn: boolean;
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

function TMySQL56DbItem.Find(sql: string): IDataSet;
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

procedure TMySQL56DbItem.SetOverTime(AValue: TDateTime);
begin
  if FOverTime = AValue then Exit;
  FOverTime := AValue;
end;

procedure TMySQL56DbItem.SetID(AValue: string);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TMySQL56DbItem.Setdbname(AValue: string);
begin
  if Fdbname = AValue then Exit;
  Fdbname := AValue;
end;

constructor TMySQL56DbItem.Create;
begin
  Conn := TMySQL56Connection.Create(nil);
  SQLTran := TSQLTransaction.Create(nil);
  SQLTran.DataBase := Conn;
end;

destructor TMySQL56DbItem.Destroy;
begin
  if conn.Connected then
    Conn.Connected := False;
  conn.Free;
  SQLTran.Free;
  inherited Destroy;
end;

initialization
  MySQL65Pool := TMySQL56DbPool.Create;

finalization
  MySQL65Pool.Free;
end.
