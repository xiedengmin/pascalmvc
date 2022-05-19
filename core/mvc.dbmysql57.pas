unit mvc.dbMySql57;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MVC.Tool, MVC.LogUnit, mvc.Config, mvc.JSON, MVC.DataSet,
  Generics.Collections, SQLDB, DB, fpjson, mysql57conn, mvc.Query;

type

  { TMySql57DbItem }

  TMySql57DbItem = class
  private
    Fdbname: string;
    FID: string;
    FOverTime: TDateTime;
    FState: integer;
    FDriverName: string;
    function filterSQL(sql: string): string;
    procedure Setdbname(AValue: string);
    procedure SetID(AValue: string);
    procedure SetOverTime(AValue: TDateTime);
    procedure SetState(AValue: integer);
    procedure setParams();
    function PageMySql(sql: ISQL; pNumber, pSize: integer): IDataSet;
    procedure SetDriverName(const Value: string);

  public
    TMP_CDS: TQuery;
    SQLTran: TSQLTransaction;
    Conn: TMySQL57Connection;
    property dbname: string read Fdbname write Setdbname;
    property ID: string read FID write SetID;
    property OverTime: TDateTime read FOverTime write SetOverTime;
    property DriverName: string read FDriverName write SetDriverName;
    property State: integer read FState write SetState;
    // 工作状态0未工作1工作中2待回收
    function TryConn: boolean;
    procedure StartTransaction(); //启动事务
    procedure Commit;        //事务提交
    procedure Rollback;      //事务回滚
    function InTransation: boolean;
    function ExecSQL(sql: string): boolean;
    function ExecSQL(sqltpl: ISQLTpl): IDataSet; overload;
    function ExecSQL_Ds(sql: string): IDataSet; overload;
    function Find(sql: string): IDataSet;
    function Find(sql: ISQL; pNumber: integer; pSize: integer): IDataSet;
    function Find(sqltpl: ISQLTpl; pNumber: integer; pSize: integer): IDataSet;
      overload; //分页查询
    function FindByKey(tablename: string; key: string; Value: string): IDataSet;
    function Add(tablename: string): TSQLQuery;
    function Edit(tablename: string; key: string; Value: string): TSQLQuery;
    function DelByKey(tablename: string; key: string; Value: string): boolean;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMySql57DbPool }

  TMySql57DbPool = class(TThread)
  private

    DBList: specialize TDictionary<string, TMySql57DbItem>;
    procedure ClearAction;
  protected
    procedure Execute; override;
  public
    procedure setParams(dbname: string);
    function getDbItem(dbname: string): TMySql57DbItem;
    procedure freeDbItem(dbitem: TMySql57DbItem);
    constructor Create;
    destructor Destroy; override;
  end;

  { TMySql57DB }

  TMySql57DB = class(TMySql57DbItem)
  private
    dbitem: TMySql57DbItem;
  public
    constructor Create(sDbName: string);
    destructor Destroy; override;
  end;

var
  MySQL57Pool: TMySql57DbPool;

implementation

{ TMySql57DB }

constructor TMySql57DB.Create(sDbName: string);
var
  charset: string;
begin
  lock();
  if not Assigned(MySQL57Pool) then
    MySQL57Pool := TMySQL57DbPool.Create;
  UnLock();
  SetDriverName('mysql');
  dbitem := MySQL57Pool.getDbItem(sDbName);

  Conn := dbitem.Conn;
  SQLTran := dbitem.SQLTran;
  SQLTran.DataBase := conn;
  //搞笑
  charset := conn.CharSet;
end;

destructor TMySql57DB.Destroy;
begin
  if Assigned(dbitem) then
    MySQL57Pool.freeDbItem(dbitem);
  // inherited Destroy;
end;

{ TMySql57DbPool }

procedure TMySql57DbPool.Execute;
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

procedure TMySql57DbPool.setParams(dbname: string);
begin

end;


procedure TMySql57DbPool.ClearAction;
var
  item: TMySql57DbItem;
  key: string;
  tmp_dblist: specialize TDictionary<string, TMySql57DbItem>;
begin
  if DBList.Count < 2 then
    exit;
  Lock();
  tmp_dblist := specialize TDictionary<string, TMySql57DbItem>.Create();
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

function TMySql57DbPool.getDbItem(dbname: string): TMySql57DbItem;
var
  key: string;
  item: TMySql57DbItem;
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
    item := TMySql57DbItem.Create();
    item.ID := IITool.GetGUID;
    item.dbname := dbname;
    item.setParams;

  end;

  item.State := 1; //修改为使用中状态
  item.OverTime := Now + (1 / 24 / 60) * 100;
  DBList.AddOrSetValue(item.ID, item);
  UnLock();
  Result := item;

end;

procedure TMySql57DbPool.freeDbItem(dbitem: TMySql57DbItem);
begin
  Lock();
  if dbitem <> nil then
  begin
    dbitem.State := 0;
    DBList.AddOrSetValue(dbitem.ID, dbitem);
  end;
  UnLock();
end;

constructor TMySql57DbPool.Create;
begin
  DBList := specialize TDictionary<string, TMySql57DbItem>.Create();
  inherited Create(False);
end;

destructor TMySql57DbPool.Destroy;
begin

  DBList.Clear;
  DBList.Free;
  inherited Destroy;
end;

{ TMySql57DbItem }

procedure TMySql57DbItem.SetDriverName(const Value: string);
begin
  FDriverName := Value;
end;

procedure TMySql57DbItem.SetState(AValue: integer);
begin
  if FState = AValue then Exit;
  FState := AValue;
end;

procedure TMySql57DbItem.setParams;
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

function TMySql57DbItem.filterSQL(sql: string): string;
begin
  if Config.show_sql then
    log(sql);
  // Result := sql.Replace(';', '').Replace('-', '');
  Result := sql;
end;

function TMySql57DbItem.TryConn: boolean;
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
      TMP_CDS := TQuery.Create(nil);
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

function TMySql57DbItem.ExecSQL(sqltpl: ISQLTpl): IDataSet;
var
  sql: string;
begin
  sql := sqltpl.AsISQL.SQL.Text;
  Result := ExecSQL_ds(sql);
end;


function TMySql57DbItem.ExecSQL(sql: string): boolean;
var
  SQLScript: TSQLScript;
begin
  if not TryConn then
    Exit;
  if (Trim(sql) = '') then
    Exit;
  SQLScript := TSQLScript.Create(nil);
  try
    try
      sql := filterSQL(sql);
      SQLScript.DataBase := Conn;
      SQLScript.Transaction := conn.Transaction;
      SQLScript.AutoCommit := True;
      SQLScript.Script.Text := sql;
      SQLScript.Execute;
      Result := True;

    except
      on e: Exception do
      begin
        log('SQL执行异常:' + e.Message + '-' + sql);
        Result := False;
      end;
    end;

  finally
    SQLScript.Free;
  end;
end;

function TMySql57DbItem.ExecSQL_Ds(sql: string): IDataSet;
var
  cds: IDataSet;
begin
  Result := nil;
  if not TryConn then
    Exit;
  if (Trim(sql) = '') then
    Exit;
  try
    sql := filterSQL(sql);
    cds := IIDataSet;
    cds.DS.Close;
    cds.DS.DataBase := Conn;
    cds.DS.SQL.Text := sql;
    cds.DS.Open;
    Result := cds;

  except
    on e: Exception do
    begin
      log('SQL执行异常:' + e.Message + '-' + sql);
      Result := nil;
    end;
  end;
end;

function TMySql57DbItem.Find(sql: string): IDataSet;
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
      cds.DS.Close;
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

function TMySql57DbItem.Find(sqltpl: ISQLTpl; pNumber, pSize: integer): IDataSet;
begin
  Result := find(sqltpl.AsISQL, pNumber, pSize);
end;

function TMySql57DbItem.Find(sql: ISQL; pNumber, pSize: integer): IDataSet;
var
  device, mssqlver, ver: string;
begin

  device := DriverName;
  if device.ToLower = 'mysql' then
  begin
    Result := PageMySql(sql, pNumber, pSize);
  end
  else if device.ToLower = 'fb' then
  begin
    // Result := PageFireBird(sql, pNumber, pSize);
  end
  else if device.ToLower = 'sqlite' then
  begin
    // Result := PageSqlite(sql, pNumber, pSize);
  end
  else if device.ToLower = 'ora' then
  begin
    //    Result := PageOracle(sql, pNumber, pSize);
  end
  else if device.ToLower = 'mssql' then
  begin
    //如果是mssql 数据库 判断当前所使用版本来使用
    {SELECT SERVERPROPERTY('ProductVersion') AS 实例版本}
   { mssqlver := GetMSSQLVer;
    ver := mssqlver.Split(['.'])[0];
    if ver.ToInteger = 10 then   // 版本是10 是 mssql2008
    begin
      Result := PageMSSQL08(sql, pNumber, pSize);
    end
    else if ver.ToInteger > 10 then   // 大于 10，11：mssql2012;12 mssql2014;13 mssql2016;14mssql2017;
    begin
      Result := PageMSSQL12(sql, pNumber, pSize);
    end
    else if ver.ToInteger = 8 then  //2000版本
    begin
      Result := PageMSSQL(sql, pNumber, pSize);
    end; }
  end
  else
    Result := nil;
end;

function TMySQL57DbItem.FindByKey(tablename: string; key: string;
  Value: string): IDataSet;
var
  sql: string;
begin

  try
    if not TryConn then
      Exit;
    if (Trim(tablename) = '') then
      Exit;
    if (Trim(key) = '') then
      Exit;
    if (Trim(Value) = '') then
      Exit;
    sql := 'select * from ' + tablename + ' where ' + key + ' = ' + QuotedStr(Value);
    Result := find(sql);
  except
    on e: Exception do
    begin
      log(e.Message);
      Result := nil;
    end;

  end;
end;



procedure TMySql57DbItem.StartTransaction;
begin
  if not TryConn then
    Exit;
  if  not conn.Transaction.Active then
  begin
  SQLTran.StartTransaction;
  end;
end;

procedure TMySql57DbItem.Commit;
begin
  if not TryConn then
    Exit;
  SQLTran.Commit;
 // conn.CloseDataSets;
 // conn.CloseTransactions;
 // SQLTran.EndTransaction;
 // sqltran.CloseDataSets;
 // sqltran.Active := False;
end;

procedure TMySql57DbItem.Rollback;
begin
  if not TryConn then
    Exit;
  SQLTran.Rollback;
end;

function TMySQL57DbItem.InTransation: boolean;
var
  trancount: integer;
begin
  trancount := Conn.TransactionCount;

  Result := Conn.TransactionCount > 0;
end;

function TMySql57DbItem.Add(tablename: string): TSQLQuery;
var
  sql: string;
begin
  Result := nil;
  if not TryConn then
    Exit;
  if (Trim(tablename) = '') then
    Exit;
  try
    sql := filterSQL(sql);
    sql := 'select * from ' + tablename + ' where 1=2';
    TMP_CDS.Close;
    TMP_CDS.SQLConnection := Conn;
    TMP_CDS.SQL.Text := sql;
    TMP_CDS.Open;
    TMP_CDS.Append;
    Result := TMP_CDS;
  except
    on e: Exception do
    begin
      Result := nil;
      log(e.ToString);
    end;
  end;
end;

function TMySql57DbItem.Edit(tablename, key, Value: string): TSQLQuery;
var
  sql: string;
begin
  Result := nil;
  if not TryConn then
    Exit;
  if (Trim(tablename) = '') then
    Exit;
  if (Trim(key) = '') then
    Exit;
  try
    sql := 'select * from ' + tablename + ' where ' + key + ' = ' + Value;
    sql := filterSQL(sql);

    TMP_CDS.SQLConnection := Conn;
    TMP_CDS.Close;
    TMP_CDS.SQL.Text := sql;
    TMP_CDS.Open();
    if (not TMP_CDS.IsEmpty) then
    begin
      //  TMP_CDS.First;
      TMP_CDS.Edit;

      Result := TMP_CDS;
    end
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

function TMySql57DbItem.DelByKey(tablename, key, Value: string): boolean;
var
  sql: string;
begin
  Result := False;
  if not TryConn then
    Exit;
  if (Trim(tablename) = '') then
    Exit;
  if (Trim(key) = '') then
    Exit;
  if (Trim(Value) = '') then
    Exit;
  try
    sql := 'delete from ' + tablename + ' where ' + key + '=' + Value;
    Result := ExecSQL(sql);
  except
    on e: Exception do
    begin
      log('SQL执行异常:' + e.Message + '-' + sql);
      Result := False;
    end;
  end;
end;

function TMySql57DbItem.PageMySql(sql: ISQL; pNumber, pSize: integer): IDataSet;
var
  sq: string;
  Count: integer;
  jo: TJSONObject;
  dataset, dscount: IDataSet;
begin
  dataset := nil;
  if (not TryConn) or (Trim(sql.getSelect) = '') or (Trim(sql.getFrom) = '') then
    Exit;
  try
    try
      sq := 'select count(1) as N' + sql.getFrom;
      sq := filterSQL(sq);
      dscount := ExecSQL_ds(sq);
      Count := dscount.DS.FieldValues['N'];
      // count:=jo.get('N')
      sq := sql.getSelect + sql.getFrom + ' ' + sql.getOrder +
        ' limit ' + IntToStr(pNumber * pSize) + ',' + IntToStr(pSize);
      dataset := ExecSQL_Ds(sq);
      dataset.setCount(Count);
    except
      on e: Exception do
      begin
        log('SQL执行异常:' + e.Message + '-' + sq);
        Result := nil;
      end;
    end;
  finally
    Result := dataset;
  end;
end;

procedure TMySql57DbItem.SetOverTime(AValue: TDateTime);
begin
  if FOverTime = AValue then Exit;
  FOverTime := AValue;
end;

procedure TMySql57DbItem.SetID(AValue: string);
begin
  if FID = AValue then Exit;
  FID := AValue;
end;

procedure TMySql57DbItem.Setdbname(AValue: string);
begin
  if Fdbname = AValue then Exit;
  Fdbname := AValue;
end;

constructor TMySql57DbItem.Create;
begin
  Conn := TMySql57Connection.Create(nil);
  SQLTran := TSQLTransaction.Create(nil);
  SQLTran.DataBase := Conn;
  conn.Transaction := SQLTran;
end;

destructor TMySql57DbItem.Destroy;
begin
  if conn.Connected then
    Conn.Connected := False;
  conn.Free;
  SQLTran.Free;
  if Assigned(TMP_CDS) then
    TMP_CDS.Free;
  inherited Destroy;
end;

initialization
  //MySQL57Pool := TMySql57DbPool.Create;

finalization
  if Assigned(MySQL57Pool) then
    MySQL57Pool.Free;
end.
