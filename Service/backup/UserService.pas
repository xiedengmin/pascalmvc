unit UserService;

interface

uses
  SysUtils, Classes, MVC.App, MVC.JSON, MVC.DataSet, MVC.Service, TableMap,
  MVC.DB;

type
  TUserService = class
  public
    function getData(map: IJObject): IDataSet;
    function getAllData(map: IJObject): IDataSet;
    function save(map: IJObject): boolean;
    function Del(id: string): boolean;
  end;

implementation

uses  SQLMap;

{ TuserService }

function TUserService.Del(id: string): boolean;
  //var
  //  conn: IConn;
begin
  // conn := IIConn;
  Result := mysql.DelByKey(Tb_Users, 'id', id);
end;

function TUserService.getAllData(map: IJObject): IDataSet;
var
  sql: ISQL;
  roleid: string;
  // conn: IConn;
begin
  // conn := IIConn;
  roleid := map.GetS('roleid');
  sql := IISQL(Tb_Users);
  if roleid <> '0' then
    sql.AndEq('roleid', roleid);
  Result := mysql.Find(sql.Text);
end;

function TUserService.getData(map: IJObject): IDataSet;
var
  sql: ISQL;
  page, limit: integer;
  roleid, limits: string;
  // conn: IConn;
begin
  //  conn := IIConn;
  page := StrToInt(map.Gets('page'));
  limit := StrToInt(map.Gets('limit'));
  roleid := map.GetS('roleid');
  sql := IISQL(Tb_Users);
  if roleid <> '0' then
    sql.And_('roleid=' + roleid);
  Result := mysql.Find(sql, page - 1, limit);
end;

function TUserService.save(map: IJObject): boolean;
var
  ret: IDataSet;
  id: string;
  sql: ISQL;
  temp: string;
  // conn: IConn;
begin
  //conn := IIConn;
  id := map.GetS('id');
  sql := IISQL;
  mysql.StartTransaction; // 事务启动
  try
    if id = '' then
    begin
      // map.Delete('id');
      sql.From(Tb_Users);
      sql.AndEqF('username', map.GetS('username'));

      ret := mysql.Find(sql.Text);
      if ret.IsEmpty then
      begin
        with mysql.Add(Tb_Users) do
        begin
          FieldByName('username').AsString := map.GetS('username');
          FieldByName('realname').AsString := map.GetS('realname');
          FieldByName('roleid').AsString := map.GetS('roleid');
          FieldByName('pwd').AsString := map.GetS('pwd');
          FieldByName('uptime').AsDateTime := Now;
          Post;
          Result := True;
        end;
      end
      else
      begin
        Result := False;
      end;
    end
    else
    begin
      sql.Clear;
      sql.AndNe('id', id);
      sql.AndEqF('username', map.GetS('username'));

      sql.From(Tb_Users);
      ret := mysql.Find(sql.Text);
      if ret.IsEmpty then
      begin
        with mysql.Edit(Tb_Users, 'id', id) do
        begin
          FieldByName('username').AsString := map.GetS('username');
          FieldByName('realname').AsString := map.GetS('realname');
          FieldByName('roleid').AsString := map.GetS('roleid');
          FieldByName('uptime').AsDateTime := Now;
          //    temp:= FieldByName('realname').AsString ;
          Post;
          ApplyUpdates;

            close;
          Result := True;
        end;
      end
      else
      begin
        Result := False;
      end;
    end;
           mysql.Commit; // 事务执行
//    mysql.InTransation;
  except
    mysql.Rollback; // 事务回滚
    Result := False;
  end;
end;

end.
