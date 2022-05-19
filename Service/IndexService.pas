unit IndexService;

interface

uses
  SysUtils,Classes, MVC.JSON, MVC.DataSet, MVC.DB;

type
  TIndexService = record
    //function checkuser(map: IJObject): IDataSet;
   // function getdata: IDataSet;
   // function getone: IDataSet;
  end;

implementation

uses
  TableMap, SQLMap;

{ TIndexService }
  {
function TIndexService.checkuser(map: IJObject): IDataSet;
var
  sql: ISQL;
  conn: IConn;
begin
  conn := IIConn;
  sql := IISQL(Tb_Uses);
  sql.AndEqF('username', map.GetS('username'));
  sql.AndEqF('pwd', map.GetS('pwd'));
  Result := conn.DB.Find(sql);
end;

function TIndexService.getdata: IDataSet;
var
  sql: ISQLTpl;
  map: IJObject;
begin
  map := IIJObject();
  map.SetS('name', '����');

  sql := IISQLTpl('sql\user.xml', 'getall', map);
  Result := IIConn.DB.use('db2').Find(sql);
end;

function TIndexService.getone: IDataSet;
var
  map: IJObject;
  sql: ISQLTpl;
  // s: Isql;
  ds: IDataSet;
  conn: IConn;
begin
  conn := IIConn;
  map := IIJObject();
  map.SetS('id', '1');
  map.SetS('name', '����');
  map.SetS('sex', '��');
  map.SetS('age', '12');
  // sql := IISQLTpl(sql_users);
  // sql := IISQLTpl('sql\user.xml', 'getone', map);
  // s := sql.AsISQL;
  // sql := IISQLTpl('sql\user.xml', 'saveuser', map);
  // sql := IISQLTpl('sql\user.xml', 'mysql', map);
  // sql := IISQLTpl('sql\user.xml', 'edituser', map);
  // sql := IISQLTpl('sql\user.xml', 'del', map);
  // s := sql.AsISQL;
  // sql := IISQLTpl('sql\user.xml', 'edituser', map);
  // sql.SetKey('saveuser', map);
  // var i: integer := conn.db.use('db2').ExecSQL(sql);
  sql.SetKey('mysql');
  ds := conn.DB.use('db2').Find(sql);
  sql.SetKey('testproc', map);
  ds := conn.DB.use('db2').Find(sql);
  Result := ds;
end;     }

end.
