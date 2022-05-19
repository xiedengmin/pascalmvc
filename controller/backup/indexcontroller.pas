unit IndexController;

{$mode ObjFPC}{$M+}

interface

uses
  Classes, SysUtils,contnrs,BaseController, MVC.App, MVC.JSON,
  MVC.DataSet, MVC.service, superobject, MVCModel, mormot.DB.sql, mormot.orm.core,
  mormot.core.base,
  mormot.core.data;

type

  { TIndexController }

  TIndexController = class(TBaseController)
  private

  published
    procedure index;
    procedure login;
    procedure check; // check 没有设置 请求路径，将以check 路径访问
    procedure verifycode;
    procedure getdata;
    procedure getjson;
    procedure save;
    function Intercept: boolean; override;
  end;

implementation

uses TableMap, SQLMap;

{ TIndexController }
procedure TIndexController.verifycode;
begin
  ShowText('data:image/jpeg;base64,' + getVCode);
end;

procedure TIndexController.check;
var
  vcode, scode: string;
  map: IJObject;
  ds: IDataSet;
  username,password: string;
  json: IDataSet;
  sql: ISQL;
  jo: ISuperObject;
  jwt:utf8string;
begin
  vcode := input('vcode');
  vcode := uppercase(vcode);
  scode := Session.getValue('vcode');
  scode := uppercase(scode);
  map := InputToJSON;
  if map<>nil then// ds := Service.index.checkuser(map);
  begin
  json := mysql.Find('select * from users where username="' +
    map.GetS('username') + '" and pwd="' + map.GetS('pwd') + '"');
  sql := IISQL(Tb_Users);
  sql.AndEqF('username', map.GetS('username'));
  // sql.AndEqF('pwd', map.GetS('pwd'));

  if json.DS.IsEmpty then
  begin
    Fail(-1, '账号密码错误');
  end
  else if (vcode = scode) then
  begin
    username := json.ds.FieldByName('username').Value;
     password := json.ds.FieldByName('pwd').Value;
    Session.setValue('username', username);

  jo := SO('{}');
    jwt := getjwttoken(username,password);
  jo.puti('code', 0);
  jo.puts('jwt', jwt);
  ShowJSON(jo);
  //  Success(0,'成功');
    end
  else
    Fail(-1, '验证码错误');

  end;
end;

procedure TIndexController.save;
begin
  SetAttr('message', '消息好');

end;
 //指定范围的随机数函数

//调用

procedure TIndexController.index;
var
 // json: IDataSet;
  users: TOrmusers;
  a, b,guid: integer;
  test:string;
 // objlist:Tormtable;
  rawjson:  utf8string  ;
  ret: IDataSet;
  id,cvs: string;
  sql: ISQL;
  temp: string;
  map: IJObject;
  objlist:Tobjectlist;
  obj:Tobject;

begin


  users := TOrmusers.Create;
  try

   // myzeos.SharedTransaction(guid, transBegin);
   // Dbserver.TransactionActiveSession;
   // Dbserver.TransactionBegin(users.Orm.Table,guid);
 //   users.username := 'coolmvc';
 //   users.pwd := '0003456';
 //   users.phone := '18950868703';
  //  Dbserver.Add(users, True);
//    test:=Dbserver.OneFieldValue(users.Orm.Table,'pwd','username="test44"');

   rawjson:=Dbserver.RetrieveListJson(users.Orm.Table,'username="coolmvc" order by id desc','');
    //  objlist:=Dbserver.Orm.Retrievelist(users.Orm.Table,'username="coolmvc" order by id desc',[],'');
      //  a:=objlist.count;
    //  obj:=objlist.First;
      //while not objlist.
   //   rawjson:=pchar(obj.FieldAddress('username'));
      //  a:=objlist.RowCount;
 //  for b:=0 to a-1 do begin
  // test:=objlist.Results[b];

  // end;
   //TAutoFree.One(users, users.CreateAndFillPrepare(
    //  Dbserver, 'order by id', 'username,pwd,phone')) ;

    //Dbserver.orm.Delete(users.Orm.Table, 'username="test44"');
  //  Dbserver.UpdateField(users.Orm.Table, 'username', 'admin7', 'pwd', 'xdm4444');
    b := 0;
    //a:=5 div b;
   //  Dbserver.Commit(guid,false);
   // myzeos.SharedTransaction(guid, transCommitWithoutException);
  except
   // myzeos.SharedTransaction(guid, transRollback);
  //   Dbserver.RollBack(guid);
  end;
 users.Free;
 // json := mysql.Find('select * from users where username="admin"');
  SetAttr('name', '李小明');
  SetAttr('message', '消息好');
  SetAttr('copyright', '版权所有');
  SetAttr('users', rawjson);
  Session.setValue('name', '首页好');
  Show('index');
end;

procedure TIndexController.getdata;
var
  res: IJObject;
  json: IDataSet;
  service: Tservice;
begin
   //  objlist:=Dbserver.MultiFieldValues(users.Orm.Table,['pwd','phone'],'username="coolmvc"');
  res := IIJObject();
  json := mysql.Find('select * from users');
  res.SetS('msg', ('返回成功'));
  ShowJSON(res);
end;



procedure TIndexController.getjson;
var
  json: IJObject;
begin
  json := InputToJSON;
  ShowJSON(json);
end;

function TIndexController.Intercept: boolean;
begin
  Result := False;
end;

procedure TIndexController.login;
begin
  Session.remove('username');
  Show('login');
end;

initialization
  SetRoute('', TIndexController);

end.
