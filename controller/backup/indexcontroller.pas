unit IndexController;

{$mode ObjFPC}{$M+}

interface

uses
  Classes, SysUtils, BaseController,MVC.App, MVC.JSON, MVC.DataSet,MVC.service, fpjson;

type

  { TIndexController }

  TIndexController = class(TBaseController)
  private
    function getJwt: string;
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
  Name: string;
  json: IDataSet;
  sql: ISQL;
begin
  vcode := input('vcode');
  vcode:=  uppercase( vcode);
  scode := Session.getValue('vcode');
    scode:=    uppercase(scode);
  map := InputToJSON;
  // ds := Service.index.checkuser(map);
  json := mysql.Find('select * from users where username="' + map.GetS('username')+'" and pwd="'+ map.GetS('pwd')+'"');
    sql := IISQL(Tb_Users);
  sql.AndEqF('username',map.GetS('username') );
  // sql.AndEqF('pwd', map.GetS('pwd'));

  if json.DS.IsEmpty then
  begin
    Fail(-1, '账号密码错误');
  end
  else if (vcode = scode) then
  begin
    Name := json.ds.FieldByName('realname').Value;
    Session.setValue('username', Name);

    Success();
  end
  else
    Fail(-1, '验证码错误');
end;

procedure TIndexController.save;
begin
  SetAttr('message', '消息好');

end;

procedure TIndexController.index;
var
  res: IJObject;
  json: IDataSet;
begin
  res := IIJObject();
  json := mysql.Find('select * from users');
    mysql.StartTransaction; // 事务启动
   with mysql.Add(Tb_Users) do
        begin
          FieldByName('username').AsString := ('u1sername');

          Post;
        ApplyUpdates;
          Close;
        end;
      mysql.Commit;
  SetAttr('message', '消息好');
  SetAttr('copyright', '版权所有');
  SetAttr('users', json);
  Session.setValue('name', '首页好');
  Show('index');
end;

procedure TIndexController.getdata;
var
  res: IJObject;
  json: IDataSet;
  service:Tservice;
begin
  res := IIJObject();
  json := mysql.Find('select * from users');
  res.SetS('msg', ('返回成功'));
  ShowJSON(res);
end;

function TIndexController.getJwt: string;
  //var
  //s: string;
  //JWT: IJWT;
begin
 { JWT := IIJWT;
  with JWT.O do
  begin
    Id := GetGUID;
    Subject := '明星';
    claimAdd('name', '周华健');
    claimAdd('name2', '刘德华');
    Expiration := DateTimeToStr(Now + 80);
    IssuedAt := DateTimeToStr(Now);
    sign := '88888888';
    Issuer := 'http://api.test.com';
    Audience := 'http://api.test.com';
    s := compact;
    Result := s;
  end;       }
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
