unit BaseController;

{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, mvc.Route, MVC.controller, mvc.JSON,mvc.Config, fpjson, superobject,
  mvc.Session, MVC.DataSet, MVC.TOOL, ServiceMap, fp.jwt.core, brookstringmap;

type

  { TBaseController }

  TBaseController = class(TController)
    private
    FJWT: TJWT;
  published
    function getVCode: string;
     function getjwtToken(username,password:string):string;
    procedure checkjwttoken;
    function Intercept: boolean; override;
    procedure CreateController; override;
    procedure ShowPage(ds: IDataSet);
    {封装分页参数，可根据自己的需要修改}
  public

    destructor Destroy; override;
  end;

procedure SetRoute(routeUrl: string; Action: TClass; tplPath: string = '');
var
  tokenkey: string;
implementation

{ TBaseController }
function TBaseController.getVCode: string;
var
  code: string;
begin
  Result := IITool.getVCode(code);
  Session.setValue('vcode', code);
end;

procedure SetRoute(routeUrl: string; Action: TClass; tplPath: string);
begin
  Route.SetRoute(routeUrl, Action, tplPath);
end;

function TBaseController.Intercept: boolean;
var
  username: string;
  json: IJObject;
begin
  json := Session.getJSON_('username');
  username := Session.getValue('username');
  if json <> nil then
    Result := False
  else
    Result := True;
end;

procedure TBaseController.ShowPage(ds: IDataSet);
var
  json: string;
  sjson: ISuperObject;
begin
  json := ds.toJSONArray;
  sjson := SO('{}');
  sjson.I['code'] := 0;
  sjson.S['msg'] := '获取成功';
  sjson.I['count'] := ds.Count;
  sjson.O['data'] := so(json);
  ShowJSON(sjson);
end;

procedure TBaseController.checkjwttoken;
var
  token, Expiration,  subject, Issuer, IssuedAt: string;
  ret,test: string;
  jpair: Tbrookstringpair;
  i: integer;
  Audience: array[0..2] of string;
  Claimpublic: array of string;
    isutf8: boolean;
    jo:isuperobject;
begin
  //JWT := IIJWT;
  Request.Headers.Find('Authorization', jpair); // 获取认证token
  token := jpair.Value;
    ret := Input('name');
 { with JWT.O do
  begin
    if parser('88888888', token) then
    begin
      ret := ret + '|' + Id + '|' + Subject + '|' + IssuedAt + '|' + Expiration
        + '|' + claimGet('name') + '|' + claimGet('name2') + '|' + Issuer + '|' + Audience;
      ShowText(ret);
    end
    else
    begin
      ShowText('验证失败');
    end;
  end;
     }

  if Assigned(FJWT) then FreeAndNil(FJWT);
  FJWT := TJWTCore.Verify(tokenkey, token);    // 解析token
  if not FJWT.Verified then
  begin
    ShowText('Token not Verified!');
    exit;
  end;
  //Get Audiences
  if FJWT.Claims.HasAudience then
  begin
    for I := 0 to Length(FJWT.Claims.Audience) - 1 do
      Audience[i] := FJWT.Claims.Audience[I];
  end;

  if (FJWT.Claims.HasExpiration) and (FJWT.Claims.Expiration < Now) then
  begin
    writeln('Your token is expired.');
  end;

  Expiration := DateTimeToStr(FJWT.Claims.Expiration);
  IssuedAt := DateTimeToStr(FJWT.Claims.IssuedAt);
  Subject := FJWT.Claims.Subject;
  Issuer := FJWT.Claims.Issuer;
  setlength(Claimpublic, FJWT.Claims.AsPublic.Count);
  for i := 0 to FJWT.Claims.AsPublic.Count - 1 do
    begin
    Claimpublic[i] :=FJWT.Claims.AsPublic.Items[i]._value;
    isutf8:= isutf8format(  Claimpublic[i]);

    end;
   // test:=forceutf8(   Claimpublic[0]);
  //  isutf8:= isutf8format(   Claimpublic[0]);
  if (FJWT.Claims.HasIssuer) and (FJWT.Claims.Issuer <> 'outroEmissor') then
  begin
    writeln('incorrect token issuer.');
    writeln('issuer:' + FJWT.Claims.Issuer);
  end;
  ret := ret + '|' + Claimpublic[2] + '|' + subject + '|' +
    IssuedAt + '|' + Expiration + '|' +  (Claimpublic[0]) + '|' +
    ( Claimpublic[1]) + '|' + Issuer + '|' + Audience[0];
  jo := SO('{}');
  jo.puti('code', 0);
  jo.puts('username', Claimpublic[0]);
  ShowJSON(jo);
//  ShowText(ret);

end;

function TBaseController.getjwtToken(username,password:string):string;
var
  signatureText, CompactToken: string;

  FISSUER,test: string;
   dest: punicodechar;
   src:pchar;
   sizetext,ok:integer;
begin
 {
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
  end;
       }
  if Assigned(FJWT) then FreeAndNil(FJWT);
  FISSUER := 'http://api.test.com';
   FJWT := TJWT.Create;
  //Payload
  FJWT.Claims.Issuer := FISSUER;
  FJWT.Claims.Expiration := Now + 200;
  FJWT.Claims.IssuedAt := Now;
  src:=   '明星';
  FJWT.Claims.AddAudience('http://api.bili7.com');
  FJWT.Claims.AddAudience('http://bili7.com');
  FJWT.Claims.Subject := src;
  FJWT.Claims.SetClaim('username', username);
  FJWT.Claims.SetClaim('password',  password);
  // Add custom
  FJWT.Claims.SetClaim('userId', GetGUID);

  try
    CompactToken := TJWTCore.SHA256CompactToken(tokenkey, FJWT);
    signatureText := FJWT.Signature;
  except
    On E: Exception do
    begin
      ShowText('失败' + E.Message);
      // ShowMessage(E.Message);
    end;
  end;
  ok:=length(   CompactToken);
  result:= CompactToken;
  //ShowText(CompactToken);
end;

procedure TBaseController.CreateController;
begin
  inherited CreateController;

end;

destructor TBaseController.Destroy;
begin

end;
  initialization
        tokenkey := 'pascalmvcframework';
end.
