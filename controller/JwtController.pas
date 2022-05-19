unit JwtController;

interface

uses
  SysUtils, Classes, BaseController, MVC.config,MVC.tool , fp.jwt.core, brookstringmap;


type
  //[MURL('jwt', 'jwt')]
  TJwtController = class(TBaseController)
  private
    FJWT: TJWT;
  published
    procedure index;
    procedure getToken;
    procedure checktoken;
    function Intercept: boolean; override;
  end;

var
  tokenkey: string;

implementation

{ TJwtController }

procedure TJwtController.index;
begin
  Show('jwt/index');
end;

procedure TJwtController.checktoken;
var
  token, Expiration,  subject, Issuer, IssuedAt: string;
  ret,test: string;
  jpair: Tbrookstringpair;
  i: integer;
  Audience: array[0..2] of string;
  Claimpublic: array of string;
    isutf8: boolean;
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
  ShowText(ret);

end;

procedure TJwtController.getToken;
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
  FJWT.Claims.AddAudience('http://api.test.com');
  FJWT.Claims.AddAudience('Aud2');
  FJWT.Claims.Subject := src;
  FJWT.Claims.SetClaim('name',  ('周华健'));
  FJWT.Claims.SetClaim('name2',  ('刘德华'));
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
  ShowText(CompactToken);
end;

function TJwtController.Intercept: boolean;
begin
  Result := False;
end;

initialization
  SetRoute('jwt', TJwtController, 'jwt');
  tokenkey := 'pascalmvcframework';
end.
