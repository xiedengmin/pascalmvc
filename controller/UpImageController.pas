unit UpImageController;

interface

uses
  SysUtils,Classes, BaseController   ;
  //, MVC.JWT;

type
  //[MURL('UpImage', 'UpImage')]
  TUpImageController = class(TBaseController)
    procedure index;
    procedure upimage;
        function Intercept: boolean; override;
  end;

implementation

{ TUpImageController }

procedure TUpImageController.index;
begin
show('UpImage/index');
end;

procedure TUpImageController.upimage;
var
  s: string;
begin
  s := UpFiles();
  Success(0, '上传成功<br>' + s);
end;
 
function TUpImageController.Intercept: boolean;
begin
  Result := False;
end;

initialization
  SetRoute('upimage', TUpImageController, 'upimage');
end.

