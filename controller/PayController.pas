unit PayController;

interface

uses
  System.SysUtils, System.Classes, MVC.JSON, MVC.DataSet, BaseController;

type
  [MURL('Pay', 'Pay')]
  TPayController = class(TBaseController)
  public
    procedure index;
  end;

implementation

{ TPayController }

procedure TPayController.index;
begin

end;

end.

