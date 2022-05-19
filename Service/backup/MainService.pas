unit MainService;

interface

uses
  SysUtils, Classes,MVC.App ,MVC.JSON, MVC.DataSet,MVC.SERVICE, MVC.DB;

type
  TMainService = class
  function getMenu(): IDataSet;
  end;

implementation

uses
  TableMap;
{ TIndexService }

function TMainService.getMenu(): IDataSet;
var
  sql: ISQL;
begin
 // conn := IIConn;
  sql := IISQL(Tb_dict_menu);
  sql.Order('s_id');
  Result := mysql.Find(sql.text);
end;

end.

