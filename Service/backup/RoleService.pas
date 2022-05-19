unit RoleService;

interface

uses
  SysUtils, Classes,MVC.App, MVC.JSON, MVC.DataSet, MVC.Service, MVC.DB;

type
  TRoleService = class
    function getdata(): IDataSet;
  end;

implementation

uses
  TableMap;

{ TRoleService }

function TRoleService.getdata: IDataSet;
var
//  conn: Iconn;
  sql: ISQL;
begin
//  conn := IIConn;
 sql := IISQL(tb_dict_role);
 Result := mysql.Find(sql);
end;

end.

