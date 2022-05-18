unit mvc.DB.unidac;

{$MODE Delphi} //设置兼容delphi模式

interface

uses Classes, Uni, UniProvider, SQLiteUniProvider ,UniConPool;

type

  { TLazMVCDataModule }
  TLazMVCDataModule = class(TDataModule)
  private
  public
    constructor Create(AOwner: TComponent); override;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DBConnectionTest;
  end;


implementation

var
   UniConnectionPool: TObjectPool<TUniConnection>; // 数据库连接池

{ TLazMVCDataModule }

procedure TLazMVCDataModule.DBConnectionTest;
var
  UniConnection: TUniConnection;
  UniQuery: TUniQuery;
  test :string;
begin
    //测试unidac组件
    UniConnection := UniConnectionPool.LockObject;
//    UniConnection.ConnectString := 'Provider Name=SQLite;Direct=True;Use Unicode=True; Database=sqlite.db;Login Prompt=False';
   UniConnection.ConnectString := 'Provider Name=SQLite;Direct=True;Use Unicode=True; Database=sqlite.db;Login Prompt=False';
 
    try
      UniConnection.Connected := True;
    except
      Writeln('UniConnection err');
    end;

    UniQuery := TUniQuery.Create(nil);
    UniQuery.Connection := UniConnection;
    UniQuery.sql.Add('select * from users');
    UniQuery.Open;

    if UniQuery.RecordCount > 0 then
    begin
       test :=  UniQuery.FieldByName('username').AsString;
       Writeln(test);
    end;
    UniQuery.Free;
    UniConnectionPool.UnlockObject(UniConnection);
end;

constructor TLazMVCDataModule.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
  if (ClassType <> TLazMVCDataModule) and not (csDesigning in ComponentState) then
  begin
    if not InitInheritedComponent(Self, TLazMVCDataModule) then exit;
    if OldCreateOrder then
      DoCreate;
  end;
end;

procedure TLazMVCDataModule.DataModuleDestroy(Sender: TObject);
begin

end;

end.
