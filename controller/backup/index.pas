unit index;

{$mode objfpc}

interface

uses
  Classes, SysUtils,
    BrookUtility,  BrookHTTPRequest,   BrookHTTPResponse,   BrookHTTPServer,   BrookURLRouter,
  SynCommons,
  SynTable,
  SynLog,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotSQLite3,
  SynDB,
  mORMotDB,
  SynDBZeos,
  model;
 function page(ASender: TObject; ARoute: TBrookURLRoute;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse):boolean;




implementation
 uses
  router;
 function page(ASender: TObject; ARoute: TBrookURLRoute;
    ARequest: TBrookHTTPRequest; AResponse: TBrookHTTPResponse):boolean;
   var

    rec: TSQLuser;
    trans: TSQLRecordClass;
    temstr: string;
 begin

   trans := TSQLuser;
    //trans.Create;
    rec := TSQLuser.Create;
    //   rec:=trans;
    rec.login := StringToUTF8('cool1');
    temstr := IntToStr(ARoute.Segments[0].ToInteger);
    rec.password := StringToUTF8('xdmloveu77' + temstr);

    // with TSQLLog.Family do
    //   Level := LOG_VERBOSE;
    try
         FLocalRestServer.TransactionBegin(trans, 1);
        FLocalRestServer.Add(rec, True);
        FLocalRestServer.(rec, True);
        flocalrestserver.Delete(trans, 'login="cool"');
           flocalrestserver.UpdateField(trans, 'login', 'cool2', 'name', 'xdm');
       // flocalrestserver.UpdateField(trans, 'login1', 'cool1', 'name', 'xdm');
        //   aa:=5 div bb;
        FLocalRestServer.commit(1, true);
      except
        FLocalRestServer.RollBack(1);
      end;
      result:=true;
   end;
end.

