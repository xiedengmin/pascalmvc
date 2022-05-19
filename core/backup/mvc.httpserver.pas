unit MVC.HttpServer;

interface

uses
  SysUtils, Classes, mvc.Config,
  mvc.tool,
  BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer,
  BrookUtility, fpmimetypes;

type

  TWebRequest = TBrookHTTPRequest;
  TWebResponse = TBrookHTTPResponse;
  { TMVCServer }
  TMVCServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(Sender: TObject; req: TBrookHTTPRequest;
      res: TBrookHTTPResponse); override;
  public
    FStop: boolean;
    FHttpServer: TBrookHTTPServer;
    procedure DoIdle(Sender: TObject);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class procedure Start;
    class procedure Stop;
  end;

var
  Serv: TMVCServer;

implementation

uses mvc.Route;

procedure TMVCServer.DoIdle(Sender: TObject);
begin
  if FStop then
    FHttpServer.Active := False;
end;


procedure TMVCServer.DoRequest(Sender: TObject; req: TBrookHTTPRequest;
  res: TBrookHTTPResponse);
var
  error: string;
begin

  try
    if FStop then
    begin
      FHttpServer.Active := False;
      exit;
    end;
   // writeln('请求路径'+req.Path)  ;
    route.OpenRoute(req, res);

  except
    on e: Exception do
    begin
      error := e.Message;
      Route.Error500(res, error);
    end;

  end;

end;

constructor TMVCServer.Create(AOwner: TComponent);
var
  temdir:string;
begin
  port := Config.Port.ToInteger;
  ConnectionTimeout := 500;
  ThreadPoolSize := Config.ThreadCount;
 // temdir:= Sagui.TmpDir;
   temdir:=Config.BasePath ;
  UploadsDir := Concat(IncludeTrailingPathDelimiter(temdir), 'uploads');
  inherited Create(AOwner);
   //on ubuntu  copy libsagui.so libsagui.so.3 libsagui.so.3.4.0 three file to /usr/lib;
end;

destructor TMVCServer.Destroy;
begin
  FHttpServer.Free;
  inherited Destroy;
end;

class procedure TMVCServer.Start;
var
  txt,errmsg:string;
  strlist:Tstringlist;
begin
  Serv := TMVCServer.Create(nil);
  try
        serv.Security.Active:=true;
     //   txt:=loadtxt('bili7.com.crt');
   // serv.Security.Certificate:=txt;
   //  serv.Security.PrivateKey:=loadtxt('bili7.com.key');
    //  serv.Security.PrivatePassword:=loadtxt('bili7.com.key'); ;

   //  GetContentFromFile('certificate.txt', errmsg,strlist);
   // txt:= loadtxt('certificate.txt') ;
     //serv.Security.Certificate:=txt;
     //serv.Security.PrivateKey:=loadtxt('privatekey.txt');
     Serv.Open;
    Serv.FStop := False;
    if not Serv.Active then
      Exit;
    WriteLn('Server running at http://localhost:', Serv.port);
    ReadLn;
  finally
    Serv.Free;
  end;
end;

class procedure TMVCServer.Stop;
begin
  Serv.FStop := True;
end;

end.
