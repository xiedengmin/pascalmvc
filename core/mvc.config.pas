unit mvc.Config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, mvc.JSON, Generics.Collections, fpjson, jsonparser;

type
  TCorss_Origin = record
    Allow_Origin: string;
    Allow_Headers: string;
    Allow_Method: string;
    Allow_Credentials: boolean;
  end;

  { TConfig }

  TConfig = class
  private
    directory_permission: specialize TDictionary<string, boolean>;
    over: boolean;
    FApp: string;
    FWebRoot: string;
    FPort: string;
    FThreadCount: integer;
    Fdocument_charset: string;
    Fopen_log: boolean;
    Fsession_start: boolean;
    Fsession_timer: integer;
    Fopen_cache: boolean;
    Fopen_debug: boolean;
    Fcache_max_age: string;
    Fmime_path: string;
    Fconfig_path: string;
    Fpassword_key: string;
    Ftemplate: string;
    Ftemplate_type: string;
    FError404: string;
    FError500: string;
    FCompress: string;
    FCorss_Origin: TCorss_Origin;
    FHTTPQueueLength: integer;
    Fauto_start: boolean;
    Fsessoin_name: string;
    FAppTitle: string;
    Froute_suffix: string;
    FJsonToLower: boolean;
    FDBConfig: string;
    Fshow_sql: boolean;
    FWinServiceConfig: string;
    FleftFmt: string;
    FrightFmt: string;
    FBasePath: string;
    procedure SetApp(const Value: string);
    procedure SetPort(const Value: string);
    procedure SetWebRoot(const Value: string);
    procedure SetThreadCount(const Value: integer);
    procedure Setdocument_charset(const Value: string);
    procedure Setopen_log(const Value: boolean);
    procedure Setsession_start(const Value: boolean);
    procedure Setsession_timer(const Value: integer);
    procedure Setopen_cache(const Value: boolean);
    procedure Setopen_debug(const Value: boolean);
    procedure Setcache_max_age(const Value: string);
    procedure Setconfig_path(const Value: string);
    procedure Setmime_path(const Value: string);
    procedure Setpassword_key(const Value: string);
    procedure Settemplate(const Value: string);
    procedure Settemplate_type(const Value: string);
    procedure SetError404(const Value: string);
    procedure SetError500(const Value: string);
    procedure SetCompress(const Value: string);
    procedure SetCorss_Origin(const Value: TCorss_Origin);
    procedure SetHTTPQueueLength(const Value: integer);
    procedure Setauto_start(const Value: boolean);
    procedure Setsessoin_name(const Value: string);
    procedure SetAppTitle(const Value: string);
    procedure Setroute_suffix(const Value: string);
    procedure SetJsonToLower(const Value: boolean);
    procedure SetDBConfig(const Value: string);
    procedure Setshow_sql(const Value: boolean);
    procedure SetWinServiceConfig(const Value: string);
    procedure SetBasePath(const Value: string);
    procedure SetleftFmt(const Value: string);
    procedure SetrightFmt(const Value: string);

  public
    LockObject: TMultiReadExclusiveWriteSynchronizer;
    MIME: specialize TDictionary<string, string>;
    property BasePath: string read FBasePath write SetBasePath;
    property AppTitle: string read FAppTitle write SetAppTitle;
    property App: string read FApp write SetApp;
    property Port: string read FPort write SetPort;
    property WebRoot: string read FWebRoot write SetWebRoot;
    property ThreadCount: integer read FThreadCount write SetThreadCount;
    property document_charset: string read Fdocument_charset write Setdocument_charset;
    property open_log: boolean read Fopen_log write Setopen_log;
    property session_start: boolean read Fsession_start write Setsession_start;
    property session_timer: integer read Fsession_timer write Setsession_timer;
    property open_cache: boolean read Fopen_cache write Setopen_cache;
    property open_debug: boolean read Fopen_debug write Setopen_debug;
    property cache_max_age: string read Fcache_max_age write Setcache_max_age;
    property config_path: string read Fconfig_path write Setconfig_path;
    property mime_path: string read Fmime_path write Setmime_path;
    property password_key: string read Fpassword_key write Setpassword_key;
    //对配置文件进行加密解密秘钥
    property template: string read Ftemplate write Settemplate;
    property template_type: string read Ftemplate_type write Settemplate_type;
    property Error404: string read FError404 write SetError404;
    property Error500: string read FError500 write SetError500;
    property Corss_Origin: TCorss_Origin read FCorss_Origin write SetCorss_Origin;
    //支持跨域访问
    property Compress: string read FCompress write SetCompress;
    property HTTPQueueLength: integer read FHTTPQueueLength write SetHTTPQueueLength;
    property auto_start: boolean read Fauto_start write Setauto_start;
    property sessoin_name: string read Fsessoin_name write Setsessoin_name;
    property suffix: string read Froute_suffix write Setroute_suffix; //伪静态后缀
    property JsonToLower: boolean read FJsonToLower write SetJsonToLower;
    property DBConfig: string read FDBConfig write SetDBConfig;
    //存储数据库配置数据
    property show_sql: boolean read Fshow_sql write Setshow_sql;
    property WinServiceConfig: string read FWinServiceConfig write SetWinServiceConfig;
    property leftFmt: string read FleftFmt write SetleftFmt; //左边分割字符
    property rightFmt: string read FrightFmt write SetrightFmt; //右边分割字符

    function check_directory_permission(path: string): boolean;
    //检查目录的访问权限
    function read_config(): IJObject; // 读取 config.json 配置文件
    function read_mime(): IJArray; //读取mime.json 配置文件
    procedure setParams(json: IJObject); //初始化config配置参数

    function isOver: boolean; //配置文件是否成功配置完毕
    function initParams(): boolean; //初始化配置参数
    procedure setPassword(password: string); //设置解密秘钥
    constructor Create();
    destructor Destroy; override;
  end;

var
  Config: TConfig;


procedure Lock();

procedure UnLock();

function GetGUID: string;

implementation

{ TConfig }
function GetGUID: string;
var
  LTep: TGUID;
  sGUID: string;
begin
  CreateGUID(LTep);
  sGUID := GUIDToString(LTep);
  sGUID := StringReplace(sGUID, '-', '', [rfReplaceAll]);
  sGUID := Copy(sGUID, 2, Length(sGUID) - 2);
  Result := sGUID;
end;

procedure Lock();
begin
  config.LockObject.Beginread;
  config.LockObject.Beginwrite;

end;

procedure UnLock();
begin
  config.LockObject.Endwrite;
  config.LockObject.Endread;

end;

function TConfig.check_directory_permission(path: string): boolean;
var
  key: string;
  ret: boolean;
begin
  ret := True;
  for key in directory_permission.Keys do
  begin
    if copy(path, 0, length(key)) = key then
    begin
      directory_permission.TryGetValue(key, ret);
      break;
    end;
  end;
  Result := ret;
end;

constructor TConfig.Create();
begin
  MIME := specialize TDictionary<string, string>.Create;
  directory_permission := specialize TDictionary<string, boolean>.Create;
  LockObject := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TConfig.Destroy;
begin
  directory_permission.Free;
  MIME.Free;
  LockObject.Free;
  inherited;
end;

function TConfig.initParams(): boolean;
begin
  show_sql := False;
  App := '';
  Port := '8001';
  WebRoot := 'WebRoot';
  ThreadCount := 10;
  document_charset := 'utf-8';
  open_log := True;
  session_start := True;
  open_cache := True;
  open_debug := True;
  cache_max_age := '315360000';
  template := 'view';
  template_type := '.html';
  Error404 := '404';
  Error500 := '500';
  leftFmt := '{#';
  rightFmt := '}';
  config_path := Config.BasePath + 'Resources/config.json';
  mime_path := Config.BasePath + 'Resources/mime.json';
  HTTPQueueLength := 1000;
  session_timer := 30;
  suffix := '.html';
  sessoin_name := '__guid_session';
  JsonToLower := False;
  over := False;
  if (Config.read_config <> nil) and (Config.read_mime <> nil) then
    over := True;
  Result := over;
end;

function TConfig.isOver: boolean;
begin
  Result := over; //检查配置文件是否成功读取
end;

function TConfig.read_config(): IJObject;
var
  f: TStringList;
  txt, filepath: string;
  key: string;
  jo: IJObject;
begin

  filepath := config_path;
  //filepath := IITool.PathFmt(filepath);
  if not FileExists(filepath) then
  begin
    //  WriteLog(config_path + '配置文件不存在');
    Result := nil;
    exit;
  end;
  key := password_key;
  f := TStringList.Create;
  f.LoadFromFile(filepath, TEncoding.UTF8);
  txt := f.Text.Trim;
  try
    if Trim(key) = '' then
    begin
      txt := f.Text;
    end
    else
    begin
      // txt := DeCryptStr(txt, key);
    end;
    try
      jo := TJObject.I(txt);
      setParams(jo); //参数初始化
    except
      on e: Exception do
      begin
        //  WriteLog(e.Message);
        jo := nil;
      end;
    end;
  finally
    f.Free;
  end;
  if jo = nil then
  begin
    //WriteLog(config_path + '配置文件加载失败');
    Result := nil;
  end
  else
  begin
    over := True;
    Result := jo;
  end;
end;

function TConfig.read_mime: IJArray;
var
  f: TStringList;
  txt: string;
  jarr: IJArray;
  i: integer;
  jo1: TJSONObject;
  ekey, mValue: string;
  filepath: string;
  s: TJSONStringType;
begin
  filepath := mime_path;
  // filepath := IITool.PathFmt(filepath);
  if not FileExists(filepath) then
  begin
    //WriteLog(config_path + '配置文件不存在');
    Result := nil;
    exit;
  end;
  f := TStringList.Create;
  try
    mime.Clear;
    f.LoadFromFile(filepath);
    try
      txt := f.Text.Trim;
      jarr := TJArray.I(txt);

      for i := 0 to jarr.A.Count - 1 do
      begin
        s := jarr.toJSON;
        jo1 := TJSONObject(jarr.A.Items[i]);
        s := jo1.AsJSON;
        ekey := jo1.Get('Extensions');
        mValue := jo1.Get('MimeType');
        MIME.Add(ekey, mValue);
      end;
    except
      jarr := nil;
      over := False;
    end;

  finally
    f.Free;
  end;
  if jarr = nil then
  begin
    //WriteLog(config_path + '配置文件加载失败');
    over := True;
  end;
  Result := jarr;
end;


procedure TConfig.SetApp(const Value: string);
begin
  FApp := Value;
end;

procedure TConfig.SetAppTitle(const Value: string);
begin
  FAppTitle := Value;
end;

procedure TConfig.Setauto_start(const Value: boolean);
begin
  Fauto_start := Value;
end;

procedure TConfig.SetBasePath(const Value: string);
begin
  FBasePath := Value;
end;

procedure TConfig.Setcache_max_age(const Value: string);
begin
  Fcache_max_age := Value;
end;

procedure TConfig.SetCompress(const Value: string);
begin
  FCompress := Value;
end;

procedure TConfig.Setconfig_path(const Value: string);
begin
  Fconfig_path := Value;
end;

procedure TConfig.SetCorss_Origin(const Value: TCorss_Origin);
begin
  FCorss_Origin := Value;
end;

procedure TConfig.SetDBConfig(const Value: string);
begin
  FDBConfig := Value;
end;

procedure TConfig.Setdocument_charset(const Value: string);
begin
  Fdocument_charset := Value;
end;

procedure TConfig.SetError404(const Value: string);
begin
  FError404 := Value;
end;

procedure TConfig.SetError500(const Value: string);
begin
  FError500 := Value;
end;

procedure TConfig.SetHTTPQueueLength(const Value: integer);
begin
  FHTTPQueueLength := Value;
end;

procedure TConfig.SetJsonToLower(const Value: boolean);
begin
  FJsonToLower := Value;
end;

procedure TConfig.Setmime_path(const Value: string);
begin
  Fmime_path := Value;
end;

procedure TConfig.Setopen_cache(const Value: boolean);
begin
  Fopen_cache := Value;
end;

procedure TConfig.Setopen_debug(const Value: boolean);
begin
  Fopen_debug := Value;
end;

procedure TConfig.Setopen_log(const Value: boolean);
begin
  Fopen_log := Value;
end;

procedure TConfig.setParams(json: IJObject);
var
  server_jo, Config_jo, corss_jo, dbconfig_jo, winservice_jo, jo: TJSONObject;
  directory_jo: TJSONArray;
  corss: TCorss_Origin;
  i: integer;
  s: string;
  path: string;
  permission: boolean;
begin
  if json.O.get('AppTitle') <> nil then
    AppTitle := json.O.Get('AppTitle');
  server_jo := TJSONObject(json.O.FindPath('Server'));
  if server_jo <> nil then
  begin
    if server_jo.Get('Port', '') <> '' then
      Port := server_jo.Get('Port');
    if server_jo.Get('Compress', '') <> '' then
      Compress := server_jo.Get('Compress');
    if server_jo.get('HTTPQueueLength', 0) <> 0 then
      HTTPQueueLength := server_jo.Get('HTTPQueueLength');
    if server_jo.Get('ChildThreadCount', 0) <> 0 then
      ThreadCount := server_jo.Get('ChildThreadCount');
  end;
  ////
  // // config文件解析
  Config_jo := TJSONObject(json.O.FindPath('Config'));
  //  //log(Config_jo.ToJSON);
  if Config_jo <> nil then
  begin

    if Config_jo.Get('APP', '') <> '' then
      APP := Config_jo.Get('APP');
    if Config_jo.Get('WebRoot', '') <> '' then
      WebRoot := Config_jo.Get('WebRoot');
    if Config_jo.Get('template', '') <> '' then
      template := Config_jo.Get('template');
    if Config_jo.Get('template_type', '') <> '' then
      template_type := Config_jo.Get('template_type');
    if Config_jo.Get('document_charset', '') <> '' then
      document_charset := Config_jo.Get('document_charset');

    if Config_jo.Get('sessoin_name', '') <> '' then
      sessoin_name := Config_jo.Get('sessoin_name');
    if Config_jo.Get('cache_max_age', '') <> '' then
      cache_max_age := Config_jo.Get('cache_max_age');
    if Config_jo.Get('session_timer', 0) <> 0 then
      session_timer := Config_jo.Get('session_timer');
    if Config_jo.Get('suffix', '') <> '' then
      suffix := Config_jo.Get('suffix');
    JsonToLower := Config_jo.Get('JsonToLower', False);
    open_log := Config_jo.Get('open_log', True);
    open_cache := Config_jo.Get('open_cache', True);
    open_debug := Config_jo.Get('open_debug', True);
    auto_start := Config_jo.Get('auto_start', True);

  end;
  //  //跨域访问设置
  with corss do
  begin
    Allow_Origin := '';
    Allow_Headers := '';
    Allow_Method := '';
    Allow_Credentials := False;
  end;
  Corss_Origin := corss;
  corss_jo := TJSONObject(Config_jo.FindPath('Corss_Origin'));
  if corss_jo <> nil then
  begin
    corss.Allow_Origin := corss_jo.Get('Allow_Origin', '*');
    corss.Allow_Headers := corss_jo.Get('Allow_Headers', '*');
    corss.Allow_Method := corss_jo.Get('Allow_Method', '*');
    corss.Allow_Credentials := corss_jo.Get('Allow_Credentials', True);
    Corss_Origin := corss;
  end;
  dbconfig_jo := TJSONObject(json.O.FindPath('DBConfig'));
  if dbconfig_jo <> nil then
  begin
    DBConfig := dbconfig_jo.AsJSON;
  end;
  //Windows服务设置
  winservice_jo := TJSONObject(json.O.FindPath('WinService'));
  if winservice_jo <> nil then
  begin
    WinServiceConfig := winservice_jo.AsJSON;
  end;

  //  //访问目录权限控制
  directory_permission.Clear;
  directory_jo := TJSONArray(Config_jo.FindPath('directory'));
  if (directory_jo <> nil) and (directory_jo.Count > 0) then
  begin
    for i := 0 to directory_jo.Count - 1 do
    begin
      try
        jo := TJSONObject(directory_jo.Items[i]);
        s := jo.AsJSON;
        path := jo.Get('path');
        path := path.Replace('\', '/');
        if (path.Substring(0, 1) <> '/') then
          path := '/' + path;
        if (path.Substring(path.Length - 1, 1) <> '/') then
          path := path + '/';
        permission := jo.Get('permission');
        directory_permission.Add(path, permission);
      except
        //log('directory参数错误,服务启动失败');
        break;
      end;
    end;
  end;
end;

procedure TConfig.setPassword(password: string);
begin
  password_key := password;
end;

procedure TConfig.Setpassword_key(const Value: string);
begin
  Fpassword_key := Value;
end;

procedure TConfig.SetPort(const Value: string);
begin
  FPort := Value;
end;

procedure TConfig.Setroute_suffix(const Value: string);
begin
  Froute_suffix := Value;
end;

procedure TConfig.Setsession_start(const Value: boolean);
begin
  Fsession_start := Value;
end;

procedure TConfig.Setsession_timer(const Value: integer);
begin
  Fsession_timer := Value;
end;

procedure TConfig.Setsessoin_name(const Value: string);
begin
  Fsessoin_name := Value;
end;

procedure TConfig.Setshow_sql(const Value: boolean);
begin
  Fshow_sql := Value;
end;

procedure TConfig.Settemplate(const Value: string);
begin
  Ftemplate := Value;
end;

procedure TConfig.Settemplate_type(const Value: string);
begin
  Ftemplate_type := Value;
end;

procedure TConfig.SetThreadCount(const Value: integer);
begin
  FThreadCount := Value;
end;

procedure TConfig.SetWebRoot(const Value: string);
begin
  FWebRoot := Value;
end;

procedure TConfig.SetWinServiceConfig(const Value: string);
begin
  FWinServiceConfig := Value;
end;

procedure TConfig.SetleftFmt(const Value: string);
begin
  FleftFmt := Value;
end;

procedure TConfig.SetrightFmt(const Value: string);
begin
  FrightFmt := Value;
end;

initialization

  Config := TConfig.Create();
  Config.BasePath := ExtractFilePath(ParamStr(0));
  Config.initParams();

finalization
  Config.Free;
end.
