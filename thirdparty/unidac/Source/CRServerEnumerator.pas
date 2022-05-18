
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Core Server Enumerator
//  Created:            27.05.15
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRServerEnumerator;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, SyncObjs;

type

{ TCRServerEnumerator }

  TCRServerEnumerator = class
  public
    constructor Create; virtual;
    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;

    procedure GetServerList(List: TStrings); virtual;
  end;

  TCRServerEnumeratorClass = class of TCRServerEnumerator;

{$IFDEF MSWINDOWS}

{ TCRNetManager }

  TCRServiceStatus = (ssStopped, ssStartPending, ssStopPending, ssRunning, ssContinuePending, ssPausePending, ssPaused); // equal to TCurrentStatus from SvcMgr

  TCRServiceInfo = record
    ServiceName, DisplayName: string;
    Status: TCRServiceStatus;
  end;

  TCRServicesInfo = array of TCRServiceInfo;
  TCRServiceNamesThread = class;
  TCRServicesThread = class(TThread)
  private
    FList: TStrings;
    FKeywords: string;
  protected
  {$IFNDEF VER7P}{$IFNDEF FPC}
    FEvent: TEvent;
  {$ENDIF}{$ENDIF}

    property Terminated;
    procedure Execute; override;
  public
    constructor Create(List: TStrings; const Keywords: string);
    destructor Destroy; override;
  end;

  TCRServiceNamesThread = class(TThread)
  protected
    FKeywords: string;
    FServer: string;
    FServiceNames: TCRServicesInfo;
    procedure Execute; override;
  public
    constructor Create(const Server: string; const Keywords: string);
  end;

  SC_HANDLE = THandle;

  TCRNetManager = class
  protected
    FServicesCS: TCriticalSection;
    FCachedServerList: TStringList;
    FLastTickCount: Cardinal;

    class procedure ServiceManagerOpen(const Server: string; const ReadOnly: boolean; out sch: SC_HANDLE);
    class procedure ServiceManagerClose(const sch: SC_HANDLE);
    class procedure ServiceOpen(const Server: string; const ServiceName: string; const ReadOnly: boolean; out sch: SC_HANDLE; out sh: SC_HANDLE);
    class procedure ServiceClose(const sch: SC_HANDLE; const sh: SC_HANDLE);

    procedure ClearCachedServerList;
    procedure AddToCachedServerList(const Keywords: string; const Server: string);
  public
    constructor Create;
    destructor Destroy; override;

    // Service Control
    class function GetServiceNames(const Server: string): TCRServicesInfo;
    class function GetServiceStatus(const Server: string; const ServiceName: string): TCRServiceStatus;
    class procedure ServiceStart(const Server: string; const ServiceName: string; ParamStr: string = '');
    class procedure ServiceStop(const Server: string; const ServiceName: string);

    // Net control
    class procedure GetServerList(List: TStrings); overload;
    procedure GetServerList(List: TStrings; const Keywords: string; const Timeout: Cardinal = 1; const CacheTimeout: Cardinal = 120); overload;
  end;

var
  CRNetManager: TCRNetManager;

{$ENDIF}

implementation

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRTypes, CLRClasses, CRFunctions, MemData;

{ TCRServerEnumerator }

constructor TCRServerEnumerator.Create;
begin
  inherited;
end;

procedure TCRServerEnumerator.GetServerList(List: TStrings);
begin
  List.Clear;
end;

function TCRServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TCRServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

{$IFDEF MSWINDOWS}
const
  advapi32 = 'advapi32.dll';
  netapi32 = 'netapi32.dll';

  // Service State -- for CurrentState
  SERVICE_STOPPED                = $00000001;
  SERVICE_START_PENDING          = $00000002;
  SERVICE_STOP_PENDING           = $00000003;
  SERVICE_RUNNING                = $00000004;
  SERVICE_CONTINUE_PENDING       = $00000005;
  SERVICE_PAUSE_PENDING          = $00000006;
  SERVICE_PAUSED                 = $00000007;

  // Service object specific access type
  SERVICE_QUERY_CONFIG           = $0001;
  SERVICE_CHANGE_CONFIG          = $0002;
  SERVICE_QUERY_STATUS           = $0004;
  SERVICE_ENUMERATE_DEPENDENTS   = $0008;
  SERVICE_START                  = $0010;
  SERVICE_STOP                   = $0020;
  SERVICE_PAUSE_CONTINUE         = $0040;
  SERVICE_INTERROGATE            = $0080;
  SERVICE_USER_DEFINED_CONTROL   = $0100;
  SERVICE_ALL_ACCESS             = (STANDARD_RIGHTS_REQUIRED or
                                    SERVICE_QUERY_CONFIG or
                                    SERVICE_CHANGE_CONFIG or
                                    SERVICE_QUERY_STATUS or
                                    SERVICE_ENUMERATE_DEPENDENTS or
                                    SERVICE_START or
                                    SERVICE_STOP or
                                    SERVICE_PAUSE_CONTINUE or
                                    SERVICE_INTERROGATE or
                                    SERVICE_USER_DEFINED_CONTROL);

  // Service Control Manager object specific access types
  SC_MANAGER_CONNECT             = $0001;
  SC_MANAGER_CREATE_SERVICE      = $0002;
  SC_MANAGER_ENUMERATE_SERVICE   = $0004;
  SC_MANAGER_LOCK                = $0008;
  SC_MANAGER_QUERY_LOCK_STATUS   = $0010;
  SC_MANAGER_MODIFY_BOOT_CONFIG  = $0020;
  SC_MANAGER_ALL_ACCESS          = (STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT or
    SC_MANAGER_CREATE_SERVICE or SC_MANAGER_ENUMERATE_SERVICE or SC_MANAGER_LOCK or
    SC_MANAGER_QUERY_LOCK_STATUS or SC_MANAGER_MODIFY_BOOT_CONFIG);

  // Service Types
  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_WIN32                 = (SERVICE_WIN32_OWN_PROCESS or
                                   SERVICE_WIN32_SHARE_PROCESS);
  // Service State -- for Enum Requests (Bit Mask)
  SERVICE_ACTIVE                 = $00000001;
  SERVICE_INACTIVE               = $00000002;
  SERVICE_STATE_ALL              = (SERVICE_ACTIVE   or
                                    SERVICE_INACTIVE);
  // Controls
  SERVICE_CONTROL_STOP           = $00000001;

type
  // Service Status Enumeration Structure
  _SERVICE_STATUS = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
  end;
  TServiceStatus = _SERVICE_STATUS;

  _ENUM_SERVICE_STATUSA = record
    lpServiceName: PAnsiChar;
    lpDisplayName: PAnsiChar;
    ServiceStatus: TServiceStatus;
  end;
  TEnumServiceStatus = _ENUM_SERVICE_STATUSA;

  TOpenSCManager = function (lpMachineName: PAnsiChar; lpDatabaseName: PAnsiChar; dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  TCloseServiceHandle = function (hSCObject: SC_HANDLE): BOOL; stdcall;
  TOpenService = function (hSCManager: SC_HANDLE; lpServiceName: PAnsiChar; dwDesiredAccess: DWORD): SC_HANDLE; stdcall;
  TEnumServicesStatus = function (hSCManager: SC_HANDLE; dwServiceType, dwServiceState: DWORD; var lpServices: TEnumServiceStatus;
    cbBufSize: DWORD; var pcbBytesNeeded, lpServicesReturned: DWORD; var lpResumeHandle: DWORD): BOOL; stdcall;
  TQueryServiceStatus = function (hService: SC_HANDLE; var lpServiceStatus: TServiceStatus): BOOL; stdcall;
  TStartService = function (hService: SC_HANDLE; dwNumServiceArgs: DWORD; var lpServiceArgVectors: PAnsiChar): BOOL; stdcall;
  TControlService = function (hService: SC_HANDLE; dwControl: DWORD; var lpServiceStatus: TServiceStatus): BOOL; stdcall;
  TNetServerEnum = function (ServerName: IntPtr; Level: integer; var BufPtr: IntPtr; PrefMaxLen: integer;
    var EntriesRead, TotalEntries: integer; ServType: integer; Domain: PWideChar; var ResumeHandle: integer): integer; stdcall;
  TNetApiBufferFree = function (BufPtr: IntPtr): integer; stdcall;

var
  hAdvapi32Lib: HMODULE;
  hNetapi32Lib: HMODULE;
  OpenSCManager: TOpenSCManager;
  CloseServiceHandle: TCloseServiceHandle;
  OpenService: TOpenService;
  EnumServicesStatus: TEnumServicesStatus;
  QueryServiceStatus: TQueryServiceStatus;
  StartService: TStartService;
  ControlService: TControlService;
  NetServerEnum: TNetServerEnum;
  NetApiBufferFree: TNetApiBufferFree;

function NotLink: integer;
begin
  raise Exception.Create('Function is not linked');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

procedure LoadNetManagerLib;
  function GetProc(hLib: HMODULE; Name: string): FARPROC;
  begin
    if hLib > 0 then
      Result := GetProcAddress(hLib, PChar(Name))
    else
      Result := nil;
    if Result = nil then
      Result := @NotLink;
  end;

begin
  hAdvapi32Lib := LoadLibrary(PChar(advapi32));

  OpenSCManager := GetProc(hAdvapi32Lib, 'OpenSCManagerA');
  CloseServiceHandle := GetProc(hAdvapi32Lib, 'CloseServiceHandle');
  OpenService := GetProc(hAdvapi32Lib, 'OpenServiceA');
  EnumServicesStatus := GetProc(hAdvapi32Lib, 'EnumServicesStatusA');
  QueryServiceStatus := GetProc(hAdvapi32Lib, 'QueryServiceStatus');
  StartService := GetProc(hAdvapi32Lib, 'StartServiceA');
  ControlService := GetProc(hAdvapi32Lib, 'ControlService');

  hNetapi32Lib := LoadLibrary(PChar(netapi32));

  NetServerEnum := GetProc(hNetapi32Lib, 'NetServerEnum');
  NetApiBufferFree := GetProc(hNetapi32Lib, 'NetApiBufferFree');
end;

procedure FreeNetManagerLib;
begin
  if hAdvapi32Lib > 0 then begin
    FreeLibrary(hAdvapi32Lib);
    hAdvapi32Lib := 0;
  end;

  if hNetapi32Lib > 0 then begin
    FreeLibrary(hNetapi32Lib);
    hNetapi32Lib := 0;
  end;
end;

{ TCRServiceNamesThread }

constructor TCRServiceNamesThread.Create(const Server: string; const Keywords: string);
begin
  inherited Create(True);
  FServer := Server;
  FKeywords := Keywords;
  Priority := tpHighest;

{$IFNDEF FPC}
  Resume;
{$ELSE}
  Start;
{$ENDIF}
end;

procedure TCRServiceNamesThread.Execute;
{var
  tc: cardinal;}
var
  j, k: integer;
  sl: TStringList;
  b: boolean;
begin
  // tc := GetTickCount;
  try
    FServiceNames := TCRNetManager.GetServiceNames(FServer);
    if Length(FServiceNames) = 0 then
      Exit;

    sl := TStringList.Create;
    try
      sl.Text := FKeywords;

      b := False;
      for j := 0 to Length(FServiceNames) - 1 do begin
        for k := 0 to sl.Count - 1 do
          if (Pos(sl[k], LowerCase(FServiceNames[j].ServiceName)) > 0) or
             (Pos(sl[k], LowerCase(FServiceNames[j].DisplayName)) > 0) then begin
            b := True;
            CRNetManager.AddToCachedServerList(FKeywords, FServer);
            Break;
          end;
        if b then
          Break;
      end;
    finally
      sl.Free;
    end;
  except
    // Silent
  end;
  {tc := GetTickInterval(tc, GetTickCount);
  OFS(FServer + ' ' + IntToStr(tc) + ' ' + IntToStr(Length(FServices.FServiceNames[FIndex])));}
end;

constructor TCRServicesThread.Create(List: TStrings; const Keywords: string);
begin
  inherited Create(True);

  FList := TStringList.Create;
  FList.Assign(List);
  FKeywords := Keywords;
  FreeOnTerminate := True;
{$IFNDEF VER7P}{$IFNDEF FPC}
  FEvent := TEvent.Create(nil, True, False, '');
{$ENDIF}{$ENDIF}

{$IFNDEF FPC}
  Resume;
{$ELSE}
  Start;
{$ENDIF}
end;

destructor TCRServicesThread.Destroy;
begin
  FList.Free;
{$IFNDEF VER7P}{$IFNDEF FPC}
  FEvent.Free;
{$ENDIF}{$ENDIF}

  inherited;
end;

procedure TCRServicesThread.Execute;
var
  i: integer;
  Threads: array of TCRServiceNamesThread;
begin
  SetLength(Threads, FList.Count);
  for i := 0 to FList.Count - 1 do
    Threads[i] := nil;
  try
    for i := 0 to FList.Count - 1 do
      Threads[i] := TCRServiceNamesThread.Create(FList[i], FKeywords);

    for i := 0 to FList.Count - 1 do
      Threads[i].WaitFor;

  finally
    for i := 0 to FList.Count - 1 do
      Threads[i].Free;
  end;

{$IFNDEF VER7P}{$IFNDEF FPC}
  WaitForSingleObject(FEvent.Handle, INFINITE);
{$ENDIF}{$ENDIF}
end;

{ TCRNetManager }

function ServiceStatusToCurrentStatus(const CurrentState: DWORD): TCRServiceStatus;
begin
  case CurrentState of
    SERVICE_STOPPED:
      Result := ssStopped;
    SERVICE_START_PENDING:
      Result := ssStartPending;
    SERVICE_STOP_PENDING:
      Result := ssStopPending;
    SERVICE_RUNNING:
      Result := ssRunning;
    SERVICE_CONTINUE_PENDING:
      Result := ssContinuePending;
    SERVICE_PAUSE_PENDING:
      Result := ssPausePending;
    SERVICE_PAUSED:
      Result := ssPaused;
    else
      raise Exception.Create(Format('Unknown service status $%X (%d)', [CurrentState, CurrentState]));
  end;
end;

constructor TCRNetManager.Create;
begin
  inherited;

  FServicesCS := TCriticalSection.Create;
end;

destructor TCRNetManager.Destroy;
begin
  ClearCachedServerList;
  FServicesCS.Free;

  inherited;
end;

class procedure TCRNetManager.ServiceManagerOpen(const Server: string; const ReadOnly: boolean; out sch: SC_HANDLE);
var
  s: AnsiString;
  dwDesiredAccess: DWORD;
begin
  sch := 0;
  if Trim(LowerCase(Server)) = 'localhost' then
    s := ''
  else
    s := AnsiString(Server);
  if ReadOnly then
    dwDesiredAccess := GENERIC_READ
  else
    dwDesiredAccess := SC_MANAGER_ALL_ACCESS;
  sch := OpenSCManager(PAnsiChar(s), nil, dwDesiredAccess);
end;

class procedure TCRNetManager.ServiceManagerClose(const sch: SC_HANDLE);
begin
  if sch <> 0 then
    CloseServiceHandle(sch);
end;

class procedure TCRNetManager.ServiceOpen(const Server: string; const ServiceName: string; const ReadOnly: boolean; out sch: SC_HANDLE; out sh: SC_HANDLE);
begin
  ServiceManagerOpen(Server, ReadOnly, sch);
  Win32Check(sch <> 0);
  try
    sh := OpenService(sch, PAnsiChar(AnsiString(ServiceName)), SERVICE_ALL_ACCESS);
    Win32Check(sh <> 0);
  except
    ServiceManagerClose(sch);
    raise;
  end;
end;

class procedure TCRNetManager.ServiceClose(const sch: SC_HANDLE; const sh: SC_HANDLE);
begin
  if sh <> 0 then
    CloseServiceHandle(sh);
  ServiceManagerClose(sch);
end;

class function TCRNetManager.GetServiceNames(const Server: string): TCRServicesInfo;
var
  sch: SC_HANDLE;
  pService: ^TEnumServiceStatus;
  pServices: IntPtr;
  pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  i: integer;
  SizeOfTEnumServiceStatus: Integer;

begin
  SetLength(Result, 0);

  ServiceManagerOpen(Server, True, sch);
  if sch = 0 then
    exit;

  pServices := nil;
  try
    lpResumeHandle := 0;
    pcbBytesNeeded := 0;
    lpServicesReturned := 0;
    EnumServicesStatus(sch, SERVICE_WIN32, SERVICE_STATE_ALL, TEnumServiceStatus(pServices^), 0, pcbBytesNeeded, lpServicesReturned, lpResumeHandle);
    SizeOfTEnumServiceStatus := SizeOf(TEnumServiceStatus);
    lpServicesReturned := 0;
    pServices := Marshal.AllocHGlobal(pcbBytesNeeded);
    Win32Check(EnumServicesStatus(sch, SERVICE_WIN32, SERVICE_STATE_ALL, TEnumServiceStatus(pServices^), pcbBytesNeeded, pcbBytesNeeded, lpServicesReturned, lpResumeHandle));
    SetLength(Result, lpServicesReturned);

    for i := 0 to lpServicesReturned - 1 do begin
      pService := PtrOffset(pServices, SizeOfTEnumServiceStatus * i);

      Result[i].ServiceName := string(AnsiString(pService.lpServiceName));
      Result[i].DisplayName := string(AnsiString(pService.lpDisplayName));
      Result[i].Status := ServiceStatusToCurrentStatus(pService.ServiceStatus.dwCurrentState);
    end;
  finally
    ServiceManagerClose(sch);
    Marshal.FreeHGlobal(pServices);
  end;
end;

class function TCRNetManager.GetServiceStatus(const Server: string; const ServiceName: string): TCRServiceStatus;
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  ss: TServiceStatus;
begin
  ServiceOpen(Server, ServiceName, True, sch, sh);
  try
    Win32Check(QueryServiceStatus(sh, ss));
    Result := ServiceStatusToCurrentStatus(ss.dwCurrentState);
  finally
    ServiceClose(sch, sh);
  end;
end;

class procedure TCRNetManager.ServiceStart(const Server: string; const ServiceName: string; ParamStr: string = '');
  // based on Delphi7 system.pas GetParamStr function
  function GetParamStr(Idx: integer; out Param: string): integer;
  var
    Len: Integer;
    StartIdx, SIdx, QIdx: Integer;
  begin
    while True do
    begin
      while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <= ' ') do
        Inc(Idx);
      if (ParamStr[Idx] = '"') and (ParamStr[Idx + 1] = '"') then Inc(Idx, 2) else Break;
    end;
    Len := 0;
    StartIdx := Idx;
    while ParamStr[Idx] > ' ' do
    begin
      if ParamStr[Idx] = '"' then
      begin
        Inc(Idx);
        while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <> '"') do
        begin
          QIdx := Idx + 1;
          Inc(Len, QIdx - Idx);
          Idx := QIdx;
        end;
        if ParamStr[Idx] <> #0 then
          Inc(Idx);
      end
      else
      begin
        QIdx := Idx + 1;
        Inc(Len, QIdx - Idx);
        Idx := QIdx;
      end;
    end;

    SetLength(Param, Len);

    Idx := StartIdx;
    SIdx := 1;
    while ParamStr[Idx] > ' ' do
    begin
      if ParamStr[Idx] = '"' then
      begin
        Inc(Idx);
        while (ParamStr[Idx] <> #0) and (ParamStr[Idx] <> '"') do
        begin
          QIdx := Idx + 1;
          while Idx < QIdx do
          begin
            Param[SIdx] := ParamStr[Idx];
            Inc(Idx);
            Inc(SIdx);
          end;
        end;
        if ParamStr[Idx] <> #0 then
          Inc(Idx);
      end
      else
      begin
        QIdx := Idx + 1;
        while Idx < QIdx do
        begin
          Param[SIdx] := ParamStr[Idx];
          Inc(Idx);
          Inc(SIdx);
        end;
      end;
    end;

    Result := Idx;
  end;
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  i, Idx: integer;
  Param: string;
  Args: array of string;
  pArgs: array of PAnsiChar;
  p: PAnsiChar;

begin
  ServiceOpen(Server, ServiceName, False, sch, sh);
  try
    ParamStr := Trim(ParamStr);
    if ParamStr <> '' then begin
      Idx := 1;
      SetLength(Args, 0);

      ParamStr := ParamStr + #0;
      while True do
      begin
        Idx := GetParamStr(Idx, Param);
        if Param = '' then Break;

        i := Length(Args);
        SetLength(Args, i + 1);
        Args[i] := Param;
      end;

      SetLength(pArgs, Length(Args));
      for i := 0 to Length(Args) - 1 do
        pArgs[i] := @Args[i][1];

      i := Length(Args);
      Win32Check(StartService(sh, i, pArgs[0]));
    end
    else
    begin
      p := nil;
      Win32Check(StartService(sh, 0, p));
    end;
  finally
    ServiceClose(sch, sh);
  end;
end;

class procedure TCRNetManager.ServiceStop(const Server: string; const ServiceName: string);
var
  sch: SC_HANDLE;
  sh: SC_HANDLE;
  ss: TServiceStatus;
begin
  ServiceOpen(Server, ServiceName, False, sch, sh);
  try
    Win32Check(ControlService(sh, SERVICE_CONTROL_STOP, ss));
  finally
    ServiceClose(sch, sh);
  end;
end;

procedure TCRNetManager.ClearCachedServerList;
var
  i: integer;
begin
  if FCachedServerList = nil then
    Exit;

  try
    for i := 0 to FCachedServerList.Count - 1 do
      FCachedServerList.Objects[i].Free;
  finally
    FreeAndNil(FCachedServerList);
  end;
end;

procedure TCRNetManager.AddToCachedServerList(const Keywords: string; const Server: string);
var
//  s: string;
  i: integer;
  sl: TStringList;
begin
{  s := '';
  for i := Low(Keywords) to High(Keywords) do begin
    if s <> '' then
      s := s + #$D#$A;
    s := s + Keywords[i];
  end;}

  FServicesCS.Acquire;
  try
    if FCachedServerList = nil then begin
      FCachedServerList := TStringList.Create;
    {$IFDEF VER6P}
      FCachedServerList.CaseSensitive := False;
    {$ENDIF}
      FCachedServerList.Sorted := True;
    end;
    i := FCachedServerList.IndexOf(Keywords);
    if i = - 1 then begin
      sl := TStringList.Create;
    {$IFDEF VER6P}
      sl.CaseSensitive := False;
    {$ENDIF}
      sl.Sorted := True;
      FCachedServerList.AddObject(Keywords, sl);
    end
    else
      sl := FCachedServerList.Objects[i] as TStringList;
    if sl.IndexOf(Server) = -1 then
      sl.Add(Server);
  finally
    FServicesCS.Release;
  end;
end;

class procedure TCRNetManager.GetServerList(List: TStrings);
const
  SizeOf_SERVER_INFO_100 = {$IFDEF WIN64}16{$ELSE}8{$ENDIF};
  OffsetOf_sv100_name = {$IFDEF WIN64}8{$ELSE}4{$ENDIF};
var
  pData, psvr_Name: IntPtr;
  EntRead, EntTotal, Resume, i: integer;
  ws: WideString;
  Info: integer;
begin
  List.Clear;
  pData := nil;
  try
    Resume := 0;
    Info := NetServerEnum(nil, 100, pData, -1{MAX_PREFERRED_LENGTH}, EntRead, EntTotal, 1{SV_TYPE_WORKSTATION - All LAN Manager workstations}, nil, Resume);
    if Info <> 0 then
      raise Exception.Create('NetServerEnum error ' + IntToStr(Info));

    for i := 0 to EntRead - 1 do begin
      psvr_Name := Marshal.ReadIntPtr(pData, i * SizeOf_SERVER_INFO_100 + OffsetOf_sv100_name);
      ws := Marshal.PtrToStringUni(psvr_Name);
      List.Add(string(ws));
    end;
  finally
    if pData <> nil then
      NetApiBufferFree(pData);
  end;
end;

procedure TCRNetManager.GetServerList(List: TStrings; const Keywords: string; const Timeout: Cardinal = 1; const CacheTimeout: Cardinal = 120);
var
  mList, sl: TStringList;
  i: integer;
  Threads: TCRServicesThread;
begin
  if Timeout = 0 then begin
    List.Clear;
    Exit;
  end;

  StartWait;
  mList := nil;
  try
    mList := TStringList.Create;
  {$IFDEF VER6P}
    mList.CaseSensitive := False;
  {$ENDIF}
    mList.Sorted := True;

    if GetTickInterval(FLastTickCount, GetTickCount) > CacheTimeout * 1000 then begin
      GetServerList(mList);
      mList.Add('localhost');
      Threads := TCRServicesThread.Create(mList, Keywords);
      WaitForSingleObject(Threads.Handle, Timeout * 1000);
    {$IFNDEF VER7P}{$IFNDEF FPC}
      Threads.FEvent.SetEvent;
    {$ENDIF}{$ENDIF}
      FLastTickCount := GetTickCount;
    end;

    if FCachedServerList <> nil then begin
      mList.Clear;
      FServicesCS.Acquire;
      try
        i := FCachedServerList.IndexOf(Keywords);
        if i <> -1 then begin
          sl := FCachedServerList.Objects[i] as TStringList;
          for i := 0 to sl.Count - 1 do begin
            if mList.IndexOf(sl[i]) = -1 then
              mList.Add(sl[i]);
          end;
        end;
      finally
        FServicesCS.Release;
      end;
    end;

    List.Assign(mList);
  finally
    StopWait;
    mList.Free;
  end;
end;

initialization
  LoadNetManagerLib;
  CRNetManager := TCRNetManager.Create;

finalization
  CRNetManager.Free;
  FreeNetManagerLib;

{$ENDIF}
end.
