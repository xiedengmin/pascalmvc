
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQLMonitor supports
//  Created:            17.11.99
//////////////////////////////////////////////////

{$I Dac.inc}
unit CRTimer;

interface

uses
  SysUtils, Classes, SyncObjs,
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  MemData, DAConsts, CRFunctions;

type
{$IFDEF MSWINDOWS}
  TCRTimer = class(TComponent)
  private
    FInterval: Cardinal;
    FWindowHandle: HWND;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;
{$ENDIF}

  TCRIntervalThread = class(TThread)
  private
    FIntervalEvent: TEvent;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure UpdateThread;

  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  TCRIntervalProcessor = class
  private
  {$IFDEF MSWINDOWS}
    FTimer: TCRTimer;
  {$ENDIF}
    FIntervalThread: TCRIntervalThread;

    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    function GetOnTimer: TNotifyEvent;
    procedure SetOnTimer(Value: TNotifyEvent);

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnTimer: TNotifyEvent read GetOnTimer write SetOnTimer;
  end;

implementation

{ TCRIntervalProcessor }

constructor TCRIntervalProcessor.Create(AOwner: TComponent);
begin
  inherited Create;

{$IFDEF MSWINDOWS}
  if not IsConsole then
    FTimer := TCRTimer.Create(AOwner)
  else
{$ENDIF}
    FIntervalThread := TCRIntervalThread.Create(AOwner);
end;

destructor TCRIntervalProcessor.Destroy;
begin
{$IFDEF MSWINDOWS}
  FTimer.Free;
{$ENDIF}
  FIntervalThread.Free;
  inherited;
end;

function TCRIntervalProcessor.GetEnabled: Boolean;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.Enabled
  else
{$ENDIF}
    Result := FIntervalThread.Enabled;
end;

procedure TCRIntervalProcessor.SetEnabled(Value: Boolean);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.Enabled := Value
  else
{$ENDIF}
    FIntervalThread.Enabled := Value;
end;

function TCRIntervalProcessor.GetInterval: Cardinal;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.Interval
  else
{$ENDIF}
    Result := FIntervalThread.Interval;
end;

procedure TCRIntervalProcessor.SetInterval(Value: Cardinal);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.Interval := Value
  else
{$ENDIF}
    FIntervalThread.Interval := Value;
end;

function TCRIntervalProcessor.GetOnTimer: TNotifyEvent;
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    Result := FTimer.OnTimer
  else
{$ENDIF}
    Result := FIntervalThread.OnTimer;
end;

procedure TCRIntervalProcessor.SetOnTimer(Value: TNotifyEvent);
begin
{$IFDEF MSWINDOWS}
  if FTimer <> nil then
    FTimer.OnTimer := Value
  else
{$ENDIF}
    FIntervalThread.OnTimer := Value;
end;

{ TCRIntervalThread }

constructor TCRIntervalThread.Create(AOwner: TComponent);
begin
  FIntervalEvent := CreateEvent;
  FEnabled := True;
  FInterval := 1000;
  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TCRIntervalThread.Destroy;
begin
  Terminate;
  FEnabled := False;
  FIntervalEvent.SetEvent;
  inherited;
  FIntervalEvent.Free;
end;

procedure TCRIntervalThread.UpdateThread;
begin
  if (FInterval > 0) and FEnabled and Assigned(FOnTimer) then
    FIntervalEvent.SetEvent;
end;

procedure TCRIntervalThread.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then begin
    FEnabled := Value;
    UpdateThread;
  end;
end;

procedure TCRIntervalThread.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then begin
    FInterval := Value;
    UpdateThread;
  end;
end;

procedure TCRIntervalThread.SetOnTimer(Value: TNotifyEvent);
begin
  if @Value <> @FOnTimer then begin
    FOnTimer := Value;
    UpdateThread;
  end;
end;

procedure TCRIntervalThread.Execute;
begin
  try
    while not Terminated do begin
      if (FInterval = 0) or not FEnabled or not Assigned(FOnTimer) then
        FIntervalEvent.WaitFor(INFINITE)
      else begin
        case FIntervalEvent.WaitFor(FInterval) of
          wrTimeout:
            if FEnabled and Assigned(FOnTimer) then
              FOnTimer(Self);
        end;
      end;

      FIntervalEvent.ResetEvent;
    end;
  except
    on E: Exception do
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(E)
      else
        ShowException(E, ExceptAddr);
  end;
end;

{$IFDEF MSWINDOWS}
function TimerWndProc(Window: HWND; Message, WParam: Integer;
  LParam: Integer): Integer; stdcall;
var
  Timer: TCRTimer;
begin
  if Message = WM_TIMER then begin
    Result := 1;
  {$IFDEF CPU64}
    Timer := TCRTimer(GetWindowLongPtr(Window, GWL_USERDATA));
  {$ELSE}
    Timer := TCRTimer(GetWindowLong(Window, GWL_USERDATA));
  {$ENDIF}
    try
      Timer.Timer;
    except
      ApplicationHandleException(Timer);
    end;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

var
  CRTimerWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @TimerWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'CRTimerWindowClass');

{ TCRTimer }

constructor TCRTimer.Create(AOwner: TComponent);
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 1000;

  // allocate timer window
  CRTimerWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, CRTimerWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or ({$IFDEF FPC}@{$ENDIF}TempClass.lpfnWndProc <> @TimerWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(CRTimerWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(CRTimerWindowClass);
  end;
  FWindowHandle := CreateWindowEx(WS_EX_TOOLWINDOW, CRTimerWindowClass.lpszClassName,
    '', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);

  // pass Self to window
{$IFDEF CPU64}
  SetWindowLongPtr(FWindowHandle, GWL_USERDATA, NativeInt(Pointer(Self)));
{$ELSE}
  SetWindowLong(FWindowHandle, GWL_USERDATA, Integer(Pointer(Self)));
{$ENDIF}
end;

destructor TCRTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  DestroyWindow(FWindowHandle);
  inherited Destroy;
end;

procedure TCRTimer.UpdateTimer;
begin
  KillTimer(FWindowHandle, 1);
  if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
    if SetTimer(FWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

procedure TCRTimer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateTimer;
  end;
end;

procedure TCRTimer.SetInterval(Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

procedure TCRTimer.SetOnTimer(Value: TNotifyEvent);
begin
  FOnTimer := Value;
  UpdateTimer;
end;

procedure TCRTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;
{$ENDIF}

end.
