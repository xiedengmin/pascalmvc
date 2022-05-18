
//////////////////////////////////////////////////
//  Devart Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CRThread
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRThread;

interface

uses
  Classes, SysUtils, SyncObjs,
{$IFDEF VER17P}
  Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CRTypes, CLRClasses, CRFunctions, CRTimer;

type
  TCRThreadWrapper = class;

  TCRThreadTerminateEvent = procedure(Sender: TObject) of object;
  TCRThreadExceptionEvent = procedure(Sender: TObject; E: Exception; var Fail: boolean) of object;
  TCRThreadEvent = procedure(Sender: TObject; Event: Pointer) of object;

  TCRThread = class(TThread)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCRThreadWrapper;
    FStartedEvent: TEvent;
    FResumedEvent: TEvent;

    procedure Execute; override;
  public
    constructor Create(Owner: TCRThreadWrapper); virtual;
    destructor Destroy; override;
  end;

  TCRThreadState = (tsSuspended, tsExecuting, tsTerminating, tsFinished);

  TCRThreadWrapper = class
  private
    FIntervalProcessor: TCRIntervalProcessor;
    FThread: TCRThread;

    FOnPostEvent: TCRThreadEvent;
    FOnSendEvent: TCRThreadEvent;
    FOnException: TCRThreadExceptionEvent;
    FOnTerminate: TCRThreadTerminateEvent;

    FLockState: {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection;
    FEvents: TThreadList;
    FException: Exception;
    FThreadState: TCRThreadState;
//    FFreeOnTerminate: boolean;
    FDoTimerProcessing, FLockDestroy: boolean;
    FDestroyAfterTimer: boolean;
    FSendEvent: Pointer;
    FSendEventProcessed: TEvent;

    procedure ProcessException;
    procedure DoTerminate;
    procedure SetTimer;
    procedure KillTimer;
    procedure DoTimer(Sender: TObject);

  protected
    procedure InternalExecute; virtual;
    function CloneException(E: Exception): Exception; virtual;
    procedure DoException(E: Exception);

  public
    constructor Create(ForceStartTimer: boolean = False);
    destructor Destroy; override;

    procedure PostEvent(Event: Pointer);
    procedure SendEvent(Event: Pointer);
    procedure WaitFor;

    procedure Resume;
    // procedure Suspend;
    procedure TryTerminate;
    procedure WaitForExit;
    function Terminated: boolean;
    function InThread: boolean;

    property LastException: Exception read FException;
//    property FreeOnTerminate: boolean read FFreeOnTerminate write FFreeOnTerminate;

    property OnPostEvent: TCRThreadEvent read FOnPostEvent write FOnPostEvent;
    property OnSendEvent: TCRThreadEvent read FOnSendEvent write FOnSendEvent;
    property OnException: TCRThreadExceptionEvent read FOnException write FOnException;
    property OnTerminate: TCRThreadTerminateEvent read FOnTerminate write FOnTerminate;
  end;

  procedure StartAsyncEventProcessor(Force: boolean = False);

var
  UseAsyncEventProcessor: boolean;

implementation

const
  SCannotCreateEvent = 'Cannot create event:'#13#10'%s.'#13#10'Error Code: %d';

var
  NeedToSetTimerList: TThreadList;
  AsyncEventProcessorThread: TCRThreadWrapper;

procedure StartAsyncEventProcessor(Force: boolean = False);
begin
  if not UseAsyncEventProcessor or (not IsMainThread and not Force) then
    raise Exception.Create('Async processor must be started from the main thread only');

  if AsyncEventProcessorThread = nil then begin
    AsyncEventProcessorThread := TCRThreadWrapper.Create(True);
    AsyncEventProcessorThread.FThreadState := tsExecuting;
  end;
end;

{ TCRThread }

constructor TCRThread.Create(Owner: TCRThreadWrapper);
{$IFDEF MSWINDOWS}
var
  LastErrorCode: cardinal;
{$ENDIF}
begin
  Assert(Owner <> nil);
  FOwner := Owner;
  FStartedEvent := TEvent.Create(nil, True, False, '');
{$IFDEF MSWINDOWS}
  if NativeUInt(FStartedEvent.Handle) = 0 then begin
    LastErrorCode := GetLastError;
    if LastErrorCode <> 0 then
      raise Exception.CreateFmt(SCannotCreateEvent, [SysErrorMessage(LastErrorCode), LastErrorCode]);
  end;
{$ENDIF}

  FResumedEvent := TEvent.Create(nil, True, False, '');
{$IFDEF MSWINDOWS}
  if NativeUInt(FResumedEvent.Handle) = 0 then begin
    LastErrorCode := GetLastError;
    if LastErrorCode <> 0 then
      raise Exception.CreateFmt(SCannotCreateEvent, [SysErrorMessage(LastErrorCode), LastErrorCode]);
  end;
{$ENDIF}

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TCRThread.Destroy;
begin
  Terminate;
  FResumedEvent.SetEvent;
  inherited;
  FStartedEvent.Free;
  FResumedEvent.Free;
end;

procedure TCRThread.Execute;
begin
  try
    try
      FResumedEvent.WaitFor(INFINITE);
      FStartedEvent.SetEvent;
      if not Terminated then
        FOwner.InternalExecute;
    except
      on E: Exception do
        if not (E is EAbort) then
          FOwner.DoException(E);
    end;
  finally
    FOwner.DoTerminate;
  end;
end;

{ TCRThreadWrapper }

constructor TCRThreadWrapper.Create(ForceStartTimer: boolean = False);
{$IFDEF MSWINDOWS}
var
  LastErrorCode: cardinal;
{$ENDIF}
begin
  inherited Create;

  FLockState := {$IFDEF FPC}SyncObjs.{$ENDIF}TCriticalSection.Create;
  FThreadState := tsSuspended;
  FEvents := TThreadList.Create;
  FSendEventProcessed := TEvent.Create(nil, True, False, '');
{$IFDEF MSWINDOWS}
  if NativeUInt(FSendEventProcessed.Handle) = 0 then begin
    LastErrorCode := GetLastError;
    if LastErrorCode <> 0 then
      raise Exception.CreateFmt(SCannotCreateEvent, [SysErrorMessage(LastErrorCode), LastErrorCode]);
  end;
{$ENDIF}
  FThread := TCRThread.Create(Self);

  if UseAsyncEventProcessor then
    if IsMainThread or ForceStartTimer then
      SetTimer
    else
      NeedToSetTimerList.Add(Self);
end;

destructor TCRThreadWrapper.Destroy;
begin
  if FLockDestroy then
    Exit;

  if FDoTimerProcessing then begin
    FDestroyAfterTimer := True;
  {$IFDEF AUTOREFCOUNT}
    __ObjAddRef;
  {$ENDIF}
    Exit;
  end;

//  if not FFreeOnTerminate then begin
    FLockDestroy := True;
    WaitForExit;
//  end;

  NeedToSetTimerList.Remove(Self);

  FreeAndNil(FThread);
  FreeAndNil(FEvents);
  FreeAndNil(FIntervalProcessor);

  FSendEventProcessed.Free;
  FLockState.Free;

  inherited;
end;

function TCRThreadWrapper.InThread: boolean;
begin
  Result := IsThread(FThread.ThreadID);
end;

procedure TCRThreadWrapper.InternalExecute;
begin

end;

procedure TCRThreadWrapper.Resume;
begin
  FLockState.Enter;
  try
    if FThreadState <> tsSuspended then
      Exit;

    FThreadState := tsExecuting;
    FThread.FResumedEvent.SetEvent;
    FThread.FStartedEvent.WaitFor(INFINITE);
  finally
    FLockState.Leave;
  end;
end;

{procedure TCRThreadWrapper.Suspend;
begin
  FLockState.Enter;
  try
    FThreadState := tsSuspended;
    FThread.Suspend;
  finally
    FLockState.Leave;
  end;
end;}

function TCRThreadWrapper.Terminated: boolean;
begin
  Result := FThreadState in [tsTerminating, tsFinished];
end;

procedure TCRThreadWrapper.TryTerminate;
begin
  FLockState.Enter;
  try
    if FThreadState = tsExecuting then begin
      FThreadState := tsTerminating;
      FThread.Terminate;
    end;
  finally
    FLockState.Leave;
  end;
end;

procedure TCRThreadWrapper.DoTerminate;
begin
  FLockState.Enter;
  FThreadState := tsFinished;
  FLockState.Leave;

  if not UseAsyncEventProcessor then
    if Assigned(FOnTerminate) then
      FOnTerminate(Self);
end;

procedure TCRThreadWrapper.WaitForExit;
begin
  case FThreadState of
    tsSuspended: ;

    tsExecuting: begin
      TryTerminate;
      FSendEventProcessed.SetEvent;
      WaitFor;
    end;

    tsTerminating, tsFinished:
      WaitFor;
  end;
end;

procedure TCRThreadWrapper.PostEvent(Event: Pointer);
begin
  if UseAsyncEventProcessor then begin
    Assert(FEvents <> nil);
    FEvents.Add(Event);
  end
  else begin
    try
      if Assigned(FOnPostEvent) then
        FOnPostEvent(Self, Event);
    except
      on E: Exception do
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(E)
        else
          ShowException(E, ExceptAddr);
    end;
  end;
end;

procedure TCRThreadWrapper.SendEvent(Event: Pointer);
begin
  if Terminated then
    Exit;

  if UseAsyncEventProcessor then begin
    FSendEvent := Event;
    FSendEventProcessed.WaitFor(INFINITE);
    FSendEventProcessed.ResetEvent;
  end
  else begin
    if Assigned(FOnSendEvent) then
      FOnSendEvent(Self, Event);
  end;
end;

procedure TCRThreadWrapper.DoException(E: Exception); // In FThread context
begin
  Assert(FException = nil);
  FException := CloneException(E);

  if not UseAsyncEventProcessor then
    ProcessException;
end;

function TCRThreadWrapper.CloneException(E: Exception): Exception;
begin
  Result := Exception.Create(E.Message);
end;

procedure TCRThreadWrapper.ProcessException;
var
  Fail: boolean;
begin
  try
    Fail := True;
    if Assigned(FOnException) then
      FOnException(Self, FException, Fail);

    if (FThreadState = tsFinished) and Assigned(FOnTerminate) then
      FOnTerminate(Self);

    if Fail and IsMainThread then begin
      try
        raise FException;
      except
        if Assigned(ApplicationHandleException) then
          ApplicationHandleException(FException)
        else
          ShowException(FException, ExceptAddr);
      end;
    end
    else
      FException.Free;
  finally
    FException := nil;
  end;
end;

procedure TCRThreadWrapper.WaitFor;
begin
  if (FThreadState = tsExecuting) or (FThreadState = tsTerminating) then begin
  {$IFDEF MSWINDOWS}
    WaitForSingleObject(FThread.Handle, INFINITE);
  {$ELSE}
    FThread.WaitFor;
  {$ENDIF}
  end;
  if (FIntervalProcessor <> nil) and FIntervalProcessor.Enabled then
    DoTimer(nil);
end;

procedure TCRThreadWrapper.SetTimer;
const
  USER_TIMER_MINIMUM = $A;
begin
  if FIntervalProcessor = nil then begin
    FIntervalProcessor := TCRIntervalProcessor.Create(nil);
    FIntervalProcessor.Enabled := False;
    FIntervalProcessor.Interval := USER_TIMER_MINIMUM;
    FIntervalProcessor.OnTimer := DoTimer;
  end;

  FIntervalProcessor.Enabled := True;
end;

procedure TCRThreadWrapper.KillTimer;
begin
  if FIntervalProcessor <> nil then
    FIntervalProcessor.Enabled := False;
end;

procedure TCRThreadWrapper.DoTimer(Sender: TObject); // In main thread context
var
  List: TList;
begin
  if FDoTimerProcessing then
    Exit; // For example - on showing error message
  FDoTimerProcessing := True;

  KillTimer; // To prevent multiple calls to DoTimer if any event handler call ProcessMessage
  try
    List := NeedToSetTimerList.LockList;
    try
      while List.Count > 0 do begin
        try
          TCRThreadWrapper(List[0]).SetTimer;
        finally
          List.Delete(0);
        end;
      end;
    finally
      NeedToSetTimerList.UnlockList;
    end;

    if FSendEvent <> nil then begin
      if Assigned(FOnSendEvent) then
        FOnSendEvent(Self, FSendEvent);
      FSendEvent := nil;
      FSendEventProcessed.SetEvent;
    end;

    if FEvents <> nil then begin
      List := FEvents.LockList;
      try
        while List.Count > 0 do begin
          try
            try
              if Assigned(FOnPostEvent) then
                FOnPostEvent(Self, List[0]);
            except
              on E: Exception do
                if Assigned(ApplicationHandleException) then
                  ApplicationHandleException(E)
                else
                  ShowException(E, ExceptAddr);
            end;
          finally
            List.Delete(0);
          end;
        end;
      finally
        FEvents.UnlockList;
      end;
    end;

    if FException <> nil then
      ProcessException
    else
      if (FThreadState = tsFinished) and Assigned(FOnTerminate) then
        FOnTerminate(Self);
  finally
    FDoTimerProcessing := False;
    if {((FThreadState = tsFinished) and FFreeOnTerminate) or} FDestroyAfterTimer then
    {$IFDEF AUTOREFCOUNT}
      __ObjRelease
    {$ELSE}
      Free
    {$ENDIF}
    else
      if FThreadState = tsExecuting then
        SetTimer;
  end;
end;

initialization
{$IFDEF MSWINDOWS}
  if not IsConsole then
    UseAsyncEventProcessor := True
  else
{$ENDIF}
    UseAsyncEventProcessor := False;

  AsyncEventProcessorThread := nil;
  NeedToSetTimerList := TThreadList.Create;

finalization
  NeedToSetTimerList.Free;
  AsyncEventProcessorThread.Free;

end.
