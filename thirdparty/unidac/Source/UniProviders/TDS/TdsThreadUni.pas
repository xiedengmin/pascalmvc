//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsThreadUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,Messages,
{$ENDIF}
Classes,SysUtils,SyncObjs,
{$IFDEF VER17P}
Types,
{$ENDIF}
CRTypes,CRFunctions,
{$IFNDEF UNIDACPRO}
TdsSSLConsts,TdsUtils;
{$ELSE}
TdsSSLConstsUni,TdsUtilsUni;
{$ENDIF}
type
{$IFDEF MSWINDOWS}
OQQ00QO0Q0=class(TComponent)
private
OCQ00QO0Q0:Cardinal;
O0C00QO0Q0:HWND;
OOC00QO0Q0:TNotifyEvent;
OQC00QO0Q0:Boolean;
procedure OCC00QO0Q0;
procedure O00O0QO0Q0(OO0O0QO0Q0:Boolean);
procedure OQ0O0QO0Q0(OC0O0QO0Q0:Cardinal);
procedure O0OO0QO0Q0(OOOO0QO0Q0:TNotifyEvent);
protected
procedure OQOO0QO0Q0;dynamic;
public
constructor Create(O0QO0QO0Q0:TComponent);override;
destructor Destroy;override;
published
property O0CO0QO0Q0:Boolean read OQC00QO0Q0 write O00O0QO0Q0 default True;
property OOCO0QO0Q0:Cardinal read OCQ00QO0Q0 write OQ0O0QO0Q0 default 1000;
property OQCO0QO0Q0:TNotifyEvent read OOC00QO0Q0 write O0OO0QO0Q0;
end;
{$ENDIF}
OCCO0QO0Q0=class(TThread)
private
O00QCQO0Q0:TEvent;
OO0QCQO0Q0:Cardinal;
OQ0QCQO0Q0:TNotifyEvent;
OC0QCQO0Q0:Boolean;
procedure O0OQCQO0Q0(OO0O0QO0Q0:Boolean);
procedure OOOQCQO0Q0(OC0O0QO0Q0:Cardinal);
procedure OQOQCQO0Q0(OOOO0QO0Q0:TNotifyEvent);
procedure OCOQCQO0Q0;
protected
procedure Execute;override;
public
constructor Create(O0QO0QO0Q0:TComponent);
destructor Destroy;override;
property OCQQCQO0Q0:Boolean read OC0QCQO0Q0 write O0OQCQO0Q0 default True;
property O0CQCQO0Q0:Cardinal read OO0QCQO0Q0 write OOOQCQO0Q0 default 1000;
property OOCQCQO0Q0:TNotifyEvent read OQ0QCQO0Q0 write OQOQCQO0Q0;
end;
OQCQCQO0Q0=class
private
{$IFDEF MSWINDOWS}
OCCQCQO0Q0:OQQ00QO0Q0;
{$ENDIF}
O00CCQO0Q0:OCCO0QO0Q0;
function OO0CCQO0Q0:Boolean;
procedure OQ0CCQO0Q0(OO0O0QO0Q0:Boolean);
function OC0CCQO0Q0:Cardinal;
procedure O0OCCQO0Q0(OC0O0QO0Q0:Cardinal);
function OOOCCQO0Q0:TNotifyEvent;
procedure OQOCCQO0Q0(OOOO0QO0Q0:TNotifyEvent);
public
constructor Create(O0QO0QO0Q0:TComponent);
destructor Destroy;override;
property OOQCCQO0Q0:Boolean read OO0CCQO0Q0 write OQ0CCQO0Q0;
property OQQCCQO0Q0:Cardinal read OC0CCQO0Q0 write O0OCCQO0Q0;
property OCQCCQO0Q0:TNotifyEvent read OOOCCQO0Q0 write OQOCCQO0Q0;
end;
O0CCCQO0Q0=class
private
{$IFDEF MSWINDOWS}
OOCCCQO0Q0:OQQ00QO0Q0;
{$ENDIF}
OQCCCQO0Q0:TThread;
OCCCCQO0Q0:boolean;
O000CQO0Q0:TCriticalSection;
OO00CQO0Q0:TEvent;
OQ00CQO0Q0:TEvent;
OC00CQO0Q0:array of TMethod;
O0O0CQO0Q0:TMethod;
OOO0CQO0Q0:TThreadID;
OQO0CQO0Q0:integer;
protected
procedure OCO0CQO0Q0;
procedure OOQ0CQO0Q0(OQQ0CQO0Q0:boolean);
procedure OCQ0CQO0Q0(O0C0CQO0Q0:TObject);
public
constructor Create;
destructor Destroy;override;
procedure OO0OCQO0Q0(OQ0OCQO0Q0:TThreadMethod);
procedure O0OOCQO0Q0(OOOOCQO0Q0:TThreadMethod);
end;
OCOOCQO0Q0=class
protected
O0QOCQO0Q0:O00CO0QOQ0;
procedure OOQOCQO0Q0;virtual;abstract;
procedure OQQOCQO0Q0;overload;
public
destructor Destroy;override;
procedure OQQOCQO0Q0(O0COCQO0Q0:O00CO0QOQ0);overload;
end;
procedure OOCOCQO0Q0;
procedure OQCOCQO0Q0(OQ0OCQO0Q0:TThreadMethod);
procedure OCCOCQO0Q0(OOOOCQO0Q0:TThreadMethod);
var
O00QQQO0Q0:boolean;
implementation
type
OO0QQQO0Q0=class(TThread)
private
{$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
OQ0QQQO0Q0:O0CCCQO0Q0;
protected
procedure Execute;override;
public
constructor Create(O0QO0QO0Q0:O0CCCQO0Q0);
destructor Destroy;override;
end;
var
OQOQQQO0Q0:TCRThreadList;
OCOQQQO0Q0:O0CCCQO0Q0;
O0QQQQO0Q0:TCriticalSection;
constructor OQCQCQO0Q0.Create(O0QO0QO0Q0:TComponent);
begin
inherited Create;
{$IFDEF MSWINDOWS}
if not IsConsole then
OCCQCQO0Q0:=OQQ00QO0Q0.Create(O0QO0QO0Q0)
else
{$ENDIF}
O00CCQO0Q0:=OCCO0QO0Q0.Create(O0QO0QO0Q0);
end;
destructor OQCQCQO0Q0.Destroy;
begin
{$IFDEF MSWINDOWS}
OCCQCQO0Q0.Free;
{$ENDIF}
O00CCQO0Q0.Free;
inherited;
end;
function OQCQCQO0Q0.OO0CCQO0Q0:Boolean;
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
Result:=OCCQCQO0Q0.O0CO0QO0Q0
else
{$ENDIF}
Result:=O00CCQO0Q0.OCQQCQO0Q0;
end;
procedure OQCQCQO0Q0.OQ0CCQO0Q0(OO0O0QO0Q0:Boolean);
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
OCCQCQO0Q0.O0CO0QO0Q0:=OO0O0QO0Q0
else
{$ENDIF}
O00CCQO0Q0.OCQQCQO0Q0:=OO0O0QO0Q0;
end;
function OQCQCQO0Q0.OC0CCQO0Q0:Cardinal;
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
Result:=OCCQCQO0Q0.OOCO0QO0Q0
else
{$ENDIF}
Result:=O00CCQO0Q0.O0CQCQO0Q0;
end;
procedure OQCQCQO0Q0.O0OCCQO0Q0(OC0O0QO0Q0:Cardinal);
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
OCCQCQO0Q0.OOCO0QO0Q0:=OC0O0QO0Q0
else
{$ENDIF}
O00CCQO0Q0.O0CQCQO0Q0:=OC0O0QO0Q0;
end;
function OQCQCQO0Q0.OOOCCQO0Q0:TNotifyEvent;
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
Result:=OCCQCQO0Q0.OQCO0QO0Q0
else
{$ENDIF}
Result:=O00CCQO0Q0.OOCQCQO0Q0;
end;
procedure OQCQCQO0Q0.OQOCCQO0Q0(OOOO0QO0Q0:TNotifyEvent);
begin
{$IFDEF MSWINDOWS}
if OCCQCQO0Q0<>nil then
OCCQCQO0Q0.OQCO0QO0Q0:=OOOO0QO0Q0
else
{$ENDIF}
O00CCQO0Q0.OOCQCQO0Q0:=OOOO0QO0Q0;
end;
constructor OCCO0QO0Q0.Create(O0QO0QO0Q0:TComponent);
begin
O00QCQO0Q0:=CreateEvent;
OC0QCQO0Q0:=True;
OO0QCQO0Q0:=1000;
inherited Create(False);
end;
destructor OCCO0QO0Q0.Destroy;
begin
Terminate;
OC0QCQO0Q0:=False;
O00QCQO0Q0.SetEvent;
inherited;
O00QCQO0Q0.Free;
end;
procedure OCCO0QO0Q0.OCOQCQO0Q0;
begin
if(OO0QCQO0Q0>0)and OC0QCQO0Q0 and Assigned(OQ0QCQO0Q0)then
O00QCQO0Q0.SetEvent;
end;
procedure OCCO0QO0Q0.O0OQCQO0Q0(OO0O0QO0Q0:Boolean);
begin
if OO0O0QO0Q0<>OC0QCQO0Q0 then begin
OC0QCQO0Q0:=OO0O0QO0Q0;
OCOQCQO0Q0;
end;
end;
procedure OCCO0QO0Q0.OOOQCQO0Q0(OC0O0QO0Q0:Cardinal);
begin
if OC0O0QO0Q0<>OO0QCQO0Q0 then begin
OO0QCQO0Q0:=OC0O0QO0Q0;
OCOQCQO0Q0;
end;
end;
procedure OCCO0QO0Q0.OQOQCQO0Q0(OOOO0QO0Q0:TNotifyEvent);
begin
if @OOOO0QO0Q0<>@OQ0QCQO0Q0 then begin
OQ0QCQO0Q0:=OOOO0QO0Q0;
OCOQCQO0Q0;
end;
end;
procedure OCCO0QO0Q0.Execute;
begin
try
while not Terminated do begin
if(OO0QCQO0Q0=0)or not OC0QCQO0Q0 or not Assigned(OQ0QCQO0Q0)then
O00QCQO0Q0.WaitFor(INFINITE)
else begin
case O00QCQO0Q0.WaitFor(OO0QCQO0Q0)of
wrTimeout:
if OC0QCQO0Q0 and Assigned(OQ0QCQO0Q0)then
OQ0QCQO0Q0(Self);
end;
end;
O00QCQO0Q0.ResetEvent;
end;
except
on E:Exception do
if Assigned(ApplicationHandleException)then
ApplicationHandleException(E)
else
ShowException(E,ExceptAddr);
end;
end;
{$IFDEF MSWINDOWS}
function OOQQQQO0Q0(OQQQQQO0Q0:HWND;OCQQQQO0Q0:UINT;O0CQQQO0Q0:WPARAM;
OOCQQQO0Q0:LPARAM):LRESULT;stdcall;
var
OQCQQQO0Q0:OQQ00QO0Q0;
begin
if OCQQQQO0Q0=WM_TIMER then begin
Result:=1;
{$IFDEF CPU64}
OQCQQQO0Q0:=OQQ00QO0Q0(GetWindowLongPtr(OQQQQQO0Q0,GWL_USERDATA));
{$ELSE}
OQCQQQO0Q0:=OQQ00QO0Q0(GetWindowLong(OQQQQQO0Q0,GWL_USERDATA));
{$ENDIF}
try
OQCQQQO0Q0.OQOO0QO0Q0;
except
ApplicationHandleException(OQCQQQO0Q0);
end;
end
else
Result:=DefWindowProc(OQQQQQO0Q0,OCQQQQO0Q0,O0CQQQO0Q0,OOCQQQO0Q0);
end;
var
OCCQQQO0Q0:TWndClass=(
style:0;
lpfnWndProc:@OOQQQQO0Q0;
cbClsExtra:0;
cbWndExtra:0;
hInstance:0;
hIcon:0;
hCursor:0;
hbrBackground:0;
lpszMenuName:nil;
lpszClassName:'CRTimerWindowClass');
constructor OQQ00QO0Q0.Create(O0QO0QO0Q0:TComponent);
var
OOQO0QO0Q0:TWndClass;
OQQO0QO0Q0:Boolean;
begin
inherited Create(O0QO0QO0Q0);
OQC00QO0Q0:=True;
OCQ00QO0Q0:=1000;
OCCQQQO0Q0.hInstance:=HInstance;
OQQO0QO0Q0:=GetClassInfo(HInstance,OCCQQQO0Q0.lpszClassName,
OOQO0QO0Q0);
if not OQQO0QO0Q0 or({$IFDEF FPC}@{$ENDIF}OOQO0QO0Q0.lpfnWndProc<>@OOQQQQO0Q0)then
begin
if OQQO0QO0Q0 then
Windows.UnregisterClass(OCCQQQO0Q0.lpszClassName,HInstance);
Windows.RegisterClass(OCCQQQO0Q0);
end;
O0C00QO0Q0:=CreateWindowEx(WS_EX_TOOLWINDOW,OCCQQQO0Q0.lpszClassName,
'',WS_POPUP,0,0,0,0,0,0,HInstance,nil);
{$IFDEF CPU64}
SetWindowLongPtr(O0C00QO0Q0,GWL_USERDATA,NativeInt(Pointer(Self)));
{$ELSE}
SetWindowLong(O0C00QO0Q0,GWL_USERDATA,Integer(Pointer(Self)));
{$ENDIF}
end;
destructor OQQ00QO0Q0.Destroy;
begin
OQC00QO0Q0:=False;
OCC00QO0Q0;
DestroyWindow(O0C00QO0Q0);
inherited;
end;
procedure OQQ00QO0Q0.OCC00QO0Q0;
begin
KillTimer(O0C00QO0Q0,1);
if(OCQ00QO0Q0<>0)and OQC00QO0Q0 and Assigned(OOC00QO0Q0)then
if SetTimer(O0C00QO0Q0,1,OCQ00QO0Q0,nil)=0 then
raise EOutOfResources.Create(SNoTimers+#13#10+SysErrorMessage(GetLastError));
end;
procedure OQQ00QO0Q0.O00O0QO0Q0(OO0O0QO0Q0:Boolean);
begin
if OO0O0QO0Q0<>OQC00QO0Q0 then begin
OQC00QO0Q0:=OO0O0QO0Q0;
OCC00QO0Q0;
end;
end;
procedure OQQ00QO0Q0.OQ0O0QO0Q0(OC0O0QO0Q0:Cardinal);
begin
if OC0O0QO0Q0<>OCQ00QO0Q0 then begin
OCQ00QO0Q0:=OC0O0QO0Q0;
OCC00QO0Q0;
end;
end;
procedure OQQ00QO0Q0.O0OO0QO0Q0(OOOO0QO0Q0:TNotifyEvent);
begin
OOC00QO0Q0:=OOOO0QO0Q0;
OCC00QO0Q0;
end;
procedure OQQ00QO0Q0.OQOO0QO0Q0;
begin
if OQC00QO0Q0 and Assigned(OOC00QO0Q0)then
OOC00QO0Q0(Self);
end;
{$ENDIF}
procedure OOCOCQO0Q0;
begin
if OCOQQQO0Q0<>nil then
Exit;
O0QQQQO0Q0.Acquire;
try
if OCOQQQO0Q0=nil then
OCOQQQO0Q0:=O0CCCQO0Q0.Create;
finally
O0QQQQO0Q0.Release;
end;
end;
procedure OQCOCQO0Q0(OQ0OCQO0Q0:TThreadMethod);
begin
if OCOQQQO0Q0=nil then
raise Exception.Create(SAsyncEventProcessorNotStarted);
OCOQQQO0Q0.OO0OCQO0Q0(OQ0OCQO0Q0);
end;
procedure OCCOCQO0Q0(OOOOCQO0Q0:TThreadMethod);
begin
if OCOQQQO0Q0<>nil then
OCOQQQO0Q0.O0OOCQO0Q0(OOOOCQO0Q0);
end;
constructor O0CCCQO0Q0.Create;
{$IFDEF MSWINDOWS}
const
OQC0CQO0Q0=$A;
var
OCC0CQO0Q0:boolean;
{$ENDIF}
begin
inherited;
O000CQO0Q0:=TCriticalSection.Create;
OO00CQO0Q0:=CreateEvent;
OQ00CQO0Q0:=CreateEvent;
SetLength(OC00CQO0Q0,32);
OQO0CQO0Q0:=0;
{$IFDEF MSWINDOWS}
if not IsConsole then
OCC0CQO0Q0:=True
else
OCC0CQO0Q0:=False;
if OCC0CQO0Q0 and IsMainThread then begin
OOCCCQO0Q0:=OQQ00QO0Q0.Create(nil);
OOCCCQO0Q0.O0CO0QO0Q0:=True;
OOCCCQO0Q0.OOCO0QO0Q0:=OQC0CQO0Q0;
OOCCCQO0Q0.OQCO0QO0Q0:=OCQ0CQO0Q0;
end
else
{$ENDIF}
OQCCCQO0Q0:=OO0QQQO0Q0.Create(Self);
end;
destructor O0CCCQO0Q0.Destroy;
begin
{$IFDEF MSWINDOWS}
OOCCCQO0Q0.Free;
{$ENDIF}
OQCCCQO0Q0.Free;
O000CQO0Q0.Free;
OO00CQO0Q0.Free;
OQ00CQO0Q0.Free;
inherited;
end;
procedure O0CCCQO0Q0.OO0OCQO0Q0(OQ0OCQO0Q0:TThreadMethod);
var
OC0OCQO0Q0:integer;
begin
if not Assigned(OQ0OCQO0Q0)then
Exit;
O000CQO0Q0.Acquire;
try
for OC0OCQO0Q0:=0 to OQO0CQO0Q0-1 do begin
if CompareMethods(OC00CQO0Q0[OC0OCQO0Q0],TMethod(OQ0OCQO0Q0))then
Exit;
end;
if OQO0CQO0Q0>=Length(OC00CQO0Q0)then
SetLength(OC00CQO0Q0,OQO0CQO0Q0 shl 1);
OC00CQO0Q0[OQO0CQO0Q0].Code:=TMethod(OQ0OCQO0Q0).Code;
OC00CQO0Q0[OQO0CQO0Q0].Data:=TMethod(OQ0OCQO0Q0).Data;
Inc(OQO0CQO0Q0);
OQ00CQO0Q0.SetEvent;
finally
O000CQO0Q0.Release;
end;
end;
procedure O0CCCQO0Q0.O0OOCQO0Q0(OOOOCQO0Q0:TThreadMethod);
var
OQOOCQO0Q0:integer;
begin
O000CQO0Q0.Acquire;
try
OQOOCQO0Q0:=0;
while OQOOCQO0Q0<OQO0CQO0Q0 do begin
if CompareMethods(OC00CQO0Q0[OQOOCQO0Q0],TMethod(OOOOCQO0Q0))then begin
Dec(OQO0CQO0Q0);
if OQOOCQO0Q0<OQO0CQO0Q0 then
Move(OC00CQO0Q0[OQOOCQO0Q0+1],OC00CQO0Q0[OQOOCQO0Q0],(OQO0CQO0Q0-OQOOCQO0Q0)*SizeOf(TMethod));
Break;
end
else
Inc(OQOOCQO0Q0);
end;
if CompareMethods(O0O0CQO0Q0,TMethod(OOOOCQO0Q0))then begin
if(OOO0CQO0Q0={$IFDEF DARWIN}nil{$ELSE}0{$ENDIF})or(OOO0CQO0Q0<>GetCurrentThreadId)then
if OO00CQO0Q0.WaitFor(INFINITE)<>wrSignaled then
raise Exception.Create('Timeout expired');
end;
finally
O000CQO0Q0.Release;
end;
end;
procedure O0CCCQO0Q0.OOQ0CQO0Q0(OQQ0CQO0Q0:boolean);
begin
O000CQO0Q0.Acquire;
try
if OQO0CQO0Q0>0 then begin
O0O0CQO0Q0:=OC00CQO0Q0[0];
OOO0CQO0Q0:={$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
Dec(OQO0CQO0Q0);
Move(OC00CQO0Q0[1],OC00CQO0Q0[0],OQO0CQO0Q0*SizeOf(TMethod));
OO00CQO0Q0.ResetEvent;
end
else begin
OQ00CQO0Q0.ResetEvent;
Exit;
end;
finally
O000CQO0Q0.Release;
end;
try
try
if OQQ0CQO0Q0 then
SynchronizeWithMainThread(OCO0CQO0Q0)
else
OCO0CQO0Q0;
except
on E:Exception do
if not(E is EAbort)then
if Assigned(ApplicationHandleException)then
ApplicationHandleException(E)
else
ShowException(E,ExceptAddr);
end;
finally
O0O0CQO0Q0.Code:=nil;
O0O0CQO0Q0.Data:=nil;
OOO0CQO0Q0:={$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
OO00CQO0Q0.SetEvent;
end;
end;
procedure O0CCCQO0Q0.OCO0CQO0Q0;
var
O0Q0CQO0Q0:TThreadMethod;
begin
O0Q0CQO0Q0:=TThreadMethod(O0O0CQO0Q0);
if Assigned(O0Q0CQO0Q0)then begin
OOO0CQO0Q0:=GetCurrentThreadId;
O0Q0CQO0Q0();
end;
end;
procedure O0CCCQO0Q0.OCQ0CQO0Q0(O0C0CQO0Q0:TObject);
begin
if OCCCCQO0Q0 then
Exit;
OCCCCQO0Q0:=True;
try
while OQO0CQO0Q0>0 do
OOQ0CQO0Q0(False);
finally
OCCCCQO0Q0:=False;
end;
end;
constructor OO0QQQO0Q0.Create(O0QO0QO0Q0:O0CCCQO0Q0);
begin
OQ0QQQO0Q0:=O0QO0QO0Q0;
inherited Create(False);
end;
destructor OO0QQQO0Q0.Destroy;
begin
Terminate;
OQ0QQQO0Q0.OQ00CQO0Q0.SetEvent;
inherited;
end;
procedure OO0QQQO0Q0.Execute;
begin
while not Terminated do begin
OQ0QQQO0Q0.OQ00CQO0Q0.WaitFor(INFINITE);
if not Terminated then
try
OQ0QQQO0Q0.OOQ0CQO0Q0(True);
except
end;
end;
end;
destructor OCOOCQO0Q0.Destroy;
begin
if O0QOCQO0Q0=OOCQO0QOQ0 then
OCCOCQO0Q0(OQQOCQO0Q0);
inherited;
end;
procedure OCOOCQO0Q0.OQQOCQO0Q0;
begin
try
OOQOCQO0Q0;
finally
if OQOQQQO0Q0<>nil then
OQOQQQO0Q0.Remove(Self);
{$IFNDEF NEXTGEN}
Free;
{$ENDIF}
end;
end;
procedure OCOOCQO0Q0.OQQOCQO0Q0(O0COCQO0Q0:O00CO0QOQ0);
begin
O0QOCQO0Q0:=O0COCQO0Q0;
case O0COCQO0Q0 of
OCCQO0QOQ0:begin
OOQOCQO0Q0;
{$IFNDEF NEXTGEN}
Free;
{$ENDIF}
end;
OQCQO0QOQ0:begin
SynchronizeWithMainThread(OOQOCQO0Q0);
{$IFNDEF NEXTGEN}
Free;
{$ENDIF}
end;
OOCQO0QOQ0:begin
if OQOQQQO0Q0<>nil then
OQOQQQO0Q0.Add(Self);
OQCOCQO0Q0(OQQOCQO0Q0);
end;
else
Assert(False);
end;
end;
initialization
OQOQQQO0Q0:=TCRThreadList.Create;
OCOQQQO0Q0:=nil;
O0QQQQO0Q0:=TCriticalSection.Create;
finalization
FreeAndNil(OQOQQQO0Q0);
FreeAndNil(OCOQQQO0Q0);
O0QQQQO0Q0.Free;
end.
