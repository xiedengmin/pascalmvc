//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsReceiveBufferUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
SysUtils,Classes,SyncObjs,
CLRClasses,CRTypes,CRFunctions,
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLConsts;
{$ELSE}
TdsUtilsUni,TdsSSLConstsUni;
{$ENDIF}
const
OOOOOQO0Q0=64*1024;
type
OQOOOQO0Q0=class
private
OCOOOQO0Q0:TList;
O0QOOQO0Q0:IntPtr;
OOQOOQO0Q0:IntPtr;
OQQOOQO0Q0:Integer;
OCQOOQO0Q0:Integer;
O0COOQO0Q0:Integer;
OOCOOQO0Q0:TEvent;
OQCOOQO0Q0:TCriticalSection;
OCCOOQO0Q0:TCriticalSection;
O00Q0QO0Q0:Boolean;
OO0Q0QO0Q0:TNotifyEvent;
function OQ0Q0QO0Q0:Integer;
procedure OC0Q0QO0Q0(O0OQ0QO0Q0:TNotifyEvent);
public
constructor Create(OQOQ0QO0Q0:Integer);
destructor Destroy;override;
procedure OOQQ0QO0Q0;
function OQQQ0QO0Q0(const OCQQ0QO0Q0:TValueArr;O0CQ0QO0Q0,OOCQ0QO0Q0:Integer):Integer;overload;
function OQQQ0QO0Q0(O00C0QO0Q0:TStream):Integer;overload;
procedure OO0C0QO0Q0(const OQ0C0QO0Q0:TValueArr;OC0C0QO0Q0,O0OC0QO0Q0:Integer);
function OQOC0QO0Q0(OCOC0QO0Q0:Integer;O0QC0QO0Q0:cardinal):boolean;
function OQQC0QO0Q0(const OCQC0QO0Q0:Integer):Byte;
function OQCC0QO0Q0(const OCCC0QO0Q0:Byte;const O0000QO0Q0:Integer):Integer;
property OCO00QO0Q0:Integer read OQ0Q0QO0Q0;
property O0Q00QO0Q0:TNotifyEvent read OO0Q0QO0Q0 write OC0Q0QO0Q0;
end;
OOQ00QO0Q0=OQOOOQO0Q0;
implementation
uses
Math;
constructor OQOOOQO0Q0.Create(OQOQ0QO0Q0:Integer);
begin
inherited Create;
OQCOOQO0Q0:=TCriticalSection.Create;
OCCOOQO0Q0:=TCriticalSection.Create;
OCOOOQO0Q0:=TList.Create;
OOCOOQO0Q0:=CreateEvent;
OQQOOQO0Q0:=0;
OCQOOQO0Q0:=0;
O0COOQO0Q0:=OQOQ0QO0Q0;
OOQOOQO0Q0:=Marshal.AllocHGlobal(OQOQ0QO0Q0);
O0QOOQO0Q0:=OOQOOQO0Q0;
OCOOOQO0Q0.Insert(0,OOQOOQO0Q0);
end;
destructor OQOOOQO0Q0.Destroy;
var
O0QQ0QO0Q0:IntPtr;
begin
while OCOOOQO0Q0.Count>0 do begin
O0QQ0QO0Q0:=IntPtr(OCOOOQO0Q0.Last);
OCOOOQO0Q0.Delete(OCOOOQO0Q0.Count-1);
Marshal.FreeHGlobal(O0QQ0QO0Q0);
end;
OCOOOQO0Q0.Free;
OOCOOQO0Q0.Free;
OQCOOQO0Q0.Free;
OCCOOQO0Q0.Free;
inherited;
end;
procedure OQOOOQO0Q0.OOQQ0QO0Q0;
begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
OOCOOQO0Q0.SetEvent;
O00Q0QO0Q0:=True;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
function OQOOOQO0Q0.OQQQ0QO0Q0(const OCQQ0QO0Q0:TValueArr;O0CQ0QO0Q0,OOCQ0QO0Q0:Integer):Integer;
var
OQCQ0QO0Q0:Integer;
OCCQ0QO0Q0:IntPtr;
begin
Result:=OOCQ0QO0Q0;
while OOCQ0QO0Q0>0 do begin
if OCOOOQO0Q0.Count=1 then begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCOOOQO0Q0.Count=1 then
OQCQ0QO0Q0:=Min(OCQOOQO0Q0-OQQOOQO0Q0,OOCQ0QO0Q0)
else
OQCQ0QO0Q0:=Min(O0COOQO0Q0-OQQOOQO0Q0,OOCQ0QO0Q0);
if OQCQ0QO0Q0<=0 then
break;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end
else
OQCQ0QO0Q0:=Min(O0COOQO0Q0-OQQOOQO0Q0,OOCQ0QO0Q0);
if OCQQ0QO0Q0<>nil then
Move(PtrOffset(O0QOOQO0Q0,OQQOOQO0Q0)^,OCQQ0QO0Q0[O0CQ0QO0Q0],OQCQ0QO0Q0);
Inc(OQQOOQO0Q0,OQCQ0QO0Q0);
Inc(O0CQ0QO0Q0,OQCQ0QO0Q0);
Dec(OOCQ0QO0Q0,OQCQ0QO0Q0);
if OQQOOQO0Q0=O0COOQO0Q0 then begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCOOOQO0Q0.Count>1 then begin
OCCQ0QO0Q0:=IntPtr(OCOOOQO0Q0.Last);
OCOOOQO0Q0.Delete(OCOOOQO0Q0.Count-1);
Marshal.FreeHGlobal(OCCQ0QO0Q0);
O0QOOQO0Q0:=IntPtr(OCOOOQO0Q0.Last);
OQQOOQO0Q0:=0;
end
else begin
if OCQOOQO0Q0=O0COOQO0Q0 then begin
OCQOOQO0Q0:=0;
OQQOOQO0Q0:=0;
end;
break;
end;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
end;
Result:=Result-OOCQ0QO0Q0;
end;
function OQOOOQO0Q0.OQQQ0QO0Q0(O00C0QO0Q0:TStream):Integer;
var
OQCQ0QO0Q0:Integer;
OCCQ0QO0Q0:IntPtr;
begin
Result:=0;
while True do begin
if OCOOOQO0Q0.Count=1 then begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCOOOQO0Q0.Count=1 then
OQCQ0QO0Q0:=OCQOOQO0Q0-OQQOOQO0Q0
else
OQCQ0QO0Q0:=O0COOQO0Q0-OQQOOQO0Q0;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end
else
OQCQ0QO0Q0:=O0COOQO0Q0-OQQOOQO0Q0;
if OQCQ0QO0Q0<=0 then
break;
O00C0QO0Q0.WriteBuffer(PtrOffset(O0QOOQO0Q0,OQQOOQO0Q0)^,OQCQ0QO0Q0);
Dec(OQQOOQO0Q0,OQCQ0QO0Q0);
Inc(Result,OQCQ0QO0Q0);
if OQQOOQO0Q0=O0COOQO0Q0 then begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCOOOQO0Q0.Count>1 then begin
OCCQ0QO0Q0:=IntPtr(OCOOOQO0Q0.Last);
OCOOOQO0Q0.Delete(OCOOOQO0Q0.Count-1);
Marshal.FreeHGlobal(OCCQ0QO0Q0);
O0QOOQO0Q0:=IntPtr(OCOOOQO0Q0.Last);
OQQOOQO0Q0:=0;
end
else begin
if OCQOOQO0Q0=O0COOQO0Q0 then begin
OCQOOQO0Q0:=0;
OQQOOQO0Q0:=0;
end;
break;
end;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
end;
end;
procedure OQOOOQO0Q0.OO0C0QO0Q0(const OQ0C0QO0Q0:TValueArr;OC0C0QO0Q0,O0OC0QO0Q0:Integer);
var
OOOC0QO0Q0:Integer;
begin
if O0OC0QO0Q0<=0 then
Exit;
while O0OC0QO0Q0>0 do begin
if OCQOOQO0Q0=O0COOQO0Q0 then begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCQOOQO0Q0=O0COOQO0Q0 then begin
OOQOOQO0Q0:=Marshal.AllocHGlobal(O0COOQO0Q0);
OCOOOQO0Q0.Insert(0,OOQOOQO0Q0);
OCQOOQO0Q0:=0;
end;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
OOOC0QO0Q0:=Min(O0COOQO0Q0-OCQOOQO0Q0,O0OC0QO0Q0);
Move(OQ0C0QO0Q0[OC0C0QO0Q0],PtrOffset(OOQOOQO0Q0,OCQOOQO0Q0)^,OOOC0QO0Q0);
Inc(OC0C0QO0Q0,OOOC0QO0Q0);
Dec(O0OC0QO0Q0,OOOC0QO0Q0);
O0QOOCOOQ0(OCCOOQO0Q0);
try
Inc(OCQOOQO0Q0,OOOC0QO0Q0);
OOCOOQO0Q0.SetEvent;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
OQCOOQO0Q0.Acquire;
try
if Assigned(OO0Q0QO0Q0)then
OO0Q0QO0Q0(Self);
finally
OQCOOQO0Q0.Release;
end;
end;
function OQOOOQO0Q0.OQOC0QO0Q0(OCOC0QO0Q0:Integer;O0QC0QO0Q0:cardinal):boolean;
var
OOQC0QO0Q0:cardinal;
begin
Result:=False;
OOQC0QO0Q0:=GetTickCount;
while True do begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCO00QO0Q0>=OCOC0QO0Q0 then begin
Result:=True;
Exit;
end
else
if O00Q0QO0Q0 or(O0QC0QO0Q0=0)or(GetTickInterval(OOQC0QO0Q0,GetTickCount)>O0QC0QO0Q0)then
Exit
else
OOCOOQO0Q0.ResetEvent;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
if OOCOOQO0Q0.WaitFor(O0QC0QO0Q0)<>wrSignaled then
Exit;
end;
end;
function OQOOOQO0Q0.OQ0Q0QO0Q0:Integer;
begin
O0QOOCOOQ0(OCCOOQO0Q0);
try
if OCOOOQO0Q0.Count=0 then
Result:=0
else
Result:=(OCOOOQO0Q0.Count-1)*O0COOQO0Q0+OCQOOQO0Q0-OQQOOQO0Q0;
finally
OQQOOCOOQ0(OCCOOQO0Q0);
end;
end;
function OQOOOQO0Q0.OQQC0QO0Q0(const OCQC0QO0Q0:Integer):Byte;
var
O0CC0QO0Q0:IntPtr;
OOCC0QO0Q0:Integer;
begin
if(OQQOOQO0Q0+OCQC0QO0Q0)<O0COOQO0Q0 then
Result:=Marshal.ReadByte(O0QOOQO0Q0,OQQOOQO0Q0+OCQC0QO0Q0)
else begin
if OCQC0QO0Q0>OCO00QO0Q0 then
raise EScError.Create(seNotEnoughData);
OOCC0QO0Q0:=(OCQC0QO0Q0+OQQOOQO0Q0)div O0COOQO0Q0;
O0CC0QO0Q0:=OCOOOQO0Q0[OCOOOQO0Q0.Count-1-OOCC0QO0Q0];
Result:=Marshal.ReadByte(O0CC0QO0Q0,OCQC0QO0Q0+OQQOOQO0Q0-OOCC0QO0Q0*O0COOQO0Q0);
end;
end;
function OQOOOQO0Q0.OQCC0QO0Q0(const OCCC0QO0Q0:Byte;const O0000QO0Q0:Integer):Integer;
var
OO000QO0Q0:IntPtr;
OQ000QO0Q0,OC000QO0Q0:Integer;
O0O00QO0Q0,OOO00QO0Q0,OQO00QO0Q0:Integer;
begin
Result:=-1;
OQO00QO0Q0:=(O0000QO0Q0+OQQOOQO0Q0)div O0COOQO0Q0;
OQ000QO0Q0:=O0000QO0Q0+OQQOOQO0Q0-OQO00QO0Q0*O0COOQO0Q0;
for O0O00QO0Q0:=OCOOOQO0Q0.Count-1-OQO00QO0Q0 downto 0 do begin
OO000QO0Q0:=IntPtr(OCOOOQO0Q0[O0O00QO0Q0]);
if O0O00QO0Q0>0 then
OC000QO0Q0:=O0COOQO0Q0-1
else
OC000QO0Q0:=OCQOOQO0Q0-1;
for OOO00QO0Q0:=OQ000QO0Q0 to OC000QO0Q0 do
if Marshal.ReadByte(OO000QO0Q0,OOO00QO0Q0)=OCCC0QO0Q0 then begin
Result:=(OCOOOQO0Q0.Count-1-O0O00QO0Q0)*O0COOQO0Q0+OOO00QO0Q0-OQQOOQO0Q0;
Exit;
end;
OQ000QO0Q0:=0;
end;
end;
procedure OQOOOQO0Q0.OC0Q0QO0Q0(O0OQ0QO0Q0:TNotifyEvent);
begin
OQCOOQO0Q0.Acquire;
OO0Q0QO0Q0:=O0OQ0QO0Q0;
OQCOOQO0Q0.Release;
end;
end.
