{$I Sdac.inc}
unit Tds7NetUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
SysUtils,CRFunctions,CRTypes,MemData,
{$IFNDEF UNIDACPRO}
Tds7Consts,TdsNet;
{$ELSE}
Tds7ConstsUni,TdsNetUni;
{$ENDIF}
type
OQOOC0CCQ0=class(OQQCC0COQ0)
public
constructor Create;override;
procedure OCCQQ0COQ0(OCOOC0CCQ0:Pointer;O0QOC0CCQ0:Integer;OOQOC0CCQ0:Boolean=False);override;
procedure OC0CQ0COQ0(OQQOC0CCQ0:TBlob;OCQOC0CCQ0:Boolean=False);override;
procedure OQOCQ0COQ0(O0COC0CCQ0:IntPtr;OOCOC0CCQ0:Integer;OQCOC0CCQ0:Boolean=False);override;
procedure O0CCQ0COQ0(const OCCOC0CCQ0:WideString;O00QQ0CCQ0:Boolean=False);override;
end;
implementation
constructor OQOOC0CCQ0.Create;
begin
inherited;
OCQCC0COQ0:=Integer(OOQCO0CCQ0);
end;
procedure OQOOC0CCQ0.OCCQQ0COQ0(OCOOC0CCQ0:Pointer;O0QOC0CCQ0:Integer;OOQOC0CCQ0:Boolean=False);
begin
if OOQOC0CCQ0 then begin
if OCOOC0CCQ0=nil then
OCC0C0COQ0(UInt64(OOQCO0CCQ0))
else begin
OOC0C0COQ0(O0QOC0CCQ0);
OOQ0C0COQ0(O0QOC0CCQ0);
if O0QOC0CCQ0>0 then begin
O0OOO0COQ0(O0QOC0CCQ0);
Move(OCOOC0CCQ0^,OOQ0O0COQ0[OCQ0O0COQ0],O0QOC0CCQ0);
Inc(OCQ0O0COQ0,O0QOC0CCQ0);
OOQ0C0COQ0(OCQCO0CCQ0);
end;
end;
end
else begin
if O0QOC0CCQ0>OO0CO0CCQ0 then
raise Exception.CreateFmt('US_VARBYTE size %d exceeds max length %d',[O0QOC0CCQ0,OO0CO0CCQ0]);
inherited;
end;
end;
procedure OQOOC0CCQ0.OC0CQ0COQ0(OQQOC0CCQ0:TBlob;OCQOC0CCQ0:Boolean=False);
begin
if OCQOC0CCQ0 then begin
if OQQOC0CCQ0=nil then
OCC0C0COQ0(UInt64(OOQCO0CCQ0))
else begin
OOC0C0COQ0(OQQOC0CCQ0.Size);
OOQ0C0COQ0(OQQOC0CCQ0.Size);
if OQQOC0CCQ0.Size>0 then begin
O0C0Q0COQ0(OQQOC0CCQ0);
OOQ0C0COQ0(OCQCO0CCQ0);
end;
end;
end
else begin
if OQQOC0CCQ0.Size>OO0CO0CCQ0 then
raise Exception.CreateFmt('US_VARBYTE size %d exceeds max length %d',[OQQOC0CCQ0.Size,OO0CO0CCQ0]);
inherited;
end;
end;
procedure OQOOC0CCQ0.OQOCQ0COQ0(O0COC0CCQ0:IntPtr;OOCOC0CCQ0:Integer;OQCOC0CCQ0:Boolean=False);
begin
if OQCOC0CCQ0 then begin
OOC0C0COQ0(OOCOC0CCQ0);
OOQ0C0COQ0(OOCOC0CCQ0);
if OOCOC0CCQ0>0 then begin
O0OOO0COQ0(OOCOC0CCQ0);
Move(O0COC0CCQ0^,OOQ0O0COQ0[OCQ0O0COQ0],OOCOC0CCQ0);
Inc(OCQ0O0COQ0,OOCOC0CCQ0);
OOQ0C0COQ0(OCQCO0CCQ0);
end;
end
else begin
if OOCOC0CCQ0>O00CO0CCQ0 then
raise Exception.CreateFmt('US_VARBYTE size %d exceeds max length %d',[OOCOC0CCQ0,O00CO0CCQ0]);
inherited;
end;
end;
procedure OQOOC0CCQ0.O0CCQ0COQ0(const OCCOC0CCQ0:WideString;O00QQ0CCQ0:Boolean=False);
var
OO0QQ0CCQ0:Integer;
begin
if O00QQ0CCQ0 then begin
OO0QQ0CCQ0:=System.Length(OCCOC0CCQ0)*2;
OOC0C0COQ0(OO0QQ0CCQ0);
OOQ0C0COQ0(OO0QQ0CCQ0);
if OO0QQ0CCQ0>0 then begin
O0OOO0COQ0(OO0QQ0CCQ0);
Move(PWideChar(OCCOC0CCQ0)^,OOQ0O0COQ0[OCQ0O0COQ0],OO0QQ0CCQ0);
Inc(OCQ0O0COQ0,OO0QQ0CCQ0);
OOQ0C0COQ0(OCQCO0CCQ0);
end;
end
else begin
if System.Length(OCCOC0CCQ0)>OCCQO0CCQ0 then
raise Exception.CreateFmt('US_VARBYTE size %d exceeds max length %d',[System.Length(OCCOC0CCQ0),OCCQO0CCQ0]);
inherited;
end;
end;
end.
