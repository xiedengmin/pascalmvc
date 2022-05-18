//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access in Direct mode
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsClassesUni;
interface
uses
SysUtils,Classes,Variants,FmtBcd,{$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF NEXTGEN}
Generics.Collections,
{$ENDIF}
{$IFNDEF NODBACCESS}
DB,DBAccess,
{$ENDIF}
CLRClasses,CRTypes,CRFunctions,CRProps,CRParser,CRVio,
MemUtils,MemData,CRAccess,SyncObjs,
{$IFNDEF UNIDACPRO}
SqlClasses,TdsConsts,TdsTypes,TdsProtocol,TdsPackets;
{$ELSE}
SqlClassesUni,TdsConstsUni,TdsTypesUni,TdsProtocolUni,TdsPacketsUni;
{$ENDIF}
type
{$IFDEF NODBACCESS}
ETDSError=class(ECRError)
{$ELSE}
ETDSError=class(EDAError)
{$ENDIF}
protected
OCCO0000Q0:Integer;
O00QC000Q0:Integer;
OO0QC000Q0:string;
OQ0QC000Q0:string;
OC0QC000Q0:Byte;
O0OQC000Q0:Byte;
OOOQC000Q0:Word;
public
constructor Create(OQOQC000Q0:integer;const OCOQC000Q0:string);
procedure Assign(O0QQC000Q0:ETDSError);virtual;
property TdsErrorCode:Integer read OCCO0000Q0 write OCCO0000Q0;
property ErrorSource:Integer read O00QC000Q0 write O00QC000Q0;
property ServerName:string read OO0QC000Q0 write OO0QC000Q0;
property ProcName:string read OQ0QC000Q0 write OQ0QC000Q0;
property State:Byte read OC0QC000Q0 write OC0QC000Q0;
property SeverityClass:Byte read O0OQC000Q0 write O0OQC000Q0;
property LineNumber:Word read OOOQC000Q0 write OOOQC000Q0;
{$IFNDEF NODBACCESS}
function IsFatalError:Boolean;override;
function IsKeyViolation:Boolean;override;
{$ENDIF}
end;
TTDSConnector=class(TCRConnector)
protected
OOQQC000Q0:OQOCQCQOQ0;
OQQQC000Q0:TCRConnection;
OCQQC000Q0:O00QCOQOQ0;
O0CQC000Q0:TCriticalSection;
function OOCQC000Q0:TCRConnectionClass;virtual;abstract;
function OQCQC000Q0:OCCQCOQOQ0;virtual;abstract;
function OCCQC000Q0:TIPVersion;virtual;abstract;
procedure O00CC000Q0;virtual;
function OO0CC000Q0:boolean;virtual;
procedure OQ0CC000Q0(OC0CC000Q0:Integer;const O0OCC000Q0:string;OOOCC000Q0:TObject);
procedure O0QCC000Q0(const OOQCC000Q0:OQO0CCQOQ0;OQQCC000Q0:TObject);
procedure OO00C000Q0(const OQ00C000Q0:OQCCCCQOQ0;OC00C000Q0:TObject);
procedure OOO0C000Q0(const OQO0C000Q0:array of const);
procedure OCO0C000Q0(var O0Q0C000Q0:string;var OOQ0C000Q0:integer);
public
constructor Create(OC0OC000Q0:TCRConnection);override;
destructor Destroy;override;
procedure Connect;override;
procedure Disconnect;override;
function GetConnectionSwap:TCRConnection;
procedure ReturnConnectionSwap(O0OOC000Q0:TCRConnection);
procedure ReleaseConnectionSwap;
class function CloneException(OOOOC000Q0:Exception):Exception;override;
procedure InitConnectionSettings;virtual;abstract;
function MultipleConnectionsSupported:boolean;virtual;
function GetClientVersion:string;override;
function GetClientMajorVersion:integer;override;
property Protocol:OQOCQCQOQ0 read OOQQC000Q0;
end;
TTDSCommand=class(TSqlCommand)
private
OCOOC000Q0:TCRConnection;
O0QOC000Q0:TCriticalSection;
procedure OOQOC000Q0;
procedure OQQOC000Q0(OCQOC000Q0:boolean=False);
procedure O0COC000Q0;
protected
OOCOC000Q0:integer;
OQCOC000Q0:boolean;
OCCOC000Q0:OOQQ0QQOQ0;
O00QQ000Q0:boolean;
OO0QQ000Q0:string;
OQ0QQ000Q0:boolean;
OC0QQ000Q0:boolean;
function O0OQQ000Q0(OOOQQ000Q0:TParamDesc):boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
procedure OQOQQ000Q0(OCOQQ000Q0,O0QQQ000Q0:integer;const OOQQQ000Q0:boolean);
procedure OQQQQ000Q0(OCQQQ000Q0,O0CQQ000Q0:integer;const OOCQQ000Q0:variant);virtual;
function OQCQQ000Q0(OCCQQ000Q0,O00CQ000Q0:integer):PVariant;
function OQ0CQ000Q0(OC0CQ000Q0,O0OCQ000Q0:integer):TSharedObject;
procedure OOOCQ000Q0(OQOCQ000Q0:integer;const OCOCQ000Q0:string;
O0QCQ000Q0:TParamDirection;OOQCQ000Q0:Word;const OQQCQ000Q0:variant);
procedure O0CCQ000Q0(OOCCQ000Q0,OQCCQ000Q0:integer);
procedure DoExecuteException(OO00Q000Q0:TObject;OQ00Q000Q0:Exception;var OC00Q000Q0:boolean);override;
procedure WaitAsynchCompletion;override;
procedure CancelCommand;override;
procedure EndExecute(OCO0Q000Q0:Exception);override;
procedure OOQ0Q000Q0;virtual;
procedure OQQ0Q000Q0;virtual;abstract;
function OCQ0Q000Q0:integer;virtual;
procedure O0C0Q000Q0(OOC0Q000Q0:boolean);virtual;
function OQC0Q000Q0:integer;virtual;abstract;
function OCC0Q000Q0:integer;virtual;abstract;
procedure O00OQ000Q0;virtual;
function OO0OQ000Q0:Boolean;
procedure OQ0OQ000Q0(OC0OQ000Q0:boolean;out O0OOQ000Q0,OOOOQ000Q0,OQOOQ000Q0,OCOOQ000Q0:string;out O0QOQ000Q0:integer);
function OCQOQ000Q0(const O0COQ000Q0:string):boolean;
function O00QOCCCQ0(const OO0QOCCCQ0:string;out OQ0QOCCCQ0:boolean):string;
function GetRowsAffected:NativeInt;override;
procedure SetRowsAffected(OQ0COCCCQ0:NativeInt);override;
public
constructor Create;override;
destructor Destroy;override;
class function GetContextClass:OQCQCOQOQ0;virtual;
function GetProtocol:OQOCQCQOQ0;
procedure SetConnection(OC0COCCCQ0:TCRConnection);override;
procedure Execute;override;
procedure Close;override;
function GetPrepared:boolean;override;
procedure Prepare;override;
procedure Unprepare;override;
property CmdContext:OOQQ0QQOQ0 read OCCOC000Q0;
property IsQuery:Boolean read OO0OQ000Q0;
end;
TTDSRecordSet=class(TSqlRecordSet)
private
OOQCOCCCQ0:TTDSCommand;
procedure OQQCOCCCQ0;
procedure OCQCOCCCQ0(O0CCOCCCQ0:boolean);
function OCCCOCCCQ0(O000OCCCQ0:Boolean):Boolean;
protected
OO00OCCCQ0:boolean;
OQ00OCCCQ0:boolean;
OC00OCCCQ0:boolean;
O0O0OCCCQ0:array of OQO0QQCOQ0;
procedure OOO0OCCCQ0;virtual;
procedure SetCommand(OCO0OCCCQ0:TCRCommand);override;
function O0Q0OCCCQ0(OOQ0OCCCQ0:TFieldDesc;OQQ0OCCCQ0,OCQ0OCCCQ0:IntPtr):boolean;
procedure O0C0OCCCQ0;
procedure CreateFieldDescs;override;
procedure InternalOpen(OO0OOCCCQ0:Boolean=False);override;
procedure InternalClose;override;
procedure InternalPrepare;override;
procedure InternalUnPrepare;override;
function OQOOOCCCQ0(OCOOOCCCQ0:TSqlFieldDesc;O0QOOCCCQ0:Integer):Integer;virtual;
function OOQOOCCCQ0(OQQOOCCCQ0:TSqlFieldDesc;OCQOOCCCQ0:Integer):Integer;virtual;
procedure O0COOCCCQ0(OOCOOCCCQ0:TSqlFieldDesc;OQCOOCCCQ0:OCO0QQCOQ0;
OCCOOCCCQ0,O00Q0CCCQ0:Pointer;var OO0Q0CCCQ0:Integer);virtual;
procedure O0OQ0CCCQ0(OOOQ0CCCQ0:TSqlFieldDesc;OQOQ0CCCQ0:OCO0QQCOQ0;
OCOQ0CCCQ0,O0QQ0CCCQ0:Pointer;var OOQQ0CCCQ0:Integer);virtual;
procedure O0CQ0CCCQ0(OOCQ0CCCQ0:TSqlFieldDesc;OQCQ0CCCQ0:OCO0QQCOQ0;
OCCQ0CCCQ0,O00C0CCCQ0:Pointer;var OO0C0CCCQ0:Integer);virtual;
procedure O0OC0CCCQ0(OOOC0CCCQ0:TSqlFieldDesc;OQOC0CCCQ0:OCO0QQCOQ0;
OCOC0CCCQ0,O0QC0CCCQ0:Pointer;var OOQC0CCCQ0:Integer);virtual;
procedure O0CC0CCCQ0(OOCC0CCCQ0:TSqlFieldDesc;OQCC0CCCQ0:IntPtr;OCCC0CCCQ0:integer);
procedure OOC00CCCQ0(OQC00CCCQ0:IntPtr;OCC00CCCQ0:integer);
procedure FetchBlock(OC0O0CCCQ0:PBlockHeader;O0OO0CCCQ0:boolean;out OOOO0CCCQ0:integer);override;
function OOQO0CCCQ0:boolean;virtual;
function FetchingAccessible(OCQO0CCCQ0:Boolean):Boolean;override;
function OQCO0CCCQ0(OCCO0CCCQ0:integer;O00QCCCCQ0:boolean):NativeUInt;virtual;
procedure ProcessNoResult(OQ0QCCCCQ0:Boolean);override;
procedure DoAfterFetch;override;
procedure O0OQCCCCQ0(var OOOQCCCCQ0:TSqlFieldDesc;OQOQCCCCQ0:integer);virtual;abstract;
public
constructor Create;override;
destructor Destroy;override;
procedure ExecCommand(OCOQCCCCQ0:Integer=1;O0QQCCCCQ0:Integer=0);override;
procedure Reopen;override;
procedure Disconnect;override;
procedure AssignToNextResult(OOQQCCCCQ0:TSqlRecordSet);override;
function HasNextResult:boolean;override;
function CheckNextResult:boolean;override;
function CanDisconnect:boolean;override;
function RowsReturn:boolean;override;
property NativeRowset:boolean read OO00OCCCQ0;
end;
OQQQCCCCQ0=class of TTDSRecordSet;
TTDSLoader=class(TCRLoader)
protected
OCQQCCCCQ0:array of Variant;
O0CQCCCCQ0:TCRObjectList;
OOCQCCCCQ0:string;
OQCQCCCCQ0:Integer;
OCCQCCCCQ0:Integer;
O00CCCCCQ0:TTDSCommand;
OO0CCCCCQ0:TCRTransaction;
OQ0CCCCCQ0:TTDSRecordSet;
function OC0CCCCCQ0(O0OCCCCCQ0:TSqlFieldDesc;OOOCCCCCQ0:TParamDesc):boolean;virtual;abstract;
procedure OQOCCCCCQ0;virtual;
procedure OCOCCCCCQ0;virtual;
function IsPutColumnDataAllowed:boolean;override;
procedure OOQCCCCCQ0;
procedure OCQCCCCCQ0(const O0CCCCCCQ0:Integer);
function OO00CCCCQ0(const OQ00CCCCQ0:TParamDesc):string;virtual;
function OC00CCCCQ0(const O0O0CCCCQ0:TParamDesc):Boolean;virtual;
procedure OOO0CCCCQ0;
public
constructor Create;override;
class function GetColumnClass:TCRLoaderColumnClass;override;
procedure Prepare;override;
procedure Prepare(OCQ0CCCCQ0:integer);overload;override;
procedure DoLoad;override;
procedure PutColumnData(OCOOCCCCQ0:integer;O0QOCCCCQ0:integer;const OOQOCCCCQ0:variant);override;
procedure Finish;override;
end;
implementation
uses
CRNumeric,DAConsts;
var
OQQOCCCCQ0:Variant;
constructor ETDSError.Create(OQOQC000Q0:integer;const OCOQC000Q0:string);
begin
inherited Create(OQOQC000Q0,OCOQC000Q0);
O00QC000Q0:=1;
OCCO0000Q0:=0;
end;
procedure ETDSError.Assign(O0QQC000Q0:ETDSError);
begin
O00QC000Q0:=O0QQC000Q0.O00QC000Q0;
FErrorCode:=O0QQC000Q0.FErrorCode;
OCCO0000Q0:=O0QQC000Q0.OCCO0000Q0;
Message:=O0QQC000Q0.Message;
{$IFNDEF NODBACCESS}
Component:=O0QQC000Q0.Component;
{$ENDIF}
end;
{$IFNDEF NODBACCESS}
function ETDSError.IsFatalError:Boolean;
begin
if FErrorCode<>0 then
Result:=SeverityClass>=OCQQ0QCOQ0
else
Result:=O00QC000Q0<>0;
end;
function ETDSError.IsKeyViolation:boolean;
begin
Result:=(ErrorCode=2627)
end;
{$ENDIF}
constructor TTDSConnector.Create(OC0OC000Q0:TCRConnection);
begin
inherited;
InitConnectionSettings;
OOQQC000Q0:=OQCQC000Q0.Create;
OQQQC000Q0:=nil;
O0CQC000Q0:=TCriticalSection.Create;
end;
destructor TTDSConnector.Destroy;
begin
FreeAndNil(OOQQC000Q0);
FreeAndNil(OQQQC000Q0);
FreeAndNil(O0CQC000Q0);
inherited;
end;
procedure TTDSConnector.OQ0CC000Q0(OC0CC000Q0:Integer;const O0OCC000Q0:string;OOOCC000Q0:TObject);
var
OQOCC000Q0:ETDSError;
OCOCC000Q0:boolean;
begin
OQOCC000Q0:=ETDSError.Create(0,O0OCC000Q0);
{$IFNDEF NODBACCESS}
OQOCC000Q0.Component:=OOOCC000Q0;
{$ENDIF}
OQOCC000Q0.TdsErrorCode:=OC0CC000Q0;
if OC0CC000Q0=0 then begin
Assert(OQOCC000Q0<>nil);
try
if Assigned(Connection.OnInfoMessage)then
Connection.OnInfoMessage(OQOCC000Q0);
finally
OQOCC000Q0.Free;
end;
end
else begin
OCOCC000Q0:=True;
try
if Assigned(Connection.OnError)then
Connection.DoError(OQOCC000Q0,OCOCC000Q0);
if OCOCC000Q0 then
raise OQOCC000Q0
else
Abort;
finally
if not OCOCC000Q0 then
OQOCC000Q0.Free;
end;
end;
end;
procedure TTDSConnector.O0QCC000Q0(const OOQCC000Q0:OQO0CCQOQ0;OQQCC000Q0:TObject);
var
OCQCC000Q0:ETDSError;
O0CCC000Q0:boolean;
OOCCC000Q0:Byte;
OQCCC000Q0,OCCCC000Q0:integer;
O000C000Q0:string;
begin
Assert(Length(OOQCC000Q0)>0);
OCCCC000Q0:=Length(OOQCC000Q0);
OCQCC000Q0:=ETDSError.Create(OOQCC000Q0[0].OCCCCCQOQ0,OOQCC000Q0[0].OC00CCQOQ0);
{$IFNDEF NODBACCESS}
OCQCC000Q0.Component:=OQQCC000Q0;
{$ENDIF}
OCQCC000Q0.ServerName:=OOQCC000Q0[0].O0O0CCQOQ0;
OCQCC000Q0.ProcName:=OOQCC000Q0[0].OOO0CCQOQ0;
OCQCC000Q0.State:=OOQCC000Q0[0].OO00CCQOQ0;
OCQCC000Q0.SeverityClass:=OOQCC000Q0[0].OQ00CCQOQ0;
OCQCC000Q0.LineNumber:=OOQCC000Q0[0].O000CCQOQ0;
OOCCC000Q0:=0;
O000C000Q0:='';
for OQCCC000Q0:=0 to OCCCC000Q0-1 do begin
if O000C000Q0='' then
O000C000Q0:=OOQCC000Q0[OQCCC000Q0].OC00CCQOQ0
else
O000C000Q0:=OOQCC000Q0[OQCCC000Q0].OC00CCQOQ0+#$D#$A+O000C000Q0;
if OOQCC000Q0[OQCCC000Q0].OQ00CCQOQ0>OOCCC000Q0 then
OOCCC000Q0:=OOQCC000Q0[OQCCC000Q0].OQ00CCQOQ0;
end;
OCQCC000Q0.Message:=O000C000Q0;
if OOCCC000Q0<=10 then begin
Assert(OCQCC000Q0<>nil);
try
if Assigned(Connection.OnInfoMessage)then
Connection.OnInfoMessage(OCQCC000Q0);
finally
OCQCC000Q0.Free;
end;
end
else begin
O0CCC000Q0:=True;
try
if Assigned(Connection.OnError)then
Connection.DoError(OCQCC000Q0,O0CCC000Q0);
if O0CCC000Q0 then
raise OCQCC000Q0
else
Abort;
finally
if not O0CCC000Q0 then
OCQCC000Q0.Free;
end;
end;
end;
procedure TTDSConnector.OO00C000Q0(const OQ00C000Q0:OQCCCCQOQ0;OC00C000Q0:TObject);
var
O0O0C000Q0:ETDSError;
begin
O0O0C000Q0:=ETDSError.Create(OQ00C000Q0.OCCCCCQOQ0,OQ00C000Q0.OC00CCQOQ0);
{$IFNDEF NODBACCESS}
O0O0C000Q0.Component:=OC00C000Q0;
{$ENDIF}
O0O0C000Q0.ServerName:=OQ00C000Q0.O0O0CCQOQ0;
O0O0C000Q0.ProcName:=OQ00C000Q0.OOO0CCQOQ0;
O0O0C000Q0.State:=OQ00C000Q0.OO00CCQOQ0;
O0O0C000Q0.SeverityClass:=OQ00C000Q0.OQ00CCQOQ0;
O0O0C000Q0.LineNumber:=OQ00C000Q0.O000CCQOQ0;
try
if Assigned(Connection.OnInfoMessage)then
Connection.OnInfoMessage(O0O0C000Q0);
finally
O0O0C000Q0.Free;
end;
end;
procedure TTDSConnector.OOO0C000Q0(const OQO0C000Q0:array of const);
begin
Assert(Length(OQO0C000Q0)>=4);
FServerName:=string(OQO0C000Q0[0].VWideString);
FServerVersion:=string(OQO0C000Q0[1].VWideString);
FServerMajorVersion:=OQO0C000Q0[2].VInteger;
FServerMinorVersion:=OQO0C000Q0[3].VInteger;
end;
procedure TTDSConnector.Connect;
begin
{$IFDEF LOG_PACKETS}
AddToLog(Format('Connecting to %s Instance "%s" on port %d',[Connection.Host,Connection.InstanceName,Connection.Port]));
{$ENDIF}
O0CQC000Q0.Enter;
try
if OOQQC000Q0.OO0Q0QQOQ0=nil then
O00CC000Q0;
try
OOQQC000Q0.OCO0OQQOQ0(OCQQC000Q0,OO0CC000Q0,OQ0CC000Q0,O0QCC000Q0,OO00C000Q0);
except
on E:ETDSError do
begin
OOQQC000Q0.O0O0OQQOQ0;
raise;
end;
on E:Exception do
begin
OOQQC000Q0.O0O0OQQOQ0;
OQ0CC000Q0(-1,E.Message,Self);
end;
end;
finally
O0CQC000Q0.Leave;
end;
end;
procedure TTDSConnector.Disconnect;
begin
O0CQC000Q0.Enter;
try
if FOwner.NativeConnection then
OOQQC000Q0.O0O0OQQOQ0
else
OOQQC000Q0.OO0Q0QQOQ0:=nil;
ReleaseConnectionSwap;
finally
O0CQC000Q0.Leave;
end;
end;
function TTDSConnector.GetConnectionSwap:TCRConnection;
begin
O0CQC000Q0.Enter;
try
if OQQQC000Q0<>nil then begin
Result:=OQQQC000Q0;
OQQQC000Q0:=nil;
end
{$IFNDEF NODBACCESS}
else if Assigned(Connection.GetPooledConnection)then
Result:=Connection.GetPooledConnection
{$ENDIF}
else begin
Result:=OOCQC000Q0.Create;
Result.Assign(Connection);
end;
finally
O0CQC000Q0.Leave;
end;
end;
procedure TTDSConnector.ReturnConnectionSwap(O0OOC000Q0:TCRConnection);
begin
O0CQC000Q0.Enter;
try
if O0OOC000Q0=nil then
Exit;
{$IFNDEF NODBACCESS}
if O0OOC000Q0.Pool<>nil then
O0OOC000Q0.ReturnToPool
else
{$ENDIF}
begin
if O0OOC000Q0.IsValid and(OQQQC000Q0=nil)then
OQQQC000Q0:=O0OOC000Q0
else
O0OOC000Q0.Free;
ReleaseConnectionSwap;
end;
finally
O0CQC000Q0.Leave;
end;
end;
procedure TTDSConnector.ReleaseConnectionSwap;
begin
O0CQC000Q0.Enter;
try
if(OOQQC000Q0=nil)or
(OOQQC000Q0.OO0Q0QQOQ0=nil)or
not OOQQC000Q0.OO0Q0QQOQ0.Connected or
OOQQC000Q0.OQOQ0QQOQ0
then
FreeAndNil(OQQQC000Q0);
finally
O0CQC000Q0.Leave;
end;
end;
function TTDSConnector.OO0CC000Q0:boolean;
begin
Result:=False;
end;
function TTDSConnector.MultipleConnectionsSupported:boolean;
begin
Result:=False;
end;
procedure TTDSConnector.O00CC000Q0;
begin
OOQQC000Q0.OCCCOQQOQ0(nil,OCQQC000Q0.OOOQCOQOQ0,OCQQC000Q0.OQQQCOQOQ0,OCCQC000Q0);
OOQQC000Q0.O00Q0QQOQ0:=OOO0C000Q0;
end;
class function TTDSConnector.CloneException(OOOOC000Q0:Exception):Exception;
begin
if OOOOC000Q0 is ETDSError then begin
Result:=ETDSError.Create(ETDSError(OOOOC000Q0).ErrorSource,ETDSError(OOOOC000Q0).Message);
ETDSError(Result).Assign(ETDSError(OOOOC000Q0));
end
else
Result:=Exception.Create(OOOOC000Q0.Message);
end;
procedure TTDSConnector.OCO0C000Q0(var O0Q0C000Q0:string;var OOQ0C000Q0:integer);
type
OQC0C000Q0=(OQQ0C000Q0,OCQ0C000Q0,O0C0C000Q0,OOC0C000Q0);
var
OCC0C000Q0:TParser;
O00OC000Q0:integer;
OO0OC000Q0:string;
OQ0OC000Q0:OQC0C000Q0;
begin
OCC0C000Q0:=TParser.Create(O0Q0C000Q0);
try
OCC0C000Q0.OmitBlank:=True;
OCC0C000Q0.OmitComment:=True;
OCC0C000Q0.QuotedString:=True;
OQ0OC000Q0:=OQQ0C000Q0;
repeat
O00OC000Q0:=OCC0C000Q0.GetNext(OO0OC000Q0);
case OQ0OC000Q0 of
OQQ0C000Q0:begin
if UpperCase(OO0OC000Q0)='TCP' then
OQ0OC000Q0:=OCQ0C000Q0
else
if O00OC000Q0=lcIdent then begin
OQ0OC000Q0:=O0C0C000Q0;
O0Q0C000Q0:=OO0OC000Q0;
end
else
Break;
end;
OCQ0C000Q0:begin
if O00OC000Q0=lxColon then begin
OQ0OC000Q0:=O0C0C000Q0;
O0Q0C000Q0:='';
end
else
Break;
end;
O0C0C000Q0:begin
if O00OC000Q0=lxComma then
OQ0OC000Q0:=OOC0C000Q0
else
O0Q0C000Q0:=O0Q0C000Q0+OO0OC000Q0;
end;
OOC0C000Q0:begin
if O00OC000Q0=lcNumber then
OOQ0C000Q0:=StrToInt(OO0OC000Q0);
end;
end;
until O00OC000Q0=lcEnd;
finally
OCC0C000Q0.Free;
end;
end;
function TTDSConnector.GetClientVersion:string;
var
OQOOC000Q0:word;
begin
OQOOC000Q0:=OOQQC000Q0.OOOOOQQOQ0;
Result:=IntToStr(OQOOC000Q0 shr 8)+'.'+IntToStr(OQOOC000Q0 and$0F);
end;
function TTDSConnector.GetClientMajorVersion:integer;
begin
Result:=OOQQC000Q0.OOOOOQQOQ0 shr 8;
end;
constructor TTDSCommand.Create;
begin
inherited Create;
O0QOC000Q0:=TCriticalSection.Create;
OCCOC000Q0:=GetContextClass.Create(nil);
end;
destructor TTDSCommand.Destroy;
begin
FreeAndNil(FExecutor);
Unprepare;
inherited;
FreeAndNil(OCCOC000Q0);
FreeAndNil(O0QOC000Q0);
end;
function TTDSCommand.O0OQQ000Q0(OOOQQ000Q0:TParamDesc):boolean;
begin
Result:=(OOOQQ000Q0.GetDataType in[dtBlob,dtMemo,dtWideMemo])and
(OOOQQ000Q0.GetParamType in[pdResult,pdOutput,pdInputOutput]);
end;
procedure TTDSCommand.OQOQQ000Q0(OCOQQ000Q0,O0QQQ000Q0:integer;const OOQQQ000Q0:boolean);
begin
Params[OCOQQ000Q0].SetNull(OOQQQ000Q0);
Params[OCOQQ000Q0].ItemNull[O0QQQ000Q0]:=OOQQQ000Q0;
end;
procedure TTDSCommand.OQQQQ000Q0(OCQQQ000Q0,O0CQQ000Q0:integer;const OOCQQ000Q0:variant);
begin
if(OCQQQ000Q0>=0)and(OCQQQ000Q0<Params.Count)then
Params[OCQQQ000Q0].ItemValue[O0CQQ000Q0]:=OOCQQ000Q0;
end;
function TTDSCommand.OQCQQ000Q0(OCCQQ000Q0,O00CQ000Q0:integer):PVariant;
var
OO0CQ000Q0:TParamDesc;
begin
OO0CQ000Q0:=Params[OCCQQ000Q0];
if(OO0CQ000Q0.GetArraySize>1)and(TVarData(OO0CQ000Q0.Value).VType=varEmpty)then
Result:=@OQQOCCCCQ0
else if OO0CQ000Q0.ItemNull[O00CQ000Q0]then
Result:=@OQQOCCCCQ0
else
Result:=OO0CQ000Q0.GetItemPtr(O00CQ000Q0);
end;
function TTDSCommand.OQ0CQ000Q0(OC0CQ000Q0,O0OCQ000Q0:integer):TSharedObject;
begin
Result:=Params[OC0CQ000Q0].GetObject;
end;
procedure TTDSCommand.OOOCQ000Q0(OQOCQ000Q0:integer;const OCOCQ000Q0:string;
O0QCQ000Q0:TParamDirection;OOQCQ000Q0:Word;const OQQCQ000Q0:variant);
var
OCQCQ000Q0:TParamDesc;
begin
OCQCQ000Q0:=GetParamDescClass.Create;
OCQCQ000Q0.SetName(OCOCQ000Q0);
OCQCQ000Q0.SetParamType(O0QCQ000Q0);
OCQCQ000Q0.SetDataType(OOQCQ000Q0);
OCQCQ000Q0.SetValue(OQQCQ000Q0);
Params.Insert(OQOCQ000Q0,OCQCQ000Q0);
end;
procedure TTDSCommand.O0CCQ000Q0(OOCCQ000Q0,OQCCQ000Q0:integer);
var
OCCCQ000Q0:integer;
begin
for OCCCQ000Q0:=0 to OQCCQ000Q0-1 do
Params.Delete(OOCCQ000Q0);
end;
procedure TTDSCommand.DoExecuteException(OO00Q000Q0:TObject;OQ00Q000Q0:Exception;var OC00Q000Q0:boolean);
begin
if(OQ00Q000Q0 is ETDSError)then
FConnection.DoError(ETDSError(OQ00Q000Q0),OC00Q000Q0);
end;
procedure TTDSCommand.WaitAsynchCompletion;
begin
OCCOC000Q0.OCQ00OQOQ0.O0O000COQ0;
while not FExecutor.Terminated and not OCCOC000Q0.OQ0O0OQOQ0 do
OCCOC000Q0.OCQ00OQOQ0.OQO000COQ0;
if not FExecutor.Terminated then
OCCOC000Q0.OOQ00OQOQ0;
end;
procedure TTDSCommand.OOQOC000Q0;
begin
O0QOC000Q0.Enter;
try
if FCursorState=csExecuted then
Exit;
if OCOOC000Q0<>nil then
Exit;
if GetProtocol.OQOQ0QQOQ0 then
Exit;
if not TTDSConnector(FConnection.GetConnector).MultipleConnectionsSupported then begin
GetProtocol.O0OQ0QQOQ0.O0QCOCQOQ0;
if GetProtocol.OQOQ0QQOQ0 then
Exit
else
raise Exception.Create(SObjectWasOpen);
end;
if FConnection.GetInternalTransaction.GetInTransaction then
raise Exception.Create(SCannotCreateNewConnectionInTransaction);
OCOOC000Q0:=FConnection;
FConnection:=TTDSConnector(OCOOC000Q0.GetConnector).GetConnectionSwap;
FConnection.Connect('');
FConnection.Additional:=True;
inherited SetConnection(FConnection);
finally
O0QOC000Q0.Leave;
end;
end;
procedure TTDSCommand.OQQOC000Q0(OCQOC000Q0:boolean=False);
begin
O0QOC000Q0.Enter;
try
if OCOOC000Q0=nil then
Exit;
if FConnection<>nil then begin
FConnection.Additional:=True;
FConnection.IsValid:=not OCQOC000Q0;
TTDSConnector(OCOOC000Q0.GetConnector).ReturnConnectionSwap(FConnection);
end;
FConnection:=OCOOC000Q0;
inherited SetConnection(OCOOC000Q0);
OCOOC000Q0:=nil;
finally
O0QOC000Q0.Leave;
end;
end;
procedure TTDSCommand.O0COC000Q0;
begin
TTDSConnector(FConnection.GetConnector).ReleaseConnectionSwap;
end;
procedure TTDSCommand.SetConnection(OC0COCCCQ0:TCRConnection);
begin
if FConnection<>OC0COCCCQ0 then
OQQOC000Q0;
inherited;
end;
class function TTDSCommand.GetContextClass:OQCQCOQOQ0;
begin
Result:=OOQQ0QQOQ0;
end;
function TTDSCommand.GetProtocol:OQOCQCQOQ0;
begin
if(FConnection<>nil)and(FConnection.GetConnector<>nil)then
Result:=TTDSConnector(FConnection.GetConnector).Protocol
else
Result:=nil;
end;
procedure TTDSCommand.Execute;
var
O0OCOCCCQ0:OQOCQCQOQ0;
OOOCOCCCQ0:OQOCQCQOQ0;
begin
if(FCursorState<>csInactive)and(FCursorState<>csPrepared)then
Exit;
O0OCOCCCQ0:=GetProtocol;
O0OCOCCCQ0.OOO0OQQOQ0;
try
OOQOC000Q0;
OOOCOCCCQ0:=GetProtocol;
if(OOOCOCCCQ0=nil)or not OOOCOCCCQ0.OQOQ0QQOQ0 then
raise Exception.Create(SConnectionIsBusy);
OOOCOCCCQ0.OOO0OQQOQ0;
try
O0OCOCCCQ0.OQO0OQQOQ0;
O0OCOCCCQ0:=nil;
OCCOC000Q0.OCC00OQOQ0:=OOOCOCCCQ0;
FCanReadParams:=False;
FExecuting:=True;
FWaitForBreak:=False;
SetCursorState(csExecuting);
try
if not O00QQ000Q0 then
OOQ0Q000Q0;
OQ0QQ000Q0:=OCQOQ000Q0(FSQL);
O00OQ000Q0;
OQQ0Q000Q0;
{$IFDEF AUTOTEST}
if not OC0QQ000Q0 then
Inc(__ServerExecuteCount);
{$ENDIF}
except
on E:Exception do begin
EndExecute(E);
raise;
end;
end;
finally
OOOCOCCCQ0.OQO0OQQOQ0;
end;
finally
if O0OCOCCCQ0<>nil then
O0OCOCCCQ0.OQO0OQQOQ0;
end;
if FNonBlocking and not FRequestResultSet then
CreateExecutor
else
EndExecute(nil);
end;
procedure TTDSCommand.Close;
begin
OCCOC000Q0.OCQ0QQQOQ0;
end;
procedure TTDSCommand.CancelCommand;
begin
if(OCCOC000Q0.OCC00OQOQ0<>nil)and OCCOC000Q0.OCC00OQOQ0.OQOOOQQOQ0 then
OCCOC000Q0.OOC0QQQOQ0;
end;
procedure TTDSCommand.O00OQ000Q0;
begin
OCCOC000Q0.OCQO0OQOQ0:=TTDSConnector(FConnection.GetConnector).MultipleConnectionsSupported;
end;
function TTDSCommand.OO0OQ000Q0:Boolean;
begin
Result:=(OCCOC000Q0.O0OO0OQOQ0 or OCCOC000Q0.OOOO0OQOQ0)
and(OCCOC000Q0.OO00QQQOQ0<>nil);
end;
function TTDSCommand.OCQ0Q000Q0:integer;
begin
Result:=-1;
end;
procedure TTDSCommand.O0C0Q000Q0(OOC0Q000Q0:boolean);
begin
end;
procedure TTDSCommand.Prepare;
var
OQOCOCCCQ0:OQOCQCQOQ0;
OCOCOCCCQ0:OQOCQCQOQ0;
begin
if O00QQ000Q0 then
Exit;
OQOCOCCCQ0:=GetProtocol;
OQOCOCCCQ0.OOO0OQQOQ0;
try
OOQOC000Q0;
OCOCOCCCQ0:=GetProtocol;
if(OCOCOCCCQ0=nil)or not OCOCOCCCQ0.OQOQ0QQOQ0 then
raise Exception.Create(SConnectionIsBusy);
OCOCOCCCQ0.OOO0OQQOQ0;
try
OQOCOCCCQ0.OQO0OQQOQ0;
OQOCOCCCQ0:=nil;
OCCOC000Q0.OCC00OQOQ0:=OCOCOCCCQ0;
OOQ0Q000Q0;
{$IFDEF AUTOTEST}
Inc(__ServerPrepareCount);
{$ENDIF}
if FUseDescribeParams then
O0C0Q000Q0(True);
inherited;
OQ0QQ000Q0:=OCQOQ000Q0(FSQL);
if not OQ0QQ000Q0 then begin
OOCOC000Q0:=OCQ0Q000Q0;
O00OQ000Q0;
OCCOC000Q0.OOCC0OQOQ0(OQC0Q000Q0,OOCOC000Q0);
end;
finally
OCOCOCCCQ0.OQO0OQQOQ0;
end;
finally
if OQOCOCCCQ0<>nil then
OQOCOCCCQ0.OQO0OQQOQ0;
end;
O00QQ000Q0:=True;
end;
procedure TTDSCommand.Unprepare;
var
O0QCOCCCQ0:OQOCQCQOQ0;
begin
inherited;
if not O00QQ000Q0 then
Exit;
try
O0QCOCCCQ0:=GetProtocol;
O0QCOCCCQ0.OOO0OQQOQ0;
try
OCCOC000Q0.OQQ00OQOQ0;
OCCOC000Q0.OCQ0QQQOQ0;
O00QQ000Q0:=False;
if OCCOC000Q0.OCC00OQOQ0.OQOQ0QQOQ0 and not OQ0QQ000Q0 then begin
O00OQ000Q0;
OCCOC000Q0.OOCC0OQOQ0(OCC0Q000Q0,OOCOC000Q0);
end;
finally
O0QCOCCCQ0.OQO0OQQOQ0;
end;
finally
OQQOC000Q0;
end;
end;
procedure TTDSCommand.EndExecute(OCO0Q000Q0:Exception);
var
O0Q0Q000Q0:OQOCQCQOQ0;
begin
FCanReadParams:=True;
if IsQuery and FRequestResultSet then
FCursorState:=csExecuted
else
if O00QQ000Q0 then
FCursorState:=csPrepared
else
FCursorState:=csInactive;
O0Q0Q000Q0:=GetProtocol;
if not IsQuery or not FRequestResultSet then begin
O0Q0Q000Q0.OOO0OQQOQ0;
try
OCCOC000Q0.OQQ00OQOQ0;
finally
O0Q0Q000Q0.OQO0OQQOQ0;
end;
if not O00QQ000Q0 then
OQQOC000Q0;
end
else if OCO0Q000Q0<>nil then begin
O0Q0Q000Q0.OOO0OQQOQ0;
try
OCCOC000Q0.O0C0QQQOQ0;
finally
O0Q0Q000Q0.OQO0OQQOQ0;
end;
if not O00QQ000Q0 then
OQQOC000Q0;
end;
inherited;
end;
procedure TTDSCommand.OQ0OQ000Q0(OC0OQ000Q0:boolean;out O0OOQ000Q0,OOOOQ000Q0,OQOOQ000Q0,OCOOQ000Q0:string;out O0QOQ000Q0:integer);
var
OOQOQ000Q0:integer;
OQQOQ000Q0:TSQLObjectInfo;
begin
SQLInfo.SplitObjectName(OO0QQ000Q0,OQQOQ000Q0);
O0OOQ000Q0:=OQQOQ000Q0.Catalog;
OQOOQ000Q0:=OQQOQ000Q0.Schema;
OCOOQ000Q0:=OQQOQ000Q0.Name;
OQOOQ000Q0:=SQLInfo.NormalizeName(OQOOQ000Q0,False,True);
OCOOQ000Q0:=SQLInfo.NormalizeName(OCOOQ000Q0,False,True);
OOQOQ000Q0:=Pos(';',OCOOQ000Q0);
if OOQOQ000Q0>0 then begin
O0QOQ000Q0:=StrToInt(Copy(OCOOQ000Q0,OOQOQ000Q0+1,Length(OCOOQ000Q0)));
OCOOQ000Q0:=Copy(OCOOQ000Q0,1,OOQOQ000Q0-1);
end
else
O0QOQ000Q0:=1;
if O0OOQ000Q0<>'' then
OOOOQ000Q0:=SQLInfo.NormalizeName(O0OOQ000Q0,False,True)
else
if not OC0OQ000Q0 then
OOOOQ000Q0:=FConnection.Database
else
OOOOQ000Q0:='master';
end;
function TTDSCommand.OCQOQ000Q0(const O0COQ000Q0:string):boolean;
var
OOCOQ000Q0:TSQLParser;
OQCOQ000Q0:integer;
OCCOQ000Q0:boolean;
begin
Result:=False;
OOCOQ000Q0:=GetParserClass.Create(O0COQ000Q0);
try
OOCOQ000Q0.OmitBlank:=True;
OOCOQ000Q0.OmitComment:=True;
OOCOQ000Q0.QuotedString:=True;
OCCOQ000Q0:=True;
repeat
OQCOQ000Q0:=OOCOQ000Q0.GetNextToken;
case OQCOQ000Q0 of
lxCALL,lxEXEC,lxEXECUTE:begin
Result:=True;
Break;
end;
lxINSERT,lxUPDATE,lxSELECT,lxDELETE,lxCREATE,lxDECLARE:begin
Result:=False;
Break;
end;
lcIdent,lxQuestion:
if OCCOQ000Q0 then begin
OQCOQ000Q0:=OOCOQ000Q0.GetNextToken;
if OQCOQ000Q0 in[lxQuestion,lxEqual]then
Result:=True;
Break;
end;
end;
OCCOQ000Q0:=False;
until OQCOQ000Q0=lcEnd;
finally
OOCOQ000Q0.Free;
end;
end;
function TTDSCommand.O00QOCCCQ0(const OO0QOCCCQ0:string;out OQ0QOCCCQ0:boolean):string;
type
O0QQOCCCQ0=(OC0QOCCCQ0,O0OQOCCCQ0,OOOQOCCCQ0,OQOQOCCCQ0,OCOQOCCCQ0);
var
OOQQOCCCQ0:StringBuilder;
OQQQOCCCQ0:TSQLParser;
OCQQOCCCQ0:integer;
O0CQOCCCQ0,OOCQOCCCQ0:string;
OQCQOCCCQ0:O0QQOCCCQ0;
OCCQOCCCQ0:boolean;
begin
Result:='';
OQ0QOCCCQ0:=True;
OCCQOCCCQ0:=False;
OOQQOCCCQ0:=StringBuilder.Create(Length(OO0QOCCCQ0)+Length(OO0QOCCCQ0)div 2);
try
if(Length(OO0QOCCCQ0)>1)and(OO0QOCCCQ0[1]='{')and(OO0QOCCCQ0[Length(OO0QOCCCQ0)]='}')then begin
O0CQOCCCQ0:=Copy(OO0QOCCCQ0,2,Length(OO0QOCCCQ0)-2);
OQQQOCCCQ0:=GetParserClass.Create(O0CQOCCCQ0);
try
OQQQOCCCQ0.OmitBlank:=False;
OQQQOCCCQ0.OmitComment:=True;
OQQQOCCCQ0.QuotedString:=True;
OQCQOCCCQ0:=OC0QOCCCQ0;
while True do begin
OCQQOCCCQ0:=OQQQOCCCQ0.GetNext(OOCQOCCCQ0);
if OCQQOCCCQ0=lcEnd then
Break;
case OQCQOCCCQ0 of
OC0QOCCCQ0:
if OCQQOCCCQ0=lxCALL then
OQCQOCCCQ0:=O0OQOCCCQ0
else if OCQQOCCCQ0=lxEqual then begin
Assert(Length(OCCOC000Q0.OC00QQQOQ0)>0,'FCmdContext.ParamRefs are empty');
OCCOC000Q0.OC00QQQOQ0[0].OCQQQCQOQ0:=pdResult;
end;
O0OQOCCCQ0:
case OCQQOCCCQ0 of
lcIdent,lxLeftSqBracket,lxRightSqBracket,lxPoint:
Result:=Result+OOCQOCCCQ0;
lxSemicolon:begin
Result:=Result+OOCQOCCCQ0;
OQCQOCCCQ0:=OOOQOCCCQ0;
end;
lcBlank:
if Result='' then
Continue
else
OQCQOCCCQ0:=OCOQOCCCQ0;
else
if Result<>'' then begin
case OCQQOCCCQ0 of
lxQuestion,lxLeftBracket:
OQCQOCCCQ0:=OCOQOCCCQ0;
else
OQ0QOCCCQ0:=False;
Break;
end;
end
else
raise Exception.CreateFmt('Expected ProcName, "%s" instead',[OOCQOCCCQ0]);
end;
OOOQOCCCQ0:
if OCQQOCCCQ0=lcNumber then begin
Result:=Result+OOCQOCCCQ0;
OQCQOCCCQ0:=OCOQOCCCQ0;
end
else
raise Exception.CreateFmt('Expected OverloadNo, "%s" instead',[OOCQOCCCQ0]);
OCOQOCCCQ0:begin
case OCQQOCCCQ0 of
lxQuestion,lxComma,lxLeftBracket,lxRightBracket,lxSemicolon,lcBlank:
Continue;
lxAt:begin
OCQQOCCCQ0:=OQQQOCCCQ0.GetNext(OOCQOCCCQ0);
if(OCQQOCCCQ0=lcIdent)or(OCQQOCCCQ0=lcNumber)or(OCQQOCCCQ0>=lxSQLFirst)then
Continue
else begin
OQ0QOCCCQ0:=False;
Break;
end;
end;
else
OQ0QOCCCQ0:=False;
Break;
end;
end;
end;
end;
finally
OQQQOCCCQ0.Free;
end;
end
else begin
OQQQOCCCQ0:=GetParserClass.Create(OO0QOCCCQ0);
try
OQQQOCCCQ0.OmitBlank:=False;
OQQQOCCCQ0.OmitComment:=True;
OQQQOCCCQ0.QuotedString:=True;
OQCQOCCCQ0:=OC0QOCCCQ0;
while True do begin
OCQQOCCCQ0:=OQQQOCCCQ0.GetNext(OOCQOCCCQ0);
if OCQQOCCCQ0=lcEnd then
Break;
case OQCQOCCCQ0 of
OC0QOCCCQ0:
case OCQQOCCCQ0 of
lcBlank:
Continue;
lxEXEC,lxEXECUTE:
OQCQOCCCQ0:=O0OQOCCCQ0;
lxQuestion:
if Result='' then
OQCQOCCCQ0:=OQOQOCCCQ0
else
Break;
lcIdent,lxLeftSqBracket,lxRightSqBracket,lxPoint:
Result:=Result+OOCQOCCCQ0;
lxSemicolon:begin
Result:=Result+OOCQOCCCQ0;
OQCQOCCCQ0:=OOOQOCCCQ0;
end;
else
if Result<>'' then begin
case OCQQOCCCQ0 of
lxQuestion,lxLeftBracket:
OQCQOCCCQ0:=OCOQOCCCQ0;
else
OQ0QOCCCQ0:=False;
Break;
end;
end
else
raise Exception.CreateFmt('Expected ProcName, "%s" instead',[OOCQOCCCQ0]);
end;
O0OQOCCCQ0:
case OCQQOCCCQ0 of
lxQuestion,lxAt:begin
if Result='' then
OQCQOCCCQ0:=OQOQOCCCQ0
else
OQCQOCCCQ0:=OCOQOCCCQ0;
OCCQOCCCQ0:=OCQQOCCCQ0=lxAt;
end;
lcIdent,lxLeftSqBracket,lxRightSqBracket,lxPoint:
Result:=Result+OOCQOCCCQ0;
lxSemicolon:begin
Result:=Result+OOCQOCCCQ0;
OQCQOCCCQ0:=OOOQOCCCQ0;
end;
lcBlank:
if Result='' then
Continue
else
OQCQOCCCQ0:=OCOQOCCCQ0;
else
if Result<>'' then begin
OQ0QOCCCQ0:=False;
Break;
end
else
raise Exception.CreateFmt('Expected ProcName, "%s" instead',[OOCQOCCCQ0]);
end;
OOOQOCCCQ0:
if OCQQOCCCQ0=lcNumber then begin
Result:=Result+OOCQOCCCQ0;
OQCQOCCCQ0:=OCOQOCCCQ0;
end
else
raise Exception.CreateFmt('Expected OverloadNo, "%s" instead',[OOCQOCCCQ0]);
OQOQOCCCQ0:
if OCQQOCCCQ0=lxEqual then begin
OQCQOCCCQ0:=O0OQOCCCQ0;
Assert(Length(OCCOC000Q0.OC00QQQOQ0)>0,'FCmdContext.ParamRefs are empty');
OCCOC000Q0.OC00QQQOQ0[0].OCQQQCQOQ0:=pdResult;
end
else if OCCQOCCCQ0 then begin
OCCQOCCCQ0:=False;
Continue;
end;
OCOQOCCCQ0:begin
case OCQQOCCCQ0 of
lxQuestion,lxComma,lxLeftBracket,lxRightBracket,lxSemicolon,lcBlank:
Continue;
lxOUT,lxOUTPUT:
Continue;
lxAt:
OCCQOCCCQ0:=True;
else
if OCCQOCCCQ0 then begin
OCCQOCCCQ0:=False;
Continue;
end;
OQ0QOCCCQ0:=False;
Break;
end;
end;
end;
end;
finally
OQQQOCCCQ0.Free;
end;
end;
finally
OOQQOCCCQ0.Free;
end;
end;
function TTDSCommand.GetPrepared:boolean;
begin
Result:=O00QQ000Q0;
end;
procedure TTDSCommand.OOQ0Q000Q0;
begin
end;
function TTDSCommand.GetRowsAffected:NativeInt;
begin
Result:=OCCOC000Q0.OC0O0OQOQ0;
end;
procedure TTDSCommand.SetRowsAffected(OQ0COCCCQ0:NativeInt);
begin
OCCOC000Q0.OC0O0OQOQ0:=OQ0COCCCQ0;
end;
constructor TTDSRecordSet.Create;
begin
inherited;
OO00OCCCQ0:=True;
FFetchAll:=True;
end;
destructor TTDSRecordSet.Destroy;
begin
inherited;
end;
procedure TTDSRecordSet.AssignToNextResult(OOQQCCCCQ0:TSqlRecordSet);
begin
raise Exception.Create('Not implemented yet');
end;
procedure TTDSRecordSet.OOO0OCCCQ0;
begin
end;
procedure TTDSRecordSet.SetCommand(OCO0OCCCQ0:TCRCommand);
begin
inherited;
OOQCOCCCQ0:=TTDSCommand(OCO0OCCCQ0);
end;
function TTDSRecordSet.CanDisconnect:boolean;
begin
Result:=inherited CanDisconnect
and not OOQCOCCCQ0.OCCOC000Q0.OCOO0OQOQ0;
end;
function TTDSRecordSet.RowsReturn:Boolean;
begin
if OOQCOCCCQ0.CommandType<>ctUnknown then
Result:=inherited RowsReturn
else
Result:=OOQCOCCCQ0.IsQuery;
end;
function TTDSRecordSet.O0Q0OCCCQ0(OOQ0OCCCQ0:TFieldDesc;OQQ0OCCCQ0,OCQ0OCCCQ0:IntPtr):boolean;
begin
if OOQ0OCCCQ0.IsBlob then
Result:=GetBlob(OOQ0OCCCQ0,OCQ0OCCCQ0).CanRollback
else
Result:=CompareFields(OQQ0OCCCQ0,OCQ0OCCCQ0,OOQ0OCCCQ0,[coOrdinalCompare],False)<>0;
end;
function TTDSRecordSet.HasNextResult:boolean;
begin
Result:=OOQCOCCCQ0.OCCOC000Q0.OCOO0OQOQ0;
end;
function TTDSRecordSet.CheckNextResult:boolean;
begin
OOQCOCCCQ0.OCCOC000Q0.OCQ0QQQOQ0;
OOQCOCCCQ0.OCCOC000Q0.OOQ00OQOQ0;
Result:=OOQCOCCCQ0.IsQuery and(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0<>nil);
if Result then
OOQCOCCCQ0.SetCursorState(csExecuted);
end;
procedure TTDSRecordSet.Disconnect;
begin
OQQCOCCCQ0;
inherited;
if OOQCOCCCQ0.GetCursorState in[csFetching,csFetchingAll]then
OOQCOCCCQ0.SetCursorState(csFetched);
end;
procedure TTDSRecordSet.Reopen;
begin
OQQCOCCCQ0;
OQ00OCCCQ0:=False;
inherited;
end;
procedure TTDSRecordSet.ExecCommand(OCOQCCCCQ0:Integer=1;O0QQCCCCQ0:Integer=0);
begin
if not OO00OCCCQ0 then
Exit;
try
if not Prepared and not OOQCOCCCQ0.IsQuery then
OOO0OCCCQ0;
inherited;
if OOQCOCCCQ0.IsQuery then
OOQCOCCCQ0.CommandType:=ctCursor
else
OOQCOCCCQ0.CommandType:=ctStatement;
finally
if OOQCOCCCQ0.CommandType<>ctCursor then
OOQCOCCCQ0.SetCursorState(csInactive);
end;
end;
procedure TTDSRecordSet.ProcessNoResult(OQ0QCCCCQ0:Boolean);
begin
if not OOQCOCCCQ0.OCCOC000Q0.OQOO0OQOQ0 or OOQCOCCCQ0.OCCOC000Q0.OCOO0OQOQ0 then begin
OOQCOCCCQ0.FCanReadParams:=True;
if Assigned(OOQCOCCCQ0.ReadParams)then
OOQCOCCCQ0.ReadParams;
OOQCOCCCQ0.OCCOC000Q0.OCQ0QQQOQ0;
OOQCOCCCQ0.SetCursorState(csFetched);
end;
end;
procedure TTDSRecordSet.DoAfterFetch;
begin
inherited;
OOQCOCCCQ0.O0COC000Q0;
end;
function TTDSRecordSet.OOQO0CCCQ0:boolean;
begin
Result:=False;
end;
function TTDSRecordSet.FetchingAccessible(OCQO0CCCQ0:Boolean):Boolean;
var
O0CO0CCCQ0:TSqlFieldDesc;
OOCO0CCCQ0:integer;
begin
if not OQ00OCCCQ0 then begin
OQ00OCCCQ0:=True;
OC00OCCCQ0:=False;
AllocFetchBuffer;
BlockMan.DefaultItemCount:=FFetchRows;
FLastFetchBack:=False;
if IsServerCursor then
FNoCountData:=True;
if IsDynamicCursor then begin
FRecordCount:=-1;
FRowsFetched:=-1;
end
else
if IsStaticCursor then begin
FRecordCount:=OOQCOCCCQ0.OCCOC000Q0.OC0O0OQOQ0;
FFetchFromBookmark:=True;
end;
FBookmarkValue:=Integer(DBBMK_FIRST);
FBookmarkSize:=sizeof(FBookmarkValue);
for OOCO0CCCQ0:=0 to FFields.Count-1 do begin
O0CO0CCCQ0:=TSqlFieldDesc(FFields[OOCO0CCCQ0]);
if O0CO0CCCQ0.IsTimestamp and(O0CO0CCCQ0.TableInfo<>nil)then
TSqlTableInfo(O0CO0CCCQ0.TableInfo).MaxTimestamp:=0;
end;
end;
begin
SetLength(O0O0OCCCQ0,1);
SetLength(O0O0OCCCQ0[0],Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0));
end;
if OOQO0CCCQ0 then
Result:=False
else
Result:=OQCO0CCCQ0(FFetchRows,OCQO0CCCQ0)>0;
end;
function TTDSRecordSet.OQCO0CCCQ0(OCCO0CCCQ0:integer;O00QCCCCQ0:boolean):NativeUInt;
begin
try
if(OOQCOCCCQ0.GetProtocol<>nil)and OOQCOCCCQ0.OCCOC000Q0.OQOC0OQOQ0(O0O0OCCCQ0[0])then
Result:=OCCO0CCCQ0
else
Result:=0;
if Result=0 then
ProcessNoResult(O00QCCCCQ0);
except
on E:Exception do begin
ProcessFetchedException(E);
raise;
end;
end;
end;
procedure TTDSRecordSet.OOC00CCCQ0(OQC00CCCQ0:IntPtr;OCC00CCCQ0:integer);
var
O00O0CCCQ0:TSqlFieldDesc;
OO0O0CCCQ0:integer;
begin
for OO0O0CCCQ0:=0 to FFields.Count-1 do begin
O00O0CCCQ0:=TSqlFieldDesc(FFields[OO0O0CCCQ0]);
if(O00O0CCCQ0.FieldDescKind<>fdkData)or(O00O0CCCQ0.ActualFieldNo=-1)then
Continue;
O0CC0CCCQ0(O00O0CCCQ0,OQC00CCCQ0,OCC00CCCQ0);
end;
end;
procedure TTDSRecordSet.FetchBlock(OC0O0CCCQ0:PBlockHeader;O0OO0CCCQ0:boolean;out OOOO0CCCQ0:integer);
var
OQOO0CCCQ0:IntPtr;
OCOO0CCCQ0:integer;
O0QO0CCCQ0:integer;
begin
OOOO0CCCQ0:=0;
try
OQOO0CCCQ0:=PtrOffset(OC0O0CCCQ0,SizeOf(TBlockHeader)+SizeOf(TItemHeader));
OCOO0CCCQ0:=0;
for O0QO0CCCQ0:=0 to FFetchRows-1 do begin
if O0QO0CCCQ0>0 then
OOQCOCCCQ0.OCCOC000Q0.OQOC0OQOQ0(O0O0OCCCQ0[OCOO0CCCQ0]);
OOC00CCCQ0(OQOO0CCCQ0,OCOO0CCCQ0);
Inc(OOOO0CCCQ0);
if not OOQCOCCCQ0.OCCOC000Q0.OQOO0OQOQ0 or OOQCOCCCQ0.OCCOC000Q0.OCOO0OQOQ0 then begin
if not IsServerCursor then begin
begin
ProcessNoResult(O0OO0CCCQ0);
OCQCOCCCQ0(True);
end;
OOQCOCCCQ0.SetCursorState(csFetched);
end;
Exit;
end;
OQOO0CCCQ0:=PtrOffset(OQOO0CCCQ0,SizeOf(TItemHeader)+RecordSize);
end;
{$IFNDEF LITE}
if FSmartFetchState=sfDataByKey then begin
OCQCOCCCQ0(False);
OOQCOCCCQ0.SetCursorState(csFetched);
end;
{$ENDIF}
except
OCQCOCCCQ0(False);
raise;
end;
end;
procedure TTDSRecordSet.O0CC0CCCQ0(OOCC0CCCQ0:TSqlFieldDesc;OQCC0CCCQ0:IntPtr;OCCC0CCCQ0:integer);
var
O0000CCCQ0:OCO0QQCOQ0;
OO000CCCQ0:integer;
OQ000CCCQ0:Pointer;
OC000CCCQ0:IntPtr;
O0O00CCCQ0:PWord;
OOO00CCCQ0:integer;
OQO00CCCQ0:AnsiString;
OCO00CCCQ0:TBlob;
O0Q00CCCQ0:TVariantObject;
OOQ00CCCQ0:Int64;
OQQ00CCCQ0:double;
OCQ00CCCQ0:variant;
O0C00CCCQ0:OOCCCCQOQ0;
begin
OOO00CCCQ0:=OOCC0CCCQ0.ActualFieldNo;
if OOO00CCCQ0>=Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)then
raise Exception.Create(SFieldIndexError);
OO000CCCQ0:=Length(O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0].OOO0QQCOQ0);
if OO000CCCQ0>0 then
OQ000CCCQ0:=@O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0].OOO0QQCOQ0[0]
else
OQ000CCCQ0:=nil;
SetNull(OOCC0CCCQ0,OQCC0CCCQ0,O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0].OC00QQCOQ0);
O0000CCCQ0:=@OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0[OOO00CCCQ0];
OC000CCCQ0:=PtrOffset(OQCC0CCCQ0,OOCC0CCCQ0.DataOffset);
if OOCC0CCCQ0.HasValueLen then
O0O00CCCQ0:=PtrOffset(OQCC0CCCQ0,OOCC0CCCQ0.Offset)
else
O0O00CCCQ0:=nil;
if not O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0].OC00QQCOQ0 then begin
if OOCC0CCCQ0.IsTimestamp and(OOCC0CCCQ0.TableInfo<>nil)then begin
Assert((OQ000CCCQ0<>nil)and(OO000CCCQ0=8));
OOQ00CCCQ0:=Reverse8(PInt64(OQ000CCCQ0)^);
if {$IFDEF VER7P}UInt64{$ENDIF}(TSqlTableInfo(OOCC0CCCQ0.TableInfo).MaxTimestamp)<{$IFDEF VER7P}UInt64{$ENDIF}(OOQ00CCCQ0)then
TSqlTableInfo(OOCC0CCCQ0.TableInfo).MaxTimestamp:=OOQ00CCCQ0;
end;
case OOCC0CCCQ0.DataType of
dtInt8,dtUInt8:begin
{$IFDEF FPC}
Marshal.WriteByte(OC000CCCQ0,1,0);
{$ENDIF}
Marshal.WriteByte(OC000CCCQ0,PByte(OQ000CCCQ0)^);
end;
dtInt16,dtUInt16:
Marshal.WriteInt16(OC000CCCQ0,PSmallInt(OQ000CCCQ0)^);
dtInt32,dtUInt32:
Marshal.WriteInt32(OC000CCCQ0,PInteger(OQ000CCCQ0)^);
dtInt64,dtUInt64:
Marshal.WriteInt64(OC000CCCQ0,Int64(OQ000CCCQ0^));
dtSingle:
Marshal.WriteInt32(OC000CCCQ0,PInteger(OQ000CCCQ0)^);
dtFloat:begin
if(OOCC0CCCQ0.SubDataType and sdtNumeric)<>0 then begin
OQQ00CCCQ0:=OOQCOCCCQ0.OCCOC000Q0.OOOQOOQOQ0(O0000CCCQ0.O0C0QQCOQ0,O0000CCCQ0.OOC0QQCOQ0,OQ000CCCQ0,OO000CCCQ0);
Marshal.WriteInt64(OC000CCCQ0,PInt64(@OQQ00CCCQ0)^);
end
else begin
if OO000CCCQ0>=SizeOf(Double)then
Marshal.WriteInt64(OC000CCCQ0,Int64(OQ000CCCQ0^))
else
Marshal.WriteInt64(OC000CCCQ0,Int64(PInteger(OQ000CCCQ0)^));
end;
end;
dtCurrency:
Marshal.WriteDouble(OC000CCCQ0,OOQCOCCCQ0.OCCOC000Q0.OO0OQQQOQ0(OQ000CCCQ0,OO000CCCQ0));
dtDate,dtTime,dtDateTime:
Marshal.WriteDouble(OC000CCCQ0,Double(OOQCOCCCQ0.OCCOC000Q0.O0QOQQQOQ0(O0000CCCQ0.OC0OQQCOQ0,O0000CCCQ0.OOC0QQCOQ0,OQ000CCCQ0,OO000CCCQ0)));
dtSQLTimeStamp:
OOQCOCCCQ0.OCCOC000Q0.OCOCOOQOQ0(O0000CCCQ0.OC0OQQCOQ0,O0000CCCQ0.OOC0QQCOQ0,OO000CCCQ0,OQ000CCCQ0,OC000CCCQ0);
dtSQLTimeStampOffset:
OOQCOCCCQ0.OCCOC000Q0.OCCCOOQOQ0(O0000CCCQ0.OOC0QQCOQ0,OO000CCCQ0,OQ000CCCQ0,OC000CCCQ0);
dtBoolean:
Marshal.WriteInt16(OC000CCCQ0,smallint(WordBool(Boolean(PByte(OQ000CCCQ0)^))));
dtGuid:begin
OQO00CCCQ0:=AnsiString(ConvertGuidToString(PGUID(OQ000CCCQ0)^,UidWithBraces));
OO000CCCQ0:=LengthA(OQO00CCCQ0);
if OO000CCCQ0>0 then
CopyBuffer(PAChar(OQO00CCCQ0),OC000CCCQ0,OO000CCCQ0);
Marshal.WriteByte(OC000CCCQ0,OO000CCCQ0,0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
dtString:begin
O0COOCCCQ0(OOCC0CCCQ0,O0000CCCQ0,OQ000CCCQ0,OC000CCCQ0,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
dtExtString:begin
begin
O0CQ0CCCQ0(OOCC0CCCQ0,O0000CCCQ0,OQ000CCCQ0,OC000CCCQ0,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
end;
dtWideString:begin
O0OQ0CCCQ0(OOCC0CCCQ0,O0000CCCQ0,OQ000CCCQ0,OC000CCCQ0,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
dtExtWideString:begin
begin
O0OC0CCCQ0(OOCC0CCCQ0,O0000CCCQ0,OQ000CCCQ0,OC000CCCQ0,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
end;
dtBytes,dtVarBytes:begin
if OO000CCCQ0>OOCC0CCCQ0.Length then
OO000CCCQ0:=OOCC0CCCQ0.Length;
Move(OQ000CCCQ0^,OC000CCCQ0^,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
dtExtVarBytes:begin
if OO000CCCQ0>OOCC0CCCQ0.Length then
OO000CCCQ0:=OOCC0CCCQ0.Length;
begin
PIntPtr(OC000CCCQ0)^:=FStringHeap.NewBuf(OO000CCCQ0);
CopyBuffer(OQ000CCCQ0,PIntPtr(OC000CCCQ0)^,OO000CCCQ0);
O0O00CCCQ0^:=Word(OO000CCCQ0);
end;
end;
dtMemo,dtWideMemo,dtBlob,dtXML:begin
OCO00CCCQ0:=CreateBlob(OOCC0CCCQ0);
Marshal.WriteIntPtr(OC000CCCQ0,OCO00CCCQ0.GCHandle);
if OOCC0CCCQ0.DataType in[dtMemo,dtWideMemo]then
if IsWideField(OOCC0CCCQ0)then
O0C00CCCQ0:=OCQCCCQOQ0
else
O0C00CCCQ0:=OQQCCCQOQ0
else
O0C00CCCQ0:=O0CCCCQOQ0;
OCO00CCCQ0.RollbackEnabled:=False;
OOQCOCCCQ0.OCCOC000Q0.OOQ0OOQOQ0(O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0],OCO00CCCQ0,O0000CCCQ0.OQCOQQCOQ0,O0C00CCCQ0);
OCO00CCCQ0.RollbackEnabled:=True;
end;
dtVariant:begin
O0Q00CCCQ0:=TVariantObject.Create;
Marshal.WriteIntPtr(OC000CCCQ0,O0Q00CCCQ0.GCHandle);
OCQ00CCCQ0:=OOQCOCCCQ0.OCCOC000Q0.OCO0OOQOQ0(O0O0OCCCQ0[OCCC0CCCQ0][OOO00CCCQ0]);
if VarIsEmpty(OCQ00CCCQ0)or VarIsNull(OCQ00CCCQ0)then
SetNull(OOCC0CCCQ0,OQCC0CCCQ0,True)
else
O0Q00CCCQ0.Value:=OCQ00CCCQ0;
end;
dtBCD:
if OOCC0CCCQ0.SubDataType=sdtNumeric then
Marshal.WriteInt64(OC000CCCQ0,OOQCOCCCQ0.OCCOC000Q0.OQCOQQQOQ0(O0000CCCQ0.O0C0QQCOQ0,O0000CCCQ0.OOC0QQCOQ0,OQ000CCCQ0,OO000CCCQ0))
else
Marshal.WriteInt64(OC000CCCQ0,OOQCOCCCQ0.OCCOC000Q0.OQC0QQQOQ0(OQ000CCCQ0,OO000CCCQ0));
dtFMTBCD:
PBcd(OC000CCCQ0)^:=OOQCOCCCQ0.OCCOC000Q0.OCQQOOQOQ0(O0000CCCQ0.O0C0QQCOQ0,O0000CCCQ0.OOC0QQCOQ0,OQ000CCCQ0,OO000CCCQ0);
else
Assert(False);
end;
end
else begin
case OOCC0CCCQ0.DataType of
dtExtString,dtExtWideString,dtExtVarBytes:
Marshal.WriteIntPtr(OC000CCCQ0,nil);
dtMemo,dtWideMemo,dtBlob,dtXML:begin
OCO00CCCQ0:=CreateBlob(OOCC0CCCQ0);
Marshal.WriteIntPtr(OC000CCCQ0,OCO00CCCQ0.GCHandle);
end;
dtVariant:begin
O0Q00CCCQ0:=TVariantObject.Create;
Marshal.WriteIntPtr(OC000CCCQ0,O0Q00CCCQ0.GCHandle);
end;
end;
end;
end;
procedure TTDSRecordSet.InternalOpen(OO0OOCCCQ0:Boolean=False);
begin
try
inherited;
except
OQQCOCCCQ0;
raise;
end;
end;
procedure TTDSRecordSet.InternalClose;
begin
if(OOQCOCCCQ0<>nil)and OOQCOCCCQ0.FNonBlocking and not OOQCOCCCQ0.FRequestResultSet then
BreakFetch;
try
if(OOQCOCCCQ0<>nil)and OOQCOCCCQ0.FOpenNext then
OCQCOCCCQ0(True)
else
OQQCOCCCQ0;
finally
if OOQCOCCCQ0<>nil then begin
OOQCOCCCQ0.FCursorState:=csInactive;
if not Prepared then
OOQCOCCCQ0.CommandType:=ctUnknown;
OOQCOCCCQ0.Close;
end;
end;
OQ00OCCCQ0:=False;
inherited;
end;
procedure TTDSRecordSet.OQQCOCCCQ0;
begin
if(OOQCOCCCQ0=nil)or(OOQCOCCCQ0.GetProtocol=nil)then
Exit;
repeat
OCQCOCCCQ0(False);
until OOQCOCCCQ0.OCCOC000Q0.OQ0O0OQOQ0;
OOQCOCCCQ0.OCCOC000Q0.O0C0QQQOQ0;
end;
procedure TTDSRecordSet.OCQCOCCCQ0(O0CCOCCCQ0:boolean);
var
OOCCOCCCQ0:OQOCQCQOQ0;
OQCCOCCCQ0:boolean;
begin
if(OOQCOCCCQ0=nil)or(OOQCOCCCQ0.GetProtocol=nil)then
Exit;
OOCCOCCCQ0:=OOQCOCCCQ0.GetProtocol;
OOCCOCCCQ0.OOO0OQQOQ0;
try
OQCCOCCCQ0:=False;
if OOQCOCCCQ0.OCCOC000Q0.OQOO0OQOQ0 then begin
OQCCOCCCQ0:=not OOQCOCCCQ0.OCCOC000Q0.OQ0O0OQOQ0 and not O0CCOCCCQ0 and OOCCOCCCQ0.OQOOOQQOQ0;
if OQCCOCCCQ0 then
OOQCOCCCQ0.OCCOC000Q0.OOC0QQQOQ0;
SetLength(O0O0OCCCQ0,1);
SetLength(O0O0OCCCQ0[0],Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0));
while OOQCOCCCQ0.OCCOC000Q0.OQOC0OQOQ0(O0O0OCCCQ0[0],True)do;
end
else
if not O0CCOCCCQ0 and not OOQCOCCCQ0.OCCOC000Q0.OQ0O0OQOQ0 then
OOQCOCCCQ0.OCCOC000Q0.OQQ00OQOQ0;
finally
OOCCOCCCQ0.OQO0OQQOQ0;
end;
if not OOQCOCCCQ0.O00QQ000Q0 and OOCCOCCCQ0.OQOQ0QQOQ0 then
OOQCOCCCQ0.OQQOC000Q0(OQCCOCCCQ0);
end;
function TTDSRecordSet.OCCCOCCCQ0(O000OCCCQ0:Boolean):Boolean;
begin
if O000OCCCQ0 then
Result:=TrimFixedChar
else
Result:=TrimVarChar;
end;
procedure TTDSRecordSet.O0C0OCCCQ0;
var
OOC0OCCCQ0:TSqlFieldDesc;
OQC0OCCCQ0:Integer;
begin
FTablesInfo.BeginUpdate;
try
for OQC0OCCCQ0:=0 to High(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)do begin
OOC0OCCCQ0:=TSqlFieldDesc(CreateFieldDesc);
O0OQCCCCQ0(OOC0OCCCQ0,OQC0OCCCQ0);
if OOC0OCCCQ0<>nil then
FFields.Add(OOC0OCCCQ0);
end;
if FFields.Count=0 then begin
OQQCOCCCQ0;
DatabaseError(SNotRows,nil);
end;
finally
FTablesInfo.EndUpdate;
end;
FillTablesAliases;
end;
procedure TTDSRecordSet.CreateFieldDescs;
begin
if not OO00OCCCQ0 then
OOQCOCCCQ0.CommandType:=ctCursor;
if(OOQCOCCCQ0.CommandType=ctUnknown)or(OOQCOCCCQ0.FIsSProc and(OOQCOCCCQ0.FCursorState<csExecuted))then begin
try
OOQCOCCCQ0.OQCOC000Q0:=True;
ExecCommand;
finally
OOQCOCCCQ0.OQCOC000Q0:=False;
end;
O0C0OCCCQ0;
OQQCOCCCQ0;
if not Prepared then
OOQCOCCCQ0.SetCursorState(csInactive);
Exit;
end;
if OO00OCCCQ0 then
O0C0OCCCQ0;
end;
procedure TTDSRecordSet.InternalPrepare;
var
O0OOOCCCQ0:cardinal;
begin
try
OOO0OCCCQ0;
inherited;
if OO00OCCCQ0 then begin
O0OOOCCCQ0:=Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0);
if O0OOOCCCQ0>0 then
OOQCOCCCQ0.CommandType:=ctCursor
else
OOQCOCCCQ0.CommandType:=ctStatement;
end;
except
InternalUnPrepare;
raise;
end;
end;
procedure TTDSRecordSet.InternalUnPrepare;
begin
try
inherited;
finally
OOQCOCCCQ0.SetCursorState(csInactive);
OOQCOCCCQ0.CommandType:=ctUnknown;
end;
end;
function TTDSRecordSet.OQOOOCCCQ0(OCOOOCCCQ0:TSqlFieldDesc;O0QOOCCCQ0:Integer):Integer;
begin
Result:=O0QOOCCCQ0;
if IsWideField(OCOOOCCCQ0)then
Result:=Result shr 1;
end;
function TTDSRecordSet.OOQOOCCCQ0(OQQOOCCCQ0:TSqlFieldDesc;OCQOOCCCQ0:Integer):Integer;
begin
Result:=OCQOOCCCQ0;
if not IsWideField(OQQOOCCCQ0)then
Result:=Result*2;
end;
procedure TTDSRecordSet.O0COOCCCQ0(OOCOOCCCQ0:TSqlFieldDesc;OQCOOCCCQ0:OCO0QQCOQ0;
OCCOOCCCQ0,O00Q0CCCQ0:Pointer;var OO0Q0CCCQ0:Integer);
var
OQ0Q0CCCQ0:Pointer;
OC0Q0CCCQ0:Integer;
begin
OC0Q0CCCQ0:=OQOOOCCCQ0(OOCOOCCCQ0,OO0Q0CCCQ0);
OQ0Q0CCCQ0:=OOQQ0QQOQ0(OOQCOCCCQ0.OCCOC000Q0).OC0OOOQOQ0(OQCOOCCCQ0^,
OCCOOCCCQ0,OO0Q0CCCQ0,OC0Q0CCCQ0{$IFDEF IS_UTF8}*MaxSizeClientCharset {$ENDIF},
OCCCOCCCQ0(OOCOOCCCQ0.Fixed),OOCOOCCCQ0.Fixed,FStringHeap);
Move(OQ0Q0CCCQ0^,O00Q0CCCQ0^,OO0Q0CCCQ0+1);
FStringHeap.DisposeBuf(OQ0Q0CCCQ0);
end;
procedure TTDSRecordSet.O0OQ0CCCQ0(OOOQ0CCCQ0:TSqlFieldDesc;OQOQ0CCCQ0:OCO0QQCOQ0;
OCOQ0CCCQ0,O0QQ0CCCQ0:Pointer;var OOQQ0CCCQ0:Integer);
var
OQQQ0CCCQ0:Pointer;
OCQQ0CCCQ0:Integer;
begin
OCQQ0CCCQ0:=OOQOOCCCQ0(OOOQ0CCCQ0,OOQQ0CCCQ0);
OQQQ0CCCQ0:=OOQQ0QQOQ0(OOQCOCCCQ0.OCCOC000Q0).OQ0Q0OQOQ0(OQOQ0CCCQ0^,
OCOQ0CCCQ0,OOQQ0CCCQ0,OCQQ0CCCQ0,OCCCOCCCQ0(OOOQ0CCCQ0.Fixed),OOOQ0CCCQ0.Fixed,FStringHeap);
Move(OQQQ0CCCQ0^,O0QQ0CCCQ0^,(OOQQ0CCCQ0+1)*SizeOf(WideChar));
FStringHeap.DisposeBuf(OQQQ0CCCQ0);
end;
procedure TTDSRecordSet.O0CQ0CCCQ0(OOCQ0CCCQ0:TSqlFieldDesc;OQCQ0CCCQ0:OCO0QQCOQ0;
OCCQ0CCCQ0,O00C0CCCQ0:Pointer;var OO0C0CCCQ0:Integer);
var
OQ0C0CCCQ0:IntPtr;
OC0C0CCCQ0:Integer;
begin
OC0C0CCCQ0:=OQOOOCCCQ0(OOCQ0CCCQ0,OO0C0CCCQ0);
OQ0C0CCCQ0:=OOQQ0QQOQ0(OOQCOCCCQ0.OCCOC000Q0).OC0OOOQOQ0(OQCQ0CCCQ0^,
OCCQ0CCCQ0,OO0C0CCCQ0,OC0C0CCCQ0{$IFDEF IS_UTF8}*MaxSizeClientCharset {$ENDIF},
OCCCOCCCQ0(OOCQ0CCCQ0.Fixed),OOCQ0CCCQ0.Fixed,FStringHeap);
Marshal.WriteIntPtr(O00C0CCCQ0,OQ0C0CCCQ0);
end;
procedure TTDSRecordSet.O0OC0CCCQ0(OOOC0CCCQ0:TSqlFieldDesc;OQOC0CCCQ0:OCO0QQCOQ0;
OCOC0CCCQ0,O0QC0CCCQ0:Pointer;var OOQC0CCCQ0:Integer);
var
OQQC0CCCQ0:IntPtr;
OCQC0CCCQ0:Integer;
begin
OCQC0CCCQ0:=OOQOOCCCQ0(OOOC0CCCQ0,OOQC0CCCQ0);
OQQC0CCCQ0:=OOQQ0QQOQ0(OOQCOCCCQ0.OCCOC000Q0).OQ0Q0OQOQ0(OQOC0CCCQ0^,
OCOC0CCCQ0,OOQC0CCCQ0,OCQC0CCCQ0,OCCCOCCCQ0(OOOC0CCCQ0.Fixed),OOOC0CCCQ0.Fixed,FStringHeap);
Marshal.WriteIntPtr(O0QC0CCCQ0,OQQC0CCCQ0);
end;
constructor TTDSLoader.Create;
begin
inherited;
FSkipReadOnlyFieldDescs:=False;
OCCQCCCCQ0:=0;
end;
procedure TTDSLoader.OQOCCCCCQ0;
begin
end;
procedure TTDSLoader.OCOCCCCCQ0;
begin
end;
class function TTDSLoader.GetColumnClass:TCRLoaderColumnClass;
begin
Result:=TSqlLoaderColumn;
end;
procedure TTDSLoader.Prepare;
begin
Prepare(OQCQCCCCQ0);
end;
procedure TTDSLoader.Prepare(OCQ0CCCCQ0:integer);
const
O0C0CCCCQ0=1000;
var
OOC0CCCCQ0:Integer;
OQC0CCCCQ0:TSqlLoaderColumn;
OCC0CCCCQ0:TSqlFieldDesc;
O00OCCCCQ0:TParamDesc;
OO0OCCCCQ0,OQ0OCCCCQ0:string;
OC0OCCCCQ0,O0OOCCCCQ0,OOOOCCCCQ0:string;
OQOOCCCCQ0:TSQLObjectInfo;
begin
inherited Prepare;
if OQCQCCCCQ0=0 then
OQCQCCCCQ0:=O0C0CCCCQ0;
OCCQCCCCQ0:=OQCQCCCCQ0;
Assert(OQ0CCCCCQ0=nil);
OQ0CCCCCQ0:=TTDSRecordSet(FConnection.GetRecordSetClass.Create);
OQ0CCCCCQ0.SetConnection(FConnection);
OQ0CCCCCQ0.SetProp(prFetchAll,True);
OQ0CCCCCQ0.OOQCOCCCQ0.SQLInfo.SplitObjectName(TableName,OQOOCCCCQ0);
OC0OCCCCQ0:=OQOOCCCCQ0.Catalog;
O0OOCCCCQ0:=OQOOCCCCQ0.Schema;
OOOOCCCCQ0:=OQOOCCCCQ0.Name;
OOCQCCCCQ0:=OQ0CCCCCQ0.GenerateTableName(OC0OCCCCQ0,O0OOCCCCQ0,OOOOCCCCQ0,FConnection.Database);
OQ0CCCCCQ0.OOQCOCCCQ0.SQL:='select top 0 * from '+OOCQCCCCQ0;
OQ0CCCCCQ0.Open;
if OQ0CCCCCQ0.Fields.Count<Columns.Count then
raise Exception.Create('Table fields count < Columns.Count');
Assert(O00CCCCCQ0=nil);
O00CCCCCQ0:=TTDSCommand(FConnection.GetCommandClass.Create);
O00CCCCCQ0.SetConnection(FConnection);
O00CCCCCQ0.OCCOC000Q0.OCC00OQOQ0:=O00CCCCCQ0.GetProtocol;
O0CQCCCCQ0:=TCRObjectList.Create;
OO0OCCCCQ0:='';
OQ0OCCCCQ0:='';
SetLength(OCQQCCCCQ0,Columns.Count);
for OOC0CCCCQ0:=0 to Columns.Count-1 do begin
OQC0CCCCQ0:=TSqlLoaderColumn(Columns[OOC0CCCCQ0]);
OCC0CCCCQ0:=TSqlFieldDesc(OQ0CCCCCQ0.FieldByName(OQC0CCCCQ0.Name));
O00OCCCCQ0:=FConnection.GetCommandClass.GetParamDescClass.Create;
O00OCCCCQ0.SetName(Format('p%d',[OOC0CCCCQ0+1]));
O00OCCCCQ0.SetDataType(OQC0CCCCQ0.DataType);
O00OCCCCQ0.SetParamType(pdInput);
OQC0CCCCQ0.ActualFieldNo:=OCC0CCCCQ0.ActualFieldNo;
O00OCCCCQ0.SetSize(OCC0CCCCQ0.Size);
OQC0CCCCQ0.Skiped:=OC0CCCCCQ0(OCC0CCCCQ0,O00OCCCCQ0);
if OQC0CCCCQ0.Skiped then begin
FreeAndNil(O00OCCCCQ0);
Continue;
end;
O00CCCCCQ0.Params.Add(O00OCCCCQ0);
OQC0CCCCQ0.ParamDesc:=O00OCCCCQ0;
OQC0CCCCQ0.Init(OCCQCCCCQ0);
O00OCCCCQ0.SetArraySize(OCCQCCCCQ0);
O00OCCCCQ0.SetValueArr(OQC0CCCCQ0.DataArrPtr);
if OO0OCCCCQ0='' then
OO0OCCCCQ0:='INSERT INTO '+TableName+' ('
else
OO0OCCCCQ0:=OO0OCCCCQ0+', ';
OO0OCCCCQ0:=OO0OCCCCQ0+O00CCCCCQ0.SQLInfo.NormalizeName(OQC0CCCCQ0.Name,QuoteNames);
if OQ0OCCCCQ0<>'' then
OQ0OCCCCQ0:=OQ0OCCCCQ0+', ';
OQ0OCCCCQ0:=OQ0OCCCCQ0+OO00CCCCQ0(O00OCCCCQ0);
O00CCCCCQ0.AddParamPosition('',0,0,O00OCCCCQ0);
end;
O00CCCCCQ0.SetProp(prDisableParamScan,True);
O00CCCCCQ0.SQL:=OO0OCCCCQ0+') VALUES ('+OQ0OCCCCQ0+')';
OOO0CCCCQ0;
OQOCCCCCQ0;
Assert(OO0CCCCCQ0=nil);
OO0CCCCCQ0:=FConnection.GetTransactionClass.Create;
OO0CCCCCQ0.AddConnection(FConnection);
OO0CCCCCQ0.StartTransaction;
end;
procedure TTDSLoader.OOQCCCCCQ0;
var
OQQCCCCCQ0:Integer;
begin
if FLoadedRows=0 then
O00CCCCCQ0.O00OQ000Q0;
if not O00CCCCCQ0.GetProtocol.OQOQ0QQOQ0 then
raise Exception.Create(SConnectionIsBusy);
if(O00CCCCCQ0.Params.Count>0)and(O00CCCCCQ0.Params[0].GetArraySize>1)then begin
if FLastRow+1-FLoadedRows<>OCCQCCCCQ0 then
OQQCCCCCQ0:=FLastRow+1-FLoadedRows
else
OQQCCCCCQ0:=O00CCCCCQ0.Params[0].GetArraySize;
O00CCCCCQ0.ExecuteBatch(OQQCCCCCQ0,0);
end
else
O00CCCCCQ0.Execute;
FLoadedRows:=FLastRow+1;
end;
procedure TTDSLoader.OCQCCCCCQ0(const O0CCCCCCQ0:Integer);
var
OOCCCCCCQ0:Integer;
OQCCCCCCQ0:TSqlLoaderColumn;
OCCCCCCCQ0:TParamDesc;
O000CCCCQ0:MemData.TBlob;
begin
for OOCCCCCCQ0:=0 to Length(OCQQCCCCQ0)-1 do begin
OQCCCCCCQ0:=TSqlLoaderColumn(Columns[OOCCCCCCQ0]);
OCCCCCCCQ0:=OQCCCCCCQ0.ParamDesc;
try
if OQCCCCCCQ0.Skiped then
Continue;
if(OQCCCCCCQ0.DataType=dtBlob)and VarIsStr(OCQQCCCCQ0[OOCCCCCCQ0])then begin
{$IFDEF HAVE_COMPRESS}
O000CCCCQ0:=TCompressedBlob.Create;
{$ELSE}
O000CCCCQ0:=MemData.TBlob.Create;
{$ENDIF}
O0CQCCCCQ0.Add(O000CCCCQ0);
O000CCCCQ0.AsString:=OCQQCCCCQ0[OOCCCCCCQ0];
OCCCCCCCQ0.ItemObject[O0CCCCCCQ0]:=O000CCCCQ0;
OCCCCCCCQ0.ItemNull[O0CCCCCCQ0]:=IntPtr(O000CCCCQ0.FirstPiece)=nil;
end
else begin
OCCCCCCCQ0.ItemValue[O0CCCCCCQ0]:=Unassigned;
OCCCCCCCQ0.ItemValue[O0CCCCCCQ0]:=OCQQCCCCQ0[OOCCCCCCQ0];
end;
finally
DoReleaseObjectRef(OCQQCCCCQ0[OOCCCCCCQ0]);
if FUseBlankValues then
OCQQCCCCQ0[OOCCCCCCQ0]:=Null;
end;
end;
end;
function TTDSLoader.OO00CCCCQ0(const OQ00CCCCQ0:TParamDesc):string;
begin
Result:='@'+OQ00CCCCQ0.GetName;
end;
function TTDSLoader.OC00CCCCQ0(const O0O0CCCCQ0:TParamDesc):Boolean;
begin
Result:=False;
end;
procedure TTDSLoader.OOO0CCCCQ0;
var
OQO0CCCCQ0:TSQLParser;
OCO0CCCCQ0,O0Q0CCCCQ0,OOQ0CCCCQ0:integer;
OQQ0CCCCQ0:string;
begin
OQO0CCCCQ0:=TSQLParser.Create(O00CCCCCQ0.SQL);
try
OQO0CCCCQ0.OmitBlank:=True;
OQO0CCCCQ0.OmitComment:=True;
OQO0CCCCQ0.QuotedString:=True;
OQO0CCCCQ0.ToLexem(lxVALUES);
OOQ0CCCCQ0:=0;
repeat
OCO0CCCCQ0:=OQO0CCCCQ0.GetNext(OQQ0CCCCQ0);
case OCO0CCCCQ0 of
lxQuestion:begin
O00CCCCCQ0.ParamsInfo[OOQ0CCCCQ0].StartPosition:=OQO0CCCCQ0.CurrPos;
O00CCCCCQ0.ParamsInfo[OOQ0CCCCQ0].EndPosition:=OQO0CCCCQ0.CurrPos+1;
Inc(OOQ0CCCCQ0);
end;
lxAt:begin
O0Q0CCCCQ0:=OQO0CCCCQ0.CurrPos;
OQO0CCCCQ0.ToLexem([lxComma,lxRightBracket]);
O00CCCCCQ0.ParamsInfo[OOQ0CCCCQ0].StartPosition:=O0Q0CCCCQ0;
O00CCCCCQ0.ParamsInfo[OOQ0CCCCQ0].EndPosition:=OQO0CCCCQ0.CurrPos;
Inc(OOQ0CCCCQ0);
end;
end;
until OCO0CCCCQ0=lcEnd;
finally
OQO0CCCCQ0.Free;
end;
end;
procedure TTDSLoader.DoLoad;
begin
if FLastRow>=FLoadedRows then begin
OCQCCCCCQ0(FLastRow-FLoadedRows);
OOQCCCCCQ0;
end;
end;
function TTDSLoader.IsPutColumnDataAllowed:boolean;
begin
Result:=O00CCCCCQ0<>nil;
end;
procedure TTDSLoader.PutColumnData(OCOOCCCCQ0:integer;O0QOCCCCQ0:integer;const OOQOCCCCQ0:variant);
begin
if(FLastRow<>-1)and(O0QOCCCCQ0=FLastRow+2)then begin
OCQCCCCCQ0(O0QOCCCCQ0-FLoadedRows-2);
if(FLastRow+1-FLoadedRows>=OCCQCCCCQ0)then
OOQCCCCCQ0;
end;
inherited;
OCQQCCCCQ0[OCOOCCCCQ0]:=OOQOCCCCQ0;
DoAddObjectRef(OCQQCCCCQ0[OCOOCCCCQ0]);
end;
procedure TTDSLoader.Finish;
begin
inherited;
OO0CCCCCQ0.Commit;
FreeAndNil(OO0CCCCCQ0);
OCOCCCCCQ0;
FreeAndNil(OQ0CCCCCQ0);
FreeAndNil(O00CCCCCQ0);
O0CQCCCCQ0.Free;
Reset;
end;
initialization
OQQOCCCCQ0:=Unassigned;
end.
