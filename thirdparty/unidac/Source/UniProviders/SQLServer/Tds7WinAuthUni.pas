{$I Sdac.inc}
unit Tds7WinAuthUni;
interface
{$IFDEF MSWINDOWS}
uses
Windows,SysUtils,DateUtils,
CRTypes,CRHash,CRCipher,CRFunctions,CRSspi,
{$IFNDEF UNIDACPRO}
TdsNet,Tds7Net;
{$ELSE}
TdsNetUni,Tds7NetUni;
{$ENDIF}
const
OQ0QQ0CCQ0=$00000010;
OC0QQ0CCQ0=$00000000;
O0OQQ0CCQ0=$00000001;
OOOQQ0CCQ0=$00000002;
OQOQQ0CCQ0=$00000003;
OCOQQ0CCQ0=$00000004;
O0QQQ0CCQ0=$F0000000;
OOQQQ0CCQ0=Integer($00090312);
OQQQQ0CCQ0=Integer($00090313);
OCQQQ0CCQ0=Integer($00090314);
O0CQQ0CCQ0=Integer($80090318);
OOCQQ0CCQ0=Integer($00090320);
type
OQCQQ0CCQ0=^OCCQQ0CCQ0;
OCCQQ0CCQ0=record
O00CQ0CCQ0:Cardinal;
OO0CQ0CCQ0:Cardinal;
OQ0CQ0CCQ0:Cardinal;
OC0CQ0CCQ0:Cardinal;
O0OCQ0CCQ0:Cardinal;
end;
OOOCQ0CCQ0=function(OQOCQ0CCQ0:PSecHandle;OCOCQ0CCQ0:Cardinal;O0QCQ0CCQ0:Pointer):Integer;stdcall;
OOQCQ0CCQ0=function(OQQCQ0CCQ0:PSecHandle;OCQCQ0CCQ0:Cardinal;O0CCQ0CCQ0:PSecBufferDesc;OOCCQ0CCQ0:Cardinal):Integer;stdcall;
OQCCQ0CCQ0=function(OCCCQ0CCQ0:PSecHandle;O000Q0CCQ0:PSecBufferDesc;OO00Q0CCQ0:Cardinal;var OQ00Q0CCQ0:Cardinal):Integer;stdcall;
OC00Q0CCQ0=function(O0O0Q0CCQ0:PSecHandle;OOO0Q0CCQ0:PSecBufferDesc):Integer;stdcall;
OQO0Q0CCQ0=function(const OCO0Q0CCQ0:TValueArr;O0Q0Q0CCQ0,OOQ0Q0CCQ0:integer):integer of object;
OQQ0Q0CCQ0=class(TCRNTLMAuth)
private
OCQ0Q0CCQ0:OCCQQ0CCQ0;
O0C0Q0CCQ0:OQQCC0COQ0;
OOC0Q0CCQ0:TSecurityProviders;
OQC0Q0CCQ0:TSecHandle;
OCC0Q0CCQ0:TSecHandle;
O00OQ0CCQ0:OQO0Q0CCQ0;
OO0OQ0CCQ0:OOOCQ0CCQ0;
OQ0OQ0CCQ0:OOQCQ0CCQ0;
OC0OQ0CCQ0:OQCCQ0CCQ0;
O0OOQ0CCQ0:OC00Q0CCQ0;
protected
function RegisterSecureDllMethods:Integer;override;
private
property OQOOQ0CCQ0:OOOCQ0CCQ0 read OO0OQ0CCQ0 write OO0OQ0CCQ0;
property OCOOQ0CCQ0:OOQCQ0CCQ0 read OQ0OQ0CCQ0 write OQ0OQ0CCQ0;
property O0QOQ0CCQ0:OQCCQ0CCQ0 read OC0OQ0CCQ0 write OC0OQ0CCQ0;
property OOQOQ0CCQ0:OC00Q0CCQ0 read O0OOQ0CCQ0 write O0OOQ0CCQ0;
public
property OQQOQ0CCQ0:OQO0Q0CCQ0 read O00OQ0CCQ0 write O00OQ0CCQ0;
constructor Create(O0COQ0CCQ0:TSecurityProviders);
destructor Destroy;override;
function Initialize:Integer;override;
function O0OQOCQCQ0(out OOOQOCQCQ0:TBytes):Integer;
function OCQQOCQCQ0(out O0CQOCQCQ0:TBytes):Integer;
function OC0COCQCQ0(const O0OCOCQCQ0:Pointer;const OOOCOCQCQ0:NativeInt;out OQOCOCQCQ0:TBytes):Integer;
function OOCCOCQCQ0(const OQCCOCQCQ0:TBytes;out OCCCOCQCQ0:TBytes):Integer;
function OQO0OCQCQ0(OCO0OCQCQ0:Pointer;O0Q0OCQCQ0,OOQ0OCQCQ0:Integer):TBytes;
procedure OCC0OCQCQ0(O00OOCQCQ0:Pointer;OO0OOCQCQ0,OQ0OOCQCQ0:Integer);
function O0QOOCQCQ0(OOQOOCQCQ0:Pointer;OQQOOCQCQ0:NativeInt):NativeInt;
end;
{$ENDIF}
implementation
{$IFDEF MSWINDOWS}
const
OCCOOCQCQ0:string='Microsoft Unified Security Protocol Provider';
O00Q0CQCQ0=$00000001;
OO0Q0CQCQ0=$00000002;
OQ0Q0CQCQ0=$00000004;
OC0Q0CQCQ0=$00000008;
O0OQ0CQCQ0=$00000010;
OOOQ0CQCQ0=$00000020;
OQOQ0CQCQ0=$00000040;
OCOQ0CQCQ0=$00000080;
O0QQ0CQCQ0=$00000100;
OOQQ0CQCQ0=$00000200;
OQQQ0CQCQ0=$00000400;
OCQQ0CQCQ0=$00000800;
O0CQ0CQCQ0=$00001000;
OOCQ0CQCQ0=$00002000;
OQCQ0CQCQ0=$00004000;
OCCQ0CQCQ0=$00008000;
O00C0CQCQ0=$00010000;
OO0C0CQCQ0=$00020000;
OQ0C0CQCQ0=$00040000;
OC0C0CQCQ0=$00080000;
O0OC0CQCQ0=$00100000;
OOOC0CQCQ0=$00200000;
OQOC0CQCQ0=OC0C0CQCQ0 or
OCCQ0CQCQ0 or
OQCQ0CQCQ0 or
OOOQ0CQCQ0 or
O0OQ0CQCQ0 or
OC0Q0CQCQ0 or
OQ0Q0CQCQ0;
OCOC0CQCQ0=O00Q0CQCQ0 or
OO0Q0CQCQ0 or
OQCQ0CQCQ0 or
O00C0CQCQ0;
constructor OQQ0Q0CCQ0.Create(O0COQ0CCQ0:TSecurityProviders);
begin
inherited Create;
O0C0Q0CCQ0:=OQQCC0COQ0.Create;
OOC0Q0CCQ0:=O0COQ0CCQ0;
UsedPackage:=spNegotiate;
end;
destructor OQQ0Q0CCQ0.Destroy;
begin
if Assigned(DeleteSecurityContext)then
DeleteSecurityContext(@OCC0Q0CCQ0);
if Assigned(FreeCredentialsHandle)then begin
FreeCredentialsHandle(@OQC0Q0CCQ0);
end;
O0C0Q0CCQ0.Free;
inherited;
end;
function OQQ0Q0CCQ0.RegisterSecureDllMethods:Integer;
begin
Result:=inherited RegisterSecureDllMethods;
if Result<>0 then
Exit;
OO0OQ0CCQ0:=GetProcAddress(hSecure32Lib,{$IFDEF UNICODE}'QueryContextAttributesW'{$ELSE}'QueryContextAttributesA'{$ENDIF});
if not Assigned(OQOOQ0CCQ0)then begin
Result:=GetLastError;
Exit;
end;
OQ0OQ0CCQ0:=GetProcAddress(hSecure32Lib,'EncryptMessage');
if not Assigned(OCOOQ0CCQ0)then begin
Result:=GetLastError;
Exit;
end;
OC0OQ0CCQ0:=GetProcAddress(hSecure32Lib,'DecryptMessage');
if not Assigned(OC0OQ0CCQ0)then begin
Result:=GetLastError;
Exit;
end;
O0OOQ0CCQ0:=GetProcAddress(hSecure32Lib,'CompleteAuthToken');
if not Assigned(OOQOQ0CCQ0)then begin
Result:=GetLastError;
Exit;
end;
end;
function OQQ0Q0CCQ0.Initialize:Integer;
var
OCCOQ0CCQ0:PSecPkgInfo;
O00QOCQCQ0:PChar;
OO0QOCQCQ0:TTimeStamp;
OQ0QOCQCQ0:TSecurityProvider;
OC0QOCQCQ0:string;
begin
Result:=RegisterSecureDllMethods;
if Result<>0 then
Exit;
UsedPackage:=spNTLM;
OOC0Q0CCQ0:=[spNTLM];
MaxMessageLen:=0;
if OOC0Q0CCQ0=[]then
OOC0Q0CCQ0:=[spNegotiate];
for OQ0QOCQCQ0:=Low(TSecurityProvider)to High(TSecurityProvider)do begin
if not(OQ0QOCQCQ0 in OOC0Q0CCQ0)then
Continue;
O00QOCQCQ0:=PChar(SecurityProtocolName(OQ0QOCQCQ0));
Result:=QuerySecurityPackageInfo(O00QOCQCQ0,OCCOQ0CCQ0);
if Result<>0 then begin
Result:=GetLastError;
Exit;
end;
if OCCOQ0CCQ0.cbMaxToken>MaxMessageLen then
MaxMessageLen:=OCCOQ0CCQ0.cbMaxToken;
FreeContextBuffer(OCCOQ0CCQ0);
end;
OC0QOCQCQ0:=UsedProtocolName;
Result:=AcquireCredentialsHandle(nil,PChar(OC0QOCQCQ0),OQOQQ0CCQ0,
nil,nil,nil,nil,@CredNegotiate,OO0QOCQCQ0);
end;
function OQQ0Q0CCQ0.O0OQOCQCQ0(out OOOQOCQCQ0:TBytes):Integer;
var
OQOQOCQCQ0:PSecPkgInfo;
OCOQOCQCQ0:TTimeStamp;
O0QQOCQCQ0:Cardinal;
OOQQOCQCQ0:TSecBufferDesc;
OQQQOCQCQ0:TSecBuffer;
begin
Result:=QuerySecurityPackageInfo(PChar(OCCOOCQCQ0),OQOQOCQCQ0);
if Result<>0 then
Exit;
Result:=AcquireCredentialsHandle(nil,PChar(OCCOOCQCQ0),OOOQQ0CCQ0,
nil,nil,nil,nil,@OQC0Q0CCQ0,OCOQOCQCQ0);
if Result<>0 then
Exit;
SetLength(OOOQOCQCQ0,MaxMessageLen);
OQQQOCQCQ0.BufferType:=2;
OQQQOCQCQ0.pvBuffer:=@OOOQOCQCQ0[0];
OQQQOCQCQ0.cbBuffer:=Length(OOOQOCQCQ0);
OOQQOCQCQ0.ulVersion:=0;
OOQQOCQCQ0.cBuffers:=1;
OOQQOCQCQ0.pBuffers:=@OQQQOCQCQ0;
Result:=InitializeSecurityContext(@OQC0Q0CCQ0,nil,nil,
OQOC0CQCQ0,0,OQ0QQ0CCQ0,
nil,0,@OCC0Q0CCQ0,@OOQQOCQCQ0,O0QQOCQCQ0,@OCOQOCQCQ0);
if Result<>OOQQQ0CCQ0 then
Exit;
SetLength(OOOQOCQCQ0,OQQQOCQCQ0.cbBuffer);
end;
function OQQ0Q0CCQ0.OCQQOCQCQ0(out O0CQOCQCQ0:TBytes):Integer;
var
OOCQOCQCQ0:TSecBufferArray;
OQCQOCQCQ0:TSecBuffer;
OCCQOCQCQ0:TSecBufferDesc;
O00COCQCQ0:TSecBufferDesc;
OO0COCQCQ0:Cardinal;
OQ0COCQCQ0:TTimeStamp;
begin
SetLength(OOCQOCQCQ0,2);
OOCQOCQCQ0[0].BufferType:=2;
OOCQOCQCQ0[0].pvBuffer:=nil;
OOCQOCQCQ0[0].cbBuffer:=0;
OOCQOCQCQ0[1].BufferType:=0;
OOCQOCQCQ0[1].pvBuffer:=nil;
OOCQOCQCQ0[1].cbBuffer:=0;
OCCQOCQCQ0.ulVersion:=0;
OCCQOCQCQ0.cBuffers:=2;
OCCQOCQCQ0.pBuffers:=@OOCQOCQCQ0[0];
SetLength(O0CQOCQCQ0,MaxMessageLen);
OQCQOCQCQ0.BufferType:=2;
OQCQOCQCQ0.pvBuffer:=@O0CQOCQCQ0[0];
OQCQOCQCQ0.cbBuffer:=MaxMessageLen;
O00COCQCQ0.ulVersion:=0;
O00COCQCQ0.cBuffers:=1;
O00COCQCQ0.pBuffers:=@OQCQOCQCQ0;
Result:=InitializeSecurityContext(@CredNegotiate,nil,nil,OCOC0CQCQ0,
0,OQ0QQ0CCQ0,@OCCQOCQCQ0,0,@SslCtxHandle,@O00COCQCQ0,OO0COCQCQ0,@OQ0COCQCQ0);
if Result<=0 then
Exit;
if(Result=OQQQQ0CCQ0)or(Result=OCQQQ0CCQ0)then begin
Result:=OOQOQ0CCQ0(@SslCtxHandle,@O00COCQCQ0);
if Result<>0 then begin
Exit
end;
end
else if(Result<>0)and(Result<>OOQQQ0CCQ0)then begin
FreeCredentialsHandle(@CredNegotiate);
Exit;
end;
SetLength(O0CQOCQCQ0,OQCQOCQCQ0.cbBuffer);
end;
function OQQ0Q0CCQ0.OC0COCQCQ0(const O0OCOCQCQ0:Pointer;const OOOCOCQCQ0:NativeInt;out OQOCOCQCQ0:TBytes):Integer;
var
OCOCOCQCQ0:Cardinal;
O0QCOCQCQ0:TTimeStamp;
OOQCOCQCQ0:TSecBufferArray;
OQQCOCQCQ0:TSecBufferDesc;
OCQCOCQCQ0:TSecBuffer;
O0CCOCQCQ0:TSecBufferDesc;
begin
SetLength(OOQCOCQCQ0,2);
OOQCOCQCQ0[0].BufferType:=2;
OOQCOCQCQ0[0].pvBuffer:=O0OCOCQCQ0;
OOQCOCQCQ0[0].cbBuffer:=Cardinal(OOOCOCQCQ0);
OOQCOCQCQ0[1].BufferType:=0;
OOQCOCQCQ0[1].pvBuffer:=nil;
OOQCOCQCQ0[1].cbBuffer:=0;
OQQCOCQCQ0.ulVersion:=0;
OQQCOCQCQ0.cBuffers:=2;
OQQCOCQCQ0.pBuffers:=@OOQCOCQCQ0[0];
SetLength(OQOCOCQCQ0,MaxMessageLen);
OCQCOCQCQ0.BufferType:=2;
OCQCOCQCQ0.pvBuffer:=@OQOCOCQCQ0[0];
OCQCOCQCQ0.cbBuffer:=Length(OQOCOCQCQ0);
O0CCOCQCQ0.ulVersion:=0;
O0CCOCQCQ0.cBuffers:=1;
O0CCOCQCQ0.pBuffers:=@OCQCOCQCQ0;
Result:=InitializeSecurityContext(@OQC0Q0CCQ0,@OCC0Q0CCQ0,nil,
OQOC0CQCQ0,0,OQ0QQ0CCQ0,
@OQQCOCQCQ0,0,nil,@O0CCOCQCQ0,OCOCOCQCQ0,@O0QCOCQCQ0);
if Result=0 then begin
Result:=OQOOQ0CCQ0(@OCC0Q0CCQ0,4,@OCQ0Q0CCQ0);
if Result<>0 then
raise Exception.Create(SysErrorMessage(Result));
end
else
if Result=OOQQQ0CCQ0 then begin
SetLength(OQOCOCQCQ0,OCQCOCQCQ0.cbBuffer);
end
else begin
OQOCOCQCQ0:=nil;
raise Exception.Create(SysErrorMessage(Result));
end;
end;
function OQQ0Q0CCQ0.OOCCOCQCQ0(const OQCCOCQCQ0:TBytes;out OCCCOCQCQ0:TBytes):Integer;
var
O000OCQCQ0:TSecBufferArray;
OO00OCQCQ0:TSecBuffer;
OQ00OCQCQ0:TSecBufferDesc;
OC00OCQCQ0:TSecBufferDesc;
O0O0OCQCQ0:Cardinal;
OOO0OCQCQ0:TTimeStamp;
begin
SetLength(O000OCQCQ0,2);
O000OCQCQ0[0].BufferType:=2;
O000OCQCQ0[0].pvBuffer:=@OQCCOCQCQ0[0];
O000OCQCQ0[0].cbBuffer:=Length(OQCCOCQCQ0);
O000OCQCQ0[1].BufferType:=0;
O000OCQCQ0[1].pvBuffer:=nil;
O000OCQCQ0[1].cbBuffer:=0;
OQ00OCQCQ0.ulVersion:=0;
OQ00OCQCQ0.cBuffers:=2;
OQ00OCQCQ0.pBuffers:=@O000OCQCQ0[0];
SetLength(OCCCOCQCQ0,MaxMessageLen);
OO00OCQCQ0.BufferType:=2;
OO00OCQCQ0.pvBuffer:=@OCCCOCQCQ0[0];
OO00OCQCQ0.cbBuffer:=Length(OCCCOCQCQ0);
OC00OCQCQ0.ulVersion:=0;
OC00OCQCQ0.cBuffers:=1;
OC00OCQCQ0.pBuffers:=@OO00OCQCQ0;
Result:=InitializeSecurityContext(@CredNegotiate,@SslCtxHandle,nil,
OCOC0CQCQ0,0,OQ0QQ0CCQ0,
@OQ00OCQCQ0,0,nil,@OC00OCQCQ0,O0O0OCQCQ0,@OOO0OCQCQ0);
if Result<>0 then
Exit;
SetLength(OCCCOCQCQ0,OO00OCQCQ0.cbBuffer);
end;
function OQQ0Q0CCQ0.OQO0OCQCQ0(OCO0OCQCQ0:Pointer;O0Q0OCQCQ0,OOQ0OCQCQ0:Integer):TBytes;
var
OQQ0OCQCQ0:TSecBufferDesc;
OCQ0OCQCQ0:array[0..3]of TSecBuffer;
O0C0OCQCQ0:Cardinal;
OOC0OCQCQ0:LongInt;
OQC0OCQCQ0:THandle;
begin
O0C0OCQCQ0:=OCQ0Q0CCQ0.O00CQ0CCQ0+OCQ0Q0CCQ0.OQ0CQ0CCQ0+OCQ0Q0CCQ0.OO0CQ0CCQ0;
OQC0OCQCQ0:=LocalAlloc(LPTR,O0C0OCQCQ0);
if OQC0OCQCQ0=0 then
raise Exception.Create('Out of memory');
try
Move(PtrOffset(OCO0OCQCQ0,O0Q0OCQCQ0)^,PtrOffset(Pointer(OQC0OCQCQ0),OCQ0Q0CCQ0.O00CQ0CCQ0)^,OOQ0OCQCQ0);
OCQ0OCQCQ0[0].pvBuffer:=Pointer(OQC0OCQCQ0);
OCQ0OCQCQ0[0].cbBuffer:=OCQ0Q0CCQ0.O00CQ0CCQ0;
OCQ0OCQCQ0[0].BufferType:=7;
OCQ0OCQCQ0[1].pvBuffer:=PtrOffset(Pointer(OQC0OCQCQ0),OCQ0Q0CCQ0.O00CQ0CCQ0);
OCQ0OCQCQ0[1].cbBuffer:=OOQ0OCQCQ0;
OCQ0OCQCQ0[1].BufferType:=1;
OCQ0OCQCQ0[2].pvBuffer:=PtrOffset(Pointer(OQC0OCQCQ0),Integer(OCQ0Q0CCQ0.O00CQ0CCQ0)+OOQ0OCQCQ0);
OCQ0OCQCQ0[2].cbBuffer:=OCQ0Q0CCQ0.OO0CQ0CCQ0;
OCQ0OCQCQ0[2].BufferType:=6;
OCQ0OCQCQ0[3].BufferType:=0;
OQQ0OCQCQ0.ulVersion:=0;
OQQ0OCQCQ0.cBuffers:=4;
OQQ0OCQCQ0.pBuffers:=@OCQ0OCQCQ0[0];
OOC0OCQCQ0:=OCOOQ0CCQ0(@OCC0Q0CCQ0,0,@OQQ0OCQCQ0,0);
if OOC0OCQCQ0<>0 then
raise Exception.CreateFmt('Encrypt error %X',[OOC0OCQCQ0]);
O0C0OCQCQ0:=OCQ0OCQCQ0[0].cbBuffer+OCQ0OCQCQ0[1].cbBuffer+OCQ0OCQCQ0[2].cbBuffer;
Assert(O0C0OCQCQ0>0);
SetLength(Result,O0C0OCQCQ0);
Move(Pointer(OQC0OCQCQ0)^,Result[0],O0C0OCQCQ0);
finally
LocalFree(OQC0OCQCQ0);
end;
end;
procedure OQQ0Q0CCQ0.OCC0OCQCQ0(O00OOCQCQ0:Pointer;OO0OOCQCQ0,OQ0OOCQCQ0:Integer);
var
OC0OOCQCQ0:TSecBufferDesc;
O0OOOCQCQ0:array[0..3]of TSecBuffer;
OOOOOCQCQ0:Cardinal;
OQOOOCQCQ0:LongInt;
OCOOOCQCQ0:Integer;
begin
O0OOOCQCQ0[0].pvBuffer:=PtrOffset(O00OOCQCQ0,OO0OOCQCQ0);
O0OOOCQCQ0[0].cbBuffer:=Cardinal(OQ0OOCQCQ0);
O0OOOCQCQ0[0].BufferType:=1;
O0OOOCQCQ0[1].BufferType:=0;
O0OOOCQCQ0[2].BufferType:=0;
O0OOOCQCQ0[3].BufferType:=0;
OC0OOCQCQ0.ulVersion:=0;
OC0OOCQCQ0.cBuffers:=4;
OC0OOCQCQ0.pBuffers:=@O0OOOCQCQ0[0];
OOOOOCQCQ0:=0;
OQOOOCQCQ0:=O0QOQ0CCQ0(@OCC0Q0CCQ0,@OC0OOCQCQ0,0,OOOOOCQCQ0);
if OQOOOCQCQ0<>0 then
raise Exception.CreateFmt('Decrypt error %X',[OQOOOCQCQ0]);
for OCOOOCQCQ0:=0 to High(O0OOOCQCQ0)do
if O0OOOCQCQ0[OCOOOCQCQ0].BufferType=1 then begin
O0C0Q0CCQ0.OOOOC0COQ0(O0OOOCQCQ0[OCOOOCQCQ0].pvBuffer,O0OOOCQCQ0[OCOOOCQCQ0].cbBuffer);
Break;
end;
end;
function OQQ0Q0CCQ0.O0QOOCQCQ0(OOQOOCQCQ0:Pointer;OQQOOCQCQ0:NativeInt):NativeInt;
type
OCQOOCQCQ0=^TTLSPlaintext;
TTLSPlaintext=packed record
ContentType:Byte;
ProtocolVersion:Word;
Length:Word;
end;
var
O0COOCQCQ0:TBytes;
OOCOOCQCQ0,OQCOOCQCQ0:Integer;
begin
while O0C0Q0CCQ0.OOQOO0COQ0-O0C0Q0CCQ0.O0QOO0COQ0<OQQOOCQCQ0 do begin
SetLength(O0COOCQCQ0,SizeOf(TTLSPlaintext));
OOCOOCQCQ0:=OQQOQ0CCQ0(TValueArr(O0COOCQCQ0),0,SizeOf(TTLSPlaintext));
if OOCOOCQCQ0<SizeOf(TTLSPlaintext)then
raise Exception.CreateFmt('TTDS7Protocol.ReadVio requested %d received %d',[SizeOf(TTLSPlaintext),OOCOOCQCQ0]);
Assert(OCQOOCQCQ0(@O0COOCQCQ0[0]).ContentType=$17);
OQCOOCQCQ0:=OCQCOCQOQ0(OCQOOCQCQ0(@O0COOCQCQ0[0]).Length);
SetLength(O0COOCQCQ0,SizeOf(TTLSPlaintext)+OQCOOCQCQ0);
OOCOOCQCQ0:=OQQOQ0CCQ0(TValueArr(O0COOCQCQ0),SizeOf(TTLSPlaintext),OQCOOCQCQ0);
if OOCOOCQCQ0<OQCOOCQCQ0 then
raise Exception.CreateFmt('TTDS7Protocol.ReadVio requested %d received %d',[OQCOOCQCQ0,OOCOOCQCQ0]);
OCC0OCQCQ0(@O0COOCQCQ0[0],0,Length(O0COOCQCQ0));
end;
Result:=OQQOOCQCQ0;
if OQQOOCQCQ0>0 then begin
Move(O0C0Q0CCQ0.OCOOO0COQ0[O0C0Q0CCQ0.O0QOO0COQ0],OOQOOCQCQ0^,OQQOOCQCQ0);
O0C0Q0CCQ0.OCCCC0COQ0(OQQOOCQCQ0);
end;
end;
{$ENDIF}
end.
