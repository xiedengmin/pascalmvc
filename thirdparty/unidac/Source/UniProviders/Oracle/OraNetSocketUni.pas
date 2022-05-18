//  Oracle Data Access Components Net
//  Copyright (c) 1998-2019 Devart. All right reserved.
//  OraNetSSL (OCI Lite)
//////////////////////////////////////////////////
{$I Odac.inc}
unit OraNetSocketUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,Registry,
{$ENDIF}
Classes,Types,SysUtils,
CRTypes,CLRClasses,MemUtils,
{$IFNDEF LITE}
CRVioHttp,
{$ENDIF}
CRVio,CRVioTcp;
type
O00QOCCOQ0=class
protected
OO0QOCCOQ0:TCRVio;
function OQ0QOCCOQ0:Boolean;
public
destructor Destroy;override;
procedure O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);virtual;abstract;
procedure OOQQOCCOQ0;virtual;
procedure OQQQOCCOQ0;virtual;
function OCQQOCCOQ0(const O0CQOCCOQ0:TValueArr;OOCQOCCOQ0,OQCQOCCOQ0:integer):Integer;
function OCCQOCCOQ0(const O00COCCOQ0:TValueArr;OO0COCCOQ0,OQ0COCCOQ0:integer):Integer;
function OC0COCCOQ0(const O0OCOCCOQ0:TValueArr;OOOCOCCOQ0,OQOCOCCOQ0:integer):Integer;
function OCOCOCCOQ0(const O0QCOCCOQ0:TValueArr;OOQCOCCOQ0,OQQCOCCOQ0:integer):Integer;
function OCQCOCCOQ0(O0CCOCCOQ0:integer=-1):Boolean;
property OOCCOCCOQ0:Boolean read OQ0QOCCOQ0;
end;
OQCCOCCOQ0=class(O00QOCCOQ0)
public
procedure O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);override;
end;
{$IFNDEF LITE}
OCCCOCCOQ0=class(OQCCOCCOQ0)
private
O000OCCOQ0:TCRIOHandler;
OO00OCCOQ0:TSSLOptions;
public
constructor Create(OC00OCCOQ0:TCRIOHandler;O0O0OCCOQ0:TSSLOptions);
procedure O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);override;
procedure OOQQOCCOQ0;override;
procedure OQQQOCCOQ0;override;
end;
OQO0OCCOQ0=class(OQCCOCCOQ0)
private
OCO0OCCOQ0:TCRIOHandler;
O0Q0OCCOQ0:TSSHOptions;
OOQ0OCCOQ0:TSSHOptions;
public
constructor Create(OC00OCCOQ0:TCRIOHandler;OCQ0OCCOQ0:TSSHOptions);
procedure O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);override;
procedure OQQQOCCOQ0;override;
end;
OOC0OCCOQ0=class(OQCCOCCOQ0)
private
OQC0OCCOQ0:THttpOptions;
OCC0OCCOQ0:TProxyOptions;
public
constructor Create(OO0OOCCOQ0:THttpOptions;OQ0OOCCOQ0:TProxyOptions);
procedure O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);override;
end;
{$ENDIF}
implementation
uses
CRFunctions,
{$IFNDEF UNIDACPRO}
OraNetClasses;
{$ELSE}
OraNetClassesUni;
{$ENDIF}
destructor O00QOCCOQ0.Destroy;
begin
OQQQOCCOQ0;
inherited;
end;
function O00QOCCOQ0.OQ0QOCCOQ0:Boolean;
begin
if OO0QOCCOQ0<>nil then
Result:=OO0QOCCOQ0.Connected
else
Result:=False;
end;
procedure O00QOCCOQ0.OOQQOCCOQ0;
begin
end;
procedure O00QOCCOQ0.OQQQOCCOQ0;
begin
FreeAndNil(OO0QOCCOQ0);
end;
function O00QOCCOQ0.OC0COCCOQ0(const O0OCOCCOQ0:TValueArr;OOOCOCCOQ0,OQOCOCCOQ0:integer):Integer;
begin
Result:=OO0QOCCOQ0.Read(O0OCOCCOQ0,OOOCOCCOQ0,OQOCOCCOQ0);
end;
function O00QOCCOQ0.OCQQOCCOQ0(const O0CQOCCOQ0:TValueArr;OOCQOCCOQ0,OQCQOCCOQ0:integer):Integer;
begin
Result:=OO0QOCCOQ0.ReadNoWait(O0CQOCCOQ0,OOCQOCCOQ0,OQCQOCCOQ0);
end;
function O00QOCCOQ0.OCOCOCCOQ0(const O0QCOCCOQ0:TValueArr;OOQCOCCOQ0,OQQCOCCOQ0:integer):Integer;
begin
Result:=OO0QOCCOQ0.Write(O0QCOCCOQ0,OOQCOCCOQ0,OQQCOCCOQ0);
end;
function O00QOCCOQ0.OCCQOCCOQ0(const O00COCCOQ0:TValueArr;OO0COCCOQ0,OQ0COCCOQ0:integer):Integer;
begin
Result:=OO0QOCCOQ0.WriteNoWait(O00COCCOQ0,OO0COCCOQ0,OQ0COCCOQ0);
end;
function O00QOCCOQ0.OCQCOCCOQ0(O0CCOCCOQ0:integer=-1):Boolean;
begin
Result:=OO0QOCCOQ0.WaitForData(O0CCOCCOQ0);
end;
procedure OQCCOCCOQ0.O0OQOCCOQ0;
begin
OO0QOCCOQ0:=TCRVioTcp.Create;
try
TCRVioTcp(OO0QOCCOQ0).IPVersion:=OOOQOCCOQ0;
TCRVioTcp(OO0QOCCOQ0).Host:=OQOQOCCOQ0;
TCRVioTcp(OO0QOCCOQ0).Port:=OCOQOCCOQ0;
TCRVioTcp(OO0QOCCOQ0).ConnectionTimeOut:=O0QQOCCOQ0;
TCRVioTcp(OO0QOCCOQ0).SendTimeout:=O0QQOCCOQ0;
TCRVioTcp(OO0QOCCOQ0).ReceiveTimeout:=O0QQOCCOQ0;
if PacketSize>0 then begin
TCRVioTcp(OO0QOCCOQ0).SendBuffer:=PacketSize;
TCRVioTcp(OO0QOCCOQ0).ReceiveBuffer:=PacketSize;
end;
OO0QOCCOQ0.Connect;
except
FreeAndNil(OO0QOCCOQ0);
raise;
end;
end;
{$IFNDEF LITE}
constructor OCCCOCCOQ0.Create(OC00OCCOQ0:TCRIOHandler;O0O0OCCOQ0:TSSLOptions);
begin
inherited Create;
O000OCCOQ0:=OC00OCCOQ0;
OO00OCCOQ0:=O0O0OCCOQ0;
end;
procedure OCCCOCCOQ0.O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);
var
OOO0OCCOQ0:TCRIOHandler;
begin
if OO00OCCOQ0<>nil then
OO00OCCOQ0.Enabled:=True
else
OCQO0QOOQ0('SSLOptions is not specefied');
OOO0OCCOQ0:=O000OCCOQ0;
if OOO0OCCOQ0=nil then
OCQO0QOOQ0('IOHandler is not specefied')
else
OO00OCCOQ0.TrustSelfSignedCertificate:=True;
OO0QOCCOQ0:=TCRVioHandler.Create(OQOQOCCOQ0,OCOQOCCOQ0,OOO0OCCOQ0,nil,nil,OO00OCCOQ0,nil,OOOQOCCOQ0);
try
OO0QOCCOQ0.ConnectionTimeOut:=O0QQOCCOQ0;
OO0QOCCOQ0.SendTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.ReceiveTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.Connect;
TCRVioHandler(OO0QOCCOQ0).IsSecure:=True;
except
OQQQOCCOQ0;
raise;
end;
end;
procedure OCCCOCCOQ0.OOQQOCCOQ0;
begin
TCRVioHandler(OO0QOCCOQ0).IsSecure:=False;
TCRVioHandler(OO0QOCCOQ0).IsSecure:=True;
end;
procedure OCCCOCCOQ0.OQQQOCCOQ0;
begin
inherited;
end;
constructor OQO0OCCOQ0.Create(OC00OCCOQ0:TCRIOHandler;OCQ0OCCOQ0:TSSHOptions);
begin
inherited Create;
OCO0OCCOQ0:=OC00OCCOQ0;
OOQ0OCCOQ0:=OCQ0OCCOQ0;
end;
procedure OQO0OCCOQ0.OQQQOCCOQ0;
begin
inherited;
FreeAndNil(O0Q0OCCOQ0);
end;
procedure OQO0OCCOQ0.O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);
var
OOO0OCCOQ0:TCRIOHandler;
O0C0OCCOQ0:TSSHOptions;
begin
O0Q0OCCOQ0:=TSSHOptions.Create;
O0C0OCCOQ0:=O0Q0OCCOQ0;
OOO0OCCOQ0:=OCO0OCCOQ0;
if OOO0OCCOQ0=nil then
OCQO0QOOQ0('IOHandler is not specefied');
if O0C0OCCOQ0.Username='' then
O0C0OCCOQ0.Username:='opc';
OO0QOCCOQ0:=TCRVioHandler.Create(OQOQOCCOQ0,OCOQOCCOQ0,OOO0OCCOQ0,nil,nil,nil,O0C0OCCOQ0,OOOQOCCOQ0);
try
OO0QOCCOQ0.ConnectionTimeOut:=O0QQOCCOQ0;
OO0QOCCOQ0.SendTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.ReceiveTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.Connect;
except
OQQQOCCOQ0;
raise;
end;
end;
constructor OOC0OCCOQ0.Create(OO0OOCCOQ0:THttpOptions;OQ0OOCCOQ0:TProxyOptions);
begin
inherited Create;
OQC0OCCOQ0:=OO0OOCCOQ0;
OCC0OCCOQ0:=OQ0OOCCOQ0;
end;
procedure OOC0OCCOQ0.O0OQOCCOQ0(OOOQOCCOQ0:TIPVersion;const OQOQOCCOQ0:string;OCOQOCCOQ0,O0QQOCCOQ0,PacketSize:Integer);
begin
if OQC0OCCOQ0<>nil then
OQC0OCCOQ0.Enabled:=True
else
OCQO0QOOQ0('HttpOptions is not specefied');
OO0QOCCOQ0:=TCRVioHttp.Create(nil,OQC0OCCOQ0,OCC0OCCOQ0,OQOQOCCOQ0,OCOQOCCOQ0,OOOQOCCOQ0);
try
OO0QOCCOQ0.ConnectionTimeOut:=O0QQOCCOQ0;
OO0QOCCOQ0.SendTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.ReceiveTimeout:=O0QQOCCOQ0;
OO0QOCCOQ0.Connect;
except
OQQQOCCOQ0;
raise;
end;
end;
{$ENDIF}
end.
