//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////
{$I Tds.inc}
unit TdsMD5SHA1CSPUni;
interface
uses
SysUtils,
CRTypes,CRHashAlgorithm,
{$IFNDEF UNIDACPRO}
TdsUtils,TdsSSLTypes,TdsBridge;
{$ELSE}
TdsUtilsUni,TdsSSLTypesUni,TdsBridgeUni;
{$ENDIF}
type
OCOCO0Q0Q0=class(THashAlgorithm)
private
O0QCO0Q0Q0:THashAlgorithm;
OOQCO0Q0Q0:THashAlgorithm;
OQQCO0Q0Q0:OOCCCQQ0Q0;
OCQCO0Q0Q0:TBytes;
procedure O0CCO0Q0Q0(const OOCCO0Q0Q0:TBytes);
protected
procedure HashCore(const OCCCO0Q0Q0:TValueArr;O000O0Q0Q0,OO00O0Q0Q0:Integer);override;
function HashFinal:TBytes;override;
public
constructor Create;override;
destructor Destroy;override;
procedure Initialize;override;
class function GetHashSize:Integer;override;
property OOQ0O0Q0Q0:OOCCCQQ0Q0 read OQQCO0Q0Q0 write OQQCO0Q0Q0;
property OQQ0O0Q0Q0:TBytes read OCQCO0Q0Q0 write O0CCO0Q0Q0;
end;
implementation
uses
CLRClasses,CRDECUtil,CRHash,
{$IFNDEF UNIDACPRO}
TdsSSL3HandshakeLayer;
{$ELSE}
TdsSSL3HandshakeLayerUni;
{$ENDIF}
constructor OCOCO0Q0Q0.Create;
begin
inherited;
OQQCO0Q0Q0:=O0QCCQQ0Q0;
O0QCO0Q0Q0:=THash_MD5.Create;
OOQCO0Q0Q0:=THash_SHA1.Create;
end;
destructor OCOCO0Q0Q0.Destroy;
begin
O0QCO0Q0Q0.Free;
OOQCO0Q0Q0.Free;
if Length(OCQCO0Q0Q0)>0 then
FillChar(OCQCO0Q0Q0[0],Length(OCQCO0Q0Q0),0);
inherited;
end;
procedure OCOCO0Q0Q0.O0CCO0Q0Q0(const OOCCO0Q0Q0:TBytes);
begin
SetLength(OCQCO0Q0Q0,Length(OOCCO0Q0Q0));
if Length(OOCCO0Q0Q0)>0 then
Move(OOCCO0Q0Q0[0],OCQCO0Q0Q0[0],Length(OOCCO0Q0Q0));
end;
class function OCOCO0Q0Q0.GetHashSize:Integer;
begin
Result:=36;
end;
procedure OCOCO0Q0Q0.Initialize;
begin
O0QCO0Q0Q0.Initialize;
OOQCO0Q0Q0.Initialize;
end;
procedure OCOCO0Q0Q0.HashCore(const OCCCO0Q0Q0:TValueArr;O000O0Q0Q0,OO00O0Q0Q0:Integer);
begin
O0QCO0Q0Q0.TransformBlock(OCCCO0Q0Q0,O000O0Q0Q0,OO00O0Q0Q0,OCCCO0Q0Q0,O000O0Q0Q0);
OOQCO0Q0Q0.TransformBlock(OCCCO0Q0Q0,O000O0Q0Q0,OO00O0Q0Q0,OCCCO0Q0Q0,O000O0Q0Q0);
end;
function OCOCO0Q0Q0.HashFinal:TBytes;
var
OC00O0Q0Q0,O0O0O0Q0Q0:THashAlgorithm;
begin
if OQQCO0Q0Q0=O0CCCQQ0Q0 then begin
OC00O0Q0Q0:=OQ0COOOCQ0.Create(OQ0OCOQOQ0,O0QCO0Q0Q0,OCQCO0Q0Q0);
O0O0O0Q0Q0:=OQ0COOOCQ0.Create(OOC0COQOQ0,OOQCO0Q0Q0,OCQCO0Q0Q0);
end
else begin
OC00O0Q0Q0:=O0QCO0Q0Q0;
O0O0O0Q0Q0:=OOQCO0Q0Q0;
end;
try
SetLength(Result,36);
OC00O0Q0Q0.TransformFinalBlock(nil,0,0);
O0O0O0Q0Q0.TransformFinalBlock(nil,0,0);
Buffer.BlockCopy(OC00O0Q0Q0.Hash,0,Result,0,16);
Buffer.BlockCopy(O0O0O0Q0Q0.Hash,0,Result,16,20);
finally
if OQQCO0Q0Q0=O0CCCQQ0Q0 then begin
OC00O0Q0Q0.Free;
O0O0O0Q0Q0.Free;
end;
end;
end;
end.
