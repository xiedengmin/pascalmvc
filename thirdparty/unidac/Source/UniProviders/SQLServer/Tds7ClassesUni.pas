//////////////////////////////////////////////////
//  SQL Server Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access in Direct mode
//////////////////////////////////////////////////
{$I Sdac.inc}
unit Tds7ClassesUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,Registry,
{$ENDIF}
Classes,SysUtils,StrUtils,Variants,FmtBcd,
{$IFDEF LOG_PACKETS}
LogHandler,
{$ENDIF}
{$IFNDEF NODBACCESS}
DB,
{$ENDIF}
{$IFDEF VER12P}
DateUtils,
{$ENDIF}
CRTypes,CRFunctions,CLRClasses,CRParser,CRVio,MemUtils,MemData,CRAccess,
{$IFNDEF LITE}
CRDataTypeMap,
{$ENDIF}
{$IFNDEF UNIDACPRO}
SqlClasses,MSClasses,MSParser,
TdsTypes,TdsClasses,Tds7Consts,TdsProtocol,Tds7Protocol,
TdsNet,Tds7Net;
{$ELSE}
SqlClassesUni,MSClassesUni,MSParserUni,
TdsTypesUni,TdsClassesUni,Tds7ConstsUni,TdsProtocolUni,Tds7ProtocolUni,
TdsNetUni,Tds7NetUni;
{$ENDIF}
type
TTDS7Connector=class(TTDSConnector)
private
function OQOQQOQCQ0:TMSSQLConnection;
procedure OCOQQOQCQ0(var O0QQQOQCQ0:string;var OOQQQOQCQ0:integer);
function O0CQQOQCQ0(const OOCQQOQCQ0:string):integer;
procedure OQQCQOQCQ0(const OCQCQOQCQ0:array of const);
function OOCCQOQCQ0:boolean;
protected
function OOCQC000Q0:TCRConnectionClass;override;
function OQCQC000Q0:OCCQCOQOQ0;override;
function OCCQC000Q0:TIPVersion;override;
procedure O00CC000Q0;override;
function OO0CC000Q0:boolean;override;
public
procedure InitConnectionSettings;override;
function MultipleConnectionsSupported:boolean;override;
procedure SetDatabase(const OCO0QOQCQ0:string);override;
end;
TTDS7Transaction=class(TCRTransaction)
private
O0Q0QOQCQ0:OOOQOQQCQ0;
OOQ0QOQCQ0:OC0OOOQCQ0;
function OQQ0QOQCQ0:TMSSQLConnection;
public
constructor Create;override;
destructor Destroy;override;
procedure StartTransaction;override;
procedure Commit;override;
procedure Rollback;override;
procedure Savepoint(const OQ0OQOQCQ0:string);override;
procedure RollbackToSavepoint(const OQOOQOQCQ0:string);override;
procedure Reset;override;
property TransactionContext:OC0OOOQCQ0 read OOQ0QOQCQ0 write OOQ0QOQCQ0;
end;
TTDS7Command=class(TTDSCommand)
private
procedure OOQOQOQCQ0(OQQOQOQCQ0:Integer;OCQOQOQCQ0:OQQCC0COQ0);
procedure O0CQO0QCQ0(OOCQO0QCQ0:boolean;out OQCQO0QCQ0:string);
protected
OQO0O0QCQ0:string;
OCO0O0QCQ0:string;
O0Q0O0QCQ0:integer;
OOQ0O0QCQ0:boolean;
OQQ0O0QCQ0:boolean;
OCQ0O0QCQ0:string;
O0C0O0QCQ0:string;
OOC0O0QCQ0:integer;
OQC0O0QCQ0:TMSCursorType;
OCC0O0QCQ0:integer;
O00OO0QCQ0:boolean;
procedure OO0OO0QCQ0;
procedure OOOOO0QCQ0;
procedure OQOOO0QCQ0(OCOOO0QCQ0,O0QOO0QCQ0:Integer);
function OOQOO0QCQ0(OQQOO0QCQ0:Integer;var OCQOO0QCQ0,O0COO0QCQ0:Integer):Integer;
procedure OOCOO0QCQ0(const OQCOO0QCQ0:string);
procedure OCCOO0QCQ0(const O00Q00QCQ0:string);
procedure OO0Q00QCQ0;
procedure OC0Q00QCQ0;
procedure OOQ0Q000Q0;override;
procedure O00OQ000Q0;override;
procedure OQQ0Q000Q0;override;
procedure O0C0Q000Q0(O00C00QCQ0:boolean);override;
function OQC0Q000Q0:integer;override;
function OCC0Q000Q0:integer;override;
class function InternalGetBatchSQL(const OOQ000QCQ0:string;OQQ000QCQ0:TParsedSQLType;
OCQ000QCQ0:TDAParamsInfo;O0C000QCQ0:integer):string;override;
public
constructor Create;override;
class function GetContextClass:OQCQCOQOQ0;override;
class function GetSQLInfoClass:TSQLInfoClass;override;
class function GetParserClass:TSQLParserClass;override;
class function GetParamDescClass:TParamDescClass;override;
{$IFNDEF LITE}
class function GetMapRulesClass:TCRMapRulesClass;override;
{$ENDIF}
procedure Close;override;
function CreateProcCall(const OOC000QCQ0:string;OQC000QCQ0:Boolean;OCC000QCQ0:Boolean):string;override;
function GetProp(O0OQC0QCQ0:Integer;out OOOQC0QCQ0:variant):Boolean;override;
function SetProp(OQOQC0QCQ0:Integer;const OCOQC0QCQ0:variant):Boolean;override;
end;
TTDS7RecordSet=class(TTDSRecordSet)
private
OOQCOCCCQ0:TTDS7Command;
O0QQC0QCQ0:boolean;
OOQQC0QCQ0:boolean;
OQQQC0QCQ0:TMSCursorType;
OCQQC0QCQ0:integer;
function O0CQC0QCQ0:boolean;
{$IFNDEF LITE}
class function OOCQC0QCQ0(OQCQC0QCQ0:Word;OCCQC0QCQ0,O00CC0QCQ0,OO0CC0QCQ0:Boolean;OQ0CC0QCQ0:Integer):Word;
{$ENDIF}
protected
function OC0CC0QCQ0:TMSCursorType;
function IsServerCursor:boolean;override;
function IsStaticCursor:boolean;override;
function IsDynamicCursor:boolean;override;
function IsWideField(O0QCC0QCQ0:TFieldDesc):boolean;override;
function GetCommandClass:TSqlCommandClass;override;
procedure SetCommand(OQQCC0QCQ0:TCRCommand);override;
procedure InternalOpen(OCQCC0QCQ0:Boolean=False);override;
function OOQO0CCCQ0:boolean;override;
function OQCO0CCCQ0(O0CCC0QCQ0:integer;OOCCC0QCQ0:boolean):NativeUInt;override;
procedure InitFetchedBlock(OOO0C0QCQ0:PBlockHeader;OQO0C0QCQ0:Integer;OCO0C0QCQ0:boolean);override;
procedure ProcessNoResult(O0Q0C0QCQ0:Boolean);override;
procedure O0OQCCCCQ0(var OOQ0C0QCQ0:TSqlFieldDesc;OQQ0C0QCQ0:integer);override;
procedure InternalAppend(OQ0OC0QCQ0:IntPtr);override;
procedure InternalDelete;override;
procedure InternalUpdate(OOQOC0QCQ0:IntPtr);override;
procedure OOO0OCCCQ0;override;
public
constructor Create;override;
function GetFieldDescType:TFieldDescClass;override;
function NeedGetRecordAfterGotoBookmark:boolean;override;
function GetProp(O0OQC0QCQ0:integer;out OOOQC0QCQ0:variant):boolean;override;
function SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;override;
end;
TTDSTableTypeRecordSet=class(TTDS7RecordSet)
protected
OCCOC0QCQ0:string;
procedure CreateFieldDescs;override;
procedure OQCCQ0QCQ0;
function OCCCQ0QCQ0:TTDS7RecordSet;
procedure InternalOpen(OCQCC0QCQ0:boolean=False);override;
procedure InternalPrepare;override;
function FetchingAccessible(O000Q0QCQ0:boolean):boolean;override;
public
function IsFullReopen:boolean;override;
procedure ExplicitInitFields;override;
function SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;override;
end;
{$IFNDEF LITE}
TTDS7MetaData=class(TMSSQLMetaData)
private
OQ00Q0QCQ0:string;
function OC00Q0QCQ0(const O0O0Q0QCQ0:string):string;
procedure OOO0Q0QCQ0;
function OQO0Q0QCQ0(OCO0Q0QCQ0:TStrings;const O0Q0Q0QCQ0,OOQ0Q0QCQ0:string):TData;
function OO0OQ0QCQ0(OQ0OQ0QCQ0:array of Integer):TData;
function OQOQOCOCQ0(OCOQOCOCQ0:TStrings;const O0QQOCOCQ0:string;
OOQQOCOCQ0:array of Integer):TData;
protected
procedure O0CCOCOCQ0;virtual;
function CreateRecordSet:TCRRecordSet;override;
function GetTables(OCQ0OCOCQ0:TStrings):TData;override;
function GetColumns(OCC0OCOCQ0:TStrings):TData;override;
function GetProcedures(OOCQ0COCQ0:TStrings):TData;override;
function GetProcedureParameters(O0QC0COCQ0:TStrings):TData;override;
function GetIndexColumns(OOQ00COCQ0:TStrings):TData;override;
function GetConstraints(OO0QCCOCQ0:TStrings):TData;override;
function GetDatabases(OOQQCCOCQ0:TStrings):TData;override;
property OQQQCCOCQ0:string read OQ00Q0QCQ0;
public
constructor Create;override;
end;
TTDS7MetaData2000=class(TTDS7MetaData)
protected
procedure O0CCOCOCQ0;override;
function GetTables(OCQ0OCOCQ0:TStrings):TData;override;
function GetProcedures(OOCQ0COCQ0:TStrings):TData;override;
function GetProcedureParameters(O0QC0COCQ0:TStrings):TData;override;
public
constructor Create;override;
end;
{$ENDIF}
TTDS7Loader=class(TTDSLoader)
private
OCQQCCOCQ0:boolean;
O0CQCCOCQ0:boolean;
OOCQCCOCQ0:integer;
OQCQCCOCQ0:boolean;
OCCQCCOCQ0:boolean;
O00CCCOCQ0:boolean;
OO0CCCOCQ0:TTDSCommand;
protected
function OC0CCCCCQ0(OQ0CCCOCQ0:TSqlFieldDesc;OC0CCCOCQ0:TParamDesc):boolean;override;
procedure OQOCCCCCQ0;override;
procedure OCOCCCCCQ0;override;
procedure FillColumn(OQOCCCOCQ0:TCRLoaderColumn;OCOCCCOCQ0:TFieldDesc);override;
function OO00CCCCQ0(const O0QCCCOCQ0:TParamDesc):string;override;
function OC00CCCCQ0(const OOQCCCOCQ0:TParamDesc):Boolean;override;
public
function SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;override;
function GetProp(O0OQC0QCQ0:integer;var OOOQC0QCQ0:variant):boolean;override;
class function GetColumnClass:TCRLoaderColumnClass;override;
procedure Prepare;override;
end;
OCQCCCOCQ0=class(TMSParamDesc)
protected
O0CCCCOCQ0:boolean;
OOCCCCOCQ0:Cardinal;
public
property OQCCCCOCQ0:boolean read O0CCCCOCQ0;
end;
{$IFDEF MSWINDOWS}
OCCCCCOCQ0=class
private
O000CCOCQ0:TStringList;
OO00CCOCQ0:String;
OQ00CCOCQ0:Integer;
procedure OC00CCOCQ0;
procedure O0O0CCOCQ0;
procedure OQQ0CCOCQ0(const OCQ0CCOCQ0:string);
public
constructor Create;
destructor Destroy;override;
function O00OCCOCQ0(const OO0OCCOCQ0:string):Boolean;
property O0OOCCOCQ0:string read OO00CCOCQ0;
property OOOOCCOCQ0:Integer read OQ00CCOCQ0;
end;
{$ENDIF}
implementation
uses
DAConsts,CRProps,CRNumeric,CRVioTcp,CRVioUdp,
{$IFNDEF UNIDACPRO}
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}MSUDT,{$ENDIF} MSDataTypeMap,
{$ENDIF}
OLEDBC,MSConsts,TdsConsts,MSProps;
{$ELSE}
{$IFDEF MSWINDOWS}MSUDTUni,{$ENDIF} MSDataTypeMapUni,
OLEDBCUni,MSConstsUni,TdsConstsUni,MSPropsUni;
{$ENDIF}
var
OQOOCCOCQ0:array[0..$FF]of Word;
OCOOCCOCQ0,O0QOCCOCQ0,OOQOCCOCQ0,
OQQOCCOCQ0:array[dtUnknown..dtXML]of Byte;
{$IFDEF MSWINDOWS}
OCQOCCOCQ0:OCCCCCOCQ0;
{$ENDIF}
function TTDS7Connector.OOCQC000Q0:TCRConnectionClass;
begin
Result:=TMSSQLConnection;
end;
function TTDS7Connector.OQCQC000Q0:OCCQCOQOQ0;
begin
Result:=OQOOOOQCQ0;
end;
function TTDS7Connector.OQOQQOQCQ0:TMSSQLConnection;
begin
Result:=TMSSQLConnection(Connection);
end;
function TTDS7Connector.OCCQC000Q0:TIPVersion;
begin
Result:=OQOQQOQCQ0.IPVersion;
end;
procedure TTDS7Connector.OQQCQOQCQ0(const OCQCQOQCQ0:array of const);
var
O0CCQOQCQ0:Integer;
begin
Assert(Length(OCQCQOQCQ0)>=1);
O0CCQOQCQ0:=0;
{$IFNDEF FPC}
Assert(OCQCQOQCQ0[0].VType=vtPointer);
Move(OCQCQOQCQ0[0].VPointer^,O0CCQOQCQ0,3);
{$ELSE}
Move(OCQCQOQCQ0[0].VInteger,O0CCQOQCQ0,3);
{$ENDIF}
OQOQQOQCQ0.Options.LocaleIdentifier:=O0CCQOQCQ0;
end;
procedure TTDS7Connector.OCOQQOQCQ0(var O0QQQOQCQ0:string;var OOQQQOQCQ0:integer);
var
OQQQQOQCQ0:Integer;
OCQQQOQCQ0:string;
begin
OQQQQOQCQ0:=Pos(',',O0QQQOQCQ0);
if OQQQQOQCQ0=0 then
Exit;
OCQQQOQCQ0:=Copy(O0QQQOQCQ0,OQQQQOQCQ0+1,Length(O0QQQOQCQ0)-OQQQQOQCQ0);
O0QQQOQCQ0:=Copy(O0QQQOQCQ0,1,OQQQQOQCQ0-1);
if not TryStrToInt(OCQQQOQCQ0,OOQQQOQCQ0)then
OOQQQOQCQ0:=0;
end;
function TTDS7Connector.O0CQQOQCQ0(const OOCQQOQCQ0:string):integer;
var
OQCQQOQCQ0:integer;
OCCQQOQCQ0:TCRVioUdp;
O00CQOQCQ0:TBytes;
OO0CQOQCQ0:Integer;
OQ0CQOQCQ0,OC0CQOQCQ0:string;
O0OCQOQCQ0,OOOCQOQCQ0:Integer;
OQOCQOQCQ0:TStringArray;
OCOCQOQCQ0,O0QCQOQCQ0:string;
OOQCQOQCQ0:boolean;
begin
{$IFNDEF VER9P}
OQOCQOQCQ0:=nil;
{$ENDIF}
Result:=0;
SetLength(O00CQOQCQ0,8192);
if(LowerCase(OOCQQOQCQ0)='(local)')or(OOCQQOQCQ0='.')then
OQ0CQOQCQ0:='localhost'
else
OQ0CQOQCQ0:=OOCQQOQCQ0;
O0QCQOQCQ0:=OQOQQOQCQ0.InstanceName;
if O0QCQOQCQ0='' then
O0QCQOQCQ0:='MSSQLSERVER';
OCCQQOQCQ0:=TCRVioUdp.Create(OQ0CQOQCQ0,DefaultSDACUdpPort,OQOQQOQCQ0.IPVersion);
try
OCCQQOQCQ0.CreateSocket;
OCCQQOQCQ0.ReceiveTimeout:=1;
for OQCQQOQCQ0:=0 to 15 do begin
O00CQOQCQ0[0]:=3;
{$IFDEF LOG_PACKETS}
AddToLog(Format('UDP querying: Host %s',[OQ0CQOQCQ0]));
{$ENDIF}
if OCCQQOQCQ0.Write(@O00CQOQCQ0[0],0,1)=0 then
Break;
OO0CQOQCQ0:=OCCQQOQCQ0.ReadFromHost(@O00CQOQCQ0[0],0,Length(O00CQOQCQ0),OOQCQOQCQ0);
if OO0CQOQCQ0<=0 then
Exit;
if OOQCQOQCQ0 then begin
OC0CQOQCQ0:=Encoding.ASCII.GetString(Copy(O00CQOQCQ0,3,OO0CQOQCQ0-3));
{$IFDEF LOG_PACKETS}
AddToLog(Format('UDP reply: Host %s instance %s',[OQ0CQOQCQ0,OC0CQOQCQ0]));
{$ENDIF}
O0OCQOQCQ0:=Pos('ServerName',OC0CQOQCQ0);
OOOCQOQCQ0:=0;
if O0OCQOQCQ0=0 then
Continue;
while True do
try
OOOCQOQCQ0:=PosEx('ServerName',OC0CQOQCQ0,O0OCQOQCQ0+1);
if OOOCQOQCQ0=0 then
if O0OCQOQCQ0<Length(OC0CQOQCQ0)then
OOOCQOQCQ0:=Length(OC0CQOQCQ0)
else
Break;
OCOCQOQCQ0:=Copy(OC0CQOQCQ0,O0OCQOQCQ0,OOOCQOQCQ0-O0OCQOQCQ0);
OQOCQOQCQ0:=SplitString(OCOCQOQCQ0,';');
if(Length(OQOCQOQCQ0)>=10)and(UpperCase(OQOCQOQCQ0[8])='TCP')and(UpperCase(O0QCQOQCQ0)=UpperCase(OQOCQOQCQ0[3]))then begin
Result:=StrToInt(OQOCQOQCQ0[9]);
Exit;
end;
finally
O0OCQOQCQ0:=OOOCQOQCQ0;
end;
Exit;
end;
end;
finally
OCCQQOQCQ0.Free;
end;
end;
procedure TTDS7Connector.InitConnectionSettings;
{$IFDEF MSWINDOWS}
function OQCCQOQCQ0:string;
var
OCCCQOQCQ0:DWord;
begin
OCCCQOQCQ0:=1024;
SetLength(Result,OCCCQOQCQ0);
if GetUserName(PChar(Result),OCCCQOQCQ0)then
SetLength(Result,OCCCQOQCQ0-1)
else
RaiseLastOSError;
end;
{$ENDIF}
var
O000QOQCQ0,OO00QOQCQ0:string;
OQ00QOQCQ0:integer;
OC00QOQCQ0,O0O0QOQCQ0,OOO0QOQCQ0:string;
OQO0QOQCQ0:integer;
begin
O0O0QOQCQ0:=Connection.GetUsername;
OOO0QOQCQ0:=Connection.GetPassword;
OC00QOQCQ0:=GetEnvironmentVariable('USERDOMAIN');
if OQOQQOQCQ0.Options.Authentication=auWindows then begin
{$IFDEF MSWINDOWS}
O0O0QOQCQ0:=OQCCQOQCQ0;
OOO0QOQCQ0:='';
{$ELSE}
raise Exception.Create(SWindowsAuthenticationNotSupported);
{$ENDIF}
OQO0QOQCQ0:=Pos('\',O0O0QOQCQ0);
if OQO0QOQCQ0>0 then begin
OC00QOQCQ0:=Copy(O0O0QOQCQ0,1,OQO0QOQCQ0-1);
O0O0QOQCQ0:=Copy(O0O0QOQCQ0,OQO0QOQCQ0+1,Length(O0O0QOQCQ0)-OQO0QOQCQ0);
end;
end;
{$IFDEF MSWINDOWS}
if OCQOCCOCQ0.O00OCCOCQ0(OQOQQOQCQ0.GetServer)then begin
OQOQQOQCQ0.SetServer(OCQOCCOCQ0.O0OOCCOCQ0);
OQOQQOQCQ0.Port:=OCQOCCOCQ0.OOOOCCOCQ0;
end;
{$ENDIF}
O000QOQCQ0:=OQOQQOQCQ0.Host;
OQ00QOQCQ0:=OQOQQOQCQ0.Port;
OO00QOQCQ0:=OQOQQOQCQ0.InstanceName;
OCO0C000Q0(O000QOQCQ0,OQ00QOQCQ0);
OCOQQOQCQ0(OO00QOQCQ0,OQ00QOQCQ0);
if((OQ00QOQCQ0=0)or(OQ00QOQCQ0=DefaultSDACPort))and(OO00QOQCQ0<>'')then
OQ00QOQCQ0:=O0CQQOQCQ0(O000QOQCQ0);
if OQ00QOQCQ0=0 then
OQ00QOQCQ0:=DefaultSDACPort;
if(LowerCase(O000QOQCQ0)='(local)')or(O000QOQCQ0='.')then
O000QOQCQ0:='localhost';
OCQQC000Q0.OO0QCOQOQ0:=O0O0QOQCQ0;
OCQQC000Q0.OQ0QCOQOQ0:=OOO0QOQCQ0;
OCQQC000Q0.OC0QCOQOQ0:=OQOQQOQCQ0.Options.ApplicationName;
OCQQC000Q0.O0OQCOQOQ0:=OO00QOQCQ0;
OCQQC000Q0.OOOQCOQOQ0:=O000QOQCQ0;
OCQQC000Q0.OQOQCOQOQ0:='';
OCQQC000Q0.OCOQCOQOQ0:=Connection.Database;
OCQQC000Q0.O0QQCOQOQ0:=OQOQQOQCQ0.Options.Authentication=auWindows;
OCQQC000Q0.OOQQCOQOQ0:=OC00QOQCQ0;
OCQQC000Q0.OQQQCOQOQ0:=OQ00QOQCQ0;
OCQQC000Q0.OCQQCOQOQ0:=OQOQQOQCQ0.Options.TrustServerCertificate;
OCQQC000Q0.O0CQCOQOQ0:=OQOQQOQCQ0.Options.MultipleActiveResultSets;
OCQQC000Q0.OOCQCOQOQ0:=OQOQQOQCQ0.Options.ApplicationIntent=aiReadOnly;
end;
procedure TTDS7Connector.O00CC000Q0;
begin
inherited;
if OQOQQOQCQ0.Options.PacketSize>16384 then
OOQQC000Q0.OCOQ0QQOQ0:=16384
else
OOQQC000Q0.OCOQ0QQOQ0:=OQOQQOQCQ0.Options.PacketSize;
OOQQC000Q0.O0QOOQQOQ0:=OQQCQOQCQ0;
OOQQC000Q0.OQ0Q0QQOQ0:=OQOQQOQCQ0.Options.ConnectionTimeout;
OQOOOOQCQ0(OOQQC000Q0).OCOOCOQCQ0:='ODBC';
OQOOOOQCQ0(OOQQC000Q0).OOQOCOQCQ0:=OQOQQOQCQ0.UuidWithBraces;
end;
function TTDS7Connector.OO0CC000Q0:boolean;
begin
Result:=OQOQQOQCQ0.Options.Encrypt;
end;
function TTDS7Connector.MultipleConnectionsSupported:boolean;
begin
Result:=OQOQQOQCQ0.Options.MultipleConnections;
end;
procedure TTDS7Connector.SetDatabase(const OCO0QOQCQ0:string);
begin
OCQQC000Q0.OCOQCOQOQ0:=OCO0QOQCQ0;
if OOQQC000Q0.OO0Q0QQOQ0=nil then
Exit;
if OQOQQOQCQ0.IsSQLAzureEdition then begin
Disconnect;
Connect;
OQOQQOQCQ0.InitConnection;
end
else
OQOQQOQCQ0.ExecuteSQL('USE '+OCO0QOQCQ0);
end;
function TTDS7Connector.OOCCQOQCQ0:boolean;
begin
Result:=FServerMajorVersion>=9;
end;
constructor TTDS7Transaction.Create;
begin
inherited;
O0Q0QOQCQ0:=OOOQOQQCQ0.Create(nil);
OOQ0QOQCQ0:=OC0OOOQCQ0.Create;
end;
destructor TTDS7Transaction.Destroy;
begin
O0Q0QOQCQ0.Free;
OOQ0QOQCQ0.Free;
inherited;
end;
function TTDS7Transaction.OQQ0QOQCQ0:TMSSQLConnection;
var
OCQ0QOQCQ0:integer;
begin
if FConnections.Count=0 then
raise Exception.Create(SNoConnectionsInTransaction);
if FConnections.Count>1 then
raise Exception.Create(SMultiConnectionsInTransaction);
for OCQ0QOQCQ0:=0 to FConnections.Count-1 do
if not FConnections[OCQ0QOQCQ0].GetConnected then
raise Exception.Create(SConnectionInTransactionNotActive);
Result:=TMSSQLConnection(FConnections[0]);
end;
procedure TTDS7Transaction.StartTransaction;
var
O0C0QOQCQ0:TMSSQLConnection;
OOC0QOQCQ0:OQOOOOQCQ0;
begin
O0C0QOQCQ0:=OQQ0QOQCQ0;
OOC0QOQCQ0:=OQOOOOQCQ0((O0C0QOQCQ0.GetConnector as TTDS7Connector).Protocol);
OOC0QOQCQ0.O0QOCOQCQ0:=OOQ0QOQCQ0;
if OOC0QOQCQ0.O0QQ0QQOQ0<O0Q0CCCOQ0 then begin
O0C0QOQCQ0.SetIsolationLevel(FIsolationLevel);
O0C0QOQCQ0.ExecuteSQL('BEGIN TRANSACTION');
end
else
O0Q0QOQCQ0.O0Q0OOQCQ0(OOC0QOQCQ0,OOOCCOCCQ0,FIsolationLevel,'',nil);
FActive:=True;
FNativeTransaction:=True;
end;
procedure TTDS7Transaction.Commit;
var
OQC0QOQCQ0:TMSSQLConnection;
OCC0QOQCQ0:OQOOOOQCQ0;
begin
if OOQ0QOQCQ0.O0OOOOQCQ0=0 then begin
FActive:=False;
Exit;
end;
OQC0QOQCQ0:=OQQ0QOQCQ0;
OCC0QOQCQ0:=OQOOOOQCQ0(TTDS7Connector(OQC0QOQCQ0.GetConnector).Protocol);
if OCC0QOQCQ0.O0QQ0QQOQ0<O0Q0CCCOQ0 then
OQC0QOQCQ0.ExecuteSQL('COMMIT')
else
O0Q0QOQCQ0.O0Q0OOQCQ0(OCC0QOQCQ0,OCOCCOCCQ0,ilCustom,'',nil);
FActive:=False;
end;
procedure TTDS7Transaction.Rollback;
var
O00OQOQCQ0:TMSSQLConnection;
OO0OQOQCQ0:OQOOOOQCQ0;
begin
if OOQ0QOQCQ0.O0OOOOQCQ0=0 then begin
FActive:=False;
Exit;
end;
O00OQOQCQ0:=OQQ0QOQCQ0;
OO0OQOQCQ0:=OQOOOOQCQ0(TTDS7Connector(O00OQOQCQ0.GetConnector).Protocol);
if OO0OQOQCQ0.O0QQ0QQOQ0<O0Q0CCCOQ0 then
O00OQOQCQ0.ExecuteSQL('ROLLBACK')
else
O0Q0QOQCQ0.O0Q0OOQCQ0(OO0OQOQCQ0,O0QCCOCCQ0,FIsolationLevel,'',nil);
FActive:=False;
end;
procedure TTDS7Transaction.RollbackToSavepoint(const OQOOQOQCQ0:string);
var
OCOOQOQCQ0:TMSSQLConnection;
O0QOQOQCQ0:OQOOOOQCQ0;
begin
OCOOQOQCQ0:=OQQ0QOQCQ0;
O0QOQOQCQ0:=OQOOOOQCQ0(TTDS7Connector(OCOOQOQCQ0.GetConnector).Protocol);
if O0QOQOQCQ0.O0QQ0QQOQ0<O0Q0CCCOQ0 then
OCOOQOQCQ0.ExecuteSQL('ROLLBACK TRANSACTION '+OQOOQOQCQ0)
else
O0Q0QOQCQ0.O0Q0OOQCQ0(O0QOQOQCQ0,O0QCCOCCQ0,ilCustom,OQOOQOQCQ0,nil);
end;
procedure TTDS7Transaction.Reset;
begin
inherited;
OOQ0QOQCQ0.O0OOOOQCQ0:=0;
OOQ0QOQCQ0.OOOOOOQCQ0:=0;
O0Q0QOQCQ0.OCC00OQOQ0:=nil;
end;
procedure TTDS7Transaction.Savepoint(const OQ0OQOQCQ0:string);
var
OC0OQOQCQ0:TMSSQLConnection;
O0OOQOQCQ0:OQOOOOQCQ0;
OOOOQOQCQ0:string;
begin
OC0OQOQCQ0:=OQQ0QOQCQ0;
O0OOQOQCQ0:=OQOOOOQCQ0(TTDS7Connector(OC0OQOQCQ0.GetConnector).Protocol);
if O0OOQOQCQ0.O0QQ0QQOQ0<O0Q0CCCOQ0 then begin
if OC0OQOQCQ0.ServerMajorVer<=8 then
OOOOQOQCQ0:='IF (@@TRANCOUNT = 0) SELECT 1 FROM SYSOBJECTS WHERE 0=1; '
else
OOOOQOQCQ0:='';
OOOOQOQCQ0:=OOOOQOQCQ0+'SAVE TRANSACTION '+OQ0OQOQCQ0;
OC0OQOQCQ0.ExecuteSQL(OOOOQOQCQ0);
end
else
O0Q0QOQCQ0.O0Q0OOQCQ0(O0OOQOQCQ0,OOQCCOCCQ0,ilCustom,OQ0OQOQCQ0,nil);
end;
constructor TTDS7Command.Create;
begin
inherited;
OOQ0O0QCQ0:=False;
end;
class function TTDS7Command.GetContextClass:OQCQCOQOQ0;
begin
Result:=OOOQOQQCQ0;
end;
class function TTDS7Command.GetSQLInfoClass:TSQLInfoClass;
begin
Result:=TMSSQLInfo;
end;
class function TTDS7Command.GetParamDescClass:TParamDescClass;
begin
Result:=OCQCCCOCQ0;
end;
class function TTDS7Command.GetParserClass:TSQLParserClass;
begin
Result:=TMSParser;
end;
{$IFNDEF LITE}
class function TTDS7Command.GetMapRulesClass:TCRMapRulesClass;
begin
Result:=TMSMapRules;
end;
{$ENDIF}
procedure TTDS7Command.OOQOQOQCQ0(OQQOQOQCQ0:Integer;OCQOQOQCQ0:OQQCC0COQ0);
var
O0COQOQCQ0:OCQCCCOCQ0;
OOCOQOQCQ0:TTDS7RecordSet;
OQCOQOQCQ0:OQOOOOQCQ0;
OCCOQOQCQ0:IntPtr;
O00QO0QCQ0,OO0QO0QCQ0,OQ0QO0QCQ0:string;
OC0QO0QCQ0:Integer;
O0OQO0QCQ0:OOOCQCQOQ0;
OOOQO0QCQ0:OQOQQCQOQ0;
OQOQO0QCQ0:O00CCQCOQ0;
OCOQO0QCQ0:TMSFieldDesc;
O0QQO0QCQ0:Word;
OOQQO0QCQ0:Variant;
OQQQO0QCQ0:PItemHeader;
OCQQO0QCQ0:O00CQQCOQ0;
begin
O0COQOQCQ0:=OCQCCCOCQ0(Params[OQQOQOQCQ0]);
Assert(IsClass(O0COQOQCQ0.GetObject,TMSSQLTableObject));
OOCOQOQCQ0:=TTDSTableTypeRecordSet(TMSSQLTableObject(O0COQOQCQ0.GetObject).RecordSet).OCCCQ0QCQ0;
OQCOQOQCQ0:=OQOOOOQCQ0(GetProtocol);
MSSQLInfo.SplitObjectName(TMSSQLTableObject(O0COQOQCQ0.GetObject).TableTypeName,O00QO0QCQ0,OO0QO0QCQ0,OQ0QO0QCQ0);
OCQOQOQCQ0.OC00C0COQ0(0);
OCQOQOQCQ0.OC00C0COQ0(Length(OO0QO0QCQ0));
OCQOQOQCQ0.OOQOC0COQ0(WideString(OO0QO0QCQ0));
OCQOQOQCQ0.OC00C0COQ0(Length(OQ0QO0QCQ0));
OCQOQOQCQ0.OOQOC0COQ0(WideString(OQ0QO0QCQ0));
SetLength(O0OQO0QCQ0,OOCOQOQCQ0.FFields.Count);
OCQQO0QCQ0.OO0CQQCOQ0:=$0409;
OCQQO0QCQ0.OQ0CQQCOQ0:=$0100;
OCQQO0QCQ0.OC0CQQCOQ0:=$68;
OCQOQOQCQ0.OCO0C0COQ0(OOCOQOQCQ0.FFields.Count);
for OC0QO0QCQ0:=0 to OOCOQOQCQ0.FFields.Count-1 do begin
OCOQO0QCQ0:=TMSFieldDesc(OOCOQOQCQ0.FFields[OC0QO0QCQ0]);
OOOQO0QCQ0:=@O0OQO0QCQ0[OC0QO0QCQ0];
OOOQO0QCQ0.O0QQQCQOQ0:=OCOQO0QCQ0.Name;
if(OQCOQOQCQ0.O0QQ0QQOQ0>=OOQ0CCCOQ0)and not(OCOQO0QCQ0.DataType in[dtMemo,dtWideMemo,dtBlob])then
OOOQO0QCQ0.O00CQCQOQ0:=OOQOCCOCQ0[OCOQO0QCQ0.DataType]
else if(OQCOQOQCQ0.O0QQ0QQOQ0>=O0Q0CCCOQ0)and not(OCOQO0QCQ0.DataType in[dtMemo,dtWideMemo,dtBlob])then
OOOQO0QCQ0.O00CQCQOQ0:=O0QOCCOCQ0[OCOQO0QCQ0.DataType]
else
OOOQO0QCQ0.O00CQCQOQ0:=OCOOCCOCQ0[OCOQO0QCQ0.DataType];
if OOOQO0QCQ0.O00CQCQOQ0=0 then
raise Exception.CreateFmt('Unknown DataType %d',[OCOQO0QCQ0.DataType]);
OOOQO0QCQ0.OO0CQCQOQ0:=OQQOCCOCQ0[OCOQO0QCQ0.DataType];
OQOQO0QCQ0:=@O0C0C0CCQ0[OOOQO0QCQ0.O00CQCQOQ0];
Assert(OQOQO0QCQ0.OQ0CCQCOQ0<>'');
if OCOQO0QCQ0.Required then
O0QQO0QCQ0:=4
else
O0QQO0QCQ0:=$8005;
OOOQOQQCQ0(OCCOC000Q0).O0CCOOQCQ0(OCOQO0QCQ0.IsTimestamp,O0QQO0QCQ0,OCOQO0QCQ0.DataType,OOOQO0QCQ0.O00CQCQOQ0,OQOQO0QCQ0,OCQQO0QCQ0,'',OCOQO0QCQ0.Length,OCOQO0QCQ0.Scale,OCOQO0QCQ0.Size);
end;
OCQOQOQCQ0.OC00C0COQ0(OCCCO0CCQ0);
OCCOQOQCQ0:=nil;
OOCOQOQCQ0.AllocRecBuf(OCCOQOQCQ0);
OQQQO0QCQ0:=OOCOQOQCQ0.CurrentItem;
try
OOCOQOQCQ0.SetToBegin;
while True do begin
OOCOQOQCQ0.GetNextRecord(OCCOQOQCQ0);
if OOCOQOQCQ0.Eof then
Break;
OCQOQOQCQ0.OC00C0COQ0(OQCCO0CCQ0);
for OC0QO0QCQ0:=0 to OOCOQOQCQ0.FFields.Count-1 do begin
OCOQO0QCQ0:=TMSFieldDesc(OOCOQOQCQ0.FFields[OC0QO0QCQ0]);
OOCOQOQCQ0.GetFieldAsVariant(OCOQO0QCQ0,OCCOQOQCQ0,OOQQO0QCQ0);
OQOQO0QCQ0:=@O0C0C0CCQ0[O0OQO0QCQ0[OC0QO0QCQ0].O00CQCQOQ0];
Assert(OQOQO0QCQ0.OQ0CCQCOQ0<>'');
case OQOQO0QCQ0.OOOCCQCOQ0 of
O0OQ0QCOQ0:
OOOQOQQCQ0(OCCOC000Q0).OOQOQQQCQ0(O0OQO0QCQ0[OC0QO0QCQ0].O00CQCQOQ0,O0OQO0QCQ0[OC0QO0QCQ0].OO0CQCQOQ0,O0OQO0QCQ0[OC0QO0QCQ0].OOCQQCQOQ0,OOQQO0QCQ0);
OOOQ0QCOQ0:
OOOQOQQCQ0(OCCOC000Q0).O00COOQCQ0(O0OQO0QCQ0[OC0QO0QCQ0].O00CQCQOQ0,False,OOQQO0QCQ0);
OQOQ0QCOQ0:
OOOQOQQCQ0(OCCOC000Q0).OCOCOOQCQ0(O0OQO0QCQ0[OC0QO0QCQ0].O00CQCQOQ0,False,OOQQO0QCQ0);
end;
end;
end;
finally
OOCOQOQCQ0.FreeRecBuf(OCCOQOQCQ0);
OOCOQOQCQ0.CurrentItem:=OQQQO0QCQ0;
end;
OCQOQOQCQ0.OC00C0COQ0(OCCCO0CCQ0);
end;
procedure TTDS7Command.O00OQ000Q0;
var
O0OQ00QCQ0:TTDS7Command;
OOOQ00QCQ0:OCQCCCOCQ0;
OQOQ00QCQ0,OCOQ00QCQ0,O0QQ00QCQ0,
OOQQ00QCQ0,
OQQQ00QCQ0:Integer;
OCQQ00QCQ0:Word;
begin
inherited;
OCCOC000Q0.OOC00OQOQ0:=Component;
OCCOC000Q0.OQC00OQOQ0:=GetParserClass;
OCCOC000Q0.O00O0OQOQ0:=SQL;
OCCOC000Q0.O0QO0OQOQ0:=FNonBlocking;
OCCOC000Q0.OOQO0OQOQ0:=FRequestResultSet;
OCCOC000Q0.O0C00OQOQ0:=TMSSQLConnection(FConnection).Options.AutoTranslate;
OCCOC000Q0.OCC00OQOQ0.OC0Q0QQOQ0:=FCommandTimeout;
OOOQOQQCQ0(OCCOC000Q0).OCC0OOQCQ0:=OQQ0O0QCQ0 and not OOQ0O0QCQ0;
if OOOQOQQCQ0(OCCOC000Q0).OCC0OOQCQ0 then begin
OOOQOQQCQ0(OCCOC000Q0).O00OOOQCQ0:=OCQ0O0QCQ0;
OOOQOQQCQ0(OCCOC000Q0).OO0OOOQCQ0:=Format('service=%s',[O0C0O0QCQ0]);
OOOQOQQCQ0(OCCOC000Q0).OQ0OOOQCQ0:=OOC0O0QCQ0;
end;
O0OQ00QCQ0:=TTDS7Command(BatchOwner);
if O0OQ00QCQ0=nil then begin
OOQQ00QCQ0:=0;
OQQQ00QCQ0:=FParams.Count
end
else begin
OOQQ00QCQ0:=O0OQ00QCQ0.FParamsInfo.Count;
OQQQ00QCQ0:=OOQQ00QCQ0*FBatchIters;
end;
SetLength(OCCOC000Q0.OC00QQQOQ0,OQQQ00QCQ0);
for OQOQ00QCQ0:=0 to OQQQ00QCQ0-1 do begin
if O0OQ00QCQ0=nil then begin
OCOQ00QCQ0:=0;
if(ParamsInfo.Count>0)and(ParamsInfo.Items[OQOQ00QCQ0].ParamRef<>nil)then begin
OOOQ00QCQ0:=OCQCCCOCQ0(ParamsInfo.Items[OQOQ00QCQ0].ParamRef);
O0QQ00QCQ0:=FParams.IndexOf(OOOQ00QCQ0);
end
else begin
O0QQ00QCQ0:=OQOQ00QCQ0;
OOOQ00QCQ0:=OCQCCCOCQ0(FParams[O0QQ00QCQ0]);
end;
end
else begin
O0QQ00QCQ0:=OQOQ00QCQ0 mod OOQQ00QCQ0;
OCOQ00QCQ0:=FBatchOffset+OQOQ00QCQ0 div OOQQ00QCQ0;
OOOQ00QCQ0:=OCQCCCOCQ0(O0OQ00QCQ0.FParamsInfo[O0QQ00QCQ0].ParamRef);
end;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OOQQQCQOQ0:=O0QQ00QCQ0;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OQQQQCQOQ0:=OCOQ00QCQ0;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O0QQQCQOQ0:=OOOQ00QCQ0.GetName;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OCQQQCQOQ0:=OOOQ00QCQ0.GetParamType;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OQCQQCQOQ0:=OOOQ00QCQ0.GetIsBound;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O0CQQCQOQ0:=OOOQ00QCQ0.GetSize;
if not FUseDescribeParams and(OOOQ00QCQ0.GetDataType in[dtSQLTimeStamp,dtSQLTimeStampOffset])then
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OOCQQCQOQ0:=3
else
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OOCQQCQOQ0:=OOOQ00QCQ0.GetScale;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OCCQQCQOQ0:=OOOQ00QCQ0.OQCCCCOCQ0;
if GetProtocol.O0QQ0QQOQ0>=OOQ0CCCOQ0 then begin
if OOOQ00QCQ0.GetDataType=dtDateTime then begin
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O00CQCQOQ0:=OOQOCCOCQ0[dtSQLTimeStamp];
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OOCQQCQOQ0:=7;
end
else
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O00CQCQOQ0:=OOQOCCOCQ0[OOOQ00QCQ0.GetDataType];
end
else if GetProtocol.O0QQ0QQOQ0>=O0Q0CCCOQ0 then
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O00CQCQOQ0:=O0QOCCOCQ0[OOOQ00QCQ0.GetDataType]
else
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O00CQCQOQ0:=OCOOCCOCQ0[OOOQ00QCQ0.GetDataType];
OCQQ00QCQ0:=OOOQ00QCQ0.GetDataType;
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OO0CQCQOQ0:=OQQOCCOCQ0[OCQQ00QCQ0];
if(OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O00CQCQOQ0=O0QQOQCOQ0)then
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].OO0CQCQOQ0:=OOOQ00QCQ0.OOCCCCOCQ0;
if OOOQ00QCQ0.GetDataType=dtTable then
if O0OQ00QCQ0=nil then
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O0OCQCQOQ0:=OOQOQOQCQ0
else
OCCOC000Q0.OC00QQQOQ0[OQOQ00QCQ0].O0OCQCQOQ0:=O0OQ00QCQ0.OOQOQOQCQ0;
end;
if O0OQ00QCQ0=nil then begin
OCCOC000Q0.O0CO0OQOQ0:=OQOQQ000Q0;
OCCOC000Q0.OOCO0OQOQ0:=OQQQQ000Q0;
OCCOC000Q0.OQCO0OQOQ0:=OQCQQ000Q0;
OCCOC000Q0.OCCO0OQOQ0:=OQ0CQ000Q0;
end
else begin
OCCOC000Q0.O0CO0OQOQ0:=O0OQ00QCQ0.OQOQQ000Q0;
OCCOC000Q0.OOCO0OQOQ0:=O0OQ00QCQ0.OQQQQ000Q0;
OCCOC000Q0.OQCO0OQOQ0:=O0OQ00QCQ0.OQCQQ000Q0;
OCCOC000Q0.OCCO0OQOQ0:=O0OQ00QCQ0.OQ0CQ000Q0;
end;
end;
procedure TTDS7Command.OO0OO0QCQ0;
const
OQ0OO0QCQ0:array[ctDefaultResultSet..ctBaseTable]of integer=(
OOCOCOCCQ0,
OO0QQOCCQ0 or OQOQQOCCQ0 or OQQQQOCCQ0,
OQCOCOCCQ0 or OQOQQOCCQ0 or OCOQQOCCQ0,
OCCOCOCCQ0 or OQOQQOCCQ0 or OCOQQOCCQ0 or O0QQQOCCQ0,
OCCOCOCCQ0 or OQOQQOCCQ0 or OCOQQOCCQ0 or O0QQQOCCQ0
);
OC0OO0QCQ0:array[ctDefaultResultSet..ctBaseTable]of integer=(
0,
OQCOCOCCQ0 or OQ0CQOCCQ0 or OC0CQOCCQ0,
OQCQQOCCQ0 or OO0CQOCCQ0 or OQ0CQOCCQ0 or OOOCQOCCQ0,
OQCQQOCCQ0 or OQ0CQOCCQ0 or OOOCQOCCQ0,
OQCQQOCCQ0 or OQ0CQOCCQ0 or OOOCQOCCQ0
);
var
O0OOO0QCQ0:integer;
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdOutput,dtInteger,Unassigned);
OOOCQ000Q0(2,'Statement',pdInput,dtWideString,FSQL);
OOOCQ000Q0(3,'scrollopt',pdOutput,dtInteger,OQ0OO0QCQ0[OQC0O0QCQ0]);
OOOCQ000Q0(4,'ccopt',pdOutput,dtInteger,OC0OO0QCQ0[OQC0O0QCQ0]);
OOOCQ000Q0(5,'rowcount',pdOutput,dtInteger,Unassigned);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.InternalOpenCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
if OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OQQ0COCCQ0,OOQOCOCCQ0)=0 then begin
O00OO0QCQ0:=True;
OCC0O0QCQ0:=Params.FindParam('CursorHandle').Value;
O0OOO0QCQ0:=Params.FindParam('scrollopt').Value;
if(O0OOO0QCQ0 and OQCOCOCCQ0)<>0 then
OQC0O0QCQ0:=ctKeyset
else
if(O0OOO0QCQ0 and OCCOCOCCQ0)<>0 then
OQC0O0QCQ0:=ctDynamic
else
if(O0OOO0QCQ0 and OO0QQOCCQ0)<>0 then
OQC0O0QCQ0:=ctStatic
else
raise Exception.CreateFmt('Unknown cursor type %.2X',[O0OOO0QCQ0]);
OCCOC000Q0.OC0O0OQOQ0:={$IFDEF FPC}Integer{$ENDIF}(Params.FindParam('rowcount').Value);
end;
finally
O0CCQ000Q0(0,6);
end;
end;
procedure TTDS7Command.OOOOO0QCQ0;
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.InternalCloseCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OO0OCOCCQ0,OOQOCOCCQ0);
O00OO0QCQ0:=False;
finally
O0CCQ000Q0(0,2);
end;
end;
procedure TTDS7Command.OQOOO0QCQ0(OCOOO0QCQ0,O0QOO0QCQ0:Integer);
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'Code',pdInput,dtInteger,OCOOO0QCQ0);
OOOCQ000Q0(3,'Value',pdInput,dtInteger,O0QOO0QCQ0);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.InternalCursorSetOption: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(O00OCOCCQ0,OOQOCOCCQ0);
finally
O0CCQ000Q0(0,4);
end;
end;
function TTDS7Command.OOQOO0QCQ0(OQQOO0QCQ0:Integer;var OCQOO0QCQ0,O0COO0QCQ0:Integer):Integer;
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'FetchType',pdInput,dtInteger,OQQOO0QCQ0);
if(OQQOO0QCQ0 and OQCCQOCCQ0)<>0 then
OOOCQ000Q0(3,'RowNum',pdInputOutput,dtInteger,OCQOO0QCQ0)
else
OOOCQ000Q0(3,'RowNum',pdInput,dtInteger,OCQOO0QCQ0);
if(OQQOO0QCQ0 and OQCCQOCCQ0)<>0 then
OOOCQ000Q0(4,'NRows',pdInputOutput,dtInteger,O0COO0QCQ0)
else
OOOCQ000Q0(4,'NRows',pdInput,dtInteger,O0COO0QCQ0);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.FetchCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
Result:=OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OCC0COCCQ0,OCQOCOCCQ0);
if(Result=0)and((OQQOO0QCQ0 and OQCCQOCCQ0)<>0)then begin
OCQOO0QCQ0:=Params.FindParam('RowNum').Value;
O0COO0QCQ0:=Params.FindParam('NRows').Value;
end;
if Result=OOO0COCCQ0 then
raise Exception.Create(O0QOCOCCQ0[OCC0COCCQ0]+' exception '+O0Q0COCCQ0[Result]);
finally
O0CCQ000Q0(0,5);
end;
end;
procedure TTDS7Command.OO0Q00QCQ0;
var
OQ0Q00QCQ0:Integer;
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'optype',pdInput,dtInteger,O0Q0QOCCQ0 or O0O0QOCCQ0);
OOOCQ000Q0(3,'RowNum',pdInput,dtInteger,1);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.DeleteCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OQ0Q00QCQ0:=OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OOQ0COCCQ0,OCQOCOCCQ0);
if OQ0Q00QCQ0=OOO0COCCQ0 then
raise Exception.Create(O0QOCOCCQ0[OOQ0COCCQ0]+' exception '+O0Q0COCCQ0[OQ0Q00QCQ0]);
finally
O0CCQ000Q0(0,4);
end;
end;
procedure TTDS7Command.OOCOO0QCQ0(const OQCOO0QCQ0:string);
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'optype',pdInput,dtInteger,OOO0QOCCQ0);
OOOCQ000Q0(3,'rownum',pdInput,dtInteger,0);
OOOCQ000Q0(4,'tablename',pdInput,dtWideString,WideString(OQCOO0QCQ0));
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.InsertCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OOQ0COCCQ0,OCQOCOCCQ0,5);
finally
O0CCQ000Q0(0,5);
end;
end;
procedure TTDS7Command.OCCOO0QCQ0(const O00Q00QCQ0:string);
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'optype',pdInput,dtInteger,O0Q0QOCCQ0 or OC00QOCCQ0);
OOOCQ000Q0(3,'rownum',pdInput,dtInteger,1);
OOOCQ000Q0(4,'tablename',pdInput,dtWideString,WideString(O00Q00QCQ0));
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.UpdateCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OOQ0COCCQ0,OCQOCOCCQ0,5);
finally
O0CCQ000Q0(0,5);
end;
end;
procedure TTDS7Command.OC0Q00QCQ0;
begin
try
OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
OOOCQ000Q0(1,'CursorHandle',pdInput,dtInteger,OCC0O0QCQ0);
OOOCQ000Q0(2,'optype',pdInput,dtInteger,O0Q0QOCCQ0 or OQO0QOCCQ0);
OOOCQ000Q0(3,'rownum',pdInput,dtInteger,1);
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
AddToLog(Format('TDS7Command.RefreshCursor: TDS7Command %p, Protocol %p, CmdContext %p',[Pointer(Self),Pointer(GetProtocol),Pointer(OCCOC000Q0)]));
{$ENDIF}
OOOQOQQCQ0(OCCOC000Q0).OOOOQQQCQ0(OOQ0COCCQ0,OCQOCOCCQ0,4);
finally
O0CCQ000Q0(0,4);
end;
end;
procedure TTDS7Command.OQQ0Q000Q0;
var
O0CQ00QCQ0,OOCQ00QCQ0:boolean;
OQCQ00QCQ0:string;
OCCQ00QCQ0:integer;
begin
if OC0QQ000Q0 then
OOOQOQQCQ0(OCCOC000Q0).OCC0QQQCQ0(OQO0O0QCQ0,OCO0O0QCQ0,O0Q0O0QCQ0)
else
if FIsSProc and(OO0QQ000Q0<>'')then
OCCOC000Q0.OCQC0OQOQ0(OO0QQ000Q0)
else
if(Params.Count>0)and OQ0QQ000Q0 then begin
OQCQ00QCQ0:=O00QOCCCQ0(FSQL,O0CQ00QCQ0);
if O0CQ00QCQ0 then begin
if OQCQ00QCQ0='' then
raise Exception.Create('spName is empty');
OCCOC000Q0.OQQO0OQOQ0:=False;
OCCOC000Q0.OCQC0OQOQ0(OQCQ00QCQ0);
end
else begin
OQCQ00QCQ0:=FSQL;
OCCOC000Q0.OQQO0OQOQ0:=True;
OCCOC000Q0.OOQC0OQOQ0(OQCOC000Q0);
end;
end
else
if O00QQ000Q0 and not OQ0QQ000Q0 then
OCCOC000Q0.OOCC0OQOQ0(O0OOCOCCQ0,OOCOC000Q0)
else begin
if O00OO0QCQ0 then
OOOOO0QCQ0;
OCCOC000Q0.O00O0OQOQ0:=FSQL;
if OQC0O0QCQ0=ctDefaultResultSet then begin
OOCQ00QCQ0:=False;
for OCCQ00QCQ0:=0 to Params.Count-1 do
if Params[OCCQ00QCQ0].GetDataType=dtUnknown then begin
OOCQ00QCQ0:=True;
Break;
end;
if OOCQ00QCQ0 then begin
O0C0Q000Q0(False);
O00OQ000Q0;
end;
OCCOC000Q0.OOQC0OQOQ0(OQCOC000Q0);
end
else begin
OO0OO0QCQ0;
OQOOO0QCQ0(3,0);
end;
end;
end;
procedure TTDS7Command.Close;
begin
if O00OO0QCQ0 then
OOOOO0QCQ0;
inherited;
end;
procedure TTDS7Command.OOQ0Q000Q0;
begin
{$IFDEF AUTOTEST}
if not OC0QQ000Q0 then
Inc(__SetCommandPropCount);
{$ENDIF}
end;
class function TTDS7Command.InternalGetBatchSQL(const OOQ000QCQ0:string;OQQ000QCQ0:TParsedSQLType;
OCQ000QCQ0:TDAParamsInfo;O0C000QCQ0:integer):string;
begin
Result:=TMSSQLCommandHelper.GetBatchSQL(OOQ000QCQ0,OQQ000QCQ0,OCQ000QCQ0,O0C000QCQ0);
end;
procedure TTDS7Command.O0CQO0QCQ0(OOCQO0QCQ0:boolean;out OQCQO0QCQ0:string);
procedure OCCQO0QCQ0(O00CO0QCQ0:TTDS7RecordSet);
var
OO0CO0QCQ0,OQ0CO0QCQ0,OC0CO0QCQ0,O0OCO0QCQ0,OOOCO0QCQ0:TFieldDesc;
OQOCO0QCQ0:OCQCCCOCQ0;
OCOCO0QCQ0:TParamDirection;
O0QCO0QCQ0,OOQCO0QCQ0,OQQCO0QCQ0:string;
OCQCO0QCQ0:word;
O0CCO0QCQ0:Boolean;
OOCCO0QCQ0:IntPtr;
OQCCO0QCQ0:word;
OCCCO0QCQ0:integer;
begin
FParams.Clear;
OO0CO0QCQ0:=O00CO0QCQ0.FieldByName('PARAMETER_TYPE');
OQ0CO0QCQ0:=O00CO0QCQ0.FieldByName('PARAMETER_HASDEFAULT');
OC0CO0QCQ0:=O00CO0QCQ0.FieldByName('DATA_TYPE');
O0OCO0QCQ0:=O00CO0QCQ0.FieldByName('CHARACTER_OCTET_LENGTH');
OOOCO0QCQ0:=O00CO0QCQ0.FieldByName('CHARACTER_MAXIMUM_LENGTH');
O00CO0QCQ0.AllocRecBuf(OOCCO0QCQ0);
try
while True do begin
O00CO0QCQ0.GetNextRecord(OOCCO0QCQ0);
if O00CO0QCQ0.Eof then
Break;
OCCCO0QCQ0:=Marshal.ReadInt32(OOCCO0QCQ0,O0OCO0QCQ0.DataOffset);
O0CCO0QCQ0:=(OCCCO0QCQ0>=O00CO0CCQ0-1)or(OCCCO0QCQ0=0);
OQCCO0QCQ0:=word(Marshal.ReadInt16(OOCCO0QCQ0,OC0CO0QCQ0.DataOffset));
O0QCO0QCQ0:=O00CO0QCQ0.GetFieldStrValue(OOCCO0QCQ0,O00CO0QCQ0.FieldByName('TYPE_NAME'));
if ConvertOLEDBTypeToInternalFormat(OQCCO0QCQ0,O0CCO0QCQ0,O0QCO0QCQ0<>'varbinary',
CalcEnableBCD,CalcEnableFMTBCD,False,{$IFDEF LITE}0,0,{$ENDIF} 0,0,
True,TMSSQLConnection(FConnection).Options.WideMemos,True,OCQCO0QCQ0,prDirect)
then begin
OCOCO0QCQ0:=ConvertOLEDBParamTypeToCR(Marshal.ReadInt16(OOCCO0QCQ0,OO0CO0QCQ0.DataOffset));
OQOCO0QCQ0:=OCQCCCOCQ0.Create;
FParams.Add(OQOCO0QCQ0);
OQOCO0QCQ0.SetParamType(OCOCO0QCQ0);
OQOCO0QCQ0.SetDataType(OCQCO0QCQ0);
if OCQCO0QCQ0=dtTable then
OQOCO0QCQ0.TableTypeName:=O0QCO0QCQ0;
OOQCO0QCQ0:=GetParamNameWODog(O00CO0QCQ0.GetFieldStrValue(OOCCO0QCQ0,O00CO0QCQ0.FieldByName('PARAMETER_NAME')));
OQOCO0QCQ0.SetName(OOQCO0QCQ0);
if Marshal.ReadByte(OOCCO0QCQ0,OQ0CO0QCQ0.DataOffset)<>0 then begin
OQQCO0QCQ0:=O00CO0QCQ0.GetFieldStrValue(OOCCO0QCQ0,O00CO0QCQ0.FieldByName('PARAMETER_HASDEFAULT'));
OQOCO0QCQ0.SetValue(OQQCO0QCQ0);
end;
if O0OQQ000Q0(OQOCO0QCQ0)and not TTDS7Connector(FConnection.GetConnector).OOCCQOQCQ0 then
OQOCO0QCQ0.SetParamType(pdInput);
OQOCO0QCQ0.SetSize(Marshal.ReadInt32(OOCCO0QCQ0,OOOCO0QCQ0.DataOffset));
end
else
DatabaseErrorFmt(SBadFieldType,[O0QCO0QCQ0,OQCCO0QCQ0]);
end;
finally
if OOCCO0QCQ0<>nil then
O00CO0QCQ0.FreeRecBuf(OOCCO0QCQ0);
end;
end;
var
O000O0QCQ0,OO00O0QCQ0,OQ00O0QCQ0,OC00O0QCQ0:string;
O0O0O0QCQ0:Integer;
OOO0O0QCQ0:TTDS7RecordSet;
begin
OQ0OQ000Q0(OOCQO0QCQ0,OQCQO0QCQ0,O000O0QCQ0,OO00O0QCQ0,OQ00O0QCQ0,O0O0O0QCQ0);
OOO0O0QCQ0:=TTDS7RecordSet.Create;
try
OOO0O0QCQ0.SetConnection(FConnection);
OOO0O0QCQ0.SetProp(prFetchAll,True);
if(Length(OQ00O0QCQ0)>1)and(OQ00O0QCQ0[1]='#')then
OC00O0QCQ0:='tempdb'
else
OC00O0QCQ0:=O000O0QCQ0;
if TMSSQLConnection(FConnection).IsSQLAzureEdition then
OOO0O0QCQ0.OOQCOCCCQ0.SQL:='sys.sp_procedure_params_managed'
else if GetProtocol.O0QQ0QQOQ0>=OOQ0CCCOQ0 then
OOO0O0QCQ0.OOQCOCCCQ0.SQL:=Format('[%s].[%s].[%s]',[OC00O0QCQ0,'sys','sp_procedure_params_100_rowset'])
else if GetProtocol.O0QQ0QQOQ0>=O0Q0CCCOQ0 then
OOO0O0QCQ0.OOQCOCCCQ0.SQL:=Format('[%s].[%s].[%s]',[OC00O0QCQ0,'sys','sp_procedure_params_rowset'])
else
OOO0O0QCQ0.OOQCOCCCQ0.SQL:=Format('[%s]..%s',[OC00O0QCQ0,'sp_procedure_params_rowset']);
TTDS7Command(OOO0O0QCQ0.GetCommand).OQO0O0QCQ0:=OO00O0QCQ0;
TTDS7Command(OOO0O0QCQ0.GetCommand).OCO0O0QCQ0:=OQ00O0QCQ0;
TTDS7Command(OOO0O0QCQ0.GetCommand).O0Q0O0QCQ0:=O0O0O0QCQ0;
TTDS7Command(OOO0O0QCQ0.GetCommand).OC0QQ000Q0:=True;
OOO0O0QCQ0.Open;
OCCQO0QCQ0(OOO0O0QCQ0);
finally
TTDS7Command(OOO0O0QCQ0.GetCommand).OC0QQ000Q0:=False;
OOO0O0QCQ0.Close;
OOO0O0QCQ0.Free;
end;
end;
function TTDS7Command.CreateProcCall(const OOC000QCQ0:string;OQC000QCQ0,OCC000QCQ0:Boolean):string;
procedure O00O00QCQ0(const OO0O00QCQ0:string;var OQ0O00QCQ0,OC0O00QCQ0:string;var O0OO00QCQ0:integer);
var
OOOO00QCQ0:string;
begin
OOOO00QCQ0:='%s';
if O0OO00QCQ0<>0 then
OOOO00QCQ0:=OOOO00QCQ0+', ';
OOOO00QCQ0:=OOOO00QCQ0+'%s%s';
OQ0O00QCQ0:=Format(OOOO00QCQ0,[OQ0O00QCQ0,'@',OO0O00QCQ0]);
OC0O00QCQ0:=Format(OOOO00QCQ0,[OC0O00QCQ0,':',OO0O00QCQ0]);
Inc(O0OO00QCQ0);
end;
var
OQOO00QCQ0,OCOO00QCQ0,O0QO00QCQ0,OOQO00QCQ0:integer;
OQQO00QCQ0,OCQO00QCQ0,O0CO00QCQ0,OOCO00QCQ0,OQCO00QCQ0,
OCCO00QCQ0,O00QC0QCQ0,OO0QC0QCQ0,OQ0QC0QCQ0:string;
OC0QC0QCQ0:string;
begin
if FConnection=nil then
DatabaseError(SConnectionNotDefined);
OO0QQ000Q0:=OOC000QCQ0;
FConnection.Connect('');
if OQC000QCQ0 then begin
O0CQO0QCQ0(False,OQQO00QCQ0);
if(FParams.Count=0)and(OQQO00QCQ0='')then begin
OOCO00QCQ0:=LowerCase(Copy(OOC000QCQ0,1,3));
if(OOCO00QCQ0='sp_')or(OOCO00QCQ0='xp_')then
O0CQO0QCQ0(True,OQQO00QCQ0);
end;
end;
OCQO00QCQ0:=OOC000QCQ0;
ParseProcName(OCQO00QCQ0,O0CO00QCQ0);
OQCO00QCQ0:=SQLInfo.NormalizeName(OCQO00QCQ0,'[',']');
if O0CO00QCQ0<>'' then
OQCO00QCQ0:=OQCO00QCQ0+';'+O0CO00QCQ0;
O0QO00QCQ0:=0;
OOQO00QCQ0:=0;
OCCO00QCQ0:='';
O00QC0QCQ0:='';
OO0QC0QCQ0:='';
OQ0QC0QCQ0:='';
OCCOC000Q0.OO00QQQOQ0:=nil;
OCOO00QCQ0:=1;
for OQOO00QCQ0:=0 to FParams.Count-1 do begin
OC0QC0QCQ0:=FParams[OQOO00QCQ0].GetName;
if OC0QC0QCQ0='' then begin
OC0QC0QCQ0:=Format('p%d',[OCOO00QCQ0]);
Inc(OCOO00QCQ0);
end;
if FParams[OQOO00QCQ0].GetParamType=pdResult then
O00O00QCQ0(OC0QC0QCQ0,O00QC0QCQ0,OQ0QC0QCQ0,OOQO00QCQ0)
else
O00O00QCQ0(OC0QC0QCQ0,OCCO00QCQ0,OO0QC0QCQ0,O0QO00QCQ0);
end;
if O00QC0QCQ0<>'' then begin
FSQL:=Format('EXEC %s = %s %s',[O00QC0QCQ0,OQCO00QCQ0,OCCO00QCQ0]);
Result:=Format('EXEC %s = %s %s',[OQ0QC0QCQ0,OQCO00QCQ0,OO0QC0QCQ0]);
end
else begin
FSQL:=Format('EXEC %s %s',[OQCO00QCQ0,OCCO00QCQ0]);
Result:=Format('EXEC %s %s',[OQCO00QCQ0,OO0QC0QCQ0]);
end;
FUserSQL:=Result;
{$IFDEF LOG_PACKETS}
AddToLog('CreateProcCall FSQL "'+FSQL+'"');
AddToLog('CreateProcCall Result "'+Result+'"');
{$ENDIF}
end;
procedure TTDS7Command.O0C0Q000Q0(O00C00QCQ0:boolean);
procedure OO0C00QCQ0(O00CO0QCQ0:TTDS7RecordSet);
var
OQ0C00QCQ0,OC0C00QCQ0,OC0CO0QCQ0,O0OC00QCQ0,OOOC00QCQ0,OOOCO0QCQ0,
OQOC00QCQ0,OCOC00QCQ0:TFieldDesc;
OQOCO0QCQ0:OCQCCCOCQ0;
OCOCO0QCQ0:TParamDirection;
OOQCO0QCQ0:string;
OCQCO0QCQ0,O0QC00QCQ0:Word;
OOQC00QCQ0:Byte;
OQQC00QCQ0:integer;
OOCCO0QCQ0:IntPtr;
OCQC00QCQ0:integer;
begin
if O00C00QCQ0 then
FParams.Clear;
OQ0C00QCQ0:=O00CO0QCQ0.FieldByName('name');
OC0C00QCQ0:=O00CO0QCQ0.FieldByName('suggested_system_type_name');
OC0CO0QCQ0:=O00CO0QCQ0.FieldByName('suggested_tds_type_id');
O0OC00QCQ0:=O00CO0QCQ0.FieldByName('suggested_is_input');
OOOC00QCQ0:=O00CO0QCQ0.FieldByName('suggested_is_output');
OOOCO0QCQ0:=O00CO0QCQ0.FieldByName('suggested_tds_length');
OQOC00QCQ0:=O00CO0QCQ0.FieldByName('suggested_scale');
OCOC00QCQ0:=O00CO0QCQ0.FieldByName('suggested_tds_length');
OCQC00QCQ0:=0;
OOCCO0QCQ0:=nil;
O00CO0QCQ0.AllocRecBuf(OOCCO0QCQ0);
try
while True do begin
O00CO0QCQ0.GetNextRecord(OOCCO0QCQ0);
if O00CO0QCQ0.Eof then
Break;
O0QC00QCQ0:=Word(Marshal.ReadInt16(OOCCO0QCQ0,OC0CO0QCQ0.DataOffset));
OCQCO0QCQ0:=OQOOCCOCQ0[O0QC00QCQ0];
OOQC00QCQ0:=Marshal.ReadByte(OOCCO0QCQ0,OQOC00QCQ0.DataOffset);
OQQC00QCQ0:=Marshal.ReadInt32(OOCCO0QCQ0,OCOC00QCQ0.DataOffset);
if Marshal.ReadInt16(OOCCO0QCQ0,OOOC00QCQ0.DataOffset)<>0 then
OCOCO0QCQ0:=pdInputOutput
else
if Marshal.ReadInt16(OOCCO0QCQ0,O0OC00QCQ0.DataOffset)<>0 then
OCOCO0QCQ0:=pdInput
else
OCOCO0QCQ0:=pdUnknown;
if O00C00QCQ0 then begin
OQOCO0QCQ0:=OCQCCCOCQ0.Create;
FParams.Add(OQOCO0QCQ0);
end
else begin
if OCQC00QCQ0>=FParams.Count then
raise Exception.CreateFmt('Params.Count %d <> Fields.Count %d',[FParams.Count,OCQC00QCQ0]);
OQOCO0QCQ0:=OCQCCCOCQ0(FParams[OCQC00QCQ0]);
Inc(OCQC00QCQ0);
if OQOCO0QCQ0.GetDataType<>dtUnknown then
Continue;
end;
OQOCO0QCQ0.SetParamType(OCOCO0QCQ0);
OQOCO0QCQ0.SetDataType(OCQCO0QCQ0);
OQOCO0QCQ0.SetScale(OOQC00QCQ0);
if(O0QC00QCQ0=O0QQOQCOQ0)and(OQQC00QCQ0=4)then
OQOCO0QCQ0.OOCCCCOCQ0:=OC00OQCOQ0;
if OCQCO0QCQ0=dtTable then
OQOCO0QCQ0.TableTypeName:=O00CO0QCQ0.GetFieldStrValue(OOCCO0QCQ0,OC0C00QCQ0);
if O0OQQ000Q0(OQOCO0QCQ0)then begin
if not TTDS7Connector(FConnection.GetConnector).OOCCQOQCQ0 then
OQOCO0QCQ0.SetParamType(pdInput);
end
else
OQOCO0QCQ0.SetSize(Marshal.ReadInt32(OOCCO0QCQ0,OOOCO0QCQ0.DataOffset));
if O00C00QCQ0 then begin
OOQCO0QCQ0:=GetParamNameWODog(O00CO0QCQ0.GetFieldStrValue(OOCCO0QCQ0,OQ0C00QCQ0));
OQOCO0QCQ0.SetName(OOQCO0QCQ0);
end;
end;
finally
if OOCCO0QCQ0<>nil then
O00CO0QCQ0.FreeRecBuf(OOCCO0QCQ0);
end;
end;
function O0CC00QCQ0(const OOCC00QCQ0:string):string;
var
OQCC00QCQ0:TSQLParser;
OCCC00QCQ0:Integer;
O00000QCQ0:string;
OO0000QCQ0,OQ0000QCQ0:Boolean;
begin
Assert(OOCC00QCQ0<>'');
Result:='';
OQCC00QCQ0:=GetParserClass.Create(OOCC00QCQ0);
try
OO0000QCQ0:=False;
OQ0000QCQ0:=False;
OQCC00QCQ0.OmitBlank:=True;
OQCC00QCQ0.OmitComment:=True;
OQCC00QCQ0.QuotedString:=True;
OQCC00QCQ0.DecSeparator:='.';
OQCC00QCQ0.ToBegin;
repeat
OCCC00QCQ0:=OQCC00QCQ0.GetNext(O00000QCQ0);
case OCCC00QCQ0 of
lxINSERT:
OQ0000QCQ0:=True;
lxINTO:
if OQ0000QCQ0 then
OO0000QCQ0:=True;
lxFROM,lxUPDATE:
OO0000QCQ0:=True;
lcIdent:
if OO0000QCQ0 then begin
Result:=O00000QCQ0;
Break;
end;
else
if OO0000QCQ0 then
Break;
end;
until(OCCC00QCQ0=lcEnd);
finally
OQCC00QCQ0.Free;
end;
end;
var
OC0000QCQ0:integer;
O0O000QCQ0:string;
OOO000QCQ0:TTDS7RecordSet;
OQO000QCQ0:OCQCCCOCQ0;
OCO000QCQ0:O0Q0QQCOQ0;
begin
if FParams.Count=0 then
Exit;
OOO000QCQ0:=TTDS7RecordSet.Create;
try
OOO000QCQ0.SetConnection(FConnection);
if TMSSQLConnection(FConnection).IsSQLAzureEdition or(GetProtocol.O0QQ0QQOQ0>=OCQ0CCCOQ0)then begin
OOO000QCQ0.SetProp(prFetchAll,True);
OOO000QCQ0.OOQCOCCCQ0.SQL:='EXEC sys.sp_describe_undeclared_parameters';
TTDS7Command(OOO000QCQ0.GetCommand).FIsSProc:=True;
O00OQ000Q0;
{$IFDEF LOG_PACKETS}
{$ENDIF}
TTDS7Command(OOO000QCQ0.GetCommand).OOOCQ000Q0(0,'tsql',pdInput,dtWideString,OCCOC000Q0.O0000OQOQ0(FSQL));
OOO000QCQ0.Open;
OO0C00QCQ0(OOO000QCQ0);
end
else begin
O0O000QCQ0:='SELECT ';
for OC0000QCQ0:=0 to FParams.Count-1 do begin
OQO000QCQ0:=OCQCCCOCQ0(FParams[OC0000QCQ0]);
O0O000QCQ0:=O0O000QCQ0+OQO000QCQ0.GetName;
if OC0000QCQ0<(FParams.Count-1)then
O0O000QCQ0:=O0O000QCQ0+',';
end;
O0O000QCQ0:=O0O000QCQ0+' FROM '+O0CC00QCQ0(FSQL)+' WHERE 1=2';
OOO000QCQ0.OOQCOCCCQ0.SQL:='SET FMTONLY ON '+O0O000QCQ0+' SET FMTONLY OFF';
OOO000QCQ0.OOQCOCCCQ0.Execute;
if FParams.Count>Length(OOO000QCQ0.OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)then
raise Exception.CreateFmt('Params.Count %d <> Fields.Count %d',[FParams.Count,Length(OOO000QCQ0.OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)]);
for OC0000QCQ0:=0 to FParams.Count-1 do begin
OQO000QCQ0:=OCQCCCOCQ0(FParams[OC0000QCQ0]);
OCO000QCQ0:=OOO000QCQ0.OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0[OC0000QCQ0];
OQO000QCQ0.FDataType:=OQOOCCOCQ0[OCO000QCQ0.OC0OQQCOQ0];
if OCO000QCQ0.O0C0QQCOQ0<>0 then
OQO000QCQ0.FSize:=OCO000QCQ0.O0C0QQCOQ0
else
if not(OQO000QCQ0.FDataType in[dtBoolean,
dtInt8,dtInt16,dtInt32,dtInt64,dtUInt8,dtUInt16,dtUInt32,dtUInt64,
dtMemo,dtWideMemo,dtXML,dtBlob])then
OQO000QCQ0.FSize:=Word(OCO000QCQ0.OCQ0QQCOQ0);
OQO000QCQ0.FScale:=OCO000QCQ0.OOC0QQCOQ0;
if OCO000QCQ0.OC0OQQCOQ0 in[OO0QOQCOQ0,O0QQOQCOQ0]then
OQO000QCQ0.FScale:=3;
if OQO000QCQ0.FDataType in[dtWideString,dtExtWideString]then
OQO000QCQ0.FSize:=OQO000QCQ0.FSize shr 1;
end;
end;
finally
OOO000QCQ0.Free;
end;
end;
function TTDS7Command.OQC0Q000Q0:integer;
begin
Result:=OC0OCOCCQ0;
end;
function TTDS7Command.OCC0Q000Q0:integer;
begin
Result:=OCOOCOCCQ0;
end;
function TTDS7Command.GetProp(O0OQC0QCQ0:Integer;out OOOQC0QCQ0:variant):Boolean;
begin
Result:=True;
case O0OQC0QCQ0 of
prDelayedSubsciption:
OOOQC0QCQ0:=OOQ0O0QCQ0;
else
Result:=inherited GetProp(O0OQC0QCQ0,OOOQC0QCQ0);
end;
end;
function TTDS7Command.SetProp(OQOQC0QCQ0:Integer;const OCOQC0QCQ0:variant):Boolean;
begin
Result:=True;
case OQOQC0QCQ0 of
prNotification:
OQQ0O0QCQ0:=OCOQC0QCQ0;
prNotificationMessage:
OCQ0O0QCQ0:=OCOQC0QCQ0;
prNotificationService:
O0C0O0QCQ0:=OCOQC0QCQ0;
prNotificationTimeout:
OOC0O0QCQ0:=OCOQC0QCQ0;
prOutputStream:
raise Exception.CreateFmt(SPropertyNotImplemented,['OutputStream']);
else
Result:=inherited SetProp(OQOQC0QCQ0,OCOQC0QCQ0);
end;
end;
constructor TTDS7RecordSet.Create;
begin
inherited;
O0QQC0QCQ0:=True;
OOQQC0QCQ0:=True;
OQQQC0QCQ0:=ctDefaultResultSet;
end;
function TTDS7RecordSet.GetFieldDescType:TFieldDescClass;
begin
Result:=TMSFieldDesc;
end;
function TTDS7RecordSet.IsWideField(O0QCC0QCQ0:TFieldDesc):boolean;
begin
Result:=(O0QCC0QCQ0.SubDataType and sdtWide)<>0;
end;
function TTDS7RecordSet.O0CQC0QCQ0:boolean;
begin
if(OOQCOCCCQ0<>nil)and(OOQCOCCCQ0.GetConnection<>nil)then
Result:=TMSSQLConnection(OOQCOCCCQ0.GetConnection).Options.WideMemos
else
Result:=OOQQC0QCQ0;
end;
function TTDS7RecordSet.OC0CC0QCQ0:TMSCursorType;
begin
if(OQQQC0QCQ0=ctBaseTable)and not FReadOnly and FCursorUpdate then
Result:=ctDynamic
else
Result:=OOQCOCCCQ0.OQC0O0QCQ0;
end;
function TTDS7RecordSet.NeedGetRecordAfterGotoBookmark:boolean;
begin
Result:=IsServerCursor;
end;
function TTDS7RecordSet.IsServerCursor:boolean;
begin
Result:=OC0CC0QCQ0 in ServerCursorTypes;
end;
function TTDS7RecordSet.IsStaticCursor:boolean;
begin
Result:=OC0CC0QCQ0 in[ctStatic,ctKeyset];
end;
function TTDS7RecordSet.IsDynamicCursor:boolean;
begin
Result:=OC0CC0QCQ0=ctDynamic;
end;
function TTDS7RecordSet.GetCommandClass:TSqlCommandClass;
begin
Result:=TTDS7Command;
end;
procedure TTDS7RecordSet.SetCommand(OQQCC0QCQ0:TCRCommand);
begin
inherited;
OOQCOCCCQ0:=TTDS7Command(OQQCC0QCQ0);
end;
procedure TTDS7RecordSet.OOO0OCCCQ0;
begin
{$IFDEF AUTOTEST}
if not OOQCOCCCQ0.OC0QQ000Q0 then
Inc(__SetRecordSetCommandPropCount);
{$ENDIF}
OOQCOCCCQ0.OQC0O0QCQ0:=OQQQC0QCQ0;
OOQCOCCCQ0.OOQ0O0QCQ0:=not(OQQQC0QCQ0 in ServerCursorTypes);
if OQQQC0QCQ0 in ServerCursorTypes then begin
if DisconnectedMode then
DatabaseError(SDMandServerCursors);
FFetchRows:=1;
FFetchAll:=False;
end;
end;
procedure TTDS7RecordSet.InitFetchedBlock(OOO0C0QCQ0:PBlockHeader;OQO0C0QCQ0:Integer;OCO0C0QCQ0:boolean);
begin
case OOQCOCCCQ0.OQC0O0QCQ0 of
ctDefaultResultSet:
inherited;
ctStatic,ctKeySet:begin
FBookmarkSize:=SizeOf(Integer);
FBookmarkValue:=OCQQC0QCQ0;
LastItem.Order:=FBookmarkValue;
end;
ctDynamic:
if OQQQC0QCQ0=ctBaseTable then
inherited
else
LastItem.Order:=0;
end;
end;
procedure TTDS7RecordSet.ProcessNoResult(O0Q0C0QCQ0:Boolean);
begin
case OOQCOCCCQ0.OQC0O0QCQ0 of
ctDefaultResultSet:
inherited;
end;
end;
function TTDS7RecordSet.OOQO0CCCQ0:boolean;
begin
Result:=((OC0CC0QCQ0 in ServerCursorTypes)and not OOQCOCCCQ0.O00OO0QCQ0)or
((OQQQC0QCQ0=ctKeySet)and not FCursorUpdate and(FBookmarkValue=-1));
end;
function TTDS7RecordSet.OQCO0CCCQ0(O0CCC0QCQ0:integer;OOCCC0QCQ0:boolean):NativeUInt;
var
OQCCC0QCQ0,OCCCC0QCQ0,O000C0QCQ0:integer;
OO00C0QCQ0:boolean;
OQ00C0QCQ0:TMSFieldDesc;
OC00C0QCQ0:integer;
begin
if not(OC0CC0QCQ0 in ServerCursorTypes)then begin
Result:=inherited OQCO0CCCQ0(O0CCC0QCQ0,OOCCC0QCQ0);
Exit;
end;
try
case OC0CC0QCQ0 of
ctKeyset,ctStatic:begin
OCCCC0QCQ0:=OCQCQOCCQ0;
if not FFetchFromBookmark then begin
if OOCCC0QCQ0 then
Dec(OCQQC0QCQ0)
else
Inc(OCQQC0QCQ0);
end
else begin
if FBookmarkSize=SizeOf(Byte)then begin
case FBookmarkValue of
DBBMK_FIRST:
OCQQC0QCQ0:=1;
DBBMK_LAST:
OOQCOCCCQ0.OOQOO0QCQ0(OQCCQOCCQ0,OQCCC0QCQ0,OCQQC0QCQ0);
end;
end
else
OCQQC0QCQ0:=FBookmarkValue;
end;
OO00C0QCQ0:=(OCQQC0QCQ0<1);
if not OO00C0QCQ0 then begin
O000C0QCQ0:=OOQCOCCCQ0.OOQOO0QCQ0(OCCCC0QCQ0,OCQQC0QCQ0,O0CCC0QCQ0);
OO00C0QCQ0:=not OOQCOCCCQ0.OCCOC000Q0.OOOO0OQOQ0;
end
else
O000C0QCQ0:=OOO0COCCQ0;
end;
ctDynamic:begin
if not OC00OCCCQ0 then begin
OC00OCCCQ0:=True;
OCCCC0QCQ0:=O0CCQOCCQ0;
OQCCC0QCQ0:=1;
end
else
if FFetchFromBookmark then begin
OCCCC0QCQ0:=OOCCQOCCQ0;
OQCCC0QCQ0:=0;
end
else begin
if OOCCC0QCQ0 then
OCCCC0QCQ0:=OCCCQOCCQ0
else
OCCCC0QCQ0:=O0QCQOCCQ0 or O000QOCCQ0;
OQCCC0QCQ0:=0;
end;
O000C0QCQ0:=OOQCOCCCQ0.OOQOO0QCQ0(OCCCC0QCQ0,OQCCC0QCQ0,O0CCC0QCQ0);
OO00C0QCQ0:=not OOQCOCCCQ0.OCCOC000Q0.OOOO0OQOQ0;
end;
else
raise Exception.CreateFmt('Not allowed cursor type %d',[Integer(OOQCOCCCQ0.OQC0O0QCQ0)]);
end;
FFetchFromBookmark:=False;
FLastFetchBack:=OOCCC0QCQ0;
if(O000C0QCQ0<>O0O0COCCQ0)or OO00C0QCQ0 then
Result:=0
else
if(OOQCOCCCQ0.GetProtocol<>nil)and OOQCOCCCQ0.OCCOC000Q0.OQOC0OQOQ0(O0O0OCCCQ0[0])then begin
Result:=O0CCC0QCQ0;
OQ00C0QCQ0:=TMSFieldDesc(FFields[FFields.Count-1]);
if UpperCase(OQ00C0QCQ0.Name)='ROWSTAT' then begin
if not O0O0OCCCQ0[0][OQ00C0QCQ0.ActualFieldNo].OC00QQCOQ0 then begin
Assert(Length(O0O0OCCCQ0[0][OQ00C0QCQ0.ActualFieldNo].OOO0QQCOQ0)>=SizeOf(Integer));
OC00C0QCQ0:=PInteger(@O0O0OCCCQ0[0][OQ00C0QCQ0.ActualFieldNo].OOO0QQCOQ0[0])^;
if OC00C0QCQ0=OQ00QOCCQ0 then
Result:=OQCO0CCCQ0(1,OOCCC0QCQ0);
end;
end;
end
else
Result:=0;
if Result=0 then
case OC0CC0QCQ0 of
ctDynamic:begin
if OOCCC0QCQ0 then begin
FBof:=True;
OCCCC0QCQ0:=O0QCQOCCQ0 or O000QOCCQ0;
end
else begin
FEof:=True;
OCCCC0QCQ0:=OCCCQOCCQ0;
end;
OQCCC0QCQ0:=0;
OOQCOCCCQ0.OOQOO0QCQ0(OCCCC0QCQ0,OQCCC0QCQ0,O0CCC0QCQ0);
if OQCCC0QCQ0>0 then begin
if FirstItem=nil then
Result:=OQCCC0QCQ0;
end
else begin
CurrentItem:=nil;
FBof:=True;
FEof:=True;
FLastFetchBack:=False;
end;
end;
else
ProcessNoResult(OOCCC0QCQ0);
end;
except
on E:Exception do begin
ProcessFetchedException(E);
raise;
end;
end;
end;
{$IFNDEF LITE}
class function TTDS7RecordSet.OOCQC0QCQ0(OQCQC0QCQ0:Word;OCCQC0QCQ0,O00CC0QCQ0,OO0CC0QCQ0:Boolean;
OQ0CC0QCQ0:Integer):Word;
begin
case OQCQC0QCQ0 of
dtBoolean:
Result:=msBit;
dtUInt8:
Result:=msTinyint;
dtInt16,dtUInt16:
Result:=msSmallint;
dtInt32,dtUInt32:
Result:=msInt;
dtInt64,dtUInt64:
Result:=msBigint;
dtBCD,dtFmtBCD:
Result:=msDecimal;
dtSingle:
Result:=msReal;
dtFloat:
Result:=msFloat;
dtCurrency:
if OQ0CC0QCQ0<=10 then
Result:=msSmallmoney
else
Result:=msMoney;
dtDateTime:
if OQ0CC0QCQ0<8 then
Result:=msSmalldatetime
else
if OQ0CC0QCQ0<24 then
Result:=msDatetime
else
Result:=msDatetime2;
dtDate:
Result:=msDate;
dtTime:
Result:=msTime;
dtSQLTimeStamp:
Result:=msDatetime2;
dtSQLTimeStampOffset:
Result:=msDatetimeoffset;
dtString:
if O00CC0QCQ0 then
Result:=msChar
else
Result:=msVarchar;
dtWideString:
if O00CC0QCQ0 then
Result:=msNChar
else
Result:=msNVarchar;
dtExtString,dtMemo:
if OCCQC0QCQ0 then
Result:=msText
else if O00CC0QCQ0 then
Result:=msChar
else
Result:=msVarchar;
dtExtWideString,dtWideMemo:
if OCCQC0QCQ0 then
Result:=msNText
else if O00CC0QCQ0 then
Result:=msNChar
else
Result:=msNVarchar;
dtBytes,dtVarBytes,dtExtVarBytes:
if OO0CC0QCQ0 then
Result:=msTimestamp
else
Result:=msVarBinary;
dtBlob:
Result:=msImage;
dtGuid:
Result:=msUniqueIdentifier;
dtVariant:
Result:=msSqlVariant;
dtXml:
Result:=msXml;
else
Result:=0;
end;
end;
{$ENDIF}
procedure TTDS7RecordSet.O0OQCCCCQ0(var OOQ0C0QCQ0:TSqlFieldDesc;OQQ0C0QCQ0:integer);
const
OCQ0C0QCQ0='.';
var
O0C0C0QCQ0:Word;
OOC0C0QCQ0:O0Q0QQCOQ0;
OQC0C0QCQ0:TSqlTableInfo;
OCC0C0QCQ0:TStringArray;
{$IFNDEF LITE}
O00OC0QCQ0:TFetchConverter;
{$IFDEF IS_UTF8}
O0OOOC0CQ0:Word;
{$ENDIF}
{$ENDIF}
begin
Assert(Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)>OQQ0C0QCQ0);
OOC0C0QCQ0:=OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0[OQQ0C0QCQ0];
{$IFDEF LITE}
if OOC0C0QCQ0.OCOOQQCOQ0 then begin
OOQ0C0QCQ0.Free;
OOQ0C0QCQ0:=nil;
Exit;
end;
{$ENDIF}
O0C0C0QCQ0:=OQOOCCOCQ0[OOC0C0QCQ0.OC0OQQCOQ0];
OOQ0C0QCQ0.SubDataType:=0;
OOQ0C0QCQ0.FieldNo:=FFields.Count+1;
OOQ0C0QCQ0.ActualFieldNo:=OOC0C0QCQ0.O00OQQCOQ0;
OOQ0C0QCQ0.Name:=OOC0C0QCQ0.OOQ0QQCOQ0;
if OOC0C0QCQ0.OQQ0QQCOQ0<>'' then
OOQ0C0QCQ0.ActualName:=OOC0C0QCQ0.OQQ0QQCOQ0
else
OOQ0C0QCQ0.ActualName:=OOC0C0QCQ0.OOQ0QQCOQ0;
if OOC0C0QCQ0.OCC0QQCOQ0>0 then begin
OOC0C0QCQ0.OO0OQQCOQ0:=OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0[OOC0C0QCQ0.OCC0QQCOQ0-1];
if Pos(OCQ0C0QCQ0,OOC0C0QCQ0.OO0OQQCOQ0)>0 then begin
SetLength(OCC0C0QCQ0,0);
OCC0C0QCQ0:=SplitString(OOC0C0QCQ0.OO0OQQCOQ0,OCQ0C0QCQ0);
case Length(OCC0C0QCQ0)of
2:begin
OOQ0C0QCQ0.BaseSchemaName:=OCC0C0QCQ0[0];
OOQ0C0QCQ0.BaseTableName:=OCC0C0QCQ0[1];
end;
3:begin
OOQ0C0QCQ0.BaseCatalogName:=OCC0C0QCQ0[0];
OOQ0C0QCQ0.BaseSchemaName:=OCC0C0QCQ0[1];
OOQ0C0QCQ0.BaseTableName:=OCC0C0QCQ0[2];
end;
4:begin
OOQ0C0QCQ0.BaseCatalogName:=OCC0C0QCQ0[1];
OOQ0C0QCQ0.BaseSchemaName:=OCC0C0QCQ0[2];
OOQ0C0QCQ0.BaseTableName:=OCC0C0QCQ0[3];
end;
else
raise Exception.CreateFmt('Wronf amount of parameters %d in TableName %s',[Length(OCC0C0QCQ0),OOC0C0QCQ0.OO0OQQCOQ0]);
end;
end
else
OOQ0C0QCQ0.BaseTableName:=OOC0C0QCQ0.OO0OQQCOQ0;
OQC0C0QCQ0:=TSqlTableInfo(FTablesInfo.FindByName(OOC0C0QCQ0.OO0OQQCOQ0));
if OQC0C0QCQ0=nil then begin
OQC0C0QCQ0:=TSqlTableInfo(FTablesInfo.Add);
OQC0C0QCQ0.TableName:=OOC0C0QCQ0.OO0OQQCOQ0;
OQC0C0QCQ0.BaseTableName:=OOQ0C0QCQ0.BaseTableName;
OQC0C0QCQ0.TableAlias:='';
OQC0C0QCQ0.IsView:=True;
end;
OOQ0C0QCQ0.TableInfo:=OQC0C0QCQ0;
end;
if OOC0C0QCQ0.O0C0QQCOQ0<>0 then
OOQ0C0QCQ0.Length:=OOC0C0QCQ0.O0C0QQCOQ0
else
if O0C0C0QCQ0=dtCurrency then
OOQ0C0QCQ0.Length:=19
else
if not(O0C0C0QCQ0 in[dtBoolean,
dtInt8,dtInt16,dtInt32,dtInt64,dtUInt8,dtUInt16,dtUInt32,dtUInt64,
dtMemo,dtWideMemo,dtXML,dtBlob])then
OOQ0C0QCQ0.Length:=Word(OOC0C0QCQ0.OCQ0QQCOQ0);
OOQ0C0QCQ0.Scale:=OOC0C0QCQ0.OOC0QQCOQ0;
if O0C0C0QCQ0 in[dtWideString,dtExtWideString]then
OOQ0C0QCQ0.Length:=OOQ0C0QCQ0.Length shr 1;
{$IFDEF IS_UTF8}
{$IFNDEF LITE}
O0OOOC0CQ0:=OOQ0C0QCQ0.Length;
{$ENDIF}
case O0C0C0QCQ0 of
dtString,dtExtString:
OOQ0C0QCQ0.Length:=OOQ0C0QCQ0.Length*MaxSizeClientCharset;
end;
{$ENDIF}
OOQ0C0QCQ0.ReadOnly:=(((OOC0C0QCQ0.OQOOQQCOQ0 and OOCOO0CCQ0)shr 2)=0)or OOC0C0QCQ0.OOQOQQCOQ0;
OOQ0C0QCQ0.Required:=(OOC0C0QCQ0.OQOOQQCOQ0 and OQQOO0CCQ0)=0;
OOQ0C0QCQ0.IsAutoIncrement:=(OOC0C0QCQ0.OQOOQQCOQ0 and OQCOO0CCQ0)<>0;
OOQ0C0QCQ0.Hidden:=OOC0C0QCQ0.OCOOQQCOQ0;
OOQ0C0QCQ0.IsKey:=OOC0C0QCQ0.O0QOQQCOQ0;
if OOQCOCCCQ0.GetProtocol.O0QQ0QQOQ0>=O0Q0CCCOQ0 then begin
OOQ0C0QCQ0.Hidden:=OOQ0C0QCQ0.Hidden or((OOC0C0QCQ0.OQOOQQCOQ0 and OOOQ00CCQ0)<>0);
OOQ0C0QCQ0.IsKey:=OOQ0C0QCQ0.IsKey or((OOC0C0QCQ0.OQOOQQCOQ0 and OQOQ00CCQ0)<>0);
end;
OOQ0C0QCQ0.FieldDescKind:=OOC0C0QCQ0.OQ0OQQCOQ0;
OOQ0C0QCQ0.IsTimestamp:=OOC0C0QCQ0.OQQOQQCOQ0;
OOQ0C0QCQ0.Fixed:=OOC0C0QCQ0.OOOOQQCOQ0.OQO0CQCOQ0;
OOQ0C0QCQ0.DataType:=O0C0C0QCQ0;
if OOOQOQQCQ0(OOQCOCCCQ0.CmdContext).OOQQQQQCQ0 and OOC0C0QCQ0.OOOOQQCOQ0.OCCCCQCOQ0 and(OOC0C0QCQ0.OCQ0QQCOQ0=O0CCO0CCQ0)then begin
OOQ0C0QCQ0.Length:=0;
case OOC0C0QCQ0.OC0OQQCOQ0 of
OQ0O0OCCQ0:
OOQ0C0QCQ0.DataType:=dtBlob;
OC0O0OCCQ0:
OOQ0C0QCQ0.DataType:=dtMemo;
OQOO0OCCQ0:
OOQ0C0QCQ0.DataType:=dtWideMemo;
end;
end;
if(OOQ0C0QCQ0.DataType=dtWideString)and not O0QQC0QCQ0 then
OOQ0C0QCQ0.DataType:=dtString
else
if(OOQ0C0QCQ0.DataType=dtWideMemo)and not(O0QQC0QCQ0 and O0CQC0QCQ0)then
OOQ0C0QCQ0.DataType:=dtMemo;
{$IFNDEF LITE}
{$IFDEF IS_UTF8}
if O0C0C0QCQ0 in[dtString,dtExtString]then
OOQ0C0QCQ0.DBLength:=O0OOOC0CQ0
else
{$ENDIF}
OOQ0C0QCQ0.DBLength:=OOQ0C0QCQ0.Length;
OOQ0C0QCQ0.DBScale:=OOQ0C0QCQ0.Scale;
OOQ0C0QCQ0.DBType:=OOCQC0QCQ0(O0C0C0QCQ0,OOC0C0QCQ0.OOOOQQCOQ0.OOOCCQCOQ0=OQOQ0QCOQ0,OOC0C0QCQ0.OOOOQQCOQ0.OQO0CQCOQ0,OOQ0C0QCQ0.IsTimestamp,OOQ0C0QCQ0.DBLength);
O00OC0QCQ0:=GetMapFetchConverter(OOQ0C0QCQ0.Name,OOQ0C0QCQ0.DBType,OOQ0C0QCQ0.DBLength,OOQ0C0QCQ0.DBScale);
if O00OC0QCQ0<>nil then
OOQ0C0QCQ0.DataType:=O00OC0QCQ0.InternalDataType;
{$ENDIF}
if OOQ0C0QCQ0.DataType in[dtFMTBCD]then begin
if(OOQ0C0QCQ0.Length>38)or(OOQ0C0QCQ0.Length<=0)then
OOQ0C0QCQ0.Length:=38;
if OOQ0C0QCQ0.Scale>OOQ0C0QCQ0.Length then
OOQ0C0QCQ0.Scale:=OOQ0C0QCQ0.Length;
end
else
if OOQ0C0QCQ0.DataType=dtBCD then begin
if(OOQ0C0QCQ0.Length>MaxBcdPrecision)or(OOQ0C0QCQ0.Length<=0)then
OOQ0C0QCQ0.Length:=MaxBcdPrecision
else
OOQ0C0QCQ0.Length:=OOQ0C0QCQ0.Length;
if OOQ0C0QCQ0.Scale>MaxBcdScale then
OOQ0C0QCQ0.Scale:=MaxBcdScale
else
OOQ0C0QCQ0.Scale:=OOQ0C0QCQ0.Scale;
end
else
if OOQ0C0QCQ0.DataType in[dtString,dtWideString,dtVarBytes]then begin
if not FFlatBuffers and(OOQ0C0QCQ0.Length>=FlatBufferLimit)then
case OOQ0C0QCQ0.DataType of
dtString:
OOQ0C0QCQ0.DataType:=dtExtString;
dtWideString:
OOQ0C0QCQ0.DataType:=dtExtWideString;
dtVarBytes:
OOQ0C0QCQ0.DataType:=dtExtVarBytes;
end;
end;
case OOQ0C0QCQ0.DataType of
dtBCD,dtFmtBCD:begin
OOQ0C0QCQ0.SubDataType:=sdtNumeric;
if OOQCOCCCQ0.FSensibleBCDMapping and OOQCOCCCQ0.CalcEnableBCD and(OOC0C0QCQ0.O0C0QQCOQ0<=MaxBCDPrecision-MaxBCDScale+1)and(OOC0C0QCQ0.OOC0QQCOQ0<=MaxBCDScale)then
OOQ0C0QCQ0.DataType:=dtBCD
else
if OOQCOCCCQ0.CalcEnableFMTBCD then
OOQ0C0QCQ0.DataType:=dtFmtBCD
else
if not OOQCOCCCQ0.FSensibleBCDMapping and OOQCOCCCQ0.CalcEnableBCD then
OOQ0C0QCQ0.DataType:=dtBCD
else
OOQ0C0QCQ0.DataType:=dtFloat;
end;
dtString,dtWideString,dtExtString,dtExtWideString,dtMemo,dtWideMemo:begin
if OOC0C0QCQ0.OOOOQQCOQ0.OCQCCQCOQ0 then
OOQ0C0QCQ0.SubDataType:=sdtWide;
if OOC0C0QCQ0.OOOOQQCOQ0.OQO0CQCOQ0 then
OOQ0C0QCQ0.SubDataType:=OOQ0C0QCQ0.SubDataType or dtFixedChar;
end;
dtSQLTimeStamp:begin
OOQ0C0QCQ0.Length:=O0C0C0CCQ0[OOC0C0QCQ0.OC0OQQCOQ0].OCOCCQCOQ0;
if not(OOC0C0QCQ0.OC0OQQCOQ0 in[OC0OQCCOQ0,O0OOQCCOQ0,OOOOQCCOQ0])then
OOQ0C0QCQ0.Scale:=O0C0C0CCQ0[OOC0C0QCQ0.OC0OQQCOQ0].O0QCCQCOQ0;
end;
dtBlob:
if OOC0C0QCQ0.OOOOQQCOQ0.OOCCCQCOQ0 then begin
OOQ0C0QCQ0.SubDataType:=sdtMSUDT;
TMSFieldDesc(OOQ0C0QCQ0).UDTCatalogname:=OOC0C0QCQ0.O0COQQCOQ0.OQQCQQCOQ0;
TMSFieldDesc(OOQ0C0QCQ0).UDTSchemaname:=OOC0C0QCQ0.O0COQQCOQ0.OCQCQQCOQ0;
TMSFieldDesc(OOQ0C0QCQ0).UDTName:=OOC0C0QCQ0.O0COQQCOQ0.O0CCQQCOQ0;
TMSFieldDesc(OOQ0C0QCQ0).AssemblyTypename:=OOC0C0QCQ0.O0COQQCOQ0.OOCCQQCOQ0;
OOQ0C0QCQ0.Length:=Word(OOC0C0QCQ0.OCQ0QQCOQ0);
{$IFNDEF LITE}
{$IFDEF MSWINDOWS}
if(TMSFieldDesc(OOQ0C0QCQ0).UDTName<>'')and(TMSFieldDesc(OOQ0C0QCQ0).AssemblyTypename<>'')then
TMSFieldDesc(OOQ0C0QCQ0).UDTDispatcher:=TUDTDispatcher.Create;
{$ENDIF}
{$ENDIF}
end;
dtXml:begin
OOQ0C0QCQ0.XMLSchemaCollectionCatalogName:=OOC0C0QCQ0.OOCOQQCOQ0.OCCCQQCOQ0;
OOQ0C0QCQ0.XMLSchemaCollectionSchemaName:=OOC0C0QCQ0.OOCOQQCOQ0.O000QQCOQ0;
OOQ0C0QCQ0.XMLSchemaCollectionName:=OOC0C0QCQ0.OOCOQQCOQ0.OO00QQCOQ0;
OOQ0C0QCQ0.SubDataType:=sdtWide;
end;
dtGuid:
OOQ0C0QCQ0.Length:=38;
end;
OOQ0C0QCQ0.Size:=GetBufferSize(OOQ0C0QCQ0.DataType,OOQ0C0QCQ0.Length);
end;
procedure TTDS7RecordSet.InternalOpen(OCQCC0QCQ0:Boolean=False);
begin
{$IFNDEF LITE}
if IsServerCursor and(FSmartFetchState<>sfNone)then
DatabaseError(SSFAndServerCursors);
{$ENDIF}
if OQQQC0QCQ0=ctBaseTable then
if not FReadOnly and FCursorUpdate then begin
FFlatBuffers:=True;
FFetchAll:=False;
FFetchRows:=1;
end;
inherited;
end;
procedure TTDS7RecordSet.InternalAppend(OQ0OC0QCQ0:IntPtr);
var
OC0OC0QCQ0,O0OOC0QCQ0:Integer;
OOOOC0QCQ0:TFieldDesc;
OQOOC0QCQ0:Variant;
begin
if(OOQCOCCCQ0.OQC0O0QCQ0 in ServerCursorTypes)and FCursorUpdate then begin
O0OOC0QCQ0:=0;
try
for OC0OC0QCQ0:=0 to FFields.Count-1 do begin
OOOOC0QCQ0:=FFields[OC0OC0QCQ0];
GetFieldAsVariant(OOOOC0QCQ0,OQ0OC0QCQ0,OQOOC0QCQ0);
if(not VarIsEmpty(OQOOC0QCQ0)and not VarIsNull(OQOOC0QCQ0))or OOOOC0QCQ0.IsBlob then begin
OOQCOCCCQ0.OOOCQ000Q0(O0OOC0QCQ0,OOOOC0QCQ0.Name,pdInput,OOOOC0QCQ0.DataType,OQOOC0QCQ0);
Inc(O0OOC0QCQ0);
end;
end;
Assert(FFields.Count>0);
OOQCOCCCQ0.OOCOO0QCQ0(TMSFieldDesc(FFields[0]).TableInfo.TableName);
finally
for OC0OC0QCQ0:=0 to O0OOC0QCQ0-1 do
OOQCOCCCQ0.Params.Delete(0);
end;
end
else
inherited;
if OOQCOCCCQ0.OQC0O0QCQ0=ctKeySet then
Inc(FRecordCount);
end;
procedure TTDS7RecordSet.InternalUpdate(OOQOC0QCQ0:IntPtr);
var
OQQOC0QCQ0,OCQOC0QCQ0:Integer;
O0COC0QCQ0:TFieldDesc;
OOCOC0QCQ0:Variant;
OQCOC0QCQ0:IntPtr;
begin
if(OOQCOCCCQ0.OQC0O0QCQ0 in ServerCursorTypes)and FCursorUpdate then begin
OCQOC0QCQ0:=0;
AllocRecBuf(OQCOC0QCQ0);
GetRecord(OQCOC0QCQ0);
if(OOQCOCCCQ0.GetProtocol.O0QQ0QQOQ0<O0Q0CCCOQ0)and(OOQCOCCCQ0.OQC0O0QCQ0 in[ctDynamic,ctBaseTable])then
OOQCOCCCQ0.OC0Q00QCQ0;
try
for OQQOC0QCQ0:=0 to FFields.Count-1 do begin
O0COC0QCQ0:=FFields[OQQOC0QCQ0];
if O0Q0OCCCQ0(O0COC0QCQ0,OQCOC0QCQ0,OOQOC0QCQ0)or O0COC0QCQ0.IsBlob then begin
GetFieldAsVariant(O0COC0QCQ0,OOQOC0QCQ0,OOCOC0QCQ0);
OOQCOCCCQ0.OOOCQ000Q0(OCQOC0QCQ0,O0COC0QCQ0.Name,pdInput,O0COC0QCQ0.DataType,OOCOC0QCQ0);
Inc(OCQOC0QCQ0);
end;
end;
Assert(FFields.Count>0);
OOQCOCCCQ0.OCCOO0QCQ0(TMSFieldDesc(FFields[0]).TableInfo.TableName);
finally
for OQQOC0QCQ0:=0 to OCQOC0QCQ0-1 do
OOQCOCCCQ0.Params.Delete(0);
FreeRecBuf(OQCOC0QCQ0);
end;
end
else
inherited;
end;
procedure TTDS7RecordSet.InternalDelete;
begin
if(OOQCOCCCQ0.OQC0O0QCQ0 in ServerCursorTypes)and FCursorUpdate then begin
if(OOQCOCCCQ0.GetProtocol.O0QQ0QQOQ0<O0Q0CCCOQ0)and(OOQCOCCCQ0.OQC0O0QCQ0 in[ctDynamic,ctBaseTable])then
OOQCOCCCQ0.OC0Q00QCQ0;
OOQCOCCCQ0.OO0Q00QCQ0;
end
else
inherited;
end;
function TTDS7RecordSet.GetProp(O0OQC0QCQ0:integer;out OOOQC0QCQ0:variant):boolean;
begin
Result:=True;
case O0OQC0QCQ0 of
prCursorType:
OOOQC0QCQ0:=Variant(OQQQC0QCQ0);
prDisableMultipleResults:
OOOQC0QCQ0:=False;
prUniqueRecords:
OOOQC0QCQ0:={$IFNDEF LITE}True{$ELSE}False{$ENDIF};
prWideMemos:
OOOQC0QCQ0:=OOQQC0QCQ0;
prWideStrings:
OOOQC0QCQ0:=O0QQC0QCQ0;
prBulkExecuting:;
else
Result:=inherited GetProp(O0OQC0QCQ0,OOOQC0QCQ0);
end;
end;
function TTDS7RecordSet.SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;
begin
Result:=True;
case OQOQC0QCQ0 of
prBaseTableName:;
prCursorType:begin
OQQQC0QCQ0:=TMSCursorType(OCOQC0QCQ0);
if OQQQC0QCQ0 in ServerCursorTypes then begin
FFetchAll:=False;
if(FIndexFieldNames<>'')and not CachedUpdates then
DatabaseError(SLocalSortingServerCursor);
end;
end;
prDisableMultipleResults:;
prHideSystemUniqueFields:;
prNotificationMessage:
OOQCOCCCQ0.SetProp(prNotificationMessage,OCOQC0QCQ0);
prNotificationService:
OOQCOCCCQ0.SetProp(prNotificationService,OCOQC0QCQ0);
prNotificationTimeout:
OOQCOCCCQ0.SetProp(prNotificationTimeout,OCOQC0QCQ0);
prUniqueRecords:;
prWideMemos:
OOQQC0QCQ0:=OCOQC0QCQ0;
prWideStrings:
O0QQC0QCQ0:=OCOQC0QCQ0;
prRequestSQLObjects:;
prBulkExecuting:;
else
Result:=inherited SetProp(OQOQC0QCQ0,OCOQC0QCQ0);
end;
end;
function TTDSTableTypeRecordSet.IsFullReopen:boolean;
begin
Result:=False;
end;
procedure TTDSTableTypeRecordSet.CreateFieldDescs;
function O00QQ0QCQ0(OO0QQ0QCQ0:Word):Integer;
begin
case OO0QQ0QCQ0 of
dtUnknown:
Result:=SizeOf_OleVariant;
dtInt8,dtUInt8:
Result:=sizeof(Byte);
dtInt16,dtWord:
Result:=sizeof(Word);
dtInt32,dtUInt32:
Result:=sizeof(Integer);
dtSingle:
Result:=sizeof(Single);
dtFloat:
Result:=sizeof(Double);
dtCurrency:
Result:=sizeof(Double);
dtDate:
Result:=sizeof(TDateTime);
dtTime:
Result:=sizeof(TDateTime);
dtDateTime:
Result:=16;
dtSQLTimeStamp:
Result:=27;
dtSQLTimeStampOffset:
Result:=34;
dtBoolean:
Result:=sizeof(WordBool);
dtInt64,dtUInt64:
Result:=sizeof(Int64);
dtXML:
Result:=SizeOf_OleVariant;
dtGuid:
Result:=38;
dtVariant:
Result:=SizeOf_OleVariant;
dtBCD:
Result:=SizeOf_TDBNumeric;
dtFmtBCD,dtExtended:
Result:=SizeOfTBcd*2;
dtTable:
Result:=sizeof(TObject);
else
raise Exception.Create('Size should be provided by CHARACTER_OCTET_LENGTH');
end;
end;
function OQ0QQ0QCQ0(OC0QQ0QCQ0:Integer):Integer;
const
O0OQQ0QCQ0:array[O0QQCQCOQ0..OO0O0QCOQ0]of Integer=(
DBTYPE_GUID,DBTYPE_WSTR,DBTYPE_WSTR,DBTYPE_WSTR,DBTYPE_BOOL,DBTYPE_UI1,DBTYPE_I8,DBTYPE_BYTES,DBTYPE_BYTES,DBTYPE_BYTES,DBTYPE_STR,
DBTYPE_EMPTY,
DBTYPE_STR,DBTYPE_NUMERIC,DBTYPE_CY,DBTYPE_I4,DBTYPE_I2,DBTYPE_R8,DBTYPE_R4,DBTYPE_R8,DBTYPE_DATE,DBTYPE_DBTIME,DBTYPE_DBTIMESTAMP,DBTYPE_STR
);
begin
if(OC0QQ0QCQ0>=Low(O0OQQ0QCQ0))and(OC0QQ0QCQ0<=High(O0OQQ0QCQ0))then
Result:=O0OQQ0QCQ0[OC0QQ0QCQ0]
else
Result:=0;
end;
var
OOOQQ0QCQ0:TTDS7RecordSet;
OQOQQ0QCQ0:IntPtr;
OCOQQ0QCQ0,O0QQQ0QCQ0,OOQQQ0QCQ0,OQQQQ0QCQ0,OCQQQ0QCQ0,O0CQQ0QCQ0,
OOCQQ0QCQ0,OQCQQ0QCQ0,OCCQQ0QCQ0,O00CQ0QCQ0:TFieldDesc;
OO0CQ0QCQ0:integer;
OQ0CQ0QCQ0:TSqlFieldDesc;
OC0CQ0QCQ0:SmallInt;
O0OCQ0QCQ0:Word;
OOOCQ0QCQ0:OCO0QQCOQ0;
OQOCQ0QCQ0,OCOCQ0QCQ0,O0QCQ0QCQ0:boolean;
OOQCQ0QCQ0,OQQCQ0QCQ0,OCQCQ0QCQ0:string;
O0CCQ0QCQ0:integer;
const
OOCCQ0QCQ0:array[-11..12]of Boolean=(
False,False,False,False,True,True,True,False,False,False,False,
False,
False,True,True,True,True,True,True,False,False,False,False,False
);
begin
MSSQLInfo.SplitObjectName(OCCOC0QCQ0,OOQCQ0QCQ0,OQQCQ0QCQ0,OCQCQ0QCQ0);
if OQQCQ0QCQ0='' then
OQQCQ0QCQ0:=DefaultSDACSchema;
if OOQCQ0QCQ0='' then
OOQCQ0QCQ0:=OOQCOCCCQ0.FConnection.Database;
OOOQQ0QCQ0:=TTDS7RecordSet.Create;
try
OOOQQ0QCQ0.SetConnection(OOQCOCCCQ0.FConnection);
OOOQQ0QCQ0.SetProp(prFetchAll,True);
OOOQQ0QCQ0.OOQCOCCCQ0.OO0QQ000Q0:=Format('[%s].[sys].sp_table_type_columns_100',[OOQCQ0QCQ0]);
OOOQQ0QCQ0.OOQCOCCCQ0.FIsSProc:=True;
OOOQQ0QCQ0.OOQCOCCCQ0.OOOCQ000Q0(0,'table_name',pdInput,dtWideString,OCQCQ0QCQ0);
OOOQQ0QCQ0.OOQCOCCCQ0.OOOCQ000Q0(1,'table_owner',pdInput,dtWideString,OQQCQ0QCQ0);
OOOQQ0QCQ0.Open;
OCOQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('ORDINAL_POSITION');
O0QQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('COLUMN_NAME');
OOQQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('DATA_TYPE');
OQQQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('CHAR_OCTET_LENGTH');
OCQQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('LENGTH');
O0CQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('PRECISION');
OOCQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('SCALE');
OQCQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('TABLE_NAME');
OCCQQ0QCQ0:=OOOQQ0QCQ0.FieldByName('NULLABLE');
O00CQ0QCQ0:=OOOQQ0QCQ0.FieldByName('SS_DATA_TYPE');
OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0:=nil;
OQOQQ0QCQ0:=nil;
OOOQQ0QCQ0.AllocRecBuf(OQOQQ0QCQ0);
OO0CQ0QCQ0:=0;
try
while True do begin
OOOQQ0QCQ0.GetNextRecord(OQOQQ0QCQ0);
if OOOQQ0QCQ0.Eof then
Break;
SetLength(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0,OO0CQ0QCQ0+1);
OOOCQ0QCQ0:=@OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0[OO0CQ0QCQ0];
OOOCQ0QCQ0.OQ0OQQCOQ0:=fdkData;
OOOCQ0QCQ0.OQC0QQCOQ0:=Marshal.ReadInt32(OQOQQ0QCQ0,OCOQQ0QCQ0.DataOffset);
OOOCQ0QCQ0.O00OQQCOQ0:=OOOCQ0QCQ0.OQC0QQCOQ0;
OOOCQ0QCQ0.OOQ0QQCOQ0:=OOOQQ0QCQ0.GetFieldStrValue(OQOQQ0QCQ0,O0QQQ0QCQ0);
OOOCQ0QCQ0.OQQ0QQCOQ0:=OOOCQ0QCQ0.OOQ0QQCOQ0;
O0QCQ0QCQ0:=Marshal.ReadByte(OQOQQ0QCQ0,OCCQQ0QCQ0.DataOffset)<>0;
OOOCQ0QCQ0.O0C0QQCOQ0:=0;
OOOCQ0QCQ0.OOC0QQCOQ0:=0;
OOOCQ0QCQ0.OC0OQQCOQ0:=Marshal.ReadByte(OQOQQ0QCQ0,O00CQ0QCQ0.DataOffset);
OC0CQ0QCQ0:=Marshal.ReadInt16(OQOQQ0QCQ0,OOQQQ0QCQ0.DataOffset);
OQOCQ0QCQ0:=OOOCQ0QCQ0.OC0OQQCOQ0 in[O0C0QCCOQ0,OOC0QCCOQ0,OO0O0OCCQ0];
OCOCQ0QCQ0:=(OC0CQ0QCQ0=OQQC0QCOQ0)or(OC0CQ0QCQ0=OCQC0QCOQ0);
if OOOCQ0QCQ0.OC0OQQCOQ0=O0OQOQCOQ0 then
OC0CQ0QCQ0:=O0C00QCOQ0;
if(OC0CQ0QCQ0>=Low(OOCCQ0QCQ0))and(OC0CQ0QCQ0<=High(OOCCQ0QCQ0))and OOCCQ0QCQ0[OC0CQ0QCQ0]then begin
if not OOOQQ0QCQ0.GetNull(O0CQQ0QCQ0,OQOQQ0QCQ0)then
OOOCQ0QCQ0.O0C0QQCOQ0:=Word(Marshal.ReadInt32(OQOQQ0QCQ0,O0CQQ0QCQ0.DataOffset));
if not OOOQQ0QCQ0.GetNull(OOCQQ0QCQ0,OQOQQ0QCQ0)then
OOOCQ0QCQ0.OOC0QQCOQ0:=Marshal.ReadInt16(OQOQQ0QCQ0,OOCQQ0QCQ0.DataOffset);
end;
if not ConvertOLEDBTypeToInternalFormat(OQ0QQ0QCQ0(OC0CQ0QCQ0),OQOCQ0QCQ0,OCOCQ0QCQ0,
OOQCOCCCQ0.CalcEnableBCD,OOQCOCCCQ0.CalcEnableFMTBCD,False,
{$IFDEF LITE}0,0,{$ENDIF} OOOCQ0QCQ0.O0C0QQCOQ0,OOOCQ0QCQ0.OOC0QQCOQ0,
True,TMSSQLConnection(OOQCOCCCQ0.FConnection).Options.WideMemos,True,O0OCQ0QCQ0,prDirect)then
raise Exception.CreateFmt('Cannot convert OLEDB type %d to internal format',[OOOQQ0QCQ0.FieldByName('DATA_TYPE')]);
case OOOCQ0QCQ0.OC0OQQCOQ0 of
O00OQCCOQ0:
case OC0CQ0QCQ0 of
OQCO0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=O0QOQCCOQ0;
O0QO0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=O0COQCCOQ0;
OOQO0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OOCOQCCOQ0;
OCCO0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OCQQOQCOQ0;
end;
OQOQOQCOQ0:
if OC0CQ0QCQ0=OCCC0QCOQ0 then
OOOCQ0QCQ0.OC0OQQCOQ0:=OCCOQCCOQ0
else
OOOCQ0QCQ0.OC0OQQCOQ0:=OQ0QOQCOQ0;
OOOQOQCOQ0:
if OC0CQ0QCQ0=OCCO0QCOQ0 then
OOOCQ0QCQ0.OC0OQQCOQ0:=OCQQOQCOQ0
else
OOOCQ0QCQ0.OC0OQQCOQ0:=O0OQOQCOQ0;
OCQOQCCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OC0QOQCOQ0;
OO0OQCCOQ0:
case OC0CQ0QCQ0 of
OQQC0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OOOO0OCCQ0;
OO0O0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OC0O0OCCQ0;
OCQC0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OCOO0OCCQ0;
O0OO0QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OQOO0OCCQ0;
end;
OOC0QCCOQ0:
if OC0CQ0QCQ0=OOOO0QCOQ0 then
OOOCQ0QCQ0.OC0OQQCOQ0:=OO0O0OCCQ0;
OCC0QCCOQ0:
case OC0CQ0QCQ0 of
OOO00QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=O0OO0OCCQ0;
OO0QCQCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OQ0O0OCCQ0;
OOC00QCOQ0:
OOOCQ0QCQ0.OC0OQQCOQ0:=OQC0QCCOQ0;
end;
end;
if not OOOQQ0QCQ0.GetNull(OQQQQ0QCQ0,OQOQQ0QCQ0)then begin
O0CCQ0QCQ0:=Marshal.ReadInt32(OQOQQ0QCQ0,OQQQQ0QCQ0.DataOffset);
if O0CCQ0QCQ0<0 then
raise Exception.Create('pFieldInfo.Size < 0')
else
if O0CCQ0QCQ0=0 then
O0CCQ0QCQ0:=O00QQ0QCQ0(O0OCQ0QCQ0);
OOOCQ0QCQ0.OCQ0QQCOQ0:=Cardinal(O0CCQ0QCQ0);
end
else if not OOOQQ0QCQ0.GetNull(OCQQQ0QCQ0,OQOQQ0QCQ0)and(OOOCQ0QCQ0.OC0OQQCOQ0 in[O00OQCCOQ0,OC0QOQCOQ0])then begin
O0CCQ0QCQ0:=Marshal.ReadInt32(OQOQQ0QCQ0,OCQQQ0QCQ0.DataOffset);
if O0CCQ0QCQ0<0 then
raise Exception.Create('pFieldInfo.Size < 0')
else
if O0CCQ0QCQ0=0 then
O0CCQ0QCQ0:=O00QQ0QCQ0(O0OCQ0QCQ0);
OOOCQ0QCQ0.OCQ0QQCOQ0:=Word(O0CCQ0QCQ0);
end
else
OOOCQ0QCQ0.OCQ0QQCOQ0:=O00QQ0QCQ0(O0OCQ0QCQ0);
if not OOOQQ0QCQ0.GetNull(OQCQQ0QCQ0,OQOQQ0QCQ0)then begin
SetLength(OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0,Length(OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0)+1);
OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0[High(OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0)]:=OOOQQ0QCQ0.GetFieldStrValue(OQOQQ0QCQ0,OQCQQ0QCQ0);
end;
OOOCQ0QCQ0.OQOOQQCOQ0:=0;
OOOCQ0QCQ0.OQOOQQCOQ0:=OOOCQ0QCQ0.OQOOQQCOQ0 or OOCOO0CCQ0;
if O0QCQ0QCQ0 then
OOOCQ0QCQ0.OQOOQQCOQ0:=OOOCQ0QCQ0.OQOOQQCOQ0 or OQQOO0CCQ0;
OOOCQ0QCQ0.OOOOQQCOQ0:=@O0C0C0CCQ0[OOOCQ0QCQ0.OC0OQQCOQ0];
Assert(OOOCQ0QCQ0.OOOOQQCOQ0.OQ0CCQCOQ0<>'');
OQ0CQ0QCQ0:=TSqlFieldDesc(CreateFieldDesc);
OQ0CQ0QCQ0.IsLong:=OQOCQ0QCQ0;
O0OQCCCCQ0(OQ0CQ0QCQ0,OO0CQ0QCQ0);
if OQ0CQ0QCQ0<>nil then
FFields.Add(OQ0CQ0QCQ0);
Inc(OO0CQ0QCQ0);
end;
finally
OOOQQ0QCQ0.FreeRecBuf(OQOQQ0QCQ0);
end;
finally
OOOQQ0QCQ0.Free;
end;
if FFields.Count=0 then
raise Exception.Create(STableTypeNotSupported);
end;
procedure TTDSTableTypeRecordSet.OQCCQ0QCQ0;
begin
OO00OCCCQ0:=False;
FFlatBuffers:=True;
FFetchAll:=False;
FFetchRows:=1;
OOQCOCCCQ0.FCursorState:=csExecuted;
OOQCOCCCQ0.CmdContext.OCC00OQOQ0:=OOQCOCCCQ0.GetProtocol;
end;
procedure TTDSTableTypeRecordSet.InternalOpen(OCQCC0QCQ0:boolean=False);
begin
OQCCQ0QCQ0;
OOQCOCCCQ0.CommandType:=ctUnknown;
OQQQC0QCQ0:=ctDefaultResultSet;
CachedUpdates:=True;
LocalUpdate:=True;
inherited;
end;
procedure TTDSTableTypeRecordSet.ExplicitInitFields;
var
OO00Q0QCQ0:boolean;
begin
if OOQCOCCCQ0.CommandType=ctUnknown then begin
OQCCQ0QCQ0;
OQQQC0QCQ0:=ctDefaultResultSet;
OO00Q0QCQ0:=True;
end
else
OO00Q0QCQ0:=False;
try
inherited;
finally
if OO00Q0QCQ0 then begin
OOQCOCCCQ0.FCursorState:=csInactive;
OOQCOCCCQ0.CommandType:=ctUnknown;
end;
end;
end;
procedure TTDSTableTypeRecordSet.InternalPrepare;
begin
end;
function TTDSTableTypeRecordSet.FetchingAccessible(O000Q0QCQ0:boolean):boolean;
begin
Result:=False;
end;
function TTDSTableTypeRecordSet.OCCCQ0QCQ0:TTDS7RecordSet;
begin
OQCCQ0QCQ0;
OQQQC0QCQ0:=TMSCursorType(ctTableType);
Result:=Self as TTDS7RecordSet;
end;
function TTDSTableTypeRecordSet.SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;
begin
Result:=True;
case OQOQC0QCQ0 of
prTableTypeName:
OCCOC0QCQ0:=OCOQC0QCQ0;
else
Result:=inherited SetProp(OQOQC0QCQ0,OCOQC0QCQ0);
end;
end;
{$IFNDEF LITE}
constructor TTDS7MetaData.Create;
begin
inherited;
OQ00Q0QCQ0:='sys';
end;
function TTDS7MetaData.OC00Q0QCQ0(const O0O0Q0QCQ0:string):string;
begin
Result:=Trim(O0O0Q0QCQ0);
if Result='' then
Result:='NULL'
else
Result:=''''+Result+'''';
end;
procedure TTDS7MetaData.OOO0Q0QCQ0;
begin
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
O0CCOCOCQ0;
FMemDataHelper.AppendRecord;
end;
end;
function TTDS7MetaData.CreateRecordSet:TCRRecordSet;
begin
FRecordSet:=TTDS7RecordSet.Create;
Result:=FRecordSet;
end;
function TTDS7MetaData.OQO0Q0QCQ0(OCO0Q0QCQ0:TStrings;const O0Q0Q0QCQ0,OOQ0Q0QCQ0:string):TData;
var
OQQ0Q0QCQ0,OCQ0Q0QCQ0,O0C0Q0QCQ0,OOC0Q0QCQ0,OQC0Q0QCQ0:string;
OCC0Q0QCQ0:TStringList;
O00OQ0QCQ0:integer;
begin
OQQ0Q0QCQ0:=OC00Q0QCQ0(OCO0Q0QCQ0.Values['TABLE_SCHEMA']);
O0C0Q0QCQ0:=UpperCase(Trim(OCO0Q0QCQ0.Values['SCOPE']));
OCQ0Q0QCQ0:=Trim(OCO0Q0QCQ0.Values['TABLE_TYPE']);
if(OCQ0Q0QCQ0='')and(O0C0Q0QCQ0='LOCAL')then
OCQ0Q0QCQ0:='TABLE,VIEW';
OCC0Q0QCQ0:=TStringList.Create;
try
ParseTypes(OCQ0Q0QCQ0,OCC0Q0QCQ0);
CreateTablesFields;
FMemData.Open;
for O00OQ0QCQ0:=0 to OCC0Q0QCQ0.Count-1 do begin
OQC0Q0QCQ0:=OCC0Q0QCQ0[O00OQ0QCQ0];
OOC0Q0QCQ0:=Format(O0Q0Q0QCQ0,[OQQ0Q0QCQ0]);
if OQC0Q0QCQ0<>'' then
OOC0Q0QCQ0:=Format(OOQ0Q0QCQ0,[OOC0Q0QCQ0,OQC0Q0QCQ0]);
FRecordSet.SetSQL(OOC0Q0QCQ0);
FRecordSet.Open;
OOO0Q0QCQ0;
FRecordSet.Close;
end;
finally
OCC0Q0QCQ0.Free;
end;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS7MetaData.OO0OQ0QCQ0(OQ0OQ0QCQ0:array of Integer):TData;
const
OC0OQ0QCQ0=1;
O0OOQ0QCQ0=2;
OOOOQ0QCQ0=3;
OQOOQ0QCQ0=4;
OCOOQ0QCQ0=5;
O0QOQ0QCQ0=6;
OOQOQ0QCQ0=7;
OQQOQ0QCQ0=8;
OCQOQ0QCQ0=9;
O0COQ0QCQ0=10;
var
OOCOQ0QCQ0,
OQCOQ0QCQ0,
OCCOQ0QCQ0,
O00QOCOCQ0,
OO0QOCOCQ0,
OQ0QOCOCQ0,
OC0QOCOCQ0,
O0OQOCOCQ0,
OOOQOCOCQ0:Integer;
begin
OOCOQ0QCQ0:=OQ0OQ0QCQ0[0];
OQCOQ0QCQ0:=OQ0OQ0QCQ0[1];
OCCOQ0QCQ0:=OQ0OQ0QCQ0[2];
O00QOCOCQ0:=OQ0OQ0QCQ0[3];
OO0QOCOCQ0:=OQ0OQ0QCQ0[4];
OQ0QOCOCQ0:=OQ0OQ0QCQ0[5];
OC0QOCOCQ0:=OQ0OQ0QCQ0[6];
O0OQOCOCQ0:=OQ0OQ0QCQ0[7];
OOOQOCOCQ0:=OQ0OQ0QCQ0[8];
CreateProcedureParametersFields;
FMemData.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do
if not VarIsNull(FRecordSetHelper.FieldValues[O00QOCOCQ0])then begin
FMemDataHelper.InitRecord;
CopyRecord(
[OOCOQ0QCQ0,OQCOQ0QCQ0,OCCOQ0QCQ0,O00QOCOCQ0,OO0QOCOCQ0,OQ0QOCOCQ0,OC0QOCOCQ0,O0OQOCOCQ0,OOOQOCOCQ0],
[OC0OQ0QCQ0,O0OOQ0QCQ0,OOOOQ0QCQ0,OQOOQ0QCQ0,OCOOQ0QCQ0,OOQOQ0QCQ0,OQQOQ0QCQ0,OCQOQ0QCQ0,O0COQ0QCQ0]);
FMemDataHelper.FieldValues[O0QOQ0QCQ0]:='';
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS7MetaData.OQOQOCOCQ0(OCOQOCOCQ0:TStrings;
const O0QQOCOCQ0:string;OOQQOCOCQ0:array of Integer):TData;
const
OQQQOCOCQ0=1;
OCQQOCOCQ0=2;
O0CQOCOCQ0=3;
OOCQOCOCQ0=4;
OQCQOCOCQ0=5;
OCCQOCOCQ0=6;
O00COCOCQ0=7;
var
OO0COCOCQ0,OQ0COCOCQ0,OC0COCOCQ0,O0OCOCOCQ0:string;
OOOCOCOCQ0:boolean;
OQOCOCOCQ0:TStringArray;
OCOCOCOCQ0,
O0QCOCOCQ0,
OOQCOCOCQ0,
OQQCOCOCQ0,
OCQCOCOCQ0:Integer;
begin
OCOCOCOCQ0:=OOQQOCOCQ0[0];
O0QCOCOCQ0:=OOQQOCOCQ0[1];
OOQCOCOCQ0:=OOQQOCOCQ0[2];
OQQCOCOCQ0:=OOQQOCOCQ0[6];
OCQCOCOCQ0:=OOQQOCOCQ0[7];
OO0COCOCQ0:=OCOQOCOCQ0.Values['TABLE_SCHEMA'];
OQ0COCOCQ0:=UpperCase(Trim(OCOQOCOCQ0.Values['SCOPE']));
OOOCOCOCQ0:=OQ0COCOCQ0='LOCAL';
SetLength(OQOCOCOCQ0,0);
if Trim(OO0COCOCQ0)='' then
OO0COCOCQ0:='null';
FRecordSet.SetSQL(Format(O0QQOCOCQ0,[OO0COCOCQ0]));
FRecordSet.Open;
CreateProceduresFields;
FMemData.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
OC0COCOCQ0:=VarToStr(FRecordSetHelper.FieldValues[O0QCOCOCQ0]);
if not OOOCOCOCQ0 or(OC0COCOCQ0<>'sys')then begin
FMemDataHelper.InitRecord;
FMemDataHelper.FieldValues[OQQQOCOCQ0]:=FRecordSetHelper.FieldValues[OCOCOCOCQ0];
FMemDataHelper.FieldValues[OCQQOCOCQ0]:=OC0COCOCQ0;
O0OCOCOCQ0:=VarToStr(FRecordSetHelper.FieldValues[OOQCOCOCQ0]);
OQOCOCOCQ0:=SplitString(O0OCOCOCQ0,';');
if Length(OQOCOCOCQ0)>0 then
FMemDataHelper.FieldValues[O0CQOCOCQ0]:=OQOCOCOCQ0[0]
else
FMemDataHelper.FieldValues[O0CQOCOCQ0]:=O0OCOCOCQ0;
FMemDataHelper.FieldValues[OOCQOCOCQ0]:='PROCEDURE';
if OQQCOCOCQ0>=0 then
FMemDataHelper.FieldValues[OQCQOCOCQ0]:=FRecordSetHelper.FieldValues[OQQCOCOCQ0];
if OCQCOCOCQ0>=0 then
FMemDataHelper.FieldValues[OCCQOCOCQ0]:=FRecordSetHelper.FieldValues[OCQCOCOCQ0];
if Length(OQOCOCOCQ0)>1 then
FMemDataHelper.FieldValues[O00COCOCQ0]:=StrToInt(OQOCOCOCQ0[1])
else
FMemDataHelper.FieldValues[O00COCOCQ0]:=1;
FMemDataHelper.AppendRecord;
end;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS7MetaData.GetColumns(OCC0OCOCQ0:TStrings):TData;
var
O00OOCOCQ0,OO0OOCOCQ0,OQ0OOCOCQ0:string;
OC0OOCOCQ0:Variant;
const
O0OOOCOCQ0='exec %s.sp_columns_rowset %s, %s, %s';
OOOOOCOCQ0=1;
OQOOOCOCQ0=2;
OCOOOCOCQ0=3;
O0QOOCOCQ0=4;
OOQOOCOCQ0=7;
OQQOOCOCQ0=12;
OCQOOCOCQ0=14;
O0COOCOCQ0=15;
OOCOOCOCQ0=16;
OQCOOCOCQ0=17;
OCCOOCOCQ0=11;
O00Q0COCQ0=9;
OO0Q0COCQ0=1;
OQ0Q0COCQ0=2;
OC0Q0COCQ0=3;
O0OQ0COCQ0=4;
OOOQ0COCQ0=5;
OQOQ0COCQ0=6;
OCOQ0COCQ0=7;
O0QQ0COCQ0=8;
OOQQ0COCQ0=9;
OQQQ0COCQ0=10;
OCQQ0COCQ0=11;
begin
O00OOCOCQ0:=OC00Q0QCQ0(OCC0OCOCQ0.Values['TABLE_SCHEMA']);
OO0OOCOCQ0:=OC00Q0QCQ0(OCC0OCOCQ0.Values['TABLE_NAME']);
OQ0OOCOCQ0:=OC00Q0QCQ0(OCC0OCOCQ0.Values['COLUMN_NAME']);
FRecordSet.SetSQL(Format(O0OOOCOCQ0,[OQQQCCOCQ0,OO0OOCOCQ0,O00OOCOCQ0,OQ0OOCOCQ0]));
FRecordSet.Open;
CreateColumnsFields;
FMemData.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
CopyRecord([OOOOOCOCQ0,OQOOOCOCQ0,OCOOOCOCQ0,O0QOOCOCQ0,OOQOOCOCQ0,OQQOOCOCQ0,OCQOOCOCQ0,OOCOOCOCQ0,OQCOOCOCQ0],
[OO0Q0COCQ0,OQ0Q0COCQ0,OC0Q0COCQ0,O0OQ0COCQ0,OOOQ0COCQ0,OQOQ0COCQ0,OCOQ0COCQ0,O0QQ0COCQ0,OOQQ0COCQ0]);
OC0OOCOCQ0:=FRecordSetHelper.FieldValues[OCCOOCOCQ0];
if Boolean(OC0OOCOCQ0)then
FMemDataHelper.FieldValues[OQQQ0COCQ0]:=1
else
FMemDataHelper.FieldValues[OQQQ0COCQ0]:=0;
FMemDataHelper.FieldValues[OCQQ0COCQ0]:=FRecordSetHelper.FieldValues[O00Q0COCQ0];
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS7MetaData.GetConstraints(OO0QCCOCQ0:TStrings):TData;
var
OQ0QCCOCQ0,OC0QCCOCQ0,O0OQCCOCQ0,OOOQCCOCQ0,OQOQCCOCQ0:string;
const
OCOQCCOCQ0='exec %s.sp_table_constraints_rowset %s, %s, %s, %s, %s, %s, %s';
begin
OQ0QCCOCQ0:=OC00Q0QCQ0(OO0QCCOCQ0.Values['TABLE_CATALOG']);
OC0QCCOCQ0:=OC00Q0QCQ0(OO0QCCOCQ0.Values['TABLE_SCHEMA']);
O0OQCCOCQ0:=OC00Q0QCQ0(OO0QCCOCQ0.Values['TABLE_NAME']);
OOOQCCOCQ0:=OC00Q0QCQ0(OO0QCCOCQ0.Values['CONSTRAINT_NAME']);
OQOQCCOCQ0:=trim(OO0QCCOCQ0.Values['CONSTRAINT_TYPE']);
if OQOQCCOCQ0='' then
OQOQCCOCQ0:='NULL'
else
OQOQCCOCQ0:=''''+UpperCase(StringReplace(OQOQCCOCQ0,',',' ',[rfReplaceAll]))+'''';
FRecordSet.SetSQL(Format(OCOQCCOCQ0,[OQQQCCOCQ0,O0OQCCOCQ0,OC0QCCOCQ0,OQ0QCCOCQ0,OOOQCCOCQ0,'NULL','NULL',OQOQCCOCQ0]));
FRecordSet.Open;
Result:=FRecordSet;
end;
function TTDS7MetaData.GetDatabases(OOQQCCOCQ0:TStrings):TData;
begin
FRecordSet.SetSQL(Format('EXEC %s.sp_databases',[OQQQCCOCQ0]));
FRecordSet.Open;
Result:=FRecordSet;
end;
function TTDS7MetaData.GetIndexColumns(OOQ00COCQ0:TStrings):TData;
const
OQQ00COCQ0='exec %s.sp_indexes_rowset %s, %s, %s';
OCQ00COCQ0=1;
O0C00COCQ0=2;
OOC00COCQ0=3;
OQC00COCQ0=8;
OCC00COCQ0=6;
O00O0COCQ0=17;
OO0O0COCQ0=18;
OQ0O0COCQ0=14;
OC0O0COCQ0=1;
O0OO0COCQ0=2;
OOOO0COCQ0=3;
OQOO0COCQ0=4;
OCOO0COCQ0=5;
O0QO0COCQ0=6;
OOQO0COCQ0=7;
OQQO0COCQ0=8;
OCQO0COCQ0=9;
var
O0CO0COCQ0,OOCO0COCQ0,OQCO0COCQ0,OCCO0COCQ0:string;
begin
O0CO0COCQ0:=OC00Q0QCQ0(OOQ00COCQ0.Values['TABLE_SCHEMA']);
OOCO0COCQ0:=Trim(OOQ00COCQ0.Values['TABLE_NAME']);
if OOCO0COCQ0='' then
raise Exception.CreateFmt(SRestrictionMustBeSet,['TABLE_NAME']);
OQCO0COCQ0:=OC00Q0QCQ0(OOQ00COCQ0.Values['INDEX_NAME']);
FRecordSet.SetSQL(Format(OQQ00COCQ0,[OQQQCCOCQ0,OOCO0COCQ0,OQCO0COCQ0,O0CO0COCQ0]));
FRecordSet.Open;
CreateIndexColumnsFields;
FMemData.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do
if not VarIsNull(FRecordSetHelper.FieldValues[OCC00COCQ0])then begin
FMemDataHelper.InitRecord;
CopyRecord(
[OCQ00COCQ0,O0C00COCQ0,OOC00COCQ0,OCQ00COCQ0,O0C00COCQ0,OCC00COCQ0,OO0O0COCQ0,O00O0COCQ0],
[OC0O0COCQ0,O0OO0COCQ0,OOOO0COCQ0,OQOO0COCQ0,OCOO0COCQ0,O0QO0COCQ0,OOQO0COCQ0,OQQO0COCQ0]);
OCCO0COCQ0:=FRecordSetHelper.FieldValues[OQ0O0COCQ0];
if OCCO0COCQ0='0' then
OCCO0COCQ0:='ASC'
else
OCCO0COCQ0:='DESC';
FMemDataHelper.FieldValues[OCQO0COCQ0]:=OCCO0COCQ0;
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS7MetaData.GetTables(OCQ0OCOCQ0:TStrings):TData;
const
O0C0OCOCQ0='exec sys.sp_tables_rowset2 %s';
OOC0OCOCQ0='%s, ''%s''';
begin
Result:=OQO0Q0QCQ0(OCQ0OCOCQ0,O0C0OCOCQ0,OOC0OCOCQ0);
end;
function TTDS7MetaData.GetProcedures(OOCQ0COCQ0:TStrings):TData;
const
OQCQ0COCQ0='exec sys.sp_procedures_rowset2 %s';
OCCQ0COCQ0=1;
O00C0COCQ0=2;
OO0C0COCQ0=3;
OQ0C0COCQ0=4;
OC0C0COCQ0=5;
O0OC0COCQ0=6;
OOOC0COCQ0=7;
OQOC0COCQ0=8;
begin
Result:=OQOQOCOCQ0(OOCQ0COCQ0,OQCQ0COCQ0,
[OCCQ0COCQ0,O00C0COCQ0,OO0C0COCQ0,OQ0C0COCQ0,OC0C0COCQ0,O0OC0COCQ0,OOOC0COCQ0,OQOC0COCQ0]);
end;
function TTDS7MetaData.GetProcedureParameters(O0QC0COCQ0:TStrings):TData;
const
OOQC0COCQ0='exec sys.sp_procedure_params_managed %s, NULL, %s, %s';
OQQC0COCQ0='exec sys.sp_procedure_params_100_rowset2 %s, %s';
OCQC0COCQ0=1;
O0CC0COCQ0=2;
OOCC0COCQ0=3;
OQCC0COCQ0=4;
OCCC0COCQ0=5;
O0000COCQ0=10;
OO000COCQ0=12;
OQ000COCQ0=13;
OC000COCQ0=14;
var
O0O00COCQ0,OOO00COCQ0,OQO00COCQ0:string;
OCO00COCQ0:TStringArray;
begin
O0O00COCQ0:=OC00Q0QCQ0(O0QC0COCQ0.Values['PROCEDURE_SCHEMA']);
OOO00COCQ0:=O0QC0COCQ0.Values['PROCEDURE_NAME'];
OQO00COCQ0:=OC00Q0QCQ0(O0QC0COCQ0.Values['PARAMETER_NAME']);
OCO00COCQ0:=SplitString(OOO00COCQ0,';');
if Length(OCO00COCQ0)>0 then
OOO00COCQ0:=OC00Q0QCQ0(OCO00COCQ0[0]);
if OOO00COCQ0='' then
FRecordSet.SetSQL(Format(OQQC0COCQ0,[O0O00COCQ0,OQO00COCQ0]))
else
FRecordSet.SetSQL(Format(OOQC0COCQ0,[OOO00COCQ0,O0O00COCQ0,OQO00COCQ0]));
FRecordSet.Open;
Result:=OO0OQ0QCQ0([OCQC0COCQ0,O0CC0COCQ0,OOCC0COCQ0,OQCC0COCQ0,OCCC0COCQ0,
O0000COCQ0,OO000COCQ0,OQ000COCQ0,OC000COCQ0]);
end;
procedure TTDS7MetaData.O0CCOCOCQ0;
const
OOCCOCOCQ0=1;
OQCCOCOCQ0=2;
OCCCOCOCQ0=3;
O000OCOCQ0=4;
OO00OCOCQ0=8;
OQ00OCOCQ0=9;
OC00OCOCQ0=1;
O0O0OCOCQ0=2;
OOO0OCOCQ0=3;
OQO0OCOCQ0=4;
OCO0OCOCQ0=5;
O0Q0OCOCQ0=6;
begin
CopyRecord([OOCCOCOCQ0,OQCCOCOCQ0,OCCCOCOCQ0,O000OCOCQ0,OO00OCOCQ0,OQ00OCOCQ0],
[OC00OCOCQ0,O0O0OCOCQ0,OOO0OCOCQ0,OQO0OCOCQ0,OCO0OCOCQ0,O0Q0OCOCQ0]);
end;
constructor TTDS7MetaData2000.Create;
begin
inherited;
OQ00Q0QCQ0:='dbo';
end;
procedure TTDS7MetaData2000.O0CCOCOCQ0;
const
OOCCOCOCQ0=1;
OQCCOCOCQ0=2;
OCCCOCOCQ0=3;
O000OCOCQ0=4;
OC00OCOCQ0=1;
O0O0OCOCQ0=2;
OOO0OCOCQ0=3;
OQO0OCOCQ0=4;
OCO0OCOCQ0=5;
O0Q0OCOCQ0=6;
begin
CopyRecord([OOCCOCOCQ0,OQCCOCOCQ0,OCCCOCOCQ0,O000OCOCQ0],
[OC00OCOCQ0,O0O0OCOCQ0,OOO0OCOCQ0,OQO0OCOCQ0]);
FMemDataHelper.FieldValues[OCO0OCOCQ0]:=Null;
FMemDataHelper.FieldValues[O0Q0OCOCQ0]:=Null;
end;
function TTDS7MetaData2000.GetTables(OCQ0OCOCQ0:TStrings):TData;
const
O0C0OCOCQ0='exec dbo.sp_tables null, %s';
OOC0OCOCQ0='%s, null, "''%s''"';
begin
Result:=OQO0Q0QCQ0(OCQ0OCOCQ0,O0C0OCOCQ0,OOC0OCOCQ0);
end;
function TTDS7MetaData2000.GetProcedures(OOCQ0COCQ0:TStrings):TData;
const
OQCQ0COCQ0='exec dbo.sp_stored_procedures null, %s';
OCCQ0COCQ0=1;
O00C0COCQ0=2;
OO0C0COCQ0=3;
OQ0C0COCQ0=4;
OC0C0COCQ0=5;
O0OC0COCQ0=6;
OOOC0COCQ0=-1;
OQOC0COCQ0=-1;
begin
Result:=OQOQOCOCQ0(OOCQ0COCQ0,OQCQ0COCQ0,
[OCCQ0COCQ0,O00C0COCQ0,OO0C0COCQ0,OQ0C0COCQ0,OC0C0COCQ0,O0OC0COCQ0,OOOC0COCQ0,OQOC0COCQ0]);
end;
function TTDS7MetaData2000.GetProcedureParameters(O0QC0COCQ0:TStrings):TData;
const
OOQC0COCQ0='exec dbo.sp_sproc_columns %s, %s, NULL, %s';
OCQC0COCQ0=1;
O0CC0COCQ0=2;
OOCC0COCQ0=3;
OQCC0COCQ0=4;
OCCC0COCQ0=5;
O0000COCQ0=18;
OO000COCQ0=9;
OQ000COCQ0=8;
OC000COCQ0=10;
var
O0O00COCQ0,OOO00COCQ0,OQO00COCQ0:string;
OCO00COCQ0:TStringArray;
begin
O0O00COCQ0:=OC00Q0QCQ0(O0QC0COCQ0.Values['PROCEDURE_SCHEMA']);
OOO00COCQ0:=O0QC0COCQ0.Values['PROCEDURE_NAME'];
OQO00COCQ0:=OC00Q0QCQ0(O0QC0COCQ0.Values['PARAMETER_NAME']);
OCO00COCQ0:=SplitString(OOO00COCQ0,';');
if Length(OCO00COCQ0)>0 then
OOO00COCQ0:=OC00Q0QCQ0(OCO00COCQ0[0]);
if Trim(O0O00COCQ0)='' then
OOO00COCQ0:='NULL';
if Trim(OOO00COCQ0)='' then
OOO00COCQ0:='NULL';
FRecordSet.SetSQL(Format(OOQC0COCQ0,[OOO00COCQ0,O0O00COCQ0,OQO00COCQ0]));
FRecordSet.Open;
Result:=OO0OQ0QCQ0([OCQC0COCQ0,O0CC0COCQ0,OOCC0COCQ0,OQCC0COCQ0,OCCC0COCQ0,
O0000COCQ0,OO000COCQ0,OQ000COCQ0,OC000COCQ0]);
end;
{$ENDIF LITE}
function TTDS7Loader.SetProp(OQOQC0QCQ0:integer;const OCOQC0QCQ0:variant):boolean;
begin
Result:=True;
case OQOQC0QCQ0 of
prKeepIdentity:
OCQQCCOCQ0:=OCOQC0QCQ0;
prKeepNulls:
O0CQCCOCQ0:=OCOQC0QCQ0;
prRowsPerBatch:
OQCQCCCCQ0:=OCOQC0QCQ0;
prKilobytesPerBatch:
OOCQCCOCQ0:=OCOQC0QCQ0;
prLockTable:
OQCQCCOCQ0:=OCOQC0QCQ0;
prCheckConstraints:
OCCQCCOCQ0:=OCOQC0QCQ0;
prFireTrigger:
O00CCCOCQ0:=OCOQC0QCQ0;
else
Result:=inherited SetProp(OQOQC0QCQ0,OCOQC0QCQ0);
end;
end;
function TTDS7Loader.GetProp(O0OQC0QCQ0:integer;var OOOQC0QCQ0:variant):boolean;
begin
Result:=True;
case O0OQC0QCQ0 of
prKeepIdentity:
OOOQC0QCQ0:=OCQQCCOCQ0;
prKeepNulls:
OOOQC0QCQ0:=O0CQCCOCQ0;
prRowsPerBatch:
OOOQC0QCQ0:=OQCQCCCCQ0;
prKilobytesPerBatch:
OOOQC0QCQ0:=OOCQCCOCQ0;
prLockTable:
OOOQC0QCQ0:=OQCQCCOCQ0;
prCheckConstraints:
OOOQC0QCQ0:=OCCQCCOCQ0;
prFireTrigger:
OOOQC0QCQ0:=O00CCCOCQ0;
else
Result:=inherited GetProp(O0OQC0QCQ0,OOOQC0QCQ0);
end;
end;
class function TTDS7Loader.GetColumnClass:TCRLoaderColumnClass;
begin
Result:=TMSLoaderColumn;
end;
function TTDS7Loader.OC0CCCCCQ0(OQ0CCCOCQ0:TSqlFieldDesc;OC0CCCOCQ0:TParamDesc):boolean;
begin
OCQCCCOCQ0(OC0CCCOCQ0).O0CCCCOCQ0:=(OQ0CCCOCQ0.IsAutoIncrement and not OCQQCCOCQ0)or(FSkipReadOnlyFieldDescs and OQ0CCCOCQ0.ReadOnly)or OQ0CCCOCQ0.IsTimestamp;
Result:=OCQCCCOCQ0(OC0CCCOCQ0).O0CCCCOCQ0;
end;
procedure TTDS7Loader.Prepare;
var
OQQCCCOCQ0:variant;
begin
FConnection.GetProp(prProvider,OQQCCCOCQ0);
if TMSProvider(OQQCCCOCQ0)<>prDirect then
raise Exception.Create('Only Direct supported');
inherited;
end;
procedure TTDS7Loader.OQOCCCCCQ0;
var
O0OCCCOCQ0:string;
begin
OQ0CCCCCQ0.DetectIdentityField;
if OQ0CCCCCQ0.IdentityField<>nil then begin
Assert(OO0CCCOCQ0=nil);
OO0CCCOCQ0:=TTDS7Command.Create;
try
O0OCCCOCQ0:='SET IDENTITY_INSERT '+OOCQCCCCQ0;
if OCQQCCOCQ0 then
O0OCCCOCQ0:=O0OCCCOCQ0+' ON'
else
O0OCCCOCQ0:=O0OCCCOCQ0+' OFF';
OO0CCCOCQ0.SQL:=O0OCCCOCQ0;
OO0CCCOCQ0.SetConnection(FConnection);
OO0CCCOCQ0.CmdContext.OCC00OQOQ0:=OO0CCCOCQ0.GetProtocol;
TTDS7Command(OO0CCCOCQ0).O00OQ000Q0;
if not OO0CCCOCQ0.GetProtocol.OQOQ0QQOQ0 then
raise Exception.Create(SConnectionIsBusy);
OO0CCCOCQ0.CmdContext.OOQC0OQOQ0(False);
except
FreeAndNil(OO0CCCOCQ0);
end;
end;
end;
procedure TTDS7Loader.OCOCCCCCQ0;
begin
if not OCQQCCOCQ0 and(OO0CCCOCQ0<>nil)then begin
try
try
OO0CCCOCQ0.SQL:='SET IDENTITY_INSERT '+OOCQCCCCQ0+' OFF';
TTDS7Command(OO0CCCOCQ0).O00OQ000Q0;
if not OO0CCCOCQ0.GetProtocol.OQOQ0QQOQ0 then
raise Exception.Create(SConnectionIsBusy);
OO0CCCOCQ0.CmdContext.OOQC0OQOQ0(False);
finally
FreeAndNil(OO0CCCOCQ0);
end;
except
end;
end;
end;
procedure TTDS7Loader.FillColumn(OQOCCCOCQ0:TCRLoaderColumn;OCOCCCOCQ0:TFieldDesc);
begin
inherited;
TMSLoaderColumn(OQOCCCOCQ0).IsWide:=(OCOCCCOCQ0.SubDataType and sdtWide)<>0;
end;
function TTDS7Loader.OO00CCCCQ0(const O0QCCCOCQ0:TParamDesc):string;
begin
Result:='?';
end;
function TTDS7Loader.OC00CCCCQ0(const OOQCCCOCQ0:TParamDesc):Boolean;
begin
Result:=(OOQCCCOCQ0 is OCQCCCOCQ0)and OCQCCCOCQ0(OOQCCCOCQ0).OQCCCCOCQ0;
end;
{$IFDEF MSWINDOWS}
constructor OCCCCCOCQ0.Create;
begin
O000CCOCQ0:=nil;
OC00CCOCQ0;
end;
destructor OCCCCCOCQ0.Destroy;
begin
if O000CCOCQ0<>nil then
O000CCOCQ0.Free;
inherited;
end;
procedure OCCCCCOCQ0.OC00CCOCQ0;
begin
OO00CCOCQ0:='';
OQ00CCOCQ0:=0;
end;
procedure OCCCCCOCQ0.O0O0CCOCQ0;
const
OOO0CCOCQ0='SOFTWARE\Microsoft\MSSQLServer\Client\ConnectTo';
OQO0CCOCQ0='DBMSSOCN';
var
OCO0CCOCQ0:TRegistry;
O0Q0CCOCQ0:Integer;
OOQ0CCOCQ0:string;
begin
if Assigned(O000CCOCQ0)then
Exit;
O000CCOCQ0:=TStringList.Create;
OCO0CCOCQ0:=TRegistry.Create;
try
OCO0CCOCQ0.RootKey:=HKEY_LOCAL_MACHINE;
if not OCO0CCOCQ0.OpenKeyReadOnly(OOO0CCOCQ0)then
Exit;
try
OCO0CCOCQ0.GetValueNames(O000CCOCQ0);
for O0Q0CCOCQ0:=O000CCOCQ0.Count-1 downto 0 do begin
OOQ0CCOCQ0:=OCO0CCOCQ0.ReadString(O000CCOCQ0[O0Q0CCOCQ0]);
if not(Copy(OOQ0CCOCQ0,1,Length(OQO0CCOCQ0))=OQO0CCOCQ0)then begin
O000CCOCQ0.Delete(O0Q0CCOCQ0);
Continue;
end;
Delete(OOQ0CCOCQ0,1,Length(OQO0CCOCQ0)+1);
O000CCOCQ0[O0Q0CCOCQ0]:=UpperCase(Trim(O000CCOCQ0[O0Q0CCOCQ0]))+'='+Trim(OOQ0CCOCQ0);
end;
finally
OCO0CCOCQ0.CloseKey;
end;
finally
OCO0CCOCQ0.Free;
end;
end;
procedure OCCCCCOCQ0.OQQ0CCOCQ0(const OCQ0CCOCQ0:string);
var
O0C0CCOCQ0:Integer;
OOC0CCOCQ0:string;
begin
OQ00CCOCQ0:=0;
O0C0CCOCQ0:=Pos(',',OCQ0CCOCQ0);
if O0C0CCOCQ0=0 then
OO00CCOCQ0:=OCQ0CCOCQ0
else begin
OO00CCOCQ0:=Copy(OCQ0CCOCQ0,1,O0C0CCOCQ0-1);
OOC0CCOCQ0:=Copy(OCQ0CCOCQ0,O0C0CCOCQ0+1,Length(OCQ0CCOCQ0)-O0C0CCOCQ0);
if OOC0CCOCQ0<>'' then
TryStrToInt(OOC0CCOCQ0,OQ00CCOCQ0);
end;
end;
function OCCCCCOCQ0.O00OCCOCQ0(const OO0OCCOCQ0:string):Boolean;
var
OQ0OCCOCQ0:Integer;
OC0OCCOCQ0:string;
begin
Result:=False;
OC00CCOCQ0;
O0O0CCOCQ0;
for OQ0OCCOCQ0:=0 to O000CCOCQ0.Count-1 do begin
OC0OCCOCQ0:=O000CCOCQ0.Names[OQ0OCCOCQ0];
if OC0OCCOCQ0=UpperCase(OO0OCCOCQ0)then begin
OQQ0CCOCQ0(O000CCOCQ0.Values[OC0OCCOCQ0]);
Result:=True;
Break;
end;
end;
end;
{$ENDIF}
initialization
FillChar(@OQOOCCOCQ0[0],SizeOf(OQOOCCOCQ0),0);
OQOOCCOCQ0[OCQ0QCCOQ0]:=dtUnknown;
OQOOCCOCQ0[O0C0QCCOQ0]:=dtBlob;
OQOOCCOCQ0[OOC0QCCOQ0]:=dtMemo;
OQOOCCOCQ0[OCC0QCCOQ0]:=dtVarBytes;
OQOOCCOCQ0[O00OQCCOQ0]:=dtInt64;
OQOOCCOCQ0[OO0OQCCOQ0]:=dtString;
OQOOCCOCQ0[OQ0OQCCOQ0]:=dtDate;
OQOOCCOCQ0[OC0OQCCOQ0]:=dtTime;
OQOOCCOCQ0[OQCOQCCOQ0]:=dtDateTime;
OQOOCCOCQ0[OO0QOQCOQ0]:=dtDateTime;
OQOOCCOCQ0[O0QQOQCOQ0]:=dtDateTime;
OQOOCCOCQ0[OCQOQCCOQ0]:=dtBoolean;
OQOOCCOCQ0[O0OQOQCOQ0]:=dtFmtBCD;
OQOOCCOCQ0[OOOQOQCOQ0]:=dtFmtBCD;
OQOOCCOCQ0[OQOOQCCOQ0]:=dtBytes;
OQOOCCOCQ0[OCOOQCCOQ0]:=dtString;
OQOOCCOCQ0[O0QOQCCOQ0]:=dtUInt8;
OQOOCCOCQ0[O0COQCCOQ0]:=dtInt16;
OQOOCCOCQ0[OOCOQCCOQ0]:=dtInt32;
OQOOCCOCQ0[OCCOQCCOQ0]:=dtSingle;
OQOOCCOCQ0[OQ0QOQCOQ0]:=dtFloat;
OQOOCCOCQ0[OC0QOQCOQ0]:=dtBoolean;
OQOOCCOCQ0[OQOQOQCOQ0]:=dtFloat;
OQOOCCOCQ0[O00QOQCOQ0]:=dtCurrency;
OQOOCCOCQ0[OCOQOQCOQ0]:=dtCurrency;
OQOOCCOCQ0[OOQQOQCOQ0]:=dtCurrency;
OQOOCCOCQ0[OQC0QCCOQ0]:=dtGuid;
OQOOCCOCQ0[O0OOQCCOQ0]:={$IFNDEF FPC}dtSQLTimeStamp{$ELSE}dtDateTime{$ENDIF};
OQOOCCOCQ0[OOOOQCCOQ0]:={$IFNDEF FPC}dtSQLTimeStampOffset{$ELSE}dtDateTime{$ENDIF};
OQOOCCOCQ0[OOQO0OCCQ0]:=dtVariant;
OQOOCCOCQ0[OO0O0OCCQ0]:=dtWideMemo;
OQOOCCOCQ0[OCQQOQCOQ0]:=dtInt64;
OQOOCCOCQ0[OQ0O0OCCQ0]:=dtVarBytes;
OQOOCCOCQ0[OC0O0OCCQ0]:=dtString;
OQOOCCOCQ0[O0OO0OCCQ0]:=dtBytes;
OQOOCCOCQ0[OOOO0OCCQ0]:=dtString;
OQOOCCOCQ0[OQOO0OCCQ0]:=dtWideString;
OQOOCCOCQ0[OCOO0OCCQ0]:=dtWideString;
OQOOCCOCQ0[O0QO0OCCQ0]:=dtBlob;
OQOOCCOCQ0[OQQO0OCCQ0]:=dtXML;
OQOOCCOCQ0[OCQO0OCCQ0]:=dtTable;
FillChar(@OCOOCCOCQ0[dtUnknown],sizeof(OCOOCCOCQ0),0);
OCOOCCOCQ0[dtUnknown]:=OCQ0QCCOQ0;
OCOOCCOCQ0[dtInt8]:=O00OQCCOQ0;
OCOOCCOCQ0[dtUInt8]:=O00OQCCOQ0;
OCOOCCOCQ0[dtInt16]:=O00OQCCOQ0;
OCOOCCOCQ0[dtUInt16]:=O00OQCCOQ0;
OCOOCCOCQ0[dtWord]:=O00OQCCOQ0;
OCOOCCOCQ0[dtSmallint]:=O00OQCCOQ0;
OCOOCCOCQ0[dtInt32]:=O00OQCCOQ0;
OCOOCCOCQ0[dtInteger]:=O00OQCCOQ0;
OCOOCCOCQ0[dtInt64]:=O00OQCCOQ0;
OCOOCCOCQ0[dtLargeint]:=O00OQCCOQ0;
OCOOCCOCQ0[dtUInt32]:=O00OQCCOQ0;
OCOOCCOCQ0[dtLongWord]:=O00OQCCOQ0;
OCOOCCOCQ0[dtUInt64]:=O00OQCCOQ0;
OCOOCCOCQ0[dtBoolean]:=OC0QOQCOQ0;
OCOOCCOCQ0[dtFloat]:=OQOQOQCOQ0;
OCOOCCOCQ0[dtSingle]:=OQOQOQCOQ0;
OCOOCCOCQ0[dtCurrency]:=OCOQOQCOQ0;
OCOOCCOCQ0[dtDate]:=O0QQOQCOQ0;
OCOOCCOCQ0[dtTime]:=O0QQOQCOQ0;
OCOOCCOCQ0[dtDateTime]:=O0QQOQCOQ0;
OCOOCCOCQ0[dtVariant]:=OQOO0OCCQ0;
OCOOCCOCQ0[dtBlob]:=O0C0QCCOQ0;
OCOOCCOCQ0[dtArray]:=OQ0O0OCCQ0;
OCOOCCOCQ0[dtBytes]:=OQ0O0OCCQ0;
OCOOCCOCQ0[dtVarBytes]:=OQ0O0OCCQ0;
OCOOCCOCQ0[dtExtVarBytes]:=OQ0O0OCCQ0;
OCOOCCOCQ0[dtTable]:=OCQO0OCCQ0;
OCOOCCOCQ0[dtMemo]:=OOC0QCCOQ0;
OCOOCCOCQ0[dtString]:=OC0O0OCCQ0;
OCOOCCOCQ0[dtExtString]:=OC0O0OCCQ0;
OCOOCCOCQ0[dtWideString]:=OQOO0OCCQ0;
OCOOCCOCQ0[dtExtWideString]:=OQOO0OCCQ0;
OCOOCCOCQ0[dtBCD]:=OQOQOQCOQ0;
OCOOCCOCQ0[dtFMTBCD]:=OOOQOQCOQ0;
OCOOCCOCQ0[dtGuid]:=OQC0QCCOQ0;
OCOOCCOCQ0[dtWideMemo]:=OO0O0OCCQ0;
OCOOCCOCQ0[dtSQLTimeStamp]:=O0QQOQCOQ0;
OCOOCCOCQ0[dtExtended]:=OQOQOQCOQ0;
OCOOCCOCQ0[dtXML]:=OQOO0OCCQ0;
Move(OCOOCCOCQ0[0],O0QOCCOCQ0[0],SizeOf(OCOOCCOCQ0));
O0QOCCOCQ0[dtBlob]:=OQ0O0OCCQ0;
O0QOCCOCQ0[dtString]:=OC0O0OCCQ0;
O0QOCCOCQ0[dtVariant]:=OOQO0OCCQ0;
O0QOCCOCQ0[dtMemo]:=OC0O0OCCQ0;
O0QOCCOCQ0[dtExtString]:=OC0O0OCCQ0;
O0QOCCOCQ0[dtWideString]:=OQOO0OCCQ0;
O0QOCCOCQ0[dtExtWideString]:=OQOO0OCCQ0;
O0QOCCOCQ0[dtWideMemo]:=OQOO0OCCQ0;
O0QOCCOCQ0[dtSQLTimeStamp]:=O0OOQCCOQ0;
O0QOCCOCQ0[dtSQLTimeStampOffset]:=OOOOQCCOQ0;
Move(O0QOCCOCQ0[0],OOQOCCOCQ0[0],SizeOf(O0QOCCOCQ0));
OOQOCCOCQ0[dtDate]:=OQ0OQCCOQ0;
OOQOCCOCQ0[dtTime]:=OC0OQCCOQ0;
FillChar(@OQQOCCOCQ0[dtUnknown],sizeof(OQQOCCOCQ0),0);
OQQOCCOCQ0[dtInt8]:=OC0COQCOQ0;
OQQOCCOCQ0[dtUInt8]:=OC0COQCOQ0;
OQQOCCOCQ0[dtBoolean]:=OOCCOQCOQ0;
OQQOCCOCQ0[dtInt16]:=O0OCOQCOQ0;
OQQOCCOCQ0[dtUInt16]:=OC0OOQCOQ0;
OQQOCCOCQ0[dtInt32]:=OOOCOQCOQ0;
OQQOCCOCQ0[dtUInt32]:=OQ0OOQCOQ0;
OQQOCCOCQ0[dtInt64]:=OQ0OOQCOQ0;
OQQOCCOCQ0[dtUInt64]:=OOOOOQCOQ0;
OQQOCCOCQ0[dtSingle]:=O0O0OQCOQ0;
OQQOCCOCQ0[dtFloat]:=OQOCOQCOQ0;
OQQOCCOCQ0[dtExtended]:=OQOCOQCOQ0;
OQQOCCOCQ0[dtDate]:=OOC0OQCOQ0;
OQQOCCOCQ0[dtTime]:=OQC0OQCOQ0;
OQQOCCOCQ0[dtDateTime]:=OOQCOQCOQ0;
OQQOCCOCQ0[dtCurrency]:=O0QCOQCOQ0;
OQQOCCOCQ0[dtBCD]:=OCO0OQCOQ0;
OQQOCCOCQ0[dtFMTBCD]:=OCO0OQCOQ0;
{$IFDEF MSWINDOWS}
OCQOCCOCQ0:=OCCCCCOCQ0.Create;
finalization
OCQOCCOCQ0.Free;
{$ENDIF}
end.
