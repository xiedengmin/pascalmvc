{$I ASEDac.inc}
unit Tds5ClassesUni;
interface
uses
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
Classes,SysUtils,StrUtils,SyncObjs,{$IFDEF VER12P}DateUtils,{$ENDIF}
Variants,FmtBcd,{$IFNDEF FPC}SqlTimSt,{$ENDIF}
{$IFDEF NEXTGEN}
Generics.Collections,
{$ENDIF}
{$IFDEF LOG_PACKETS}
LogHandler,
{$ENDIF}
{$IFNDEF LITE}
CRDataTypeMap,
{$ENDIF}
CRTypes,CRFunctions,CRTimeStamp,CLRClasses,CRParser,CRVio,CRAccess,MemUtils,MemData,
{$IFNDEF UNIDACPRO}
ASEConsts,ASEDataTypeMap,ASEParser,ASEConnection,ASEProps,SqlClasses,
TdsTypes,TdsConsts,TdsPackets,Tds5Consts,TdsProtocol,Tds5Protocol,TdsClasses;
{$ELSE}
ASEConstsUni,ASEDataTypeMapUni,ASEParserUni,ASEConnectionUni,ASEPropsUni,SqlClassesUni,
TdsTypesUni,TdsConstsUni,TdsPacketsUni,Tds5ConstsUni,TdsProtocolUni,Tds5ProtocolUni,TdsClassesUni;
{$ENDIF}
const
O0COCCCCQ0=101;
OOCOCCCCQ0=102;
OQCOCCCCQ0=103;
OCCOCCCCQ0=104;
type
TTDS5Connector=class(TTDSConnector)
protected
function OOCQC000Q0:TCRConnectionClass;override;
function OQCQC000Q0:OCCQCOQOQ0;override;
function OCCQC000Q0:TIPVersion;override;
procedure O00CC000Q0;override;
function OO0CC000Q0:boolean;override;
public
procedure InitConnectionSettings;override;
function MultipleConnectionsSupported:boolean;override;
procedure SetDatabase(const O0OQQCCCQ0:string);override;
procedure SetTextSize(const OOOQQCCCQ0:string);
end;
TTDS5Transaction=class(TCRTransaction)
private
function OQOQQCCCQ0:TASEConnection;
public
procedure StartTransaction;override;
procedure Commit;override;
procedure Rollback;override;
procedure Savepoint(const OCQQQCCCQ0:string);override;
procedure RollbackToSavepoint(const OOCQQCCCQ0:string);override;
end;
TTDS5Command=class(TTDSCommand)
private
OCCQQCCCQ0:boolean;
O00CQCCCQ0:boolean;
OO0CQCCCQ0:boolean;
procedure OQ0CQCCCQ0(const OC0CQCCCQ0:string;var O0OCQCCCQ0:string;const OOOCQCCCQ0:string;OQOCQCCCQ0:integer;OCOCQCCCQ0:boolean);
procedure OO0OQCCCQ0;
protected
OOCOQCCCQ0:boolean;
OQCOQCCCQ0:string;
OCCOQCCCQ0:string;
O00QOQCCQ0:integer;
procedure OQQQQ000Q0(OO0QOQCCQ0,OQ0QOQCCQ0:integer;const OC0QOQCCQ0:variant);override;
procedure O00OQ000Q0;override;
procedure OQQ0Q000Q0;override;
function OCQ0Q000Q0:integer;override;
function OQC0Q000Q0:integer;override;
function OCC0Q000Q0:integer;override;
class function InternalGetBatchSQL(const O0OCOQCCQ0:string;OOOCOQCCQ0:TParsedSQLType;
OQOCOQCCQ0:TDAParamsInfo;OCOCOQCCQ0:integer):string;override;
function GetBatchIters(OOC0OQCCQ0:integer):integer;override;
public
class function GetContextClass:OQCQCOQOQ0;override;
class function GetSQLInfoClass:TSQLInfoClass;override;
class function GetParserClass:TSQLParserClass;override;
{$IFNDEF LITE}
class function GetMapRulesClass:TCRMapRulesClass;override;
{$ENDIF}
function AddForBrowseToSQL:boolean;
function ParseSQL(const OQC0OQCCQ0:string;OCC0OQCCQ0:TParamDescs;const O00OOQCCQ0:string=''):string;override;
function CreateProcCall(const OQOOOQCCQ0:string;OCOOOQCCQ0:Boolean;O0QOOQCCQ0:Boolean):string;override;
end;
TTDS5RecordSet=class(TTDSRecordSet)
private
OOQCOCCCQ0:TTDS5Command;
{$IFNDEF LITE}
class function O00C0QCCQ0(OO0C0QCCQ0,OQ0C0QCCQ0:Word;OC0C0QCCQ0,O0OC0QCCQ0,OOOC0QCCQ0:Boolean;OQOC0QCCQ0:Integer):Word;
{$ENDIF}
protected
function GetCommandClass:TSqlCommandClass;override;
procedure OOO0OCCCQ0;override;
procedure SetCommand(OOQC0QCCQ0:TCRCommand);override;
function IsWideField(OCQC0QCCQ0:TFieldDesc):boolean;override;
procedure O0OQCCCCQ0(var O0CC0QCCQ0:TSqlFieldDesc;OOCC0QCCQ0:integer);override;
function LoadKeyFieldsFromDB:boolean;override;
function OQOOOCCCQ0(OOO00QCCQ0:TSqlFieldDesc;OQO00QCCQ0:Integer):Integer;override;
function OOQOOCCCQ0(OCO00QCCQ0:TSqlFieldDesc;O0Q00QCCQ0:Integer):Integer;override;
end;
{$IFNDEF LITE}
TTDS5MetaDataKind=(mkTables,mkColumns,mkProcedures,mkProcedureColumns,
mkStatistics,mkSpecialColumns,mkTypeInfo,mkPrimaryKey,mkForeignKeys);
TMetaDataArgs=class
public
CatalogName:string;
SchemaName:string;
ObjectName:string;
ObjectType:string;
ColumnName:string;
Param1,Param2,Param3:smallint;
end;
TTDS5MetaDataCommand=class(TTDS5Command)
private
OOQ00QCCQ0:TTDS5MetaDataKind;
OQQ00QCCQ0:TMetaDataArgs;
public
constructor Create;override;
destructor Destroy;override;
property MetaDataKind:TTDS5MetaDataKind read OOQ00QCCQ0 write OOQ00QCCQ0;
property MetaDataArgs:TMetaDataArgs read OQQ00QCCQ0;
end;
TTDS5MetaDataRecordSet=class(TTDS5RecordSet)
protected
procedure CreateCommand;override;
end;
TTDS5MetaData=class(TCRMetaData)
private
function O0C00QCCQ0(const OOC00QCCQ0:string):string;
procedure OQC00QCCQ0(OCC00QCCQ0:TStrings);
function O0CO0QCCQ0(OOCO0QCCQ0:TStrings):string;
function OQCO0QCCQ0(OCCO0QCCQ0:TStrings):string;
protected
function CreateRecordSet:TCRRecordSet;override;
function GetTables(OQ0QCQCCQ0:TStrings):TData;override;
function GetColumns(O0CQCQCCQ0:TStrings):TData;override;
function GetIndexes(OQO0CQCCQ0:TStrings):TData;override;
function GetProcedures(O00QQQCCQ0:TStrings):TData;override;
function GetProcedureParameters(OCCQQQCCQ0:TStrings):TData;override;
function GetIndexColumns(O0COQQCCQ0:TStrings):TData;override;
function GetConstraints(O0QCOOCCQ0:TStrings):TData;override;
function GetConstraintColumns(OCQ0OOCCQ0:TStrings):TData;override;
function GetDatabases(OQCOOOCCQ0:TStrings):TData;override;
end;
{$ENDIF}
TTDS5Loader=class(TTDSLoader)
protected
function OC0CCCCCQ0(OCCOOOCCQ0:TSqlFieldDesc;O00Q0OCCQ0:TParamDesc):boolean;override;
public
function SetProp(OO0Q0OCCQ0:integer;const OQ0Q0OCCQ0:variant):boolean;override;
function GetProp(OC0Q0OCCQ0:integer;var O0OQ0OCCQ0:variant):boolean;override;
end;
function OOOQ0OCCQ0(OQOQ0OCCQ0:Word):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
function OCOQ0OCCQ0(O0QQ0OCCQ0:Word):Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
implementation
uses
DAConsts,CRProps,CRNumeric,
{$IFNDEF UNIDACPRO}
ASESQLGenerator;
{$ELSE}
ASESQLGeneratorUni;
{$ENDIF}
var
OOQQ0OCCQ0:Integer;
OQQQ0OCCQ0:TCriticalSection;
OCQQ0OCCQ0:array[0..$FF]of Word;
O0CQ0OCCQ0,OOCQ0OCCQ0:array[dtUnknown..dtXML]of Byte;
function OOOQ0OCCQ0(OQOQ0OCCQ0:Word):Boolean;
begin
Result:=OQOQ0OCCQ0 in[dtWideString,dtExtWideString,dtFixedWideChar,dtWideMemo];
end;
function OCOQ0OCCQ0(O0QQ0OCCQ0:Word):Boolean;
begin
Result:=O0QQ0OCCQ0 in[O0COCCCCQ0,OOCOCCCCQ0];
end;
function TTDS5Connector.OOCQC000Q0:TCRConnectionClass;
begin
Result:=TASEConnection;
end;
function TTDS5Connector.OQCQC000Q0:OCCQCOQOQ0;
begin
Result:=OQOCO000Q0;
end;
function TTDS5Connector.OCCQC000Q0:TIPVersion;
begin
Result:=TASEConnection(Connection).IPVersion;
end;
procedure TTDS5Connector.O00CC000Q0;
var
O00QQCCCQ0:Variant;
begin
inherited;
Connection.GetProp(prConnectionTimeout,O00QQCCCQ0);
OOQQC000Q0.OQ0Q0QQOQ0:=O00QQCCCQ0;
end;
function TTDS5Connector.OO0CC000Q0:boolean;
begin
Result:=TASEConnection(Connection).EncryptPassword<>epDisable;
end;
procedure TTDS5Connector.InitConnectionSettings;
var
OO0QQCCCQ0:string;
OQ0QQCCCQ0:integer;
begin
OQ0QQCCCQ0:=TASEConnection(Connection).Port;
if OQ0QQCCCQ0=0 then
OQ0QQCCCQ0:=DefaultASEPort;
OO0QQCCCQ0:=TASEConnection(Connection).Server;
if LowerCase(OO0QQCCCQ0)='(local)' then
OO0QQCCCQ0:='localhost';
OCQQC000Q0.OO0QCOQOQ0:=Connection.GetUsername;
OCQQC000Q0.OQ0QCOQOQ0:=Connection.GetPassword;
OCQQC000Q0.OOOQCOQOQ0:=OO0QQCCCQ0;
OCQQC000Q0.OQOQCOQOQ0:='';
OCQQC000Q0.OCOQCOQOQ0:=Connection.Database;
OCQQC000Q0.OQQQCOQOQ0:=OQ0QQCCCQ0;
end;
function TTDS5Connector.MultipleConnectionsSupported:boolean;
var
OC0QQCCCQ0:Variant;
begin
TASEConnection(Connection).GetProp(prMultipleConnections,OC0QQCCCQ0);
Result:=OC0QQCCCQ0;
end;
procedure TTDS5Connector.SetDatabase(const O0OQQCCCQ0:string);
begin
TASEConnection(Connection).ExecuteSQL(TCustomASESQLGenerator.GenerateDatabaseSQL(O0OQQCCCQ0));
end;
procedure TTDS5Connector.SetTextSize(const OOOQQCCCQ0:string);
begin
TASEConnection(Connection).ExecuteSQL(TCustomASESQLGenerator.GenerateTextSizeSQL(OOOQQCCCQ0));
end;
function TTDS5Transaction.OQOQQCCCQ0:TASEConnection;
var
OCOQQCCCQ0:integer;
begin
if FConnections.Count=0 then
raise Exception.Create(SNoConnectionsInTransaction);
if FConnections.Count>1 then
raise Exception.Create(SMultiConnectionsInTransaction);
for OCOQQCCCQ0:=0 to FConnections.Count-1 do
if not FConnections[OCOQQCCCQ0].GetConnected then
raise Exception.Create(SConnectionInTransactionNotActive);
Result:=TASEConnection(FConnections[0]);
end;
procedure TTDS5Transaction.StartTransaction;
var
O0QQQCCCQ0:TASEConnection;
begin
O0QQQCCCQ0:=OQOQQCCCQ0;
O0QQQCCCQ0.SetIsolationLevel(FIsolationLevel);
O0QQQCCCQ0.ExecuteSQL('BEGIN TRANSACTION');
FActive:=True;
FNativeTransaction:=True;
end;
procedure TTDS5Transaction.Commit;
var
OOQQQCCCQ0:TASEConnection;
begin
OOQQQCCCQ0:=OQOQQCCCQ0;
OOQQQCCCQ0.ExecuteSQL('COMMIT');
FActive:=False;
end;
procedure TTDS5Transaction.Rollback;
var
OQQQQCCCQ0:TASEConnection;
begin
OQQQQCCCQ0:=OQOQQCCCQ0;
OQQQQCCCQ0.ExecuteSQL('ROLLBACK');
FActive:=False;
end;
procedure TTDS5Transaction.RollbackToSavepoint(const OOCQQCCCQ0:string);
var
OQCQQCCCQ0:TASEConnection;
begin
OQCQQCCCQ0:=OQOQQCCCQ0;
OQCQQCCCQ0.ExecuteSQL('ROLLBACK TRANSACTION '+OOCQQCCCQ0);
end;
procedure TTDS5Transaction.Savepoint(const OCQQQCCCQ0:string);
var
O0CQQCCCQ0:TASEConnection;
begin
O0CQQCCCQ0:=OQOQQCCCQ0;
O0CQQCCCQ0.ExecuteSQL('SAVE TRANSACTION '+OCQQQCCCQ0);
end;
class function TTDS5Command.GetContextClass:OQCQCOQOQ0;
begin
Result:=OQQ0QQ00Q0;
end;
class function TTDS5Command.GetSQLInfoClass:TSQLInfoClass;
begin
Result:=TASESQLInfo;
end;
class function TTDS5Command.GetParserClass:TSQLParserClass;
begin
Result:=TASEParser;
end;
{$IFNDEF LITE}
class function TTDS5Command.GetMapRulesClass:TCRMapRulesClass;
begin
Result:=TASEMapRules;
end;
{$ENDIF}
class function TTDS5Command.InternalGetBatchSQL(const O0OCOQCCQ0:string;OOOCOQCCQ0:TParsedSQLType;
OQOCOQCCQ0:TDAParamsInfo;OCOCOQCCQ0:integer):string;
procedure O0QCOQCCQ0(OOQCOQCCQ0:StringBuilder;const OQQCOQCCQ0:string;var OCQCOQCCQ0:Integer;O0CCOQCCQ0,OOCCOQCCQ0:Integer;OQCCOQCCQ0,OCCCOQCCQ0:integer);
var
O000OQCCQ0,OO00OQCCQ0:integer;
begin
OO00OQCCQ0:=OCCCOQCCQ0;
for O000OQCCQ0:=0 to OQOCOQCCQ0.Count-1 do begin
if O000OQCCQ0=0 then
OOQCOQCCQ0.Append(OQQCOQCCQ0,O0CCOQCCQ0-1,OCCCOQCCQ0-O0CCOQCCQ0+1)
else
OOQCOQCCQ0.Append(OQQCOQCCQ0,OO00OQCCQ0,OQOCOQCCQ0[O000OQCCQ0].StartPosition-OO00OQCCQ0-1);
OOQCOQCCQ0.Append('?');
OO00OQCCQ0:=OQOCOQCCQ0[O000OQCCQ0].EndPosition-1;
Inc(OCQCOQCCQ0);
if O000OQCCQ0=(OQOCOQCCQ0.Count-1)then
OOQCOQCCQ0.Append(OQQCOQCCQ0,OO00OQCCQ0,OOCCOQCCQ0-OO00OQCCQ0);
end;
end;
var
OQ00OQCCQ0,OC00OQCCQ0,O0O0OQCCQ0:Integer;
OOO0OQCCQ0,OQO0OQCCQ0,OCO0OQCCQ0,O0Q0OQCCQ0:Integer;
OOQ0OQCCQ0:string;
OQQ0OQCCQ0:StringBuilder;
OCQ0OQCCQ0:boolean;
begin
OOQ0OQCCQ0:=Trim(O0OCOQCCQ0);
Result:=OOQ0OQCCQ0;
O0Q0OQCCQ0:=Length(OOQ0OQCCQ0);
OQQ0OQCCQ0:=StringBuilder.Create(O0Q0OQCCQ0*OCOCOQCCQ0);
OOO0OQCCQ0:=1;
OCQ0OQCCQ0:=OQOCOQCCQ0.Count>0;
try
case OOOCOQCCQ0 of
qtInsert:
for OQ00OQCCQ0:=0 to OCOCOQCCQ0-1 do begin
O0O0OQCCQ0:=0;
for OC00OQCCQ0:=0 to OQOCOQCCQ0.Count-1 do begin
OQQ0OQCCQ0.Append(OOQ0OQCCQ0,O0O0OQCCQ0,OQOCOQCCQ0[OC00OQCCQ0].StartPosition-O0O0OQCCQ0-1);
OQQ0OQCCQ0.Append('?');
O0O0OQCCQ0:=OQOCOQCCQ0[OC00OQCCQ0].EndPosition-1;
end;
OQQ0OQCCQ0.Append(OOQ0OQCCQ0,OQOCOQCCQ0[OQOCOQCCQ0.Count-1].EndPosition-1,Length(OOQ0OQCCQ0)-OQOCOQCCQ0[OQOCOQCCQ0.Count-1].EndPosition+1);
OQQ0OQCCQ0.Append(sLineBreak);
end;
qtUpdate,
qtDelete:begin
OQO0OQCCQ0:=1;
OCO0OQCCQ0:=O0Q0OQCCQ0;
for OQ00OQCCQ0:=0 to OCOCOQCCQ0-1 do begin
if OCQ0OQCCQ0 then
O0QCOQCCQ0(OQQ0OQCCQ0,OOQ0OQCCQ0,OOO0OQCCQ0,OQO0OQCCQ0,OCO0OQCCQ0,OQ00OQCCQ0,OQOCOQCCQ0[0].StartPosition-1)
else
OQQ0OQCCQ0.Append(OOQ0OQCCQ0);
OQQ0OQCCQ0.Append(sLineBreak);
end;
end;
end;
Result:=OQQ0OQCCQ0.ToString;
finally
OQQ0OQCCQ0.Free;
end;
end;
function TTDS5Command.GetBatchIters(OOC0OQCCQ0:integer):integer;
begin
if OOC0OQCCQ0<10 then
Result:=OOC0OQCCQ0
else
Result:=10;
end;
procedure TTDS5Command.OQQQQ000Q0(OO0QOQCCQ0,OQ0QOQCCQ0:integer;const OC0QOQCCQ0:variant);
var
O0OQOQCCQ0:TParamDesc;
begin
if(OO0QOQCCQ0>=0)and(OO0QOQCCQ0<Params.Count)then begin
O0OQOQCCQ0:=Params[OO0QOQCCQ0];
if O0OQOQCCQ0.GetParamType in[pdResult,pdInputOutput,pdOutput]then
case O0OQOQCCQ0.GetDataType of
dtString:
if O0OQOQCCQ0.GetSubDataType in[dtFixedChar,dtFixedWideChar,O0COCCCCQ0]then
if FTrimFixedChar then
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0)
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0);
dtFixedChar:
if FTrimFixedChar then
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0)
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0;
dtWideString:
if O0OQOQCCQ0.GetSubDataType in[dtFixedChar,dtFixedWideChar,O0COCCCCQ0]then
if FTrimFixedChar then
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0)
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0);
dtFixedWideChar:
if FTrimFixedChar then
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=TrimRight(OC0QOQCCQ0)
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0;
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0;
end
else
O0OQOQCCQ0.ItemValue[OQ0QOQCCQ0]:=OC0QOQCCQ0;
end;
end;
procedure TTDS5Command.O00OQ000Q0;
var
OOOQOQCCQ0:TTDS5Command;
OQOQOQCCQ0:TParamDesc;
OCOQOQCCQ0,O0QQOQCCQ0,OOQQOQCCQ0,
OQQQOQCCQ0,
OCQQOQCCQ0:Integer;
O0CQOQCCQ0:Word;
OOCQOQCCQ0:OQOQQCQOQ0;
OQCQOQCCQ0:Word;
begin
inherited;
OCCOC000Q0.OOC00OQOQ0:=Component;
OCCOC000Q0.OQC00OQOQ0:=GetParserClass;
OCCOC000Q0.OOQO0OQOQ0:=FRequestResultSet;
OCCOC000Q0.O0C00OQOQ0:=True;
OCCOC000Q0.OCC00OQOQ0.OC0Q0QQOQ0:=FCommandTimeout;
OCCOC000Q0.O00O0OQOQ0:=SQL;
if(FParsedSQLType=qtSelect)and AddForBrowseToSQL then
OCCOC000Q0.O00O0OQOQ0:=OCCOC000Q0.O00O0OQOQ0+' FOR BROWSE';
if FParsedSQLType=qtUnparsed then
ParseSQLType;
OOOQOQCCQ0:=TTDS5Command(BatchOwner);
if OOOQOQCCQ0=nil then begin
OQQQOQCCQ0:=0;
OCQQOQCCQ0:=FParams.Count
end
else begin
OQQQOQCCQ0:=OOOQOQCCQ0.FParamsInfo.Count;
OCQQOQCCQ0:=OQQQOQCCQ0*FBatchIters;
end;
SetLength(OCCOC000Q0.OC00QQQOQ0,OCQQOQCCQ0);
for OCOQOQCCQ0:=0 to OCQQOQCCQ0-1 do begin
if OOOQOQCCQ0=nil then begin
OOQQOQCCQ0:=OCOQOQCCQ0;
O0QQOQCCQ0:=0;
OQOQOQCCQ0:=FParams[OOQQOQCCQ0];
end
else begin
OOQQOQCCQ0:=OCOQOQCCQ0 mod OQQQOQCCQ0;
O0QQOQCCQ0:=FBatchOffset+OCOQOQCCQ0 div OQQQOQCCQ0;
OQOQOQCCQ0:=OOOQOQCCQ0.FParamsInfo[OOQQOQCCQ0].ParamRef;
end;
OOCQOQCCQ0:=@OCCOC000Q0.OC00QQQOQ0[OCOQOQCCQ0];
OOCQOQCCQ0.OOQQQCQOQ0:=OOQQOQCCQ0;
OOCQOQCCQ0.OQQQQCQOQ0:=O0QQOQCCQ0;
OOCQOQCCQ0.O0QQQCQOQ0:=OQOQOQCCQ0.GetName;
OOCQOQCCQ0.OCQQQCQOQ0:=OQOQOQCCQ0.GetParamType;
OOCQOQCCQ0.OQCQQCQOQ0:=OQOQOQCCQ0.GetIsBound;
OOCQOQCCQ0.O0CQQCQOQ0:=OQOQOQCCQ0.GetSize;
OOCQOQCCQ0.OOCQQCQOQ0:=OQOQOQCCQ0.GetScale;
OQCQOQCCQ0:=OQOQOQCCQ0.GetDataType;
OOCQOQCCQ0.O00CQCQOQ0:=O0CQ0OCCQ0[OQCQOQCCQ0];
if(OQCQOQCCQ0=dtSQLTimeStamp)or(OQOQOQCCQ0.GetSubDataType=OQCOCCCCQ0)then
case OQCQOQCCQ0 of
dtSQLTimeStamp,dtDateTime:
OOCQOQCCQ0.O00CQCQOQ0:=OOCQOQCOQ0;
dtTime:
OOCQOQCCQ0.O00CQCQOQ0:=OQCQOQCOQ0;
else
raise Exception.CreateFmt('Unknown DataType %d for bigdatetime types',[OQCQOQCCQ0]);
end;
if(OOCQOQCCQ0.O00CQCQOQ0=O0QQOQCOQ0)then
if not(VarIsNull(OQOQOQCCQ0.Value)or VarIsEmpty(OQOQOQCCQ0.Value))and(VarType(OQOQOQCCQ0.Value)and(varArray or varByRef)=0)then
if VarIsSQLTimeStamp(OQOQOQCCQ0.Value)then
OOCQOQCCQ0.O00CQCQOQ0:=OOCQOQCOQ0
else if(TDateTime(OQOQOQCCQ0.Value)<OOCCO0COQ0)or(TDateTime(OQOQOQCCQ0.Value)>OQCCO0COQ0)then
OOCQOQCCQ0.O00CQCQOQ0:=OOCQOQCOQ0;
O0CQOQCCQ0:=OQOQOQCCQ0.GetDataType;
OOCQOQCCQ0.OO0CQCQOQ0:=OOCQ0OCCQ0[O0CQOQCCQ0];
case O0CQOQCCQ0 of
dtFixedChar,dtString,dtExtString,dtMemo,dtXML:begin
if OOCQOQCCQ0.O00CQCQOQ0=OOO0OOCOQ0 then
OOCQOQCCQ0.OO0CQCQOQ0:=O00COQCOQ0;
end;
dtFixedWideChar,dtWideString,dtExtWideString,dtWideMemo:begin
if OOCQOQCCQ0.O00CQCQOQ0 in[OO0OQCCOQ0,O00OOOCOQ0,OC0OOOCOQ0]then begin
case O0CQOQCCQ0 of
dtFixedWideChar:
OOCQOQCCQ0.OO0CQCQOQ0:=OQQ0OQCOQ0;
dtWideString,dtExtWideString:
OOCQOQCCQ0.OO0CQCQOQ0:=OCQ0OQCOQ0;
else
raise Exception.CreateFmt('SetCmdContext: unknown DataType %d',[O0CQOQCCQ0]);
end;
end
else
if(OOCQOQCCQ0.O00CQCQOQ0=OC0OOOCOQ0)and(O0CQOQCCQ0 in[dtExtWideString,dtWideMemo])then
OOCQOQCCQ0.OO0CQCQOQ0:=OCQ0OQCOQ0
else
if OOCQOQCCQ0.O00CQCQOQ0=OOO0OOCOQ0 then
OOCQOQCCQ0.OO0CQCQOQ0:=OCQ0OQCOQ0;
end;
end;
end;
if OOOQOQCCQ0=nil then begin
OCCOC000Q0.O0CO0OQOQ0:=OQOQQ000Q0;
OCCOC000Q0.OOCO0OQOQ0:=OQQQQ000Q0;
OCCOC000Q0.OQCO0OQOQ0:=OQCQQ000Q0;
OCCOC000Q0.OCCO0OQOQ0:=OQ0CQ000Q0;
end
else begin
OCCOC000Q0.O0CO0OQOQ0:=OOOQOQCCQ0.OQOQQ000Q0;
OCCOC000Q0.OOCO0OQOQ0:=OOOQOQCCQ0.OQQQQ000Q0;
OCCOC000Q0.OQCO0OQOQ0:=OOOQOQCCQ0.OQCQQ000Q0;
OCCOC000Q0.OCCO0OQOQ0:=OOOQOQCCQ0.OQ0CQ000Q0;
end;
end;
procedure TTDS5Command.OQQ0Q000Q0;
function OCCQOQCCQ0:boolean;
var
O00COQCCQ0:integer;
begin
if OCCOC000Q0.OC00QQQOQ0=nil then begin
Result:=False;
Exit;
end;
Result:=True;
for O00COQCCQ0:=0 to High(OCCOC000Q0.OC00QQQOQ0)do
if OCCOC000Q0.OC00QQQOQ0[O00COQCCQ0].O00CQCQOQ0=OOO0OOCOQ0 then begin
Result:=False;
Break;
end;
end;
var
OO0COQCCQ0:boolean;
OQ0COQCCQ0:string;
begin
if OC0QQ000Q0 then
OCCOC000Q0.OCQC0OQOQ0(OCCOC000Q0.O00O0OQOQ0)
else if FIsSProc and OO0CQCCCQ0 then
OO0OQCCCQ0
else if FIsSProc and(OO0QQ000Q0<>'')then
OCCOC000Q0.OCQC0OQOQ0(OO0QQ000Q0)
else if OQ0QQ000Q0 and(Params.Count>0)then begin
OQ0COQCCQ0:=O00QOCCCQ0(FSQL,OO0COQCCQ0);
if OO0COQCCQ0 then begin
if OQ0COQCCQ0='' then
raise Exception.Create('spName is empty');
OCCOC000Q0.OCQC0OQOQ0(OQ0COQCCQ0);
end
else
OCCOC000Q0.OOQC0OQOQ0(OQCOC000Q0);
end
else if O00QQ000Q0 and OCCQOQCCQ0 then
OCCOC000Q0.OOCC0OQOQ0(OQCC0OCOQ0,OOCOC000Q0)
else
OCCOC000Q0.OOQC0OQOQ0(OQCOC000Q0);
end;
function TTDS5Command.OCQ0Q000Q0:integer;
begin
OQQQ0OCCQ0.Enter;
try
Inc(OOQQ0OCCQ0);
Result:=OOQQ0OCCQ0;
finally
OQQQ0OCCQ0.Leave;
end;
end;
function TTDS5Command.OQC0Q000Q0:integer;
begin
Result:=OOCC0OCOQ0;
end;
function TTDS5Command.OCC0Q000Q0:integer;
begin
Result:=OCCC0OCOQ0;
end;
function OQCQ0OCCQ0(const OCCQ0OCCQ0:string;O00C0OCCQ0:TTDS5RecordSet):integer;
var
OO0C0OCCQ0:integer;
begin
for OO0C0OCCQ0:=0 to O00C0OCCQ0.Fields.Count-1 do
if O00C0OCCQ0.Fields[OO0C0OCCQ0].Name=OCCQ0OCCQ0 then begin
Result:=OO0C0OCCQ0;
Exit;
end;
raise Exception.CreateFmt('GetFieldIndexByName: field %s not found',[OCCQ0OCCQ0]);
end;
function OQ0C0OCCQ0(const OC0C0OCCQ0:string;O0OC0OCCQ0:TTDS5RecordSet):Word;
var
OOOC0OCCQ0:integer;
begin
for OOOC0OCCQ0:=0 to O0OC0OCCQ0.Fields.Count-1 do
if O0OC0OCCQ0.Fields[OOOC0OCCQ0].Name=OC0C0OCCQ0 then begin
Result:=O0OC0OCCQ0.Fields[OOOC0OCCQ0].DataType;
Exit;
end;
raise Exception.CreateFmt('GetFieldDataTypeByName: field %s not found',[OC0C0OCCQ0]);
end;
function OQOC0OCCQ0(
const OCOC0OCCQ0:Integer;
const O0QC0OCCQ0,OOQC0OCCQ0:boolean;
const OQQC0OCCQ0,OCQC0OCCQ0,O0CC0OCCQ0:boolean;
{$IFDEF LITE}
const OCC000OCQ0,O00O00OCQ0:integer;
{$ENDIF}
const OOCC0OCCQ0,OQCC0OCCQ0:integer;
const OCCC0OCCQ0,O0000OCCQ0:boolean;
const OO000OCCQ0:boolean;
var OQ000OCCQ0:word):boolean;
begin
Result:=True;
case OCOC0OCCQ0 of
OO000QCOQ0:
OQ000OCCQ0:=dtBoolean;
OQCO0QCOQ0:
OQ000OCCQ0:=dtInt8;
OQ000QCOQ0:
OQ000OCCQ0:=dtUInt8;
O0QO0QCOQ0:
OQ000OCCQ0:=dtInt16;
OOQO0QCOQ0:
OQ000OCCQ0:=dtInt32;
OOCC0QCOQ0:
OQ000OCCQ0:=dtUInt32;
OCCO0QCOQ0:
OQ000OCCQ0:=dtInt64;
O0O00QCOQ0:
OQ000OCCQ0:=dtUInt64;
OCOO0QCOQ0:
if O0CC0OCCQ0 and OQQC0OCCQ0 and(OOCC0OCCQ0<=MaxBCDPrecision-MaxBCDScale+1)and(OQCC0OCCQ0<=MaxBCDScale)then
OQ000OCCQ0:=dtBCD
else
{$IFDEF VER6P}
if OCQC0OCCQ0 then begin
{$IFDEF LITE}
if(OCC000OCQ0>0)and(OOCC0OCCQ0<=MaxBCDPrecision-MaxBCDScale)and
(OOCC0OCCQ0<=OCC000OCQ0)and(OQCC0OCCQ0<=O00O00OCQ0)then
OQ000OCCQ0:=dtBCD
else
{$ENDIF}
OQ000OCCQ0:=dtFmtBCD;
end
else
{$ENDIF}
if not O0CC0OCCQ0 and OQQC0OCCQ0 then
OQ000OCCQ0:=dtBCD
else
OQ000OCCQ0:=dtFloat;
OQQO0QCOQ0:
OQ000OCCQ0:=dtSingle;
OCQO0QCOQ0,O0000QCOQ0:
OQ000OCCQ0:=dtFloat;
OQOO0QCOQ0:
OQ000OCCQ0:=dtCurrency;
O0Q00QCOQ0,OCQ00QCOQ0:
{$IFNDEF FPC}
OQ000OCCQ0:=dtSQLTimeStamp;
{$ELSE}
OQ000OCCQ0:=dtDateTime;
{$ENDIF}
OQ0O0QCOQ0,OQQC0QCOQ0,OO0O0QCOQ0:begin
if O0QC0OCCQ0 then
OQ000OCCQ0:=dtMemo
else
OQ000OCCQ0:=dtString;
end;
OC0O0QCOQ0,O0OO0QCOQ0,OOOO0QCOQ0:begin
if O0QC0OCCQ0 then begin
if OCCC0OCCQ0 and O0000OCCQ0 then
OQ000OCCQ0:=dtWideMemo
else
OQ000OCCQ0:=dtMemo;
end
else
if OCCC0OCCQ0 then
OQ000OCCQ0:=dtWideString
else
OQ000OCCQ0:=dtString;
end;
OOO00QCOQ0,OO0QCQCOQ0,OQ0QCQCOQ0:begin
if O0QC0OCCQ0 then
OQ000OCCQ0:=dtBlob
else
if OOQC0OCCQ0 then
OQ000OCCQ0:=dtBytes
else
OQ000OCCQ0:=dtVarBytes;
end;
O0QQCQCOQ0:
OQ000OCCQ0:=dtGuid;
OOQQCQCOQ0:
{$IFDEF LITE}
OQ000OCCQ0:=dtString;
{$ELSE}
OQ000OCCQ0:=dtVariant;
{$ENDIF}
OCQQCQCOQ0:
OQ000OCCQ0:=dtMemo;
OQQQCQCOQ0:
OQ000OCCQ0:=dtBlob;
OOQ00QCOQ0:
OQ000OCCQ0:=dtDate;
OQQ00QCOQ0,OCO00QCOQ0:
OQ000OCCQ0:=dtTime;
else
Result:=False;
end;
end;
function OC000OCCQ0(const O0O00OCCQ0:Word):TParamDirection;
begin
case O0O00OCCQ0 of
1:
Result:=pdInput;
4:
Result:=pdInputOutput;
5:
Result:=pdResult;
else begin
Assert(False,Format('Invalid value %d',[O0O00OCCQ0]));
Result:=pdUnknown;
end;
end;
end;
function TTDS5Command.AddForBrowseToSQL:boolean;
begin
Result:=OCCQQCCCQ0 and O00CQCCCQ0;
end;
function TTDS5Command.ParseSQL(const OQC0OQCCQ0:string;OCC0OQCCQ0:TParamDescs;const O00OOQCCQ0:string=''):string;
var
OO0OOQCCQ0:TSQLParser;
OQ0OOQCCQ0,OC0OOQCCQ0:Integer;
O0OOOQCCQ0:string;
OOOOOQCCQ0:TParamDesc;
begin
if O00OOQCCQ0<>'' then
Result:=inherited ParseSQL(OQC0OQCCQ0,nil,O00OOQCCQ0)
else
Result:=OQC0OQCCQ0;
FParamsInfo.Clear;
if FScanParams and(OCC0OQCCQ0<>nil)then
OCC0OQCCQ0.Clear;
FParsedSQLType:=qtUnknown;
OO0OOQCCQ0:=GetParserClass.Create(Result);
try
OO0OOQCCQ0.OmitBlank:=False;
OO0OOQCCQ0.OmitComment:=True;
OO0OOQCCQ0.QuotedString:=True;
OO0OOQCCQ0.DecSeparator:='.';
O00CQCCCQ0:=True;
FParsedSQLType:=qtUnknown;
while True do begin
OQ0OOQCCQ0:=OO0OOQCCQ0.GetNext(O0OOOQCCQ0);
case OQ0OOQCCQ0 of
lcEnd:
break;
lxFOR,lxDESC,lxUNION:
O00CQCCCQ0:=False;
lxColon:begin
OC0OOQCCQ0:=OQ0OOQCCQ0;
OQ0OOQCCQ0:=OO0OOQCCQ0.GetNext(O0OOOQCCQ0);
if(OQ0OOQCCQ0=lcIdent)or(OQ0OOQCCQ0=lcNumber)or(OQ0OOQCCQ0>=lxSQLFirst)then begin
if OC0OOQCCQ0=lxColon then
Result[OO0OOQCCQ0.PrevPos]:='@';
if FScanParams then begin
OOOOOQCCQ0:=AddParam;
OOOOOQCCQ0.SetName(O0OOOQCCQ0);
end
else if(OCC0OQCCQ0<>nil)and(OCC0OQCCQ0.Count>FParamsInfo.Count)then
OOOOOQCCQ0:=OCC0OQCCQ0[FParamsInfo.Count]
else
OOOOOQCCQ0:=nil;
AddParamPosition(O0OOOQCCQ0,OO0OOQCCQ0.PrevPos,OO0OOQCCQ0.CurrPos+1,OOOOOQCCQ0);
end;
end;
else begin
if FParsedSQLType=qtUnknown then
case OQ0OOQCCQ0 of
lxDECLARE:
FParsedSQLType:=qtExecuteBlock;
lxSELECT:
FParsedSQLType:=qtSelect;
lxINSERT:
FParsedSQLType:=qtInsert;
lxUPDATE:
FParsedSQLType:=qtUpdate;
lxDELETE:
FParsedSQLType:=qtDelete;
lxBegin:
FParsedSQLType:=qtExecuteBlock;
lxGO:
FParsedSQLType:=qtUnparsed;
end
else if(FParsedSQLType=qtSelect)and(OQ0OOQCCQ0=lxINTO)then
FParsedSQLType:=qtSelectInto;
end;
end;
end;
finally
OO0OOQCCQ0.Free;
end;
end;
procedure TTDS5Command.OQ0CQCCCQ0(const OC0CQCCCQ0:string;var O0OCQCCCQ0:string;const OOOCQCCCQ0:string;OQOCQCCCQ0:integer;OCOCQCCCQ0:boolean);
var
O0QCQCCCQ0:TTDS5RecordSet;
function OOQCQCCCQ0(OQQCQCCCQ0:IntPtr;OCQCQCCCQ0:TTDS5RecordSet;const O0CCQCCCQ0:TFieldDesc):string;
var
OOCCQCCCQ0:IntPtr;
OQCCQCCCQ0:Word;
begin
OQCCQCCCQ0:=Marshal.ReadUInt16(OQQCQCCCQ0,O0CCQCCCQ0.Offset);
OOCCQCCCQ0:=PtrOffset(OQQCQCCCQ0,O0CCQCCCQ0.DataOffset);
case O0CCQCCCQ0.DataType of
dtString,dtExtString:begin
if O0CCQCCCQ0.DataType=dtExtString then
OOCCQCCCQ0:=PIntPtr(OOCCQCCCQ0)^;
Result:=string(Marshal.PtrToStringAnsi(OOCCQCCCQ0,OQCCQCCCQ0));
end;
dtWideString,dtExtWideString:begin
if O0CCQCCCQ0.DataType=dtExtWideString then
OOCCQCCCQ0:=PIntPtr(OOCCQCCCQ0)^;
Result:=string(Marshal.PtrToStringUni(OOCCQCCCQ0,OQCCQCCCQ0));
end;
else
raise Exception.CreateFmt('Unknown FDT type %d',[O0CCQCCCQ0.DataType]);
end;
end;
procedure OCCCQCCCQ0;
var
O000QCCCQ0,OO00QCCCQ0,OQ00QCCCQ0,OC00QCCCQ0,O0O0QCCCQ0,OOO0QCCCQ0:TFieldDesc;
OQO0QCCCQ0:TParamDesc;
OCO0QCCCQ0:TParamDirection;
O0Q0QCCCQ0,OOQ0QCCCQ0:string;
OQQ0QCCCQ0:word;
OCQ0QCCCQ0:Boolean;
O0C0QCCCQ0:IntPtr;
OOC0QCCCQ0:string;
OQC0QCCCQ0:Integer;
OCC0QCCCQ0:integer;
begin
FParams.Clear;
O000QCCCQ0:=O0QCQCCCQ0.FieldByName('COLUMN_NAME');
OO00QCCCQ0:=O0QCQCCCQ0.FieldByName('TYPE_NAME');
OQ00QCCCQ0:=O0QCQCCCQ0.FieldByName('COLUMN_TYPE');
O0O0QCCCQ0:=O0QCQCCCQ0.FieldByName('DATA_TYPE');
if not OCOCQCCCQ0 then begin
OC00QCCCQ0:=O0QCQCCCQ0.FieldByName('COLUMN_DEF');
OOO0QCCCQ0:=O0QCQCCCQ0.FieldByName('BUFFER_LENGTH');
end
else begin
OC00QCCCQ0:=nil;
OOO0QCCCQ0:=O0QCQCCCQ0.FieldByName('LENGTH');
end;
O0C0QCCCQ0:=nil;
O0QCQCCCQ0.AllocRecBuf(O0C0QCCCQ0);
try
while True do begin
O0QCQCCCQ0.GetNextRecord(O0C0QCCCQ0);
if O0QCQCCCQ0.Eof then
Break;
OCC0QCCCQ0:=Marshal.ReadInt32(O0C0QCCCQ0,OOO0QCCCQ0.DataOffset);
OCQ0QCCCQ0:=OCC0QCCCQ0=0;
O0Q0QCCCQ0:=OOQCQCCCQ0(O0C0QCCCQ0,O0QCQCCCQ0,OO00QCCCQ0);
OQC0QCCCQ0:=Marshal.ReadInt16(O0C0QCCCQ0,O0O0QCCCQ0.DataOffset);
if O0Q0QCCCQ0='decimal' then
OQC0QCCCQ0:=2;
if OQOC0OCCQ0(OQC0QCCCQ0,OCQ0QCCCQ0,O0Q0QCCCQ0<>'varbinary',
CalcEnableBCD,CalcEnableFMTBCD,False,{$IFDEF LITE}0,0,{$ENDIF} 0,0,
True,True,True,OQQ0QCCCQ0)
then begin
OCO0QCCCQ0:=OC000OCCQ0(Marshal.ReadInt16(O0C0QCCCQ0,OQ00QCCCQ0.DataOffset));
OOQ0QCCCQ0:=GetParamNameWODog(OOQCQCCCQ0(O0C0QCCCQ0,O0QCQCCCQ0,O000QCCCQ0));
if OOQ0QCCCQ0='RETURN_VALUE' then
OCO0QCCCQ0:=pdResult;
OQO0QCCCQ0:=TParamDesc.Create;
FParams.Add(OQO0QCCCQ0);
OQO0QCCCQ0.SetParamType(OCO0QCCCQ0);
OQO0QCCCQ0.SetDataType(OQQ0QCCCQ0);
OQO0QCCCQ0.SetName(OOQ0QCCCQ0);
if(OC00QCCCQ0<>nil)and not O0QCQCCCQ0.GetNull(OC00QCCCQ0,O0C0QCCCQ0)then begin
OOC0QCCCQ0:=OOQCQCCCQ0(O0C0QCCCQ0,O0QCQCCCQ0,OC00QCCCQ0);
OQO0QCCCQ0.SetValue(OOC0QCCCQ0);
end;
if O0OQQ000Q0(OQO0QCCCQ0)then
OQO0QCCCQ0.SetParamType(pdInput);
OQO0QCCCQ0.SetSize(Marshal.ReadInt32(O0C0QCCCQ0,OOO0QCCCQ0.DataOffset));
{$IFDEF LOG_PACKETS}
AddToLog(Format('DescribeParams: %s, TypeName %s, ParamType %d, sqldt %d, DataType %s, size %d',
[OQO0QCCCQ0.GetName,O0Q0QCCCQ0,Integer(OQO0QCCCQ0.GetParamType),OQC0QCCCQ0,DataTypeToName[OQQ0QCCCQ0],OQO0QCCCQ0.GetSize]));
{$ENDIF}
end
else
raise Exception.CreateFmt('Unknown field type "%s" (OLE DB code = %Xh)',[O0Q0QCCCQ0,OQC0QCCCQ0]);
end;
finally
if O0C0QCCCQ0<>nil then
O0QCQCCCQ0.FreeRecBuf(O0C0QCCCQ0);
end;
end;
var
O00OQCCCQ0:TTDS5Command;
begin
O0QCQCCCQ0:=TTDS5RecordSet.Create;
try
O0QCQCCCQ0.SetConnection(FConnection);
O0QCQCCCQ0.SetProp(prFetchAll,True);
if not OCOCQCCCQ0 then begin
O0QCQCCCQ0.OOQCOCCCQ0.SQL:='sp_odbc_getprocedurecolumns';
if OC0CQCCCQ0<>'' then
O0QCQCCCQ0.OOQCOCCCQ0.SQL:=OC0CQCCCQ0+'..'+O0QCQCCCQ0.OOQCOCCCQ0.SQL;
if O0OCQCCCQ0='' then
O0OCQCCCQ0:='dbo';
O00OQCCCQ0:=TTDS5Command(O0QCQCCCQ0.GetCommand);
TTDS5Command(O0QCQCCCQ0.GetCommand).OC0QQ000Q0:=True;
try
O00OQCCCQ0.OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
if OQOCQCCCQ0>1 then
O00OQCCCQ0.OOOCQ000Q0(1,'procedure_name',pdInput,dtString,AnsiString(OOOCQCCCQ0+';'+IntToStr(OQOCQCCCQ0)))
else
O00OQCCCQ0.OOOCQ000Q0(1,'procedure_name',pdInput,dtString,AnsiString(OOOCQCCCQ0));
O00OQCCCQ0.OOOCQ000Q0(2,'procedure_owner',pdInput,dtString,AnsiString(O0OCQCCCQ0));
O00OQCCCQ0.OOOCQ000Q0(3,'procedure_qualifier',pdInput,dtString,AnsiString(OC0CQCCCQ0));
O0QCQCCCQ0.Open;
finally
O00OQCCCQ0.O0CCQ000Q0(0,4);
end;
end
else begin
O0QCQCCCQ0.OOQCOCCCQ0.SQL:='sp_jdbc_getfunctioncolumns';
if OC0CQCCCQ0<>'' then
O0QCQCCCQ0.OOQCOCCCQ0.SQL:=OC0CQCCCQ0+'..'+O0QCQCCCQ0.OOQCOCCCQ0.SQL;
if O0OCQCCCQ0='' then
O0OCQCCCQ0:='dbo';
O00OQCCCQ0:=TTDS5Command(O0QCQCCCQ0.GetCommand);
TTDS5Command(O0QCQCCCQ0.GetCommand).OC0QQ000Q0:=True;
try
O00OQCCCQ0.OOOCQ000Q0(0,'RETURN_VALUE',pdResult,dtInteger,Unassigned);
O00OQCCCQ0.OOOCQ000Q0(1,'fn_qualifier',pdInput,dtString,AnsiString(OC0CQCCCQ0));
O00OQCCCQ0.OOOCQ000Q0(2,'fn_owner',pdInput,dtString,AnsiString(O0OCQCCCQ0));
if OQOCQCCCQ0>1 then
O00OQCCCQ0.OOOCQ000Q0(3,'fn_name',pdInput,dtString,AnsiString(OOOCQCCCQ0+';'+IntToStr(OQOCQCCCQ0)))
else
O00OQCCCQ0.OOOCQ000Q0(3,'fn_name',pdInput,dtString,AnsiString(OOOCQCCCQ0));
O0QCQCCCQ0.Open;
finally
O00OQCCCQ0.O0CCQ000Q0(0,4);
end;
end;
OCCCQCCCQ0;
finally
TTDS5Command(O0QCQCCCQ0.GetCommand).OC0QQ000Q0:=False;
O0QCQCCCQ0.Close;
O0QCQCCCQ0.Free;
end;
end;
function TTDS5Command.CreateProcCall(const OQOOOQCCQ0:string;OCOOOQCCQ0,O0QOOQCCQ0:Boolean):string;
procedure OOQOOQCCQ0(const OQQOOQCCQ0:string;var OCQOOQCCQ0,O0COOQCCQ0:string;var OOCOOQCCQ0:integer);
var
OQCOOQCCQ0:string;
begin
OQCOOQCCQ0:='%s';
if OOCOOQCCQ0<>0 then
OQCOOQCCQ0:=OQCOOQCCQ0+', ';
OQCOOQCCQ0:=OQCOOQCCQ0+'%s%s';
OCQOOQCCQ0:=Format(OQCOOQCCQ0,[OCQOOQCCQ0,'@',OQQOOQCCQ0]);
O0COOQCCQ0:=Format(OQCOOQCCQ0,[O0COOQCCQ0,':',OQQOOQCCQ0]);
Inc(OOCOOQCCQ0);
end;
var
OCCOOQCCQ0,O00Q0QCCQ0,OO0Q0QCCQ0,OQ0Q0QCCQ0:integer;
OC0Q0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OQOQ0QCCQ0:string;
OCOQ0QCCQ0:Integer;
O0QQ0QCCQ0,OOQQ0QCCQ0,OQQQ0QCCQ0,OCQQ0QCCQ0,O0CQ0QCCQ0,OOCQ0QCCQ0:string;
OQCQ0QCCQ0,OCCQ0QCCQ0:string;
begin
if FConnection=nil then
raise Exception.Create(SConnectionNotDefined);
OO0QQ000Q0:=OQOOOQCCQ0;
FConnection.Connect('');
OO0CQCCCQ0:=False;
if OCOOOQCCQ0 then begin
OQ0OQ000Q0(False,OQOQ0QCCQ0,OC0Q0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OCOQ0QCCQ0);
OQ0CQCCCQ0(OC0Q0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OCOQ0QCCQ0,False);
if(FParams.Count=0)and(OC0Q0QCCQ0='')then begin
O0QQ0QCCQ0:=LowerCase(Copy(OQOOOQCCQ0,1,3));
if(O0QQ0QCCQ0='sp_')or(O0QQ0QCCQ0='xp_')then
OQ0CQCCCQ0(OQOQ0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OCOQ0QCCQ0,False);
end;
if FParams.Count=0 then begin
OQ0CQCCCQ0(OC0Q0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OCOQ0QCCQ0,True);
if(FParams.Count=0)and(OC0Q0QCCQ0='')then begin
O0QQ0QCCQ0:=LowerCase(Copy(OQOOOQCCQ0,1,3));
if(O0QQ0QCCQ0='sp_')or(O0QQ0QCCQ0='xp_')then
OQ0CQCCCQ0(OQOQ0QCCQ0,O0OQ0QCCQ0,OOOQ0QCCQ0,OCOQ0QCCQ0,True);
end;
OO0CQCCCQ0:=FParams.Count>0;
end;
end;
OOOQ0QCCQ0:=OQOOOQCCQ0;
ParseProcName(OOOQ0QCCQ0,OCCQ0QCCQ0);
OOQQ0QCCQ0:=SQLInfo.NormalizeName(OOOQ0QCCQ0,'[',']');
if OCCQ0QCCQ0<>'' then
OOQQ0QCCQ0:=OOQQ0QCCQ0+';'+OCCQ0QCCQ0;
OO0Q0QCCQ0:=0;
OQ0Q0QCCQ0:=0;
OQQQ0QCCQ0:='';
OCQQ0QCCQ0:='';
O0CQ0QCCQ0:='';
OOCQ0QCCQ0:='';
OCCOC000Q0.OO00QQQOQ0:=nil;
O00Q0QCCQ0:=1;
for OCCOOQCCQ0:=0 to FParams.Count-1 do begin
OQCQ0QCCQ0:=FParams[OCCOOQCCQ0].GetName;
if OQCQ0QCCQ0='' then begin
OQCQ0QCCQ0:=Format('p%d',[O00Q0QCCQ0]);
Inc(O00Q0QCCQ0);
end;
if FParams[OCCOOQCCQ0].GetParamType=pdResult then
OOQOOQCCQ0(OQCQ0QCCQ0,OCQQ0QCCQ0,OOCQ0QCCQ0,OQ0Q0QCCQ0)
else
if not(OO0CQCCCQ0 and(OQCQ0QCCQ0='RETURN_VALUE'))then
OOQOOQCCQ0(OQCQ0QCCQ0,OQQQ0QCCQ0,O0CQ0QCCQ0,OO0Q0QCCQ0);
end;
if OO0CQCCCQ0 then begin
FSQL:=Format('SELECT %s.%s(%s)',[O0OQ0QCCQ0,OOQQ0QCCQ0,OQQQ0QCCQ0]);
Result:=Format('SELECT %s.%s(%s)',[O0OQ0QCCQ0,OOQQ0QCCQ0,O0CQ0QCCQ0]);
end
else
if OCQQ0QCCQ0<>'' then begin
FSQL:=Format('EXEC %s = %s %s',[OCQQ0QCCQ0,OOQQ0QCCQ0,OQQQ0QCCQ0]);
Result:=Format('EXEC %s = %s %s',[OOCQ0QCCQ0,OOQQ0QCCQ0,O0CQ0QCCQ0]);
end
else begin
FSQL:=Format('EXEC %s %s',[OOQQ0QCCQ0,OQQQ0QCCQ0]);
Result:=Format('EXEC %s %s',[OOQQ0QCCQ0,O0CQ0QCCQ0]);
end;
FUserSQL:=Result;
{$IFDEF LOG_PACKETS}
AddToLog('CreateProcCall FSQL "'+FSQL+'"');
AddToLog('CreateProcCall Result "'+Result+'"');
{$ENDIF}
end;
procedure TTDS5Command.OO0OQCCCQ0;
var
OQ0OQCCCQ0:TTDS5RecordSet;
OC0OQCCCQ0,O0OOQCCCQ0:integer;
OOOOQCCCQ0,OQOOQCCCQ0:TParamDesc;
OCOOQCCCQ0:TFieldDesc;
O0QOQCCCQ0:IntPtr;
OOQOQCCCQ0:Variant;
OQQOQCCCQ0:TSharedObject;
OCQOQCCCQ0:TBlob;
O0COQCCCQ0:integer;
begin
OQ0OQCCCQ0:=TTDS5RecordSet.Create;
OQ0OQCCCQ0.SetTrimFixedChar(Self.FTrimFixedChar);
try
OQ0OQCCCQ0.SetConnection(FConnection);
OQ0OQCCCQ0.GetCommand.SetSQL(Self.SQL);
OQ0OQCCCQ0.GetCommand.Params.Clear;
for OC0OQCCCQ0:=0 to Self.Params.Count-1 do begin
OQOOQCCCQ0:=Self.Params[OC0OQCCCQ0];
OOOOQCCCQ0:=TParamDesc.Create;
OOOOQCCCQ0.SetNull(OQOOQCCCQ0.GetNull);
OOOOQCCCQ0.SetName(OQOOQCCCQ0.GetName);
OOOOQCCCQ0.SetParamType(OQOOQCCCQ0.GetParamType);
OOOOQCCCQ0.SetIsBound(OQOOQCCCQ0.GetIsBound);
OOOOQCCCQ0.SetSize(OQOOQCCCQ0.GetSize);
OOOOQCCCQ0.SetScale(OQOOQCCCQ0.GetScale);
OOOOQCCCQ0.SetDataType(OQOOQCCCQ0.GetDataType);
OOOOQCCCQ0.SetSubDataType(OQOOQCCCQ0.GetSubDataType);
OOOOQCCCQ0.Value:=Self.Params[OC0OQCCCQ0].Value;
OQ0OQCCCQ0.GetCommand.Params.Add(OOOOQCCCQ0);
end;
OQ0OQCCCQ0.SetProp(prFetchRows,1);
OQ0OQCCCQ0.SetProp(prFlatBuffers,False);
OQ0OQCCCQ0.Open;
OQ0OQCCCQ0.AllocRecBuf(IntPtr(O0QOQCCCQ0));
try
O0COQCCCQ0:=0;
repeat
OQ0OQCCCQ0.GetNextRecord(O0QOQCCCQ0);
if OQ0OQCCCQ0.Eof then
Exit;
O0OOQCCCQ0:=0;
for OC0OQCCCQ0:=0 to Self.Params.Count-1 do begin
OOOOQCCCQ0:=TParamDesc(FParams[OC0OQCCCQ0]);
Assert(OOOOQCCCQ0<>nil);
if OOOOQCCCQ0.GetParamType in[pdOutput,pdInputOutput,pdResult]then begin
if O0OOQCCCQ0>=OQ0OQCCCQ0.Fields.Count then
break;
repeat
OCOOQCCCQ0:=OQ0OQCCCQ0.Fields[O0OOQCCCQ0];
Inc(O0OOQCCCQ0);
until OCOOQCCCQ0.ParentField=nil;
if OQ0OQCCCQ0.GetNull(OCOOQCCCQ0,O0QOQCCCQ0)then
OOOOQCCCQ0.ItemNull[O0COQCCCQ0]:=True
else begin
OOOOQCCCQ0.SetNull(False);
OOOOQCCCQ0.ItemNull[O0COQCCCQ0]:=False;
if OCOOQCCCQ0.IsComplex and
not(OCOOQCCCQ0.DataType in[dtExtString,dtExtWideString])
then begin
OQQOQCCCQ0:=OQ0OQCCCQ0.GetBlob(OCOOQCCCQ0,O0QOQCCCQ0);
case OCOOQCCCQ0.DataType of
dtBlob,dtMemo,dtWideMemo:begin
if OOOOQCCCQ0.GetDataType=dtString then
OOOOQCCCQ0.ItemValue[O0COQCCCQ0]:=TBlob(OQQOQCCCQ0).AsString
else
if OOOOQCCCQ0.GetDataType=dtWideString then
OOOOQCCCQ0.ItemValue[O0COQCCCQ0]:=TBlob(OQQOQCCCQ0).AsWideString
else begin
OCQOQCCCQ0:=TBlob(TVarData(OOOOQCCCQ0.ItemValue[O0COQCCCQ0]).VPointer);
OCQOQCCCQ0.Assign(TBlob(OQQOQCCCQ0));
end;
end;
end
end
else begin
OQ0OQCCCQ0.GetFieldAsVariant(OCOOQCCCQ0,O0QOQCCCQ0,OOQOQCCCQ0);
OOOOQCCCQ0.ItemValue[O0COQCCCQ0]:=OOQOQCCCQ0;
end;
end;
end;
end;
Inc(O0COQCCCQ0);
until False;
finally
Marshal.FreeHGlobal(O0QOQCCCQ0);
OQ0OQCCCQ0.Close;
end;
finally
OQ0OQCCCQ0.Free;
end;
end;
function TTDS5RecordSet.GetCommandClass:TSqlCommandClass;
begin
Result:=TTDS5Command;
end;
procedure TTDS5RecordSet.OOO0OCCCQ0;
var
O0QC0QCCQ0:Variant;
begin
inherited;
if GetProp(prExtendedFieldsInfo,O0QC0QCCQ0)then
OOQCOCCCQ0.OCCQQCCCQ0:=O0QC0QCCQ0;
end;
procedure TTDS5RecordSet.SetCommand(OOQC0QCCQ0:TCRCommand);
begin
inherited;
OOQCOCCCQ0:=TTDS5Command(OOQC0QCCQ0);
end;
function TTDS5RecordSet.LoadKeyFieldsFromDB:boolean;
begin
Result:=not OOQCOCCCQ0.AddForBrowseToSQL;
end;
function TTDS5RecordSet.OQOOOCCCQ0(OOO00QCCQ0:TSqlFieldDesc;OQO00QCCQ0:Integer):Integer;
begin
if OOO00QCCQ0.Fixed then
Result:=OOO00QCCQ0.Length
else
Result:=inherited OQOOOCCCQ0(OOO00QCCQ0,OQO00QCCQ0);
end;
function TTDS5RecordSet.OOQOOCCCQ0(OCO00QCCQ0:TSqlFieldDesc;O0Q00QCCQ0:Integer):Integer;
begin
if OCO00QCCQ0.Fixed then
Result:=OCO00QCCQ0.Length*2
else
Result:=inherited OOQOOCCCQ0(OCO00QCCQ0,O0Q00QCCQ0);
end;
{$IFNDEF LITE}
class function TTDS5RecordSet.O00C0QCCQ0(OO0C0QCCQ0,OQ0C0QCCQ0:Word;OC0C0QCCQ0,O0OC0QCCQ0,OOOC0QCCQ0:Boolean;
OQOC0QCCQ0:Integer):Word;
begin
case OO0C0QCCQ0 of
dtBoolean:
Result:=aseBit;
dtUInt8:
Result:=aseTinyint;
dtInt16:
Result:=aseSmallint;
dtUInt16:
Result:=aseUSmallint;
dtInt32:
Result:=aseInteger;
dtUInt32:
Result:=aseUInteger;
dtInt64:
Result:=aseBigint;
dtUInt64:
Result:=aseUBigint;
dtSingle:
Result:=aseReal;
dtFloat:
Result:=aseFloat;
dtCurrency:
if OQOC0QCCQ0<=10 then
Result:=aseSmallmoney
else
Result:=aseMoney;
dtBCD:
if OQ0C0QCCQ0=sdtNumeric then
Result:=aseDecimal
else if OQOC0QCCQ0<=10 then
Result:=aseSmallmoney
else
Result:=aseMoney;
dtFmtBCD:
Result:=aseDecimal;
dtDateTime:
Result:=aseDatetime;
dtDate:
Result:=aseDate;
dtTime:
Result:=aseTime;
dtSQLTimeStamp:
Result:=aseTimestamp;
dtString:
if O0OC0QCCQ0 then
Result:=aseChar
else
Result:=aseVarchar;
dtWideString:
if O0OC0QCCQ0 then
Result:=aseUniChar
else
Result:=aseUniVarchar;
dtExtString,dtMemo:
case OQ0C0QCCQ0 of
O0COCCCCQ0,
OOCOCCCCQ0,
dtFixedWideChar,
dtWideString,
dtWideMemo:
if OC0C0QCCQ0 then
Result:=aseUniText
else if O0OC0QCCQ0 then
Result:=aseUniChar
else
Result:=aseUniVarchar;
else
if OC0C0QCCQ0 then
Result:=aseText
else if O0OC0QCCQ0 then
Result:=aseChar
else
Result:=aseVarchar;
end;
dtExtWideString,dtWideMemo:
if OC0C0QCCQ0 then
Result:=aseUniText
else if O0OC0QCCQ0 then
Result:=aseUniChar
else
Result:=aseUniVarchar;
dtBytes:
if OOOC0QCCQ0 then
Result:=aseDatetime
else
Result:=aseBinary;
dtVarBytes,dtExtVarBytes:
if OOOC0QCCQ0 then
Result:=aseDatetime
else
Result:=aseVarBinary;
dtBlob:
Result:=aseImage;
dtXml:
Result:=aseXml;
else
Result:=0;
end;
end;
{$ENDIF}
function TTDS5RecordSet.IsWideField(OCQC0QCCQ0:TFieldDesc):boolean;
begin
Result:=OCQC0QCCQ0.SubDataType in[dtWideString,dtExtWideString,dtFixedWideChar,dtWideMemo];
end;
procedure TTDS5RecordSet.O0OQCCCCQ0(var O0CC0QCCQ0:TSqlFieldDesc;OOCC0QCCQ0:Integer);
var
OQCC0QCCQ0:Word;
OCCC0QCCQ0:O0Q0QQCOQ0;
O0000QCCQ0:TCRTableInfo;
OO000QCCQ0:Boolean;
OQ000QCCQ0:Variant;
{$IFNDEF LITE}
OC000QCCQ0:TFetchConverter;
{$ENDIF}
begin
Assert(Length(OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0)>OOCC0QCCQ0);
OCCC0QCCQ0:=OOQCOCCCQ0.OCCOC000Q0.OO00QQQOQ0[OOCC0QCCQ0];
{$IFDEF LITE}
if OCCC0QCCQ0.OCOOQQCOQ0 then begin
O0CC0QCCQ0.Free;
O0CC0QCCQ0:=nil;
Exit;
end;
{$ENDIF}
if OCCC0QCCQ0.OC0OQQCOQ0=OCQ0OOCOQ0 then begin
case OCCC0QCCQ0.O0OOQQCOQ0 of
OC0COQCOQ0:
OQCC0QCCQ0:=dtByte;
O0OCOQCOQ0:
OQCC0QCCQ0:=dtInt16;
OC0OOQCOQ0:
OQCC0QCCQ0:=dtUInt16;
OOOCOQCOQ0:
OQCC0QCCQ0:=dtInt32;
OQQOOQCOQ0,O0OOOQCOQ0:
OQCC0QCCQ0:=dtUInt32;
OQ0OOQCOQ0:
OQCC0QCCQ0:=dtInt64;
OOOOOQCOQ0:
OQCC0QCCQ0:=dtUInt64;
else
OQCC0QCCQ0:=dtInt64;
end;
end
else
OQCC0QCCQ0:=OCQQ0OCCQ0[OCCC0QCCQ0.OC0OQQCOQ0];
if OQCC0QCCQ0=dtBytes then
case OCCC0QCCQ0.O0OOQQCOQ0 of
OQ0COQCOQ0:
OQCC0QCCQ0:=dtVarBytes;
OQQ0OQCOQ0:
OQCC0QCCQ0:=dtWideString;
OCQ0OQCOQ0:
OQCC0QCCQ0:=dtExtWideString;
end;
O0CC0QCCQ0.FieldNo:=FFields.Count+1;
O0CC0QCCQ0.ActualFieldNo:=OCCC0QCCQ0.O00OQQCOQ0;
O0CC0QCCQ0.Name:=OCCC0QCCQ0.OOQ0QQCOQ0;
if OCCC0QCCQ0.OQQ0QQCOQ0<>'' then
O0CC0QCCQ0.ActualName:=OCCC0QCCQ0.OQQ0QQCOQ0
else
O0CC0QCCQ0.ActualName:=OCCC0QCCQ0.OOQ0QQCOQ0;
if OCCC0QCCQ0.OCC0QQCOQ0>0 then begin
OCCC0QCCQ0.OO0OQQCOQ0:=OOQCOCCCQ0.OCCOC000Q0.OQ00QQQOQ0[OCCC0QCCQ0.OCC0QQCOQ0-1];
O0000QCCQ0:=FTablesInfo.FindByName(OCCC0QCCQ0.OO0OQQCOQ0);
if O0000QCCQ0=nil then begin
O0000QCCQ0:=FTablesInfo.Add;
O0000QCCQ0.TableName:=OCCC0QCCQ0.OO0OQQCOQ0;
O0000QCCQ0.TableAlias:='';
O0000QCCQ0.IsView:=True;
end;
O0CC0QCCQ0.TableInfo:=O0000QCCQ0;
end;
if OCCC0QCCQ0.O0C0QQCOQ0<>0 then
O0CC0QCCQ0.Length:=OCCC0QCCQ0.O0C0QQCOQ0
else if not(OQCC0QCCQ0 in[dtBoolean,
dtInt8,dtInt16,dtInt32,dtInt64,dtUInt8,dtUInt16,dtUInt32,dtUInt64,
dtMemo,dtWideMemo,dtBlob])
then
O0CC0QCCQ0.Length:=Word(OCCC0QCCQ0.OCQ0QQCOQ0);
O0CC0QCCQ0.Scale:=OCCC0QCCQ0.OOC0QQCOQ0;
O0CC0QCCQ0.ReadOnly:=((OCCC0QCCQ0.OQOOQQCOQ0 and O0C00OCOQ0)=0)or OCCC0QCCQ0.OOQOQQCOQ0;
O0CC0QCCQ0.Required:=(OCCC0QCCQ0.OQOOQQCOQ0 and OOC00OCOQ0)=0;
O0CC0QCCQ0.IsAutoIncrement:=(OCCC0QCCQ0.OQOOQQCOQ0 and OQC00OCOQ0)<>0;
O0CC0QCCQ0.Hidden:=(OCCC0QCCQ0.OQOOQQCOQ0 and O0Q00OCOQ0)<>0;
O0CC0QCCQ0.IsKey:=((OCCC0QCCQ0.OQOOQQCOQ0 and OOQ00OCOQ0)<>0)and((OCCC0QCCQ0.OQOOQQCOQ0 and OOC00OCOQ0)=0);
O0CC0QCCQ0.FieldDescKind:=OCCC0QCCQ0.OQ0OQQCOQ0;
O0CC0QCCQ0.IsTimestamp:=OCCC0QCCQ0.OQQOQQCOQ0;
O0CC0QCCQ0.Fixed:=OCCC0QCCQ0.OOOOQQCOQ0.OQO0CQCOQ0;
O0CC0QCCQ0.DataType:=OQCC0QCCQ0;
O0CC0QCCQ0.SubDataType:=OQCC0QCCQ0;
if OCCC0QCCQ0.OC0OQQCOQ0 in[OOCQOQCOQ0,OQCQOQCOQ0]then
O0CC0QCCQ0.SubDataType:=OQCOCCCCQ0;
if OCCC0QCCQ0.OC0OQQCOQ0 in[O00QOQCOQ0,OCOQOQCOQ0,OOQQOQCOQ0]then
O0CC0QCCQ0.SubDataType:=dtBCD;
if OCCC0QCCQ0.OC0OQQCOQ0=OCOOQCCOQ0 then begin
O0CC0QCCQ0.SubDataType:=dtFixedChar;
O0CC0QCCQ0.Fixed:=True;
end;
case OCCC0QCCQ0.O0OOQQCOQ0 of
OO0COQCOQ0:begin
O0CC0QCCQ0.SubDataType:=dtBytes;
O0CC0QCCQ0.Fixed:=True;
end;
OCCQOQCOQ0:begin
O0CC0QCCQ0.SubDataType:=dtFixedChar;
O0CC0QCCQ0.Fixed:=True;
end;
OOO0OQCOQ0:begin
O0CC0QCCQ0.SubDataType:=O0COCCCCQ0;
O0CC0QCCQ0.Fixed:=True;
end;
OQO0OQCOQ0:
O0CC0QCCQ0.SubDataType:=OOCOCCCCQ0;
OQQ0OQCOQ0:begin
O0CC0QCCQ0.SubDataType:=dtFixedWideChar;
O0CC0QCCQ0.Fixed:=True;
end;
OCQ0OQCOQ0:
O0CC0QCCQ0.SubDataType:=dtWideString;
O0C0OQCOQ0:
O0CC0QCCQ0.SubDataType:=dtWideMemo;
end;
if OOOQ0OCCQ0(OQCC0QCCQ0)then
O0CC0QCCQ0.Length:=O0CC0QCCQ0.Length div TASEConnection(OOQCOCCCQ0.FConnection).UniCharSize
else if OCOQ0OCCQ0(O0CC0QCCQ0.SubDataType)then
O0CC0QCCQ0.Length:=O0CC0QCCQ0.Length div TASEConnection(OOQCOCCCQ0.FConnection).NCharSize;
OO000QCCQ0:=(OCCC0QCCQ0.OOOOQQCOQ0.OOOCCQCOQ0=OQOQ0QCOQ0)and((O0CC0QCCQ0.Length=0)or(O0CC0QCCQ0.Length>4000));
O0CC0QCCQ0.DBLength:=O0CC0QCCQ0.Length;
O0CC0QCCQ0.DBScale:=O0CC0QCCQ0.Scale;
O0CC0QCCQ0.DBType:=O00C0QCCQ0(OQCC0QCCQ0,O0CC0QCCQ0.SubDataType,OO000QCCQ0,O0CC0QCCQ0.Fixed,O0CC0QCCQ0.IsTimestamp,O0CC0QCCQ0.DBLength);
TASEConnection(OOQCOCCCQ0.FConnection).GetProp(prUseUnicode,OQ000QCCQ0);
if Boolean(OQ000QCCQ0)then
case O0CC0QCCQ0.DataType of
dtFixedChar:
O0CC0QCCQ0.DataType:=dtFixedWideChar;
dtString:
O0CC0QCCQ0.DataType:=dtWideString;
dtExtString:
O0CC0QCCQ0.DataType:=dtExtWideString;
dtMemo:
O0CC0QCCQ0.DataType:=dtWideMemo;
end;
{$IFNDEF LITE}
OC000QCCQ0:=GetMapFetchConverter(O0CC0QCCQ0.Name,O0CC0QCCQ0.DBType,O0CC0QCCQ0.DBLength,O0CC0QCCQ0.DBScale);
if OC000QCCQ0<>nil then
O0CC0QCCQ0.DataType:=OC000QCCQ0.InternalDataType;
{$ENDIF}
if(OOOQ0OCCQ0(OQCC0QCCQ0)or OCOQ0OCCQ0(O0CC0QCCQ0.SubDataType))and
(O0CC0QCCQ0.DataType in[dtFixedChar,dtString,dtExtString])and
(OQOCO000Q0(OOQCOCCCQ0.GetProtocol).O00O0000Q0>1)
then
O0CC0QCCQ0.Length:=O0CC0QCCQ0.Length*OQOCO000Q0(OOQCOCCCQ0.GetProtocol).O00O0000Q0;
if O0CC0QCCQ0.DataType in[dtFMTBCD]then begin
if(O0CC0QCCQ0.Length>MaxFMTBcdDigits)or(O0CC0QCCQ0.Length<=0)then
O0CC0QCCQ0.Length:=MaxFMTBcdDigits;
if O0CC0QCCQ0.Scale>O0CC0QCCQ0.Length then
O0CC0QCCQ0.Scale:=O0CC0QCCQ0.Length;
{$IFDEF FPC}
if O0CC0QCCQ0.Scale>MaxBcdScale then
O0CC0QCCQ0.Scale:=MaxBcdScale;
{$ENDIF}
end
else if O0CC0QCCQ0.DataType=dtBCD then begin
if(O0CC0QCCQ0.Length>MaxBcdPrecision)or(O0CC0QCCQ0.Length<=0)then
O0CC0QCCQ0.Length:=19
else
O0CC0QCCQ0.Length:=O0CC0QCCQ0.Length;
if O0CC0QCCQ0.Scale>MaxBcdScale then
O0CC0QCCQ0.Scale:=MaxBcdScale;
end
else if O0CC0QCCQ0.DataType in[dtString,dtWideString,dtVarBytes]then begin
if not FFlatBuffers and(O0CC0QCCQ0.Length>=FlatBufferLimit)then
case O0CC0QCCQ0.DataType of
dtString:
O0CC0QCCQ0.DataType:=dtExtString;
dtWideString:
O0CC0QCCQ0.DataType:=dtExtWideString;
dtVarBytes:
O0CC0QCCQ0.DataType:=dtExtVarBytes;
end;
end;
if O0CC0QCCQ0.DataType in[dtBCD,dtFmtBCD]then begin
if not(O0CC0QCCQ0.SubDataType in[dtCurrency,dtBCD])then
O0CC0QCCQ0.SubDataType:=sdtNumeric;
if OOQCOCCCQ0.FSensibleBCDMapping and
OOQCOCCCQ0.CalcEnableBCD and
(OCCC0QCCQ0.O0C0QQCOQ0<=MaxBCDPrecision-MaxBCDScale+1)and
(OCCC0QCCQ0.OOC0QQCOQ0<=MaxBCDScale)
then
O0CC0QCCQ0.DataType:=dtBCD
else if not(O0CC0QCCQ0.SubDataType in[dtCurrency,dtBCD])and OOQCOCCCQ0.CalcEnableFMTBCD then
O0CC0QCCQ0.DataType:=dtFmtBCD
else if not OOQCOCCCQ0.FSensibleBCDMapping and OOQCOCCCQ0.CalcEnableBCD then
O0CC0QCCQ0.DataType:=dtBCD
else if O0CC0QCCQ0.SubDataType in[dtCurrency,dtBCD]then
O0CC0QCCQ0.DataType:=dtCurrency
else
O0CC0QCCQ0.DataType:=dtFloat;
end;
O0CC0QCCQ0.Size:=GetBufferSize(O0CC0QCCQ0.DataType,O0CC0QCCQ0.Length);
{$IFDEF LOG_PACKETS}
AddToLog(Format('DescribeFieldDesc field[%d] %s, %s, dt %s, TableName %s, size %d, length %d, scale %d, readonly %d, required %d, IsAutoIncrement %d, hidden %d, IsKey %d, IsTimestamp %d, fixed %d, SubDataType %d',
[O0CC0QCCQ0.FieldNo,O0CC0QCCQ0.Name,O0CC0QCCQ0.ActualName,DataTypeToName[O0CC0QCCQ0.DataType],OCCC0QCCQ0.OO0OQQCOQ0,O0CC0QCCQ0.Size,O0CC0QCCQ0.Length,O0CC0QCCQ0.Scale,
Integer(O0CC0QCCQ0.ReadOnly),Integer(O0CC0QCCQ0.Required),Integer(O0CC0QCCQ0.IsAutoIncrement),
Integer(O0CC0QCCQ0.Hidden),Integer(O0CC0QCCQ0.IsKey),Integer(O0CC0QCCQ0.IsTimestamp),Integer(O0CC0QCCQ0.Fixed),O0CC0QCCQ0.SubDataType]));
{$ENDIF}
end;
{$IFNDEF LITE}
constructor TTDS5MetaDataCommand.Create;
begin
inherited;
OQQ00QCCQ0:=TMetaDataArgs.Create;
end;
destructor TTDS5MetaDataCommand.Destroy;
begin
OQQ00QCCQ0.Free;
inherited;
end;
procedure TTDS5MetaDataRecordSet.CreateCommand;
begin
SetCommand(TTDS5MetaDataCommand.Create);
end;
function TTDS5MetaData.CreateRecordSet:TCRRecordSet;
begin
FRecordSet:=TTDS5RecordSet.Create;
Result:=FRecordSet;
end;
function TTDS5MetaData.O0C00QCCQ0(const OOC00QCCQ0:string):string;
begin
Result:=Trim(OOC00QCCQ0);
if Result='' then
Result:='NULL'
else
Result:=''''+Result+'''';
end;
function TTDS5MetaData.O0CO0QCCQ0(OOCO0QCCQ0:TStrings):string;
begin
Result:=Trim(OOCO0QCCQ0.Values['TABLE_CATALOG']);
if Result='' then
Result:=FRecordSet.GetConnection.Database;
end;
function TTDS5MetaData.OQCO0QCCQ0(OCCO0QCCQ0:TStrings):string;
begin
Result:=Trim(OCCO0QCCQ0.Values['TABLE_SCHEMA']);
if Result='' then
Result:='dbo';
end;
function TTDS5MetaData.GetColumns(O0CQCQCCQ0:TStrings):TData;
var
OOCQCQCCQ0,OQCQCQCCQ0,OCCQCQCCQ0,O00CCQCCQ0:string;
const
OO0CCQCCQ0='exec %s.%s.sp_odbc_columns %s, %s';
OQ0CCQCCQ0=1;
OC0CCQCCQ0=2;
O0OCCQCCQ0=3;
OOOCCQCCQ0=4;
OQOCCQCCQ0=17;
OCOCCQCCQ0=5;
O0QCCQCCQ0=7;
OOQCCQCCQ0=8;
OQQCCQCCQ0=10;
OCQCCQCCQ0=9;
O0CCCQCCQ0=1;
OOCCCQCCQ0=2;
OQCCCQCCQ0=3;
OCCCCQCCQ0=4;
O000CQCCQ0=5;
OO00CQCCQ0=6;
OQ00CQCCQ0=7;
OC00CQCCQ0=8;
O0O0CQCCQ0=9;
begin
OOCQCQCCQ0:=O0CO0QCCQ0(O0CQCQCCQ0);
OQCQCQCCQ0:=OQCO0QCCQ0(O0CQCQCCQ0);
OCCQCQCCQ0:=O0C00QCCQ0(O0CQCQCCQ0.Values['TABLE_NAME']);
O00CCQCCQ0:=O0C00QCCQ0(O0CQCQCCQ0.Values['COLUMN_NAME']);
if O00CCQCCQ0='NULL' then
O00CCQCCQ0:=O0C00QCCQ0('%');
FMemData.Fields.Clear;
AddField('TABLE_CATALOG',dtString,100);
AddField('TABLE_SCHEMA',dtString,100);
AddField('TABLE_NAME',dtString,100);
AddField('COLUMN_NAME',dtString,100);
AddField('POSITION',dtInt32);
AddField('DATA_TYPE',dtString,50);
AddField('DATA_LENGTH',dtInt32);
AddField('DATA_PRECISION',dtInt32);
AddField('DATA_SCALE',dtInt32);
FMemData.InitFields;
FMemData.Open;
FRecordSet.SetSQL(Format(OO0CCQCCQ0,[OOCQCQCCQ0,OQCQCQCCQ0,OCCQCQCCQ0,O00CCQCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
CopyRecord([OQ0CCQCCQ0,OC0CCQCCQ0,O0OCCQCCQ0,OOOCCQCCQ0,OQOCCQCCQ0,OCOCCQCCQ0,O0QCCQCCQ0,OQQCCQCCQ0,OCQCCQCCQ0],
[O0CCCQCCQ0,OOCCCQCCQ0,OQCCCQCCQ0,OCCCCQCCQ0,O000CQCCQ0,OO00CQCCQ0,OQ00CQCCQ0,OC00CQCCQ0,O0O0CQCCQ0]);
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetIndexes(OQO0CQCCQ0:TStrings):TData;
var
OCO0CQCCQ0,O0Q0CQCCQ0,OOQ0CQCCQ0,OQQ0CQCCQ0,OCQ0CQCCQ0:string;
const
O0C0CQCCQ0='exec %s.%s.sp_odbc_getindexinfo ''%s'', %s, %s, %s, %s';
OOC0CQCCQ0=1;
OQC0CQCCQ0=2;
OCC0CQCCQ0=3;
O00OCQCCQ0=4;
OO0OCQCCQ0=5;
OQ0OCQCCQ0=6;
OC0OCQCCQ0=7;
O0OOCQCCQ0=8;
OOOOCQCCQ0=9;
OQOOCQCCQ0=10;
OCOOCQCCQ0=1;
O0QOCQCCQ0=2;
OOQOCQCCQ0=3;
OQQOCQCCQ0=4;
OCQOCQCCQ0=5;
O0COCQCCQ0=6;
OOCOCQCCQ0=7;
OQCOCQCCQ0=0;
begin
OCO0CQCCQ0:=O0CO0QCCQ0(OQO0CQCCQ0);
O0Q0CQCCQ0:=OQCO0QCCQ0(OQO0CQCCQ0);
OOQ0CQCCQ0:=OQO0CQCCQ0.Values['TABLE_NAME'];
if OOQ0CQCCQ0='' then
raise Exception.CreateFmt(SRestrictionMustBeSet,['TABLE_NAME']);
OQQ0CQCCQ0:=O0C00QCCQ0(OQO0CQCCQ0.Values['COLUMN_NAME']);
if OQQ0CQCCQ0='NULL' then
OQQ0CQCCQ0:=O0C00QCCQ0('%');
OCQ0CQCCQ0:=O0C00QCCQ0('N');
CreateIndexesFields;
FMemData.Open;
FRecordSet.SetSQL(Format(O0C0CQCCQ0,[OCO0CQCCQ0,O0Q0CQCCQ0,OOQ0CQCCQ0,O0Q0CQCCQ0,OCO0CQCCQ0,OQQ0CQCCQ0,OCQ0CQCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do
if not VarIsNull(FRecordSetHelper.FieldValues[OQ0OCQCCQ0])then begin
FMemDataHelper.InitRecord;
CopyRecord([OOC0CQCCQ0,OQC0CQCCQ0,OCC0CQCCQ0,OOC0CQCCQ0,OQC0CQCCQ0,OQ0OCQCCQ0],
[OCOOCQCCQ0,O0QOCQCCQ0,OOQOCQCCQ0,OQQOCQCCQ0,OCQOCQCCQ0,O0COCQCCQ0]);
if FRecordSetHelper.FieldValues[O00OCQCCQ0]=OQCOCQCCQ0 then
FMemDataHelper.FieldValues[OOCOCQCCQ0]:=1
else
FMemDataHelper.FieldValues[OOCOCQCCQ0]:=0;
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetConstraints(O0QCOOCCQ0:TStrings):TData;
var
OOQCOOCCQ0,OQQCOOCCQ0,OCQCOOCCQ0:string;
const
O0CCOOCCQ0='exec sp_odbc_primarykey %s, %s, %s';
OOCCOOCCQ0=1;
OQCCOOCCQ0=2;
OCCCOOCCQ0=3;
O000OOCCQ0=4;
OO00OOCCQ0=5;
OQ00OOCCQ0=6;
OC00OOCCQ0=1;
O0O0OOCCQ0=2;
OOO0OOCCQ0=3;
OQO0OOCCQ0=4;
OCO0OOCCQ0=5;
O0Q0OOCCQ0=6;
OOQ0OOCCQ0=7;
begin
OOQCOOCCQ0:=O0CO0QCCQ0(O0QCOOCCQ0);
OQQCOOCCQ0:=OQCO0QCCQ0(O0QCOOCCQ0);
OCQCOOCCQ0:=O0QCOOCCQ0.Values['TABLE_NAME'];
if OCQCOOCCQ0='' then
raise Exception.CreateFmt(SRestrictionMustBeSet,['TABLE_NAME']);
OCQCOOCCQ0:=O0C00QCCQ0(OCQCOOCCQ0);
FMemData.Fields.Clear;
AddField('CONSTRAINT_CATALOG',dtString,100);
AddField('CONSTRAINT_SCHEMA',dtString,100);
AddField('CONSTRAINT_NAME',dtString,100);
AddField('TABLE_CATALOG',dtString,100);
AddField('TABLE_SCHEMA',dtString,100);
AddField('TABLE_NAME',dtString,100);
AddField('CONSTRAINT_TYPE',dtString,100);
FMemData.InitFields;
FMemData.Open;
FRecordSet.SetSQL(Format(O0CCOOCCQ0,[OCQCOOCCQ0,OQQCOOCCQ0,OOQCOOCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
CopyRecord([OOCCOOCCQ0,OQCCOOCCQ0,OQ00OOCCQ0,OOCCOOCCQ0,OQCCOOCCQ0,OCCCOOCCQ0],
[OC00OOCCQ0,O0O0OOCCQ0,OOO0OOCCQ0,OQO0OOCCQ0,OCO0OOCCQ0,O0Q0OOCCQ0]);
FMemDataHelper.FieldValues[OOQ0OOCCQ0]:='PRIMARY KEY';
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetConstraintColumns(OCQ0OOCCQ0:TStrings):TData;
const
O0C0OOCCQ0='exec %s.%s.sp_odbc_getindexinfo ''%s'', NULL, NULL, ''%s'', ''Y''';
OOC0OOCCQ0=1;
OQC0OOCCQ0=2;
OCC0OOCCQ0=3;
O00OOOCCQ0=6;
OO0OOOCCQ0=8;
OQ0OOOCCQ0=9;
OC0OOOCCQ0=1;
O0OOOOCCQ0=2;
OOOOOOCCQ0=3;
OQOOOOCCQ0=4;
OCOOOOCCQ0=5;
O0QOOOCCQ0=6;
var
OOQOOOCCQ0,OQQOOOCCQ0,OCQOOOCCQ0,O0COOOCCQ0:string;
begin
OOQOOOCCQ0:=O0CO0QCCQ0(OCQ0OOCCQ0);
OQQOOOCCQ0:=OQCO0QCCQ0(OCQ0OOCCQ0);
OCQOOOCCQ0:=Trim(OCQ0OOCCQ0.Values['TABLE_NAME']);
if OCQOOOCCQ0='' then
raise Exception.CreateFmt(SRestrictionMustBeSet,['TABLE_NAME']);
O0COOOCCQ0:=Trim(OCQ0OOCCQ0.Values['CONSTRAINT_NAME']);
if O0COOOCCQ0='' then
O0COOOCCQ0:='%';
CreateConstraintColumnsFields;
FMemData.Open;
FMemDataHelper.AllocBuffer;
FRecordSet.SetSQL(Format(O0C0OOCCQ0,[OOQOOOCCQ0,OQQOOOCCQ0,OCQOOOCCQ0,O0COOOCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do
if not VarIsNull(FRecordSetHelper.FieldValues[OQ0OOOCCQ0])then begin
FMemDataHelper.InitRecord;
CopyRecord([OOC0OOCCQ0,OQC0OOCCQ0,OCC0OOCCQ0,O00OOOCCQ0,OQ0OOOCCQ0,OO0OOOCCQ0],
[OC0OOOCCQ0,O0OOOOCCQ0,OOOOOOCCQ0,OQOOOOCCQ0,OCOOOOCCQ0,O0QOOOCCQ0]);
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetDatabases(OQCOOOCCQ0:TStrings):TData;
begin
FRecordSet.SetSQL('SELECT name AS DATABASE_NAME FROM master.dbo.sysdatabases');
FRecordSet.Open;
Result:=FRecordSet;
end;
function TTDS5MetaData.GetIndexColumns(O0COQQCCQ0:TStrings):TData;
var
OOCOQQCCQ0,OQCOQQCCQ0,OCCOQQCCQ0,O00QOOCCQ0,OO0QOOCCQ0,OQ0QOOCCQ0:string;
const
OC0QOOCCQ0='exec sp_odbc_getindexinfo ''%s'', %s, %s, ''%s'', %s';
O0OQOOCCQ0=1;
OOOQOOCCQ0=2;
OQOQOOCCQ0=3;
OCOQOOCCQ0=4;
O0QQOOCCQ0=5;
OOQQOOCCQ0=6;
OQQQOOCCQ0=7;
OCQQOOCCQ0=8;
O0CQOOCCQ0=9;
OOCQOOCCQ0=10;
OQCQOOCCQ0=1;
OCCQOOCCQ0=2;
O00COOCCQ0=3;
OO0COOCCQ0=4;
OQ0COOCCQ0=5;
OC0COOCCQ0=6;
O0OCOOCCQ0=7;
OOOCOOCCQ0=8;
OQOCOOCCQ0=9;
begin
OOCOQQCCQ0:=O0CO0QCCQ0(O0COQQCCQ0);
OQCOQQCCQ0:=OQCO0QCCQ0(O0COQQCCQ0);
OCCOQQCCQ0:=Trim(O0COQQCCQ0.Values['TABLE_NAME']);
if OCCOQQCCQ0='' then
raise Exception.CreateFmt(SRestrictionMustBeSet,['TABLE_NAME']);
O00QOOCCQ0:=Trim(O0COQQCCQ0.Values['INDEX_NAME']);
if O00QOOCCQ0='' then
O00QOOCCQ0:='%';
OO0QOOCCQ0:=O0C00QCCQ0('Y');
CreateIndexColumnsFields;
FMemData.Open;
FRecordSet.SetSQL(Format(OC0QOOCCQ0,[OCCOQQCCQ0,OQCOQQCCQ0,OOCOQQCCQ0,O00QOOCCQ0,OO0QOOCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do
if not VarIsNull(FRecordSetHelper.FieldValues[OOQQOOCCQ0])then begin
FMemDataHelper.InitRecord;
CopyRecord(
[O0OQOOCCQ0,OOOQOOCCQ0,OQOQOOCCQ0,O0OQOOCCQ0,OOOQOOCCQ0,OOQQOOCCQ0,O0CQOOCCQ0,OCQQOOCCQ0],
[OQCQOOCCQ0,OCCQOOCCQ0,O00COOCCQ0,OO0COOCCQ0,OQ0COOCCQ0,OC0COOCCQ0,O0OCOOCCQ0,OOOCOOCCQ0]);
OQ0QOOCCQ0:=FRecordSetHelper.FieldValues[OOCQOOCCQ0];
if OQ0QOOCCQ0='D' then
OQ0QOOCCQ0:='DESC'
else
OQ0QOOCCQ0:='ASC';
FMemDataHelper.FieldValues[OQOCOOCCQ0]:=OQ0QOOCCQ0;
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetProcedures(O00QQQCCQ0:TStrings):TData;
const
OO0QQQCCQ0='exec sp_jdbc_stored_procedures ''%s'', ''%s'', NULL, NULL, %d';
OQ0QQQCCQ0=1;
OC0QQQCCQ0=2;
O0OQQQCCQ0=3;
OOOQQQCCQ0=6;
OQOQQQCCQ0=1;
OCOQQQCCQ0=2;
O0QQQQCCQ0=3;
OOQQQQCCQ0=4;
OQQQQQCCQ0=2;
var
OCQQQQCCQ0,O0CQQQCCQ0:string;
OOCQQQCCQ0:TStringArray;
begin
OCQQQQCCQ0:=O0CO0QCCQ0(O00QQQCCQ0);
O0CQQQCCQ0:=OQCO0QCCQ0(O00QQQCCQ0);
SetLength(OOCQQQCCQ0,0);
CreateProceduresFields;
FMemData.Open;
FRecordSet.SetSQL(Format(OO0QQQCCQ0,[OCQQQQCCQ0,O0CQQQCCQ0,0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
FMemDataHelper.FieldValues[OQOQQQCCQ0]:=FRecordSetHelper.FieldValues[OQ0QQQCCQ0];
FMemDataHelper.FieldValues[OCOQQQCCQ0]:=FRecordSetHelper.FieldValues[OC0QQQCCQ0];
OOCQQQCCQ0:=SplitString(FRecordSetHelper.FieldValues[O0OQQQCCQ0],';');
FMemDataHelper.FieldValues[O0QQQQCCQ0]:=OOCQQQCCQ0[0];
FMemDataHelper.FieldValues[OOQQQQCCQ0]:='PROCEDURE';
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FRecordSet.SetSQL(Format(OO0QQQCCQ0,[OCQQQQCCQ0,O0CQQQCCQ0,1]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
FMemDataHelper.FieldValues[OQOQQQCCQ0]:=FRecordSetHelper.FieldValues[OQ0QQQCCQ0];
FMemDataHelper.FieldValues[OCOQQQCCQ0]:=FRecordSetHelper.FieldValues[OC0QQQCCQ0];
OOCQQQCCQ0:=SplitString(FRecordSetHelper.FieldValues[OOOQQQCCQ0],';');
FMemDataHelper.FieldValues[O0QQQQCCQ0]:=OOCQQQCCQ0[0];
FMemDataHelper.FieldValues[OOQQQQCCQ0]:='FUNCTION';
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetProcedureParameters(OCCQQQCCQ0:TStrings):TData;
const
O00CQQCCQ0='exec sp_odbc_getprocedurecolumns %s, %s, %s, ''%s''';
OO0CQQCCQ0='exec sp_jdbc_getfunctioncolumns %s, %s, %s, %s';
OQ0CQQCCQ0=1;
OC0CQQCCQ0=2;
O0OCQQCCQ0=3;
OOOCQQCCQ0=4;
OQOCQQCCQ0=18;
OCOCQQCCQ0=5;
O0QCQQCCQ0=7;
OOQCQQCCQ0=8;
OQQCQQCCQ0=10;
OCQCQQCCQ0=1;
O0CCQQCCQ0=2;
OOCCQQCCQ0=3;
OQCCQQCCQ0=4;
OCCCQQCCQ0=15;
O000QQCCQ0=5;
OO00QQCCQ0=7;
OQ00QQCCQ0=8;
OC00QQCCQ0=9;
O0O0QQCCQ0=10;
OOO0QQCCQ0=1;
OQO0QQCCQ0=2;
OCO0QQCCQ0=3;
O0Q0QQCCQ0=4;
OOQ0QQCCQ0=5;
OQQ0QQCCQ0=6;
OCQ0QQCCQ0=7;
O0C0QQCCQ0=8;
OOC0QQCCQ0=9;
OQC0QQCCQ0=10;
OCC0QQCCQ0=1;
O00OQQCCQ0=2;
OO0OQQCCQ0=3;
OQ0OQQCCQ0=4;
OC0OQQCCQ0=5;
var
O0OOQQCCQ0,OOOOQQCCQ0,OQOOQQCCQ0,OCOOQQCCQ0,O0QOQQCCQ0:string;
OOQOQQCCQ0:integer;
OQQOQQCCQ0:TStringArray;
begin
O0OOQQCCQ0:=O0CO0QCCQ0(OCCQQQCCQ0);
OOOOQQCCQ0:=OQCO0QCCQ0(OCCQQQCCQ0);
OQOOQQCCQ0:=O0C00QCCQ0(OCCQQQCCQ0.Values['PROCEDURE_NAME']);
OCOOQQCCQ0:=OCCQQQCCQ0.Values['PARAMETER_NAME'];
SetLength(OQQOQQCCQ0,0);
CreateProcedureParametersFields;
FMemData.Open;
if OCOOQQCCQ0='' then
OCOOQQCCQ0:='%';
FRecordSet.SetSQL(Format(O00CQQCCQ0,[OQOOQQCCQ0,OOOOQQCCQ0,O0OOQQCCQ0,OCOOQQCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
OOQOQQCCQ0:=FRecordSetHelper.FieldValues[OCOCQQCCQ0];
if OOQOQQCCQ0=OO0OQQCCQ0 then
continue;
FMemDataHelper.InitRecord;
CopyRecord(
[OQ0CQQCCQ0,OC0CQQCCQ0,O0OCQQCCQ0,OOOCQQCCQ0,O0QCQQCCQ0,OOQCQQCCQ0,OOQCQQCCQ0,OQQCQQCCQ0],
[OOO0QQCCQ0,OQO0QQCCQ0,OCO0QQCCQ0,O0Q0QQCCQ0,OCQ0QQCCQ0,O0C0QQCCQ0,OOC0QQCCQ0,OQC0QQCCQ0]);
FMemDataHelper.FieldValues[OOQ0QQCCQ0]:=FRecordSetHelper.FieldValues[OQOCQQCCQ0];
case OOQOQQCCQ0 of
OCC0QQCCQ0:
O0QOQQCCQ0:='IN';
O00OQQCCQ0:
O0QOQQCCQ0:='IN/OUT';
OQ0OQQCCQ0,OC0OQQCCQ0:
O0QOQQCCQ0:='OUT';
end;
FMemDataHelper.FieldValues[OQQ0QQCCQ0]:=O0QOQQCCQ0;
FMemDataHelper.FieldValues[OOQ0QQCCQ0]:=FRecordSetHelper.FieldValues[OQOCQQCCQ0];
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
if FMemData.RecordCount=0 then begin
if OCOOQQCCQ0='%' then
OCOOQQCCQ0:='NULL';
FRecordSet.SetSQL(Format(OO0CQQCCQ0,[O0OOQQCCQ0,OOOOQQCCQ0,OQOOQQCCQ0,OCOOQQCCQ0]));
FRecordSet.Open;
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
OOQOQQCCQ0:=FRecordSetHelper.FieldValues[OCOCQQCCQ0];
if OOQOQQCCQ0=OO0OQQCCQ0 then
continue;
FMemDataHelper.InitRecord;
CopyRecord(
[OCQCQQCCQ0,O0CCQQCCQ0,OQCCQQCCQ0,OO00QQCCQ0,OC00QQCCQ0,OQ00QQCCQ0,O0O0QQCCQ0],
[OOO0QQCCQ0,OQO0QQCCQ0,O0Q0QQCCQ0,OCQ0QQCCQ0,O0C0QQCCQ0,OOC0QQCCQ0,OQC0QQCCQ0]);
OQQOQQCCQ0:=SplitString(FRecordSetHelper.FieldValues[OOCCQQCCQ0],'.');
FMemDataHelper.FieldValues[OCO0QQCCQ0]:=OQQOQQCCQ0[1];
FMemDataHelper.FieldValues[OOQ0QQCCQ0]:=FRecordSetHelper.FieldValues[OCCCQQCCQ0];
case OOQOQQCCQ0 of
OCC0QQCCQ0:
O0QOQQCCQ0:='IN';
O00OQQCCQ0:
O0QOQQCCQ0:='IN/OUT';
OQ0OQQCCQ0,OC0OQQCCQ0:
O0QOQQCCQ0:='OUT';
end;
FMemDataHelper.FieldValues[OQQ0QQCCQ0]:=O0QOQQCCQ0;
FMemDataHelper.FieldValues[OOQ0QQCCQ0]:=FRecordSet.RecordNo;
FMemDataHelper.AppendRecord;
end;
FRecordSet.Close;
end;
FMemData.SetToBegin;
Result:=FMemData;
end;
function TTDS5MetaData.GetTables(OQ0QCQCCQ0:TStrings):TData;
var
OC0QCQCCQ0,O0OQCQCCQ0,OOOQCQCCQ0,OQOQCQCCQ0,OCOQCQCCQ0:string;
O0QQCQCCQ0:TStringList;
OOQQCQCCQ0:integer;
const
OQQQCQCCQ0='exec %s..sp_odbc_tables ''%s'', ''%s'', ''%s'', "%s"';
begin
OC0QCQCCQ0:=O0CO0QCCQ0(OQ0QCQCCQ0);
O0OQCQCCQ0:=OQCO0QCCQ0(OQ0QCQCCQ0);
OOOQCQCCQ0:=trim(OQ0QCQCCQ0.Values['TABLE_TYPE']);
if OOOQCQCCQ0='' then
OOOQCQCCQ0:='TABLE,SYSTEM TABLE,VIEW,SYNONYM,ALIAS';
OQOQCQCCQ0:=Trim(OQ0QCQCCQ0.Values['TABLE_NAME']);
if OQOQCQCCQ0='' then
OQOQCQCCQ0:='%';
OCOQCQCCQ0:='';
O0QQCQCCQ0:=TStringList.Create;
try
ParseTypes(OOOQCQCCQ0,O0QQCQCCQ0);
for OOQQCQCCQ0:=0 to O0QQCQCCQ0.Count-1 do begin
if OCOQCQCCQ0<>'' then
OCOQCQCCQ0:=OCOQCQCCQ0+',';
OCOQCQCCQ0:=OCOQCQCCQ0+''''+O0QQCQCCQ0[OOQQCQCCQ0]+'''';
end;
finally
O0QQCQCCQ0.Free;
end;
CreateTablesFields;
FMemData.Open;
FRecordSet.SetSQL(Format(OQQQCQCCQ0,[OC0QCQCCQ0,OQOQCQCCQ0,O0OQCQCCQ0,OC0QCQCCQ0,OCOQCQCCQ0]));
FRecordSet.Open;
OQC00QCCQ0(OQ0QCQCCQ0);
FRecordSet.Close;
FMemData.SetToBegin;
Result:=FMemData;
end;
procedure TTDS5MetaData.OQC00QCCQ0(OCC00QCCQ0:TStrings);
const
O00O0QCCQ0=1;
OO0O0QCCQ0=2;
OQ0O0QCCQ0=3;
OC0O0QCCQ0=4;
O0OO0QCCQ0=8;
OOOO0QCCQ0=9;
OQOO0QCCQ0=1;
OCOO0QCCQ0=2;
O0QO0QCCQ0=3;
OOQO0QCCQ0=4;
OQQO0QCCQ0=5;
OCQO0QCCQ0=6;
begin
FMemDataHelper.AllocBuffer;
FRecordSetHelper.AllocBuffer;
while FRecordSetHelper.NextRecord do begin
FMemDataHelper.InitRecord;
CopyRecord([O00O0QCCQ0,OO0O0QCCQ0,OQ0O0QCCQ0,OC0O0QCCQ0],
[OQOO0QCCQ0,OCOO0QCCQ0,O0QO0QCCQ0,OOQO0QCCQ0]);
FMemDataHelper.AppendRecord;
end;
end;
{$ENDIF LITE}
function TTDS5Loader.OC0CCCCCQ0(OCCOOOCCQ0:TSqlFieldDesc;O00Q0OCCQ0:TParamDesc):boolean;
begin
Result:=(FSkipReadOnlyFieldDescs and OCCOOOCCQ0.ReadOnly)or OCCOOOCCQ0.IsTimestamp;
end;
function TTDS5Loader.SetProp(OO0Q0OCCQ0:integer;const OQ0Q0OCCQ0:variant):boolean;
begin
Result:=True;
case OO0Q0OCCQ0 of
prRowsPerBatch:
OQCQCCCCQ0:=OQ0Q0OCCQ0;
else
Result:=inherited SetProp(OO0Q0OCCQ0,OQ0Q0OCCQ0);
end;
end;
function TTDS5Loader.GetProp(OC0Q0OCCQ0:integer;var O0OQ0OCCQ0:variant):boolean;
begin
Result:=True;
case OC0Q0OCCQ0 of
prRowsPerBatch:
O0OQ0OCCQ0:=OQCQCCCCQ0;
else
Result:=inherited GetProp(OC0Q0OCCQ0,O0OQ0OCCQ0);
end;
end;
initialization
OQQQ0OCCQ0:=TCriticalSection.Create;
FillChar(@OCQQ0OCCQ0[0],SizeOf(OCQQ0OCCQ0),0);
OCQQ0OCCQ0[OCQ0QCCOQ0]:=dtUnknown;
OCQQ0OCCQ0[O0C0QCCOQ0]:=dtBlob;
OCQQ0OCCQ0[OOC0QCCOQ0]:=dtMemo;
OCQQ0OCCQ0[OCC0QCCOQ0]:=dtVarBytes;
OCQQ0OCCQ0[O00OQCCOQ0]:=dtInt64;
OCQQ0OCCQ0[OO0OQCCOQ0]:=dtString;
OCQQ0OCCQ0[OCQOQCCOQ0]:=dtBoolean;
OCQQ0OCCQ0[O0OQOQCOQ0]:=dtFmtBCD;
OCQQ0OCCQ0[OOOQOQCOQ0]:=dtFmtBCD;
OCQQ0OCCQ0[OOQOQCCOQ0]:=dtDate;
OCQQ0OCCQ0[OQQOQCCOQ0]:=dtTime;
OCQQ0OCCQ0[OQQQOQCOQ0]:=dtDate;
OCQQ0OCCQ0[O0CQOQCOQ0]:=dtTime;
OCQQ0OCCQ0[OQCOQCCOQ0]:=dtDateTime;
OCQQ0OCCQ0[OO0QOQCOQ0]:=dtDateTime;
OCQQ0OCCQ0[O0QQOQCOQ0]:=dtDateTime;
OCQQ0OCCQ0[OOCQOQCOQ0]:={$IFNDEF FPC}dtSQLTimeStamp{$ELSE}dtDateTime{$ENDIF};
OCQQ0OCCQ0[OQCQOQCOQ0]:=dtTime;
OCQQ0OCCQ0[OQOOQCCOQ0]:=dtBytes;
OCQQ0OCCQ0[OCOOQCCOQ0]:=dtString;
OCQQ0OCCQ0[O0QOQCCOQ0]:=dtUInt8;
OCQQ0OCCQ0[O0COQCCOQ0]:=dtInt16;
OCQQ0OCCQ0[OOCOQCCOQ0]:=dtInt32;
OCQQ0OCCQ0[OCCOQCCOQ0]:=dtSingle;
OCQQ0OCCQ0[OQ0QOQCOQ0]:=dtFloat;
OCQQ0OCCQ0[OC0QOQCOQ0]:=dtBoolean;
OCQQ0OCCQ0[OQOQOQCOQ0]:=dtFloat;
OCQQ0OCCQ0[O00QOQCOQ0]:=dtBCD;
OCQQ0OCCQ0[OCOQOQCOQ0]:=dtBCD;
OCQQ0OCCQ0[OOQQOQCOQ0]:=dtBCD;
OCQQ0OCCQ0[OOO0OOCOQ0]:=dtBlob;
OCQQ0OCCQ0[OCO0OOCOQ0]:=dtByte;
OCQQ0OCCQ0[O0Q0OOCOQ0]:=dtWord;
OCQQ0OCCQ0[OOQ0OOCOQ0]:=dtUInt32;
OCQQ0OCCQ0[OQQ0OOCOQ0]:=dtUInt64;
OCQQ0OCCQ0[OCQ0OOCOQ0]:=dtUInt64;
OCQQ0OCCQ0[OQQQOQCOQ0]:=dtDate;
OCQQ0OCCQ0[O0CQOQCOQ0]:=dtTime;
OCQQ0OCCQ0[OQC0OOCOQ0]:=dtXML;
OCQQ0OCCQ0[OCC0OOCOQ0]:=dtWideMemo;
OCQQ0OCCQ0[O00OOOCOQ0]:=dtExtString;
OCQQ0OCCQ0[OO0OOOCOQ0]:=dtInt8;
OCQQ0OCCQ0[OQ0OOOCOQ0]:=dtInt64;
OCQQ0OCCQ0[OC0OOOCOQ0]:=dtBytes;
FillChar(@O0CQ0OCCQ0[dtUnknown],sizeof(O0CQ0OCCQ0),0);
O0CQ0OCCQ0[dtUnknown]:=OCQ0QCCOQ0;
O0CQ0OCCQ0[dtInt8]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtUInt8]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtInt16]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtUInt16]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtWord]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtSmallint]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtInt32]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtInteger]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtInt64]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtLargeint]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtUInt32]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtLongWord]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtUInt64]:=O00OQCCOQ0;
O0CQ0OCCQ0[dtBoolean]:=OCQOQCCOQ0;
O0CQ0OCCQ0[dtFloat]:=OQOQOQCOQ0;
O0CQ0OCCQ0[dtSingle]:=OQOQOQCOQ0;
O0CQ0OCCQ0[dtCurrency]:=OCOQOQCOQ0;
O0CQ0OCCQ0[dtDate]:=O0QQOQCOQ0;
O0CQ0OCCQ0[dtTime]:=O0QQOQCOQ0;
O0CQ0OCCQ0[dtDateTime]:=O0QQOQCOQ0;
O0CQ0OCCQ0[dtBlob]:=OOO0OOCOQ0;
O0CQ0OCCQ0[dtArray]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtBytes]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtVarBytes]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtExtVarBytes]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtMemo]:=OOO0OOCOQ0;
O0CQ0OCCQ0[dtString]:=O00OOOCOQ0;
O0CQ0OCCQ0[dtExtString]:=O00OOOCOQ0;
O0CQ0OCCQ0[dtWideString]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtExtWideString]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtFixedChar]:=O00OOOCOQ0;
O0CQ0OCCQ0[dtFixedWideChar]:=OC0OOOCOQ0;
O0CQ0OCCQ0[dtBCD]:=OQOQOQCOQ0;
O0CQ0OCCQ0[dtFMTBCD]:=OOOQOQCOQ0;
O0CQ0OCCQ0[dtGuid]:=OO0OQCCOQ0;
O0CQ0OCCQ0[dtWideMemo]:=OOO0OOCOQ0;
O0CQ0OCCQ0[dtSQLTimeStamp]:=O0QQOQCOQ0;
O0CQ0OCCQ0[dtExtended]:=OQOQOQCOQ0;
O0CQ0OCCQ0[dtXML]:=OQC0OOCOQ0;
FillChar(@OOCQ0OCCQ0[dtUnknown],sizeof(OOCQ0OCCQ0),0);
OOCQ0OCCQ0[dtInt8]:=OC0COQCOQ0;
OOCQ0OCCQ0[dtUInt8]:=OC0COQCOQ0;
OOCQ0OCCQ0[dtBoolean]:=OOCCOQCOQ0;
OOCQ0OCCQ0[dtInt16]:=O0OCOQCOQ0;
OOCQ0OCCQ0[dtUInt16]:=O0OOOQCOQ0;
OOCQ0OCCQ0[dtInt32]:=OOOCOQCOQ0;
OOCQ0OCCQ0[dtUInt32]:=OOOOOQCOQ0;
OOCQ0OCCQ0[dtInt64]:=OQ0OOQCOQ0;
OOCQ0OCCQ0[dtUInt64]:=OOOOOQCOQ0;
OOCQ0OCCQ0[dtSingle]:=O0O0OQCOQ0;
OOCQ0OCCQ0[dtFloat]:=OQOCOQCOQ0;
OOCQ0OCCQ0[dtExtended]:=OQOCOQCOQ0;
OOCQ0OCCQ0[dtDate]:=OOC0OQCOQ0;
OOCQ0OCCQ0[dtTime]:=OQC0OQCOQ0;
OOCQ0OCCQ0[dtDateTime]:=OOQCOQCOQ0;
OOCQ0OCCQ0[dtCurrency]:=O0QCOQCOQ0;
OOCQ0OCCQ0[dtBCD]:=OCO0OQCOQ0;
OOCQ0OCCQ0[dtFMTBCD]:=OCO0OQCOQ0;
finalization
OQQQ0OCCQ0.Free;
end.
