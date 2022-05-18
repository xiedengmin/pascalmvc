
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEClassesUni;

interface

{$IFNDEF ODBC_PROVIDER}
  Error
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CLRClasses,
  Classes, SysUtils, Variants, SyncObjs,
{$IFNDEF FPC}
  FMTBcd,
{$ENDIF}
{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}
{$ENDIF}
  CRTypes, MemData, CRAccess, CRParser, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  {$IFDEF TDS}Tds5Classes,{$ENDIF}
  ODBCCall, ODBCClasses,
  ASEParser, ASEConnection;
{$ELSE}
  {$IFDEF TDS}Tds5ClassesUni,{$ENDIF}
  ODBCCallUni, ODBCClassesUni,
  ASEParserUni, ASEConnectionUni;
{$ENDIF}

type
{ TASETransaction }

  TASETransaction = class(TODBCTransaction)
  public
    procedure Savepoint(const Name: string); override;
    procedure ReleaseSavepoint(const Name: string); override;
    procedure RollbackToSavepoint(const Name: string); override;
  end;

{ TASEParamDesc }

  TASEParamDesc = class (TODBCParamDesc)
  public
    constructor Create; override;
  end;

{ TASECommand }

  TASECommand = class(TODBCCommand)
  protected
    procedure SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer); override;
    function DetectSpecificType(const TypeName: string; var SQLLength: Integer; var SQLScale: SmallInt): integer; override;
  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;
    class function GetParamDescClass: TParamDescClass; override;
  end;

{ TASERecordSet }

  TASERecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
    function NumericAsString: Boolean; override;
  end;

{ TASEMetaData }

  TASEMetaData = class(TODBCMetaData)
  protected
    FRecordSet2: TASERecordSet;

    procedure InternalGetMetaDataKindsList(List: TStringList); override;

    function GetDatabases(Restrictions: TStrings): TData; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$IFNDEF LITE}

{ TASELoader }

  TASELoader = class (TODBCLoader)
  protected
    procedure CreateCommand; override;
  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;
  end;

{$ENDIF}

implementation

uses
  CRProps, CRFunctions, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts,
  ASEProps, ASEDataTypeMap;
{$ELSE}
  ODBCConstsUni,
  ASEPropsUni, ASEDataTypeMapUni;
{$ENDIF}

{ TASETransaction }

procedure TASETransaction.Savepoint(const Name: string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecuteSQL('SAVE TRANSACTION ' + Name);
end;

procedure TASETransaction.ReleaseSavepoint(const Name: string);
begin
  raise Exception.Create(SOperationNotSupported);
end;

procedure TASETransaction.RollbackToSavepoint(const Name: string);
var
  Connection: TODBCConnection;
begin
  CheckActive;

  Connection := TODBCConnection(FConnections[0]);
  Connection.ExecuteSQL('ROLLBACK ' + Name);
end;

{ TASEParamDesc }

constructor TASEParamDesc.Create;
begin
  inherited;
  FEnableMSec := False;
end;

{ TASECommand }

class function TASECommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TASESQLInfo;
end;

class function TASECommand.GetParserClass: TSQLParserClass;
begin
  Result := TASEParser;
end;

class function TASECommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TASEMapRules;
end;

class function TASECommand.GetParamDescClass: TParamDescClass;
begin
  Result := TASEParamDesc;
end;

procedure TASECommand.SetSpecificParamPrec(Param: TODBCParamDesc; var Prec, Scale: integer);
begin
  case Param.GetDataType of
    dtDateTime:
      Prec := 19;
    else
      inherited;
  end;
end;

function TASECommand.DetectSpecificType(const TypeName: string; var SQLLength: Integer; var SQLScale: Smallint): integer;
begin
  if UpperCase(TypeName) = 'NUMERIC' then
    Result := SQL_NUMERIC
  else if UpperCase(TypeName) = 'IMAGE' then
    Result := SQL_LONGVARBINARY
  else
    Result := inherited DetectSpecificType(TypeName, SQLLength, SQLScale);
end;

{ TASERecordSet }

procedure TASERecordSet.CreateCommand;
begin
  SetCommand(TASECommand.Create);
end;

function TASERecordSet.NumericAsString: Boolean;
begin
  Result := True;
end;

{ TASEMetaData }

constructor TASEMetaData.Create;
begin
  inherited;

  FRecordSet2 := TASERecordSet.Create;
  FRecordSet2.SetProp(prFetchAll, True);
  FRecordSet2.SetProp(prFlatBuffers, False);
end;

destructor TASEMetaData.Destroy;
begin
  FRecordSet2.Free;

  inherited;
end;

procedure TASEMetaData.InternalGetMetaDataKindsList(List: TStringList);
begin
  inherited;

  List.Add('Databases');
  List.Sort;
end;

function TASEMetaData.GetDatabases(Restrictions: TStrings): TData;
const
  SQL = 'SELECT name AS DATABASE_NAME FROM master.dbo.sysdatabases';
begin
  FRecordSet2.SetConnection(FRecordSet.GetCommand.GetConnection);
  FRecordSet2.SetSQL(SQL);
  FRecordSet2.Open;
  Result := FRecordSet2;
end;

{$IFNDEF LITE}

{ TASELoader }

procedure TASELoader.CreateCommand;
begin
  FCommand := TASECommand.Create;
end;

{$ENDIF}

function TASELoader.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsPerBatch:
      ;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TASELoader.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prRowsPerBatch:
      Value := 0;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

end.
