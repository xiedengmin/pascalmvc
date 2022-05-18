
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I DB2Dac.inc}
unit DB2ClassesUni;

interface

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
  AnsiStrings,
{$ENDIF}
  CRTypes, MemData, CRAccess, CRParser, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCClasses;
{$ELSE}
  ODBCCallUni, ODBCClassesUni;
{$ENDIF}

type
  TDB2RecordSet = class;

{ TDB2Connection }

  TDB2Connection = class(TODBCConnection)
  private
    FDatabase: string;
    FPort: integer;
    FSchema: string;
    FFunctionPath: string;

  protected
    function GetConnectionString: string; override;
    procedure ApplyConnectProps; override;
  public
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetTransactionClass: TCRTransactionClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
    function GetCurrentSchema: string; override;
    procedure SetCurrentSchema(Value: string);
    function GetCachedSchema: string; override;
    procedure SetCurrentPath(Value: string);
  end;

{ TDB2Transaction }

  TDB2Transaction = class(TODBCTransaction)
  public
    procedure Savepoint(const Name: string); override;
  end;

{ TDB2Command }

  TDB2Command = class(TODBCCommand)
  public
    class function GetMapRulesClass: TCRMapRulesClass; override;
  end;

{ TDB2RecordSet }

  TDB2RecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;


{$IFNDEF LITE}
  TDB2Loader = class (TODBCLoader)
  protected
    procedure CreateCommand; override;
  end;
{$ENDIF}

implementation

uses
  CRProps, CRFunctions, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts, DB2Props, DB2DataTypeMap;
{$ELSE}
  ODBCConstsUni, DB2PropsUni, DB2DataTypeMapUni;
{$ENDIF}

{ TDB2Connection }

function TDB2Connection.GetConnectionString: string;
var
  Port: integer;
  Database: string;
begin
  Result := 'driver={IBM DB2 ODBC DRIVER}';

  Database := Trim(FDatabase);
  if Database <> '' then
    Result := Result + ';database=' + Database
  else
    raise Exception.Create('Database is not specified');

  if FServer <> '' then begin
    Port := FPort;
    if Port = 0 then
      Port := 50000;
    Result := Result + ';hostname=' + FServer +
                       ';port=' + IntToStr(Port) +
                       ';protocol=TCPIP';
  end;

  Result := Result + ';uid=' + FUsername +
                     ';pwd=' + FPassword;
end;

procedure TDB2Connection.ApplyConnectProps;
begin
  inherited;

  if FSchema <> '' then
    SetCurrentSchema(FSchema);
  if FFunctionPath <> '' then
    SetCurrentPath(FFunctionPath);
end;

function TDB2Connection.GetCommandClass: TCRCommandClass;
begin
  Result := TDB2Command;
end;

function TDB2Connection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TDB2RecordSet;
end;

function TDB2Connection.GetTransactionClass: TCRTransactionClass;
begin
  Result := TDB2Transaction;
end;

function TDB2Connection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TDB2Loader;
end;

class function TDB2Connection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TDB2Command.GetMapRulesClass;
end;

function TDB2Connection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prPort:
      FPort := Value;
    prSchema: begin
      if Value <> FSchema then begin
        if GetConnected then
          SetCurrentSchema(Value);
        FSchema := Value;
      end;
    end;
    prFunctionPath: begin
      if Value <> FFunctionPath then begin
        if GetConnected then
          SetCurrentPath(Value);
        FFunctionPath := Value;
      end;
    end;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TDB2Connection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prPort:
      Value := FPort;
    prSchema:
      Value := FSchema;
    prFunctionPath:
      Value := FFunctionPath;
  else
    Result := inherited GetProp(Prop, Value);
  end
end;

procedure TDB2Connection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TDB2Connection(Source).FDatabase;
  FPort := TDB2Connection(Source).FPort;
  FSchema := TDB2Connection(Source).FSchema;
  FFunctionPath := TDB2Connection(Source).FFunctionPath;
end;

function TDB2Connection.GetCurrentSchema: string;
var
  RecordSet: TCRRecordSet;
  RecBuf: IntPtr;
  v: variant;
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  RecordSet := OpenRecordSet('VALUES (CURRENT SCHEMA)');
  try
    RecordSet.AllocRecBuf(RecBuf);
    try
      RecordSet.GetNextRecord(RecBuf);
      if RecordSet.Eof then begin
        Result := '';
        exit;
      end;
      RecordSet.GetFieldAsVariant(RecordSet.Fields[0], RecBuf, v);
      Result := Trim(VarToStr(v));
    finally
      Marshal.FreeHGlobal(RecBuf);
    end;
  finally
    ReleaseRecordSet(RecordSet);
  end;
end;

procedure TDB2Connection.SetCurrentSchema(Value: string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    Value := FUsername;

  ExecuteSQL('SET SCHEMA ' + Value);
  FSchema := Value;
  FCachedSchema := '';
end;

function TDB2Connection.GetCachedSchema: string;
begin
  if FCachedSchema = '' then
    if FSchema <> '' then
      FCachedSchema := SQLInfo.NormalizeName(FSchema, False, True)
    else
      FCachedSchema := GetCurrentSchema;

  Result := FCachedSchema;
end;

procedure TDB2Connection.SetCurrentPath(Value: string);
begin
  if not FConnected then
    raise Exception.Create(SConnectionIsClosed);

  if Value = '' then
    Value := 'SYSTEM PATH, USER';

  ExecuteSQL('SET PATH ' + Value);
  FFunctionPath := Value;
end;

{ TDB2Transaction }

procedure TDB2Transaction.Savepoint(const Name: string);
var
  Connection: TDB2Connection;
begin
  CheckActive;

  Connection := TDB2Connection(FConnections[0]);
  Connection.ExecuteSQL('SAVEPOINT ' + Name + ' ON ROLLBACK RETAIN CURSORS');
end;

{ TDB2Command }

class function TDB2Command.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TDB2MapRules;
end;

{ TDB2RecordSet }

procedure TDB2RecordSet.CreateCommand;
begin
  SetCommand(TDB2Command.Create);
end;

{$IFNDEF LITE}

{ TDB2Loader }

procedure TDB2Loader.CreateCommand;
begin
  FCommand := TDB2Command.Create;
end;

{$ENDIF}

end.
