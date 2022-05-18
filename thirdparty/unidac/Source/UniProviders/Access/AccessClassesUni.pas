
//////////////////////////////////////////////////
//  MS Access Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////



{$I AccessDac.inc}
unit AccessClassesUni;

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
  ODBCCall, ODBCClasses, AccessParser;
{$ELSE}
  ODBCCallUni, ODBCClassesUni, AccessParserUni;
{$ENDIF}

type
  TDriverVersion = (dvAuto, dvMdb, dvAccdb);

{ TAccessConnection }

  TAccessConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FSystemDatabase: string;
    FExtendedAnsiSQL: Boolean;
    FExclusiveLock: Boolean;
    FForceCreateDatabase: boolean;
    FDriverVersion: TDriverVersion;
  protected
    function GetConnectionString: string; override;
    function IsCommentAllowed: boolean; override;
    function IsEmptyStringAllowed: boolean; override;

  public
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    procedure Connect(const ConnectString: string); override;
    
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
  end;

{ TAccessSQLInfo }

  TAccessSQLInfo = class(TODBCSQLInfo)
  protected
    function FirstCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean; override;
  public
    function LeftQuote: char; override;
    function RightQuote: char; override;
    function IdentCase: TIdentCase; override;
  end;

{ TAccessCommand }

  TAccessCommand = class(TODBCCommand)
  protected
    procedure AllocStatement; override;
    function GetBlobSize(SQLLength: longword): Integer; override;
  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetParserClass: TSQLParserClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    procedure Close; override;
  end;

{ TAccessRecordSet }

  TAccessRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{$IFNDEF LITE}

{ TAccessLoader }

  TAccessLoader = class (TODBCLoader)
  protected
    procedure CreateCommand; override;
  end;
{$ENDIF}

implementation

uses
  CRProps, CRFunctions, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts, AccessProps, AccessDataTypeMap, AccessCall;
{$ELSE}
  ODBCConstsUni, AccessPropsUni, AccessDataTypeMapUni, AccessCallUni;
{$ENDIF}

{ TAccessConnection }

function TAccessConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TAccessCommand;
end;

function TAccessConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TAccessRecordSet;
end;

function TAccessConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TAccessLoader;
end;

class function TAccessConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TAccessCommand.GetMapRulesClass;
end;

function TAccessConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prSystemDatabase:
      FSystemDatabase := Value;
    prExtendedAnsiSQL:
      FExtendedAnsiSQL := Value;
    prExclusiveLock:
      FExclusiveLock := Value;
    prForceCreateDatabase:
      FForceCreateDatabase := Value;
    prDriverVersion:
      FDriverVersion := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TAccessConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prSystemDatabase:
      Value := FSystemDatabase;
    prMaxStringSize:
      Value := 255;
    prExtendedAnsiSQL:
      Value := FExtendedAnsiSQL;
    prExclusiveLock:
      Value:= FExclusiveLock;
    prForceCreateDatabase:
      Value := FForceCreateDatabase;
    prDriverVersion:
      Value := FDriverVersion;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TAccessConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TAccessConnection(Source).FDatabase;
  FSystemDatabase := TAccessConnection(Source).FSystemDatabase;
  FExtendedAnsiSQL := TAccessConnection(Source).FExtendedAnsiSQL;
  FExclusiveLock := TAccessConnection(Source).FExclusiveLock;
  FForceCreateDatabase := TAccessConnection(Source).FForceCreateDatabase;
  FDriverVersion := TAccessConnection(Source).FDriverVersion;
end;

function TAccessConnection.GetConnectionString: string;
var
  Driver: string;
begin
  Result := '';

  case FDriverVersion of
    dvAuto: begin
      if IsDriverPresent(accdbDriver) then
        Driver := accdbDriver
      else
        Driver := mdbDriver;
    end;
    dvAccdb:
      Driver := accdbDriver;
    dvMdb:
      Driver := mdbDriver;
    end;

  Result := Result + Format('DRIVER={%s};DefaultDir=%s;DBQ=%s;UID=%s;PWD=%s',
    [Driver, ExtractFilePath(FDatabase), FDatabase, FUsername, FPassword]);

  if FExclusiveLock then
    Result:= Result +  ';Exclusive=1';

  if FSystemDatabase <> '' then
    Result := Result + ';SystemDB=' + FSystemDatabase;

  if FExtendedAnsiSQL then
    Result := Result + ';ExtendedAnsiSQL=1';
end;

function TAccessConnection.IsCommentAllowed: boolean;
begin
  Result := False;
end;

function TAccessConnection.IsEmptyStringAllowed: boolean;
begin
  Result := True;
end;

procedure TAccessConnection.Connect(const ConnectString: string);
begin
  if FForceCreateDatabase then
    CreateNewDataBase(FDatabase, FDriverVersion In [dvAccdb]);

  inherited Connect(ConnectString);
end;

{ TAccessSQLInfo }

function TAccessSQLInfo.FirstCharQuotesNeed(Ch: Char; IdCase: TIdentCase): boolean;
begin
  Result := inherited FirstCharQuotesNeed(Ch, IdCase) or CharInSet(Ch, ['0'..'9']);
end;

function TAccessSQLInfo.LeftQuote: char;
begin
  Result := '`';
end;

function TAccessSQLInfo.RightQuote: char;
begin
  Result := '`';
end;

function TAccessSQLInfo.IdentCase: TIdentCase;
begin
  Result := icMixed;
end;

{ TAccessCommand }

procedure TAccessCommand.AllocStatement;
begin
  inherited;

  Cli.SQLSetStmtAttrInt(FStmt, SQL_ATTR_CURSOR_TYPE, SQL_CURSOR_KEYSET_DRIVEN, 0);
end;

function TAccessCommand.GetBlobSize(SQLLength: longword): Integer;
begin
  if Integer(SQLLength) = 1073741823 then
    Result := 0
  else
    Result := inherited GetBlobSize(SQLLength);
end;

class function TAccessCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TAccessSQLInfo;
end;

class function TAccessCommand.GetParserClass: TSQLParserClass;
begin
  Result := TAccessParser;
end;

class function TAccessCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TAccessMapRules;
end;

procedure TAccessCommand.Close;
begin
  if FStmt = nil then
    Exit;

  // workaround for performance: call Cli.SQLFreeStmt instaed of Cli.SQLCloseCursor
  Cli.SQLFreeStmt(FStmt, SQL_CLOSE);
end;

{ TAccessRecordSet }

procedure TAccessRecordSet.CreateCommand;
begin
  SetCommand(TAccessCommand.Create);
end;

{$IFNDEF LITE}

{ TAccessLoader }

procedure TAccessLoader.CreateCommand;
begin
  FCommand := TAccessCommand.Create;
end;

{$ENDIF}

end.
