
//////////////////////////////////////////////////
//  Advantage Database Server Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I ADSDac.inc}
unit ADSClassesUni;

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

resourcestring
  SInvalidServerTypes = 'Invalid server types';

type
  TADSServerType = (stLocal, stRemote, stInternet);
  TADSServerTypes = set of TADSServerType;
  TADSDefaultType = (dtAdvantage, dtFoxPro, dtVisualFoxPro, dtClipper);

{ TADSConnection }

  TADSConnection = class(TODBCConnection)
  private
    FDatabase: string;
    FServerTypes: string;
    FDefaultType: TADSDefaultType;

    function ParseServerTypes(const ServerTypes: string): TADSServerTypes;
  protected
    function GetConnectionString: string; override;
    function IsBlockFetchAllowed: boolean; override;
  public
    function GetCommandClass: TCRCommandClass; override;
    function GetRecordSetClass: TCRRecordSetClass; override;
    function GetLoaderClass: TCRLoaderClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; out Value: variant): boolean; override;

    procedure Assign(Source: TCRConnection); override;
  end;

{ TADSCommand }

  TADSCommand = class(TODBCCommand)
  public
    class function GetMapRulesClass: TCRMapRulesClass; override;
  end;

{ TADSRecordSet }

  TADSRecordSet = class(TODBCRecordSet)
  protected
    procedure CreateCommand; override;
  end;

{$IFNDEF LITE}

{ TADSLoader }

  TADSLoader = class (TODBCLoader)
  protected
    procedure CreateCommand; override;
  end;
{$ENDIF}

implementation

uses
  CRProps, CRFunctions, DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ADSProps, ADSDataTypeMap;
{$ELSE}
  ODBCConstsUni, ADSPropsUni, ADSDataTypeMapUni;
{$ENDIF}

{ TADSConnection }

function TADSConnection.GetCommandClass: TCRCommandClass;
begin
  Result := TADSCommand;
end;

function TADSConnection.GetRecordSetClass: TCRRecordSetClass;
begin
  Result := TADSRecordSet;
end;

function TADSConnection.GetLoaderClass: TCRLoaderClass;
begin
  Result := TADSLoader;
end;

class function TADSConnection.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TADSCommand.GetMapRulesClass;
end;

function TADSConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      FDatabase := Value;
    prADSServerTypes:
      FServerTypes := Value;
    prADSDefaultType:
      FDefaultType := TADSDefaultType(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

function TADSConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Value := FDatabase;
    prADSServerTypes:
      Value := FServerTypes;
    prADSDefaultType:
      Value := Variant(FDefaultType);
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

procedure TADSConnection.Assign(Source: TCRConnection);
begin
  inherited;

  FDatabase := TADSConnection(Source).FDatabase;
end;

function TADSConnection.GetConnectionString: string;
var
  ServerTypes: TADSServerTypes;
  DefaultType: TADSDefaultType;
  i: integer;
begin
  ServerTypes := ParseServerTypes(FServerTypes);
  if ServerTypes = [] then
    raise Exception.Create(SInvalidServerTypes);
  DefaultType := FDefaultType;

  i := 0;
  if stLocal in ServerTypes then
    i := 1;
  if stRemote in ServerTypes then
    i := i + 2;
  if stInternet in ServerTypes then
    i := i + 4;

  Result := Format('DRIVER={Advantage StreamlineSQL ODBC};DataDirectory=%s;UID=%s;PWD=%s;ServerTypes=%d',
    [FDatabase, FUsername, FPassword, i]);
  if not(DefaultType in [dtAdvantage]) then begin
    case DefaultType of
      dtFoxPro: Result := Result + ';DefaultType=FoxPro';
      dtVisualFoxPro: Result := Result + ';DefaultType=Visual FoxPro';
      dtClipper: Result := Result + ';DefaultType=Clipper';
    end;
  end;
end;

function TADSConnection.IsBlockFetchAllowed: boolean;
begin
  Result := False;
end;

function TADSConnection.ParseServerTypes(const ServerTypes: string): TADSServerTypes;
var
  Str: string;
begin
  Str := Trim(ServerTypes);
  if Str = '' then begin
    Result := [stRemote, stInternet];
    exit;
  end;

  Result := [];
  if Pos('ALS', Str) > 0 then
    Include(Result, stLocal);
  if Pos('ADS', Str) > 0 then
    Include(Result, stRemote);
  if Pos('AIS', Str) > 0 then
    Include(Result, stInternet);
end;

{ TADSCommand }

class function TADSCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TADSMapRules;
end;

{ TADSRecordSet }

procedure TADSRecordSet.CreateCommand;
begin
  SetCommand(TADSCommand.Create);
end;

{$IFNDEF LITE}

{ TADSLoader }

procedure TADSLoader.CreateCommand;
begin
  FCommand := TADSCommand.Create;
end;

{$ENDIF}

end.
