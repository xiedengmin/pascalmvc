
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFClassesUni;

{$IFNDEF ODBC_PROVIDER}
  Error
{$ENDIF}

interface

uses
  Classes, SysUtils, FMTBcd, Variants,
{$IFDEF VER12P}{$IFNDEF NEXTGEN}
  AnsiStrings,
{$ENDIF}{$ENDIF}
  CLRClasses, CRTypes, CRParser, CRAccess, CRDataTypeMap,
{$IFDEF ODBC_PROVIDER}
{$IFNDEF UNIDACPRO}
  ODBCCall, ODBCClasses,
{$ELSE}
  ODBCCallUni, ODBCClassesUni,
{$ENDIF}
{$ENDIF}
  MemData;

type
  TDBFCommand = class({$IFDEF ODBC_PROVIDER}TODBCCommand{$ELSE}TCRCommand{$ENDIF})
  protected
  public
    class function GetSQLInfoClass: TSQLInfoClass; override;
    class function GetMapRulesClass: TCRMapRulesClass; override;

    procedure Prepare; override;
  end;

  TDBFRecordSet = class({$IFDEF ODBC_PROVIDER}TODBCRecordSet{$ELSE}TCRRecordSet{$ENDIF})
  protected
    procedure CreateCommand; override;
  end;

  TDBFTransaction = class({$IFDEF ODBC_PROVIDER}TODBCTransaction{$ELSE}TCRTransaction{$ENDIF})
  end;

{$IFDEF ODBC_PROVIDER}
{$IFNDEF LITE}
  TDBFLoader = class(TODBCLoader)
  protected
    procedure CreateCommand; override;
  end;
{$ENDIF}
{$ENDIF}

implementation

uses
  CRFunctions,
{$IFNDEF UNIDACPRO}
  DBFConnection, DBFDataTypeMap;
{$ELSE}
  DBFConnectionUni, DBFDataTypeMapUni;
{$ENDIF}

{ TDBFCommand }

class function TDBFCommand.GetSQLInfoClass: TSQLInfoClass;
begin
  Result := TDBFSQLInfo;
end;

class function TDBFCommand.GetMapRulesClass: TCRMapRulesClass;
begin
  Result := TDBFMapRules;
end;

procedure TDBFCommand.Prepare;
begin
{$IFDEF DBFENGINE}
  if not TDBFConnection(FConnection).Direct then
{$ENDIF}
    inherited;
end;

{ TDBFRecordSet }

procedure TDBFRecordSet.CreateCommand;
begin
{$IFDEF ODBC_PROVIDER}
  SetCommand(TDBFCommand.Create);
{$ELSE}
  Assert(False);
{$ENDIF}
end;

{$IFDEF ODBC_PROVIDER}
{$IFNDEF LITE}

{ TDBFLoader }

procedure TDBFLoader.CreateCommand;
begin
  FCommand := TDBFCommand.Create;
end;

{$ENDIF}
{$ENDIF}

end.

