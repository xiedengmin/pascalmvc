
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I DB2Dac.inc}
unit DB2CallUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CLRClasses, Classes, SysUtils, SyncObjs, CRTypes,
{$IFNDEF UNIDACPRO}
  ODBCCall;
{$ELSE}
  ODBCCallUni;
{$ENDIF}

type
  _SQLGetLength = function(
    StatementHandle: TSQLHStmt;
    LocatorCType: smallint;
    Locator: integer;
    var StringLength: integer;
    var IndicatorValue: integer
  ): smallint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  _SQLGetSubString = function(
    StatementHandle: TSQLHStmt;
    LocatorCType: smallint;
    SourceLocator: integer;
    FromPosition: cardinal;
    ForLength: cardinal;
    TargetCType: smallint;
    DataPtr: IntPtr;
    BufferLength: integer;
    var StringLength: integer;
    var IndicatorValue: integer
  ): smallint; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

  TDB2CliAPI = class (TODBCCliAPI)
  public
    SQLGetLength: _SQLGetLength;
    SQLGetSubString: _SQLGetSubString;
  end;

  IDB2Environment = interface (IODBCEnvironment)
  ['{28FCB180-3D7E-4E85-B977-0E1963BB8E30}']
    function DB2Cli: TDB2CliAPI;
  end;

  TDB2Environment = class (TODBCEnvironment, IDB2Environment)
  protected
    function CreateAPI: TODBCCliAPI; override;
    procedure InitAPI(IsUnicode: Boolean); override;
  public
    destructor Destroy; override;

    function DB2Cli: TDB2CliAPI;
  end;

function GetDB2Environment: IDB2Environment;

implementation

var
  DB2Env: TDB2Environment;
  LockDB2Env: TCriticalSection;

function GetDB2Environment: IDB2Environment;
begin
  LockDB2Env.Enter;
  try
    if DB2Env = nil then
      DB2Env := TDB2Environment.Create;
    Result := DB2Env as IDB2Environment;
  finally
    LockDB2Env.Leave;
  end;
end;

function TDB2Environment.CreateAPI: TODBCCliAPI;
begin
  Result := TDB2CliAPI.Create(self);
end;

procedure TDB2Environment.InitAPI(IsUnicode: Boolean);
begin
  inherited InitAPI(IsUnicode);

  TDB2CliAPI(FCli).SQLGetLength := GetProc('SQLGetLength');
  TDB2CliAPI(FCli).SQLGetSubString := GetProc('SQLGetSubString');
end;

function TDB2Environment.DB2Cli: TDB2CliAPI;
begin
  Result := TDB2CliAPI(FCli);
end;

destructor TDB2Environment.Destroy;
begin
  DB2Env := nil;

  inherited;
end;

initialization
  LockDB2Env := TCriticalSection.Create;
  DB2Env := nil;

finalization
  LockDB2Env.Free;

end.

