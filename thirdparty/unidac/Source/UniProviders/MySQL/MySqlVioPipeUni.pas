
//////////////////////////////////////////////////
//  MySQL Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MySqlVioPipeUni;

interface

{$IFDEF MSWINDOWS}

uses
  Windows, Classes, SysUtils, CLRClasses, CRTypes, CRVio,
{$IFNDEF UNIDACPRO}
  MyCall;
{$ELSE}
  MyCallUni;
{$ENDIF}

var
  MYSQL_NAMEDPIPE: string = 'MySQL';

type
  TMySqlVioPipe = class(TCRVio)
  protected
    FPipe: THandle;
    Fhostname: string;
    Fport: integer;
    Funix_socket: string;
    Ftimeout: integer;

    function GetTimeout: integer; override;
    procedure SetTimeout(Value: integer); override;
    function GetReceiveTimeout: integer; override;
    procedure SetReceiveTimeout(Value: integer); override;

  public
    constructor Create(const hostname: string; const port: integer; const unix_socket: string);

    procedure Connect; override;
    procedure Close; override;

    function ReadNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
    function WriteNoWait(const buffer: TValueArr; offset, count: integer): integer; override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  {$IFNDEF UNIDACPRO}MySqlErrors{$ELSE}MySqlErrorsUni{$ENDIF};

{ TMySqlVioPipe }

constructor TMySqlVioPipe.Create(const hostname: string; const port: integer; const unix_socket: string);
begin
  inherited Create;
  Fhostname := hostname;
  Fport := port;
  Funix_socket := unix_socket;
  FPipe := INVALID_HANDLE_VALUE;
end;

function TMySqlVioPipe.GetTimeout: integer;
begin
  Result := Ftimeout;
end;

procedure TMySqlVioPipe.SetTimeout(Value: integer);
begin
  Ftimeout := Value;
end;

function TMySqlVioPipe.GetReceiveTimeout: integer;
begin
  Result := Ftimeout;
end;

procedure TMySqlVioPipe.SetReceiveTimeout(Value: integer);
begin
  Ftimeout := Value;
end;

procedure TMySqlVioPipe.Connect;
var
  szPipeName: string;
  dwMode: DWORD;
  i: integer;
begin
  TryConnect;

  if Funix_socket = '' then
    Funix_socket := MYSQL_NAMEDPIPE;

  if (Fhostname = '') or (AnsiCompareText(Fhostname, LOCAL_HOST) = 0) then
    Fhostname := LOCAL_HOST_NAMEDPIPE;

  szPipeName := '\\' + Fhostname + '\pipe\' + Funix_socket;

  for i := 0 to 99 do begin (* Don't retry forever *)
    FPipe := CreateFile(PChar(szPipeName),
      GENERIC_READ or GENERIC_WRITE,
      0,
      nil,
      OPEN_EXISTING,
      0,
      0);
    if FPipe <> INVALID_HANDLE_VALUE then
      Break;
    if GetLastError <> ERROR_PIPE_BUSY then
      raise EMySqlException.Create(CR_NAMEDPIPEOPEN_ERROR, [Fhostname, szPipeName, Funix_socket], SysErrorMessage(GetLastError));
    (* wait for an other instance *)
    if not WaitNamedPipe(PChar(szPipeName), Ftimeout * 1000) then
      raise EMySqlException.Create(CR_NAMEDPIPEWAIT_ERROR, [Fhostname, szPipeName, Funix_socket], '');
  end;
  if FPipe = INVALID_HANDLE_VALUE then
    raise EMySqlException.Create(CR_NAMEDPIPEOPEN_ERROR, [Fhostname, szPipeName, Funix_socket], '');

  dwMode := PIPE_READMODE_BYTE or PIPE_WAIT;
  if not SetNamedPipeHandleState(FPipe, dwMode, nil, nil) then begin
    FPipe := INVALID_HANDLE_VALUE;
    CloseHandle(FPipe);
    raise EMySqlException.Create(CR_NAMEDPIPESETSTATE_ERROR, [Fhostname, szPipeName, Funix_socket], '');
  end;
end;

procedure TMySqlVioPipe.Close;
begin
  CloseHandle(FPipe);
end;

function TMySqlVioPipe.ReadNoWait(const buffer: TValueArr; offset,
  count: integer): integer;
var
  len1: cardinal;
begin
  if not ReadFile(FPipe, buffer[offset], count, len1, nil) then
    Result := 0
  else
    Result := len1;
end;

function TMySqlVioPipe.WriteNoWait(const buffer: TValueArr; offset,
  count: integer): integer;
var
  len1: cardinal;
begin
  if count > $FFFF then
    count := $FFFF;

  if not WriteFile(FPipe, buffer[offset], count, len1, nil) then
    Result := 0
  else
    Result := len1;
end;

{$ENDIF}

end.
