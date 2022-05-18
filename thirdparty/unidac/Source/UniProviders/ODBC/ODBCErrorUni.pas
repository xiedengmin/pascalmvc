
//////////////////////////////////////////////////
//  ODBC Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCErrorUni;

interface

uses
  SysUtils, Classes,
  {$IFNDEF LITE}DB, DBAccess,{$ENDIF} CRTypes;

type

{ EODBCError }

{$IFDEF NODBACCESS}
  EODBCError = class(ECRError)
{$ELSE}
  EODBCError = class(EDAError)
{$ENDIF}
  private
    FState: string;
    FNativeErrorCode: integer;

  public
    constructor Create(ErrorCode: integer; const ErrorMsg, State: string; NativeErrorCode: integer);
    destructor Destroy; override;

  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

    property State: string read FState;
    property NativeErrorCode: integer read FNativeErrorCode;
  end;

implementation

uses
{$IFNDEF UNIDACPRO}
  ODBCCall;
{$ELSE}
  ODBCCallUni;
{$ENDIF}

{ EODBCError }

constructor EODBCError.Create(ErrorCode: integer; const ErrorMsg, State: string; NativeErrorCode: integer);
begin
  inherited Create(ErrorCode, ErrorMsg);

  FState := State;
  FNativeErrorCode := NativeErrorCode;
end;

destructor EODBCError.Destroy;
begin
  inherited;
end;

{$IFNDEF NODBACCESS}
function EODBCError.IsFatalError: boolean;
begin
  Result := False;
end;

function EODBCError.IsKeyViolation: boolean;
begin
  Result := Copy(FState, 1, 2) = '23';
end;
{$ENDIF}

end.
