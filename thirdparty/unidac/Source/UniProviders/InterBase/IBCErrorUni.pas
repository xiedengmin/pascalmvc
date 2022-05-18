
//////////////////////////////////////////////////
//  InterBase Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  InterBase Error
//////////////////////////////////////////////////


{$I IbDac.inc}
unit IBCErrorUni;


interface
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF UNIX}
  Unix,
{$ENDIF}
  SysUtils, Classes, CRTypes, {$IFNDEF NODBACCESS} DB, DBAccess,{$ENDIF}
  {$IFNDEF UNIDACPRO}IBCCall{$ELSE}IBCCallUni{$ENDIF};

type

{ EIBCError }

{$IFDEF NODBACCESS}
  EIBCError = class(ECRError)
{$ELSE}
  EIBCError = class(EDAError)
{$ENDIF}
  private
    FSQLErrorMsg: string;
    FErrorNumber: integer;
    FSender: TComponent;

  public
    constructor Create(ErrorCode, ErrorNumber: integer; const ErrorMsg, SQLErrorMsg: string);
    destructor Destroy; override;

    class function IsFatalError(ErrorNumber: integer): boolean; reintroduce; overload;
  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; overload; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}

    property SQLErrorMsg: string read FSQLErrorMsg;
    property ErrorNumber: integer read FErrorNumber;
    property Sender: TComponent read FSender write FSender;
  end;

  procedure RaiseError(Msg: string);

  procedure RaiseIBCError(ErrorCode, ErrorNumber: integer; const ErrorMsg, SQLErrorMsg: string);

implementation

procedure RaiseError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseIBCError(ErrorCode, ErrorNumber: integer; const ErrorMsg, SQLErrorMsg: string);
begin
  raise EIBCError.Create(ErrorCode, ErrorNumber, ErrorMsg, SQLErrorMsg);
end;

{ EIBCError }

constructor EIBCError.Create(ErrorCode, ErrorNumber: integer; const ErrorMsg, SQLErrorMsg: string);
begin
  inherited Create(ErrorCode, ErrorMsg);

  FSQLErrorMsg := SQLErrorMsg;
  FErrorNumber := ErrorNumber;
  FSender := nil;
end;

destructor EIBCError.Destroy;
begin
  inherited;
end;

class function EIBCError.IsFatalError(ErrorNumber: integer): boolean;
begin
  case ErrorNumber of
    isc_network_error,
    isc_lost_db_connection,
    isc_conn_lost,
    isc_net_read_err,
    isc_net_write_err,
    isc_conn_shutdown_err,
    isc_db_shutdown_err:
      Result := True;
  else
    Result := False;
  end;
end;

{$IFNDEF NODBACCESS}
function EIBCError.IsFatalError: boolean;
begin
  Result := IsFatalError(ErrorNumber);
end;

function EIBCError.IsKeyViolation: boolean;
begin
  Result := ErrorNumber = isc_unique_key_violation;
end;
{$ENDIF}

end.
