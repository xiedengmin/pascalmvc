
//////////////////////////////////////////////////
//  PostgreSQL Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I PgDac.inc}
unit PgErrorUni;


interface

uses
  Classes, SysUtils,
{$IFNDEF NODBACCESS}
  DBAccess,
{$ENDIF}
  CRTypes;

type
  TPgSeverity = (sError, sFatal, sPanic, sWarning, sNotice, sDebug, sInfo, sLog, sUnknown);

{$IFDEF NODBACCESS}
  EPgError = class(ECRError)
{$ELSE}
  EPgError = class(EDAError)
{$ENDIF}
  private
    FSeverity: TPgSeverity;
    FErrorCode: string;
    FDetailMsg: string;
    FHint: string;
    FCallStack: string;
    FFileName: string;
    FProcedureName: string;
    FPosition: integer;
    FLineNumber: integer;
  public
    constructor Create(Severity: TPgSeverity; const Msg: string); overload;
    constructor Create(Severity: TPgSeverity; ErrorCode: integer; const Msg: string); overload;
    constructor Create(Severity: TPgSeverity; const ErrorCode, Msg, DetailMsg, Hint, CallStack,
      FileName, ProcedureName: string; Position, LineNumber: integer); overload;

    function IsFatalError: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
  {$IFNDEF LITE}
    function IsKeyViolation: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
  {$ENDIF}

    property Severity: TPgSeverity read FSeverity;
    property ErrorCode: string read FErrorCode;
    property DetailMsg: string read FDetailMsg;
    property Hint: string read FHint;
    property CallStack: string read FCallStack;
    property FileName: string read FFileName;
    property ProcedureName: string read FProcedureName;
    property Position: integer read FPosition;
    property LineNumber: integer read FLineNumber;
  end;

  TPgErrors = class(TCRObjectList)
  private
    function GetError(Index: integer): EPgError;
  public
    property Errors[Index: integer]: EPgError read GetError; default;
  end;

implementation

uses
  {$IFNDEF UNIDACPRO}PgSQLProtocol{$ELSE}PgSQLProtocolUni{$ENDIF};

constructor EPgError.Create(Severity: TPgSeverity; const Msg: string);
begin
  Create(0, Msg);
end;

constructor EPgError.Create(Severity: TPgSeverity; ErrorCode: integer; const Msg: string);
begin
  inherited Create(ErrorCode, Msg);

  FSeverity := Severity;
  FErrorCode := IntToStr(ErrorCode);
end;

constructor EPgError.Create(Severity: TPgSeverity; const ErrorCode, Msg, DetailMsg, Hint, CallStack,
  FileName, ProcedureName: string; Position, LineNumber: Integer);
begin
  inherited Create(StrToIntDef(ErrorCode, 0), Msg);

  FSeverity := Severity;
  FErrorCode := ErrorCode;
  FDetailMsg := DetailMsg;
  FHint := Hint;
  FCallStack := CallStack;
  FFileName := FileName;
  FProcedureName := ProcedureName;
  FPosition := Position;
  FLineNumber := LineNumber;
end;

function EPgError.IsFatalError: boolean;
begin
  Result := FSeverity in [sFatal, sPanic];
end;

{$IFNDEF LITE}
function EPgError.IsKeyViolation: boolean;
begin
  Result := FErrorCode = '23505';
end;
{$ENDIF}

{ TPgErrors }

function TPgErrors.GetError(Index: integer): EPgError;
begin
  Result := EPgError(inherited Items[Index]);
end;

end.
