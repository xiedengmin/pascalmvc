
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF VIRTUAL_QUERY}
{$I LiteDac.inc}
unit LiteErrorUni;
{$ENDIF}

interface

uses
  SysUtils, Classes,
  CRTypes, {$IFNDEF NODBACCESS} DB, DBAccess,{$ENDIF}
{$IFDEF VIRTUAL_QUERY}
  LiteCallVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteCall;
{$ELSE}
  LiteCallUni;
{$ENDIF}
{$ENDIF}

type

{ ESQLiteError }

{$IFDEF NODBACCESS}
  ESQLiteError = class(ECRError)
{$ELSE}
  ESQLiteError = class(EDAError)
{$ENDIF}
  public
    constructor Create(ErrorCode: integer; const ErrorMsg: string);
    destructor Destroy; override;

    function IsFatalError: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
    function IsKeyViolation: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
  end;

implementation

{ ESQLiteError }

constructor ESQLiteError.Create(ErrorCode: integer; const ErrorMsg: string);
begin
  inherited Create(ErrorCode, ErrorMsg);
end;

destructor ESQLiteError.Destroy;
begin
  inherited;
end;

function ESQLiteError.IsFatalError: boolean;
begin
  Result := False;
end;

function ESQLiteError.IsKeyViolation: boolean;
begin
  Result := (FErrorCode and SQLITE_CONSTRAINT) = SQLITE_CONSTRAINT;
end;

end.
