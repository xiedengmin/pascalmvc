//////////////////////////////////////////////////
//  NexusDB Data Access Components
//  Copyright © 2009-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I NexusDac.inc}
unit NexusErrorUni;
{$ENDIF}

interface

{$IFNDEF DUMMY}
uses
  SysUtils, Classes,
{$IFNDEF NODBACCESS}
  DBAccess,
{$ENDIF}
  CRTypes;

type
{$IFDEF NODBACCESS}
  ENexusError = class(ECRError)
{$ELSE}
  ENexusError = class(EDAError)
{$ENDIF}
  private
  public
    constructor Create(ErrorCode: integer; const ErrorMsg: string);
    destructor Destroy; override;

  {$IFNDEF NODBACCESS}
    function IsFatalError: boolean; override;
    function IsKeyViolation: boolean; override;
  {$ENDIF}
  end;
{$ENDIF} // DUMMY

implementation

{$IFNDEF DUMMY}

{$IFNDEF NODBACCESS}
uses
  nxllBde;
{$ENDIF}

{ ENexusError }

constructor ENexusError.Create(ErrorCode: integer; const ErrorMsg: string);
begin
  inherited Create(ErrorCode, ErrorMsg);
end;

destructor ENexusError.Destroy;
begin
  inherited;
end;

{$IFNDEF NODBACCESS}
function ENexusError.IsFatalError: boolean;
begin
  Result := FErrorCode = DBIERR_SERVERCOMMLOST;
end;

function ENexusError.IsKeyViolation: boolean;
begin
  Result := FErrorCode = DBIERR_KEYVIOL;
end;
{$ENDIF}

{$ENDIF} // DUMMY

end.
