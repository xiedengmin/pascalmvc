
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoErrorUni;

interface

uses
  SysUtils, Classes,
{$IFNDEF NODBACCESS}
  DB, DBAccess,
{$ENDIF}
  CRTypes;

type

{ ESQLiteError }

{$IFDEF NODBACCESS}
  EMongoDBError = class(ECRError)
{$ELSE}
  EMongoDBError = class(EDAError)
{$ENDIF}
  public
    constructor Create(ErrorCode: integer; const ErrorMsg: string);
    destructor Destroy; override;

    function IsFatalError: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
    function IsKeyViolation: boolean; {$IFNDEF NODBACCESS}override;{$ENDIF}
  end;

implementation

{ EMongoDBError }

constructor EMongoDBError.Create(ErrorCode: integer; const ErrorMsg: string);
begin
  inherited Create(ErrorCode, ErrorMsg)
end;

destructor EMongoDBError.Destroy;
begin
  inherited;
end;

function EMongoDBError.IsFatalError: boolean;
begin
  Result := False;
end;

function EMongoDBError.IsKeyViolation: boolean;
begin
  Result := False;
end;

end.
