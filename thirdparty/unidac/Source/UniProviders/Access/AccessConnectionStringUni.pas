/////////////////////////////////////////////////
//  MS Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Access ConnectionString
//////////////////////////////////////////////////

{$I AccessDac.inc}
unit AccessConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess,
{$IFNDEF UNIDACPRO}
  AccessClasses, ODBCConnectionString;
{$ELSE}
  AccessClassesUni, ODBCConnectionStringUni;
{$ENDIF}

type

  TAccessConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  public
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  CRConnectionString,
{$IFNDEF UNIDACPRO}
  AccessProps;
{$ELSE}
  AccessPropsUni;
{$ENDIF}

{ TAccessConnectionStringBuilder }

procedure TAccessConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'SystemDatabase', [], prSystemDatabase, varString, '');
  AddParam(ppNormal, 'ExtendedAnsiSQL', [], prExtendedAnsiSQL, varBoolean, False);
  AddParam(ppNormal, 'ExclusiveLock', [], prExclusiveLock, varBoolean, False);
  AddParam(ppNormal, 'ForceCreateDatabase', [], prForceCreateDatabase, varBoolean, False);
end;

end.
