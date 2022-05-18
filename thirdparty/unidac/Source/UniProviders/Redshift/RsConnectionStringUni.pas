
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Amazon Redshift ConnectionString
//////////////////////////////////////////////////

{$I RsDac.inc}
unit RsConnectionStringUni;

interface

uses
  SysUtils,
  CRConnectionString,
{$IFNDEF UNIDACPRO}
  PgConnectionString;
{$ELSE}
  PgConnectionStringUni;
{$ENDIF}

type
  TRsConnectionStringBuilder = class(TCustomPgConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  PgConsts;
{$ELSE}
  PgConstsUni;
{$ENDIF}

{ TRsConnectionStringBuilder }

procedure TRsConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Port', [], prPort, varInteger, RsDefValPort, [0]);
end;

end.
