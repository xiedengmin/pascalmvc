
/////////////////////////////////////////////////
//  Dynamics CRM Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DynamicsDac.inc}
unit DynamicsConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  DynamicsProps;
{$ELSE}
  ODBCConnectionStringUni,
  DynamicsPropsUni;
{$ENDIF}

type

  TDynamicsConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TDynamicsConnectionStringBuilder }

procedure TDynamicsConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppNormal, 'Data Source', ['Server', 'Host'], prServer, varString, '');
end;

end.
