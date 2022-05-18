
/////////////////////////////////////////////////
//  SugarCRM Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SugarDac.inc}
unit SugarConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  SugarProps;
{$ELSE}
  ODBCConnectionStringUni,
  SugarPropsUni;
{$ENDIF}

type

  TSugarConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TSugarConnectionStringBuilder }

procedure TSugarConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppNormal, 'Server', ['Data Source', 'Host'], prServer, varString, '');
end;

end.
