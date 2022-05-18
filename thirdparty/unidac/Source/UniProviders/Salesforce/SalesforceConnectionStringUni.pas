
/////////////////////////////////////////////////
//  Salesforce Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SalesforceDac.inc}
unit SalesforceConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  SalesforceProps;
{$ELSE}
  ODBCConnectionStringUni,
  SalesforcePropsUni;
{$ENDIF}

type

  TSalesforceConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TSalesforceConnectionStringBuilder }

procedure TSalesforceConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppNormal, 'Data Source', ['Server', 'Host'], prServer, varString, '');

  AddParam(ppNormal, 'Security Token', ['SecurityToken'], prSecurityToken, varString, '');
  AddParam(ppNormal, 'Include Deleted', ['IncludeDeleted', 'Include Deleted'], prIncludeDeleted, varBoolean, false);
end;

end.
