
/////////////////////////////////////////////////
//  NetSuite Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NetSuiteDac.inc}
unit NetSuiteConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  NetSuiteConsts, NetSuiteProps, NetSuiteClasses;
{$ELSE}
  ODBCConnectionStringUni,
  NetSuiteConstsUni, NetSuitePropsUni, NetSuiteClassesUni;
{$ENDIF}

type

  TNetSuiteConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TNetSuiteConnectionStringBuilder }

procedure TNetSuiteConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prServer); // Server is not supported
  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppNormal, 'Account Id', ['AccountId'], prAccountId, varString, '');
  AddParam(ppNormal, 'Role Id', ['RoleId'], prRoleId, varString, '');
  AddParam(ppNormal, 'Application Id', ['ApplicationId'], prApplicationId, varString, '');
  AddParam(ppHigh, 'Authentication Type', ['AuthenticationType'], prAuthentication, varEnum, DefAuthenticationType, TypeInfo(TAuthenticationType), 'at');  AddParam(ppNormal, 'Application Id', ['ApplicationId'], prApplicationId, varString, '');

  AddParam(ppNormal, 'Consumer Key', ['ConsumerKey'], prConsumerKey, varString, '');
  AddParam(ppNormal, 'Consumer Secret', ['ConsumerSecret'], prConsumerSecret, varString, '');
  AddParam(ppNormal, 'Token Id', ['TokenId'], prToken, varString, '');
  AddParam(ppNormal, 'Token Secret', ['TokenSecret'], prTokenSecret, varString, '');

  AddParam(ppNormal, 'Custom Tables', ['CustomTables', 'UseCustomTables'], prCustomTables, varBoolean, false);
  AddParam(ppNormal, 'Custom Fields', ['CustomFields', 'UseCustomFields'], prCustomFields, varBoolean, false);
  AddParam(ppNormal, 'Sandbox', [], prSandbox, varBoolean, false);
end;

end.
