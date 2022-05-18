
/////////////////////////////////////////////////
//  Salesforce Marketing Cloud Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ExactTargetDac.inc}
unit ExactTargetConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  ExactTargetConsts, ExactTargetProps, ExactTargetClasses;
{$ELSE}
  ODBCConnectionStringUni,
  ExactTargetConstsUni, ExactTargetPropsUni, ExactTargetClassesUni;
{$ENDIF}

type

  TExactTargetConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TExactTargetConnectionStringBuilder }

procedure TExactTargetConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppHigh, 'Authentication', ['Authentication Type'], prAuthentication, varEnum, DefAuthenticationType, TypeInfo(TAuthenticationType), 'at');

  AddParam(ppNormal, 'Data Source', ['Url', 'Server', 'Host'], prServer, varString, '');
  AddParam(ppNormal, 'Partner IDs', ['PartnerIDs'], prPartnerIDs, varString, '');
  AddParam(ppNormal, 'App Sandbox', ['AppSandbox'], prAppSandbox, varBoolean, False);
  AddParam(ppNormal, 'App Client ID', ['AppClientID', 'App Center Client ID', 'AppCenterClientID'], prAppClientID, varString, '');
  AddParam(ppNormal, 'App Client Secret', ['AppClientSecret', 'App Center Client Secret', 'AppCenterClientSecret'], prAppClientSecret, varString, '');
end;

end.
