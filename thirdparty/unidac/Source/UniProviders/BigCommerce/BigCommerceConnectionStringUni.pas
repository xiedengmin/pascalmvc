
/////////////////////////////////////////////////
//  BigCommerce Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I BigCommerceDac.inc}
unit BigCommerceConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  BigCommerceConsts, BigCommerceProps, BigCommerceClasses;
{$ELSE}
  ODBCConnectionStringUni,
  BigCommerceConstsUni, BigCommercePropsUni, BigCommerceClassesUni;
{$ENDIF}

type

  TBigCommerceConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TBigCommerceConnectionStringBuilder }

procedure TBigCommerceConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported
  DeleteParam(prPassword); // Password is not supported

  AddParam(ppHigh, 'Authentication', ['AuthenticationType', 'Authentication Type'], prAuthentication, varEnum, DefAuthenticationType, TypeInfo(TAuthenticationType), 'at');

  AddParam(ppNormal, 'Server', ['Data Source', 'DataSource', 'Host'], prServer, varString, '');
  AddParam(ppNormal, 'Store Id', ['StoreId'], prStoreId, varString, '');
  AddParam(ppNormal, 'Client Id', ['ClientId'], prClientId, varString, '');
  AddParam(ppNormal, 'Access Token', ['AccessToken'], prAccessToken, varString, '');
  AddParam(ppNormal, 'Authentication Token', ['AuthenticationToken'], prAuthenticationToken, varString, '');
end;

end.
