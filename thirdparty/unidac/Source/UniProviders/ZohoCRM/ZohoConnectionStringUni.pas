
/////////////////////////////////////////////////
//  Zoho CRM Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ZohoDac.inc}
unit ZohoConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  ZohoConsts, ZohoProps, ZohoClasses;
{$ELSE}
  ODBCConnectionStringUni,
  ZohoConstsUni, ZohoPropsUni, ZohoClassesUni;
{$ENDIF}

type

  TZohoConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;
  end;

implementation

uses
  CRProps;

{ TZohoConnectionStringBuilder }

procedure TZohoConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported
//  DeleteParam(prUsername); // Username is not supported
  DeleteParam(prPassword); // Password is not supported

  AddParam(ppHighest, 'Version', ['API Version', 'APIVersion'], prApiVersion, varEnum, DefApiVersion, TypeInfo(TApiVersion), 'api');

  AddParam(ppNormal, 'Domain', ['Server'], prServer, varString, '');
  AddParam(ppNormal, 'Access Token', ['AccessToken'], prAccessToken, varString, '');
  AddParam(ppNormal, 'Refresh Token', ['RefreshToken'], prRefreshToken, varString, '');
  AddParam(ppNormal, 'EnableNonApprovedRecords', ['Enable NonApprovedRecords', 'Enable NonApproved Records'], prEnableNonApprovedRecords, varBoolean, false);
//  AddParam(ppNormal, 'Authentication Token', ['AuthenticationToken', 'Security Token', 'SecurityToken'], prAuthenticationToken, varString, '');
end;

function TZohoConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  // in some third-party tools Username is required
  if Code = prUsername then
    Result := True
  else if Code = prAuthenticationToken then
    Result := True
  else
    Result := inherited IgnoreParam(Code);
end;

end.
