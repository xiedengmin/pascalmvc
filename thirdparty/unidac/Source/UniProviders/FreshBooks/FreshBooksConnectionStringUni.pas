
/////////////////////////////////////////////////
//  FreshBooks Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I FreshBooksDac.inc}
unit FreshBooksConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  FreshBooksConsts, FreshBooksProps, FreshBooksClasses;
{$ELSE}
  ODBCConnectionStringUni,
  FreshBooksConstsUni, FreshBooksPropsUni, FreshBooksClassesUni;
{$ENDIF}

type

  TFreshBooksConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;
  end;

implementation

uses
  CRProps;

{ TFreshBooksConnectionStringBuilder }

procedure TFreshBooksConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported
//  DeleteParam(prUsername); // Username is not supported
  DeleteParam(prPassword); // Password is not supported

  AddParam(ppHighest, 'Version', ['API Version', 'APIVersion'], prApiVersion, varEnum, DefApiVersion, TypeInfo(TApiVersion), 'api');

  AddParam(ppNormal, 'Server', ['Data Source', 'DataSource', 'Host'], prServer, varString, '');
  AddParam(ppNormal, 'Company Name', ['CompanyName', 'Company'], prCompanyName, varString, '');
  AddParam(ppNormal, 'Access Token', ['AccessToken'], prAccessToken, varString, '');
  AddParam(ppNormal, 'Refresh Token', ['RefreshToken'], prRefreshToken, varString, '');
  AddParam(ppNormal, 'Authentication Token', ['AuthenticationToken'], prAuthenticationToken, varString, '');
end;

function TFreshBooksConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  // in some third-party tools Username is required
  if Code = prUsername then
    Result := True
  else
    Result := inherited IgnoreParam(Code);
end;

end.
