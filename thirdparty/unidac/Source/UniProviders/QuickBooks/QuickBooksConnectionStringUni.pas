
/////////////////////////////////////////////////
//  QuickBooks Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I QuickBooksDac.inc}
unit QuickBooksConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  QuickBooksProps;
{$ELSE}
  ODBCConnectionStringUni,
  QuickBooksPropsUni;
{$ENDIF}

type

  TQuickBooksConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;
  end;

implementation

uses
  CRProps;

{ TQuickBooksConnectionStringBuilder }

procedure TQuickBooksConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prServer); // Server is not supported
  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported
//  DeleteParam(prUsername); // Username is not supported
  DeleteParam(prPassword); // Password is not supported

  AddParam(ppNormal, 'Company Id', ['CompanyId'], prCompanyId, varString, '');
  AddParam(ppNormal, 'Refresh Token', ['RefreshToken'], prRefreshToken, varString, '');
//  AddParam(ppNormal, 'Access Token', ['AccessToken'], prAccessToken, varString, '');
//  AddParam(ppNormal, 'Access Token Secret', ['AccessTokenSecret'], prAccessTokenSecret, varString, '');
//  AddParam(ppNormal, 'Consumer Key', ['ConsumerKey'], prConsumerKey, varString, '');
//  AddParam(ppNormal, 'Consumer Key Secret', ['ConsumerKeySecret', 'Consumer Secret', 'ConsumerSecret'], prConsumerKeySecret, varString, '');
  AddParam(ppNormal, 'Sandbox', [], prSandbox, varBoolean, False);
end;

function TQuickBooksConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  // in some third-party tools Username is required
  if Code = prUsername then
    Result := True
  // depricated for OAuth1
  else if Code = prAccessToken then
    Result := True
  else if Code = prAccessTokenSecret then
    Result := True
  else if Code = prConsumerKey then
    Result := True
  else if Code = prConsumerKeySecret then
    Result := True
  else
    Result := inherited IgnoreParam(Code);
end;

end.
