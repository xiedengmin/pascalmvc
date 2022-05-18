
/////////////////////////////////////////////////
//  MailChimp Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MailChimpDac.inc}
unit MailChimpConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  MailChimpConsts, MailChimpProps, MailChimpClasses;
{$ELSE}
  ODBCConnectionStringUni,
  MailChimpConstsUni, MailChimpPropsUni, MailChimpClassesUni;
{$ENDIF}

type

  TMailChimpConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;
  end;

implementation

uses
  CRProps;

{ TMailChimpConnectionStringBuilder }

procedure TMailChimpConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prServer); // Server is not supported
  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported
//  DeleteParam(prUsername); // Username is not supported
  DeleteParam(prPassword); // Password is not supported

  AddParam(ppHighest, 'Version', ['API Version', 'APIVersion'], prApiVersion, varEnum, DefApiVersion, TypeInfo(TApiVersion), 'api');
  AddParam(ppNormal, 'Api Key', ['ApiKey'], prApiKey, varString, '');
  AddParam(ppNormal, 'MergeCustomFields', [], prMergeCustomFields, varEnum, DefMergeCustomFields, TypeInfo(TMergeCustomFields), 'mcf');
  AddParam(ppNormal, 'UseMergeTagAsFieldName', ['Use Merge Tag As FieldName'], prMergeTagAsFieldName, varBoolean, False);
end;

function TMailChimpConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  // in some third-party tools Username is required
  if Code = prUsername then
    Result := True
  else
    Result := inherited IgnoreParam(Code);
end;

end.
