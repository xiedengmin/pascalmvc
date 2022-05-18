
/////////////////////////////////////////////////
//  Magento Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MagentoDac.inc}
unit MagentoConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCConnectionString,
  MagentoConsts, MagentoProps, MagentoClasses;
{$ELSE}
  ODBCConnectionStringUni,
  MagentoConstsUni, MagentoPropsUni, MagentoClassesUni;
{$ENDIF}

type

  TMagentoConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps;

{ TMagentoConnectionStringBuilder }

procedure TMagentoConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(prPort); // Port is not supported
  DeleteParam(prDatabase); // Database is not supported

  AddParam(ppHighest, 'Version', ['API Version', 'APIVersion'], prApiVersion, varEnum, DefApiVersion, TypeInfo(TApiVersion), 'api');

  AddParam(ppNormal, 'Domain', ['Server'], prServer, varString, '');
  AddParam(ppNormal, 'Api Key', ['ApiKey'], prApiKey, varString, '');
end;

end.
