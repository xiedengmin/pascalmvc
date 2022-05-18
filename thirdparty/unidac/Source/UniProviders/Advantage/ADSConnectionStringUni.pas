/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Advantage ConnectionString
//////////////////////////////////////////////////

{$I ADSDac.inc}
unit ADSConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess,
{$IFNDEF UNIDACPRO}
  ADSClasses, ODBCConnectionString;
{$ELSE}
  ADSClassesUni, ODBCConnectionStringUni;
{$ENDIF}

type

  TAdvantageConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  public
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  CRConnectionString,
{$IFNDEF UNIDACPRO}
  ADSProps;
{$ELSE}
  ADSPropsUni;
{$ENDIF}

{ TAdvantageConnectionStringBuilder }

procedure TAdvantageConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'ServerTypes', [], prADSServerTypes, varString, 'ADS,AIS');
  AddParam(ppNormal, 'DefaultType', [], prADSDefaultType, varEnum, dtAdvantage, TypeInfo(TADSDefaultType));
end;

end.
