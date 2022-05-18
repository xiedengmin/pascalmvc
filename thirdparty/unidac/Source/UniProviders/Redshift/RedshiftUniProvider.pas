
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I RsDac.inc}
{$IFDEF VER10P}
{$I RedshiftUniProvider.inc}
{$ENDIF}
unit RedshiftUniProvider;

interface

uses
  SysUtils, Classes, Variants, DB,
  CRTypes, CRConnectionString,
  UniProvider, PostgreSQLUniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TRedshiftUniProvider = class(TCustomPostgreSQLUniProvider)
  public
    class function GetProviderName: string; override;
    class function GetConnectionStringClass: TCRConnectionStringBuilderClass; override;

    procedure SetObjectProps(Obj: TObject; Options: TStrings); override;
  end;

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}

implementation

uses
  DAConsts, UniConsts, CRProps, CRVio, CRVioTcp,
{$IFNDEF UNIDACPRO}
  PgProps, PgClasses, PgConnectionPool,
  RsConnectionString;
{$ELSE}
  PgPropsUni, PgClassesUni, PgConnectionPoolUni,
  RsConnectionStringUni;
{$ENDIF}

{$IFNDEF FPC}
procedure RegisterComponent;
{$ELSE}
procedure Register;
{$ENDIF}
begin
  RegisterComponents('UniDAC Providers', [TRedshiftUniProvider]);
end;

{ TRedshiftUniProvider }

class function TRedshiftUniProvider.GetProviderName: string;
begin
  Result := 'Redshift';
end;

class function TRedshiftUniProvider.GetConnectionStringClass: TCRConnectionStringBuilderClass;
begin
  Result := TRsConnectionStringBuilder;
end;

procedure TRedshiftUniProvider.SetObjectProps(Obj: TObject; Options: TStrings);
begin
  if Obj is TPgSQLConnection then begin
    TPgSQLConnection(Obj).SetProp(prRedshiftConnection, True);
    TPgSQLConnection(Obj).SetProp(prProtocolVersion, pv30);
    TPgSQLConnection(Obj).SetProp(prSchema, '');
    TPgSQLConnection(Obj).SetProp(prCharset, '');
    TPgSQLConnection(Obj).SetProp(prUseUnicode, True);
  end;

  inherited;
end;

initialization
  UniProviders.RegisterProvider(TRedshiftUniProvider);

finalization
  UniProviders.UnRegisterProvider(TRedshiftUniProvider);

end.
