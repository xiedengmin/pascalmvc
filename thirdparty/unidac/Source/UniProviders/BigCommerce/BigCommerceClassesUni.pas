
/////////////////////////////////////////////////
//  BigCommerce Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I BigCommerceDac.inc}
unit BigCommerceClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  BigCommerceConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  BigCommerceConstsUni;
{$ENDIF}

type
  TBigCommerceConnection = class;

{ TBigCommerceConnection }

  TBigCommerceConnection = class (TODBCConnection)
  private
    FAuthentication: TAuthenticationType;
    FStoreId: string;
    FClientId: string;
    FAccessToken: string;
    FAuthenticationToken: string;
    FConnectionTimeout: Integer;
    FUTCDates: Boolean;
  protected
    function GetConnectionString: string; override;
  public
    procedure Connect(const ConnectString: string); override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;


implementation

uses
  SyncObjs,
  DAConsts,
{$IFNDEF UNIDACPRO}
  BigCommerceProps;
{$ELSE}
  BigCommercePropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for BigCommerce';
  DriverUrl = 'www.devart.com/odbc/bigcommerce';


{ TBigCommerceConnection }


function TBigCommerceConnection.GetConnectionString: string;
begin
  // Basic
  if FAuthentication = atBasic then
    Result := Format('DRIVER={%s};Server=%s;UID=%s;AuthenticationToken=%s',
      [DriverName, FServer, FUsername, FAuthenticationToken])

  // OAuth
  else
    Result := Format('DRIVER={%s};Authentication Type=OAuth;Store Id=%s;Client Id=%s;Access Token=%s',
      [DriverName, FStoreId, FClientId, FAccessToken]);


  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TBigCommerceConnection.Connect(const ConnectString: string);
begin
  try
    inherited Connect(ConnectString);
  except
    on E: EODBCError do begin
      if E.State = 'IM002' then
        E.Message := DriverName + ' is not installed.'#$D#$A +
                     'You can download the driver at ' + DriverUrl;
      raise;
    end;
    on E: Exception do
      raise;
  end;
end;


function TBigCommerceConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAuthentication:
      Value := Variant(FAuthentication);
    prStoreId:
      Value := FStoreId;
    prClientId:
      Value := FClientId;
    prAccessToken:
      Value := FAccessToken;
    prAuthenticationToken:
      Value := FAuthenticationToken;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TBigCommerceConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAuthentication:
      FAuthentication := TAuthenticationType(Value);
    prStoreId:
      FStoreId := Value;
    prClientId:
      FClientId := Value;
    prAccessToken:
      FAccessToken := Value;
    prAuthenticationToken:
      FAuthenticationToken := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
