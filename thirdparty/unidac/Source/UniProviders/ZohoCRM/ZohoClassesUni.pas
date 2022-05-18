
/////////////////////////////////////////////////
//  Zoho CRM Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ZohoDac.inc}
unit ZohoClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  ZohoConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  ZohoConstsUni;
{$ENDIF}

type
  TZohoConnection = class;

{ TZohoConnection }

  TZohoConnection = class (TODBCConnection)
  private
    FApiVersion: TApiVersion;
    FAccessToken: string;
    FRefreshToken: string;
    FAuthenticationToken: string;
    FEnableNonApprovedRecords: Boolean;
    FConnectionTimeout: Integer;
    FUTCDates: Boolean;
  protected
    function GetConnectionString: string; override;
  public
    constructor Create; override;

    procedure Connect(const ConnectString: string); override;

    function GetProp(Prop: integer; out Value: variant): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;


implementation

uses
  SyncObjs,
  DAConsts,
{$IFNDEF UNIDACPRO}
  ZohoProps;
{$ELSE}
  ZohoPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for Zoho CRM';
  DriverUrl = 'www.devart.com/odbc/zoho';


{ TZohoConnection }

constructor TZohoConnection.Create;
begin
  inherited;

  FApiVersion := DefApiVersion;
end;


function TZohoConnection.GetConnectionString: string;
begin
  Result := 'DRIVER=' + DriverName;

  if FServer <> '' then
    Result := Result + ';Domain=' + FServer;

  // Zoho Version2
  if FApiVersion = apiVer2 then begin
    Result := Result + ';Version=Ver2';
    if FAccessToken <> '' then
      Result := Result +  ';AccessToken=' + FAccessToken;
    if FRefreshToken <> '' then
      Result := Result + ';RefreshToken=' + FRefreshToken;
  end
  else
    Result := Result +  ';AuthenticationToken=' + FAuthenticationToken;

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';

  if FEnableNonApprovedRecords then
    Result := Result + ';EnableNonApprovedRecords=True';
end;

procedure TZohoConnection.Connect(const ConnectString: string);
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


function TZohoConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      Value := Variant(FApiVersion);
    prAccessToken:
      Value := FAccessToken;
    prRefreshToken:
      Value := FRefreshToken;
    prAuthenticationToken:
      Value := FAuthenticationToken;
    prEnableNonApprovedRecords:
      Value := FEnableNonApprovedRecords;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TZohoConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      FApiVersion := TApiVersion(Value);
    prAccessToken:
      FAccessToken := Value;
    prRefreshToken:
      FRefreshToken := Value;
    prAuthenticationToken:
      FAuthenticationToken := Value;
    prEnableNonApprovedRecords:
      FEnableNonApprovedRecords := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
