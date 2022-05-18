
/////////////////////////////////////////////////
//  Salesforce Marketing Cloud Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ExactTargetDac.inc}
unit ExactTargetClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  ExactTargetConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  ExactTargetConstsUni;
{$ENDIF}

type
  TExactTargetConnection = class;

{ TExactTargetConnection }

  TExactTargetConnection = class (TODBCConnection)
  private
    FAuthentication: TAuthenticationType;
    FPartnerIDs: string;
    FAppSandbox: Boolean;
    FAppClientID: string;
    FAppClientSecret: string;
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
  ExactTargetProps;
{$ELSE}
  ExactTargetPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for Salesforce Marketing Cloud';
  DriverUrl = 'www.devart.com/odbc/exacttarget';


{ TExactTargetConnection }


function TExactTargetConnection.GetConnectionString: string;
begin
  // User and Password
  if FAuthentication = atUserAndPassword then
    Result := Format('DRIVER={%s};Server=%s;UID=%s;PWD=%s;PartnerIDs=%s',
      [DriverName, FServer, FUsername, FPassword, FPartnerIDs])

  // App Center
  else
    Result := Format('DRIVER={%s};Authentication=AppCenterClient;AppSandbox=%s;AppClientID=%s;AppClientSecret=%s;PartnerIDs=%s',
      [DriverName, BoolToStr(FAppSandbox, True), FAppClientID, FAppClientSecret, FPartnerIDs]);

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TExactTargetConnection.Connect(const ConnectString: string);
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


function TExactTargetConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAuthentication:
      Value := Variant(FAuthentication);
    prPartnerIDs:
      Value := FPartnerIDs;
    prAppSandbox:
      Value := FAppSandbox;
    prAppClientID:
      Value := FAppClientID;
    prAppClientSecret:
      Value := FAppClientSecret;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TExactTargetConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAuthentication:
      FAuthentication := TAuthenticationType(Value);
    prPartnerIDs:
      FPartnerIDs := Value;
    prAppSandbox:
      FAppSandbox := Value;
    prAppClientID:
      FAppClientID := Value;
    prAppClientSecret:
      FAppClientSecret := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
