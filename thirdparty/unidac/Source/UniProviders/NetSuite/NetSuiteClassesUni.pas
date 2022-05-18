
/////////////////////////////////////////////////
//  NetSuite Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I NetSuiteDac.inc}
unit NetSuiteClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  NetSuiteConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  NetSuiteConstsUni;
{$ENDIF}

type
  TNetSuiteConnection = class;

{ TNetSuiteConnection }

  TNetSuiteConnection = class (TODBCConnection)
  private
    FAuthentication: TAuthenticationType;
    FAccountId: string;
    FRoleId: string;
    FApplicationId: string;
    FCustomTables: boolean;
    FCustomFields: boolean;
    FSandbox: boolean;
    FConsumerKey: string;
    FConsumerSecret: string;
    FToken: string;
    FTokenSecret: string;
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
  NetSuiteProps;
{$ELSE}
  NetSuitePropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for NetSuite';
  DriverUrl = 'www.devart.com/odbc/netsuite';


{ TNetSuiteConnection }


function TNetSuiteConnection.GetConnectionString: string;
begin
  // Basic Authentication
  if FAuthentication = atBasic then
    Result := Format('DRIVER={%s};UID=%s;PWD=%s;AccountId=%s;ApplicationId=%s',
      [DriverName, FUsername, FPassword, FAccountId, FApplicationId])
  // TokenBased Authentication
  else
    Result := Format('DRIVER={%s};AuthenticationType=TokenBased;AccountId=%s;Consumer Key=%s;Consumer Secret=%s;Token Id=%s;Token Secret=%s',
      [DriverName, FAccountId, FConsumerKey, FConsumerSecret, FToken, FTokenSecret]);

  if FRoleId <> '' then
    Result := Result + ';RoleId=' + FRoleId;

  if FCustomTables then
    Result := Result + ';CustomTables=True';

  if FCustomFields then
    Result := Result + ';CustomFields=True';

  if FSandbox then
    Result := Result + ';Sandbox=True';

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TNetSuiteConnection.Connect(const ConnectString: string);
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


function TNetSuiteConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAccountId:
      Value := FAccountId;
    prRoleId:
      Value := FRoleId;
    prApplicationId:
      Value := FApplicationId;
    prCustomTables:
      Value := FCustomTables;
    prCustomFields:
      Value := FCustomFields;
    prSandBox:
      Value := FSandbox;
    prAuthentication:
      Value := FAuthentication;
    prConsumerKey:
      Value := FConsumerKey;
    prConsumerSecret:
      Value := FConsumerSecret;
    prToken:
      Value := FToken;
    prTokenSecret:
      Value := FTokenSecret;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TNetSuiteConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prAccountId:
      FAccountId := Value;
    prRoleId:
      FRoleId := Value;
    prApplicationId:
      FApplicationId := Value;
    prCustomTables:
      FCustomTables := Value;
    prCustomFields:
      FCustomFields := Value;
    prSandbox:
      FSandbox := Value;
    prAuthentication:
      FAuthentication := Value;
    prConsumerKey:
      FConsumerKey := Value;
    prConsumerSecret:
      FConsumerSecret := Value;
    prToken:
      FToken := Value;
    prTokenSecret:
      FTokenSecret := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
