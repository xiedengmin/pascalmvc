
/////////////////////////////////////////////////
//  Salesforce Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SalesforceDac.inc}
unit SalesforceClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  SalesforceConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  SalesforceConstsUni;
{$ENDIF}

type
  TSalesforceConnection = class;

{ TSalesforceConnection }

  TSalesforceConnection = class (TODBCConnection)
  private
    FSecurityToken: string;
    FIncludeDeleted: Boolean;
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
  SalesforceProps;
{$ELSE}
  SalesforcePropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for Salesforce';
  DriverUrl = 'www.devart.com/odbc/salesforce';


{ TSalesforceConnection }


function TSalesforceConnection.GetConnectionString: string;
begin
  Result := Format('DRIVER={%s};Host=%s;UID=%s;PWD=%s;SecurityToken=%s',
    [DriverName, FServer, FUsername, FPassword, FSecurityToken]);

  if FIncludeDeleted then
    Result := Result + ';Include Deleted=True';

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TSalesforceConnection.Connect(const ConnectString: string);
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


function TSalesforceConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prSecurityToken:
      Value := FSecurityToken;
    prIncludeDeleted:
      Value := FIncludeDeleted;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TSalesforceConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prSecurityToken:
      FSecurityToken := Value;
    prIncludeDeleted:
      FIncludeDeleted := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
