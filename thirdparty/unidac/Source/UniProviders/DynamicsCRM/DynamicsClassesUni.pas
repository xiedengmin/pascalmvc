
/////////////////////////////////////////////////
//  Dynamics CRM Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I DynamicsDac.inc}
unit DynamicsClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni;
{$ENDIF}

type
  TDynamicsConnection = class;

{ TDynamicsConnection }

  TDynamicsConnection = class (TODBCConnection)
  private
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
  DynamicsProps;
{$ELSE}
  DynamicsPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for Dynamics CRM';
  DriverUrl = 'www.devart.com/odbc/dynamics';


{ TDynamicsConnection }


function TDynamicsConnection.GetConnectionString: string;
begin
  Result := Format('DRIVER={%s};Server=%s;UID=%s;PWD=%s',
    [DriverName, FServer, FUsername, FPassword]);

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TDynamicsConnection.Connect(const ConnectString: string);
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


function TDynamicsConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TDynamicsConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
