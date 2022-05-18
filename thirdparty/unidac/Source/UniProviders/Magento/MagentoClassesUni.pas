
/////////////////////////////////////////////////
//  Magento Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MagentoDac.inc}
unit MagentoClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  MagentoConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  MagentoConstsUni;
{$ENDIF}

type
  TMagentoConnection = class;

{ TMagentoConnection }

  TMagentoConnection = class (TODBCConnection)
  private
    FApiVersion: TApiVersion;
    FApiKey: string;
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
  MagentoProps;
{$ELSE}
  MagentoPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for Magento';
  DriverUrl = 'www.devart.com/odbc/magento';


{ TMagentoConnection }

constructor TMagentoConnection.Create;
begin
  inherited;

  FApiVersion := DefApiVersion;
end;


function TMagentoConnection.GetConnectionString: string;
begin
  // Magento Version2
  if FApiVersion = apiVer2 then
    Result := Format('DRIVER={%s};Version=Ver2;Domain=%s;UID=%s;PWD=%s',
      [DriverName, FServer, FUsername, FPassword])

  // Magento Version1
  else
    Result := Format('DRIVER={%s};Version=Ver1;Domain=%s;UID=%s;ApiKey=%s',
      [DriverName, FServer, FUsername, FApiKey]);

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TMagentoConnection.Connect(const ConnectString: string);
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


function TMagentoConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      Value := Variant(FApiVersion);
//    prDomain:
//      Value := FServer;
    prApiKey:
      Value := FApiKey;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TMagentoConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      FApiVersion := TApiVersion(Value);
//    prDomain:
//      FServer := Value;
    prApiKey:
      FApiKey := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
