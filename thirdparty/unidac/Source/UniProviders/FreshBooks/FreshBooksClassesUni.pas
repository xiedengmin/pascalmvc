
/////////////////////////////////////////////////
//  FreshBooks Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I FreshBooksDac.inc}
unit FreshBooksClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  FreshBooksConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  FreshBooksConstsUni;
{$ENDIF}

type
  TFreshBooksConnection = class;

{ TFreshBooksConnection }

  TFreshBooksConnection = class (TODBCConnection)
  private
    FApiVersion: TApiVersion;
    FCompanyName: string;
    FAccessToken: string;
    FRefreshToken: string;
    FAuthenticationToken: string;
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
  FreshBooksProps;
{$ELSE}
  FreshBooksPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for FreshBooks';
  DriverUrl = 'www.devart.com/odbc/freshbooks';


{ TFreshBooksConnection }

constructor TFreshBooksConnection.Create;
begin
  inherited;

  FApiVersion := DefApiVersion;
end;


function TFreshBooksConnection.GetConnectionString: string;
begin
  // New FreshBooks
  if FApiVersion = apiNew then begin
    Result := Format('DRIVER={%s};Version=New;CompanyName=%s',
      [DriverName, FCompanyName]);
    if FAccessToken <> '' then
      Result := Result + ';AccessToken=' + FAccessToken;
    if FRefreshToken <> '' then
      Result := Result + ';RefreshToken=' + FRefreshToken;
  end

  // Classic FreshBooks
  else
    Result := Format('DRIVER={%s};Version=Classic;Server=%s;AuthenticationToken=%s',
      [DriverName, FServer, FAuthenticationToken]);

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TFreshBooksConnection.Connect(const ConnectString: string);
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


function TFreshBooksConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      Value := Variant(FApiVersion);
    prServer:
      Result := inherited GetProp(Prop, Value);
    prAuthenticationToken:
      Value := FAuthenticationToken;
    prCompanyName:
      Value := FCompanyName;
    prAccessToken:
      Value := FAccessToken;
    prRefreshToken:
      Value := FRefreshToken;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TFreshBooksConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiVersion:
      FApiVersion := TApiVersion(Value);
    prServer:
      Result := inherited SetProp(Prop, Value);
    prAuthenticationToken:
      FAuthenticationToken := Value;
    prCompanyName:
      FCompanyName := Value;
    prAccessToken:
      FAccessToken := Value;
    prRefreshToken:
      FRefreshToken := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
