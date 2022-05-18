
/////////////////////////////////////////////////
//  MailChimp Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MailChimpDac.inc}
unit MailChimpClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  MailChimpConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  MailChimpConstsUni;
{$ENDIF}

type
  TMailChimpConnection = class;

{ TMailChimpConnection }

  TMailChimpConnection = class (TODBCConnection)
  private
    FApiKey: string;
    FApiVersion: TApiVersion;
    FMergeCustomFields: TMergeCustomFields;
    FMergeTagAsFieldName: Boolean;
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
  MailChimpProps;
{$ELSE}
  MailChimpPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for MailChimp';
  DriverUrl = 'www.devart.com/odbc/mailchimp';


{ TMailChimpConnection }

constructor TMailChimpConnection.Create;
begin
  inherited;

  FMergeCustomFields := DefMergeCustomFields;
  FApiVersion := DefApiVersion;
end;


function TMailChimpConnection.GetConnectionString: string;
begin
  Result := Format('DRIVER={%s};UID=%s;PWD=%s;ApiKey=%s',
    [DriverName, FUsername, FPassword, FApiKey]);

  if FApiVersion = apiVer2 then
    Result := Result + ';API Version=2'
  else
    Result := Result + ';API Version=3';

  case FMergeCustomFields of
    mcfNone:
      Result := Result + ';MergeFieldsDetectionBehavior=None';
    mcfJoinCommon:
      Result := Result + ';MergeFieldsDetectionBehavior=JoinCommon';
    mcfJoinAll:
      Result := Result + ';MergeFieldsDetectionBehavior=JoinAll';
  end;
  if FMergeTagAsFieldName then
    Result := Result + ';UseMergeTagAsFieldName=True';

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TMailChimpConnection.Connect(const ConnectString: string);
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


function TMailChimpConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiKey:
      Value := FApiKey;
    prApiVersion:
      Value := Variant(FApiVersion);
    prMergeCustomFields:
      Value := Variant(FMergeCustomFields);
    prMergeTagAsFieldName:
      Value := FMergeTagAsFieldName; 
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TMailChimpConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prApiKey:
      FApiKey := Value;
    prApiVersion:
      FApiVersion := TApiVersion(Value);
    prMergeCustomFields:
      FMergeCustomFields := TMergeCustomFields(Value);
    prMergeTagAsFieldName:
      FMergeTagAsFieldName := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
