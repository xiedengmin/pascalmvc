
/////////////////////////////////////////////////
//  QuickBooks Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I QuickBooksDac.inc}
unit QuickBooksClassesUni;

interface

uses
  Classes, SysUtils, Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRTypes, CRFunctions, CRProps, CRAccess, CRDataTypeMap,
{$IFNDEF UNIDACPRO}
  ODBCConsts, ODBCCall, ODBCClasses, ODBCError, ODBCDataTypeMap,
  QuickBooksConsts;
{$ELSE}
  ODBCConstsUni, ODBCCallUni, ODBCClassesUni, ODBCErrorUni, ODBCDataTypeMapUni,
  QuickBooksConstsUni;
{$ENDIF}

type
  TQuickBooksConnection = class;

{ TQuickBooksConnection }

  TQuickBooksConnection = class (TODBCConnection)
  private
    FCompanyId: string;
    FRefreshToken: string;
    FSandbox: boolean;
    FAccessToken: string;       // deprecated for OAuth1
    FAccessTokenSecret: string; // deprecated for OAuth1
    FConsumerKey: string;       // deprecated for OAuth1
    FConsumerKeySecret: string; // deprecated for OAuth1
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
  QuickBooksProps;
{$ELSE}
  QuickBooksPropsUni;
{$ENDIF}


const
  DriverName = 'Devart ODBC Driver for QuickBooks';
  DriverUrl = 'www.devart.com/odbc/quickbooks';


{ TQuickBooksConnection }


function TQuickBooksConnection.GetConnectionString: string;
begin
//  Result := Format('DRIVER={%s};CompanyId=%s;AccessToken=%s;AccessTokenSecret=%s;ConsumerKey=%s;ConsumerKeySecret=%s',
//    [DriverName, FCompanyId, FAccessToken, FAccessTokenSecret, FConsumerKey, FConsumerKeySecret]);

  Result := Format('DRIVER={%s};CompanyId=%s;RefreshToken=%s',
    [DriverName, FCompanyId, FRefreshToken]);

  if FSandbox then
    Result := Result + ';Sandbox=True';

  if FConnectionTimeout > 0 then
    Result := Result + ';ConnectionTimeout=' + IntToStr(FConnectionTimeout);

  if FUTCDates then
    Result := Result + ';UTCDates=True';
end;

procedure TQuickBooksConnection.Connect(const ConnectString: string);
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


function TQuickBooksConnection.GetProp(Prop: integer; out Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prCompanyId:
      Value := FCompanyId;
    prRefreshToken:
      Value := FRefreshToken;
    prAccessToken:
      Value := FAccessToken;
    prAccessTokenSecret:
      Value := FAccessTokenSecret;
    prConsumerKey:
      Value := FConsumerKey;
    prConsumerKeySecret:
      Value := FConsumerKeySecret;
    prSandBox:
      Value := FSandbox;
    prConnectionTimeout:
      Value := FConnectionTimeout;
    prUTCDAtes:
      Value := FUTCDates;
    else
      Result := inherited GetProp(Prop, Value);
  end;
end;

function TQuickBooksConnection.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;

  case Prop of
    prCompanyId:
      FCompanyId := Value;
    prRefreshToken:
      FRefreshToken := Value;
    prAccessToken:
      FAccessToken := Value;
    prAccessTokenSecret:
      FAccessTokenSecret := Value;
    prConsumerKey:
      FConsumerKey := Value;
    prConsumerKeySecret:
      FConsumerKeySecret := Value;
    prSandbox:
      FSandbox := Value;
    prConnectionTimeout:
      FConnectionTimeout := Value;
    prUTCDates:
      FUTCDates := Value;
    else
      Result := inherited SetProp(Prop, Value);
  end;
end;


end.
