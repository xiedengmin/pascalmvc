
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle ConnectionString
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRParser, CRAccess, CRConnectionString, DAConsts,
{$IFNDEF UNIDACPRO}
  OraConsts, OraCall, OraClasses;
{$ELSE}
  OraConstsUni, OraCallUni, OraClassesUni;
{$ENDIF}

const
  cpHost             = -101; // boolean
  cpPort             = -102; // boolean
  cpSID              = -103; // integer
  cpServiceName      = -104; // integer

type
  TOraConnectionStringBuilder = class(TCRConnectionStringBuilder)
  private
    FServerInfo: TDirectServerInfo;
  protected
    procedure InitParams; override;
    function GetParamValue(Param: TConnectionStringParam): Variant; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;
  public
    constructor Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod); override;
    destructor Destroy; override;
  end;

implementation

uses
  TypInfo,
  CRProps, CRFunctions, CRVio,
{$IFNDEF UNIDACPRO}
  OraProps;
{$ELSE}
  OraPropsUni;
{$ENDIF}

{ TOraConnectionStringBuilder }

constructor TOraConnectionStringBuilder.Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod);
begin
  inherited;

  FServerInfo := TDirectServerInfo.Create;
end;

destructor TOraConnectionStringBuilder.Destroy;
begin
  FServerInfo.Free;

  inherited;
end;

procedure TOraConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppHighest, 'Direct', [], prDirect, varBoolean, DefValDirect);

  AddParam(ppHigh, 'Data Source', ['Server'], prServer, varString, '');

  AddParam(ppNormal, 'Home Name', ['HomeName', 'Home'], prHomeName, varString, '');
  AddParam(ppNormal, 'Host', [], cpHost, varString, '');
  AddParam(ppNormal, 'Port', [], cpPort, varString, '1521'); // varString - support for RAC: Server=db1,db2;Port=1521,1521
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVersion, TypeInfo(TIPVersion) );
  AddParam(ppNormal, 'Service Name', ['ServiceName'], cpServiceName, varString, '');
  AddParam(ppNormal, 'SID', [], cpSID, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connect Mode', ['ConnectMode'], prConnectMode, varEnum, DefValConnectMode, TypeInfo(TConnectMode) );
  AddParam(ppNormal, 'Schema', [], prSchema, varString, '');
  AddParam(ppNormal, 'Character Set', ['CharacterSet', 'Charset'], prCharset, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValOraConnectionTimeout);
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
  AddParam(ppNormal, 'Unicode Environment', ['UnicodeEnvironment'], prUnicodeEnvironment, varBoolean, False);

  AddParam(ppNormal, 'SSL CA Cert', ['SSL CACert'], prSSLCA, varString, '');
  AddParam(ppNormal, 'SSL Cert', [], prSSLCert, varString, '');
  AddParam(ppNormal, 'SSL Key', [], prSSLKey, varString, '');
  AddParam(ppNormal, 'SSL Cipher List', ['SSL Cipher', 'SSL Chipher List', 'SSL CipherList', 'SSL ChipherList'], prSSLCipher, varString, '');
  AddParam(ppNormal, 'SSL ServerCertDN', ['SSL Server CertDN', 'SSL Server Cert DN', 'SSLServerCertDN'], prSSLServerCertDN, varString, '');

  AddParam(ppNormal, 'Http Url', [], prHttpUrl, varString, '');
  AddParam(ppNormal, 'Http User Name', ['Http Username'], prHttpUsername, varString, '');
  AddParam(ppNormal, 'Http Password', [], prHttpPassword, varString, '');
  AddParam(ppNormal, 'Http Trust Server Certificate', ['HttpTrustServerCertificate'], prHttpTrustServerCertificate, varBoolean, False);

  AddParam(ppNormal, 'Proxy Host Name', ['Proxy Hostname'], prProxyHostname, varString, '');
  AddParam(ppNormal, 'Proxy Port', [], prProxyPort, varInteger, DefValProxyPort);
  AddParam(ppNormal, 'Proxy User Name', ['Proxy Username'], prProxyUsername, varString, '');
  AddParam(ppNormal, 'Proxy Password', [], prProxyPassword, varString, '');

end;

function TOraConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
var
  Direct: boolean;
begin
  Direct := GetProp(prDirect);

  case Param.Code of
    prServer:
      if Direct then
        Result := Null
      else
        Result := GetProp(prServer);
    cpHost:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        Result := FServerInfo.Host;
      end
      else
        Result := Null;
    cpPort:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        if FServerInfo.Port <> '' then
          Result := FServerInfo.Port
        else
          Result := Null;
      end
      else
        Result := Null;
    cpSID:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        Result := FServerInfo.SID;
      end
      else
        Result := Null;
    cpServiceName:
      if Direct then  begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        Result := FServerInfo.ServiceName;
      end
      else
        Result := Null;
    prIPVersion:
      if Direct then
        Result := GetProp(prIPVersion)
      else
        Result := Null;
    else
      Result := inherited GetParamValue(Param);
  end;
end;

procedure TOraConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
var
  Direct: boolean;
begin
  Direct := GetProp(prDirect);

  case Param.Code of
    prServer:
      SetProp(prServer, Value);
    cpHost:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        FServerInfo.Host := VarToStr(Value);
        SetProp(prServer, FServerInfo.GetServerInfo);
      end;
    cpPort:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        FServerInfo.Port := VarToStr(Value);
        SetProp(prServer, FServerInfo.GetServerInfo);
      end;
    cpSID:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        FServerInfo.SID := VarToStr(Value);
        SetProp(prServer, FServerInfo.GetServerInfo);
      end;
    cpServiceName:
      if Direct then begin
        FServerInfo.SetServerInfo(GetProp(prServer));
        FServerInfo.ServiceName := VarToStr(Value);
        SetProp(prServer, FServerInfo.GetServerInfo);
      end;
    prIPVersion:
      if Direct then
        SetProp(prIPVersion, Value);
  else
    inherited SetParamValue(Param, Value);
  end;
end;

end.
