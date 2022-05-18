
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  PostgreSQL ConnectionString
//////////////////////////////////////////////////

{$I PgDac.inc}
unit PgConnectionStringUni;

interface

uses
  SysUtils,
  CRConnectionString;

type
  TCustomPgConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

  TPgConnectionStringBuilder = class(TCustomPgConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  TypInfo,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRProps, CRAccess, CRVio,
  DAConsts,
{$IFNDEF UNIDACPRO}
  PgProps, PgClasses, PgConsts, PgCall;
{$ELSE}
  UniConsts,
  PgPropsUni, PgClassesUni, PgConstsUni, PgCallUni;
{$ENDIF}

{ TCustomPgConnectionStringBuilder }

procedure TCustomPgConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Server', 'Host'], prServer, varString, '');
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVersion, TypeInfo(TIPVersion));

  AddParam(ppNormal, 'SSL CA Cert', ['SSL CACert'], prSSLCA, varString, '');
  AddParam(ppNormal, 'SSL Cert', [], prSSLCert, varString, '');
  AddParam(ppNormal, 'SSL Cipher List', ['SSL Cipher', 'SSL Chipher List', 'SSL CipherList', 'SSL ChipherList'], prSSLCipher, varString, '');
  AddParam(ppNormal, 'SSL Key', [], prSSLKey, varString, '');
  AddParam(ppNormal, 'SSL Mode', [], prSSLMode, varEnum, DefValSSLMode, TypeInfo(TSSLMode));

  AddParam(ppNormal, 'Use Http', ['UseHttp'], prUseHttp, varBoolean, False);
  AddParam(ppNormal, 'Http Url', [], prHttpUrl, varString, '');
  AddParam(ppNormal, 'Http User Name', ['Http Username'], prHttpUsername, varString, '');
  AddParam(ppNormal, 'Http Password', [], prHttpPassword, varString, '');
  AddParam(ppNormal, 'Http Trust Server Certificate', ['HttpTrustServerCertificate'], prHttpTrustServerCertificate, varBoolean, False);

  AddParam(ppNormal, 'Proxy Host Name', ['Proxy Hostname'], prProxyHostname, varString, '');
  AddParam(ppNormal, 'Proxy Port', [], prProxyPort, varInteger, DefValProxyPort);
  AddParam(ppNormal, 'Proxy User Name', ['Proxy Username'], prProxyUsername, varString, '');
  AddParam(ppNormal, 'Proxy Password', [], prProxyPassword, varString, '');

end;

{ TPgConnectionStringBuilder }

procedure TPgConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Protocol', ['Protocol Version', 'ProtocolVersion'], prProtocolVersion, varEnum, DefValProtocol, TypeInfo(TProtocolVersion));
  AddParam(ppNormal, 'Port', [], prPort, varInteger, PgDefValPort, [0]);
  AddParam(ppNormal, 'Schema', ['Initial Schema', 'InitialSchema'], prSchema, varString, '');
  AddParam(ppNormal, 'Character Set', ['CharacterSet', 'Charset'], prCharset, varString, '');
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
  AddParam(ppNormal, 'Multiple Connections', ['MultipleConnections'], prMultipleConnections, varBoolean, DefValMultipleConnections);
end;

end.
