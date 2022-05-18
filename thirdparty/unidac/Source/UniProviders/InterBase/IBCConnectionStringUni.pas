
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  InterBase ConnectionString
//////////////////////////////////////////////////

{$I IbDac.inc}
unit IBCConnectionStringUni;

interface

uses
  SysUtils,
  CRConnectionString;

type
  TIBCConnectionStringBuilder = class(TCRConnectionStringBuilder)
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
{$IFDEF VER5}
  CRFunctions,
{$ENDIF}
  DAConsts,
{$IFNDEF UNIDACPRO}
  IBCProps, IBCClasses, IBCConsts;
{$ELSE}
  UniConsts,
  IBCPropsUni, IBCClassesUni, IBCConstsUni;
{$ENDIF}

{ TIBCConnectionStringBuilder }

procedure TIBCConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Host', 'Server'], prServer, varString, '');
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, DefaultIBDACPort, [0]);
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Client Library', ['ClientLibrary'], prClientLibrary, varString, '');
  AddParam(ppNormal, 'Character Set', ['CharacterSet', 'Charset'], prCharset, varString, '');
  AddParam(ppNormal, 'Protocol', [], prProtocol, varString, DefaultIBDACSProtocol, ['0']);
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVer, TypeInfo(TIPVersion));
  AddParam(ppNormal, 'Use SSL', ['UseSSL'], prUseSSL, varBoolean, False);
  AddParam(ppNormal, 'SSL ServerPublicFile', ['SSL Server Public File', 'SSLServerPublicFile'], prServerPublicFile, varString, '');
  AddParam(ppNormal, 'SSL ServerPublicPath', ['SSL Server Public Path', 'SSLServerPublicPath'], prServerPublicPath, varString, '');
  AddParam(ppNormal, 'SSL ClientCertFile', ['SSL Client Cert File', 'SSLClientCertFile'], prClientCertFile, varString, '');
  AddParam(ppNormal, 'SSL ClientPassPhraseFile', ['SSL Client Passphrase File', 'SSLClientPassPhraseFile'], prClientPassPhraseFile, varString, '');
  AddParam(ppNormal, 'SSL ClientPassPhrase', ['SSL Client Passphrase', 'SSLClientPassPhrase'], prClientPassPhrase, varString, '');
  AddParam(ppNormal, 'Role', [], prRole, varString, '');
  AddParam(ppNormal, 'SQLDialect', [], prSQLDialect, varInteger, 3);
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
end;

end.
