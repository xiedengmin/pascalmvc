
/////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoConnectionStringUni;

interface

uses
  SysUtils,
  CRConnectionString;

type
  TMongoConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRProps,
  CRAccess,
  DAConsts,
{$IFNDEF UNIDACPRO}
  MongoProps, MongoConsts;
{$ELSE}
  MongoPropsUni, MongoConstsUni;
{$ENDIF}

{ TMongoConnectionStringBuilder }

procedure TMongoConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Server', 'Host'], prServer, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, MnDefValPort, [0]);
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, False);
  AddParam(ppNormal, 'Client Library', ['ClientLibrary'], prClientLibrary, varString, '');
  AddParam(ppNormal, 'BSON Library', ['BSONLibrary'], prBSONLibrary, varString, '');
  AddParam(ppNormal, 'Additional Servers', ['AdditionalServers'], prAdditionalServers, varString, '');
  AddParam(ppNormal, 'Connection Options', ['ConnectionOptions'], prConnectionOptions, varString, '');
end;

end.
