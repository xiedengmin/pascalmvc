/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Nexus ConnectionString
//  Created:            17.09.14
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I NexusDac.inc}
unit NexusConnectionStringUni;
{$ENDIF}
interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString;

type
  TNexusConnectionStringBuilder = class(TCRConnectionStringBuilder)
  public
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  DAConsts,
{$IFNDEF UNIDACPRO}
  NexusConsts,
  NexusClasses,
  NexusProps;
{$ELSE}
  NexusConstsUni,
  NexusClassesUni,
  NexusPropsUni;
{$ENDIF}

{ TNexusConnectionStringBuilder }

procedure TNexusConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppHigh, 'Server', ['Host', 'Data Source'], prServer, varString, '');

  AddParam(ppNormal, 'Port', [], prPort, varInteger, 0);
  AddParam(ppNormal, 'Database', ['Database Path', 'DatabasePath', 'Database Alias', 'DatabaseAlias'], prDatabase, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
{$IFNDEF DUMMY}
  AddParam(ppNormal, 'Protocol', [], prProtocol, varEnum, NexusDefValProtocol, TypeInfo(TNexusProtocol));
  AddParam(ppNormal, 'Secret Key', ['SecretKey'], prSecretKey, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'LostConnectionTimeout', [], prLostConnectionTimeout, varInteger, 10);
  AddParam(ppNormal, 'CommandTimeout', [], prCommandTimeout, varInteger, 15);
  AddParam(ppNormal, 'HeartbeatInterval', [], prHeartbeatInterval, varInteger, 10);
  AddParam(ppNormal, 'WatchdogInterval', [], prWatchdogInterval, varInteger, 10);
  AddParam(ppNormal, 'DatabaseReadOnly', [], prDatabaseReadOnly, varBoolean, False);
{$ENDIF}
end;

end.
