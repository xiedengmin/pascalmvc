/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  ODBC ConnectionString
//////////////////////////////////////////////////

{$I ODBCDac.inc}
unit ODBCConnectionStringUni;

interface

uses
  SysUtils, Classes, 
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  ODBCClasses;
{$ELSE}
  ODBCClassesUni;
{$ENDIF}

type

  TCustomODBCConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

  TODBCConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  DAConsts,
{$IFNDEF UNIDACPRO}
  ODBCProps;
{$ELSE}
  ODBCPropsUni;
{$ENDIF}

{ TCustomODBCConnectionStringBuilder }

procedure TCustomODBCConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Server', [], prServer, varString, '');
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
end;

{ TODBCConnectionStringBuilder }

procedure TODBCConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppHighest, 'DSN Type', ['DSNType'], prDSNType, varEnum, ntAuto, TypeInfo(TDSNType));

  AddParam(ppNormal, 'DetectFieldsOnPrepare', [], prDetectFieldsOnPrepare, varBoolean, True);
  AddParam(ppNormal, 'VarBinaryAsBlob', [], prVarBinaryAsBlob, varBoolean, False);
  AddParam(ppNormal, 'LongVarBinaryAsBlob', [], prLongVarBinaryAsBlob, varBoolean, True);
end;

end.
