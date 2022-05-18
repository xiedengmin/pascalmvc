/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  ASE ConnectionString
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCClasses, ODBCConnectionString,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClasses,{$ENDIF}
  ASEConnection;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCClassesUni, ODBCConnectionStringUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}ASEClassesUni,{$ENDIF}
  ASEConnectionUni;
{$ENDIF}

type

  TASEConnectionStringBuilder = class({$IFDEF ODBC_PROVIDER}TCustomODBCConnectionStringBuilder{$ELSE}TCRConnectionStringBuilder{$ENDIF})
  protected
    procedure InitParams; override;
    function GetParamValue(Param: TConnectionStringParam): Variant; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;
  end;

implementation

uses
  TypInfo,
  CRProps, CRVio,DAConsts,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCProps,{$ENDIF}
  ASEConsts, ASEProps;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCPropsUni,{$ENDIF}
  ASEConstsUni, ASEPropsUni;
{$ENDIF}

{ TASEConnectionStringBuilder }

procedure TASEConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppHighest, 'Direct', [], prDirect, varBoolean, False);

  AddParam(ppNormal, 'Data Source', ['Host', 'Server'], prServer, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, DefValPort);
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVersion, TypeInfo(TIPVersion));
  AddParam(ppNormal, 'MultipleConnections', ['Multiple Connections'], prMultipleConnections, varBoolean, DefValMultipleConnections);
  AddParam(ppNormal, 'QuotedIdentifier', [], prQuotedIdentifier, varBoolean, DefValQuotedIdentifier);

{$IFNDEF ODBC_PROVIDER}
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
{$ELSE}
  AddParam(ppNormal, 'DetectFieldsOnPrepare', [], prDetectFieldsOnPrepare, varBoolean, True);
{$ENDIF}

  AddParam(ppNormal, 'AnsiNull', [], prAnsiNull, varBoolean, True);
  AddParam(ppNormal, 'ApplicationName', [], prApplicationName, varString, '');
  AddParam(ppNormal, 'CharSet', [], prCharSet, varString, '');
  AddParam(ppNormal, 'ClientHostName', [], prClientHostName, varString, '');
  AddParam(ppNormal, 'EncryptPassword', [], prEncryptPassword, varEnum, epDisable, TypeInfo(TASEEncryptPassword));
  AddParam(ppNormal, 'PrepareMethod', [], prPrepareMethod, varEnum, pmPartial, TypeInfo(TASEPrepareMethod));
  AddParam(ppNormal, 'SelectMethod', [], prSelectMethod, varEnum, smDirect, TypeInfo(TASESelectMethod));
end;

function TASEConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
var
  Direct: boolean;
begin
  case Param.Code of
    prIPVersion: begin
      Direct := GetProp(prDirect);
      if not Direct then
        Result := Null
      else
        Result := GetProp(prIPVersion);
    end
    else
      Result := inherited GetParamValue(Param);
  end;
end;

procedure TASEConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
var
  Direct: boolean;
begin
  case Param.Code of
    prIPVersion: begin
      Direct := GetProp(prDirect);
      if Direct then
        SetProp(prIPVersion, Value);
    end
  else
    inherited SetParamValue(Param, Value);
  end;
end;

end.
