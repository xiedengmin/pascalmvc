/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  DBF ConnectionString
//////////////////////////////////////////////////

{$I DBFDac.inc}
unit DBFConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess, CRConnectionString,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCConnectionString,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClasses,{$ENDIF}
  DBFConnection;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCConnectionStringUni,{$ENDIF}
  {$IFDEF ODBC_PROVIDER}DBFClassesUni,{$ENDIF}
  DBFConnectionUni;
{$ENDIF}

type

  TDBFConnectionStringBuilder = class({$IFDEF ODBC_PROVIDER}TCustomODBCConnectionStringBuilder{$ELSE}TCRConnectionStringBuilder{$ENDIF})
  public
    procedure InitParams; override;
    function GetParamValue(Param: TConnectionStringParam): Variant; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;
  end;

implementation

uses
  CRProps,
{$IFNDEF UNIDACPRO}
  DBFProps, DBFConsts;
{$ELSE}
  DBFPropsUni, DBFConstsUni;
{$ENDIF}

{ TDBFConnectionStringBuilder }

procedure TDBFConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'CollatingSequence', [], prCollatingSequence, varString, '');
  AddParam(ppNormal, 'Direct', [], prDirect, varBoolean, False);
  AddParam(ppNormal, 'DBFFormat', ['DBF Format'], prDBFFormat, varEnum, Variant(dfAuto), TypeInfo(TDBFFormat), 'df');
  AddParam(ppNormal, 'CodePage', ['Code Page'], prCodePage, varEnum, Variant(dpDefault), TypeInfo(TDBFCodePage), 'dp');
  AddParam(ppNormal, 'ConnectMode', ['Connect Mode'], prConnectMode, varEnum, Variant(cmShared), TypeInfo(TDBFConnectMode), 'cm');
  AddParam(ppNormal, 'IndexOnReading', ['Index On Reading'], prIndexOnReading, varEnum, Variant(ikNative), TypeInfo(TDBFIndexKind), 'ik');
  AddParam(ppNormal, 'IgnoreDataErrors', ['Ignore Data Errors'], prIgnoreDataErrors, varBoolean, False);
  AddParam(ppNormal, 'IgnoreMetadataErrors', ['Ignore Metadata Errors'], prIgnoreMetadataErrors, varBoolean, False);
  AddParam(ppNormal, 'IgnoreBrokenTables', ['Ignore Broken Tables'], prIgnoreBrokenTables, varBoolean, False);
  AddParam(ppNormal, 'IgnoreIndexErrors', ['Ignore Index Errors'], prIgnoreIndexErrors, varBoolean, False);
  AddParam(ppNormal, 'IdentifierCase', ['Identifier Case'], prIdentifierCase, varEnum, Variant(icOriginal), TypeInfo(TDBFIdentifierCase), 'ic');
  AddParam(ppNormal, 'AllFieldsAsNullable', ['All Fields As Nullable'], prAllFieldsAsNullable, varBoolean, False);
end;

function TDBFConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
begin
  case Param.Code of
    prIgnoreIndexErrors:
      Result := SuppressIndexOpenErrors;
    else
      Result := inherited GetParamValue(Param);
  end;
end;

procedure TDBFConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
begin
  case Param.Code of
    prIgnoreIndexErrors:
      SuppressIndexOpenErrors := Value;
    else
      inherited SetParamValue(Param, Value);
  end;
end;

end.
