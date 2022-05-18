
//////////////////////////////////////////////////
//  DBF Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I DBFDac.inc}
unit DBFServicesUni;
{$ENDIF}

interface

uses
  CLRClasses,
  SysUtils, Classes, Variants, DB,
  CRTypes, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump,
{$IFNDEF UNIDACPRO}
  {$IFDEF ODBC_PROVIDER}ODBCClasses, ODBCParser, ODBCCall, ODBCServices,{$ENDIF}
  DBFConnection;
{$ELSE}
  {$IFDEF ODBC_PROVIDER}ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni,{$ENDIF}
  DBFConnectionUni;
{$ENDIF}

type

{  TCustomDBFDataSetUpdater }

  TCustomDBFDataSetUpdater = class({$IFDEF ODBC_PROVIDER}TCustomODBCDataSetUpdater{$ELSE}TDADataSetUpdater{$ENDIF})
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomDBFDataSetService }

  TCustomDBFDataSetService = class({$IFDEF ODBC_PROVIDER}TCustomODBCDataSetService{$ELSE}TDADataSetService{$ENDIF})
  protected
  {$IFNDEF ODBC_PROVIDER}
    function DetectCanModify: boolean; override;
  {$ENDIF}
    procedure CreateDataSetUpdater; override;
  end;

{$IFNDEF ODBC_PROVIDER}

{ TCustomDBFFieldTypeMap }

  TCustomDBFFieldTypeMap = class(TDAFieldTypeMap)
  end;

{$ENDIF}

implementation

uses
  CRProps;

{ TCustomDBFDataSetUpdater }

function TCustomDBFDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
{$IFDEF DBFENGINE}
  GetIRecordSet.GetConnection.GetProp(prLastInsertId, Value);
{$ELSE}
  Value := SelectDBValue('Get identity value', 'SELECT @@IDENTITY');
{$ENDIF}
  Result := True;
end;

{ TCustomDBFDataSetService }

{$IFNDEF ODBC_PROVIDER}

function TCustomDBFDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

{$ENDIF}

procedure TCustomDBFDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomDBFDataSetUpdater.Create(Self));
end;

end.
