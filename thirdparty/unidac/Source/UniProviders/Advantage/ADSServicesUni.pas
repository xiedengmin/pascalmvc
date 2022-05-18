
//////////////////////////////////////////////////
//  Advantage Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I ADSDac.inc}
unit ADSServicesUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  CRTypes, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump, DASQLGenerator,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, ADSClasses;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, ADSClassesUni;
{$ENDIF}

type

{ TCustomADSSQLGenerator }

  TCustomADSSQLGenerator = class(TDASQLGenerator)
  public
    function GenerateSelectValues(const ValuesList: string): string; override;
  end;

{  TCustomADSDataSetUpdater }

  TCustomADSDataSetUpdater = class(TCustomODBCDataSetUpdater)
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomADSDataSetService }

  TCustomADSDataSetService = class(TCustomODBCDataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
  end;

{ TADSScriptProcessor }

  TADSScriptProcessor = class (TODBCScriptProcessor)
  protected
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
  end;

implementation

uses
  CRFunctions, DAConsts;

{ TCustomADSSQLGenerator }

function TCustomADSSQLGenerator.GenerateSelectValues(const ValuesList: string): string;
begin
  Result := 'SELECT ' + ValuesList + ' FROM system.iota';
end;

{ TCustomADSDataSetUpdater }

function TCustomADSDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT LASTAUTOINC(CONNECTION) FROM system.iota');
  Result := True;
end;

{ TCustomADSDataSetService }

procedure TCustomADSDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomADSDataSetUpdater.Create(Self));
end;

procedure TCustomADSDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomADSSQLGenerator.Create(TDASQLGeneratorService));
end;

{ TADSScriptProcessor }

procedure TADSScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if Code = lxBEGIN then
    StatementType := ST_SPECIFIC_SQL;
end;

end.
