
//////////////////////////////////////////////////
//  MS Access Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I AccessDac.inc}
unit AccessServicesUni;
{$ENDIF}

interface

uses
{$IFNDEF CLR}
  CLRClasses,
{$ELSE}
  System.Text,
{$ENDIF}
  SysUtils, Classes, Variants, DB,
  MemUtils, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRParser, CRAccess, DBAccess, DAScript, DADump,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, AccessClasses;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, AccessClassesUni;
{$ENDIF}

type
{  TCustomAccessDataSetUpdater }

  TCustomAccessDataSetUpdater = class(TCustomODBCDataSetUpdater)
  protected
    function GetIdentityFieldValue(var Value: variant): boolean; override;
  end;

{ TCustomAccessDataSetService }

  TCustomAccessDataSetService = class(TCustomODBCDataSetService)
  protected
    procedure CreateDataSetUpdater; override;
  end;

implementation

{ TCustomAccessDataSetUpdater }

function TCustomAccessDataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'SELECT @@IDENTITY');
  Result := True;
end;

{ TCustomAccessDataSetService }

procedure TCustomAccessDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomAccessDataSetUpdater.Create(Self));
end;

end.
