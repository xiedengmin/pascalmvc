/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  DB2 ConnectionString
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I DB2Dac.inc}
unit DB2ConnectionStringUni;
{$ENDIF}
interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRAccess,
{$IFNDEF UNIDACPRO}
  DB2Classes, ODBCConnectionString;
{$ELSE}
  DB2ClassesUni, ODBCConnectionStringUni;
{$ENDIF}

type

  TDB2ConnectionStringBuilder = class(TCustomODBCConnectionStringBuilder)
  public
    procedure InitParams; override;
  end;

implementation

uses
  CRProps,
  CRConnectionString,
{$IFNDEF UNIDACPRO}
  DB2Props;
{$ELSE}
  DB2PropsUni;
{$ENDIF}

{ TDB2ConnectionStringBuilder }

procedure TDB2ConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Host', 'Server'], prServer, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, 0);
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'Schema', [], prSchema, varString, '');
  AddParam(ppNormal, 'FunctionPath', [], prFunctionPath, varString, '');
end;

end.
