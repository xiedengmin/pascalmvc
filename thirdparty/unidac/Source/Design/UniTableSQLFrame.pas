
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  UniTableSQL Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniTableSQLFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  Classes, SysUtils,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess, CRFrame, CRTabEditor, CREditor, DATableSQLFrame;

type
  TUniTableSQLFrame = class(TDATableSQLFrame)
    cbAllTables: TCheckBox;
    procedure cbAllTablesClick(Sender: TObject);
  protected
  end;

implementation

{$IFNDEF FPC}
{$R UniTableSQLFrame.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  Uni;

{ TUniTableSQLFrame }

procedure TUniTableSQLFrame.cbAllTablesClick(Sender: TObject);
begin
  FListGot := False;
  FAllTables := cbAllTables.Checked;
end;

end.
