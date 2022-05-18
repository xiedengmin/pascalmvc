
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright � 1998-2021 Devart. All right reserved.
//  UniUpdateSQL Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniUpdateSQLEditor;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, ComCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils, DBAccess,
  DAUpdateSQLEditor;

type
  TUniUpdateSQLEditorForm = class(TDAUpdateSQLEditorForm)
  protected
    procedure DoInit; override;
  end;

implementation

{$IFNDEF FPC}
{$R UniUpdateSQLEditor.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DAUpdateSQLFrame, DASQLGeneratorFrame, Uni;

{ TUniUpdateSQLEditorForm }

procedure TUniUpdateSQLEditorForm.DoInit;
begin
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  inherited;
end;

end.
