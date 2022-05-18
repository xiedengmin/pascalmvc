
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright � 1998-2021 Devart. All right reserved.
//  Stored Proc Call Generator Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSPCallFrame;
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
  Classes, SysUtils, DBAccess, CRFrame, CRTabEditor,
  DASPCallFrame, CREditor;

type
  TUniSPCallFrame = class(TDASPCallFrame)
    btCreateSQL: TSpeedButton;
    cbAllProcs: TCheckBox;
    procedure btCreateSQLClick(Sender: TObject);
    procedure cbAllProcsClick(Sender: TObject);
  protected
    function ShowAllProc: boolean; override;
  end;

implementation

{$IFNDEF FPC}
{$R UniSPCallFrame.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  Uni, DASQLFrame, DAConsts;

{ TUniSPCallFrame }

procedure TUniSPCallFrame.btCreateSQLClick(Sender: TObject);
begin
  CreateProcedureCall;
end;

procedure TUniSPCallFrame.cbAllProcsClick(Sender: TObject);
begin
  FListGot := False;
end;

function TUniSPCallFrame.ShowAllProc: boolean;
begin
  Result := cbAllProcs.Checked;
end;

end.