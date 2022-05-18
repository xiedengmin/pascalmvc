{$IFNDEF CLR}

{$I UniDac.inc}

unit UniTableEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess, Uni,
  CRFrame, DATableEditor, DATableSQLFrame, DADataTypeMapFrame,
  UniTableSQLFrame, UniSQLOptionsFrame, DAConditionsFrame;

type
  TUniTableEditorForm = class(TDATableEditorForm)
    shOptions: TTabSheet;

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;

    function GetSQLFrameClass: TDATableSQLFrameClass; override;
    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; override;
  end;

implementation

{$IFNDEF FPC}
{$R UniTableEditor.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  UniDataTypeMapFrame;

{ TUniTableEditorForm }

procedure TUniTableEditorForm.DoInit;
begin
  inherited;

  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
  FDataTypeMapFrame := AddTab(TUniDataTypeMapFrame, shDataTypeMap) as TDADataTypeMapFrame;
  FConditionsFrame := AddTab(TDAConditionsFrame, shConditions) as  TDAConditionsFrame;
end;

function TUniTableEditorForm.GetSQLFrameClass: TDATableSQLFrameClass;
begin
  Result := TUniTableSQLFrame;
end;

function TUniTableEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TUniDataTypeMapFrame;
end;

function TUniTableEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

end.
