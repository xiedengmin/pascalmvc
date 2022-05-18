
{$I UniDac.inc}

unit UniQueryEditor;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, DB, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Uni, DBAccess,
  CREditor, CRTabEditor, CRFrame,
  DASQLComponentEditor, DAQueryEditor, DADataTypeMapFrame,
  UniQueryOptionsFrame;

type
  TUniQueryEditorForm = class(TDAQueryEditorForm)
    shOptions: TTabSheet;
    btMacros: TButton;
    procedure btMacrosClick(Sender: TObject);

  protected
    FOptionsFrame: TUniQueryOptionsFrame;

    procedure DoInit; override;
    function GetFrameByInitProp: TCRFrame; override;

    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; override;
  public
    property Query;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CRColFrame, DASQLFrame, DAParamsFrame, UniParamsFrame, DAMacrosFrame,
  DASPCallFrame, DASQLGeneratorFrame, DAUpdateSQLFrame,
  UniSPCallFrame, UniDesignUtils, UniConnectionEditor, UniDataTypeMapFrame, DAConditionsFrame;

{ TUniQueryEditorForm }

procedure TUniQueryEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TUniParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TUniSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  FOptionsFrame := AddTab(TUniQueryOptionsFrame, shOptions) as TUniQueryOptionsFrame;
  FDataTypeMapFrame := AddTab(GetDataTypeMapFrameClass, shDataTypeMap) as TDADataTypeMapFrame;
  FConditionsFrame := AddTab(TDAConditionsFrame, shConditions) as  TDAConditionsFrame;

  btMacros.Enabled := Query.Connection <> nil;

  inherited;

  if (Query <> nil) and (TUniUtils.CanGetProvider(TCustomUniDataSet(Query).Connection)) then begin
    shParameters.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsParamsSupported;
    shMacros.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsMacrosSupported;
    shGeneratorSPC.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsStoredProcSupported;
    shEditSQL.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsUpdateSQLSupported;
    shGenerator.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsUpdateSQLSupported;
  end;
end;

function TUniQueryEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

function TUniQueryEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TUniDataTypeMapFrame;
end;

procedure TUniQueryEditorForm.btMacrosClick(Sender: TObject);
var
  CREditor: TCREditorForm;
  mr: integer;
begin
  if Query.Connection <> nil then begin
    CREditor := TUniConnectionEditorForm.Create(nil, TUniDesignUtils);
    try
      CREditor.Component := Query.Connection;
      TCREditorForm(CREditor).InitialProperty := 'Macros';

      mr := CREditor.ShowModal;
      if mr = mrOk then
        Modified := True;
    finally
      CREditor.Free;
    end;
  end;
end;

end.
