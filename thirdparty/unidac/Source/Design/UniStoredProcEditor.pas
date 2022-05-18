{$IFNDEF CLR}

{$I UniDac.inc}

unit UniStoredProcEditor;
{$ENDIF}
interface
uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  SysUtils, DB, Classes, DBAccess,
  CRFrame, DAStoredProcEditor, DADataTypeMapFrame,
  Uni, UniSQLOptionsFrame;

type
  TUniStoredProcEditorForm = class(TDAStoredProcEditorForm)
    shOptions: TTabSheet;

  protected
    FOptionsFrame: TUniSQLOptionsFrame;

    procedure DoInit; override;

    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; override;

    function GetFrameByInitProp: TCRFrame; override;
  public
    property StoredProc;
  end;

implementation

{$IFNDEF FPC}
{$R UniStoredProcEditor.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DASQLFrame, DAPAramsFrame, DAMacrosFrame, DAUpdateSQLFrame, DASQLGeneratorFrame,
  DASPCallFrame, UniParamsFrame, UniSPCallFrame, UniDataTypeMapFrame;

{ TUniStoredProcEditorForm }

procedure TUniStoredProcEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TUniSPCallFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TUniParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FSPCallFrame := AddTab(TUniSPCallFrame, shGeneratorSPC) as TDASPCallFrame;
  FUpdateSQLFrame := AddTab(TDAUpdateSQLFrame, shEditSQL) as TDAUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TDASQLGeneratorFrame, shGenerator) as TDASQLGeneratorFrame;
  FOptionsFrame := AddTab(TUniSQLOptionsFrame, shOptions) as TUniSQLOptionsFrame;
  FDataTypeMapFrame := AddTab(GetDataTypeMapFrameClass, shDataTypeMap) as TDADataTypeMapFrame;
  FDataTypeMapFrame.FieldNameListAllowed := False;
  shGenerator.TabVisible := False;

  inherited;

  if (Query <> nil) and (TUniUtils.CanGetProvider(TCustomUniDataSet(Query).Connection)) then begin
    shParameters.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsParamsSupported;
    shMacros.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsMacrosSupported;
    shGeneratorSPC.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsStoredProcSupported;
    shEditSQL.TabVisible := TUniUtils.GetProvider(TCustomUniDataSet(Query).Connection).IsUpdateSQLSupported;
  end;
end;

function TUniStoredProcEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TUniDataTypeMapFrame;
end;

function TUniStoredProcEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SpecificOptions' then
    Result := FOptionsFrame
  else
    Result := inherited GetFrameByInitProp;
end;

end.
