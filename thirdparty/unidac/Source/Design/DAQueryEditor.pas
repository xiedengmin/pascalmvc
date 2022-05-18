
//////////////////////////////////////////////////
//  Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Query Editor
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAQueryEditor;
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
  SysUtils, DB, Classes,
  DBAccess, MemUtils, DASQLGenerator,
  CRFrame, CREditor, CRTabEditor, DASQLFrame, DAParamsFrame, DAMacrosFrame, DASPCallFrame,
  DASQLComponentEditor, DAUpdateSQLFrame, DASQLGeneratorFrame, DADataTypeMapFrame, DAConditionsFrame;

type

  { TDAQueryEditorForm }

  TDAQueryEditorForm = class(TDASQLEditorForm)
    shEditSQL: TTabSheet;
    shGenerator: TTabSheet;
    shDataTypeMap: TTabSheet;
    btnDataEditor: TBitBtn;
    btnCodeEditor: TBitBtn;
    shConditions: TTabSheet;
    procedure btnDataEditorClick(Sender: TObject);
    procedure btnCodeEditorClick(Sender: TObject);
  protected
    FUpdateSQLFrame: TDAUpdateSQLFrame;
    FSQLGeneratorFrame: TDASQLGeneratorFrame;
    FDataTypeMapFrame: TDADataTypeMapFrame;
    FConditionsFrame: TDAConditionsFrame;

    procedure DoInit; override;
    procedure DoActivate; override;
    procedure DoSave; override;

    function GetQuery: TCustomDADataSet;
    procedure SetQuery(Value: TCustomDADataSet);
    function GetComponent: TComponent; override;
    procedure SetComponent(Value: TComponent); override;
    function GetFrameByInitProp: TCRFrame; override;

    function GetDataTypeMapFrameClass: TDADataTypeMapFrameClass; virtual;

    property Query: TCustomDADataSet read GetQuery write SetQuery;

  public
    property UpdateSQLFrame: TDAUpdateSQLFrame read FUpdateSQLFrame;

  end;

implementation

uses 
  DADesignUtils, DADataEditor{$IFDEF USE_CODE_EDITOR}, TypInfo, Menus{$ENDIF}{$IFDEF DBTOOLS}, DBToolsClient{$ENDIF};

{$IFNDEF FPC}
{$IFDEF CLR}
{$R DAQueryEditor.dfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

procedure TDAQueryEditorForm.DoInit;
{$IFDEF USE_CODE_EDITOR}
var
  obj: TPersistent;
{$ENDIF}
begin
  try
    inherited;
  {$IFDEF USE_CODE_EDITOR}
    btnCodeEditor.Enabled := True;
    if not DADesignUtilsClass.DBToolsAvailable then begin
      obj := TPersistent(GetObjectProp(FSQLFrame.meSQL, 'PopupMenu'));
      if obj <> nil then begin
        btnCodeEditor.OnClick := TMenuItem(TPopupMenu(obj).Items[9]).OnClick;
        btnCodeEditor.Tag := 9;
      end;
    end;
  {$ELSE}
    if DADesignUtilsClass.DBToolsAvailable then
      btnCodeEditor.Enabled := True;
  {$ENDIF}
  finally
    Assert(FUpdateSQLFrame <> nil);
    Assert(FSQLGeneratorFrame <> nil);
    //Assert(FConditionsFrame <> nil);
    if FConditionsFrame <> nil then
      FSPCallFrame.Mode := spQuery;
  end;
end;

procedure TDAQueryEditorForm.DoActivate;
begin
  inherited;
  if PageControl.ActivePage = FUpdateSQLFrame.Page then
    ActiveControl := FUpdateSQLFrame.ActiveControl;
end;

procedure TDAQueryEditorForm.DoSave;
var
  OldActive: boolean;
  OldDebug: boolean;
begin
  OldActive := TCustomDADataSet(FComponent).Active;
  OldDebug := TCustomDADataSet(FComponent).Debug;
  try
    // CR-M12021
    TCustomDADataSet(FLocalComponent).MasterSource := TCustomDADataSet(FComponent).MasterSource;
    TCustomDADataSet(FLocalComponent).MasterFields := TCustomDADataSet(FComponent).MasterFields;
    TCustomDADataSet(FLocalComponent).DetailFields := TCustomDADataSet(FComponent).DetailFields;

    inherited;
    TCustomDADataSet(FComponent).Debug := False;
    try
      TCustomDADataSet(FComponent).Active := OldActive;
    except
    end;
  finally
    TCustomDADataSet(FComponent).Debug := OldDebug;
  end;
end;

function TDAQueryEditorForm.GetQuery: TCustomDADataSet;
begin
  Result := FComponent as TCustomDADataSet;
end;

procedure TDAQueryEditorForm.SetQuery(Value: TCustomDADataSet);
begin
  FComponent := Value;
end;

function TDAQueryEditorForm.GetComponent: TComponent;
begin
  Result := Query;
end;

procedure TDAQueryEditorForm.SetComponent(Value: TComponent);
begin
  Query := Value as TCustomDADataSet;
end;

function TDAQueryEditorForm.GetFrameByInitProp: TCRFrame;
begin
  if InitialProperty = 'SQLDelete' then begin
    FUpdateSQLFrame.SetStatementType(stDelete);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLInsert' then begin
    FUpdateSQLFrame.SetStatementType(stInsert);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLRefresh' then begin
    FUpdateSQLFrame.SetStatementType(stRefresh);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLUpdate' then begin
    FUpdateSQLFrame.SetStatementType(stUpdate);  
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLLock' then begin
    FUpdateSQLFrame.SetStatementType(stLock);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'SQLRecCount' then begin
    FUpdateSQLFrame.SetStatementType(stRecCount);
    Result := FUpdateSQLFrame;
  end
  else
  if InitialProperty = 'DataTypeMap' then
    Result := FDataTypeMapFrame
  else
    Result := inherited GetFrameByInitProp;
end;

function TDAQueryEditorForm.GetDataTypeMapFrameClass: TDADataTypeMapFrameClass;
begin
  Result := TDADataTypeMapFrame;
end;

procedure TDAQueryEditorForm.btnDataEditorClick(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  if DADesignUtilsClass.DBToolsAvailable then begin
    DoSave;
    TCustomDBToolsService(DADesignUtilsClass.DBToolsService).RetrieveData(true);
  end
  else
{$ENDIF}
  begin
    SaveControlData;
    CheckConnection(LocalComponent);
    with TDADataEditorForm.Create(nil, FCRDesignUtilsClass) do
      try
        Component := Self.LocalComponent;
        ShowModal;
      finally
        Free;
      end;
  end;
end;

procedure TDAQueryEditorForm.btnCodeEditorClick(Sender: TObject);
begin
{$IFDEF DBTOOLS}
  if DADesignUtilsClass.DBToolsAvailable then
    DoSave;
    TCustomDBToolsService(DADesignUtilsClass.DBToolsService).EditSql(False, FUpdateSQLFrame.StatementType);
{$ENDIF}
end;

end.
