
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualQueryDesign;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes,  Dialogs, DB,
{$IFDEF FPC}
  PropEdits, ComponentEditors,
{$ELSE}
  {$IFDEF VER6P}
    DesignIntf, DesignEditors,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  {$IFNDEF BCB}
    {$IFDEF VER5P}
      FldLinks,
    {$ENDIF}
    ColnEdit,
  {$ENDIF}
{$ENDIF}
  CRTypes, CRDesign, DADesign,
  VirtualQuery;

type
  TVirtualQueryEditor = class(TDAComponentEditor)
{$IFNDEF BCB}
  private
    procedure ShowSourceDataSetsEditor;
{$ENDIF}
  protected
    procedure InitVerbs; override;
  end;

  TSourceDataSetProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FQuery: TCustomVirtualQuery;

  {$IFNDEF FPC}
    procedure CheckComponent(const Value: string);
  {$ENDIF}
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  DBAccess,
{$IFNDEF FPC}
{$IFNDEF UNIDACPRO}
  VirtualMenu,
{$ENDIF}
{$ENDIF}
  VirtualQueryDesignUtils,
  VirtualQueryEditor;

type
  TDataSetLinksHelper = class(TDataSetLinks)
  end;

procedure Register;
begin
  // Register property editors
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLLock', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLUpdate', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TVirtualQuery, 'SQLRecCount', TDAPropertyEditor);

{$IFNDEF FPC}
  RegisterPropertyEditor(TypeInfo(TDataSet), TDataSetLink, 'DataSet', TSourceDataSetProperty);
{$ENDIF}  

  DARegisterComponentEditor(TVirtualQuery, TVirtualQueryEditor, TVirtualQueryEditorForm, TVirtualDesignUtils);

{$IFNDEF FPC}
{$IFNDEF UNIDACPRO}
  Menu.AddItems(SysInit.HInstance);
{$ENDIF}
{$ENDIF}
end;

{ TVirtualQueryEditor }

{$IFNDEF BCB}
procedure TVirtualQueryEditor.ShowSourceDataSetsEditor;
begin
{$IFNDEF FPC}
  ShowCollectionEditor(Designer, Component, (Component as TVirtualQuery).SourceDataSets, 'SourceDataSets');
{$ELSE}
  TCollectionPropertyEditor.ShowCollectionEditor((Component as TVirtualQuery).SourceDataSets, Component, 'SourceDataSets');
{$ENDIF}
end;
{$ENDIF}

procedure TVirtualQueryEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('VirtualQuery E&ditor...', TVirtualQueryEditorForm, TVirtualDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);
{$IFNDEF BCB}
  AddVerb('Source DataSets &Editor...', ShowSourceDataSetsEditor);
{$ENDIF}

  inherited;
end;

{ TDataSetProperty }

{$IFNDEF FPC}
procedure TSourceDataSetProperty.CheckComponent(const Value: string);
var
  J: Integer;
  Dataset: TDataset;
begin
  Dataset := TDataset(Designer.GetComponent(Value));

  if Dataset = FQuery then
    Exit;

  for J := 0 to PropCount - 1 do
    if TDataSource(GetComponent(J)).IsLinkedTo(Dataset) then
      Exit;
  FCheckProc(Value);
end;
{$ENDIF}

procedure TSourceDataSetProperty.GetValues(Proc: TGetStrProc);
var
  List: {$IFNDEF FPC}IDesignerSelections{$ELSE}TPersistentSelectionList{$ENDIF};
begin
{$IFNDEF FPC}
  List := CreateSelectionList;
  Designer.GetSelections(List);
{$ELSE}
  List := GetSelections;
{$ENDIF}

  if (List.Count > 0) and (List[0] is TDataSetLink) then
    FQuery := TDataSetLinksHelper(TDataSetLink(List[0]).Collection).GetOwner as TCustomVirtualQuery
  else
    FQuery := nil;

  FCheckProc := Proc;

  inherited GetValues({$IFNDEF FPC}CheckComponent{$ELSE}Proc{$ENDIF});
end;

end.
