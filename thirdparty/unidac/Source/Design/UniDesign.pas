
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  UniDAC Design
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniDesign;
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, TypInfo,
{$IFDEF CLR}
  Borland.Vcl.Design.DesignEditors, Borland.Vcl.Design.DesignIntf,
  Borland.Vcl.Design.FldLinks, WinUtils,
{$ELSE}
{$IFDEF FPC}
  PropEdits, ComponentEditors,
{$ELSE}
  DesignIntf, DesignEditors,
  {$IFNDEF BCB}FldLinks, ColnEdit,{$ENDIF}
{$ENDIF}
{$ENDIF}
  UniDacVcl, CRFunctions, CRTypes, DBAccess,
  UniProvider, Uni, UniScript,
{$IFNDEF STD}
  UniDump, UniLoader, UniAlerter,
{$ENDIF}
  CRDesign, DADesign;

procedure Register;

type
{$IFNDEF FPC}
  TUniConnectionList = class (TDAConnectionList)
  protected
    function GetConnectionType: TCustomDAConnectionClass; override;
  end;

  TUniDesignNotification = class(TDADesignNotification)
  public
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent); override;
    function CreateConnectionList: TDAConnectionList; override;
    function GetConnectionPropertyName: string; override;
  end;
{$ENDIF}

  TUniTransactionProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TUniConnectionEditor = class (TDAConnectionEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniConnectDialogPropertyEditor = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TUniQueryEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniTableEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniStoredProcEditor = class (TDAComponentEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniSQLEditor = class (TDASQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniScriptEditor = class (TDAScriptEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniUpdateSQLEditor = class (TDAUpdateSQLEditor)
  protected
    procedure InitVerbs; override;
  end;

  TUniConnectionProviderNameEditor = class (TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TSpecificOptionsEditor = class (TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

implementation

uses
  DAScript, DALoader, DAAlerter,
{$IFNDEF FPC}
  UniMenu,
{$ENDIF}
  UniDesignUtils, UniConnectionEditor, UniQueryEditor,
  UniSQLEditor, UniTableEditor, UniStoredProcEditor, UniScriptEditor,
  UniUpdateSQLEditor, UniSpecificOptionsEditor;

{$IFNDEF FPC}
var
  Notificator: IDesignNotification;
{$ENDIF}

procedure Register;
begin
// Register property editors
  RegisterPropertyEditor(TypeInfo(String), TUniConnection, 'ProviderName',
    TUniConnectionProviderNameEditor);
  RegisterPropertyEditor(TypeInfo(TUniMacros), TUniConnection, 'Macros', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TCustomConnectDialog), TUniConnection, 'ConnectDialog', TUniConnectDialogPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLLock', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLRecCount', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniQuery, 'SQLUpdate', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TDAParams), TUniTable, 'Params', nil);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniSQL, 'SQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniScript, 'SQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQL', nil);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLDelete', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLInsert', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLRecCount', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLRefresh', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLUpdate', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniStoredProc, 'SQLLock', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniUpdateSQL, 'InsertSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniUpdateSQL, 'ModifySQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniUpdateSQL, 'DeleteSQL', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniUpdateSQL, 'RefreshSQL', TDAPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TUniConnection, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCustomUniDataSet, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniSQL, 'SpecificOptions', TDAPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniScript, 'SpecificOptions', TDAPropertyEditor);
{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(TStrings), TUniLoader, 'SpecificOptions', TSpecificOptionsEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniDump, 'SpecificOptions', TSpecificOptionsEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TUniAlerter, 'SpecificOptions', TSpecificOptionsEditor);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TStrings), TUniTransaction, 'SpecificOptions', TSpecificOptionsEditor);

  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniQuery, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniTable, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniSQL, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniStoredProc, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniScript, 'Transaction', TUniTransactionProperty);
{$IFNDEF STD}
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniAlerter, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniLoader, 'Transaction', TUniTransactionProperty);
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniMetaData, 'Transaction', TUniTransactionProperty);
{$ENDIF}
  RegisterPropertyEditor(TypeInfo(TUniTransaction), TUniConnection, 'DefaultTransaction', TUniTransactionProperty);

// Register component editors
  DARegisterComponentEditor(TUniConnection, TUniConnectionEditor, TUniConnectionEditorForm, TUniDesignUtils);
  // DAComponentEditor is needed to display connection select dialog with Delphi 5
  DARegisterComponentEditor(TUniTransaction, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniQuery, TUniQueryEditor, TUniQueryEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniSQL, TUniSQLEditor, TUniSQLEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniTable, TUniTableEditor, TUniTableEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniStoredProc, TUniStoredProcEditor, TUniStoredProcEditorForm, TUniDesignUtils);
  DARegisterComponentEditor(TUniScript, TUniScriptEditor, TUniScriptEditorForm, TUniDesignUtils);
{$IFNDEF STD}
  DARegisterComponentEditor(TUniLoader, TDALoaderEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniDump, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniAlerter, TDAComponentEditor, nil, TUniDesignUtils);
  DARegisterComponentEditor(TUniMetaData, TDAComponentEditor, nil, TUniDesignUtils);
{$ENDIF}
  DARegisterComponentEditor(TUniUpdateSQL, TUniUpdateSQLEditor, TUniUpdateSQLEditorForm, TUniDesignUtils);
{$IFNDEF FPC}
  DARegisterComponentEditor(TUniDataSource, TCRDataSourceEditor, nil, TUniDesignUtils);
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF FPC}
  Menu.AddItems({$IFDEF CLR}WinUtils{$ELSE}SysInit{$ENDIF}.HInstance);
{$ENDIF}
{$ENDIF}
end;

{$IFNDEF FPC}

{ TUniConnectionList }

function TUniConnectionList.GetConnectionType: TCustomDAConnectionClass;
begin
  Result := TUniConnection;
end;

{ TUniDesignNotification }

procedure TUniDesignNotification.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem <> nil) and ((AItem is TCustomUniDataSet)
    or (AItem is TUniScript)
    or (AItem is TUniDataSource)
    or (AItem is TUniTransaction)
    or (AItem is TUniSQL)
  {$IFNDEF STD}
    or (AItem is TUniLoader)
    or (AItem is TUniDump)
    or (AItem is TUniAlerter)
    or (AItem is TUniMetaData)
  {$ENDIF}
    )
  then
    FItem := AItem;
end;

function TUniDesignNotification.CreateConnectionList: TDAConnectionList;
begin
  Result := TUniConnectionList.Create;
end;

function TUniDesignNotification.GetConnectionPropertyName: string;
begin
  if FItem is TUniTransaction then
    Result := 'DefaultConnection'
  else
    Result := 'Connection'
end;
{$ENDIF}

{ TUniTransactionProperty }

function TUniTransactionProperty.GetValue: string;
var
  Component: TComponent;
  FTransaction: TDATransaction;
begin
  Component := GetComponent(0) as TComponent;
  FTransaction := nil;
  if Component is TCustomDADataSet then begin
    FTransaction := TDBAccessUtils.GetFTransaction(TCustomDADataSet(Component));
  end
  else
  if Component is TCustomDASQL then begin
    FTransaction := TDBAccessUtils.GetFTransaction(TCustomDASQL(Component));
  end
  else
  if Component is TDAMetaData then begin
    FTransaction := TDBAccessUtils.GetFTransaction(TDAMetaData(Component));
  end
  else
  if Component is TDAScript then begin
    FTransaction := TDBAccessUtils.GetFTransaction(TDAScriptUtils.GetCommand(TDAScript(Component)));
  end
  else
  if Component is TDALoader then begin
    FTransaction := TDALoaderUtils.GetFTransaction(TDALoader(Component));
  end
  else
  if Component is TDAAlerter then begin
    FTransaction := TDAAlerterUtils.GetFTransaction(TDAAlerter(Component));
  end
  else
  if Component is TCustomDAConnection then
    FTransaction := TDBAccessUtils.GetFDefaultTransaction(TCustomDAConnection(Component))
  else
    Assert(False);

  if FTransaction = nil then
    Result := '<Default>'
  else
    Result := inherited GetValue;
end;

procedure TUniTransactionProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('<Default>');
  inherited GetValues(Proc);
end;

procedure TUniTransactionProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  if (Value = '<Default>') or (Value = '') then begin
    Component := GetComponent(0) as TComponent;
    if Component is TCustomDADataSet then
      TDBAccessUtils.SetTransaction(TCustomDADataSet(Component), nil)
    else
    if Component is TCustomDASQL then
      TDBAccessUtils.SetTransaction(TCustomDASQL(Component), nil)
    else
    if Component is TDAScript then
      TDAScriptUtils.SetTransaction(TDAScript(Component), nil)
    else
    if Component is TDAMetaData then
      TDBAccessUtils.SetTransaction(TDAMetaData(Component), nil)
    else
    if Component is TDALoader then
      TDALoaderUtils.SetTransaction(TDALoader(Component), nil)
    else
    if Component is TDAAlerter then
      TDAAlerterUtils.SetTransaction(TDAAlerter(Component), nil)
    else
    if Component is TCustomDAConnection then
      TDBAccessUtils.SetDefaultTransaction(TCustomDAConnection(Component), nil)
    else
      Assert(False);
    Modified;
  end
  else
    inherited SetValue(Value);
end;

{ TUniConnectDialogPropertyEditor }

procedure TUniConnectDialogPropertyEditor.CheckComponent(const Value: string);
var
  Component: TComponent;
begin
  Component := {$IFDEF FPC}PropertyHook{$ELSE}Designer{$ENDIF}.GetComponent(Value);
  if Component <> nil then begin
    if not ({$IFDEF VER17P}(GetIsClassByName(Component, 'TUniConnectDialogFmx')) or {$ENDIF}(Component is TUniConnectDialog)) then
      Exit;
  end;
  FCheckProc(Value);
end;

procedure TUniConnectDialogPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  FCheckProc := Proc;
  inherited GetValues(CheckComponent);
end;

{ Component editors }

procedure TUniConnectionEditor.InitVerbs;
begin
  inherited;

  AddVerb('Connection Editor...', TUniConnectionEditorForm, TUniDesignUtils);
end;

procedure TUniQueryEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('UniQuery E&ditor...', TUniQueryEditorForm, TUniDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);
  inherited;
end;

procedure TUniTableEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('UniTable E&ditor...', TUniTableEditorForm, TUniDesignUtils);
  AddVerb('Data Editor...', ShowDataEditor);

  inherited;
end;

procedure TUniStoredProcEditor.InitVerbs;
begin
  AddVerb('Fields &Editor...', ShowFieldsEditor);
  AddVerb('StoredProc E&ditor...', TUniStoredProcEditorForm, TUniDesignUtils);

  inherited;
end;

procedure TUniSQLEditor.InitVerbs;
begin
  inherited;

  AddVerb('UniSQL E&ditor...', TUniSQLEditorForm, TUniDesignUtils);
end;

procedure TUniScriptEditor.InitVerbs;
begin
  inherited;

  AddVerb('UniScript E&ditor...', TUniScriptEditorForm, TUniDesignUtils);
end;

procedure TUniUpdateSQLEditor.InitVerbs;
begin
  inherited;
  AddVerb('UniUpdateSQL E&ditor...', TUniUpdateSQLEditorForm, TUniDesignUtils);
end;

{ TUniConnectionProviderNameEditor }

function TUniConnectionProviderNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure TUniConnectionProviderNameEditor.GetValues(Proc: TGetStrProc);
var
  List: TStrings;
  i: integer;
begin
  List := TStringList.Create;
  try
    UniProviders.GetProviderNames(List);
    for i := 0 to List.Count - 1 do
      Proc(List[i]);
  finally
    List.Free;
  end;
end;

{ TSpecificOptionsEditor }

function TSpecificOptionsEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TSpecificOptionsEditor.GetValue: string;
begin
  Result := '(TStrings)';
end;

procedure TSpecificOptionsEditor.Edit;
var
  Component: TComponent;
begin
  Component := GetComponent(0) as TComponent;

  TDAComponentEditor.ShowEditorEx(TUniSpecificOptionsEditorForm, TUniDesignUtils, Component, {$IFNDEF FPC}Designer{$ELSE}FindRootDesigner(Component){$ENDIF}, GetName)
end;

{$IFNDEF FPC}
initialization
  Notificator := TUniDesignNotification.Create;
  RegisterDesignNotification(Notificator);

finalization
  UnRegisterDesignNotification(Notificator);
  Notificator := nil;
{$ENDIF}

end.
