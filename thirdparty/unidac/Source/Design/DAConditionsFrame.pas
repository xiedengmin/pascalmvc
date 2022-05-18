
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Conditions Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DAConditionsFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  DBAccess, CRFrame, CRColFrame, CRTabEditor;

type
  TDAConditionsFrame = class(TCRFrame)
    lbCName: TLabel;
    lbItemName: TListBox;
    PanelItem: TPanel;
    lbCValue: TLabel;
    lbConditionEnable: TLabel;
    meConditionValue: TMemo;
    cbConditionEnable: TCheckBox;
    btnAdd: TButton;
    btnRemove: TButton;
    procedure cbConditionEnableClick(Sender: TObject);
    procedure meConditionValueExit(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lbItemNameClick(Sender: TObject);
  protected
    FOldItemIndex: integer;
    FInStoreItem, FInSelectItem: boolean;

    function GetItems: TDAConditions;
    function GetConditions: TDAConditions;

    function GetItemName(Item: TDACondition): string;
    procedure ItemToControls(Item: TDACondition);
    procedure ControlsToItem(Item: TDACondition);
    function IsControlEnabled(Control: TControl): boolean;
    procedure UpdateControlState(Control: TControl);
    procedure UpdateControlsState;
    procedure InitItems;
    procedure StoreItem;
    procedure DoActivate; override;
    procedure DoFinish; override;

    property Conditions: TDAConditions read GetConditions;
    property Items: TDAConditions read GetItems;
  public
    procedure SelectItem;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R DAConditionsFrame.dfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DB, DASQLComponentEditor;

function TDAConditionsFrame.GetItems: TDAConditions;
begin
  Result := Editor.DADesignUtilsClass.GetConditions(Editor.localComponent);
end;

function TDAConditionsFrame.GetConditions: TDAConditions;
begin
  Result := Items as TDAConditions;
end;

function TDAConditionsFrame.GetItemName(Item: TDACondition): string;
begin
  Result := TDACondition(Item).Name;
end;

procedure TDAConditionsFrame.ItemToControls(Item: TDACondition);
begin
  meConditionValue.Lines.Text := TDACondition(Item).Value;
  cbConditionEnable.Checked := TDACondition(Item).Enabled;
end;

procedure TDAConditionsFrame.lbItemNameClick(Sender: TObject);
begin
  if lbItemName.ItemIndex <> FOldItemIndex then begin
    StoreItem;
    SelectItem;
  end;
end;

procedure TDAConditionsFrame.ControlsToItem(Item: TDACondition);
begin
  TDACondition(Item).Value := meConditionValue.Lines.Text;
  TDACondition(Item).Enabled := cbConditionEnable.Checked;
end;

function TDAConditionsFrame.IsControlEnabled(Control: TControl): boolean;
begin
  Result := Enabled and Control.Enabled and Control.Parent.Enabled and Control.Visible;
end;

procedure TDAConditionsFrame.UpdateControlState(Control: TControl);
var
  e: boolean;
  i: integer;
begin
  e := IsControlEnabled(Control);

  if Control is TPanel then begin
    for i := 0 to TPanel(Control).ControlCount - 1 do begin
      TPanel(Control).Controls[i].Enabled := e;
      UpdateControlState(TPanel(Control).Controls[i]);
    end;
  end
  else
  if Control is TComboBox then begin
    if e then
      TComboBox(Control).Color := clWindow
    else
    begin
      TComboBox(Control).Color := clBtnFace;
      TComboBox(Control).Text := '';
      TComboBox(Control).ItemIndex := -1;
    end;
  end
  else
  if Control is TEdit then begin
    if e then begin
      if TEdit(Control).ReadOnly then
        TEdit(Control).Color := clBtnFace
      else
        TEdit(Control).Color := clWindow;
      TEdit(Control).ParentFont := True;
    end
    else
    begin
      TEdit(Control).Color := clBtnFace;
      TEdit(Control).Font.Color := clBtnFace;
      TEdit(Control).Text := '';
    end;
  end
  else
  if Control is TMemo then begin
    if e then begin
      TMemo(Control).Color := clWindow;
      TMemo(Control).ParentFont := True;
    end
    else
    begin
      TMemo(Control).Color := clBtnFace;
      TMemo(Control).Font.Color := clBtnFace;
      TMemo(Control).Text := '';
    end;
  end
  else
  if not (Control is TLabel) and not (Control is TButton) and
    not (Control is TBevel) and not (Control is TCheckBox) then
    Assert(False, Control.Name + ' is ' + Control.ClassName);
end;

procedure TDAConditionsFrame.UpdateControlsState;
var
  i: integer;
begin
  for i := 0 to PanelItem.ControlCount - 1 do
    UpdateControlState(PanelItem.Controls[i]);
end;

procedure TDAConditionsFrame.InitItems;
var
  i: integer;
  OldIndex, MaxTextWidth: integer;
begin
  OldIndex := lbItemName.ItemIndex;
  lbItemName.Items.Clear;
//  Enabled := Items.Count > 0;
  if not Enabled then begin
    UpdateControlsState;
    if (Editor.ActiveControl = nil) and (TCRTabEditorForm(Editor).PageControl.ActivePage = Page) then
      Editor.ActiveControl := Page;
    Exit;
  end;
  FOldItemIndex := -1;

  MaxTextWidth := 0;
  lbItemName.Canvas.Font := lbItemName.Font;
  for i := 0 to Items.Count - 1 do begin
    lbItemName.Items.Add(GetItemName(Items.Condition[i]));
    if MaxTextWidth < lbItemName.Canvas.TextWidth(lbItemName.Items[i]) then
      MaxTextWidth := lbItemName.Canvas.TextWidth(lbItemName.Items[i]);
  end;
  {$IFDEF VER5}
  lbItemName.Perform(LB_SETHORIZONTALEXTENT, MaxTextWidth + 3, 0);
  {$ELSE}
  lbItemName.ScrollWidth := MaxTextWidth + 3;
  {$ENDIF}

  if (OldIndex >= 0) and (Items.Count > 0) then begin
    if OldIndex < Items.Count then
      lbItemName.ItemIndex := OldIndex
    else
      lbItemName.ItemIndex := Items.Count - 1;
  end;

  SelectItem;
end;

procedure TDAConditionsFrame.StoreItem;
var
  Item: TDACondition;
begin
  if (FOldItemIndex <> - 1) and (Items.Count > FOldItemIndex) then begin
    Item := Items.Condition[FOldItemIndex];
    FInStoreItem := True;
    try
      ControlsToItem(Item);
    finally
      FInStoreItem := False;
    end;
  end;
end;

procedure TDAConditionsFrame.DoActivate;
begin
  inherited;

  if not FInStoreItem then
    InitItems;
end;

procedure TDAConditionsFrame.DoFinish;
begin
  inherited;

  StoreItem;
end;


procedure TDAConditionsFrame.SelectItem;
var
  Item: TDACondition;
  OldModified: boolean;
  i: integer;

begin
  OldModified := Modified;
  FInSelectItem := True;
  try
    PanelItem.Enabled := lbItemName.Items.Count > 0;

    if PanelItem.Enabled then begin
      for i := 0 to PanelItem.ControlCount - 1 do
        PanelItem.Controls[i].Enabled := True;

      if lbItemName.ItemIndex <> - 1 then begin
        Item := Items.Condition[lbItemName.ItemIndex];
        ItemToControls(Item);
      end;

      if lbItemName.ItemIndex = -1 then begin
        lbItemName.ItemIndex := 0;
        ItemToControls(Items.Condition[0]);
      end;

      FOldItemIndex := lbItemName.ItemIndex;
    end;

  finally
    UpdateControlsState;
    FInSelectItem := False;
    Modified := OldModified;
  end;
end;

procedure TDAConditionsFrame.btnAddClick(Sender: TObject);
var
  index: Integer;
  name: string;

  function frmConditionName: string;
  var
    Form: TForm;
    edName: TEdit;
    btnOk, btnCancel: TButton;
  begin
    Form := TForm.Create(Self.Owner);
    try
      Form.Position := poOwnerFormCenter;
      Form.BorderStyle := bsDialog;
      Form.Width := 220;
      Form.Height := 80;
      Form.Caption := 'Condition Name';
      edName := TEdit.Create(Form);
      edName.Parent := Form;
      edName.Left := 2;
      edName.Top := 2;
      edName.Width := 240;
      edName.Height := 21;
      btnOk := TButton.Create(Form);
      btnOk.Parent := Form;
      btnOk.ModalResult := mrOk;
      btnOk.Top := 27;
      btnOk.Left := 25;
      btnOk.Caption := 'OK';
      btnCancel := TButton.Create(Form);
      btnCancel.Parent := Form;
      btnCancel.ModalResult := mrCancel;
      btnCancel.Top := 27;
      btnCancel.Left := 105;
      btnCancel.Caption := 'Cancel';
      if (Form.ShowModal <> mrOk) or (trim(edName.Text) = '')  then
        Result := ''
      else
        Result := trim(edName.Text);
    finally
      Form.Free;
    end;

  end;
begin
  name := frmConditionName;
  if (name = '') or (Conditions.Find(name) <> nil) then exit;

  index := lbItemName.Items.Add(name);
  Conditions.Add(name, '');
  lbItemName.Selected[index] := True;

  SelectItem;
end;

procedure TDAConditionsFrame.btnRemoveClick(Sender: TObject);
begin
  if lbItemName.ItemIndex = -1  then exit;

  Conditions.Remove(lbItemName.Items[lbItemName.ItemIndex]);
  lbItemName.Items.Delete(lbItemName.ItemIndex);

  SelectItem;
end;

procedure TDAConditionsFrame.cbConditionEnableClick(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;

  Conditions[lbItemName.ItemIndex].Enabled := cbConditionEnable.Checked;
  Modified := True;

  UpdateControlsState;
end;

procedure TDAConditionsFrame.meConditionValueExit(Sender: TObject);
begin
  if FInSelectItem or not IsControlEnabled(Sender as TControl) then
    Exit;
    
  if meConditionValue.Lines.Text <> Conditions[lbItemName.ItemIndex].Value then begin
    Conditions[lbItemName.ItemIndex].Value := meConditionValue.Lines.Text;

    Modified := True;

    UpdateControlsState;
  end;
end;

end.
