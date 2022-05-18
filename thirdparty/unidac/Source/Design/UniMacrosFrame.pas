//////////////////////////////////////////////////
//  Universal Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Macros Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit UniMacrosFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, StdCtrls, Buttons,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  Forms, Dialogs, Grids, DB, CRFrame, Uni;

type
  TUniMacrosFrame = class(TCRFrame)
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    sgMacros: TDrawGrid;
    btClearAllMacros: TButton;
    btDeleteMacro: TButton;
    btReplaceMacro: TButton;
    btAddMacro: TButton;
    Label8: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    edMacroValue: TEdit;
    edMacroName: TEdit;
    edMacroCondition: TComboBox;

    procedure btAddMacroClick(Sender: TObject);
    procedure btDeleteMacroClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);
    procedure sgMacrosDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure btReplaceMacroClick(Sender: TObject);
    procedure btClearAllMacrosClick(Sender: TObject);
    procedure sgMacrosSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure edMacroConditionDropDown(Sender: TObject);
  private
    function GetConnection: TUniConnection;
    function GetLocalConnection: TUniConnection;
  protected
    procedure Resize; override;

    procedure DoActivate; override;
    procedure DoFinish; override;
//    procedure ConnToControls; override;
    procedure EnableMacroButtons;
  public
    property Connection: TUniConnection read GetConnection;
    property LocalConnection: TUniConnection read GetLocalConnection;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R UniMacrosFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  UniProvider;

{ TUniMacrosFrame }

function TUniMacrosFrame.GetConnection: TUniConnection;
begin
  Result := Editor.Component as TUniConnection;
end;

function TUniMacrosFrame.GetLocalConnection: TUniConnection;
begin
  Result := Editor.LocalComponent as TUniConnection;
end;

//procedure TUniMacrosFrame.ConnToControls;
//begin
//  inherited;
//
//  sgMacros.RowCount := LocalConnection.Macros.Count + 2;
//  EnableMacroButtons;
//end;

procedure TUniMacrosFrame.Resize;
var
  defWidth: Integer;
begin
  inherited;

  if sgMacros <> nil then begin
    UpBtn.Left := Self.ClientWidth - UpBtn.Width - 8;
    DownBtn.Left := UpBtn.Left;

    btClearAllMacros.Top := Self.ClientHeight - btClearAllMacros.Height - 8;
    btDeleteMacro.Top := btClearAllMacros.Top;
    btReplaceMacro.Top := btClearAllMacros.Top;
    btAddMacro.Top := btClearAllMacros.Top;

    edMacroValue.Top := btClearAllMacros.Top - edMacroValue.Height - 8;
    edMacroName.Top := edMacroValue.Top;
    edMacroCondition.Top := edMacroValue.Top;

    Label8.Top := edMacroValue.Top - Label8.Height - 8;
    Label7.Top := Label8.Top;
    Label4.Top := Label8.Top;

    sgMacros.Height := Label8.Top - sgMacros.Top - 8;
    sgMacros.Width := UpBtn.Left - sgMacros.Left - 8;

    defWidth := sgMacros.ClientWidth div 3;
    sgMacros.DefaultColWidth := defWidth;

    edMacroName.Width :=  defWidth - 8;
    edMacroValue.Width := defWidth - 8;
    edMacroValue.Left := edMacroName.Left + defWidth;
    edMacroCondition.Left := edMacroValue.Left + defWidth;
    edMacroCondition.Width := sgMacros.Left + sgMacros.Width - edMacroCondition.Left;

    Label4.Left := edMacroName.Left;
    Label7.Left := edMacroValue.Left;
    Label8.Left := edMacroCondition.Left;
  end;
end;

procedure TUniMacrosFrame.DoActivate;
begin
  sgMacros.RowCount := LocalConnection.Macros.Count + 2;
  EnableMacroButtons;
end;

procedure TUniMacrosFrame.DoFinish;
begin

end;

procedure TUniMacrosFrame.EnableMacroButtons;
var
  i: integer;
begin
  i := sgMacros.Row;

  btDeleteMacro.Enabled := LocalConnection.Macros.Count >= i;
  UpBtn.Enabled := (i > 1) and (LocalConnection.Macros.Count >= i);
  DownBtn.Enabled := i < LocalConnection.Macros.Count;
end;

procedure TUniMacrosFrame.btAddMacroClick(Sender: TObject);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
  Sel: TGridRect;
  i: integer;
begin
  Name := Trim(edMacroName.Text);
  Value := Trim(edMacroValue.Text);
  Cond := Trim(edMacroCondition.Text);
  if (Name <> '') or (Value <> '') or (Cond <> '') then begin
    Sel := sgMacros.Selection;
    i := Sel.Top;

    Macro := TUniMacro(LocalConnection.Macros.Insert(i - 1));
    Macro.Name := Name;
    Macro.Value := Value;
    Macro.Condition := Cond;

    sgMacros.RowCount := LocalConnection.Macros.Count + 2;
    Sel.Top := i + 1;
    Sel.Bottom := i + 1;
    sgMacros.Selection := Sel;
    sgMacros.Invalidate;
    EnableMacroButtons;

    edMacroName.Text := '';
    edMacroValue.Text := '';
    edMacroCondition.Text := '';
    edMacroName.SetFocus;

    FModified := True;
  end;
end;

procedure TUniMacrosFrame.btReplaceMacroClick(Sender: TObject);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
  i: integer;
begin
  i := sgMacros.Selection.Top;
  if i > LocalConnection.Macros.Count then begin
    btAddMacroClick(nil);
    exit;
  end;

  Name := Trim(edMacroName.Text);
  Value := Trim(edMacroValue.Text);
  Cond := Trim(edMacroCondition.Text);
  if (Name <> '') or (Value <> '') or (Cond <> '') then begin
    Macro := LocalConnection.Macros[i - 1];
    Macro.Name := Name;
    Macro.Value := Value;
    Macro.Condition := Cond;
    FModified := True;

    sgMacros.Invalidate;
  end;
end;

procedure TUniMacrosFrame.btDeleteMacroClick(Sender: TObject);
var
  i: integer;
begin
  i := sgMacros.Selection.Top;
  if LocalConnection.Macros.Count < i then
    exit;

  LocalConnection.Macros[i - 1].Free;
  FModified := True;

  sgMacros.RowCount := LocalConnection.Macros.Count + 2;
  sgMacros.Invalidate;
  EnableMacroButtons;

end;

procedure TUniMacrosFrame.btClearAllMacrosClick(Sender: TObject);
begin
  LocalConnection.Macros.Clear;
  FModified := True;

  sgMacros.RowCount := LocalConnection.Macros.Count + 2;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniMacrosFrame.UpBtnClick(Sender: TObject);
var
  i: integer;
  Sel: TGridRect;
begin
  Sel := sgMacros.Selection;
  i := Sel.Top;
  if (LocalConnection.Macros.Count < i) or (i <= 1) then
    exit;

  LocalConnection.Macros[i - 1].Index := i - 2;
  FModified := True;

  Sel.Top := i - 1;
  Sel.Bottom := i - 1;
  sgMacros.Selection := Sel;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniMacrosFrame.DownBtnClick(Sender: TObject);
var
  i: integer;
  Sel: TGridRect;
begin
  Sel := sgMacros.Selection;
  i := Sel.Top;
  if i >= LocalConnection.Macros.Count then
    exit;

  LocalConnection.Macros[i - 1].Index := i;
  FModified := True;

  Sel.Top := i + 1;
  Sel.Bottom := i + 1;
  sgMacros.Selection := Sel;
  sgMacros.Invalidate;
  EnableMacroButtons;
end;

procedure TUniMacrosFrame.sgMacrosDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  s: string;
begin
  if ARow = 0 then begin
    case ACol of
      0: s := 'Name';
      1: s := 'Value';
      2: s := 'Condition';
    end;
  end
  else
  if ARow <= LocalConnection.Macros.Count then begin
    case ACol of
      0: s := LocalConnection.Macros[ARow - 1].Name;
      1: s := LocalConnection.Macros[ARow - 1].Value;
      2: s := LocalConnection.Macros[ARow - 1].Condition;
    end;
  end;

  sgMacros.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, s);
end;

procedure TUniMacrosFrame.sgMacrosSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  Name, Value, Cond: string;
  Macro: TUniMacro;
begin
  if (ARow > 0) and (ARow <= LocalConnection.Macros.Count) then begin
    Macro := LocalConnection.Macros[ARow - 1];
    Name := Macro.Name;
    Value := Macro.Value;
    Cond := Macro.Condition;
  end
  else begin
    Name := '';
    Value := '';
    Cond := '';
  end;

  edMacroName.Text := Name;
  edMacroValue.Text := Value;
  edMacroCondition.Text := Cond;

  EnableMacroButtons;
end;

procedure TUniMacrosFrame.edMacroConditionDropDown(
  Sender: TObject);
var
  List: TStringList;
  i: integer;
begin
  List := TStringList.Create;
  try
    UniProviders.GetProviderNames(List);
    for i := 0 to List.Count - 1 do
      List[i] := StringReplace(List[i], ' ', '', [rfReplaceAll]);

    edMacroCondition.Items.Assign(List);

    List.Clear;
    List.Sorted := True;
    List.Duplicates := dupIgnore;
    List.CaseSensitive := False;
    for i := 0 to LocalConnection.Macros.Count - 1 do
      List.Add(LocalConnection.Macros[i].Name);

    for i := 0 to List.Count - 1 do
      edMacroCondition.Items.Add(List[i]);
  finally
    List.Free;
  end;
end;

end.
