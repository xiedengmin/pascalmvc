//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR Controls
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit CRCtrls;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}
  SysUtils, Classes, Controls, Graphics,
{$IFNDEF VER6P}
  StdCtrls,
{$ENDIF}
  Grids, Forms;

type

{$IFNDEF VER6P}
  TEditStyle =  (esSimple, esEllipsis, esPickList);
  TOnGetPickListItems = procedure(ACol, ARow: Integer; Items: TStrings) of Object;
{$ENDIF}
{$IFDEF FPC}
  TOnGetPickListItems = procedure(ACol, ARow: Integer; Items: TStrings) of Object;
{$ENDIF}

  TOnPaint = procedure(DC: HDC) of Object;
  TOnCellCheck = function (ACol, ARow: Integer): boolean of object;

  TCRStringGrid = class(TStringGrid)
  private
    FOnPaint: TOnPaint;
    FOnEditCell: TOnCellCheck;
    FOnComboCell: TOnCellCheck;
    FOnNumericCell: TOnCellCheck;
    FOnGetPickListItems: TOnGetPickListItems;
  protected
    function IsComboBox(ACol, ARow: Integer): boolean;
    function IsNumeric(ACol, ARow: Integer): boolean;
    function CanEditShow: Boolean; override;
  {$IFNDEF FPC}
    function CreateEditor: TInplaceEdit; override;
    function GetEditStyle(ACol, ARow: Integer): TEditStyle; {$IFDEF VER6P}override;{$ENDIF}
  {$ELSE}
    function GetDefaultEditor(Column: Integer): TWinControl; override;
  {$ENDIF}
    procedure PaintWindow(DC: HDC); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;

  {$IFNDEF FPC}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  {$ENDIF}

    property Col;
    property Row;
  public
    procedure PaintRectTo(Rect: TRect; DC: HDC; X, Y: Integer); overload;

    property OnPaint: TOnPaint read FOnPaint write FOnPaint;
    property OnEditCell: TOnCellCheck read FOnEditCell write FOnEditCell;
    property OnComboCell: TOnCellCheck read FOnComboCell write FOnComboCell;
    property OnNumericCell: TOnCellCheck read FOnNumericCell write FOnNumericCell;
    property OnGetPickListItems: TOnGetPickListItems read FOnGetPickListItems write FOnGetPickListItems;

  end;

{$IFNDEF VER6P}

{ TPopupListbox }

  TPopupListbox = class(TCustomListbox)
  private
    FSearchText: String;
    FSearchTickCount: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  TInplaceEditList = class(TInPlaceEdit)
  private
    FButtonWidth: Integer;
    FPickList: TCustomListbox;
    FActiveList: TWinControl;
    FEditStyle: TEditStyle;
    FDropDownRows: Integer;
    FListVisible: Boolean;
    FTracking: Boolean;
    FPressed: Boolean;
    FPickListLoaded: Boolean;
    FOnGetPickListitems: TOnGetPickListItems;
    FOnEditButtonClick: TNotifyEvent;
    function GetPickList: TCustomListbox;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;
    procedure WMCancelMode(var Message: TMessage); message WM_CancelMode;
    procedure WMKillFocus(var Message: TMessage); message WM_KillFocus;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message wm_LButtonDblClk;
    procedure WMPaint(var Message: TWMPaint); message wm_Paint;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
  protected
    procedure BoundsChanged; override;
    function ButtonRect: TRect;
    procedure CloseUp(Accept: Boolean); dynamic;
    procedure DblClick; override;
    procedure DoDropDownKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure DoEditButtonClick; virtual;
    procedure DoGetPickListItems; dynamic;
    procedure DropDown; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function OverButton(const P: TPoint): Boolean;
    procedure PaintWindow(DC: HDC); override;
    procedure StopTracking;
    procedure TrackButton(X,Y: Integer);
    procedure UpdateContents; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(Owner: TComponent); override;
    procedure RestoreContents;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property ButtonWidth: Integer read FButtonWidth write FButtonWidth;
    property DropDownRows: Integer read FDropDownRows write FDropDownRows;
    property EditStyle: TEditStyle read FEditStyle;
    property ListVisible: Boolean read FListVisible write FListVisible;
    property PickList: TCustomListbox read GetPickList;
    property PickListLoaded: Boolean read FPickListLoaded write FPickListLoaded;
    property Pressed: Boolean read FPressed;
    property OnEditButtonClick: TNotifyEvent read FOnEditButtonClick
      write FOnEditButtonClick;
    property OnGetPickListitems: TOnGetPickListItems read FOnGetPickListitems
      write FOnGetPickListitems;
  end;
{$ENDIF}

{$IFNDEF FPC}  
  TCRInplaceEditList = class (TInplaceEditList)
  {$IFNDEF VER9P}
  private
    FReadOnly: boolean;
  {$ENDIF}
  protected
    procedure WndProc(var Message: TMessage); override;
  {$IFNDEF VER9P}
    property ReadOnly: boolean read FReadOnly write FReadOnly;
  {$ENDIF}
  end;
{$ENDIF}

implementation

{ TCRStringGrid }

function TCRStringGrid.IsComboBox(ACol, ARow: Integer): boolean;
begin
  Result := Assigned(FOnComboCell) and FOnComboCell(ACol, ARow);
end;

function TCRStringGrid.IsNumeric(ACol, ARow: Integer): boolean;
begin
  Result := Assigned(FOnNumericCell) and FOnNumericCell(ACol, ARow);
end;

function TCRStringGrid.CanEditShow: Boolean;
begin
  Result := (inherited CanEditShow);
  if Assigned(FOnEditCell) then
    Result := Result and FOnEditCell(Col, Row);
end;

{$IFNDEF FPC}

function TCRStringGrid.CreateEditor: TInplaceEdit;
var
  EditList: TInplaceEditList;
begin
  EditList := TCRInplaceEditList.Create(Self);
  EditList.DropDownRows := 10;
  EditList.OnGetPickListitems := FOnGetPickListItems;
  Result := EditList;
end;

function TCRStringGrid.GetEditStyle(ACol, ARow: Integer): TEditStyle;
begin
  if IsComboBox(ACol, ARow) then begin
    Result := esPickList;
    if InplaceEditor <> nil then
      TCRInplaceEditList(InplaceEditor).ReadOnly := True;
  end
  else begin
    Result := esSimple;
    if InplaceEditor <> nil then
      TCRInplaceEditList(InplaceEditor).ReadOnly := False;
  end;
end;

{$ELSE}

function TCRStringGrid.GetDefaultEditor(Column: Integer): TWinControl;
begin
  if IsComboBox(Column, Row) then begin
    Result := EditorByStyle(cbsPicklist);
    if Assigned(FOnGetPickListItems) then
      FOnGetPickListItems(Column, Row, TPickListCellEditor(Result).Items);
  end
  else
    Result := inherited GetDefaultEditor(Column);
end;

{$ENDIF}

procedure TCRStringGrid.PaintWindow(DC: HDC);
begin
  inherited;

  if Assigned(FOnPaint) then
    FOnPaint(DC);
end;

function TCRStringGrid.GetEditText(ACol, ARow: Integer): string;
begin
  Result := inherited GetEditText(ACol, ARow);
end;

procedure TCRStringGrid.SetEditText(ACol, ARow: Longint; const Value: string);
var
  i: integer;
begin
  // Avoid to set Modified flag if Value hasn't been changed
  if Cells[ACol, ARow] <> Value then
    if IsComboBox(ACol, ARow) or (Value = '') then
      inherited
    else if IsNumeric(ACol, ARow) then begin
      i := StrToInt(Value);
      inherited SetEditText(ACol, ARow, IntToStr(i));
    end
    else
      inherited SetEditText(ACol, ARow, Value);
end;

{$IFNDEF FPC}

procedure TCRStringGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Integer;
begin
  if (Button = mbLeft) and (ssDouble in Shift) then begin
    MouseToCell(X, Y, ACol, ARow);
    if GetEditStyle(ACol, ARow) = esPickList then
      Shift := Shift - [ssDouble];
  end;

  inherited  MouseDown(Button, Shift, X, Y);
end;

{$ENDIF}

procedure TCRStringGrid.PaintRectTo(Rect: TRect; DC: HDC; X, Y: Integer);
var
{$IFNDEF FPC}
  SaveIndex: Integer;
{$ELSE}
  OldHandle: HDC;
{$ENDIF}
begin
  ControlState := ControlState + [csPaintCopy];
  try
  {$IFDEF FPC}
    OldHandle := Canvas.Handle;
    Canvas.Handle := DC;
    try
      Rect.Left := Rect.Left + X;
      Rect.Top := Rect.Top + Y;
      Rect.Right := Rect.Right + X;
      Rect.Bottom := Rect.Bottom + Y;
      DrawCell(0, 0, Rect, [gdFixed]);
    finally
      Canvas.Handle := OldHandle;
    end;
  {$ELSE}
    SaveIndex := SaveDC(DC);
    try
      MoveWindowOrg(DC, X - Rect.Left, Y - Rect.Top);
      IntersectClipRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
      Perform(WM_ERASEBKGND, DC, 0);
      Perform(WM_PAINT, DC, 0);
    finally
      RestoreDC(DC, SaveIndex);
    end;
  {$ENDIF}
  finally
    ControlState := ControlState - [csPaintCopy];
  end;
end;

{$IFNDEF VER6P}

{ Functions }

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

{ TInplaceEditList }

constructor TInplaceEditList.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  FEditStyle := esSimple;
end;

procedure TInplaceEditList.BoundsChanged;
var
  R: TRect;
begin
  SetRect(R, 2, 2, Width - 2, Height);
  if EditStyle <> esSimple then
    if not Grid.UseRightToLeftAlignment then
      Dec(R.Right, ButtonWidth)
    else
      Inc(R.Left, ButtonWidth - 2);
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
  SendMessage(Handle, EM_SCROLLCARET, 0, 0);
  if SysLocale.FarEast then
    SetImeCompositionWindow(Font, R.Left, R.Top);
end;

procedure TInplaceEditList.CloseUp(Accept: Boolean);
var
  ListValue: Variant;
begin
  if ListVisible and (ActiveList = FPickList) then
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    if PickList.ItemIndex <> -1 then
      ListValue := PickList.Items[PickList.ItemIndex];
    SetWindowPos(ActiveList.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or
      SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW);
    FListVisible := False;
    Invalidate;
    if Accept then
      if (not VarIsEmpty(ListValue) or VarIsNull(ListValue))
         and (ListValue <> Text) then
      begin
        { Here we store the new value directly in the edit control so that
          we bypass the CMTextChanged method on TCustomMaskedEdit.  This
          preserves the old value so that we can restore it later by calling
          the Reset method. }
        Perform(WM_SETTEXT, 0, Longint(string(ListValue)));
        Modified := True;
        with TCRStringGrid(Grid) do
          SetEditText(Col, Row, ListValue);
      end;
  end;
end;

procedure TInplaceEditList.DoDropDownKeys(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP, VK_DOWN:
      if ssAlt in Shift then
      begin
        if ListVisible then CloseUp(True) else DropDown;
        Key := 0;
      end;
    VK_RETURN, VK_ESCAPE:
      if ListVisible and not (ssAlt in Shift) then
      begin
        CloseUp(Key = VK_RETURN);
        Key := 0;
      end;
  end;
end;

procedure TInplaceEditList.DoEditButtonClick;
begin
  if Assigned(FOnEditButtonClick) then
    FOnEditButtonClick(Grid);
end;

procedure TInplaceEditList.DoGetPickListItems;
begin
  if not PickListLoaded then
  begin
    if Assigned(OnGetPickListItems) then
      OnGetPickListItems(TCRStringGrid(Grid).Col, TCRStringGrid(Grid).Row, PickList.Items);
    PickListLoaded := (PickList.Items.Count > 0);
  end;
end;

function TInplaceEditList.GetPickList: TCustomListbox;
var
  PopupListbox: TPopupListbox;
begin
  if not Assigned(FPickList) then
  begin
    PopupListbox := TPopupListbox.Create(Self);
    PopupListbox.Visible := False;
    PopupListbox.Parent := Self;
    PopupListbox.OnMouseUp := ListMouseUp;
    PopupListbox.IntegralHeight := True;
    PopupListbox.ItemHeight := 11;
    FPickList := PopupListBox;
  end;
  Result := FPickList;
end;

procedure TInplaceEditList.DropDown;
var
  P: TPoint;
  I,J,Y: Integer;
begin
  if not ListVisible then
  begin
    ActiveList.Width := Width;
    if ActiveList = FPickList then
    begin
      DoGetPickListItems;
      TPopupListbox(PickList).Color := Color;
      TPopupListbox(PickList).Font := Font;
      if (DropDownRows > 0) and (PickList.Items.Count >= DropDownRows) then
        PickList.Height := DropDownRows * TPopupListbox(PickList).ItemHeight + 4
      else
        PickList.Height := PickList.Items.Count * TPopupListbox(PickList).ItemHeight + 4;
      if Text = '' then
        PickList.ItemIndex := -1
      else
        PickList.ItemIndex := PickList.Items.IndexOf(Text);
      J := PickList.ClientWidth;
      for I := 0 to PickList.Items.Count - 1 do
      begin
        Y := PickList.Canvas.TextWidth(PickList.Items[I]);
        if Y > J then J := Y;
      end;
      PickList.ClientWidth := J;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;
    if Y + ActiveList.Height > Screen.Height then Y := P.Y - ActiveList.Height;
    SetWindowPos(ActiveList.Handle, HWND_TOP, P.X, Y, 0, 0,
      SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    FListVisible := True;
    Invalidate;
    Windows.SetFocus(Handle);
  end;
end;

procedure TInplaceEditList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (EditStyle = esEllipsis) and (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    DoEditButtonClick;
    KillMessage(Handle, WM_CHAR);
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TInplaceEditList.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CloseUp(PtInRect(ActiveList.ClientRect, Point(X, Y)));
end;

procedure TInplaceEditList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> esSimple) and
    OverButton(Point(X,Y)) then
  begin
    if ListVisible then
      CloseUp(False)
    else
    begin
      MouseCapture := True;
      FTracking := True;
      TrackButton(X, Y);
      if Assigned(ActiveList) then
        DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TInplaceEditList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ListPos: TPoint;
  MousePos: TSmallPoint;
begin
  if FTracking then
  begin
    TrackButton(X, Y);
    if ListVisible then
    begin
      ListPos := ActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(ActiveList.ClientRect, ListPos) then
      begin
        StopTracking;
        MousePos := PointToSmallPoint(ListPos);
        SendMessage(ActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(MousePos));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TInplaceEditList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  WasPressed: Boolean;
begin
  WasPressed := Pressed;
  StopTracking;
  if (Button = mbLeft) and (EditStyle = esEllipsis) and WasPressed then
    DoEditButtonClick;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TInplaceEditList.PaintWindow(DC: HDC);
var
  R: TRect;
  Flags: Integer;
  W, X, Y: Integer;
begin
  if EditStyle <> esSimple then
  begin
    R := ButtonRect;
    Flags := 0;
    case EditStyle of
      esPickList:
        begin
          begin
            if ActiveList = nil then
              Flags := DFCS_INACTIVE
            else if Pressed then
              Flags := DFCS_FLAT or DFCS_PUSHED;
            DrawFrameControl(DC, R, DFC_SCROLL, Flags or DFCS_SCROLLCOMBOBOX);
          end;
        end;
      esEllipsis:
        begin
          begin
            if Pressed then Flags := BF_FLAT;
            DrawEdge(DC, R, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
          end;

          X := R.Left + ((R.Right - R.Left) shr 1) - 1 + Ord(Pressed);
          Y := R.Top + ((R.Bottom - R.Top) shr 1) - 1 + Ord(Pressed);
          W := ButtonWidth shr 3;
          if W = 0 then W := 1;
          PatBlt(DC, X, Y, W, W, BLACKNESS);
          PatBlt(DC, X - (W * 2), Y, W, W, BLACKNESS);
          PatBlt(DC, X + (W * 2), Y, W, W, BLACKNESS);
        end;
    end;
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited PaintWindow(DC);
end;

procedure TInplaceEditList.StopTracking;
begin
  if FTracking then
  begin
    TrackButton(-1, -1);
    FTracking := False;
    MouseCapture := False;
  end;
end;

procedure TInplaceEditList.TrackButton(X,Y: Integer);
var
  NewState: Boolean;
  R: TRect;
begin
  R := ButtonRect;
  NewState := PtInRect(R, Point(X, Y));
  if Pressed <> NewState then
  begin
    FPressed := NewState;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TInplaceEditList.UpdateContents;
begin
  ActiveList := nil;
  PickListLoaded := False;
  FEditStyle := TCRStringGrid(Grid).GetEditStyle(TCRStringGrid(Grid).Col, TCRStringGrid(Grid).Row);
  if EditStyle = esPickList then
    ActiveList := PickList;
  inherited UpdateContents;
end;

procedure TInplaceEditList.RestoreContents;
begin
  Reset;
  TCRStringGrid(Grid).Perform(WM_COMMAND, EN_CHANGE, 0);
end;

procedure TInplaceEditList.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> ActiveList) then
    CloseUp(False);
end;

procedure TInplaceEditList.WMCancelMode(var Message: TMessage);
begin
  StopTracking;
  inherited;
end;

procedure TInplaceEditList.WMKillFocus(var Message: TMessage);
begin
  if not SysLocale.FarEast then inherited
  else
  begin
    ImeName := Screen.DefaultIme;
    ImeMode := imDontCare;
    inherited;
    if HWND(Message.WParam) <> Grid.Handle then
      ActivateKeyboardLayout(Screen.DefaultKbLayout, KLF_ACTIVATE);
  end;
  CloseUp(False);
end;

function TInplaceEditList.ButtonRect: TRect;
begin
  if not Grid.UseRightToLeftAlignment then
    Result := Rect(Width - ButtonWidth, 0, Width, Height)
  else
    Result := Rect(0, 0, ButtonWidth, Height);
end;

function TInplaceEditList.OverButton(const P: TPoint): Boolean;
begin
  Result := PtInRect(ButtonRect, P);
end;

procedure TInplaceEditList.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  with Message do
  if (EditStyle <> esSimple) and OverButton(Point(XPos, YPos)) then
    Exit;
  inherited;
end;

procedure TInplaceEditList.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TInplaceEditList.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if (EditStyle <> esSimple) and OverButton(P) then
    Windows.SetCursor(LoadCursor(0, idc_Arrow))
  else
    inherited;
end;

procedure TInplaceEditList.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    wm_KeyDown, wm_SysKeyDown, wm_Char:
      if EditStyle = esPickList then
      with TWMKey(Message) do
      begin
        DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
        if (CharCode <> 0) and ListVisible then
        begin
          with TMessage(Message) do
            SendMessage(ActiveList.Handle, Msg, WParam, LParam);
          Exit;
        end;
      end
  end;
  inherited;
end;

procedure TInplaceEditList.DblClick;
var
  Index: Integer;
  ListValue: string;
begin
  if (EditStyle = esSimple) or Assigned(TStringGrid(Grid).OnDblClick) then
    inherited
  else if (EditStyle = esPickList) and (ActiveList = PickList) then
  begin
    DoGetPickListItems;
    if PickList.Items.Count > 0 then
    begin
      Index := PickList.ItemIndex + 1;
      if Index >= PickList.Items.Count then
        Index := 0;
      PickList.ItemIndex := Index;
      ListValue := PickList.Items[PickList.ItemIndex];
      Perform(WM_SETTEXT, 0, Longint(ListValue));
      Modified := True;
      with TCRStringGrid(Grid) do
        SetEditText(Col, Row, ListValue);
      SelectAll;
    end;
  end
  else if EditStyle = esEllipsis then
    DoEditButtonClick;
end;

{ TPopupListbox }

procedure TPopupListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    AddBiDiModeExStyle(ExStyle);
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TPopupListbox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end;

procedure TPopupListbox.Keypress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27: FSearchText := '';
    #32..#255:
      begin
        TickCount := GetTickCount;
        if TickCount - FSearchTickCount > 2000 then FSearchText := '';
        FSearchTickCount := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SelectString, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TPopupListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  TInplaceEditList(Owner).CloseUp((X >= 0) and (Y >= 0) and
      (X < Width) and (Y < Height));
end;
{$ENDIF}

{$IFNDEF FPC}
{ TCRInplaceEditList }

procedure TCRInplaceEditList.WndProc(var Message: TMessage);
begin
  if Message.Msg = wm_Char then
    if ReadOnly then
    {$IFDEF VER9P}
      Message.WParamLo := 0;
    {$ELSE}
      TWMKey(Message).CharCode := 0;
    {$ENDIF}

  inherited;
end;
{$ENDIF}

end.
