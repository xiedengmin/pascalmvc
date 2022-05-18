//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Data Type Mapping Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I Dac.inc}
unit DADataTypeMapFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF FPC}
  LCLType,
{$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, StdCtrls, ComCtrls, Buttons, CRCtrls,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  Forms, Dialogs, Grids, DB,
  CRDataTypeMap, DBAccess, {$IFDEF FPC}MemDataSet,{$ELSE} MemDS,{$ENDIF}
  CRFrame;

const
  rowTitleCount = 2;

  colDBType = 0;
  colDBLengthMin = 1;
  colDBLengthMax = 2;
  colDBScaleMin = 3;
  colDBScaleMax = 4;
  colFieldName = 5;
  colFieldType = 6;
  colFieldLength = 7;
  colFieldScale = 8;
  colIgnoreError = 9;

  widthDBType = 100;
  widthDBLengthMin = 30;
  widthDBLengthMax = 30;
  widthDBScaleMin = 30;
  widthDBScaleMax = 30;
  widthFieldName = 100;
  widthFieldType = 100;
  widthFieldLength = 30;
  widthFieldScale = 30;
  widthIgnoreError = 40;

type
  TDADataTypeMapFrame = class;
  TDADataTypeMapFrameClass = class of TDADataTypeMapFrame;

  TDADataTypeMapFrame = class(TCRFrame)
    chkIgnoreError: TCheckBox;
    btnAddRule: TButton;
    btnRemoveRule: TButton;
    btnMoveUp: TButton;
    btnMoveDown: TButton;
    procedure grdDataTypeMapDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdDataTypeMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdDataTypeMapSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure btnAddRuleClick(Sender: TObject);
    procedure btnRemoveRuleClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
  private
    FFieldNameListAllowed: boolean;
    FFakeGrid: TCRStringGrid;
    function GetFieldTypeInfos: TFieldTypeInfos;
    function GetRules: TDAMapRules;
    function GetIgnoreError(ARow: Integer): Boolean;
    procedure SetIgnoreError(ARow: Integer; Value: Boolean);
    procedure PaintGridTitle(DC: HDC);
    function GetColumnLeft(ColumnIndex: Integer): integer;
    function GetColumnWidth(ColumnIndex: Integer): integer;
    function CalculateColumnWidth(ColumnIndex: Integer): integer;
    procedure IsSelectableCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: boolean);
    function IsEditableCell(ACol, ARow: integer): boolean;
    function IsComboBox(ACol, ARow: integer): boolean;
    function IsNumeric(ACol, ARow: integer): boolean;
    procedure GetPickListItems(ACol, ARow: Integer; Items: TStrings);
  protected
    FDataTypeMapGrid: TCRStringGrid;

    function GetConverterManagerClass: TConverterManagerClass; virtual;
    procedure Resize; override;

    procedure DoActivate; override;
    procedure DoFinish; override;

    procedure InitCheckBox;
    procedure InitGrid;
    procedure FillFieldList(Items: TStrings);
    procedure FillDBTypeList(Items: TStrings);
    procedure FillFieldTypeList(Items: TStrings);
    procedure ResizeGrid; virtual;
    function GetCheckBoxRect(ACol, ARow: Integer): TRect;

    function IsEmptyRule(Row: Integer): boolean;
    procedure ClearRule(Row: Integer);
    procedure LoadRule(Index: Integer; Rules: TDAMapRules; RuleIndex: Integer);
    procedure LoadRules;
    procedure SaveRule(Index: Integer; Rules: TDAMapRules);
    procedure SaveRules;
  public
    constructor Create(AOwner: TComponent); override;

    property FieldNameListAllowed: boolean read FFieldNameListAllowed write FFieldNameListAllowed;
  end;

implementation

uses
  DAQueryEditor;

{$IFNDEF FPC}
{$IFDEF CLR}
{$R DADataTypeMapFrame.dfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{ TDADataTypeMapFrame }

procedure TDADataTypeMapFrame.grdDataTypeMapDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  R: TRect;
  Str: string;
  TextWidth, TextHeight: Integer;
begin
{$IFNDEF UNIX}
  if (ARow >= rowTitleCount) and (ACol = colIgnoreError) then
  begin
    chkIgnoreError.Checked := GetIgnoreError(ARow);

    R := TCRStringGrid(Sender).CellRect(ACol, ARow);
    TCRStringGrid(Sender).Canvas.FillRect(R);

    R := GetCheckBoxRect(ACol, ARow);
    chkIgnoreError.PaintTo(TCRStringGrid(Sender).Canvas.Handle, R.Left, R.Top);
  end
  else
{$ENDIF}  
  if ACol in [colDBLengthMin, colDBLengthMax, colDBScaleMin, colDBScaleMax, colFieldLength, colFieldScale] then begin
    R := TCRStringGrid(Sender).CellRect(ACol, ARow);
    TCRStringGrid(Sender).Canvas.FillRect(R);

    Str := TCRStringGrid(Sender).Cells[ACol, ARow];
    TextWidth := TCRStringGrid(Sender).Canvas.TextWidth(Str);
    TextHeight := TCRStringGrid(Sender).Canvas.TextHeight(Str);
    TCRStringGrid(Sender).Canvas.TextOut(R.Right - TextWidth - 2, R.Top + (R.Bottom - R.Top - TextHeight) div 2, Str);
  end;
end;

procedure TDADataTypeMapFrame.grdDataTypeMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col, Row: Integer;
  R: TRect;
begin
  TCRStringGrid(Sender).MouseToCell(X, Y, Col, Row);
  if Col = colIgnoreError then
  begin
    R := GetCheckBoxRect(Col, Row);

    if (X >= R.Left) and (X <= R.Right) and (Y >= R.Top) and (Y <= R.Bottom)
      then
    begin
      SetIgnoreError(Row, not GetIgnoreError(Row));
      Modified := True;
      TCRStringGrid(Sender).Invalidate;
    end;
  end;
end;

procedure TDADataTypeMapFrame.grdDataTypeMapSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);

  procedure ClearFieldNameInfo;
  begin
    FDataTypeMapGrid.Cells[colFieldName, ARow] := '';
  end;

  procedure ClearDBLength;
  begin
    FDataTypeMapGrid.Cells[colDBLengthMin, ARow] := '';
    FDataTypeMapGrid.Cells[colDBLengthMax, ARow] := '';
  end;

  procedure ClearDBScale;
  begin
    FDataTypeMapGrid.Cells[colDBScaleMin, ARow] := '';
    FDataTypeMapGrid.Cells[colDBScaleMax, ARow] := '';
  end;

  procedure ClearDBTypeInfo;
  begin
    FDataTypeMapGrid.Cells[colDBType, ARow] := '';
    ClearDBLength;
    ClearDBScale;
  end;

  procedure ClearFieldLength;
  begin
    FDataTypeMapGrid.Cells[colFieldLength, ARow] := '';
  end;

  procedure ClearFieldScale;
  begin
    FDataTypeMapGrid.Cells[colFieldScale, ARow] := '';
  end;

var
  DBTypeInfo: TDBTypeInfo;
  FieldTypeInfo: TFieldTypeInfo;
  ConverterManagerClass: TConverterManagerClass;
begin
  if ARow < rowTitleCount then
    exit;

  Modified := True;

  if IsComboBox(ACol, ARow) then begin
    if ACol = colFieldName then begin
      if Value <> '' then
        ClearDBTypeInfo;
    end
    else if ACol = colDBType then begin
      if Value <> '' then begin
        ClearFieldNameInfo;
        ConverterManagerClass := GetConverterManagerClass;
        if ConverterManagerClass <> nil then
          DBTypeInfo := DBTypeInfos.FindTypeInfo(Value, ConverterManagerClass.GetDBProvider)
        else
          DBTypeInfo := nil;
        if DBTypeInfo = nil then begin
          ClearDBLength;
          ClearDBScale;
        end
        else begin
          if not DBTypeInfo.Length then
            ClearDBLength;
          if not DBTypeInfo.Scale then
            ClearDBScale;
        end;
      end
      else begin
        ClearDBLength;
        ClearDBScale;
      end;
    end
    else if ACol = colFieldType then begin
      if Value <> '' then begin
        FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(Value);
        if FieldTypeInfo = nil then begin
          ClearFieldLength;
          ClearFieldScale;
        end
        else begin
          if not FieldTypeInfo.Length then
            ClearFieldLength;
          if not FieldTypeInfo.Scale then
            ClearFieldScale;
        end;
      end
      else begin
        ClearDBLength;
        ClearDBScale;
      end;
    end;
  end;
end;

procedure TDADataTypeMapFrame.btnAddRuleClick(Sender: TObject);
var
  Col: integer;
  HasValues: boolean;
begin
  HasValues := False;
  for Col := 0 to FDataTypeMapGrid.ColCount - 1 do
    if FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.RowCount - 1] <> '' then  begin
      HasValues := True;
      break;
    end;

  if not HasValues then
    exit;

  FDataTypeMapGrid.RowCount := FDataTypeMapGrid.RowCount + 1;

  FModified := True;
  ResizeGrid;
end;

procedure TDADataTypeMapFrame.btnRemoveRuleClick(Sender: TObject);
var
  Col: integer;
  Row: integer;
begin
  if FDataTypeMapGrid.RowCount <= rowTitleCount then
    exit;

  if FDataTypeMapGrid.RowCount = rowTitleCount + 1 then begin
    for Col := 0 to FDataTypeMapGrid.ColCount - 1 do
      FDataTypeMapGrid.Cells[Col, rowTitleCount] := '';
  end
  else begin
    for Row := FDataTypeMapGrid.Row + 1 to FDataTypeMapGrid.RowCount - 1 do
      for Col := 0 to FDataTypeMapGrid.ColCount - 1 do
        FDataTypeMapGrid.Cells[Col, Row - 1] := FDataTypeMapGrid.Cells[Col, Row];

    FDataTypeMapGrid.RowCount := FDataTypeMapGrid.RowCount - 1;
  end;

  FModified := True;
  ResizeGrid;
end;

procedure TDADataTypeMapFrame.btnMoveDownClick(Sender: TObject);
var
  tmp: string;
  Col: integer;
begin
  if FDataTypeMapGrid.Row >= FDataTypeMapGrid.RowCount - 1 then
    exit;

  for Col := 0 to FDataTypeMapGrid.ColCount - 1 do begin
    tmp := FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row];
    FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row] := FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row + 1];
    FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row + 1] := tmp;
  end;

  FModified := True;
end;

procedure TDADataTypeMapFrame.btnMoveUpClick(Sender: TObject);
var
  tmp: string;
  Col: integer;
begin
  if FDataTypeMapGrid.Row <= rowTitleCount then
    exit;

  for Col := 0 to FDataTypeMapGrid.ColCount - 1 do begin
    tmp := FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row];
    FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row] := FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row - 1];
    FDataTypeMapGrid.Cells[Col, FDataTypeMapGrid.Row - 1] := tmp;
  end;

  FModified := True;
end;

function TDADataTypeMapFrame.GetConverterManagerClass: TConverterManagerClass;
begin
  Result := Editor.DADesignUtilsClass.GetConverterManagerClass;
end;

function TDADataTypeMapFrame.GetFieldTypeInfos: TFieldTypeInfos;
begin
  Result := nil;
  if Editor.LocalComponent is TCustomDADataSet then
    Result := TDBAccessUtils.GetFieldTypeMapClass(TCustomDADataSet(Editor.LocalComponent)).GetFieldTypeInfos
  else if Editor.LocalComponent is TCustomDAConnection then
    Result := TDBAccessUtils.GetFieldTypeMapClass(TCustomDAConnection(Editor.LocalComponent)).GetFieldTypeInfos
  else
    Assert(False, Editor.LocalComponent.ClassName);
end;

function TDADataTypeMapFrame.GetRules: TDAMapRules;
begin
  Result := nil;
  if Editor.LocalComponent is TCustomDADataSet then
    Result := TCustomDADataSet(Editor.LocalComponent).DataTypeMap
  else if Editor.LocalComponent is TCustomDAConnection then
    Result := TCustomDAConnection(Editor.LocalComponent).DataTypeMap
  else
    Assert(False, Editor.LocalComponent.ClassName);
end;

function TDADataTypeMapFrame.GetIgnoreError(ARow: Integer): Boolean;
begin
  Result := FDataTypeMapGrid.Cells[colIgnoreError, ARow] <> '';
end;

procedure TDADataTypeMapFrame.SetIgnoreError(ARow: Integer; Value: Boolean);
begin
  if Value then
    FDataTypeMapGrid.Cells[colIgnoreError, ARow] := '*'
  else
    FDataTypeMapGrid.Cells[colIgnoreError, ARow] := '';
end;

procedure TDADataTypeMapFrame.PaintGridTitle(DC: HDC);

  procedure DrawTitle(Left, Top, Width, Height: Integer);
  begin
    if Width <= 0 then
      exit;

    FFakeGrid.DefaultColWidth := Width;
    FFakeGrid.DefaultRowHeight := Height;

    FFakeGrid.PaintRectTo({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(0, 0, FFakeGrid.DefaultColWidth, FFakeGrid.DefaultRowHeight), DC, Left, Top);
  end;

  procedure DrawText(Rect: TRect; Text: string);
  const
    TextInterval = 2;
  var
    TextWidth, TextHeigh, TextHeighFull: Integer;
    TextLeft, TextTop: Integer;
    SubText: String;
  begin
    if Rect.Right - Rect.Left <= 0 then
      exit;

    TextHeigh := FDataTypeMapGrid.Canvas.TextHeight(Text);
    if Pos(#13, Text) > 0 then
      TextHeighFull := TextHeigh + TextHeigh + TextInterval
    else
      TextHeighFull := TextHeigh;
    TextTop := Rect.Top + (Rect.Bottom - Rect.Top - TextHeighFull) div 2;

    if Pos(#13, Text) > 0 then begin
      SubText := copy(Text, 1, Pos(#13, Text) - 1);
      TextWidth := FDataTypeMapGrid.Canvas.TextWidth(SubText);
      TextLeft := Rect.Left + (Rect.Right - Rect.Left - TextWidth) div 2;
    {$IFNDEF FPC}
      FDataTypeMapGrid.Canvas.Brush.Style := bsClear;
    {$ENDIF}
      FDataTypeMapGrid.Canvas.TextOut(TextLeft,  TextTop, SubText);

      SubText := copy(Text, Pos(#13, Text) + 1, Length(Text));
      TextWidth := FDataTypeMapGrid.Canvas.TextWidth(SubText);
      TextLeft := Rect.Left + (Rect.Right - Rect.Left - TextWidth) div 2;
    {$IFNDEF FPC}
      FDataTypeMapGrid.Canvas.Brush.Style := bsClear;
    {$ENDIF}
      FDataTypeMapGrid.Canvas.TextOut(TextLeft,  TextTop + TextHeigh + TextInterval, SubText);
    end
    else begin
      TextWidth := FDataTypeMapGrid.Canvas.TextWidth(Text);
      TextLeft := Rect.Left + (Rect.Right - Rect.Left - TextWidth) div 2;
    {$IFNDEF FPC}
      FDataTypeMapGrid.Canvas.Brush.Style := bsClear;
    {$ENDIF}
      FDataTypeMapGrid.Canvas.TextOut(TextLeft,  TextTop, Text);
    end;
  end;

var
  colLeft: Integer;
  colTop: Integer;
  colWidth: Integer;
  colHeight: Integer;
{$IFNDEF FPC}
  SaveIndex: Integer;
{$ELSE}
  oldHandle: HDC;
{$ENDIF}
begin
  FDataTypeMapGrid.Canvas.Lock;
  try
  {$IFNDEF FPC}
    SaveIndex := SaveDC(FDataTypeMapGrid.Canvas.Handle);
  {$ELSE}
    oldHandle := FDataTypeMapGrid.Canvas.Handle;
  {$ENDIF}
    try
      FDataTypeMapGrid.Canvas.Handle := DC;

      colLeft := GetColumnLeft(colFieldName);
      colWidth := GetColumnWidth(colFieldName);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, ColHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Field Name');

      colLeft := GetColumnLeft(colDBType);
      colWidth := GetColumnWidth(colDBType);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, ColHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Database Type');

      colLeft := GetColumnLeft(colDBLengthMin);
      colWidth := GetColumnWidth(colDBLengthMin) + GetColumnWidth(colDBLengthMax) {$IFNDEF FPC}+1{$ENDIF};
      colHeight := 18;
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth + 1, colHeight), 'DB Size');
      colLeft := GetColumnLeft(colDBLengthMin);
      colTop := 18 {$IFNDEF FPC}+1{$ENDIF};
      colWidth := GetColumnWidth(colDBLengthMin);
      colHeight := 18;
    {$IFDEF FPC}
      DrawTitle(colLeft, colTop, colWidth, colHeight);
    {$ENDIF}
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, colTop, colLeft + colWidth, colTop + colHeight), 'Min');
      colLeft := GetColumnLeft(colDBLengthMax);
      colWidth := GetColumnWidth(colDBLengthMax);
    {$IFDEF FPC}
      DrawTitle(colLeft, colTop, colWidth, colHeight);
    {$ENDIF}
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, colTop, colLeft + colWidth, colTop + colHeight), 'Max');

      colLeft := GetColumnLeft(colDBScaleMin);
      colWidth := GetColumnWidth(colDBScaleMin) + GetColumnWidth(colDBScaleMax) {$IFNDEF FPC}+1{$ENDIF};
      colHeight := 18;
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'DB Scale');
      colLeft := GetColumnLeft(colDBScaleMin);
      colTop := 18 {$IFNDEF FPC}+1{$ENDIF};
      colWidth := GetColumnWidth(colDBScaleMin);
      colHeight := 18;
    {$IFDEF FPC}
      DrawTitle(colLeft, colTop, colWidth, colHeight);
    {$ENDIF}
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, colTop, colLeft + colWidth, colTop + colHeight), 'Min');
      colLeft := GetColumnLeft(colDBScaleMax);
      colWidth := GetColumnWidth(colDBScaleMax);
    {$IFDEF FPC}
      DrawTitle(colLeft, colTop, colWidth, colHeight);
    {$ENDIF}
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, colTop, colLeft + colWidth, colTop + colHeight), 'Max');

      colLeft := GetColumnLeft(colFieldType);
      colWidth := GetColumnWidth(colFieldType);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Field Type');

      colLeft := GetColumnLeft(colFieldLength);
      colWidth := GetColumnWidth(colFieldLength);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Field' + #13 + 'Size');

      colLeft := GetColumnLeft(colFieldScale);
      colWidth := GetColumnWidth(colFieldScale);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Field' + #13 + 'Scale');

      colLeft := GetColumnLeft(colIgnoreError);
      colWidth := GetColumnWidth(colIgnoreError);
      colHeight := 36 {$IFNDEF FPC}+1{$ENDIF};
      DrawTitle(colLeft, 0, colWidth, colHeight);
      DrawText({$IFDEF CLR}TRect.Create{$ELSE}Rect{$ENDIF}(colLeft, 0, colLeft + colWidth, colHeight), 'Ignore' + #13 + 'Error');
    finally
    {$IFNDEF FPC}
      RestoreDC(FDataTypeMapGrid.Canvas.Handle, SaveIndex);
    {$ELSE}
      FDataTypeMapGrid.Canvas.Handle := oldHandle;
    {$ENDIF}
    end;
  finally
    FDataTypeMapGrid.Canvas.Unlock;
  end;
end;

function TDADataTypeMapFrame.GetColumnLeft(ColumnIndex: Integer): integer;
var
  R: TRect;
begin
  R := FDataTypeMapGrid.CellRect(ColumnIndex, 0);
  Result := R.Left;
end;

function TDADataTypeMapFrame.GetColumnWidth(ColumnIndex: Integer): integer;
var
  R: TRect;
begin
  R := FDataTypeMapGrid.CellRect(ColumnIndex, 0);
  Result := R.Right - R.Left;
end;

function TDADataTypeMapFrame.CalculateColumnWidth(ColumnIndex: Integer): integer;

  function GetDefaultColumnWidth(ColumnIndex: Integer): integer;
  begin
    case ColumnIndex of
      colFieldName:
        Result := widthFieldName;
      colDBType:
        Result := widthDBType;
      colDBLengthMin:
        Result := widthDBLengthMin;
      colDBLengthMax:
        Result := widthDBLengthMax;
      colDBScaleMin:
        Result := widthDBScaleMin;
      colDBScaleMax:
        Result := widthDBScaleMax;
      colFieldType:
        Result := widthFieldType;
      colFieldLength:
        Result := widthFieldLength;
      colFieldScale:
        Result := widthFieldScale;
      colIgnoreError:
        Result := widthIgnoreError;
    else
      Result := 0;
    end;
  end;

  function GetAllColumnWidth: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to colIgnoreError do
      Result := Result + GetDefaultColumnWidth(i);
  end;

begin
  if GetAllColumnWidth > FDataTypeMapGrid.ClientWidth then
    Result := GetDefaultColumnWidth(ColumnIndex)
  else if ColumnIndex < colIgnoreError then
    Result := GetDefaultColumnWidth(ColumnIndex) * FDataTypeMapGrid.ClientWidth div GetAllColumnWidth
  else
    Result := FDataTypeMapGrid.ClientWidth - GetColumnLeft(colIgnoreError);
end;

procedure TDADataTypeMapFrame.IsSelectableCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ARow >= rowTitleCount) and (ACol = colIgnoreError) then
    CanSelect := False
  else
    CanSelect := True;
end;

function TDADataTypeMapFrame.IsEditableCell(ACol, ARow: integer): boolean;

  function IsDBTypeLengthAllowed(TypeName: string): boolean;
  var
    DBTypeInfo: TDBTypeInfo;
    ConverterManagerClass: TConverterManagerClass;
  begin
    if TypeName = '' then
      Result := False
    else begin
      ConverterManagerClass := GetConverterManagerClass;
      if ConverterManagerClass <> nil then
        DBTypeInfo := DBTypeInfos.FindTypeInfo(TypeName, ConverterManagerClass.GetDBProvider)
      else
        DBTypeInfo := nil;
      if DBTypeInfo <> nil then
        Result := DBTypeInfo.Length
      else
        Result := False;
    end;
  end;

  function IsDBTypeScaleAllowed(TypeName: string): boolean;
  var
    DBTypeInfo: TDBTypeInfo;
    ConverterManagerClass: TConverterManagerClass;
  begin
    if TypeName = '' then
      Result := False
    else begin
      ConverterManagerClass := GetConverterManagerClass;
      if ConverterManagerClass <> nil then
        DBTypeInfo := DBTypeInfos.FindTypeInfo(TypeName, ConverterManagerClass.GetDBProvider)
      else
        DBTypeInfo := nil;
      if DBTypeInfo <> nil then
        Result := DBTypeInfo.Scale
      else
        Result := False;
    end;
  end;

  function IsFieldTypeLengthAllowed(TypeName: string): boolean;
  var
    FieldTypeInfo: TFieldTypeInfo;
  begin
    if TypeName = '' then
      Result := False
    else begin
      FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(TypeName);
      if FieldTypeInfo <> nil then
        Result := FieldTypeInfo.Length
      else
        Result := False;
    end;
  end;

  function IsFieldTypeScaleAllowed(TypeName: string): boolean;
  var
    FieldTypeInfo: TFieldTypeInfo;
  begin
    if TypeName = '' then
      Result := False
    else begin
      FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(TypeName);
      if FieldTypeInfo <> nil then
        Result := FieldTypeInfo.Scale
      else
        Result := False;
    end;
  end;

begin
  if ARow < rowTitleCount then
    Result := False
  else if ACol = colIgnoreError then
    Result := False
  else if (ACol = colDBLengthMin) or (ACol = colDBLengthMax) then
    Result := IsDBTypeLengthAllowed(FDataTypeMapGrid.Cells[colDBType, ARow])
  else if (ACol = colDBScaleMin) or (ACol = colDBScaleMax) then
    Result := IsDBTypeScaleAllowed(FDataTypeMapGrid.Cells[colDBType, ARow])
  else if ACol = colFieldLength then
    Result := IsFieldTypeLengthAllowed(FDataTypeMapGrid.Cells[colFieldType, ARow])
  else if ACol = colFieldScale then
    Result := IsFieldTypeScaleAllowed(FDataTypeMapGrid.Cells[colFieldType, ARow])
  else
    Result := True;
end;

function TDADataTypeMapFrame.IsComboBox(ACol, ARow: integer): boolean;
begin
  if FFieldNameListAllowed and
     (Editor.LocalComponent is TCustomDADataSet) and
     (ACol = colFieldName) and
     (ARow >= rowTitleCount)
  then
    Result := True
  else if (ACol in [colDBType, colFieldType]) and (ARow >= rowTitleCount) then
    Result := True
  else
    Result := False;
end;

function TDADataTypeMapFrame.IsNumeric(ACol, ARow: integer): boolean;
begin
  if (ACol in [colDBLengthMin, colDBLengthMax,
               colDBScaleMin, colDBScaleMax,
               colFieldLength, colFieldScale]) and
     (ARow >= rowTitleCount)
  then
    Result := True
  else
    Result := False;
end;

procedure TDADataTypeMapFrame.GetPickListItems(ACol, ARow: Integer; Items: TStrings);
begin
  Items.Clear;

  if ACol = colFieldName then
    FillFieldList(Items)
  else if ACol = colDBType then
    FillDBTypeList(Items)
  else if ACol = colFieldType then
    FillFieldTypeList(Items);
end;

constructor TDADataTypeMapFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFieldNameListAllowed := True;
  InitGrid;
end;

procedure TDADataTypeMapFrame.Resize;
begin
  inherited;

  if btnAddRule <> nil then begin
    btnAddRule.Left := 8;
    btnAddRule.Top := self.ClientHeight - btnAddRule.Height - 8;

    btnRemoveRule.Left := btnAddRule.Left + btnAddRule.Width;
    btnRemoveRule.Top := btnAddRule.Top;

    btnMoveDown.Left := self.ClientWidth - btnMoveDown.Width - 8;
    btnMoveDown.Top := btnAddRule.Top;

    btnMoveUp.Left := btnMoveDown.Left - btnMoveUp.Width;
    btnMoveUp.Top := btnAddRule.Top;

    ResizeGrid;
  end;
end;

procedure TDADataTypeMapFrame.DoActivate;
begin
  inherited;

  InitCheckBox;

  LoadRules;

  FDataTypeMapGrid.Parent := self;
  FDataTypeMapGrid.TabOrder := 0;
  ResizeGrid;
end;

procedure TDADataTypeMapFrame.DoFinish;
begin
  if Modified then
    SaveRules;

  inherited;
end;

procedure TDADataTypeMapFrame.InitGrid;
begin
  FDataTypeMapGrid := TCRStringGrid.Create(self);
  FDataTypeMapGrid.DefaultColWidth := 20;
  FDataTypeMapGrid.DefaultRowHeight := 18;
  FDataTypeMapGrid.ColCount := 10;
  FDataTypeMapGrid.RowCount := 3;
  FDataTypeMapGrid.Left := 8;
  FDataTypeMapGrid.Top := 8;
  FDataTypeMapGrid.FixedCols := 0;
  FDataTypeMapGrid.FixedRows := rowTitleCount;
  FDataTypeMapGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing];
  FDataTypeMapGrid.TabOrder := 0;
  FDataTypeMapGrid.OnDrawCell := grdDataTypeMapDrawCell;
  FDataTypeMapGrid.OnMouseDown := grdDataTypeMapMouseDown;
  FDataTypeMapGrid.OnSetEditText := grdDataTypeMapSetEditText;
  FDataTypeMapGrid.OnPaint := PaintGridTitle;
  FDataTypeMapGrid.OnEditCell := IsEditableCell;
  FDataTypeMapGrid.OnSelectCell := IsSelectableCell;
  FDataTypeMapGrid.OnComboCell := IsComboBox;
  FDataTypeMapGrid.OnNumericCell := IsNumeric;
  FDataTypeMapGrid.OnGetPickListItems := GetPickListItems;
  FDataTypeMapGrid.Visible := True;

  FFakeGrid := TCRStringGrid.Create(self);
  FFakeGrid.BorderStyle := bsNone;
  FFakeGrid.Visible := False;
  FFakeGrid.Parent := self;
  FFakeGrid.Width := 500;
  FFakeGrid.Height := 100;
  FFakeGrid.RowCount := 2;
  FFakeGrid.ColCount := 2;
  FFakeGrid.FixedCols := 1;
  FFakeGrid.FixedRows := 1;
  FFakeGrid.ScrollBars := ssNone;
end;

procedure TDADataTypeMapFrame.InitCheckBox;
begin
{$IFDEF FPC}
  chkIgnoreError.Parent := Editor; // FPC bug with PaintTo
{$ENDIF}
  chkIgnoreError.Top := -50;
  chkIgnoreError.Width := 13;
  chkIgnoreError.Height := 13;
  chkIgnoreError.Visible := True;
  chkIgnoreError.TabStop := False;
end;

procedure TDADataTypeMapFrame.FillFieldList(Items: TStrings);
var
  i: Integer;
  Query: TCustomDADataSet;
begin
  Items.Clear;
  Items.Add('');
  if Editor.LocalComponent is TCustomDADataSet then begin
    Query := TCustomDADataSet(Editor.LocalComponent);
    if Query <> nil then
    begin
      try
        // Query can contain invalid SQL
        Query.FieldDefs.Update;
      except
      end;
      for i := 0 to Query.FieldDefs.Count - 1 do
        Items.Add(Query.FieldDefs[i].Name);
    end;
  end
end;

procedure TDADataTypeMapFrame.FillDBTypeList(Items: TStrings);
var
  i: Integer;
  DBProvider: Integer;
  ConverterManagerClass: TConverterManagerClass;
begin
  Items.Clear;
  Items.Add('');

  ConverterManagerClass := GetConverterManagerClass;
  if ConverterManagerClass <> nil then
    DBProvider := GetConverterManagerClass.GetDBProvider
  else
    exit;

  for i := 0 to DBTypeInfos.Count - 1 do
    if DBTypeInfos[i].DBProvider = DBProvider then
      Items.Add(DBTypeInfos.TypeInfos[i].Name);
end;

procedure TDADataTypeMapFrame.FillFieldTypeList(Items: TStrings);
var
  i: Integer;
  FieldTypeInfos: TFieldTypeInfos;
begin
  Items.Clear;
  Items.Add('');

  FieldTypeInfos := GetFieldTypeInfos;
  for i := 0 to FieldTypeInfos.Count - 1 do
    Items.Add(FieldTypeInfos.TypeInfos[i].Name);
end;

procedure TDADataTypeMapFrame.ResizeGrid;
var
  NewWidth: Integer;
  NewHeight: Integer;
begin
  if FDataTypeMapGrid.Parent = nil then
    exit;

  NewHeight := btnAddRule.top - FDataTypeMapGrid.Top - 8;
  if FDataTypeMapGrid.Height <> NewHeight then 
    FDataTypeMapGrid.Height := NewHeight;

  NewWidth := self.ClientWidth - FDataTypeMapGrid.Left * 2;
  if FDataTypeMapGrid.Width <> NewWidth then
    FDataTypeMapGrid.Width := NewWidth;

  FDataTypeMapGrid.ColWidths[colFieldName] := CalculateColumnWidth(colFieldName);
  FDataTypeMapGrid.ColWidths[colDBType] := CalculateColumnWidth(colDBType);
  FDataTypeMapGrid.ColWidths[colDBLengthMin] := CalculateColumnWidth(colDBLengthMin);
  FDataTypeMapGrid.ColWidths[colDBLengthMax] := CalculateColumnWidth(colDBLengthMax);
  FDataTypeMapGrid.ColWidths[colDBScaleMin] := CalculateColumnWidth(colDBScaleMin);
  FDataTypeMapGrid.ColWidths[colDBScaleMax] := CalculateColumnWidth(colDBScaleMax);
  FDataTypeMapGrid.ColWidths[colFieldType] := CalculateColumnWidth(colFieldType);
  FDataTypeMapGrid.ColWidths[colFieldLength] := CalculateColumnWidth(colFieldLength);
  FDataTypeMapGrid.ColWidths[colFieldScale] := CalculateColumnWidth(colFieldScale);
  FDataTypeMapGrid.ColWidths[colIgnoreError] := CalculateColumnWidth(colIgnoreError);
end;

function TDADataTypeMapFrame.GetCheckBoxRect(ACol, ARow: Integer): TRect;
var
  R: TRect;
begin
  R := FDataTypeMapGrid.CellRect(ACol, ARow);
  Result.Left := R.Left + (R.Right - R.Left - chkIgnoreError.Width) div 2 + 1;
  Result.Top := R.Top + (R.Bottom - R.Top - chkIgnoreError.Height) div 2 + 1;
  Result.Right := Result.Left + chkIgnoreError.Width - 1;
  Result.Bottom := Result.Top + chkIgnoreError.Height - 1;
end;

function TDADataTypeMapFrame.IsEmptyRule(Row: Integer): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to FDataTypeMapGrid.ColCount - 2 do // Ignore last column
    if FDataTypeMapGrid.Cells[i, rowTitleCount] <> '' then begin
      Result := False;
      exit;
    end;
end;

procedure TDADataTypeMapFrame.ClearRule(Row: Integer);
var
  i: integer;
begin
  for i := 0 to FDataTypeMapGrid.ColCount - 1 do
    FDataTypeMapGrid.Cells[i, rowTitleCount] := '';
end;

procedure TDADataTypeMapFrame.LoadRule(Index: Integer; Rules: TDAMapRules; RuleIndex: Integer);

  function GetStrValue(Value: Integer): string;
  begin
    if Value >= 0 then
      Result := IntToStr(Value)
    else
      Result := '';
  end;

  function GetDBTypeName(DBType: Word): string;
  var
    DBTypeInfo: TDBTypeInfo;
  begin
    DBTypeInfo := DBTypeInfos.FindTypeInfo(DBType);
    if DBTypeInfo <> nil then
      Result := DBTypeInfo.Name
    else
      Result := '';
  end;

  function GetFieldTypeName(FieldType: TFieldType): string;
  var
    FieldTypeInfo: TFieldTypeInfo;
  begin
    FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(FieldType);
    if FieldTypeInfo <> nil then
      Result := FieldTypeInfo.Name
    else
      Result := '';
  end;

var
  Rule: TDAMapRule;
begin
  Rule := Rules[RuleIndex];

  FDataTypeMapGrid.Cells[colFieldName, Index] := Rule.FieldName;
  FDataTypeMapGrid.Cells[colDBType, Index] := GetDBTypeName(Rule.DBType);
  FDataTypeMapGrid.Cells[colDBLengthMin, Index] := GetStrValue(Rule.DBLengthMin);
  FDataTypeMapGrid.Cells[colDBLengthMax, Index] := GetStrValue(Rule.DBLengthMax);
  FDataTypeMapGrid.Cells[colDBScaleMin, Index] := GetStrValue(Rule.DBScaleMin);
  FDataTypeMapGrid.Cells[colDBScaleMax, Index] := GetStrValue(Rule.DBScaleMax);
  FDataTypeMapGrid.Cells[colFieldType, Index] := GetFieldTypeName(Rule.FieldType);
  FDataTypeMapGrid.Cells[colFieldLength, Index] := GetStrValue(Rule.FieldLength);
  FDataTypeMapGrid.Cells[colFieldScale, Index] := GetStrValue(Rule.FieldScale);
  SetIgnoreError(Index, Rule.IgnoreErrors);
end;

procedure TDADataTypeMapFrame.LoadRules;
var
  i: Integer;
  Rules: TDAMapRules;
  DBProvider: Word;
  ConverterManagerClass: TConverterManagerClass;
begin
  ConverterManagerClass := GetConverterManagerClass;
  if ConverterManagerClass <> nil then
    DBProvider := ConverterManagerClass.GetDBProvider
  else
    exit;

  Rules := GetRules;

  FDataTypeMapGrid.RowCount := rowTitleCount + 1;
  for i := 0 to Rules.Count - 1 do
    if (Rules[i].DBProvider = DBProvider) or
       (Rules[i].DBProvider = 0)
    then begin
      FDataTypeMapGrid.RowCount := FDataTypeMapGrid.RowCount + 1;
      LoadRule(FDataTypeMapGrid.RowCount - 2, Rules, i);
    end;

  if FDataTypeMapGrid.RowCount = rowTitleCount + 1 then begin
    for i := 0 to FDataTypeMapGrid.ColCount - 1 do
      FDataTypeMapGrid.Cells[i, rowTitleCount] := '';
  end
  else
    FDataTypeMapGrid.RowCount := FDataTypeMapGrid.RowCount - 1;
end;

procedure TDADataTypeMapFrame.SaveRule(Index: Integer; Rules: TDAMapRules);

  function GetIntValue(Value: string): Integer;
  begin
    if Value = '' then
      Result := -1
    else
      Result := StrToInt(Value);
  end;

  function GetDBType(TypeName: string): Word;
  var
    DBTypeInfo: TDBTypeInfo;
    ConverterManagerClass: TConverterManagerClass;
  begin
    ConverterManagerClass := GetConverterManagerClass;
    if ConverterManagerClass <> nil then
      DBTypeInfo := DBTypeInfos.FindTypeInfo(TypeName, ConverterManagerClass.GetDBProvider)
    else
      DBTypeInfo := nil;
    if DBTypeInfo <> nil then
      Result := DBTypeInfo.DBType
    else
      Result := 0;
  end;

  function GetFieldType(TypeName: string): TFieldType;
  var
    FieldTypeInfo: TFieldTypeInfo;
  begin
    FieldTypeInfo := GetFieldTypeInfos.FindTypeInfo(TypeName);
    if FieldTypeInfo <> nil then
      Result := FieldTypeInfo.FieldType
    else
      Result := ftUnknown;
  end;

var
  FieldName: string;
  DBType: Word;
  DBLengthMin: Integer;
  DBLengthMax: Integer;
  DBScaleMin: Integer;
  DBScaleMax: Integer;
  FieldType: TFieldType;
  FieldLength: Integer;
  FieldScale: Integer;
  IgnoreError: Boolean;
begin
  FieldName := FDataTypeMapGrid.Cells[colFieldName, Index];
  DBType := GetDBType(FDataTypeMapGrid.Cells[colDBType, Index]);
  DBLengthMin := GetIntValue(FDataTypeMapGrid.Cells[colDBLengthMin, Index]);
  DBLengthMax := GetIntValue(FDataTypeMapGrid.Cells[colDBLengthMax, Index]);
  DBScaleMin := GetIntValue(FDataTypeMapGrid.Cells[colDBScaleMin, Index]);
  DBScaleMax := GetIntValue(FDataTypeMapGrid.Cells[colDBScaleMax, Index]);
  FieldType := GetFieldType(FDataTypeMapGrid.Cells[colFieldType, Index]);
  FieldLength := GetIntValue(FDataTypeMapGrid.Cells[colFieldLength, Index]);
  FieldScale := GetIntValue(FDataTypeMapGrid.Cells[colFieldScale, Index]);
  IgnoreError := GetIgnoreError(Index);

  if (FieldName = '') and (DBType = 0) and (FieldType = ftUnknown) then
    exit;

  Rules.AddRule(FieldName,
    DBType, DBLengthMin, DBLengthMax, DBScaleMin, DBScaleMax,
    FieldType, FieldLength, FieldScale, IgnoreError);
end;

procedure TDADataTypeMapFrame.SaveRules;
var
  i: Integer;
  NewRules, Rules: TDAMapRules;
  DBProvider: Word;
  ConverterManagerClass: TConverterManagerClass;
begin
  ConverterManagerClass := GetConverterManagerClass;
  if ConverterManagerClass <> nil then
    DBProvider := ConverterManagerClass.GetDBProvider
  else
    exit;

  if Editor.LocalComponent is TCustomDAConnection then
    NewRules := TDAConnectionMapRulesClass(GetRules.ClassType).Create(TDAConnectionMapRules(GetRules).Connection)
  else if Editor.LocalComponent is TCustomDADataSet then
    NewRules := TDADataSetMapRulesClass(GetRules.ClassType).Create(TDADataSetMapRules(GetRules).DataSet)
  else begin
    NewRules := nil;
    Assert(False, Editor.LocalComponent.ClassName);
  end;

  try
    for i := 0 to FDataTypeMapGrid.RowCount - rowTitleCount - 1 do
      SaveRule(i + rowTitleCount, NewRules);

    Rules := GetRules;

    for i := Rules.Count - 1 downto 0  do
      if (Rules[i].DBProvider = DBProvider) or
         (Rules[i].DBProvider = 0)
      then
        Rules[i].Free;

    for i := 0 to NewRules.Count - 1 do
      Rules.Add.Assign(NewRules[i]);
  finally
    NewRules.Free;
  end;
end;

end.
