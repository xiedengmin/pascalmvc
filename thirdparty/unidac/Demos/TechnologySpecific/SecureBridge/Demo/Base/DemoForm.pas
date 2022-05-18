{$I SBDemo.inc}

unit DemoForm;

interface

uses
{$IFNDEF MSWINDOWS}
  Types,
{$ENDIF}
  SysUtils, Classes,
  Windows, Forms, Messages, Controls, StdCtrls, Graphics, ImgList,
  ComCtrls, Dialogs, ExtCtrls, Tabs, Menus, DBCtrls, Buttons,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  DemoBase, DemoFrame
  {$IFDEF XPMAN}, UxTheme{$ENDIF}
  {$IFDEF USE_SYNEDIT}, SynMemo, SynEdit, SynEditHighlighter, SynHighlighterPas{$ENDIF}
  ;

type
  TDemoForm = class(TForm)
    MainPanel: TPanel;
    Shape1: TShape;
    pnTopLabel: TPanel;
    Panel2: TPanel;
    pnSource: TPanel;
    Panel6: TPanel;
    Panel3: TPanel;
    sbOpenDemoDir: TSpeedButton;
    pnDemo: TPanel;
    sbDemo: TSpeedButton;
    pnShowSource: TPanel;
    lbTitle: TLabel;
    Panel1: TPanel;
    btRandom: TSpeedButton;

    procedure FormCreate(Sender: TObject); virtual;
    procedure sbOpenDemoDirClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbDemoClick(Sender: TObject);
    procedure btRandomClick(Sender: TObject);

  protected
    Demos: TDemos;
    FRandomized: boolean;

    //Product customization
    function ApplicationTitle: string; virtual; abstract; //This function should return DAC product specific title
    procedure RegisterDemos; virtual; abstract;          //This procedure should regiter DAC product specific demos
    //XP manifest
    function GetIsXPMan: boolean;
  {$IFDEF XPMAN}
    procedure ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
  {$ENDIF}
    //Demo selection
    procedure InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType); virtual;
    procedure UpdateDemo;
  public
{$IFDEF USE_SYNEDIT}
    SourceBrowser: TSynMemo;
{$ELSE}
    SourceBrowser: TMemo;
{$ENDIF}
    function ProductColor: TColor; virtual; abstract;     //This function should return DAC product specific color
    procedure Randomize;
    property Randomized: boolean read FRandomized;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{$IFDEF XPMAN}
  {$R WindowsXP.res}
{$ENDIF}

uses
  RandomForm, ScBridge;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  Demos := TDemos.Create;
  RegisterDemos;
{$IFDEF XPMAN}
  if GetIsXPMan then begin
    ReplaceFlatStyle(Self, False);
    pnTopLabel.Color := ProductColor;
  end;
{$ENDIF}

{$IFDEF USE_SYNEDIT}
  SourceBrowser := TSynMemo.Create(pnSource);

  SourceBrowser.Highlighter := TSynPasSyn.Create(SourceBrowser);
  SourceBrowser.Options := [eoAltSetsColumnMode, eoAutoIndent, eoAutoSizeMaxScrollWidth, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabIndent, eoTabsToSpaces];
  with SourceBrowser.Gutter do begin
    Visible := True;
    AutoSize := True;
    DigitCount := 3;
    LeftOffset := 0;
    RightOffset := 0;
    ShowLineNumbers := True;
    Width := 1;
 end;
{$ELSE}
  SourceBrowser := TMemo.Create(pnSource);
  SourceBrowser.ScrollBars := ssVertical;
  SourceBrowser.Font.Name := 'Courier New';
  SourceBrowser.Font.Size := 10;
{$ENDIF}
  with SourceBrowser do begin
    Parent := pnSource;
    Align := alClient;
    ReadOnly := True;
  end;
  Resize;
  UpdateDemo;
end;

procedure TDemoForm.FormDestroy(Sender: TObject);
begin
  Demos.Free;
end;

//Sizing constraints
function TDemoForm.GetIsXPMan: boolean;
begin
  Result := {$IFDEF XPMAN}UseThemes; {$ELSE} False;{$ENDIF}
end;

{$IFDEF XPMAN}
procedure TDemoForm.ReplaceFlatStyle(Control: TWinControl; Flat: boolean);
var
  i: integer;
begin
  for i := 0 to Control.ControlCount - 1 do
    if Control.Controls[i] is TSpeedButton then
      TSpeedButton(Control.Controls[i]).Flat := Flat
    else
    if Control.Controls[i] is TDBNavigator then
      TDBNavigator(Control.Controls[i]).Flat := Flat
    else
      if Control.Controls[i] is TWinControl then begin
        if Control.Controls[i] is TPanel then begin
          TPanel(Control.Controls[i]).ParentBackground := False;
          if Control.Controls[i].Tag <> 0 then
            TPanel(Control.Controls[i]).Color := ProductColor
          else
            TPanel(Control.Controls[i]).Color := clBtnFace;
        end;
        ReplaceFlatStyle(TWinControl(Control.Controls[i]), Flat);
      end;

end;
{$ENDIF}

//Demo Change
procedure TDemoForm.InitializeDemoFrame(Frame: TDemoFrame; DemoType: TDemoType);
begin
  Frame.Parent := pnDemo;
  Frame.Initialize;
{$IFDEF XPMAN}
  if GetIsXPMan then
    ReplaceFlatStyle(Frame, False);
{$ENDIF}
end;

procedure TDemoForm.UpdateDemo;
begin
//  for i := 1 to StatusBar.Panels.Count - 1 do
//    StatusBar.Panels[i].Text := '';

  with Demos.SelectDemo(1) do begin
    InitializeDemoFrame(Frame, DemoType);
    LoadDemoCode(SourceBrowser.Lines);            //Load demo sources
    if DemoType = dtCategory then begin
      pnSource.Visible := False;
      pnDemo.Visible := True;
      sbDemo.Enabled := False;
    end
    else begin
      sbDemo.Enabled := True;
      if sbDemo.Down then begin
        pnSource.Visible := True;
        pnDemo.Visible := False;
      end;
    end;
    Self.Caption := ApplicationTitle + ' - ' + Hint;
    Application.Title := Self.Caption;
  end;
//  StatusBar.Repaint;
end;

procedure TDemoForm.sbOpenDemoDirClick(Sender: TObject);
begin
  Demos.SelectedDemo.OpenDemoFolder;
end;

procedure TDemoForm.sbDemoClick(Sender: TObject);
begin
  with sbDemo do
    if Down then begin
      pnSource.Visible := True;
      pnDemo.Visible := False;
      Caption := 'Show demo'
    end
    else begin
      pnSource.Visible := False;
      pnDemo.Visible := True;
      Caption := 'Show source'
    end;
end;

procedure TDemoForm.Randomize;
begin
  fmRandom := TfmRandom.Create(Self);
  try
    if fmRandom.ShowModal = mrOk then begin
      Random.Randomize(fmRandom.Data);
      FRandomized := True;
    end;
  finally
    fmRandom.Free;
  end;
end;

procedure TDemoForm.btRandomClick(Sender: TObject);
begin
  Self.Randomize;
end;

end.
