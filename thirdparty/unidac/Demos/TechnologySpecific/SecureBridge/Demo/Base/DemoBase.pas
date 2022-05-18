unit DemoBase;

interface
{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
{$ENDIF}

uses
  Classes, SysUtils, DemoFrame,
  Windows, ComCtrls, Controls, Graphics, Forms,
  ShellAPI;

type
  TDemoType = (dtDemo, dtCategory);

  TDemo = class
  protected
    FName: string;
    FHint: string;
    FDescription: string;
    FDemoType: TDemoType;
    FFrameClass: TDemoFrameClass;
    FFrame: TDemoFrame;
  public
    constructor Create(Name, Hint, Description: string; DemoType: TDemoType; FrameClass: TDemoFrameClass);
    destructor Destroy; override;

    procedure LoadDemoCode(Strings: TStrings);
    procedure OpenDemoFolder;
    procedure FreeFrame;

    property Name: string read FName;
    property Hint: string read FHint;
    property Description: string read FDescription;
    property DemoType: TDemoType read FDemoType;
    property FrameClass: TDemoFrameClass read FFrameClass;
    property Frame: TDemoFrame read FFrame;
  end;

  TDemos = class
  protected
    FSelectedDemo: TDemo;
  public
    destructor Destroy; override;
    procedure RegisterDemo(DemoName, DemoHint, DemoDescription, DemoCategory: string; FrameClass: TDemoFrameClass; ImgIndex: integer);
    function SelectDemo(DemoIndex: integer): TDemo;    //Create demo frame by DemoIndex

    property SelectedDemo: TDemo read FSelectedDemo;
  end;

implementation

destructor TDemos.Destroy;
begin
  FSelectedDemo.Free;
  inherited;
end;

procedure TDemos.RegisterDemo(DemoName, DemoHint, DemoDescription, DemoCategory: string; FrameClass: TDemoFrameClass; ImgIndex: integer);
begin
  FSelectedDemo := TDemo.Create(DemoName, DemoHint, DemoDescription, dtDemo, FrameClass);
end;

function TDemos.SelectDemo(DemoIndex: integer): TDemo;  //Init and show demo by DemoIndex
begin
  Result := FSelectedDemo;
  with FSelectedDemo do
    if FFrame = nil then begin
      FFrame := FFrameClass.Create(nil);
    end
    else
      FFrame.Show;
end;

{TDemo}
constructor TDemo.Create(Name, Hint, Description: string; DemoType: TDemoType; FrameClass: TDemoFrameClass);
begin
  inherited Create;

  FName := Name;
  FHint := Hint;
  FDescription := Description;
  FFrameClass := FrameClass;
  FDemoType := DemoType;
end;

destructor TDemo.Destroy;
begin
  FreeFrame;

  inherited;
end;

procedure TDemo.LoadDemoCode(Strings: TStrings);
var
  FileName: string;
begin
  if DemoType = dtCategory then
    Strings.Clear
  else begin
  {$IFDEF MSWINDOWS}
    FileName := Format('%s\%s\%s.pas', [ExtractFilePath(Application.ExeName), Description, Name]);
  {$ELSE}
    FileName := Format('%s/%s/%s.pas', [ExtractFilePath(Application.ExeName), Description, Name]);
  {$ENDIF}

    if FileExists(FileName) then
      Strings.LoadFromFile(FileName)
    else
      Strings.Clear;
  end;
end;

procedure TDemo.OpenDemoFolder;
var
  FolderName: string;
begin
  if DemoType = dtDemo then begin
    FolderName := ExtractFilePath(Application.ExeName) + Description;
    ShellExecute(0, 'open', PChar(FolderName), '', '.', SW_SHOW);
  end;
end;

procedure TDemo.FreeFrame;
begin
  FFrame.Finalize;
  FFrame.Free;
  FFrame := nil;
end;


end.
