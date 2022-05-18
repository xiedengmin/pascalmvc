unit DemoFrame;

interface

uses
  Classes,
  Forms, Windows, Messages, SysUtils, StdCtrls, Graphics, Controls, Dialogs,
  ComCtrls, ShellAPI, Buttons, ExtCtrls {$IFNDEF VER130}, Variants{$ENDIF};

type
  TDemoFrame = class(TFrame)
  public
    procedure Initialize; virtual;
    procedure Finalize; virtual;
  end;

  TDemoFrameClass = class of TDemoFrame;


implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

procedure TDemoFrame.Initialize;
begin
end;

procedure TDemoFrame.Finalize;
begin
end;

end.
