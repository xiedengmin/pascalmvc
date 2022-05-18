
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualAbout;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, HelpUtils,
{$ENDIF}
  Classes, SysUtils;

type
  TVirtualDacAboutForm = class(TForm)
    OKBtn: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lbVersion: TLabel;
    lbIDE: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbMail: TLabel;
    lbWeb: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    Label8: TLabel;
    lblDBMonitorVer: TLabel;
    lbForum: TLabel;
    Label10: TLabel;
    lbEdition: TLabel;
    Bevel2: TBevel;
    procedure FormShow(Sender: TObject);
    procedure lbWebClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure lbForumClick(Sender: TObject);
    procedure lbForumMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
  public
  end;

procedure ShowAbout;

implementation

uses
{$IFDEF MSWINDOWS}
  ShellApi,
{$ENDIF}
  CRTypes;

{$I VirtualDacVer.inc}

{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R VirtualAbout.dfm}
{$ENDIF}

procedure ShowAbout;
begin
  with TVirtualDacAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TVirtualDacAboutForm.FormShow(Sender: TObject);
{$I IdeConsts.inc}
begin
  lbVersion.Caption := VirtualDACVersion + ' ';
  lbIDE.Caption := ' for ' + IDEInfos[IDEVer].Name;
  lbIDE.Left := lbVersion.Left + lbVersion.Width;

{$IFDEF BETA}
  lbEdition.Caption := 'Beta';
  lbEdition.Font.Color := clRed;
{$ELSE}
{$IFDEF RC}
  lbEdition.Caption := 'Release Candidate';
  lbEdition.Font.Color := clGreen;
{$ELSE}
{$IFDEF EXPRESS}
  lbEdition.Caption := 'Express Edition';
{$ELSE}
  lbEdition.Caption := 'Standard Edition';
{$ENDIF}
{$ENDIF}
{$ENDIF}

//{$IFDEF MSWINDOWS}
//  lblDBMonitorVer.Caption := GetDBMonitorVersion;
//{$ENDIF}
end;

procedure TVirtualDacAboutForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://www.devart.com/virtualdac');
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TVirtualDacAboutForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  MailTo('virtualdac@devart.com');
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TVirtualDacAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TVirtualDacAboutForm.lbMailMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TVirtualDacAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
  lbForum.Font.Color := $FF0000;
end;

procedure TVirtualDacAboutForm.lbForumClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://forums.devart.com/viewforum.php?f=8');
  lbForum.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TVirtualDacAboutForm.lbForumMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  lbForum.Font.Color := $4080FF;
end;

end.
