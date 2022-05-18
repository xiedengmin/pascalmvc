
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright � 1998-2021 Devart. All right reserved.
//  UniDAC About Window
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}
unit UniAbout;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Graphics, Forms, Controls, StdCtrls, Buttons, ExtCtrls, HelpUtils,
{$ENDIF}
  Classes, SysUtils;

type
  TUniDacAboutForm = class(TForm)
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
    procedure lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbMailMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbForumClick(Sender: TObject);
    procedure lbForumMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAbout;

implementation

{$IFDEF MSWINDOWS}
uses
  DBMonitorClient,
  ShellApi,
{$ENDIF}
  CRTypes;

{$I UniDacVer.inc}

{$IFDEF IDE}
{$R *.dfm}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$R UniAbout.dfm}
{$ENDIF}

procedure ShowAbout;
begin
  with TUniDacAboutForm.Create(Application) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TUniDacAboutForm.FormShow(Sender: TObject);
{$I IdeConsts.inc}
begin
  lbVersion.Caption := UniDacVersion + ' ';
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
{$IFDEF STD}
  lbEdition.Caption := 'Standard Edition';
{$ELSE}
  lbEdition.Caption := 'Professional Edition';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOWS}
  lblDBMonitorVer.Caption := GetDBMonitorVersion;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbWebClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://www.devart.com/unidac');
  lbWeb.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbMailClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  MailTo('unidac@devart.com');
  lbMail.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbWebMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $4080FF;
end;

procedure TUniDacAboutForm.lbMailMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  lbMail.Font.Color := $4080FF;
end;

procedure TUniDacAboutForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lbWeb.Font.Color := $FF0000;
  lbMail.Font.Color := $FF0000;
  lbForum.Font.Color := $FF0000;
end;

procedure TUniDacAboutForm.lbForumClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenUrl('http://forums.devart.com/viewforum.php?f=28');
  lbForum.Font.Color := $FF0000;
{$ENDIF}
end;

procedure TUniDacAboutForm.lbForumMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lbForum.Font.Color := $4080FF;
end;

end.