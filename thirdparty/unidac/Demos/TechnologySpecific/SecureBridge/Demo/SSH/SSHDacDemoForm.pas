{$I ..\Base\SBDemo.inc}

unit SSHDacDemoForm;

interface

uses
  SysUtils, Classes, DB,
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  DBAccess, DemoForm, DemoBase;

type
  TSSHDacForm = class(TDemoForm)
  private
    { Private declarations }
  protected
    //Product customization
    function ApplicationTitle: string; override;
    procedure RegisterDemos; override;
  public
    function ProductColor: TColor; override;
  end;

var
  SSHDacForm: TSSHDacForm;

implementation

uses
  VirtualTable, SSH_Client;

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

function TSSHDacForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSSHDacForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge demo';
end;

procedure TSSHDacForm.RegisterDemos;
begin
  Demos.RegisterDemo('SSH_Client', 'CRSSHIOHandler', 'SSH_Client', '', TSSHClientFrame, 3);
end;

end.
