{$I ..\Base\SBDemo.inc}

unit SSLDacDemoForm;

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
  TSSLDacForm = class(TDemoForm)
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
  SSLDacForm: TSSLDacForm;

implementation

uses
  VirtualTable, SSL_Client;

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

function TSSLDacForm.ProductColor: TColor;
begin
  Result := $0000BBFF;
end;

function TSSLDacForm.ApplicationTitle: string;
begin
  Result := 'SecureBridge demo';
end;

procedure TSSLDacForm.RegisterDemos;
begin
  Demos.RegisterDemo('SSL_Client', 'CRSSLIOHandler', 'SSL_Client', '', TSSLClientFrame, 3);
end;

end.
