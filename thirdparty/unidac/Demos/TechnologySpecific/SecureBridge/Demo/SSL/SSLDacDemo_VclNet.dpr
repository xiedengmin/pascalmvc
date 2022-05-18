program SSLDacDemo_VclNet;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\system.drawing.dll'}

uses
  Forms,
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSLDacDemoForm in 'SSLDacDemoForm.pas' {SSLDacForm},
  SSL_Client in 'SSL_Client\SSL_Client.pas' {SSLClientFrame},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSLDacForm, SSLDacForm);
  Application.Run;
end.
