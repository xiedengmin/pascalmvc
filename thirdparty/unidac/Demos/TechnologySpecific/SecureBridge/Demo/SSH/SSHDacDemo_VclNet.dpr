program SSHDacDemo_VclNet;

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\system.drawing.dll'}

uses
  Forms,
  DemoForm in '..\Base\DemoForm.pas' {DemoForm},
  DemoFrame in '..\Base\DemoFrame.pas' {DemoFrame},
  SSHDacDemoForm in 'SSHDacDemoForm.pas' {SSHDacForm},
  SSH_Client in 'SSH_Client\SSH_Client.pas' {SSHClientFrame},
  RandomForm in '..\Base\RandomForm.pas' {fmRandom},
  DemoBase in '..\Base\DemoBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSSHDacForm, SSHDacForm);
  Application.Run;
end.
