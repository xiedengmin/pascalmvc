{$I DacDemo.inc}

unit DemoFrame;

interface

uses
  Classes,
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Forms, SysUtils, StdCtrls, Graphics, Controls, Dialogs,
  ComCtrls, Buttons, ExtCtrls, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  DBAccess;

type
  TDemoFrame = class(TFrame)
  public
    Connection: TCustomDAConnection;
    procedure Initialize; virtual;
    procedure SetDebug(Value: boolean); virtual;

    procedure AssignConnectionTo(Dest: TCustomDAConnection);
    function CheckProperty(PatternName, NameToVerify: string): Boolean;
  end;

  TDemoFrameClass = class of TDemoFrame;


implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

{$IFNDEF VER130}
uses
  Variants;
{$ENDIF}

procedure TDemoFrame.Initialize;
begin
end;

procedure TDemoFrame.SetDebug(Value: boolean);
begin
end;

procedure TDemoFrame.AssignConnectionTo(Dest: TCustomDAConnection);
begin
  Dest.Assign(Connection);
  Dest.AfterConnect := nil;
  Dest.AfterDisconnect := nil;
end;

function TDemoFrame.CheckProperty(PatternName, NameToVerify: string): Boolean;
begin
  if PatternName = '' then
    raise Exception.Create('PatternName can not be empty');
  Result := (NameToVerify = '') or (UpperCase(PatternName) = UpperCase(NameToVerify));
end;

{$IFDEF FPC}
initialization
  {$i DemoFrame.lrs}
{$ENDIF}

end.
