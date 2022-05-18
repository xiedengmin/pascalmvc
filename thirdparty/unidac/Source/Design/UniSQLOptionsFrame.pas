
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  UniSQLOptions Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSQLOptionsFrame;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  CRTypes, CRFrame, Uni, UniSpecificOptionsFrame;

type
  TUniSQLOptionsFrame = class(TCRFrame)
    pnOptions: TPanel;
  protected
    FOptionsFrame: TUniSpecificOptionsFrame;

    function GetOptionsFrameTop: integer; virtual;
    procedure Resize; override;

    procedure DoActivate; override;
    procedure DoFinish; override;
  public
    constructor Create(Owner: TComponent); override;
  end;

implementation

{$IFNDEF FPC}
{$R UniSQLOptionsFrame.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DBAccess, DAScript, UniScript, 
{$IFNDEF STD}
  UniLoader, UniDump, UniAlerter,
{$ENDIF}
  CREditor, DASQLComponentEditor;

constructor TUniSQLOptionsFrame.Create(Owner: TComponent);
begin
  inherited;

  FOptionsFrame := TUniSpecificOptionsFrame.Create(Self);
  FOptionsFrame.Parent := pnOptions;
end;

function TUniSQLOptionsFrame.GetOptionsFrameTop: integer;
begin
  Result := 0;
end;

procedure TUniSQLOptionsFrame.Resize;
begin
  inherited;

  if pnOptions <> nil then
  begin
    pnOptions.Left := 0;
    pnOptions.Top := GetOptionsFrameTop;
    pnOptions.Width := Self.ClientWidth;
    pnOptions.Height := Self.ClientHeight - pnOptions.Top;
  end;
end;

procedure TUniSQLOptionsFrame.DoActivate;
var
  OptionsType: TOptionsType;
  SpecificOptions: TStrings;
  Connection: TUniConnection;
begin
  if Editor.LocalComponent is TUniSQL then begin
    Connection := TUniConnection(TDBAccessUtils.UsedConnection(TUniSQL(Editor.LocalComponent)));
    SpecificOptions := TUniSQL(Editor.LocalComponent).SpecificOptions;
    OptionsType := otSQL;
  end
  else
  if Editor.LocalComponent is TCustomUniDataSet then begin
    Connection := TUniConnection(TDBAccessUtils.UsedConnection(TCustomUniDataSet(Editor.LocalComponent)));
    SpecificOptions := TCustomUniDataSet(Editor.LocalComponent).SpecificOptions;
    OptionsType := otDataSet;
  end
  else
  if Editor.LocalComponent is TUniScript then begin
    Connection := TUniConnection(TDAScriptUtils.UsedConnection(TUniScript(Editor.LocalComponent)));
    SpecificOptions := TUniScript(Editor.LocalComponent).SpecificOptions;
    OptionsType := otScript;
  end
{$IFNDEF STD}
  else
  if Editor.LocalComponent is TUniLoader then begin
    Connection := TUniLoader(Editor.LocalComponent).Connection;
    SpecificOptions := TUniLoader(Editor.LocalComponent).SpecificOptions;
    OptionsType := otLoader;
  end
  else
  if Editor.LocalComponent is TUniDump then begin
    Connection := TUniDump(Editor.LocalComponent).Connection;
    SpecificOptions := TUniDump(Editor.LocalComponent).SpecificOptions;
    OptionsType := otDump;
  end
  else
  if Editor.LocalComponent is TUniAlerter then begin
    Connection := TUniAlerter(Editor.LocalComponent).Connection;
    SpecificOptions := TUniAlerter(Editor.LocalComponent).SpecificOptions;
    OptionsType := otAlerter;
  end
{$ENDIF}
  else
  if Editor.LocalComponent is TUniTransaction then begin
    Connection := TUniTransaction(Editor.LocalComponent).DefaultConnection;
    SpecificOptions := TUniTransaction(Editor.LocalComponent).SpecificOptions;
    OptionsType := otTransaction;
  end
  else begin
    Assert(False);
    exit;
  end;

  FOptionsFrame.InitOptions(SpecificOptions);
  if (Connection <> nil) then
    FOptionsFrame.LoadOptions(Connection.ProviderName, OptionsType)
  else
    FOptionsFrame.LoadOptions('', OptionsType);
end;

procedure TUniSQLOptionsFrame.DoFinish;
begin
  FOptionsFrame.Finish;
  Modified := FOptionsFrame.Modified;
end;


end.
