
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQL Editor Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}

{$I Dac.inc}

unit DASQLFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF DBTOOLS}
  DBToolsClient,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  CRTypes, CRFrame, CRTabEditor;

type
  TDASQLFrame = class(TCRFrame)
    meSQL: TMemo;
    Panel1: TPanel;
    procedure meSQLExit(Sender: TObject);

  protected
    function GetSQLText: string;
    procedure SetSQLText(const Value: string);

    procedure LoadMemo; virtual;
    procedure SaveMemo; virtual;
    procedure DoActivate; override;
    procedure DoFinish; override;
    function GetLocalComponentSQL: TStrings; virtual;
    procedure SetLocalComponentSQL(Value: TStrings); virtual;

    property SQLText: string read GetSQLText write SetSQLText;
    property LocalComponentSQL: TStrings read GetLocalComponentSQL write SetLocalComponentSQL;
  public
  {$IFDEF USE_SYNEDIT}
    constructor Create(Owner: TComponent); override;
  {$ENDIF}

    function ActiveControl: TWinControl; override;
  end;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R DASQLFrame.dfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  CREditor;

{$IFDEF USE_SYNEDIT}
constructor TDASQLFrame.Create(Owner: TComponent);
var
  WinControl: TWinControl;
begin
  inherited;
  
{$IFDEF DBTOOLS}
  if TCREditorForm(Owner).CRDesignUtilsClass.DBToolsAvailable then
    Exit;
{$ENDIF}
  WinControl := meSQL;
  Assert(Owner is TCREditorForm);
  TCREditorForm(Owner).ReplaceMemo(WinControl, True);
  meSQL := TMemo(WinControl);
end;
{$ENDIF}

function TDASQLFrame.GetSQLText: string;
begin
  Result := GetMemoText(meSQL);
end;

procedure TDASQLFrame.SetSQLText(const Value: string);
begin
  if SQLText <> Value then
    SetMemoText(meSQL, Value);
end;

function TDASQLFrame.GetLocalComponentSQL: TStrings;
begin
  Assert(Editor <> nil);
  Result := Editor.DADesignUtilsClass.GetSQL(Editor.LocalComponent);
end;

procedure TDASQLFrame.SetLocalComponentSQL(Value: TStrings);
begin
  Assert(Editor <> nil);
  Editor.DADesignUtilsClass.SetSQL(Editor.LocalComponent, Value);
end;

procedure TDASQLFrame.meSQLExit(Sender: TObject);
begin
  if LocalComponentSQL <> nil then
    if LocalComponentSQL.Text <> SQLText then begin
      SaveMemo;
      Modified := True;
    end;
end;

function TDASQLFrame.ActiveControl: TWinControl;
begin
{$IFDEF DBTOOLS}
  if Assigned(DBTools) and DBTools.HasDACSqlEditorFrame(meSQL) then
    Result := DBTools.GetDACSqlEditorFrame(meSQL)
  else
{$ENDIF}
    Result := meSQL;
end;

procedure TDASQLFrame.LoadMemo;
begin
  SQLText := LocalComponentSQL.Text;
end;

procedure TDASQLFrame.SaveMemo;
begin
  LocalComponentSQL.Text := SQLText;
end;

procedure TDASQLFrame.DoActivate;
begin
{$IFDEF DBTOOLS}
  if Assigned(DBTools) then
    DBTools.ReplaceMemo(meSQL, Editor.DADesignUtilsClass, Editor.Component);
{$ENDIF}
  LoadMemo;
end;

procedure TDASQLFrame.DoFinish;
begin
  meSQLExit(nil);

  inherited;
end;

end.
