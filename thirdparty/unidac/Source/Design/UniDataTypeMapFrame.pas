
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Data Type Mapping Frame
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}
unit UniDataTypeMapFrame;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics,
  Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  Forms, Dialogs, Grids, DB,
  {$IFNDEF FPC}MemDS, {$ELSE}MemDataSet, {$ENDIF} 
  CRDataTypeMap, DBAccess, Uni,
  CRFrame, DADataTypeMapFrame;

type
  TUniDataTypeMapFrame = class(TDADataTypeMapFrame)
    lbProvider: TLabel;
    edProvider: TComboBox;
    procedure edProviderChange(Sender: TObject);
  private
    FProviderName: string;
    function GetConnection: TUniConnection;
  protected
    procedure DoActivate; override;

    function GetConverterManagerClass: TConverterManagerClass; override;
    procedure ResizeGrid; override;
  public
  end;

implementation

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R UniDataTypeMapFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  UniProvider;

{ TUniDataTypeMapFrame }

procedure TUniDataTypeMapFrame.edProviderChange(Sender: TObject);
begin
  if FProviderName <> '' then
    SaveRules;
  FProviderName := TComboBox(Sender).Text;
  LoadRules;
end;

function TUniDataTypeMapFrame.GetConverterManagerClass: TConverterManagerClass;
var
  Provider: TUniProvider;
begin
  if FProviderName <> '' then
    Provider := UniProviders.GetProvider(FProviderName)
  else
    Provider := nil;
  if Provider <> nil then
    Result := Provider.GetConverterManagerClass
  else
    Result := nil;
end;

function TUniDataTypeMapFrame.GetConnection: TUniConnection;
begin
  Result := nil;
  if Editor.LocalComponent is TCustomUniDataSet then begin
    Result := (Editor.LocalComponent as TCustomUniDataSet).Connection
  end
  else if Editor.LocalComponent is TUniConnection then
    Result := Editor.LocalComponent as TUniConnection
  else
    Assert(False, Editor.LocalComponent.ClassName);
end;

procedure TUniDataTypeMapFrame.DoActivate;
begin
  if GetConnection <> nil then
    FProviderName := GetConnection.ProviderName
  else
    FProviderName := '';

  inherited;

  UniProviders.GetProviderNames(edProvider.Items);
  edProvider.ItemIndex := edProvider.Items.IndexOf(FProviderName);
end;

procedure TUniDataTypeMapFrame.ResizeGrid;
var
  NewTop: Integer;
begin
  NewTop := edProvider.Top + edProvider.Height + 8;
  if FDataTypeMapGrid.Top <> NewTop then
    FDataTypeMapGrid.Top := NewTop;

  inherited;
end;

end.
