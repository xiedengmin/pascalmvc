{$IFNDEF CLR}

{$I UniDac.inc}

unit UniSpecificOptionsFrame;
{$ENDIF}

{$IFNDEF FPC}
{$DEFINE USE_VALEDIT}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DacVcl, Buttons, Grids,
{$IFDEF USE_VALEDIT}
  ValEdit,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  CRTypes, CRFrame, DBAccess, UniProvider;

type
  TOptionsType = (otConnection, otSQL, otDataSet, otScript, 
                {$IFNDEF STD}
                  otLoader, otDump, otAlerter, 
                {$ENDIF}
                  otTransaction);

  TOptionsMemo = class(TMemo);

  TUniSpecificOptionsFrame = class(TCRFrame)
    pnProvider: TPanel;
    lbProvider: TLabel;
    edProvider: TComboBox;
    procedure edProviderChange(Sender: TObject);
  protected
    edOptions: {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TOptionsMemo{$ENDIF};
    FEditorList: TStrings;

    FProviderName: string;
    FOptionsType: TOptionsType;
    FOptionsList: TOptionsList;
    FSpecificOptions: TStrings;

  {$IFDEF USE_VALEDIT}
    procedure edOptionsGetPickList(Sender: TObject; const KeyName: String; Values: TStrings);
    procedure edOptionsValidate(Sender: TObject; ACol, ARow: Integer; const KeyName, KeyValue: String);
    procedure edOptionsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure edOptionsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
  {$ENDIF}
    procedure edOptionsExit(Sender: TObject);
    procedure edOptionsChanged(Sender: TObject);

    function CheckOptionDefault(Option: TOption; var Value: string): boolean;

    procedure DoFinish; override;
    procedure Resize; override;
  public
    constructor Create(Owner: TComponent); override;

    procedure InitOptions(SpecificOptions: TStrings);
    procedure SaveOptions;
    procedure LoadOptions(const ProviderName: string; OptionsType: TOptionsType);
  end;

implementation

uses
  CRFunctions, Uni;

{$IFNDEF FPC}
{$IFDEF IDE}
{$R *.dfm}
{$ELSE}
{$R UniSpecificOptionsFrame.dfm}
{$ENDIF}
{$ELSE}
{$R *.lfm}
{$ENDIF}

constructor TUniSpecificOptionsFrame.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSpecificOptions := nil;
  UniProviders.GetProviderNames(edProvider.Items);

  edOptions := {$IFDEF USE_VALEDIT}TValueListEditor{$ELSE}TOptionsMemo{$ENDIF}.Create(Self);
  edOptions.Parent := Self;
  edOptions.OnExit := edOptionsExit;
{$IFDEF USE_VALEDIT}
  edOptions.OnGetPickList := edOptionsGetPickList;
  edOptions.OnValidate := edOptionsValidate;
  edOptions.OnDrawCell := edOptionsDrawCell;
  edOptions.OnSetEditText := edOptionsSetEditText;
  FEditorList := edOptions.Strings;
{$ELSE}
  FEditorList := edOptions.Lines;
{$ENDIF}
end;

function TUniSpecificOptionsFrame.CheckOptionDefault(Option: TOption; var Value: string): boolean;
var
  DefaultValue: string;
begin
  if Option.CheckValue(Value) then
    Value := Option.GetAsString(Option.GetAsNative(Value)); // normalize

  DefaultValue := Option.GetAsString(Option.GetDefaultValue);

  Result := Value = DefaultValue;
end;

procedure TUniSpecificOptionsFrame.Resize;
begin
  inherited;

  if pnProvider <> nil then begin
    pnProvider.Left := 8;
    pnProvider.Top := 8;
    pnProvider.Width := Self.ClientWidth - pnProvider.Left * 2;
  end;

  if edOptions <> nil then begin
    edOptions.Left := pnProvider.Left;
    edOptions.Top := pnProvider.Top + pnProvider.Height;
    edOptions.Width := pnProvider.Width;
    edOptions.Height := Self.ClientHeight - edOptions.Top - 8;
  end;
end;

procedure TUniSpecificOptionsFrame.LoadOptions(const ProviderName: string; OptionsType: TOptionsType);
var
  OptionValue: string;
  i: integer;
  Provider: TUniProvider;
  OldModified: boolean;
begin
  Assert(FSpecificOptions <> nil);

{$IFDEF FPC}
  FEditorList := edOptions.Lines;
  FEditorList.NameValueSeparator := '=';
{$ENDIF}

  FProviderName := ProviderName;
  FOptionsType := OptionsType;

  edProvider.Text := ProviderName;
  FEditorList.Clear;

  FOptionsList := nil;

  if ProviderName <> '' then begin
    Provider := UniProviders.GetProvider(ProviderName);
    if Provider = nil then
      exit;

    case OptionsType of
      otConnection:
        FOptionsList := Provider.GetConnectionOptions;
      otSQL:
        FOptionsList := Provider.GetSQLOptions;
      otDataSet:
        FOptionsList := Provider.GetDataSetOptions;
      otScript:
        FOptionsList := Provider.GetScriptOptions;
    {$IFNDEF STD}
      otLoader:
        FOptionsList := Provider.GetLoaderOptions;
      otDump:
        FOptionsList := Provider.GetDumpOptions;
      otAlerter:
        FOptionsList := Provider.GetAlerterOptions;
    {$ENDIF}
      otTransaction:
        FOptionsList := Provider.GetTransactionOptions;
    else
      Assert(False);
    end;

    OldModified := Modified;
    try
      for i := 0 to FOptionsList.Count - 1 do begin
        OptionValue := FSpecificOptions.Values[
          FProviderName + '.' + FOptionsList[i].OptionName];
        if OptionValue = '' then
          OptionValue := FOptionsList[i].GetAsString(FOptionsList[i].GetDefaultValue);

      {$IFDEF USE_VALEDIT}
        edOptions.InsertRow(FOptionsList[i].OptionName, OptionValue, True);
      {$ELSE}
        FEditorList.Add(FOptionsList[i].OptionName + '=' + OptionValue);
      {$ENDIF}
      end;
    finally
    {$IFNDEF USE_VALEDIT}
      edOptions.OnChange := edOptionsChanged;
    {$ENDIF}
      Modified := OldModified;
    end;
  end;
end;

procedure TUniSpecificOptionsFrame.InitOptions(SpecificOptions: TStrings);
begin
  FSpecificOptions := SpecificOptions;
end;

procedure TUniSpecificOptionsFrame.SaveOptions;
var
  i, j: integer;
  Value, Name: string;
  Option: TOption;
begin
  Assert(FSpecificOptions <> nil);

{$IFDEF FPC}
  FEditorList := edOptions.Lines;
  FEditorList.NameValueSeparator := '=';
{$ENDIF}
  if FOptionsList <> nil then
    for i := 0 to FOptionsList.Count - 1 do begin
      Option := FOptionsList[i];
      Name := FProviderName + '.' + Option.OptionName;
      Value := FEditorList.Values[Option.OptionName];
      if not CheckOptionDefault(Option, Value) then
        FSpecificOptions.Values[Name] := Value
      else begin
        j := FSpecificOptions.IndexOfName(Name);
        if j >= 0 then
          FSpecificOptions.Delete(j);
      end;
    end;
end;

{$IFDEF USE_VALEDIT}
procedure TUniSpecificOptionsFrame.edOptionsGetPickList(Sender: TObject;
  const KeyName: String; Values: TStrings);
var
  List: TStringList;
begin
  if FOptionsList <> nil then begin
    List := TStringList.Create;
    try
      GetOptionValuesList(KeyName, FOptionsList, List);
      AssignStrings(List, Values);
    finally
      List.Free;
    end;
  end;
end;

procedure TUniSpecificOptionsFrame.edOptionsValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: String);
var
  Option: TOption;
  OldValue: string;
  j: integer;
begin
  if FOptionsList <> nil then begin
    Option := FOptionsList.OptionByName(KeyName);
    if Option <> nil then // Delphi bug : event can be called with KeyName = ''
      try
        Option.Validate(KeyValue);
      except
        j := FSpecificOptions.IndexOfName(FProviderName + '.' + KeyName);
        if j >= 0 then
          OldValue := FSpecificOptions.Values[FProviderName + '.' + KeyName]
        else
          OldValue := Option.GetAsString(Option.GetDefaultValue);

        FEditorList.Values[KeyName] := OldValue;
        raise;
      end;
  end;
end;

procedure TUniSpecificOptionsFrame.edOptionsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Value, CellText: string;
  Option: TOption;
begin
  with edOptions do begin
    if (FOptionsList <> nil) and (ARow > 0) and (FOptionsList.Count >= ARow) and (ACol = 1) {and not (gdSelected in State)} then begin
      Option := FOptionsList[ARow - 1];
      Value := Trim(Cells[1, ARow]);

      if not CheckOptionDefault(Option, Value) then
        Canvas.Font.Style := [fsBold];
    end;

    // do not draw header
    if ARow > 0 then begin
      CellText := Cells[ACol, ARow];
      Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, CellText);
    end;
  end;
end;

procedure TUniSpecificOptionsFrame.edOptionsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
begin
  if ARow >= 0 then
    Modified := True;
end;

{$ENDIF}

procedure TUniSpecificOptionsFrame.edOptionsExit(Sender: TObject);
begin
  {$IFNDEF USE_VALEDIT}
    Modified := True;
  {$ENDIF}
end;

procedure TUniSpecificOptionsFrame.edOptionsChanged(Sender: TObject);
begin
  {$IFNDEF USE_VALEDIT}
    Modified := True;
  {$ENDIF}
end;

procedure TUniSpecificOptionsFrame.DoFinish;
begin
  if Modified then
    SaveOptions;

  inherited;
end;

procedure TUniSpecificOptionsFrame.edProviderChange(Sender: TObject);
begin
  SaveOptions;
  LoadOptions(edProvider.Text, FOptionsType);
end;

end.
