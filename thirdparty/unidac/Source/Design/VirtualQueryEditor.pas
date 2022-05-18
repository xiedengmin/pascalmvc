
//////////////////////////////////////////////////
//  Virtual Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}
{$I VirtualQuery.inc}
unit VirtualQueryEditor;

interface

uses
  SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons,
  DB, DBCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  CRAccess, DBAccess,
  CRTabEditor,
  DASQLComponentEditor, DASQLFrame, DAParamsFrame, DAMacrosFrame,
  DASQLGeneratorFrame, DAUpdateSQLFrame, DAQueryEditor;

type
  TVirtualSQLGeneratorFrame = class(TDASQLGeneratorFrame)
  protected
    function GenerateSQLforUpdTable(TableInfo: TCRTableInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const StatementType: TStatementType;
      const ModifiedFieldsOnly: boolean): string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualUpdateSQLFrame = class(TDAUpdateSQLFrame)
  protected
    procedure DoActivate; override;
  end;

  TVirtualParamsFrame = class(TDAParamsFrame)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualQueryEditorForm = class(TDAQueryEditorForm)
  protected
    procedure DoInit; override;
  public
    property Query;
  end;

implementation

{$IFNDEF FPC}
{$R VirtualQueryEditor.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  DADataTypeMapFrame, DAConditionsFrame;

{ TVirtualSQLGeneratorFrame }

constructor TVirtualSQLGeneratorFrame.Create(AOwner: TComponent);
begin
  inherited;

  cbLock.Visible := False;
  cbRecCount.Top := cbRefresh.Top;
  cbRefresh.Top := cbLock.Top;
end;

function TVirtualSQLGeneratorFrame.GenerateSQLforUpdTable(TableInfo: TCRTableInfo; const KeyAndDataFields: TKeyAndDataFields;
  const StatementType: TStatementType; const ModifiedFieldsOnly: boolean): string;
begin
  if StatementType = stLock then
    Result := ''
  else
    Result := inherited GenerateSQLforUpdTable(TableInfo, KeyAndDataFields, StatementType, ModifiedFieldsOnly);
end;

{ TVirtualUpdateSQLFrame }

procedure TVirtualUpdateSQLFrame.DoActivate;
var
  i, n, l, ll: integer;
  rb: TRadioButton;
begin
  inherited;

  n := -1;
  l := 0;
  for i := 0 to gbStatementType.ControlCount - 1 do
    if gbStatementType.Controls[i] is TRadioButton then begin
      rb := TRadioButton(gbStatementType.Controls[i]);

      if rb.Caption = 'Lock' then begin
        if not rb.Visible then
          Break;

        rb.Visible := False;
        n := i;
        l := rb.Left;
      end
      else if (n <> -1) and (i > n) then begin
        ll := rb.Left;
        rb.Left := l;
        l := ll;
      end;
    end;
end;

{ TLiteParamsFrame }

constructor TVirtualParamsFrame.Create(AOwner: TComponent);
begin
  inherited;

  AddDataType('Unknown',    ftUnknown,    True,  False, False, '');
  AddDataType('String',     ftString,     False, True,  True,  '');
  AddDataType('WideString', ftWideString, False, True,  True,  '');
  AddDataType('Smallint',   ftSmallint,   True,  True,  False, '0');
  AddDataType('Integer',    ftInteger,    True,  True,  False, '0');
  AddDataType('LargeInt',   ftLargeInt,   True,  True,  False, '0');
  AddDataType('Float',      ftFloat,      True,  True,  False, '0');
  AddDataType('BCD',        ftBCD,        True,  True,  False, '0');
{$IFDEF VER6P}
  AddDataType('FMTBcd',     ftFMTBcd,     True,  True,  False, '0');
{$ENDIF}
  AddDataType('Date',       ftDate,       True,  True,  False, '');
  AddDataType('Time',       ftTime,       True,  True,  False, '');
  AddDataType('DateTime',   ftDateTime,   True,  True,  False, '');
  AddDataType('Blob',       ftBlob,       False, True,  False, '');
  AddDataType('Memo',       ftMemo,       False, True,  False, '');
{$IFDEF VER10P}
  AddDataType('WideMemo',   ftWideMemo,   False, True,  False, '');
{$ENDIF}
  AddDataType('Boolean',    ftBoolean,    False, True,  True,  'False');

  AddParamType('Unknown',   ptUnknown);
  AddParamType('IN',        ptInput);
  AddParamType('OUT',       ptOutput);
end;

{ TVirtualQueryEditorForm }

procedure TVirtualQueryEditorForm.DoInit;
begin
  FSQLFrame := AddTab(TDASQLFrame, shSQL) as TDASQLFrame;
  FParamsFrame := AddTab(TVirtualParamsFrame, shParameters) as TDAParamsFrame;
  FMacrosFrame := AddTab(TDAMacrosFrame, shMacros) as TDAMacrosFrame;
  FUpdateSQLFrame := AddTab(TVirtualUpdateSQLFrame, shEditSQL) as TVirtualUpdateSQLFrame;
  FSQLGeneratorFrame := AddTab(TVirtualSQLGeneratorFrame, shGenerator) as TVirtualSQLGeneratorFrame;
  FDataTypeMapFrame := AddTab(GetDataTypeMapFrameClass, shDataTypeMap) as TDADataTypeMapFrame;
  FConditionsFrame := AddTab(TDAConditionsFrame, shConditions) as  TDAConditionsFrame;
  
  try
    // to prevent FSPCallFrame assertion failure in the TDASQLEditorForm.OnInit
    inherited;
  except
  end;
  
  Modified := False;
  shGeneratorSPC.TabVisible := false;
end;

end.

