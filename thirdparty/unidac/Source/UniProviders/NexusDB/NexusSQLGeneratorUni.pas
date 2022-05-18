{$IFNDEF CLR}
{$I NexusDac.inc}
unit NexusSQLGeneratorUni;
{$ENDIF}

interface

uses
  Classes,
  CLRClasses, CRTypes, CRAccess, MemData, DASQLGenerator;

type
{ TCustomNexusSQLGenerator }

  TCustomNexusSQLGenerator = class(TDASQLGenerator)
  protected
    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
  end;

implementation

{ TCustomNexusSQLGenerator }

procedure TCustomNexusSQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);
begin
// Lock can be executed only by nxTable.Lock. LOCK SQL doesn't exist
end;

procedure TCustomNexusSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean; const Index: integer);
begin
  inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, False, Index);
  end;
end;

end.
