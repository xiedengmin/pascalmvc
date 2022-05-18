
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASESQLGeneratorUni;

interface

uses
  Classes, SysUtils,
  CRTypes, CRAccess, DASQLGenerator;

type
  TCustomASESQLGenerator = class(TDASQLGenerator)
  protected
    function ParamPrefix: Char; override;
    function IsSubstituteParamName: boolean; override;
    procedure GenerateLockSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const Index: integer = -1); override;
    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
  public
    class function GenerateAnsiNullSQL(const Value: Boolean): string;
    class function GenerateDatabaseSQL(const Value: string): string;
    class function GenerateIsolationLevelSQL(const Value: TCRIsolationLevel): string;
    class function GenerateQuotedIdentifierSQL(const Value: Boolean): string;
    class function GenerateTextSizeSQL(const Value: string): string;
  end;

implementation

uses
  DAConsts,
{$IFNDEF UNIDACPRO}
  ASEConnection;
{$ELSE}
  ASEConnectionUni;
{$ENDIF}

{ TCustomASESQLGenerator }

function TCustomASESQLGenerator.ParamPrefix: Char;
begin
{$IFDEF ODBC_PROVIDER}
  if not TASEConnection(GetIConnection).Direct then
    Result := ':'
  else
{$ENDIF}
  if inherited IsSubstituteParamName then
    Result := '@'
  else
    Result := ':';
end;

function TCustomASESQLGenerator.IsSubstituteParamName: boolean;
begin
{$IFDEF ODBC_PROVIDER}
  if not TASEConnection(GetIConnection).Direct then
    Result := inherited IsSubstituteParamName
  else
{$ENDIF}
    Result := False;
end;

procedure TCustomASESQLGenerator.GenerateLockSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const Index: integer = -1);

  procedure GenerateLockCondition;
  var
    AllFields: TKeyAndDataFields;
    i: integer;
  begin
    //LockSQL where clause should contains all fields to check that there are no changes in locked record
    if FDesignMode then  // Design-Time generation we should include key fields only (IS NULL issue)
      AllFields.DataFieldDescs := KeyAndDataFields.KeyFieldDescs
    else
      AllFields.DataFieldDescs := KeyAndDataFields.DataFieldDescs; //in Run-Time include all field (IS NULL issue)
    //Include ReadOnly Key Fields
    for i := 0 to High(KeyAndDataFields.KeyFieldDescs) do
      if KeyAndDataFields.KeyFieldDescs[i].ReadOnly then begin//This field is not included in DataFields
        SetLength(AllFields.DataFieldDescs, Length(AllFields.DataFieldDescs) + 1);
        AllFields.DataFieldDescs[High(AllFields.DataFieldDescs)] := KeyAndDataFields.KeyFieldDescs[i];
      end;
    SetLength(AllFields.KeyFieldDescs, 0);   //we should use DataFields to perform IsLargeDataTypeUsed check
    GenerateConditions(ParamsInfo, FCondSB, _stUpdate, AllFields);
  end;

var
  FieldDesc: TCRFieldDesc;
begin
  if High(KeyAndDataFields.DataFieldDescs) > 0 then begin
    FHeaderSB.Append('UPDATE ');
    FHeaderSB.Append(SQLInfo.NormalizeName(FTableInfo.TableNameFull, QuoteNames));
    FHeaderSB.Append(DALineSeparator + 'SET' + DALineSeparator + '  ');

    FieldDesc := TCRFieldDesc(KeyAndDataFields.DataFieldDescs[0]);

    FFldSB.Append(FieldDesc.ActualNameQuoted(SQLInfo, QuoteNames));
    FFldSB.Append(' = ');

//    case TDBAccessUtils.GetLockMode(TDASQLGeneratorService(FService).Dataset) of
//      lmPessimistic:
//        AddParam(ParamsInfo, FFldSB, FieldDesc, _stLock, pdUnknown, Index, False);
//      lmOptimistic:
//        AddParam(ParamsInfo, FFldSB, FieldDesc, _stLock, pdUnknown, Index, True);
//      else
//        Assert(False, 'Invalid lock mode');
//    end;

    AddParam(ParamsInfo, FFldSB, FieldDesc, _stLock, pdUnknown, Index, False);

    FMiddleSB.Append(DALineSeparator + 'WHERE' + DALineSeparator + '  ');
    GenerateLockCondition; // FCondSB
  end;
end;

procedure TCustomASESQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer = -1);
begin
  inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, False, Index);
  end;
end;

class function TCustomASESQLGenerator.GenerateAnsiNullSQL(const Value: Boolean): string;
begin
  if Value then
    Result := 'SET ANSINULL ON'
  else
    Result := 'SET ANSINULL OFF';
end;

class function TCustomASESQLGenerator.GenerateDatabaseSQL(const Value: string): string;
begin
  if Value = '' then
    Result := 'USE master' // master is default database
  else
    Result := 'USE ' + Value;
end;

class function TCustomASESQLGenerator.GenerateQuotedIdentifierSQL(const Value: Boolean): string;
begin
  if Value then
    Result := 'SET QUOTED_IDENTIFIER ON'
  else
    Result := 'SET QUOTED_IDENTIFIER OFF';
end;

class function TCustomASESQLGenerator.GenerateIsolationLevelSQL(const Value: TCRIsolationLevel): string;
begin
  {[read uncommitted | 0] |
  	[read committed | 1] | by default
	  [repeatable read | 2] |
	  [serializable | 3]}
  case Value of
    ilReadCommitted:
      Result := 'SET TRANSACTION ISOLATION LEVEL READ COMMITTED';
    ilReadUnCommitted:
      Result := 'SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED';
    ilRepeatableRead:
      Result := 'SET TRANSACTION ISOLATION LEVEL REPEATABLE READ';
    ilIsolated:
      Result := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    //ilSnapshot:
    //  Result := 'SET TRANSACTION ISOLATION LEVEL SNAPSHOT';
    else
      raise Exception.Create(SUnsupportedIsolationLevel);
  end;
end;

class function TCustomASESQLGenerator.GenerateTextSizeSQL(const Value: string): string;
begin
  Result := 'SET TEXTSIZE ' + Value;
end;

end.
