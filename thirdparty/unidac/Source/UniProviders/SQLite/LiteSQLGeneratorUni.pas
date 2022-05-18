
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I LiteDac.inc}
unit LiteSQLGeneratorUni;
{$ENDIF}

interface

uses
  CRAccess, DASQLGenerator;

type
  TCustomLiteSQLGenerator = class(TDASQLGenerator)
  private
    FLimit: integer;
    FOffset: integer;
  protected
    procedure GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
      const KeyAndDataFields: TKeyAndDataFields;
      const ModifiedFieldsOnly: boolean;
      const Index: integer = -1); override;
  public
    constructor Create(ServiceClass: TSQLGeneratorServiceClass); override;

    function GenerateRecCountSQL(UseBaseSQL: boolean = False): string; override;
    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
  {$IFNDEF LITE}
    function GenerateSmartFetchMetaInfoSQL: string; override;
  {$ENDIF}

    property Limit: integer write FLimit;
    property Offset: integer write FOffset;
  end;

implementation

uses
  SysUtils,
  {$IFNDEF UNIDACPRO}LiteClasses;{$ELSE}LiteClassesUni;{$ENDIF}

{ TCustomLiteSQLGenerator }

constructor TCustomLiteSQLGenerator.Create(ServiceClass: TSQLGeneratorServiceClass);
begin
  inherited;

  FLimit := -1;
  FOffset := 0;
end;

procedure TCustomLiteSQLGenerator.GenerateInsertSQL(ParamsInfo: TDAParamsInfo;
  const KeyAndDataFields: TKeyAndDataFields;
  const ModifiedFieldsOnly: boolean;
  const Index: integer);
begin
  inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, ModifiedFieldsOnly, Index);

  if FFldSB.Length = 0 then begin
    Clear;
    inherited GenerateInsertSQL(ParamsInfo, KeyAndDataFields, False, Index);
  end;
end;

function TCustomLiteSQLGenerator.GenerateRecCountSQL(UseBaseSQL: boolean = False): string;
var
  SelectSQL: string;
begin
  if UseBaseSQL then
    SelectSQL := FService.BaseSQL
  else
    SelectSQL := FService.FinalSQL;
  SelectSQL := SQLSetOrderBy(SelectSQL, '');

  Clear;
  FHeaderSB.Append('SELECT count(*) FROM (');
  FMiddleSB.Append(SelectSQL);
  FFooterSB.Append(')');
  Result := AssembleSB();
end;

function TCustomLiteSQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  QTableName: string;
begin
  QTableName := SQLiteInfo.NormalizeName(TableName, QuoteNames);

  Result := 'SELECT * FROM ' + QTableName;

  if OrderFields <> '' then
    Result := Result + ' ORDER BY ' + OrderFields;

  if (FLimit <> -1) or (FOffset <> 0) then
    Result := Result + ' LIMIT ' + IntToStr(FOffset) + ', ' + IntToStr(FLimit);
end;

{$IFNDEF LITE}
function TCustomLiteSQLGenerator.GenerateSmartFetchMetaInfoSQL: string;
begin
  Result := 'SELECT * FROM (' + FIRecordSet.OriginalSQL + ') LIMIT 1';
end;
{$ENDIF}

end.
