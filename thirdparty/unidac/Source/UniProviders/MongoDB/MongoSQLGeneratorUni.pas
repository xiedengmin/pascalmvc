
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoSQLGeneratorUni;

interface

uses
  SysUtils,
  CRTypes, CRAccess,
  DASQLGenerator;

type
  TMongoSQLGenerator = class(TDASQLGenerator)
  public
    function GenerateSQL(ParamsInfo: TDAParamsInfo; const StatementType: _TStatementType; const ModifiedFieldsOnly: boolean;
      const Index: Integer = -1): string; override;
    function GenerateTableSQL(const TableName, OrderFields: string): string; override;
  end;

implementation

uses
  CLRClasses,
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoClasses, MongoObjects,
{$ELSE}
  MongoConstsUni, MongoClassesUni, MongoObjectsUni,
{$ENDIF}
  MemUtils;

{ TMongoSQLGenerator }

function TMongoSQLGenerator.GenerateSQL(ParamsInfo: TDAParamsInfo; const StatementType: _TStatementType; const ModifiedFieldsOnly: boolean;
  const Index: Integer): string;
var
  Document: TMongoDocument;
  RecBuf: IntPtr;
  Field: IDocumentExpression;
begin
  Assert(FIRecordSet <> nil);
  Result := '';

{$IFNDEF NOSQL}
  if FIRecordSet is TMongoVirtualRecordset then
    Result := inherited GenerateSQL(ParamsInfo, StatementType, ModifiedFieldsOnly)
  else
{$ENDIF}
  if StatementType = _stRefresh then begin
    RecBuf := FService.GetNewRecBuf;
    Document := TMongoDocument(GetGCHandleTarget(Marshal.ReadIntPtr(RecBuf)));
    Field := Document['_id'];
    if Field <> nil then
      Result := '{"find":"' + TMongoCommand(FIRecordSet.GetCommand).CollectionName + '", "filter":{"_id":{"$oid":"' + Field.Value +'"}}}'
    else
      raise Exception.CreateFmt(SFieldNotFound, ['_id']);
  end;
end;

function TMongoSQLGenerator.GenerateTableSQL(const TableName, OrderFields: string): string;
var
  AOrderFields, Field: string;
  i, n: integer;
begin
  Result := '{"find":"' + TableName + '", "filter":{}';

  AOrderFields := Trim(OrderFields);
  if AOrderFields <> '' then begin
    Result := Result + ', "sort":{';
    i := 0;

    while True do begin
      if AOrderFields = '' then
        Break;

      n := Pos(',', AOrderFields);
      if n > 0 then begin
        Field := Trim(Copy(AOrderFields, 1, n - 1));
        Delete(AOrderFields, 1, n);
      end
      else begin
        Field := AOrderFields;
        AOrderFields := '';
      end;

      if Field <> '' then begin
        if i > 0 then
          Result := Result + ', ';
        Result := Result + '"' + Field + '":1';
      end
      else
        Break;
      Inc(i);
    end;

    Result := Result + '}';
  end;

  Result := Result + '}';
end;

end.
