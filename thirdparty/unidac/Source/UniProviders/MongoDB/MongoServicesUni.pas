
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoServicesUni;

interface

uses
  SysUtils, DB,
  {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CRTypes, CRAccess, CRFunctions, CRParser,
  DBAccess, DAScript, DADump;

type
  TMongoDataSetUpdater = class(TDADataSetUpdater)
  protected
    function PerformRefreshRecord: boolean; override;
  end;

  TMongoDataSetService = class(TDADataSetService)
  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;

    procedure InitFieldsOptions; override;
    procedure UpdateFieldsOptions; override;
    function DetectCanModify: boolean; override;

    function GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass; override;
  public
    constructor Create(AOwner: TMemDataSet); override;

    function GetDBKeyList(const TableName, IndexName: string): string; override;
  end;

  TMongoParser = class(TSQLParser);

  TMongoScriptProcessor = class (TDAScriptProcessor)
  protected
    function GetParserClass: TSQLParserClass; override;
  end;

  TMongoDumpProcessor = class(TDADumpProcessor)
  protected
    function CreateQuery: TCustomDADataSet; override;
  end;

implementation

uses
  CLRClasses,
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoClasses, MongoObjects, MongoSQLGenerator,
{$ELSE}
  MongoConstsUni, MongoClassesUni, MongoObjectsUni, MongoSQLGeneratorUni,
{$ENDIF}
  MemUtils;

type
  TInternalDataSet = class(TCustomDADataSet);

{ TMongoDataSetUpdater }

function TMongoDataSetUpdater.PerformRefreshRecord: boolean;
begin
  TInternalDataSet(FDataSet).FNeedAddRef := True;
  // to prevent freeing the document in PerformSQL.CopyRecBuf
  try
    TInternalDataSet(FDataSet).AddRefComplexFields(IntPtr(FDataSet.ActiveBuffer));
    TInternalDataSet(FDataSet).AddRefComplexFields(IntPtr(FDataSet.ActiveBuffer));
    TInternalDataSet(FDataSet).FNeedAddRef := False;

    Result := inherited PerformRefreshRecord;
  finally
    TInternalDataSet(FDataSet).FreeRefComplexFields(IntPtr(FDataSet.ActiveBuffer));
    TInternalDataSet(FDataSet).FNeedAddRef := False;
  end;
end;

{ TMongoDataSetService }

constructor TMongoDataSetService.Create(AOwner: TMemDataSet);
begin
  inherited;
end;

procedure TMongoDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TMongoDataSetUpdater.Create(Self));
end;

procedure TMongoDataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TMongoSQLGenerator.Create(TDASQLGeneratorService));
end;

procedure TMongoDataSetService.InitFieldsOptions;
begin
  UpdateFieldsOptions;

  if FDataSet.Options.DefaultValues then
    FillFieldsDefaultValues;
end;

procedure TMongoDataSetService.UpdateFieldsOptions;
var
  i: integer;
begin
  FIsAnyFieldCanBeModified := True;
  for i := 0 to FDataSet.Fields.Count - 1 do
    if FDataSet.Fields[i].ReadOnly then begin
      FIsAnyFieldCanBeModified := False;
      Break;
    end;
end;

function TMongoDataSetService.DetectCanModify: boolean;
begin
  Result := inherited DetectCanModify or
    not FDataSet.ReadOnly and FIsAnyFieldCanBeModified;
end;

function TMongoDataSetService.GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldType, DataType);
end;

function TMongoDataSetService.GetDBKeyList(const TableName, IndexName: string): string;
begin
  Result := '_id';
end;

{ TLiteScriptProcessor }

function TMongoScriptProcessor.GetParserClass: TSQLParserClass;
begin
  Result := TMongoParser;
end;

{ TCustomLiteDumpProcessor }

function TMongoDumpProcessor.CreateQuery: TCustomDADataSet;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise Exception.Create(SFeatureNotSupported);
end;

end.
