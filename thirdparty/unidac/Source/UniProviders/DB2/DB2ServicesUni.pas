
//////////////////////////////////////////////////
//  DB2 Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////


{$I DB2Dac.inc}
unit DB2ServicesUni;

interface

uses
  SysUtils, Classes, Variants, DB,
  CRTypes, MemData, {$IFNDEF FPC}MemDS{$ELSE}MemDataSet{$ENDIF},
  CLRClasses, CRParser, CRAccess, CRServerEnumerator, DBAccess, DAScript, DADump, DASQLGenerator,
{$IFNDEF UNIDACPRO}
  ODBCClasses, ODBCParser, ODBCCall, ODBCServices, DB2Classes;
{$ELSE}
  ODBCClassesUni, ODBCParserUni, ODBCCallUni, ODBCServicesUni, DB2ClassesUni;
{$ENDIF}

const
  prSequenceMode = 101; // integer

type
  TDB2SequenceMode = (smInsert, smPost);

  TCustomDB2DataSetService = class;

{ TCustomDB2SQLGenerator }

  TCustomDB2SQLGenerator = class(TDASQLGenerator)
  protected
    function SQLInfo: TSQLInfo;
    function GenerateIndexName(const Name: string): string; override;
  public
    function DecodeFieldIndex(const FieldName: string): integer; override;
    function GenerateSelectValues(const ValuesList: string): string; override;
  end;

{  TCustomDB2DataSetUpdater }

  TCustomDB2DataSetUpdater = class(TCustomODBCDataSetUpdater)
  private
    procedure GetSequenceNextVal;

  protected
    FDataSetService: TCustomDB2DataSetService;

    function IsNeedInsertPreconnect: boolean; override;
    procedure PrepareAppend; override;
    function PerformAppend: boolean; override;
    function GetIdentityFieldValue(var Value: variant): boolean; override;

  public
    constructor Create(AOwner: TDataSetService); override;
  end;

{ TCustomDB2DataSetService }

  TCustomDB2DataSetService = class(TCustomODBCDataSetService)
  private
    FSequenceMode: TDB2SequenceMode;

  protected
    procedure CreateDataSetUpdater; override;
    procedure CreateSQLGenerator; override;
    function GetCurrentSchema: string; override;

  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

{ TDB2ScriptProcessor }

  TDB2ScriptProcessor = class (TODBCScriptProcessor)
  protected
    procedure CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean); override;
  end;

{ TDB2ServerEnumerator }

  TDB2ServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: TStrings); override;
  end;

implementation

uses
  DAConsts;

{ TCustomDB2SQLGenerator }

function TCustomDB2SQLGenerator.SQLInfo: TSQLInfo;
begin
  Result := inherited SQLInfo;
end;

function TCustomDB2SQLGenerator.GenerateSelectValues(const ValuesList: string): string;
begin
  Result := 'VALUES (' + ValuesList + ')';
end;

function TCustomDB2SQLGenerator.GenerateIndexName(const Name: string): string;
begin
  Result := 'F__' + Name;
end;

function TCustomDB2SQLGenerator.DecodeFieldIndex(const FieldName: string): integer;
var
  e: integer;
begin
  Result := -1;
  if (Length(FieldName) >= 4) and (Copy(FieldName, 1, 3) = 'F__') then begin
    Val(Copy(FieldName, 4, MaxInt), Result, e);
    if e <> 0 then
      Result := -1;
  end;
end;

{ TCustomDB2DataSetUpdater }

constructor TCustomDB2DataSetUpdater.Create(AOwner: TDataSetService);
begin
  FDataSetService := TCustomDB2DataSetService(AOwner);

  inherited Create(AOwner);
end;

function TCustomDB2DataSetUpdater.IsNeedInsertPreconnect: boolean;
begin
  Result := (FDataSetService.FSequenceMode = smInsert);
end;

procedure TCustomDB2DataSetUpdater.PrepareAppend;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FSequenceMode = smInsert) then
    GetSequenceNextVal;
end;

function TCustomDB2DataSetUpdater.PerformAppend: boolean;
begin
  if (FDataSetService.KeyGeneratorField <> nil) and (FDataSetService.FSequenceMode = smPost) then
    GetSequenceNextVal;

  Result := inherited PerformAppend;
end;

function TCustomDB2DataSetUpdater.GetIdentityFieldValue(var Value: variant): boolean;
begin
  Value := SelectDBValue('Get identity value', 'VALUES (IDENTITY_VAL_LOCAL())');
  Result := True;
end;

procedure TCustomDB2DataSetUpdater.GetSequenceNextVal;
var
  Value: variant;
begin
  Value := SelectDBValue('Get sequence value',
    'VALUES (NEXT VALUE FOR ' + FDataSetService.SQLGenerator.KeySequence + ')');
  FDataSetService.SetKeyGeneratorValue(Value);
end;

{ TCustomDB2DataSetService }

function TCustomDB2DataSetService.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prSequenceMode:
      FSequenceMode := TDB2SequenceMode(Value);
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

procedure TCustomDB2DataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TCustomDB2DataSetUpdater.Create(Self));
end;

procedure TCustomDB2DataSetService.CreateSQLGenerator;
begin
  SetSQLGenerator(TCustomDB2SQLGenerator.Create(TDASQLGeneratorService));
end;

function TCustomDB2DataSetService.GetCurrentSchema: string;
begin
  Result := TDB2Connection(TDBAccessUtils.GetIConnection(UsedConnection)).GetCachedSchema;
  // to preserve character case
  Result := '"' + Result + '"';
end;

{ TDB2ScriptProcessor }

procedure TDB2ScriptProcessor.CheckLexem(Code: Integer; var StatementType: integer; var Omit: boolean);
begin
  if Code = lxBEGIN then
    StatementType := ST_SPECIFIC_SQL;
end;

{ TDB2ServerEnumerator }

procedure TDB2ServerEnumerator.GetServerList(List: TStrings);
begin
  List.Clear;
end;

end.
