
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I Dac.inc}

unit DADump;

interface

uses
  Classes, SysUtils, Variants, DB, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CRTypes, CLRClasses, CRAccess, DBAccess, DAScript;

type
  TDADump = class;
  TDADumpProcessor = class;
  TDADumpProcessorClass = class of TDADumpProcessor;

  TDABackupProgressEvent = procedure (Sender: TObject; ObjectName: string; ObjectNum, ObjectCount, Percent: integer) of object;
  TDARestoreProgressEvent = procedure (Sender: TObject; Percent: integer) of object;

  TDADumpOptions = class(TPersistent)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TDADump;
    FGenerateHeader: boolean;
    FAddDrop: boolean;
    FQuoteNames: boolean;
    FCompleteInsert: boolean;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TDADump);
  published
    property GenerateHeader: boolean read FGenerateHeader write FGenerateHeader default True;
    property AddDrop: boolean read FAddDrop write FAddDrop default True;
    property QuoteNames: boolean read FQuoteNames write FQuoteNames default False;
    property CompleteInsert: boolean read FCompleteInsert write FCompleteInsert default False;
  end;

  TDADumpProcessor = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TDADump;
    FQuery: TCustomDADataSet;

    function GetConnection: TCustomDAConnection;
    function GetSQL: TStrings;
    function GetTables: TStringList;
    function GetTriggers: TStringList;
    function GetStoredProcs: TStringList;
    function GetStream: TStream;

    procedure Add(const Line: string); overload; virtual; // Line must be w/o #$D#$A
    procedure Add(const sl: TStringList); overload;
    procedure AddLineToSQL(const Line: string); overload; // Line may contains #$D#$A
    procedure AddLineToSQL(const Line: string; const Args: array of const); overload;

    procedure Backup(Query: string); virtual;
    procedure CheckTables(const QueryText: string); virtual;
    procedure AddHeader;
    function CreateQuery: TCustomDADataSet; virtual;
    function CreateDataQuery: TCustomDADataSet; virtual;
    procedure CheckQuery;
    procedure AddSettings; virtual;
    procedure RestoreSettings; virtual;
    procedure BackupObjects(const QueryText: string); virtual;
    procedure BackupData(const TableName, QueryText: string; TableNum, TableCount: integer); virtual;
    function GetFieldValueForDump(Field: TField): string; virtual;

    procedure DoBackupProgress(const ObjectName: string; ObjectNum, ObjectCount, Percent: integer);
    function SQLInfo: TSQLInfo; virtual;
    function QuoteName(const AName: string): string; virtual;
  public
    constructor Create(Owner: TDADump); virtual;
    destructor Destroy; override;

    function SetProp(Prop: integer; const Value: variant): boolean; virtual;
    function GetProp(Prop: integer; var Value: variant): boolean; virtual;
  end;

  TDADump = class(TComponent)
  protected
    FProcessor: TDADumpProcessor;
    FConnection: TCustomDAConnection;
    FSQL: TStrings;
    FStream: TStream;
    FOptions: TDADumpOptions;
    FDebug: boolean;
    FDesignCreate: boolean;
    FTables: TStringList;
    FTriggers: TStringList;
    FStoredProcs: TStringList;

    FOnBackupProgress: TDABackupProgressEvent;
    FOnRestoreProgress: TDARestoreProgressEvent;
    FOnError: TOnErrorEvent;

    function GetProcessorClass: TDADumpProcessorClass; virtual;
    procedure SetProcessor(Value: TDADumpProcessor); virtual;
    procedure CreateProcessor;
    procedure FreeProcessor;
    procedure CheckProcessor;

    procedure AssignTo(Dest: TPersistent); override;
    function GetTableNames: string; virtual;
    procedure SetTableNames(Value: string); virtual;

    function CreateOptions: TDADumpOptions; virtual;
    function CreateScript: TDAScript; virtual;

    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure SetConnection(Value: TCustomDAConnection);
    procedure BeginConnection; virtual;
    procedure EndConnection;

    procedure SetSQL(Value: TStrings);
    procedure SetOptions(Value: TDADumpOptions);

    procedure Loaded; override;

    procedure InternalBackup(const Query: string);
    function GenerateHeader: string; virtual; abstract;

    property Processor: TDADumpProcessor read FProcessor;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Backup;
    procedure BackupToStream(Stream: TStream; const Query: string = '');
    procedure BackupToFile(const FileName: string; const Query: string = '');
    procedure BackupQuery(const Query: string);
    procedure Restore;
    procedure RestoreFromStream(Stream: TStream);
    procedure RestoreFromFile(const FileName: string);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property Options: TDADumpOptions read FOptions write SetOptions;
  published
    property TableNames: string read GetTableNames write SetTableNames;
    property SQL: TStrings read FSQL write SetSQL;
    property Debug: boolean read FDebug write FDebug default False;

    property OnBackupProgress: TDABackupProgressEvent read FOnBackupProgress write FOnBackupProgress;
    property OnRestoreProgress: TDARestoreProgressEvent read FOnRestoreProgress write FOnRestoreProgress;
    property OnError: TOnErrorEvent read FOnError write FOnError;
  end;

  TDADumpUtils = class
  public
    class procedure SetDesignCreate(Obj: TDADump; Value: boolean);
    class function GetDesignCreate(Obj: TDADump): boolean;
  end;

implementation

uses
  CRProps, CRFunctions, DAConsts;

{ TDADumpOptions }

constructor TDADumpOptions.Create(Owner: TDADump);
begin
  inherited Create;

  FOwner := Owner;
  FGenerateHeader := True;
  FAddDrop := True;
  FQuoteNames := False;
end;

procedure TDADumpOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDADumpOptions then begin
    TDADumpOptions(Dest).GenerateHeader := GenerateHeader;
    TDADumpOptions(Dest).AddDrop := AddDrop;
    TDADumpOptions(Dest).QuoteNames := QuoteNames;
    TDADumpOptions(Dest).CompleteInsert := CompleteInsert;
  end
  else
    inherited;
end;

{ TDADumpProcessor }

constructor TDADumpProcessor.Create(Owner: TDADump);
begin
  inherited Create;

  FOwner := Owner;
end;

destructor TDADumpProcessor.Destroy;
begin
  FQuery.Free;

  inherited;
end;

function TDADumpProcessor.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TDADumpProcessor.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Assert(False, IntToStr(Prop));
  Result := False;
end;

function TDADumpProcessor.GetConnection: TCustomDAConnection;
begin
  Result := FOwner.FConnection;
end;

function TDADumpProcessor.GetSQL: TStrings;
begin
  Result := FOwner.FSQL;
end;

function TDADumpProcessor.GetTables: TStringList;
begin
  Result := FOwner.FTables;
end;

function TDADumpProcessor.GetTriggers: TStringList;
begin
  Result := FOwner.FTriggers;
end;

function TDADumpProcessor.GetStoredProcs: TStringList;
begin
  Result := FOwner.FStoredProcs;
end;

function TDADumpProcessor.GetStream: TStream;
begin
  Result := FOwner.FStream;
end;

procedure TDADumpProcessor.Add(const Line: string);  // Line must be w/o #$D#$A
var
  s: AnsiString;
begin
  if FOwner.FStream = nil then
    FOwner.FSQL.Add(Line)
  else
  begin
    s := AnsiString(Line + #$D#$A);
    FOwner.FStream.WriteBuffer(s[1]{$IFDEF NEXTGEN}^{$ENDIF}, LengthA(s));
  end;
end;

procedure TDADumpProcessor.Add(const sl: TStringList);
var
  i: integer;
begin
  for i := 0 to sl.Count - 1 do
    Add(sl[i]);
end;

procedure TDADumpProcessor.AddLineToSQL(const Line: string); // Line may contains #$D#$A
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Line;
    Add(sl);
  finally
    sl.Free;
  end;
end;

procedure TDADumpProcessor.AddLineToSQL(const Line: string; const Args: array of const);
begin
  AddLineToSQL(Format(Line, Args));
end;

procedure TDADumpProcessor.AddHeader;
begin
  if FOwner.Options.GenerateHeader then
    AddLineToSQL(FOwner.GenerateHeader);
end;

procedure TDADumpProcessor.Backup(Query: string);
begin
  Query := Trim(Query);
  CheckTables(Query);

  GetSQL.BeginUpdate;
  try
    GetSQL.Clear;
    AddHeader;
    AddSettings;

    BackupObjects(Query);

    RestoreSettings;
  finally
    GetSQL.EndUpdate;
  end;
end;

procedure TDADumpProcessor.CheckTables(const QueryText: string);
begin
  if (QueryText <> '') and (GetTables.Count > 1) then
    raise Exception.Create(SWrongTblCount);
end;

function TDADumpProcessor.CreateQuery: TCustomDADataSet;
begin
  Result := GetConnection.CreateDataSet;
  TDBAccessUtils.CheckConnection(Result);
  TDBAccessUtils.GetIRecordSet(Result).SetProp(prExtendedFieldsInfo, False);
end;

function TDADumpProcessor.CreateDataQuery: TCustomDADataSet;
begin
  Result := CreateQuery;
end;

procedure TDADumpProcessor.CheckQuery;
begin
  if FQuery = nil then
    FQuery := CreateQuery
  else
    FQuery.Connection := GetConnection;
end;

procedure TDADumpProcessor.AddSettings;
begin
end;

procedure TDADumpProcessor.RestoreSettings;
begin
end;

procedure TDADumpProcessor.BackupObjects(const QueryText: string);
var
  i: integer;
  TableName: string;
  TablesList: TStringList;
  ExactNames: boolean;
begin
  CheckQuery;
  if QueryText <> '' then
    BackupData('', QueryText, 1, 1)
  else begin
    TablesList := TStringList.Create;
    try
      if GetTables.Count = 0 then begin
        GetConnection.GetTableNames(TablesList, False, True);
        ExactNames := True;
      end
      else begin
        TablesList.Assign(GetTables);
        ExactNames := False;
      end;

      for i := 0 to TablesList.Count - 1 do begin
        if ExactNames then
          TableName := QuoteName(TablesList[i])
        else
          TableName := SQLInfo.NormalizeName(TablesList[i], FOwner.Options.QuoteNames);
        BackupData(TableName, QueryText, i + 1, TablesList.Count);
      end;
    finally
      TablesList.Free;
    end;
  end;
end;

procedure TDADumpProcessor.BackupData(const TableName, QueryText: string;
  TableNum, TableCount: integer);
var
  Query: TCustomDADataSet;
  i: integer;
  TblName, FieldList, Sql, Header: string;
  TablesInfo: TCRTablesInfo;
begin
  Query := CreateDataQuery;
  try
    if QueryText <> '' then
      Query.SQL.Text := QueryText
    else
      Query.SQL.Text := 'SELECT * FROM ' + TableName;
    Query.Open;

    if TableName = '' then begin
      TablesInfo := TDBAccessUtils.GetTablesInfo(Query);
      if TablesInfo.Count <> 1 then
        raise Exception.Create(SBackupQueryWrongTableCount);
      TblName := SQLInfo.NormalizeName(TablesInfo[0].TableName, FOwner.Options.QuoteNames);
    end
    else
      TblName := TableName;

    Add('');
    if FOwner.Options.AddDrop then
      Add('DELETE FROM ' + TblName + ';');

    if Query.RecordCount = 0 then
      exit;

    Header := 'INSERT INTO ' + TblName;
    if FOwner.Options.CompleteInsert or (QueryText <> '') then begin
      FieldList := '';
      for i := 0 to Query.Fields.Count - 1 do begin
        if i > 0 then
          FieldList := FieldList + ', ';
        FieldList := FieldList + QuoteName(Query.Fields[i].FieldName);
      end;
      Header := Header + '(' + FieldList + ')';
    end;
    Header := Header + ' VALUES (';

    while not Query.Eof do begin
      Sql := '';
      for i := 0 to Query.Fields.Count - 1 do begin
        if i > 0 then
          Sql := Sql + ', ';
        Sql := Sql + GetFieldValueForDump(Query.Fields[i]);
      end;
      Sql := Header + Sql + ');';
      Add(Sql);

      DoBackupProgress(TblName, TableNum, TableCount, Trunc((Query.RecNo / Query.RecordCount) * 100));

      Query.Next;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

function TDADumpProcessor.GetFieldValueForDump(Field: TField): string;
var
{$IFDEF VER7P}
  FmtSet: TFormatSettings;
{$ELSE}
  s: string;
{$ENDIF}
  i: integer;
  i64: int64;
  d: double;
  cur: currency;
  dt: TDateTime;
{$IFNDEF FPC}
  ts: TSQLTimeStamp;
{$ENDIF}
begin
  if Field.IsNull then
    Result := 'NULL'
  else begin
    case Field.DataType of
      {$IFDEF VER12P}ftByte,{$ENDIF} ftSmallint, ftWord, ftInteger: begin
        i := Field.AsInteger;
        Result := IntToStr(i);
      end;
      ftLargeint: begin
        i64 := TLargeintField(Field).AsLargeInt;
        Result := IntToStr(i64);
      end;
      {$IFDEF VER14P}ftSingle,{$ENDIF} ftFloat: begin
        d := Field.AsFloat;
      {$IFDEF VER7P}
        FmtSet.DecimalSeparator := '.';
        Result := FloatToStr(d, FmtSet);
      {$ELSE}
        Result := FloatToStr(d);
        if DecimalSeparator <> '.' then
          Result := StringReplace(Result, DecimalSeparator, '.', []);
      {$ENDIF}
      end;
      ftBCD, ftCurrency: begin
        cur := Field.AsCurrency;
      {$IFDEF VER7P}
        FmtSet.DecimalSeparator := '.';
        Result := CurrToStr(cur, FmtSet);
      {$ELSE}
        Result := CurrToStr(cur);
        if DecimalSeparator <> '.' then
          Result := StringReplace(Result, DecimalSeparator, '.', []);
      {$ENDIF}
      end;
      ftFMTBcd: begin
        Result := Field.AsString;
        if {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator <> '.' then
          Result := StringReplace(Result, {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '.', []);
      end;
      ftBoolean: begin
        Result := BoolToStr(Field.AsBoolean, False);
      end;
      ftDate: begin
        dt := Field.AsDateTime;
      {$IFDEF VER7P}
        FmtSet.DateSeparator := '-';
        Result := '''' + FormatDateTime('yyyy-mm-dd', dt, FmtSet) + '''';
      {$ELSE}
        Result := '''' + FormatDateTime('yyyy-mm-dd', dt) + '''';
        if DateSeparator <> '-' then
          Result := StringReplace(Result, DateSeparator, '-', [rfReplaceAll]);
      {$ENDIF}
      end;
      ftTime: begin
        dt := Field.AsDateTime;
      {$IFDEF VER7P}
        FmtSet.TimeSeparator := ':';
        Result := '''' + FormatDateTime('hh:nn:ss', dt, FmtSet) + '''';
      {$ELSE}
        Result := '''' + FormatDateTime('hh:nn:ss', dt) + '''';
        if TimeSeparator <> ':' then
          Result := StringReplace(Result, TimeSeparator, ':', [rfReplaceAll]);
      {$ENDIF}
      end;
      ftDateTime: begin
        dt := Field.AsDateTime;
      {$IFDEF VER7P}
        FmtSet.DateSeparator := '-';
        FmtSet.TimeSeparator := ':';
        Result := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', dt, FmtSet) + '''';
      {$ELSE}
        Result := FormatDateTime('yyyy-mm-dd', dt);
        if DateSeparator <> '-' then
          Result := StringReplace(Result, DateSeparator, '-', [rfReplaceAll]);

        s := FormatDateTime('hh:nn:ss', dt);
        if TimeSeparator <> ':' then
          s := StringReplace(s, TimeSeparator, ':', [rfReplaceAll]);

        Result := '''' + Result + ' ' + s + '''';
      {$ENDIF}
      end;
    {$IFNDEF FPC}
      ftTimeStamp: begin
        ts := Field.AsSQLTimeStamp;
        if ts.Fractions = 0 then
          Result := '''' + SQLTimeStampToStr('yyyy-mm-dd hh:nn:ss', ts) + ''''
        else
          Result := '''' + SQLTimeStampToStr('yyyy-mm-dd hh:nn:ss.z', ts) + '''';
      end;
    {$ENDIF}
    else
      Result := AnsiQuotedStr(VarToStr(Field.Value), '''');
    end;
  end;
end;

procedure TDADumpProcessor.DoBackupProgress(const ObjectName: string; ObjectNum, ObjectCount, Percent: integer);
begin
  if Assigned(FOwner.FOnBackupProgress) then
    FOwner.FOnBackupProgress(FOwner, ObjectName, ObjectNum, ObjectCount, Percent);
end;

function TDADumpProcessor.SQLInfo: TSQLInfo;
begin
  Assert(FQuery <> nil);
  Result := TDBAccessUtils.GetICommand(FQuery).SQLInfo;
end;

function TDADumpProcessor.QuoteName(const AName: string): string;
var
  SQLInfo: TSQLInfo;
begin
  if AName = '' then
    Result := AName
  else begin
    SQLInfo := Self.SQLInfo;
    if FOwner.Options.QuoteNames or SQLInfo.QuotesNeeded(AName) then
      Result := SQLInfo.Quote(AName)
    else
      Result := AName;
  end;
end;

{ TDADump }

constructor TDADump.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FDesignCreate := csDesigning in ComponentState;
  FOptions := CreateOptions;
  FSQL := TStringList.Create;
  FTables := TStringList.Create;
  FTriggers := TStringList.Create;
  FStoredProcs := TStringList.Create;
end;

destructor TDADump.Destroy;
begin
  FStoredProcs.Free;
  FTriggers.Free;
  FTables.Free;
  FSQL.Free;
  FOptions.Free;
  FProcessor.Free;

  inherited;
end;

function TDADump.GetProcessorClass: TDADumpProcessorClass;
begin
  Assert(False);
  Result := TDADumpProcessor;
end;

procedure TDADump.SetProcessor(Value: TDADumpProcessor);
begin
  if Value <> FProcessor then begin
    FreeProcessor;

    FProcessor := Value;
  end;
end;

procedure TDADump.CheckProcessor;
begin
  if not (FProcessor is GetProcessorClass) then begin
    FreeProcessor;
    CreateProcessor;
  end;
end;

procedure TDADump.CreateProcessor;
begin
  SetProcessor(GetProcessorClass.Create(Self));
end;

procedure TDADump.FreeProcessor;
begin
  FProcessor.Free;
  FProcessor := nil;
end;

procedure TDADump.AssignTo(Dest: TPersistent);
begin
  if Dest is TDADump then begin
    TDADump(Dest).TableNames := TableNames;
    TDADump(Dest).Connection := Connection;
    TDADump(Dest).SQL.Text := SQL.Text;
    TDADump(Dest).Debug := Debug;
    TDADump(Dest).Options := Options;
  end
  else
    inherited;
end;

function TDADump.GetTableNames: string;
begin
  Result := '';
  Assert(False);
end;

procedure TDADump.SetTableNames(Value: string);
begin
  Assert(False);
end;

function TDADump.CreateOptions: TDADumpOptions;
begin
  Result := TDADumpOptions.Create(Self);
end;

function TDADump.CreateScript: TDAScript;
begin
  Result := TDAScript.Create(nil);
end;

procedure TDADump.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FConnection) and (Operation = opRemove) then
    Connection := nil;

  inherited;
end;

procedure TDADump.SetConnection(Value: TCustomDAConnection);
begin
  if Value <> FConnection then begin
    if FConnection <> nil then
      FConnection.RemoveFreeNotification(Self);

    FConnection := Value;

    if FConnection <> nil then
      FConnection.FreeNotification(Self);
  end;
end;

procedure TDADump.BeginConnection;
begin
  if FConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  TDBAccessUtils.InternalConnect(FConnection);
end;

procedure TDADump.EndConnection;
begin
  TDBAccessUtils.InternalDisconnect(FConnection);
end;

procedure TDADump.SetSQL(Value: TStrings);
begin
  if FSQL.Text <> Value.Text then begin
    FSQL.BeginUpdate;
    try
      FSQL.Assign(Value);
    finally
      FSQL.EndUpdate;
    end;
  end;
end;

procedure TDADump.SetOptions(Value: TDADumpOptions);
begin
  FOptions.Assign(Value);
end;

procedure TDADump.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDADump.InternalBackup(const Query: string);
begin
  BeginConnection;
  try
    CheckProcessor;
    FProcessor.Backup(Query);
  finally
    EndConnection;
  end;
end;

procedure TDADump.Backup;
begin
  InternalBackup('');
end;

procedure TDADump.BackupToStream(Stream: TStream; const Query: string = '');
begin
  FStream := Stream;
  try
    InternalBackup(Query);
  finally
    FStream := nil;
  end;
end;

procedure TDADump.BackupToFile(const FileName: string; const Query: string = '');
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    BackupToStream(Stream, Query);
  finally
    Stream.Free;
  end;
end;

procedure TDADump.BackupQuery(const Query: string);
var
  OldTables: TStringList;
begin
  if Trim(Query) = '' then
    raise EDatabaseError.Create(SEmptySQLStatement);

  OldTables := TStringList.Create;
  OldTables.Assign(FTables);
  FTables.Clear;
  try
    InternalBackup(Query);
  finally
    FTables.Assign(OldTables);
    OldTables.Free;
  end;
end;

procedure TDADump.Restore;
var
  Script: TDAScript;
  Len: integer;
begin
  Script := CreateScript;
  try
    Script.Connection := Connection;
    Script.Debug := Debug;
    Script.SQL := SQL;
    Len := Length(SQL.Text);
    Script.OnError := OnError; 

    while Script.ExecuteNext do
      if Assigned(FOnRestoreProgress) and (Len > 0) then
        FOnRestoreProgress(Self, Trunc((Script.StartPos / Len) * 100{"*" after "/" to prevent IntOverflow}));
  finally
    Script.Free;
  end;
end;

procedure TDADump.RestoreFromStream(Stream: TStream);
var
  Script: TDAScript;
  TotalCount: Int64;
  percent: cardinal;
begin
  Script := CreateScript;
  try
    percent := 0;
    Script.Connection := Connection;
    Script.Debug := Debug;
    Script.OnError := OnError;
    TDAScriptUtils.Open(Script, Stream);

    Script.CheckProcessor;
    Script.Processor.Init(Stream);
    if Script.Processor.GetEncoding = Encoding.Unicode then
      TotalCount := (Stream.Size - Stream.Position) shr 1
    else
      TotalCount := Stream.Size - Stream.Position;

    try
      while Script.ExecuteNext do
        if Assigned(FOnRestoreProgress) and (TotalCount > 0) then begin
          percent := Trunc((Script.StartPos / TotalCount) * 100);
          FOnRestoreProgress(Self, percent{"*" after "/" to prevent IntOverflow});
        end;
      //To retrieve 100% on data restore
      if Assigned(FOnRestoreProgress) and (TotalCount > 0) and (percent <> 100) and (Script.StartPos = TotalCount) then
        FOnRestoreProgress(Self, 100);
    finally
      TDAScriptUtils.Close(Script);
    end;
  finally
    Script.Free;
  end;
end;

procedure TDADump.RestoreFromFile(const FileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    RestoreFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

{ TDADumpUtils }

class procedure TDADumpUtils.SetDesignCreate(Obj: TDADump; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDADumpUtils.GetDesignCreate(Obj: TDADump): boolean;
begin
  Result := Obj.FDesignCreate;
end;

end.
