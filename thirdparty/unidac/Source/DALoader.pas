
//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  TDALoader
//////////////////////////////////////////////////

{$I Dac.inc}

unit DALoader;

interface

uses
  Classes, SysUtils, DB, Variants,
{$IFDEF VER26P}
  Generics.Collections,
{$ENDIF}
  MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF},
  CRTypes, CRAccess, DBAccess;

type
  TDAColumnDataType = (ctString, ctDate, ctInteger, ctUInteger, ctFloat);

  TDAColumnClass = class of TDAColumn;

  TDAColumn = class (TCollectionItem)
  private
    FName: string;
    FFieldType: TFieldType;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDataType: TDAColumnDataType; virtual;
    procedure SetDataType(Value: TDAColumnDataType); virtual;
    procedure SetFieldType(Value: TFieldType); virtual;
    function GetDisplayName: string; override;
    property DataType: TDAColumnDataType read GetDataType write SetDataType;

  public
    constructor Create(Collection: TCollection); override;

  published
    property Name: string read FName write FName;
    property FieldType: TFieldType read FFieldType write SetFieldType default ftString;
  end;

  TDAColumns = class (TOwnedCollection)
  private
    FInCreation: boolean;
    FChangedByUser: boolean;
    FCreatedFromFields: boolean;
    function GetColumn(Index: integer): TDAColumn;
    procedure SetColumn(Index: integer; Value: TDAColumn);

  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    property ChangedByUser: boolean read FChangedByUser;

  public
    property Items[Index: integer]: TDAColumn read GetColumn write SetColumn; default;
  end;

  TDALoader = class;

  TDAPutDataEvent = procedure (Sender: TDALoader) of object;
  TGetColumnDataEvent = procedure (Sender: TObject; Column: TDAColumn; Row: integer;
    var Value: variant; var IsEOF: boolean) of object;
  TLoaderProgressEvent = procedure (Sender: TObject; Percent: integer) of object;

  TDALoaderOptions = class(TPersistent)
  private
    FQuoteNames: boolean;
    FUseBlankValues: boolean;

    procedure SetQuoteNames(Value: boolean);
    procedure SetUseBlankValues(Value: boolean);
  protected
  {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TDALoader;

    procedure AssignTo(Dest: TPersistent); override;
    function GetInternalLoader: TCRLoader;
  public
    constructor Create(Owner: TDALoader); virtual;

    property QuoteNames: boolean read FQuoteNames write SetQuoteNames default False;
    property UseBlankValues: boolean read FUseBlankValues write SetUseBlankValues default True;
  end;

  TDALoader = class (TComponent)
  private
    FTableName: string;

    FOnPutData: TDAPutDataEvent;
    FOnGetColumnData: TGetColumnDataEvent;

    FOptions: TDALoaderOptions;

    procedure SetConnection(Value: TCustomDAConnection);
    procedure SetColumns(Value: TDAColumns);

    function IsColumnsStored: boolean;
    procedure CreateColumnsByFields(Fields: TFields);
    procedure SetOptions(const Value: TDALoaderOptions);
  protected
    FILoader: TCRLoader;
    FColumns: TDAColumns;
    FConnection: TCustomDAConnection;
    FTransaction: TDATransaction;
    FAutoCommit: boolean;
    FDesignCreate: boolean;
    FOnLoaderProgress: TLoaderProgressEvent;

    function GetInternalLoaderClass: TCRLoaderClass; virtual;
    procedure SetInternalLoader(Value: TCRLoader); virtual;
    procedure CreateInternalLoader;
    procedure FreeInternalLoader;
    procedure CheckInternalLoader;

    procedure DoLoaderProgress(Percent: integer);
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure BeginConnection; virtual;
    procedure EndConnection; virtual;
    procedure CommitData; virtual;

    procedure InternalPutData; virtual;
    procedure PutData; virtual;
    procedure SetTableName(const Value: string);

    class function GetColumnClass: TDAColumnClass; virtual;
    procedure CreateColumnByField(Field: TField; Column: TDAColumn); virtual;
    function GetFieldTypeMapClass: TFieldTypeMapClass; virtual;

    function IsTransactionStored: boolean;
    function GetTransaction: TDATransaction; virtual;
    procedure SetTransaction(Value: TDATransaction); virtual;
    function UsedConnection: TCustomDAConnection; virtual;
    function UsedTransaction: TDATransaction; virtual;

    function  NeedRecreateColumns: boolean; virtual;
    procedure ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); virtual;
    procedure WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); virtual;
    procedure ClearColumns;
    procedure ReadColumns;
    procedure WriteColumns;

    function CreateOptions: TDALoaderOptions; virtual;

    property Transaction: TDATransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property AutoCommit: boolean read FAutoCommit write FAutoCommit default True;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure PutColumnData(Col: integer; Row: integer; const Value: variant); overload; virtual;
    procedure PutColumnData(const ColName: string; Row: integer; const Value: variant); overload;

    procedure Load; virtual;

    procedure CreateColumns;
    procedure LoadFromDataSet(DataSet: TDataSet);

    property Connection: TCustomDAConnection read FConnection write SetConnection;
    property TableName: string read FTableName write SetTableName;
    property Columns: TDAColumns read FColumns write SetColumns stored IsColumnsStored;

    property OnPutData: TDAPutDataEvent read FOnPutData write FOnPutData;
    property OnGetColumnData: TGetColumnDataEvent read FOnGetColumnData write FOnGetColumnData;
    property OnProgress: TLoaderProgressEvent read FOnLoaderProgress write FOnLoaderProgress;
    property Options: TDALoaderOptions read FOptions write SetOptions;
  end;

  TDALoaderUtils = class
  public
    class procedure SetDesignCreate(Obj: TDALoader; Value: boolean);
    class function GetDesignCreate(Obj: TDALoader): boolean;
    class function UsedConnection(Obj: TDALoader): TCustomDAConnection;
    class function GetTransaction(Obj: TDALoader): TDATransaction;
    class procedure SetTransaction(Obj: TDALoader; Value: TDATransaction);
    class function GetFTransaction(Obj: TDALoader): TDATransaction;
    class procedure SetAutoCommit(Obj: TDALoader; Value: boolean);
  end;

implementation

uses
  CRProps, CRFunctions, DAConsts;

{ TDAColumn }

constructor TDAColumn.Create(Collection: TCollection);
begin
  inherited;

  FFieldType := ftString;
end;

procedure TDAColumn.AssignTo(Dest: TPersistent);
begin
  if Dest is TDAColumn then begin
    TDAColumn(Dest).Name := Name;
    TDAColumn(Dest).FieldType := FieldType;
  end
  else
    inherited;
end;

function TDAColumn.GetDataType: TDAColumnDataType;
begin
  case FieldType of
    ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString, ftGuid {$IFDEF VER6P}, ftTimeStamp{$ENDIF}
    {$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}:
      Result := ctString;
    ftSmallint, ftInteger, ftWord, ftAutoInc:
      Result := ctInteger;
    ftLargeint:
      Result := ctUInteger;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      Result := ctFloat;
    ftDate, ftTime, ftDateTime:
      Result := ctDate;
    else
      Result := ctString;
  end;
end;

procedure TDAColumn.SetDataType(Value: TDAColumnDataType);
begin
  case Value of
    ctString:
      FieldType := ftString;
    ctDate:
      FieldType := ftDateTime;
    ctInteger:
      FieldType := ftInteger;
    ctUInteger:
      FieldType := ftLargeint;
    ctFloat:
      FieldType := ftFloat;
  end;
end;

procedure TDAColumn.SetFieldType(Value: TFieldType);
begin
  FFieldType := Value;
end;

function TDAColumn.GetDisplayName: string;
begin
  Result := FName;
end;

{ TDAColumns }

procedure TDAColumns.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  if (Action = cnAdded) and not FInCreation then
    FChangedByUser := True;

  inherited;
end;

function TDAColumns.GetColumn(Index: integer): TDAColumn;
begin
  Result := TDAColumn(inherited Items[Index]);
end;

procedure TDAColumns.SetColumn(Index: integer; Value: TDAColumn);
begin
  Items[Index].Assign(Value);
end;

{ TDALoader }

constructor TDALoader.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FColumns := TDAColumns.Create(Self, GetColumnClass);
  FAutoCommit := True;
  FOptions := CreateOptions;

  FDesignCreate := csDesigning in ComponentState;
end;

destructor TDALoader.Destroy;
begin
  FColumns.Free;
  FILoader.Free;
  FOptions.Free;

  inherited;
end;

function TDALoader.GetInternalLoaderClass: TCRLoaderClass;
begin
  Assert(False);
  Result := TCRLoader;
end;

procedure TDALoader.SetInternalLoader(Value: TCRLoader);
begin
  if Value <> FILoader then begin
    FreeInternalLoader;

    FILoader := Value;
    if FILoader <> nil then begin
      FILoader.TableName := FTableName;
      FILoader.SetProp(prQuoteNames, Options.QuoteNames);
      FILoader.SetProp(prUseBlankValues, Options.UseBlankValues);
    end;
  end;
end;

procedure TDALoader.CheckInternalLoader;
begin
  if not (FILoader is GetInternalLoaderClass) then begin
    FreeInternalLoader;
    CreateInternalLoader;
  end;
end;

procedure TDALoader.CreateInternalLoader;
begin
  SetInternalLoader(GetInternalLoaderClass.Create);
end;

procedure TDALoader.FreeInternalLoader;
begin
  FILoader.Free;
  FILoader := nil;
end;

procedure TDALoader.DoLoaderProgress(Percent: integer);
begin
  if Assigned(FOnLoaderProgress) then
    FOnLoaderProgress(Self, Percent);
end;

procedure TDALoader.Loaded;
begin
  inherited;

  FDesignCreate := False;
end;

procedure TDALoader.AssignTo(Dest: TPersistent);
begin
  if Dest is TDALoader then begin
    TDALoader(Dest).Connection := Connection;
    TDALoader(Dest).Transaction := Transaction;
    TDALoader(Dest).FTableName := TableName;
    TDALoader(Dest).Columns.Assign(Columns);
    TDALoader(Dest).AutoCommit := AutoCommit;
    TDALoader(Dest).Options.Assign(Options);
  end
  else
    inherited;
end;

procedure TDALoader.BeginConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;
  if vUsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);

  TDBAccessUtils.InternalConnect(vUsedConnection);
  CheckInternalLoader;
  FILoader.Connection := TDBAccessUtils.GetIConnection(vUsedConnection);

  if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
    vUsedTransaction := UsedTransaction;
    if vUsedTransaction = nil then
      DatabaseError(STransactionNotAssigned);

    TDBAccessUtils.GainTransaction(vUsedTransaction);
    FILoader.Transaction := TDBAccessUtils.GetITransaction(vUsedTransaction);
  end;
end;

procedure TDALoader.EndConnection;
var
  vUsedConnection: TCustomDAConnection;
  vUsedTransaction: TDATransaction;
begin
  vUsedConnection := UsedConnection;

  if TDBAccessUtils.IsMultipleTransactionsSupported(vUsedConnection) then begin
    vUsedTransaction := UsedTransaction;
    TDBAccessUtils.ReleaseTransaction(vUsedTransaction);
  end;

  TDBAccessUtils.InternalDisconnect(vUsedConnection);
end;

procedure TDALoader.CommitData;
var
  AutoCommitUsed: boolean;
  Connection: TCustomDAConnection;
begin
  Connection := UsedConnection;
  AutoCommitUsed := TDBAccessUtils.GetAutoCommit(Connection) and AutoCommit;
  if TDBAccessUtils.IsMultipleTransactionsSupported(Connection) then
    TDBAccessUtils.AutoCommitTransaction(UsedTransaction, AutoCommitUsed);
end;

procedure TDALoader.PutColumnData(Col: integer; Row: integer; const Value: variant);
begin
  CheckInternalLoader;
  FILoader.PutColumnData(Col, Row, Value);
end;

procedure TDALoader.PutColumnData(const ColName: string; Row: integer; const Value: variant);
var
  i: integer;
begin
  for i := 0 to FColumns.Count - 1 do
    if SameText(ColName, FColumns[i].Name) then begin
      PutColumnData(i, Row, Value);
      Exit;
    end;
  raise Exception.Create(Format(SColumnNotFound, [ColName]));
end;

procedure TDALoader.InternalPutData;
var
  Value: variant;
  EOF: boolean;
  i,Row: integer;
begin
  if Assigned(FOnGetColumnData) then begin
    Row := 1;
    EOF := False;
    while not EOF do begin
      for i := 0 to FColumns.Count - 1 do begin
        if FOptions.FUseBlankValues then
          Value := Unassigned;
        FOnGetColumnData(Self, FColumns[i], Row, Value, EOF);
        if not EOF then
          FILoader.PutColumnData(i, Row, Value)
        else begin
          if i <> 0 then
            FILoader.DiscardRow; // to prevent insertion of incomplete row. If EOF is set to True on getting value 1..last field, all values of this record is ignored.
          break;                 // stop loading immediately after getting EOF
        end;
      end;
      if not EOF then
        Inc(Row);
    end;
  end;
end;

procedure TDALoader.PutData;
begin
  if Assigned(FOnPutData) then
    FOnPutData(Self)
  else
    InternalPutData;
end;

procedure TDALoader.Load;
begin
  BeginConnection;
  try
    if Columns.Count = 0 then
      CreateColumns
    else
      WriteColumns;
    try
      FILoader.Prepare;
      StartWait;
      PutData;
      FILoader.DoLoad;
      CommitData; // InterBase & SQLite
    finally
      FILoader.Finish;
      StopWait;
    end;
  finally
    EndConnection;
  end;
end;

function TDALoader.NeedRecreateColumns: boolean;
begin
  Result := False;
end;

procedure TDALoader.ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  Column.Name := CRColumn.Name;
  Column.FieldType := GetFieldTypeMapClass.GetFieldType(CRColumn.DataType);
end;

procedure TDALoader.WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  CRColumn.Name := Column.Name;
  CRColumn.UpdateDataType(GetFieldTypeMapClass.GetDataType(Column.FieldType));
end;

procedure TDALoader.ClearColumns;
begin
  FColumns.BeginUpdate;
  try
    FColumns.FChangedByUser := False;
    FColumns.Clear;
  finally
    FColumns.EndUpdate;
  end;
end;

procedure TDALoader.CreateColumns;
begin
  BeginConnection;
  try
    FILoader.CreateColumns;
    ReadColumns;
  finally
    EndConnection;
  end;
end;

procedure TDALoader.ReadColumns;
var
  i: integer;
  Col: TDAColumn;
begin
  FColumns.BeginUpdate;
  try
    FColumns.FChangedByUser := False;
    FColumns.FInCreation := True;
    FColumns.FCreatedFromFields := False;

    FColumns.Clear;
    for i := 0 to FILoader.Columns.Count - 1 do begin
      Col := TDAColumn(FColumns.Add);
      ReadColumn(Col, FILoader.Columns[i]);
    end;
  finally
    FColumns.FInCreation := False;
    FColumns.EndUpdate;
  end;
end;

procedure TDALoader.WriteColumns;
var
  i: integer;
  Column: TCRLoaderColumn;
  ColumnIndex: Integer;
begin
  if not NeedRecreateColumns then begin
    FILoader.Columns.Clear;
    for i := 0 to Columns.Count - 1 do begin
      Column := FILoader.GetColumnClass.Create;
      WriteColumn(Columns[i], Column);
      FILoader.Columns.Add(Column);
    end;
  end
  else begin
    // Creating CR level columns by database
    FILoader.CreateColumns;

    for i := 0 to Columns.Count - 1 do begin
      // Find column in the CR level by name from DA level
      ColumnIndex := FILoader.Columns.GetColumnIndexByName(Columns[i].Name);
      if ColumnIndex <> -1 then begin
        Column := FILoader.Columns.Items[ColumnIndex];
        if not FColumns.FCreatedFromFields then
          WriteColumn(Columns[i], Column);
      end
      else begin
        // If column hasn't been found in the CR level then add
        Column := FILoader.GetColumnClass.Create;
        ColumnIndex := FILoader.Columns.Add(Column);
        // Write CR column from DA level
        WriteColumn(Columns[i], Column);
      end;

      // If column has invalid order then move it
      if ColumnIndex <> i then
        FILoader.Columns.Move(ColumnIndex, i);
    end;

    // Remove all CR columns that is absent in the DA level
    for i := FILoader.Columns.Count - 1 downto Columns.Count do
      FILoader.Columns.Delete(i);
  end;
end;

procedure TDALoader.CreateColumnsByFields(Fields: TFields);
var
  i: word;
  Field: TField;
begin
  FColumns.BeginUpdate;
  try
    FColumns.FChangedByUser := False;
    FColumns.FInCreation := True;
    FColumns.FCreatedFromFields := True;

    FColumns.Clear;
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if not Field.ReadOnly then
        CreateColumnByField(Field, TDAColumn(FColumns.Add));
    end;
  finally
    FColumns.FInCreation := False;
    FColumns.EndUpdate;
  end;
end;

procedure TDALoader.Notification(Component: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then begin
    if Component = FConnection then
      FConnection := nil
    else
    if Component = FTransaction then
      FTransaction := nil
  end;

  inherited;
end;

procedure TDALoader.SetConnection(Value: TCustomDAConnection);
begin
  if Value <> FConnection then begin
    if FConnection <> nil then
      FConnection.RemoveFreeNotification(Self);

    FConnection := Value;

    if FConnection <> nil then
      FConnection.FreeNotification(Self);
  end;
end;

function TDALoader.IsTransactionStored: boolean;
begin
  Result := FTransaction <> nil;
end;

function TDALoader.GetTransaction: TDATransaction;
begin
  Result := FTransaction;
end;

procedure TDALoader.SetTransaction(Value: TDATransaction);
begin
  if Value <> FTransaction then begin
    if FTransaction <> nil then
      FTransaction.RemoveFreeNotification(Self);

    FTransaction := Value;

    if FTransaction <> nil then
      FTransaction.FreeNotification(Self);
  end;
end;

procedure TDALoader.SetColumns(Value: TDAColumns);
begin
  FColumns.Assign(Value);
end;

function TDALoader.IsColumnsStored: boolean;
begin
  Result := FColumns.Count > 0;
end;

procedure TDALoader.SetTableName(const Value: string);
begin
  if Value <> FTableName then begin
    FTableName := Value;
    if FILoader <> nil then
      FILoader.TableName := Value;
    if not (csLoading in ComponentState) and (UsedConnection <> nil) and UsedConnection.Connected and not FColumns.ChangedByUser then
      if FTableName = '' then
        ClearColumns
      else
        CreateColumns;
  end;
end;

class function TDALoader.GetColumnClass: TDAColumnClass;
begin
  Result := TDAColumn;
end;

procedure TDALoader.CreateColumnByField(Field: TField; Column: TDAColumn);
begin
  Column.Name := Field.FieldName;
  Column.FieldType := Field.DataType;
end;

function TDALoader.GetFieldTypeMapClass: TFieldTypeMapClass;
begin
  Result := TFieldTypeMap;
  Assert(False);
end;

function TDALoader.UsedConnection: TCustomDAConnection;
begin
  Result := FConnection;
end;

function TDALoader.UsedTransaction: TDATransaction;
var
  UsedCon: TCustomDAConnection;
begin
  UsedCon := UsedConnection;
  if UsedCon <> nil then begin
    if TDBAccessUtils.IsMultipleTransactionsSupported(UsedCon) then
      Result := Transaction
    else
      Result := nil;

    if Result = nil then
      Result := TDBAccessUtils.UsedTransaction(UsedCon);
  end
  else
    Result := nil;
end;

procedure TDALoader.LoadFromDataSet(DataSet: TDataSet);
var
  row, col: integer;
  ColNo: array of integer;
  OldActive: boolean;

  Field: TField;
  FieldDesc: TFieldDesc;
  RecordSet: TCRRecordSet;
  IsBlank: boolean;
  Blob: TBlob;
  AValue: variant;
  Bookmark: TBookmark;
  RecCount: integer;
  IsUniDirectional, Prepared: boolean;

  procedure FillColumsNumber;
  var
    i, j: integer;
    fname: string;
  begin
    for i := 0 to DataSet.FieldCount - 1 do begin
      ColNo[i] := -1;
      fname := DataSet.Fields[i].FieldName;

      for j := 0 to FColumns.Count - 1 do
        if SameText(fname, FColumns[j].Name) then begin
          ColNo[i] := j;
          break;
        end;
    end;
  end;

begin
  BeginConnection;
  try
    if DataSet = nil then
      raise Exception.Create(SDataSetNotDefined);

    IsUniDirectional := (DataSet is TCustomDADataSet) and (DataSet as TCustomDADataSet).UniDirectional;
    OldActive := DataSet.Active;
    Bookmark := nil;
    try
      DataSet.DisableControls;
      if not IsUniDirectional then
        Bookmark := DataSet.GetBookmark;

      DataSet.Open;
      if not IsUniDirectional then
        DataSet.First;

      SetLength(ColNo, DataSet.FieldCount);

      if Columns.Count = 0 then begin
        CreateColumnsByFields(DataSet.Fields);

        for col := 0 to DataSet.FieldCount - 1 do
          ColNo[col] := col;
      end
      else
        FillColumsNumber;
      WriteColumns;

      Prepared := False;
      StartWait;
      try
        if not IsUniDirectional then
          DataSet.First;
        RecCount := DataSet.RecordCount;
        FILoader.Prepare(RecCount);
        Prepared := True;
        row := 1;

        while not DataSet.Eof do begin
          for col := 0 to DataSet.FieldCount - 1 do
            if ColNo[col] >= 0 then begin
              Field := DataSet.Fields[col];
              if DataSet is TCustomDADataSet then
                with TCustomDADataSet(DataSet) do begin
                  FieldDesc := GetFieldDesc(Field);
                  if FieldDesc <> nil then begin
                    RecordSet := TDBAccessUtils.GetIRecordSet(TCustomDADataSet(DataSet));
                    if FieldDesc.IsBlob then begin
                      IsBlank := RecordSet.GetNull(FieldDesc, IntPtr(ActiveBuffer));
                      if IsBlank then
                        AValue := Null
                      else begin
                        AValue := Unassigned;
                        Blob := RecordSet.GetBlob(FieldDesc, IntPtr(ActiveBuffer));
                        TVarData(AValue).VType := varSharedObject;
                        TVarData(AValue).VPointer := Blob;
                      end;
                      FILoader.PutColumnData(ColNo[col], row, AValue);
                      Continue;
                    end;
                  end;
                end;
              // To avoid memory leak
              AValue := Unassigned;
              if not Field.IsNull then
                AValue := Field.Value
              else
                AValue := null;
              PutColumnData(ColNo[col], row, AValue);
            end;

          DoLoaderProgress(Round((row * 100) / RecCount));
          DataSet.Next;
          Inc(row);
        end;

        FILoader.DoLoad;
        CommitData;
      finally
        if Prepared then
          FILoader.Finish;
        StopWait;
      end;
    finally
      DataSet.Active := OldActive;

      if not IsUniDirectional then begin
        DataSet.GotoBookmark(Bookmark);
        DataSet.FreeBookmark(Bookmark);
      end;
      DataSet.EnableControls;
    end;
  finally
    EndConnection;
  end;
end;

procedure TDALoader.SetOptions(const Value: TDALoaderOptions);
begin
  FOptions.Assign(Value);
end;

function TDALoader.CreateOptions: TDALoaderOptions;
begin
  Result := TDALoaderOptions.Create(Self);
end;

{ TDALoaderUtils }

class procedure TDALoaderUtils.SetDesignCreate(Obj: TDALoader; Value: boolean);
begin
  Obj.FDesignCreate := Value;
end;

class function TDALoaderUtils.GetDesignCreate(Obj: TDALoader): boolean;
begin
  Result := Obj.FDesignCreate;
end;

class function TDALoaderUtils.UsedConnection(Obj: TDALoader): TCustomDAConnection;
begin
  Result := Obj.UsedConnection;
end;

class function TDALoaderUtils.GetTransaction(Obj: TDALoader): TDATransaction;
begin
  Result := Obj.Transaction;
end;

class procedure TDALoaderUtils.SetTransaction(Obj: TDALoader; Value: TDATransaction);
begin
  Obj.Transaction := Value;
end;

class function TDALoaderUtils.GetFTransaction(Obj: TDALoader): TDATransaction;
begin
  Result := Obj.FTransaction;
end;

class procedure TDALoaderUtils.SetAutoCommit(Obj: TDALoader; Value: boolean);
begin
  Obj.AutoCommit := Value;
end;

{ TDALoaderOptions }

constructor TDALoaderOptions.Create(Owner: TDALoader);
begin
  inherited Create;

  FOwner := Owner;
  FQuoteNames := False;
  FUseBlankValues := True;
end;

function TDALoaderOptions.GetInternalLoader: TCRLoader;
begin
  Result := nil;
  if FOwner <> nil then
    Result := FOwner.FILoader;
end;

procedure TDALoaderOptions.SetQuoteNames(Value: boolean);
begin
  if Value <> FQuoteNames then begin
    FQuoteNames := Value;
    if (FOwner <> nil) and (FOwner.FILoader <> nil) then
      FOwner.FILoader.SetProp(prQuoteNames, Value);
  end;
end;

procedure TDALoaderOptions.SetUseBlankValues(Value: boolean);
begin
  if Value <> FUseBlankValues then begin
    FUseBlankValues := Value;
    if (FOwner <> nil) and (FOwner.FILoader <> nil) then
      FOwner.FILoader.SetProp(prUseBlankValues, Value);
  end;
end;

procedure TDALoaderOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TDALoaderOptions then begin
    TDALoaderOptions(Dest).QuoteNames := QuoteNames;
    TDALoaderOptions(Dest).UseBlankValues := UseBlankValues;
  end
  else
    inherited;
end;

end.
