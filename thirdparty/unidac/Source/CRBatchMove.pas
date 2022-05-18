//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  CR BatchMove
//////////////////////////////////////////////////

{$I Dac.inc}

unit CRBatchMove;

interface

uses
  Classes, SysUtils, Variants, DB,
  CRTypes, DBAccess;

type
  TCRBatchMode = (bmAppend, bmUpdate, bmAppendUpdate, bmDelete);

  TCRBatchMoveProgressEvent = procedure (Sender: TObject; Percent: integer) of object;
  TDALocate = function: boolean of object;

  TCRFieldMappingMode = (mmFieldIndex, mmFieldName);

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TCRBatchMove = class(TComponent)
  private
    FFldDestKeys: TFieldArray;
    FStrDestKeys: string;
    FSrcKeyFields: array of TField;
    FDestKeyFields: array of boolean;
    FKeyValues: array of Variant;
    FFieldMap: array of word;
  {$IFDEF WITH_IPROVIDER}
    FPSDestination: IProviderSupport;
  {$ENDIF}
    Locate: TDALocate;
    procedure SetMappings(Value: TStrings);
    procedure SetSource(Value: TDataSet);
    procedure SetDestination(Value: TDataSet);
  {$IFDEF WITH_IPROVIDER}
    function GetProviderSupport(DataSet: TDataSet): IProviderSupport;
  {$ENDIF}
    function LocateForCustomDaDataSet: boolean;
    function LocateForDataSet: boolean;

  protected
    FDestination: TDataSet;
    FSource: TDataSet;
    FMode: TCRBatchMode;
    FAbortOnKeyViol: boolean;
    FAbortOnProblem: boolean;
    FRecordCount: Integer;
    FMovedCount: Integer;
    FKeyViolCount: Integer;
    FProblemCount: Integer;
    FChangedCount: Integer;
    FMappings: TStrings;
    FFieldMappingMode: TCRFieldMappingMode;
    FCommitCount: integer;
    FOnBatchMoveProgress: TCRBatchMoveProgressEvent;
  {$IFDEF WITH_IPROVIDER}
    FTransactionNeeds: boolean;
  {$ENDIF}
    FAppliedCount: Integer;
    FDestCountKeys: word;

    procedure DoBatchMoveProgress(Percent: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetKeyValues: Variant;
    procedure SetFieldsValues(SetKeyFields: boolean);
    procedure Append;
    procedure Update;
    procedure AppendUpdate;
    procedure Delete;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Execute;

    property ChangedCount: Integer read FChangedCount;
    property KeyViolCount: Integer read FKeyViolCount;
    property MovedCount: Integer read FMovedCount;
    property ProblemCount: Integer read FProblemCount;
  published
    property AbortOnKeyViol: boolean read FAbortOnKeyViol write FAbortOnKeyViol default True;
    property AbortOnProblem: boolean read FAbortOnProblem write FAbortOnProblem default True;
    property CommitCount: integer read FCommitCount write FCommitCount default 0;
    property Destination: TDataSet read FDestination write SetDestination;
    property Mappings: TStrings read FMappings write SetMappings;
    property FieldMappingMode: TCRFieldMappingMode read FFieldMappingMode write FFieldMappingMode default mmFieldIndex;
    property Mode: TCRBatchMode read FMode write FMode default bmAppend;
    property RecordCount: Integer read FRecordCount write FRecordCount default 0;
    property Source: TDataSet read FSource write SetSource;
    property OnBatchMoveProgress: TCRBatchMoveProgressEvent read FOnBatchMoveProgress write FOnBatchMoveProgress;
  end;

implementation

uses
{$IFDEF VER7P}
  StrUtils,
{$ENDIF}
{$IFDEF CLR}
  System.Xml, System.Runtime.InteropServices,
{$ELSE}
  CLRClasses,
{$ENDIF}
  CRFunctions, CRAccess, CRParser, DAConsts,
  MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF};

{ TCRBatchMove }

constructor TCRBatchMove.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAbortOnKeyViol := True;
  FAbortOnProblem := True;
  FCommitCount := 0;
  FMode := bmAppend;
  FMappings := TStringList.Create;
  FFieldMappingMode := mmFieldIndex;
end;

destructor TCRBatchMove.Destroy;
begin
  Source := nil;
  Destination := nil;
  FMappings.Free;

  inherited;
end;

procedure TCRBatchMove.SetSource(Value: TDataSet);
begin
  if FSource <> Value then begin
    if FSource <> nil then
      FSource.RemoveFreeNotification(Self);
    FSource := Value;
    if FSource <> nil then
      FSource.FreeNotification(Self);
  end;
end;

procedure TCRBatchMove.SetDestination(Value: TDataSet);
begin
  if FDestination <> Value then begin
    if FDestination <> nil then
      FDestination.RemoveFreeNotification(Self);
  {$IFDEF WITH_IPROVIDER}
    FPSDestination := nil;
  {$ENDIF}
    FDestination := Value;
    if FDestination <> nil then
      FDestination.FreeNotification(Self);
  end;
end;

procedure TCRBatchMove.SetMappings(Value: TStrings);
begin
  FMappings.Assign(Value);
end;

procedure TCRBatchMove.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if Destination = AComponent then
      Destination := nil;
    if Source = AComponent then
      Source := nil;
  end;
end;

procedure TCRBatchMove.DoBatchMoveProgress(Percent: integer);
begin
  if Assigned(FOnBatchMoveProgress) then
    FOnBatchMoveProgress(Self, Percent);
end;

{$IFDEF WITH_IPROVIDER}
function TCRBatchMove.GetProviderSupport(DataSet: TDataSet): IProviderSupport;
begin
  Result := nil;
  if DataSet <> nil then
     Result := IProviderSupport(DataSet);
end;
{$ENDIF}

function TCRBatchMove.GetKeyValues: Variant;
var
  i: integer;
begin
  for i := 0 to FDestCountKeys - 1 do begin
    if FSrcKeyFields[i] <> nil then
      FKeyValues[i] := FSrcKeyFields[i].AsVariant
    else
      FKeyValues[i] := ''
  end;

  if FDestCountKeys > 1 then
    Result := VarArrayOf(FKeyValues)
  else
  if FDestCountKeys > 0 then
    Result := FKeyValues[0]
  else
    Result := Null;
end;

procedure TCRBatchMove.SetFieldsValues(SetKeyFields: boolean);
var
  fn: integer;
  SrcField, DestField: TField;
  SrcFieldDesc, DestFieldDesc: TFieldDesc;
  SrcBlob, DestBlob: TBlob;
  SkipNulls: boolean;
  SrcStream, DestStream: TStream;
  SrcPtr: IntPtr;
  Optimization: Boolean;
{$IFDEF CLR}
  Data: TBytes;
{$ENDIF}
begin
  SkipNulls := (Mode = bmAppend) or (Mode = bmAppend) and SetKeyFields;
  for fn := 0 to Destination.FieldCount - 1 do begin
    if (FFieldMap[fn] = 0) or (not Destination.Fields[fn].CanModify) or
       (FDestKeyFields[fn] and (not SetkeyFields)) or
       Source.Fields[FFieldMap[fn] - 1].IsNull and SkipNulls then
         Continue;

    SrcField := Source.Fields[FFieldMap[fn] - 1];
    Assert(SrcField <> nil);
    DestField := Destination.Fields[fn];
    Assert(DestField <> nil);

    try
      if SrcField.IsNull then
        DestField.Clear
      else
      case DestField.DataType of
        ftString, ftFixedChar:
          case SrcField.DataType of
            ftMemo, ftFmtMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}: begin
              Assert(SrcField is TBlobField);
              DestField.AsString := TBlobField(SrcField).AsString;
            end;
            else
              DestField.AsString := SrcField.AsString;
          end;
        ftWideString{$IFDEF VER10P}, ftFixedWideChar{$ENDIF}:
          case SrcField.DataType of
            ftMemo, ftFmtMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}: begin
              Assert(SrcField is TBlobField);
              Assert(DestField is TWideStringField);
              TWideStringField(DestField).AsString := TBlobField(SrcField).AsString;
            end;
            ftWideString{$IFDEF VER10P}, ftFixedWideChar{$ENDIF}: begin
              Assert(SrcField is TWideStringField);
              Assert(DestField is TWideStringField);
              TWideStringField(DestField).Value := TWideStringField(SrcField).Value;
            end
            else
              DestField.AsString := SrcField.AsString;
          end;
        ftSmallint, ftInteger, ftWord, ftAutoInc:
          DestField.AsInteger := SrcField.AsInteger;
        ftLargeint: begin
          Assert(DestField is TLargeIntField);
          case SrcField.DataType of
            ftLargeInt:
              TLargeIntField(DestField).AsLargeInt := TLargeIntField(SrcField).AsLargeInt;
            else
              TLargeIntField(DestField).AsLargeInt := SrcField.AsVariant;
          end;
        end;
        ftBoolean:
          DestField.AsBoolean := SrcField.AsBoolean;
        ftFloat:
          DestField.AsFloat := SrcField.AsFloat;
        ftCurrency, ftBCD:
          DestField.AsCurrency := SrcField.AsCurrency;
        ftTimeStamp:
        {$IFNDEF FPC}
          DestField.AsSQLTimeStamp := SrcField.AsSQLTimeStamp;
        {$ELSE}
          DestField.AsVariant := SrcField.AsVariant;
        {$ENDIF}
        ftFMTBCD:
          DestField.AsBCD := SrcField.AsBCD;
        ftTime:
        {$IFNDEF FPC}
          DestField.AsDateTime := SrcField.AsDateTime;
        {$ELSE}
          DestField.AsString := SrcField.AsString;
        {$ENDIF}
        ftDate, ftDateTime:
          DestField.AsDateTime := SrcField.AsDateTime;
        ftBytes, ftVarBytes:
          DestField.AsVariant := SrcField.AsVariant;
        ftBlob, ftOraBlob, ftOraClob: begin
          DestStream := nil;
          try
            if SrcField is TBlobField then begin
              Optimization := False;
              if (SrcField.DataSet is TMemDataSet) and (DestField.DataSet is TMemDataSet) then begin
                // TCustomDADataSet optimization
                SrcFieldDesc := TMemDataSet(SrcField.DataSet).GetFieldDesc(SrcField);
                DestFieldDesc := TMemDataSet(SrcField.DataSet).GetFieldDesc(SrcField);
                if SrcFieldDesc.DataType = DestFieldDesc.DataType then begin
                  SrcBlob := TMemDataSet(SrcField.DataSet).GetBlob(SrcField);
                  DestBlob := TMemDataSet(DestField.DataSet).GetBlob(DestField);
                  if SrcBlob.ClassType = DestBlob.ClassType then begin
                  {$IFDEF HAVE_COMPRESS}
                    if (SrcField.DataSet is TCustomDADataSet) and (DestField.DataSet is TCustomDADataSet) then
                      Optimization := TCustomDADataSet(SrcField.DataSet).Options.CompressBlobMode = TCustomDADataSet(DestField.DataSet).Options.CompressBlobMode
                    else
                      Optimization := True;
                  {$ENDIF}
                    if Optimization then begin
                      TMemDSUtils.SetBlob(TMemDataSet(DestField.DataSet), DestField, SrcBlob);
                      TBlobField(DestField).Modified := True;
                    end;
                  end;
                end;
              end;
              if not Optimization then begin
                DestStream := DestField.DataSet.CreateBlobStream(DestField, bmWrite);
                SrcStream := SrcField.DataSet.CreateBlobStream(SrcField, bmRead);
                try
                  DestStream.CopyFrom(SrcStream, 0);
                finally
                  SrcStream.Free;
                end;
              end;
            end
            else
            if SrcField.DataSize > 0 then begin
              DestStream := DestField.DataSet.CreateBlobStream(DestField, bmWrite);
              SrcPtr :=  Marshal.AllocHGlobal(SrcField.DataSize);
              try
                SrcField.GetData(TValueBuffer(SrcPtr));
                DestStream.ReadBuffer(SrcPtr^, SrcField.DataSize);
              finally
                Marshal.FreeHGlobal(SrcPtr);
              end;
            end
            else
              DestField.AsVariant := SrcField.AsVariant;
          finally
            DestStream.Free;
          end;
        end;
        ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}:
          DestField.AsString := SrcField.AsString;
        else
          DestField.AsVariant := SrcField.AsVariant;
      end;
    except
      on Exception do begin
        Inc(FProblemCount);
        if FAbortOnProblem then
          raise;
      end;
    end;

  end;
end;

function TCRBatchMove.LocateForCustomDaDataSet: boolean;
begin
  Result := TCustomDADataSet(Destination).Locate(FFldDestKeys, GetKeyValues, []);
end;

function TCRBatchMove.LocateForDataSet: boolean;
begin
  Result := Destination.Locate(FStrDestKeys, GetKeyValues, []);
end;

procedure TCRBatchMove.Append;
begin
{$IFDEF WITH_IPROVIDER}
  if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
    FPSDestination.PSStartTransaction;
{$ENDIF}

  Destination.Append;
  SetFieldsValues(True);
  Destination.Post;

  Inc(FAppliedCount);
{$IFDEF WITH_IPROVIDER}
  if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
    FPSDestination.PSEndTransaction(True);
{$ENDIF}
end;

procedure TCRBatchMove.Update;
begin
  if Locate then begin
  {$IFDEF WITH_IPROVIDER}
    if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
      FPSDestination.PSStartTransaction;
  {$ENDIF}

    Destination.Edit;
    SetFieldsValues(False);
    Destination.Post;
    Inc(FChangedCount);

    Inc(FAppliedCount);
  {$IFDEF WITH_IPROVIDER}
    if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
      FPSDestination.PSEndTransaction(True);
  {$ENDIF}
  end;
end;

procedure TCRBatchMove.AppendUpdate;
begin
{$IFDEF WITH_IPROVIDER}
  if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
    FPSDestination.PSStartTransaction;
{$ENDIF}

  if Locate then begin
    Destination.Edit;
    SetFieldsValues(False);
    Destination.Post;
    Inc(FChangedCount);
  end
  else begin
    Destination.Append;
    SetFieldsValues(True);
    Destination.Post;
  end;

  Inc(FAppliedCount);
{$IFDEF WITH_IPROVIDER}
  if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
    FPSDestination.PSEndTransaction(True);
{$ENDIF}
end;

procedure TCRBatchMove.Delete;
begin
  if Locate then begin
  {$IFDEF WITH_IPROVIDER}
    if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
      FPSDestination.PSStartTransaction;
  {$ENDIF}

    Destination.Delete;
    Inc(FChangedCount);

    Inc(FAppliedCount);
  {$IFDEF WITH_IPROVIDER}
    if FTransactionNeeds and ((FAppliedCount mod FCommitCount) = 0) then
      FPSDestination.PSEndTransaction(True);
  {$ENDIF}
  end;
end;

procedure TCRBatchMove.Execute;
type
  TAction = procedure of object;
var
  OldSourceActive, OldDestinationActive: Boolean;
  Bookmark: {$IFNDEF FPC}{$IFDEF VER12P}TBytes{$ELSE}string{$ENDIF}{$ELSE}TBytes{$ENDIF};
  DestName, SourceName: string;
  DestFieldCount: word;
  Action: TAction;

  procedure GetKeyFields;
  var
    i, p1, p2: integer;
    fieldNo: integer;
    KeyFields: TKeyAndDataFields;

    procedure SetKeyField;
    begin
      FDestKeyFields[fieldNo] := True;
      if FFieldMap[fieldNo] > 0 then
        FSrcKeyFields[FDestCountKeys] := Source.Fields[FFieldMap[fieldNo] - 1]
      else
        FSrcKeyFields[FDestCountKeys] := nil;
    end;

  begin
    DestFieldCount := Destination.FieldCount;
    SetLength(FSrcKeyFields, DestFieldCount);
    SetLength(FDestKeyFields, DestFieldCount);
    for i := 0 to DestFieldCount - 1 do
      FDestKeyFields[i] := False;

    FDestCountKeys := 0;
    if Destination is TCustomDADataSet then begin
      TDBAccessUtils.GetKeyAndDataFields(TCustomDADataSet(Destination), KeyFields, True);
      if Length(KeyFields.KeyFieldDescs) = 0 then
        KeyFields.KeyFieldDescs := KeyFields.DataFieldDescs;

      SetLength(FFldDestKeys, Length(KeyFields.KeyFieldDescs));
      for i := 0 to Length(FFldDestKeys) - 1 do begin
        FFldDestKeys[i] := TCustomDADataSet(Destination).GetField(KeyFields.KeyFieldDescs[i]);
        fieldNo := FFldDestKeys[i].Index;
        SetKeyField;
        Inc(FDestCountKeys);
      end;
    end
    else begin
    {$IFDEF WITH_IPROVIDER}
      FStrDestKeys := FPSDestination.PSGetKeyFields;
    {$ENDIF}
      p1 := 1;
      p2 := Pos(';', FStrDestKeys);
      while p2 > 0 do begin
        fieldNo := Destination.FieldByName(Copy(FStrDestKeys, p1, p2 - 1)).Index;
        SetKeyField;
        Inc(FDestCountKeys);
        p1 := p2 + 1;
        p2 := PosEx(';', FStrDestKeys, p1);
      end;

      if Length(FStrDestKeys) > 0 then begin
        fieldNo := Destination.FieldByName(Copy(FStrDestKeys, p1, Length(FStrDestKeys))).Index;
        SetKeyField;
        Inc(FDestCountKeys);
      end
      else begin
        for i := 0 to Destination.FieldCount - 1 do begin
          fieldNo := Destination.Fields[i].Index;
          SetKeyField;
          Inc(FDestCountKeys);
          if FStrDestKeys <> '' then
            FStrDestKeys := FStrDestKeys + ';';
          FStrDestKeys := FStrDestKeys + Destination.Fields[i].FieldName;
        end;
      end;
    end;

    SetLength(FSrcKeyFields, FDestCountKeys);
    SetLength(FKeyValues, FDestCountKeys);
  end;

  procedure GetMappingNames(num: integer);
  var
    p: Integer;
    Mapping: string;
  begin
    Mapping := FMappings[num];
    p := Pos('=', Mapping);
    if p > 0 then begin
      DestName := Copy(Mapping, 1, p - 1);
      SourceName := Copy(Mapping, p + 1, 255);
    end
    else begin
      DestName := Mapping;
      SourceName := Mapping;
    end;
  end;

  procedure SetMapping;
  var
    i: integer;
    SrcField, DestField: TField;
  begin
    DestFieldCount := Destination.FieldCount;
    SetLength(FFieldMap, DestFieldCount);
    // SetLength(FFieldComp, DestFieldCount);

    if FMappings.Count <> 0 then
    begin
      for i := 0 to DestFieldCount - 1 do
        FFieldMap[i] := 0;

      for i := 0 to FMappings.Count - 1 do begin
        GetMappingNames(i);
        SrcField := Source.FindField(SourceName);
        if SrcField = nil then
          raise Exception.CreateFmt(SFieldNotFound, [SourceName]);
        DestField := Destination.FindField(DestName);
        if DestField = nil then
          raise Exception.CreateFmt(SFieldNotFound, [DestName]);
        FFieldMap[DestField.Index] := SrcField.Index + 1;
      end;
    end
    else
      case FFieldMappingMode of
        mmFieldIndex:
          for i := 0 to DestFieldCount - 1 do
            if i < Source.FieldCount then
              FFieldMap[i] := i + 1
            else
              FFieldMap[i] := 0;
        mmFieldName:
          for i := 0 to DestFieldCount - 1 do begin
            SrcField := Source.FindField(Destination.Fields[i].FieldName);
            if SrcField <> nil then
              FFieldMap[i] := SrcField.Index + 1
            else
              FFieldMap[i] := 0;
          end;
      end;
  end;

var
  IsKeyViolation: boolean;

begin
  if (Destination = nil) or (Source = nil) or (Destination = Source) then
    DatabaseError(SInvalidBatchMove, Self);
  OldSourceActive := Source.Active;
  OldDestinationActive := Destination.Active;
  try
    Source.DisableControls;
    Destination.DisableControls;
    Source.Open;
    Source.CheckBrowseMode;
    Source.UpdateCursorPos;
    Destination.Open;
    Destination.CheckBrowseMode;
    Source.CursorPosChanged;
    FChangedCount := 0;
    FKeyViolCount := 0;
    FProblemCount := 0;
    FMovedCount := 0;
    FAppliedCount := 0;

  {$IFDEF WITH_IPROVIDER}
    FPSDestination := GetProviderSupport(Destination);
  {$ENDIF}
    if Destination is TCustomDADataSet then
      Locate := LocateForCustomDaDataSet
    else
      Locate := LocateForDataSet;

    SetMapping;
    GetKeyFields;

  {$IFDEF WITH_IPROVIDER}
    FTransactionNeeds := (not FPSDestination.PSInTransaction) and (FCommitCount > 0);
  {$ENDIF}
    if FRecordCount = 0 then begin
      Bookmark := Source.Bookmark;
      Source.First;
    end;

    Action := nil;
    case FMode of
      bmAppend:
        Action := Append;
      bmUpdate:
        Action := Update;
      bmAppendUpdate:
        Action := AppendUpdate;
      bmDelete:
        Action := Delete;
    else
      Assert(False);
    end;

    try
      while not Source.Eof do begin
        try
          if (FAppliedCount >= FRecordCount) and (FRecordCount > 0) then
            Exit;
          Inc(FMovedCount);

          Action;
        except
          on E: EDatabaseError do begin
            Destination.Cancel;

            if not (Destination is TCustomDADataSet) then
              IsKeyViolation := True
            else
              if E is EDAError then
                IsKeyViolation := EDAError(E).IsKeyViolation
              else
                IsKeyViolation := False;

            if IsKeyViolation then begin
              Inc(FKeyViolCount);
              if FAbortOnKeyViol or (FMode = bmDelete) then begin
              {$IFDEF WITH_IPROVIDER}
                if FTransactionNeeds then
                  FPSDestination.PSEndTransaction(False);
              {$ENDIF}
                raise;
              end;
            end
            else
              raise;
          end;
        end;

        // If Source.FetchAll = False and QueryRecCount = False
        // Percent swings about 100%.
        DoBatchMoveProgress((Source.RecNo * 100) div Source.RecordCount);
        Source.Next;
      end;

    finally
    {$IFDEF WITH_IPROVIDER}
      if FTransactionNeeds and FPSDestination.PSInTransaction then
        FPSDestination.PSEndTransaction(True);
    {$ENDIF}
    end;

  finally
    if FRecordCount = 0 then
      // for descendant of TCustomDADataSet
      if Source is TCustomDADataSet then begin
        if not TCustomDADataSet(Source).UniDirectional then
          Source.Bookmark := Bookmark;
      end
      // for descendant of TDataSet
      else if not Source.IsUniDirectional then
        Source.Bookmark := Bookmark;
    if OldDestinationActive then
      Destination.First;
    Destination.Active := OldDestinationActive;
    Source.Active := OldSourceActive;
    Destination.EnableControls;
    Source.EnableControls;
  end;
end;

end.
