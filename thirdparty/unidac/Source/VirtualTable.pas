//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  Virtual table
//  Created:            11.12.98
//////////////////////////////////////////////////

unit VirtualTable;

{$I Dac.inc}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, DB,
  MemData, {$IFDEF FPC}MemDataSet,{$ELSE}MemDS,{$ENDIF}
  CRTypes;

const
  ftVirtualAutoInc = 555; 

type
  TCRFileFormat = (ffVTD, ffXML);

  TVirtualTableOption = (voPersistentData, voStored, voSetEmptyStrToNull, voSkipUnSupportedFieldTypes);
  TVirtualTableOptions = set of TVirtualTableOption;
  TVirtualTableProgressEvent = procedure (Sender: TObject; Percent: integer) of object;

  TVirtualAutoIncField = class(TAutoIncField)
  private
    FCurrentValue,
    FInitialValue,
    FIncrement: integer;

    procedure SetInitialValue(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
  {$IFNDEF FPC}
    property AutoGenerateValue default arAutoInc;
  {$ENDIF}

    property InitialValue: integer read FInitialValue write SetInitialValue default -1;
    property Increment: integer read FIncrement write FIncrement default 1;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TVirtualTable = class(TMemDataSet {$IFDEF FPC},IFPObserver{$ENDIF})
  private
    FOptions: TVirtualTableOptions;
    FStreamedActive: boolean;
    FAvoidRefreshData: boolean;
    FAvoidReload: integer;
    FRecordDataStream: TMemoryStream;
    FIsCursorOpen: boolean;
    FOnVirtualTableProgress: TVirtualTableProgressEvent;
    FReadOnly,
    FLoadingData: boolean;
    FDefaultSortType: TSortType;

    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);

    function IsFieldDefsStored: boolean;
    function GetFieldDefs: TFieldDefs;
    procedure SetFieldDefs(Value: TFieldDefs);
    procedure SetDefaultSortType(const Value: TSortType);
    procedure SetOptions(Value: TVirtualTableOptions);
    procedure InternalSaveToStream(Stream: TStream; StoreFields: boolean; StoreAllData: boolean);
    procedure InitAutoIncValues(RecBuf: IntPtr);

  protected
    FFieldDefsByField: boolean;

    procedure Loaded; override;
    procedure CreateIRecordSet; override;
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;
    procedure CreateFieldDefs; override;
  {$IFDEF FPC}
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  {$ENDIF}
    procedure DefChanged(Sender: TObject); {$IFNDEF FPC}override;{$ENDIF}
    procedure Reload;
  {$IFDEF FPC}
    procedure DataEvent(Event: TDataEvent; Info: PtrInt); override;
  {$ELSE}
  {$IFDEF VER16P}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  {$ELSE}
    procedure DataEvent(Event: TDataEvent; Info: Longint); override;
  {$ENDIF}
  {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignDataSet(Source: TDataSet);
    procedure SetActive(Value:boolean); override;
    procedure DoVirtualTableProgress(Percent: integer);
    function GetCanModify: boolean; override;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass; reintroduce; overload; override;
  {$IFDEF VER12P}
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; overload; override;
  {$ENDIF}
    procedure InternalInsert; override;
  {$IFDEF FPC}
  public
  {$ENDIF}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    function IsSequenced: boolean; override;

    procedure AddField(const Name: string; FieldType: TFieldType; Size: integer = 0; Required: boolean = False);
    procedure DeleteField(const Name: string);
    procedure DeleteFields;

    procedure Clear;

  { Stream/File }
    procedure LoadFromStream(Stream: TStream; LoadFields: boolean = True; DecodeHTMLEntities: boolean = True);
    procedure SaveToStream(Stream: TStream; StoreFields: boolean = True; StoreAllData: boolean = false);

    procedure LoadFromFile(const FileName: string; LoadFields: boolean = True; DecodeHTMLEntities: boolean = True);
    procedure SaveToFile(const FileName: string; StoreFields: boolean = True; StoreAllData: boolean = false);

    procedure Assign(Source: TPersistent); override;

    function GetData: TMemData;
  published
    property DefaultSortType: TSortType read FDefaultSortType write SetDefaultSortType default stCaseSensitive;
    property Options: TVirtualTableOptions read FOptions write SetOptions default [voPersistentData, voStored, voSkipUnSupportedFieldTypes];
    property OnProgress: TVirtualTableProgressEvent read FOnVirtualTableProgress write FOnVirtualTableProgress;

    property Active;
    property AutoCalcFields;
    property Constraints stored IsConstraintsStored;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
    property MasterSource;
    property MasterFields;
    property DetailFields;

    //property Fields stored False;
    property FieldDefs: TFieldDefs read GetFieldDefs write SetFieldDefs stored IsFieldDefsStored;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;

    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnUpdateError;
    property OnUpdateRecord;
  end;

var
  VTOldBehavior: boolean;

implementation

uses
  Variants, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  CLRClasses, CRXml,
  CRFunctions, CRParser, MemUtils, DAConsts;

{$IFDEF VER12P}
{$IFNDEF NEXTGEN}
  {$DEFINE USE_ASANSISTRING}
{$ENDIF}
{$ENDIF}

{$UNDEF USE_ASBYTES}

{$IFDEF LINUX}
  {$DEFINE USE_ASBYTES}
{$ENDIF}
{$IFDEF NEXTGEN}
   {$DEFINE USE_ASBYTES}
{$ENDIF}

const
  // Must be sync with 'case' in AddFieldDesc and 'case' in SaveToStream
  SupportFieldTypes = [ftString, ftWideString, ftBoolean,
    ftSmallint, ftInteger, ftLargeint, ftWord, ftAutoInc,
    {$IFDEF VER12P}ftByte, ftShortInt, ftLongWord, ftExtended,{$ENDIF}
    ftFloat, ftCurrency, {$IFDEF VER14P}ftSingle,{$ENDIF}
    ftDate, ftTime, ftDateTime, {$IFNDEF FPC}ftTimeStamp,{$ENDIF}
    ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF},
    ftGuid, ftBCD, ftFmtBcd, ftBytes, ftVarBytes, ftVariant];
  SNotSupportFieldType = 'Field type is not supported by TVirtualTable. '#13 +
    'Valid types is String, WideString, Smallint, Integer, Word, Boolean, Largeint, Float,' + {$IFDEF VER14P}' ftSingle,' +{$ENDIF}{$IFDEF VER12P}' ftExtended,' +{$ENDIF} ' Currency, Date, Time, DateTime,' + {$IFNDEF FPC}' SQLTimeStamp,' + {$ENDIF} ' Blob, Memo, Guid, Bcd, FmtBcd, Bytes, VarBytes, Variant';

  EmptyString = '';
  EmptyWString = WideString('');

type

  TVirtualTableData = class (TMemData)
  protected
    FOwner: TDataSet;

    procedure DetectFieldType(DBType: TFieldType; DBLength: Integer; DBPrecision: Integer; DBScale: Integer;
      out DataType: Word; out Length, Scale: Integer);
    procedure CreateFieldDescs; override;

    procedure SetSortDefaults(SortColumn: TSortColumn); override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
  public
    constructor Create;

    procedure Reopen; override;
  end;

{ TVirtualTableData }

constructor TVirtualTableData.Create;
begin
  inherited;

  FOwner := nil;
end;

procedure TVirtualTableData.DetectFieldType(DBType: TFieldType; DBLength: Integer; DBPrecision: Integer; DBScale: Integer;
  out DataType: Word; out Length, Scale: Integer);
begin
  Length := 0;
  Scale := 0;

  case DBType of
    ftString: begin
      DataType := dtString;
      Length := DBLength;
    end;
    ftWideString: begin
      DataType := dtWideString;
      Length := DBLength;
    end;
    ftSmallint{$IFDEF VER12P}, ftShortInt{$ENDIF}:
      if (DBLength <= 4) and (DBLength <> 0){$IFDEF FPC} and (DBLength <> -1){$ENDIF} then begin
        DataType := dtInt8;
        Length := DBPrecision;
      end
      else
        DataType := dtSmallInt;
  {$IFDEF VER12P}
    ftByte:
      DataType := dtByte;
  {$ENDIF}
    ftInteger, ftAutoInc:
      DataType := dtInteger;
    ftWord:
      DataType := dtWord;
    ftBoolean:
      DataType := dtBoolean;
    ftLargeint{$IFDEF VER12P}, ftLongWord{$ENDIF}:
      if (DBPrecision <= 10) and (DBPrecision <> 0){$IFDEF FPC} and (DBPrecision <> -1){$ENDIF} then begin
        DataType := dtUInt32;
        Length := DBPrecision;
      end
      else
        DataType := dtLargeint;
    ftFloat: begin
      DataType := dtFloat;
      Length := DBPrecision;
    end;
  {$IFDEF VER14P}
    ftSingle: begin
      DataType := dtSingle;
      Length := DBPrecision;
    end;
  {$ENDIF}
  {$IFDEF VER12P}
    ftExtended: begin
      DataType := dtExtended;
      Length := DBPrecision;
    end;
  {$ENDIF}
    ftCurrency:
      DataType := dtCurrency;
    ftDate: begin
      DataType := dtDate;
      Length := DBPrecision;
    end;
    ftTime: begin
      DataType := dtTime;
      Length := DBPrecision;
    end;
    ftDateTime: begin
      DataType := dtDateTime;
      Length := DBPrecision;
    end;
  {$IFNDEF FPC}
    ftTimeStamp: begin
      DataType := dtSQLTimeStamp;
      Length := DBPrecision;
    end;
  {$ENDIF}
    ftBlob:
      DataType := dtBlob;
    ftMemo:
      DataType := dtMemo;
  {$IFDEF VER10P}
    ftWideMemo:
      DataType := dtWideMemo;
  {$ENDIF}
  {$IFDEF FPC}
    ftWideMemo:
      DataType := dtWideMemo;
  {$ENDIF}
    ftGuid: begin
      DataType := dtGuid;
      Length := DBLength;
    end;
    ftBCD: begin
      DataType := dtBCD;
      Length := DBPrecision;
      Scale := DBScale;
    end;
    ftFmtBcd: begin
      DataType := dtFmtBCD;
      Length := DBPrecision;
      Scale := DBScale;
    end;
    ftBytes: begin
      DataType := dtBytes;
      Length := DBLength;
    end;
    ftVarbytes: begin
      DataType := dtVarBytes;
      Length := DBLength;
    end;
    ftVariant:
      DataType := dtVariant;
{
ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftVariant
}
  else
    DatabaseError(SNotSupportFieldType);
  end;
end;

procedure TVirtualTableData.CreateFieldDescs;

  procedure AddFieldDesc(const FieldName: string; const DBType: TFieldType;
    const DBLength: integer; const DBPrecision: integer; const DBScale: integer;
    const Required: boolean; const Fixed: boolean);
  var
    Field: TFieldDesc;
    DataType: Word;
    Length: Integer;
    Scale: Integer;
    Size: Integer;
  begin
    DetectFieldType(DBType, DBLength, DBPrecision, DBScale, DataType, Length, Scale);
    Size := GetBufferSize(DataType, Length);

    Field := CreateFieldDesc;
    try
      Field.FieldNo := FFields.Count + 1;
      Field.Name := FieldName;
      Field.DataType := DataType;
      Field.Length := Length;
      Field.Scale := Scale;
      Field.Size := Size;
      Field.Required := Required;
      Field.Fixed := Fixed;
      FFields.Add(Field);
    except
      Field.Free;
    end;
  end;

var
  i: integer;
  OwnerField: TField;
  OwnerFieldDef: TFieldDef;
  DataFieldCount: integer;
  FieldPrecision, FieldScale: integer;
begin
  DataFieldCount := 0;
  for i := 0 to FOwner.FieldCount - 1 do
    if FOwner.Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

{$IFDEF VER20P}
  if (lcPersistent in FOwner.Fields.LifeCycles) and
{$ELSE}
  if not FOwner.DefaultFields and
{$ENDIF}
     ((DataFieldCount > FOwner.FieldDefs.Count) or (TVirtualTable(FOwner).FFieldDefsByField and not VTOldBehavior))
  then
  // From fields
    for i := 0 to FOwner.FieldCount - 1 do begin
      if FOwner.Fields[i].FieldKind = fkData then begin
        OwnerField := FOwner.Fields[i];

        FieldPrecision := 0;
        FieldScale := 0;
        case OwnerField.DataType of
          ftFloat: begin
            FieldPrecision := TFloatField(OwnerField).Precision;
            FieldScale := 0;
          end;
        {$IFDEF VER14P}
          ftSingle: begin
            FieldPrecision := TSingleField(OwnerField).Precision;
            FieldScale := 0;
          end;
        {$ENDIF}
        {$IFDEF VER12P}
          ftExtended: begin
            FieldPrecision := TExtendedField(OwnerField).Precision;
            FieldScale := 0;
          end;
        {$ENDIF}
        end;

        AddFieldDesc(OwnerField.FieldName, OwnerField.DataType, OwnerField.Size, FieldPrecision, FieldScale, False, False);
      end
    end
  else
  // From FieldDefs
    for i := 0 to FOwner.FieldDefs.Count - 1 do begin
      OwnerFieldDef := FOwner.FieldDefs[i];

      AddFieldDesc(OwnerFieldDef.Name, OwnerFieldDef.DataType, OwnerFieldDef.Size, OwnerFieldDef.Precision,
        OwnerFieldDef.Size, faRequired in OwnerFieldDef.Attributes, faFixed in OwnerFieldDef.Attributes);
    end;
end;

procedure TVirtualTableData.SetSortDefaults(SortColumn: TSortColumn);
begin
  inherited;

  SortColumn.SortType := (FOwner as TVirtualTable).FDefaultSortType;
end;

procedure TVirtualTableData.InternalOpen(DisableInitFields: boolean = False);
begin
  if not DisableInitFields then
    InitFields;

  inherited;
end;

procedure TVirtualTableData.Reopen;
begin
end;

{ TVirtualAutoIncField }

constructor TVirtualAutoIncField.Create(AOwner: TComponent);
begin
  inherited;

  FCurrentValue := 1;
  FInitialValue := -1;
  FIncrement := 1;
{$IFNDEF FPC}
  AutoGenerateValue := arAutoInc;
{$ENDIF}  
end;

procedure TVirtualAutoIncField.SetInitialValue(const Value: integer);
begin
  if FInitialValue <> Value then begin
    FInitialValue := Value;
    if FInitialValue <> -1 then
      FCurrentValue := Value
    else
      FCurrentValue := 1;
  end;
end;

{ TVirtualTable }

function TVirtualTable.GetData: TMemData;
begin
  Result := Data as TMemData;
end;

constructor TVirtualTable.Create(Owner: TComponent);
begin
  inherited;

  CreateIRecordSet;
  FDefaultSortType := stCaseSensitive;
  FOptions := [voPersistentData, voStored, voSkipUnSupportedFieldTypes];
  FRecordDataStream := TMemoryStream.Create;

{$IFDEF FPC}
  FieldDefs.FPOAttachObserver(Self);
{$ENDIF}
end;

destructor TVirtualTable.Destroy;
begin
  Data.Close; // Clear data
  FRecordDataStream.Free;

  inherited;
end;

procedure TVirtualTable.Loaded;
begin
  inherited;

  try
    try
      try
        FRecordDataStream.Seek(LongInt(0), soFromBeginning);
        if FRecordDataStream.Size > 0 then
          LoadFromStream(FRecordDataStream, False);
      finally
        FRecordDataStream.Clear;
      end;
      if FStreamedActive then
        Active := True;
    except
      if csDesigning in ComponentState then
        InternalHandleException
      else
        raise;
    end;
  finally
    FStreamedActive := False;
  end;
end;

procedure TVirtualTable.CreateIRecordSet;
begin
  FCreateCalcFieldDescs := False;

  SetIRecordSet(TVirtualTableData.Create);
  TVirtualTableData(Data).FOwner := Self;
end;

procedure TVirtualTable.OpenCursor(InfoQuery: boolean);
begin
  Inc(FAvoidReload);
  try
    inherited;
  finally
    Dec(FAvoidReload);
  end;
end;

procedure TVirtualTable.InternalOpen;
begin
  if FAvoidReload = 0 then
    FFieldDefsByField := False;

  inherited;

  FIsCursorOpen := True;
end;

procedure TVirtualTable.InternalClose;
begin
  Inc(FAvoidReload);
  try
    BindFields(False);
  {$IFDEF VER20P}
    if not (lcPersistent in Fields.LifeCycles) then
  {$ELSE}
    if DefaultFields then
  {$ENDIF}
      DestroyFields;

    if not (voPersistentData in FOptions) then
      Data.Close
    else
      Data.SetToBegin;

    FIsCursorOpen := False;
  finally
    Dec(FAvoidReload);
  end;
end;

function TVirtualTable.IsCursorOpen: boolean;
begin
  Result := FIsCursorOpen;
end;

procedure TVirtualTable.CreateFieldDefs;
var
  DataFieldCount: integer;
  OldFieldDefsCount: integer;
  i: integer;
begin
  OldFieldDefsCount := FieldDefs.Count;
  DataFieldCount := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

{$IFDEF VER20P}
  if (lcPersistent in Fields.LifeCycles) and
{$ELSE}
  if not DefaultFields and
{$ENDIF}
     ((DataFieldCount > FieldDefs.Count) or FFieldDefsByField)
  then
    try
      // Used to prevent save/load table DefChanged
      Inc(FAvoidReload);
      inherited;
      if FFieldDefsByField then
        FFieldDefsByField := (DataFieldCount = FieldDefs.Count)
      else
        FFieldDefsByField := (OldFieldDefsCount = 0) and (FieldDefs.Count > 0);
    finally
      Dec(FAvoidReload);
    end;
end;

procedure TVirtualTable.Reload;
var
  OldActive: boolean;
  Stream: TMemoryStream;
begin
  if Data.RecordCount > 0 then begin
    OldActive := Active;
    Stream := TMemoryStream.Create;
    DisableControls;
    try
      InternalSaveToStream(Stream, False, True);
      Close;
      Clear;
    finally
      LoadFromStream(Stream, False);
      Active := OldActive;
      Stream.Free;
      EnableControls;
    end;
  end
  else begin
    OldActive := Active;
    Close;
    Clear;
    Active := OldActive;
  end;
end;

{$IFDEF FPC}
procedure TVirtualTable.FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
begin
  case Operation of
    ooChange, ooAddItem, ooDeleteItem:
      DefChanged(FieldDefs);
  end;
end;
{$ENDIF}

procedure TVirtualTable.DefChanged(Sender: TObject);
var
  FieldDef: TFieldDef;
  i: integer;
begin
  if not FAvoidRefreshData then begin
    if Active then
      FFieldDefsByField := False;
    for i := 0 to TFieldDefs(Sender).Count - 1 do begin
      FieldDef := TFieldDefs(Sender)[i];
      if FieldDef.DataType = ftUnknown then begin
        FAvoidRefreshData := True;
        FieldDef.DataType := ftString;
        FieldDef.Size := 20;
        FAvoidRefreshData := False;
      end;
      if FieldDef.DataType = ftGuid then begin
        FAvoidRefreshData := True;
        FieldDef.Size := 38;
        FAvoidRefreshData := False;
      end;
    end;

    if FAvoidReload = 0 then
      Reload;
  end;
end;

{$IFDEF FPC}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: PtrInt);
{$ELSE}
{$IFDEF VER16P}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: NativeInt);
{$ELSE}
procedure TVirtualTable.DataEvent(Event: TDataEvent; Info: Longint);
{$ENDIF}
{$ENDIF}
begin
  if FFieldDefsByField and (Event = deFieldListChange) and (FAvoidReload = 0) and not VTOldBehavior then begin
    Inc(FAvoidReload);
    try
      FieldDefs.Updated := False;
      if Data.Active {and (voPersistentData in FOptions)} then
        Reload;
    finally
      Dec(FAvoidReload);
    end;
  end;
  inherited DataEvent(Event, Info);
end;

procedure TVirtualTable.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TVirtualTable then begin
    Stream := TMemoryStream.Create;
    try
      TVirtualTable(Source).SaveToStream(Stream);
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    FFieldDefsByField := TVirtualTable(Source).FFieldDefsByField;
  end
  else
  if Source is TDataSet then
    AssignDataSet(TDataSet(Source))
  else
    inherited;
end;

procedure TVirtualTable.AssignDataSet(Source: TDataSet);

  procedure InternalCreateFieldDefs(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    Field: TField;
    FieldDef: TFieldDef;
    SourceFieldDef: TFieldDef;
    NewDataType: TFieldType;
    FieldDefIndex: integer;
  begin
    FieldDefs.BeginUpdate;
    try
      for I := 0 to Fields.Count - 1 do begin
        Field := Fields[I];

        FieldDefIndex := FieldDefs.IndexOf(Field.FieldName);
        if FieldDefIndex <> -1 then
          SourceFieldDef := Source.FieldDefs[FieldDefIndex]
        else
          SourceFieldDef := nil; // For non fkData fields, create FieldDef without the [faFixed] attribute

        case Field.DataType of
          ftOraBlob: NewDataType := ftBlob;
          ftOraClob: NewDataType := ftMemo;
        {$IFDEF VER9P}
          ftOraTimeStamp: NewDataType := ftTimeStamp;
        {$ENDIF}
        else
          NewDataType := Field.DataType;
        end;

        if NewDataType in SupportFieldTypes then begin
          FieldDef := FieldDefs.AddFieldDef;
          FieldDef.Name := Field.FieldName;
          FieldDef.DataType := NewDataType;
          FieldDef.Size := Field.Size;
          if Field.Required then
            FieldDef.Attributes := [faRequired];
          if ReadOnly then
            FieldDef.Attributes := FieldDef.Attributes + [faReadonly];

          if SourceFieldDef <> nil then // for non fkData fields the [faFixed] attribute is not set
            if faFixed in SourceFieldDef.Attributes then
              FieldDef.Attributes := FieldDef.Attributes + [faFixed];

          if (Field.DataType = ftBCD) and (Field is TBCDField) then
            FieldDef.Precision := TBCDField(Field).Precision;

        {$IFDEF FPC}
          if (Field.DataType = ftLargeint) and (Field is TLargeIntField) then
            FieldDef.Precision := 0;
          if (Field.DataType = ftSmallint) and (Field is TSmallintField) then
            FieldDef.Precision := 0;
        {$ENDIF}

        {$IFNDEF FPC}
          if Field is TObjectField then
            InternalCreateFieldDefs(TObjectField(Field).Fields, FieldDef.ChildDefs);
        {$ENDIF}
        end
        else
        if not (voSkipUnSupportedFieldTypes in FOptions) then
          DatabaseError(SNotSupportFieldType);
      end;

    finally
      FieldDefs.EndUpdate;
    end;
  end;

var
  i: integer;
  OldActive: boolean;
  Bookmark: {$IFNDEF FPC}{$IFDEF VER12P}TBytes{$ELSE}string{$ENDIF}{$ELSE}TBytes{$ENDIF};
  SourceField: TField;

  FieldsRO: array of boolean;
  FieldsRQ: array of boolean;
  Value: variant;
  FieldsMap: array of TField;
begin
  OldActive := Active;
  Close;
  Clear;
  DeleteFields;

  InternalCreateFieldDefs(Source.Fields, FieldDefs);

  if Source.Active then begin
    DisableControls;
    Source.DisableControls;
    Bookmark := Source.Bookmark;
    Source.First;

    Open;

    // Temporary clear Field.ReadOnly and Field.Required flags
    SetLength(FieldsRO, Fields.Count);
    SetLength(FieldsRQ, Fields.Count);
    for i := 0 to Fields.Count - 1 do begin
      FieldsRO[i] := Fields[i].ReadOnly;
      Fields[i].ReadOnly := False;
      FieldsRQ[i] := Fields[i].Required;
      Fields[i].Required := False;
    end;

    try
      SetLength(FieldsMap, Fields.Count);
      for i := 0 to Fields.Count - 1 do
        FieldsMap[i] := Source.FieldByName(Fields[i].FieldName);

      while not Source.EOF do begin
        Append;
        for i := 0 to Fields.Count - 1 do begin
          SourceField := FieldsMap[i];
          if not SourceField.IsNull then
            if Fields[i] is TLargeIntField then
              TLargeIntField(Fields[i]).AsLargeInt := TLargeIntField(SourceField).AsLargeInt
          {$IFDEF FPC}
            else
            if Fields[i] is TTimeField then
              TTimeField(Fields[i]).AsString := TTimeField(SourceField).AsString
            else
            if Fields[i] is TBinaryField then
              Fields[i].AsString := SourceField.AsString
          {$ENDIF}
            else begin
              // To avoid memory leaks
              Value := Unassigned;
              Value := SourceField.Value;
              Fields[i].Value := Value;
            end;
        end;
        Post;
        Source.Next;
      end;
    finally
      First;

      // Restore Field.ReadOnly and Field.Required flags
      for i := 0 to Fields.Count - 1 do begin
        Fields[i].ReadOnly := FieldsRO[i];
        Fields[i].Required := FieldsRQ[i];
      end;

      if Source.RecordCount > 0 then
        Source.Bookmark := Bookmark;

      Source.EnableControls;
      EnableControls
    end;
  end;
  Active := OldActive;
end;

procedure TVirtualTable.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData,
    voStored in FOptions);
end;

procedure TVirtualTable.ReadBinaryData(Stream: TStream);
begin
  if voStored in FOptions then begin
    FRecordDataStream.Clear;
    FRecordDataStream.CopyFrom(Stream, Stream.Size - Stream.Position);
  end;
end;

procedure TVirtualTable.WriteBinaryData(Stream: TStream);
begin
  if voStored in FOptions then
    InternalSaveToStream(Stream, False, True);
end;

function TVirtualTable.IsSequenced: boolean;
begin
  Result := True;
end;

procedure TVirtualTable.AddField(const Name: string; FieldType: TFieldType; Size: integer; Required: boolean);
begin
  if not (FieldType in SupportFieldTypes) then
    DatabaseError(SNotSupportFieldType);

  FieldDefs.Add(Name, FieldType, Size, Required);
end;

procedure TVirtualTable.DeleteField(const Name: string);
var
  Stream: TMemoryStream;
  OldActive: boolean;
  FieldDef: TFieldDef;
begin
  FieldDef := TFieldDef(FieldDefs.Find(Name));
  if VTOldBehavior then begin
    OldActive := Active;
    Stream := TMemoryStream.Create;
    try
      InternalSaveToStream(Stream, False, True);
      Close;
      Clear;

      FieldDef.Free;
      //FieldDefs.Delete(FieldDef.Index);
    finally
      LoadFromStream(Stream, False);
      Active := OldActive;
      Stream.Free;
    end;
  end
  else
    FieldDef.Free;
end;

procedure TVirtualTable.DeleteFields;
begin
  Clear;
  FieldDefs.Clear;
  Fields.Clear;
end;

procedure TVirtualTable.Clear;
begin
  if State in [dsInsert,dsEdit] then
    Cancel;
  Data.Close;
  if Active then begin
    Data.Open;
    Resync([]);
  end;
end;

{ Stream/File }

{ Storage format:
  Version        2 // 0 = 2.00, 1 = 2.10, 2 = 5.10.1.8 (Blob storage)
-- FieldDefs
  FieldCount     2
    NameLength   2
    Name         Length(Name)
    DataType     2
    Size         2

-- Fields
  FieldCount     2              -|
    NameLength   2               |
    Name         Length(Name)    | for 1
    Kind         2               |
    DataType     2               |
    Size         2              -|

  RecordCount    4
    Size         2 (4 from Version = 2)
    Value        Size
}

type
{$IFNDEF FPC}
  TStringListQ = class(TStringList)
  protected
    function CompareStrings(const S1, S2: string): Integer; override;
  end;

function TStringListQ.CompareStrings(const S1, S2: string): Integer; // +10% to performance
begin
  if S1 > S2 then
    Result := 1
  else
  if S1 < S2 then
    Result := -1
  else
    Result := 0;
end;
{$ELSE}
  TStringListQ = TStringList;
{$ENDIF}


procedure TVirtualTable.LoadFromStream(Stream: TStream; LoadFields: boolean = True; DecodeHTMLEntities: boolean = True);
var
  LocFieldDefs: TFieldDefs;
  FieldAliases: TStringList;
  Reader: XMLTextReader;
  Version: word;

  function DetectFileFormat: TCRFileFormat;
  var
    Signature: TBytes;
    Offset: integer;
  begin
    Result := ffVTD;
    Stream.Position := 0;
    SetLength(Signature, 5);
    Stream.ReadBuffer(Signature[0], 5);
    Stream.Position := 0;

    if (Signature[0] = $EF) and (Signature[1] = $BB) and (Signature[2] = $BF) then begin // UTF8 preamble
      Stream.Position := 3;
      Stream.ReadBuffer(Signature[0], 5);
      Stream.Position := 3;
    end;

    if Signature[0] <> Byte('<') then
      Exit;

    if Signature[1] = Byte('?') then
      Offset := 1
    else
      Offset := 0;
    if
      ((Signature[1 + Offset] <> Byte('x')) and (Signature[1 + Offset] <> Byte('X'))) or
      ((Signature[2 + Offset] <> Byte('m')) and (Signature[2 + Offset] <> Byte('M'))) or
      ((Signature[3 + Offset] <> Byte('l')) and (Signature[3 + Offset] <> Byte('L')))
    then
      Exit;

    Result := ffXML;
  end;

  procedure ReadFieldDefsVTD;
  var
    FieldCount: word;
    i: integer;
    D2: word;
    FieldName: TBytes;
    FieldType: word;
    FieldSize: word;
    FieldPrecision: integer;
    FieldDef: TFieldDef;
  begin
    Stream.Read(FieldCount, 2);
    for i := 0 to FieldCount - 1 do begin
      Stream.Read(D2, 2);
      SetLength(FieldName, D2);
      Stream.ReadBuffer(FieldName[0], D2);
      Stream.Read(FieldType, 2);

      Stream.Read(FieldSize, 2);

      if Version >= 3 then
        Stream.Read(FieldPrecision, 4);

      LocFieldDefs.Add(Encoding.Default.GetString(FieldName), TFieldType(FieldType), FieldSize, False);
      FieldDef := LocFieldDefs.Items[LocFieldDefs.Count - 1];

      if TFieldType(FieldType) in [ftSmallint, ftInteger, ftLargeint, ftWord,
                                   {$IFDEF VER12P}ftByte, ftShortInt, ftLongWord, ftExtended,{$ENDIF}
                                   ftFloat, ftCurrency, {$IFDEF VER14P}ftSingle,{$ENDIF}
                                   {$IFNDEF FPC}ftTimeStamp,{$ENDIF}
                                   ftDate, ftTime, ftDateTime]
      then
        FieldDef.Precision := FieldPrecision;
    end;
  end;

  procedure ReadFieldDefsXML;
    function FieldTypeFromXML(const FieldType, FieldDBType: string;
      const IsLong, FixedLength: boolean): TFieldType;
    var
      InternalType: Word;
    begin
      if (FieldType = 'i8') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt64
      else
      if (FieldType = 'bin.hex') and (FieldDBType = '') then
        if IsLong then
          InternalType := dtBlob
        else
          InternalType := dtBytes
      else
      if (FieldType = 'boolean') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtBoolean
      else
      if (FieldType = 'string') and (FieldDBType = 'variant') then
        InternalType := dtVariant
      else
      if (FieldType = 'string') and (FieldDBType = '') then
        if IsLong then
          InternalType := {$IFDEF VER10P}dtWideMemo{$ELSE}{$IFDEF FPC}dtWideMemo{$ELSE}dtMemo{$ENDIF}{$ENDIF}
        else
          InternalType := dtWideString
      else
      if (FieldType = 'string') and ((FieldDBType = 'str') or (FieldDBType = 'string')) then
        if IsLong then
          InternalType := dtMemo
        else
          InternalType := dtString
      else
      if (FieldType = 'i8') and (FieldDBType = 'currency') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'number') and (FieldDBType = 'currency') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'datetime') and (FieldDBType = 'variantdate') {and FixedLength} then
        InternalType := dtDateTime
      else
      if (FieldType = 'date') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtDate
      else
      if (FieldType = 'time') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtTime
      else
      if (FieldType = 'datetime') and (FieldDBType = 'timestamp') {and FixedLength} then
        InternalType := dtDateTime
      else
      if (FieldType = 'number') and (FieldDBType = 'decimal') {and FixedLength} then
        InternalType := dtCurrency
      else
      if (FieldType = 'float') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtFloat
      else
      if (FieldType = 'extended') and (FieldDBType = '') {and FixedLength} then
      {$IFDEF VER12P}
        InternalType := dtExtended
      {$ELSE}
        InternalType := dtFloat
      {$ENDIF}
      else
      if (FieldType = 'uuid') and (FieldDBType = ''){ and FixedLength} then
        InternalType := dtGuid
      else
      if (FieldType = 'number') and (FieldDBType = 'numeric') {and FixedLength} then
        InternalType := dtFmtBCD
      else
      if (FieldType = 'r4') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtSingle
      else
      if (FieldType = 'i2') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt16
      else
      if (FieldType = 'ui2') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtWord
      else
      if (FieldType = 'i1') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt8
      else
      if (FieldType = 'ui1') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtUInt8
      else
      if (FieldType = 'int') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt32
      else
      if (FieldType = 'ui4') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtUInt32
      else
      if (FieldType = 'ui8') and (FieldDBType = '') {and FixedLength} then
        InternalType := dtInt64
      else begin
        DatabaseError(SDataTypeNotSupported, Self);
        InternalType := 0; // to prevent compiler warning
      end;

      Result := GetFieldType(InternalType);
    end;

  var
    i: integer;
    AttrName, AttrValue: string;
    AttributeCount: integer;
    FieldDef: TFieldDef;
    FieldName: string;
    FieldAlias: string;
    FieldDataType: TFieldType;
    FieldSize: integer;
    FieldPrecision: integer;
    FieldScale: integer;
    FieldRequired: boolean;
    FieldFixed: boolean;
    FieldType, FieldDBType: string;
    FieldIsLong: boolean;
    HaveLength: boolean;
    TmpValue: string;
  begin
    while Reader.Read do begin
      if (UpperCase(Reader.FullName) = 'S:SCHEMA') and (Reader.NodeType = ntEndElement) then
        break;
      if (UpperCase(Reader.FullName) = 'S:ATTRIBUTETYPE') and (Reader.NodeType <> ntEndElement) then begin
        AttributeCount := Reader.AttributeCount;
        FieldAlias := '';
        for i := 0 to AttributeCount - 1 do begin
          Reader.MoveToAttribute(i);
          if LowerCase(Reader.FullName) = 'name' then
            FieldName := Reader.Value;
          if LowerCase(Reader.FullName) = 'rs:name' then
            FieldAlias := Reader.Value;
        end;
        if FieldAlias <> '' then begin
          TmpValue := FieldName;
          FieldName := FieldAlias;
          FieldAlias := TmpValue;
          FieldAliases.Add(FieldName + '=' + FieldAlias);
        end;
        while not ((UpperCase(Reader.FullName) = 'S:DATATYPE') and (Reader.NodeType <> ntEndElement)) do begin
          Reader.Read;
          if Reader.EOF then
            raise Exception.Create(SInvalidXML);
        end;
        FieldSize := 0;
        FieldPrecision := 0;
        FieldScale := 0;
        FieldType := '';
        FieldDBType := '';
        FieldIsLong := False;
        FieldRequired := False;
        FieldFixed := False;
        HaveLength := False;
        AttributeCount := Reader.AttributeCount;
        for i := 0 to AttributeCount - 1 do begin
          Reader.MoveToAttribute(i);
          AttrName := LowerCase(Reader.FullName);
          AttrValue := Reader.Value;

          if AttrName = 'rs:fixedlength' then
            FieldFixed := StrToBool(AttrValue)
          else
          if AttrName = 'rs:maybenull' then
            FieldRequired := not StrToBool(AttrValue)
          else
          if AttrName = 'dt:maxlength' then begin
            FieldSize := Integer(Round(StrToFloat(AttrValue)) and $7FFFFFFF);
            HaveLength := True;
          end
          else
          if AttrName = 'rs:precision' then
            FieldPrecision := StrToInt(AttrValue)
          else
          if AttrName = 'rs:scale' then
            FieldScale := StrToInt(AttrValue)
          else
          if AttrName = 'dt:type' then
            FieldType := AnsiLowerCase(AttrValue)
          else
          if AttrName = 'rs:dbtype' then
            FieldDBType := AnsiLowerCase(AttrValue)
          else
          if AttrName = 'rs:long' then
            FieldIsLong := StrToBool(AttrValue);
        end;

        if (FieldType = '') and (FieldDBType = '') then
          raise Exception.Create(SInvalidXML);

        if not HaveLength and (FieldDBType = '') then
          FieldDBType := 'variant';
        FieldDataType := FieldTypeFromXML(FieldType, FieldDBType, FieldIsLong, FieldFixed);

        if not (FieldDataType in [ftString, ftWideString, ftVariant, ftGuid, ftBcd, ftFmtBcd,
          ftBytes, ftVarBytes, ftBlob, ftMemo, ftFixedChar{$IFNDEF FPC}, ftTimeStamp{$ENDIF}
          {$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}]) then
          FieldSize := 0;

        if FieldIsLong then
          FieldSize := 0;

        if (FieldDataType = ftBytes) and not FieldFixed then
          FieldDataType := ftVarBytes;

        LocFieldDefs.Add(FieldName, FieldDataType, FieldSize, FieldRequired);
        FieldDef := LocFieldDefs.Items[LocFieldDefs.Count - 1];

        if FieldDataType in [ftInteger, ftLargeint, ftSmallint, ftWord,
                             {$IFDEF VER12P}ftByte, ftShortInt, ftLongWord, ftExtended,{$ENDIF}
                             ftFloat, ftCurrency, {$IFDEF VER14P}ftSingle,{$ENDIF}
                             {$IFNDEF FPC}ftTimeStamp,{$ENDIF}
                             ftDate, ftTime, ftDateTime]
        then
          FieldDef.Precision := FieldPrecision
        else
        if FieldDataType in [ftBCD, ftFMTBCD] then begin
          FieldDef.Precision := FieldPrecision;
          FieldDef.Size := FieldScale;
        end;

        if FieldFixed then
          FieldDef.Attributes := FieldDef.Attributes + [DB.faFixed];
      end;
    end;
  end;

  procedure SetXMLValueToField(Field: TField; const FieldValue: string);
  var
    Year: integer;
    Month: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;

    function GetNext(const Value: string; Offset: integer; Digits: Integer): string;
    var
      i: Integer;
    begin
      SetLength(Result, Digits);
      for i := 1 to Digits do begin
        if (Value[Offset + i] >= '0') and (Value[Offset + i] <= '9') then
          Result[i] := Value[Offset + i]
        else
          raise Exception.Create(SInvalidXML);
      end;
    end;

    procedure ConvertDate(const Value: string);
    var
      Offset: integer;
    begin
      Offset := 0;
      if Value[1] = '-' then
        Inc(Offset);
      Year := StrToInt(GetNext(Value, Offset, 4));
      Inc(Offset, 5);
      Month := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Day := StrToInt(GetNext(Value, Offset, 2));
    end;

    procedure ConvertTime(const Value: string);
    var
      Offset: integer;
    begin
      if Length(Value) < 8 then
        raise Exception.Create(SInvalidXML);
      Offset := 0;
      Hour := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Minute := StrToInt(GetNext(Value, Offset, 2));
      Inc(Offset, 3);
      Second := StrToInt(GetNext(Value, Offset, 2));
    end;

    function DecodeXMLDateTime(const XMLDateTime: string): TDateTime;
    var
      TimePosition: integer;
    begin
      TimePosition := Pos('T', XMLDateTime);
      if TimePosition > 0 then begin
        ConvertDate(Copy(XMLDateTime, 1, TimePosition -1));
        ConvertTime(Copy(XMLDateTime, TimePosition + 1, Length(XMLDateTime) - TimePosition));
      end
      else begin
        Hour := 0;
        Minute := 0;
        Second := 0;
        ConvertDate(XMLDateTime);
      end;
      Result := MemUtils.EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
    end;

    function DecodeXMLTime(const XMLTime: string): TDateTime;
    begin
      Year := 1000;
      Month := 1;
      Day := 1;
      ConvertTime(XMLTime);
      Result := MemUtils.EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
    end;

  var
    FieldDesc: TFieldDesc;
    Bcd: TBCD;
    TmpBcd: TBCD;
    FieldLength, FieldScale: integer;
  {$IFDEF VER9P}
    Delta: word;
  {$ENDIF}
    Buffer: TBytes;
    i: Integer;
    IsValidChar: boolean;
    TextOffset, BuffOffset, Count: integer;
    StrValue: string;
  begin
    FieldDesc := GetFieldDesc(Field);
    case FieldDesc.DataType of
      dtBoolean:
        Field.AsBoolean := StrToBool(FieldValue);
      dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32:
        Field.AsString := FieldValue;
      dtSingle: begin
        StrValue := FieldValue;
        ChangeDecimalSeparator(StrValue, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        Field.{$IFDEF VER14P}AsSingle{$ELSE}AsFloat{$ENDIF} := StrToFloat(StrValue);
      end;
      dtFloat: begin
        StrValue := FieldValue;
        ChangeDecimalSeparator(StrValue, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        Field.AsFloat := StrToFloat(StrValue);
      end;
      dtExtended: begin
        StrValue := FieldValue;
        ChangeDecimalSeparator(StrValue, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        Field.{$IFDEF VER12P}AsExtended{$ELSE}AsFloat{$ENDIF} := StrToFloat(StrValue);
      end;
      dtDate, dtDateTime:
        Field.AsDateTime := DecodeXMLDateTime(FieldValue);
      dtTime:
      {$IFNDEF FPC}
        Field.AsDateTime := DecodeXMLTime(FieldValue);
      {$ELSE}
        Field.AsString := FieldValue;
      {$ENDIF}
      dtCurrency, dtBcd: begin
        StrValue := FieldValue;
        ChangeDecimalSeparator(StrValue, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        Field.AsCurrency := StrToCurr(StrValue);
      end;
      dtFmtBCD: begin
        StrValue := FieldValue;
        ChangeDecimalSeparator(StrValue, '.', {$IFDEF USE_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator);
        BCD := StrToBCD(StrValue);
        FieldLength := FieldDesc.Length;
        FieldScale := FieldDesc.Scale;
      {$IFDEF VER9P} // Delphi 9 NormalizeBcd Bug
        Delta := FieldLength - FieldScale;
        if Delta > 34 then begin
          Delta := 34;
          FieldLength := FieldScale + Delta;
        end;
      {$ENDIF}
        CRFunctions.NormalizeBcd(Bcd, TmpBcd, FieldLength, FieldScale);
        Field.AsBCD := TmpBcd;
      end;
      dtBlob, dtBytes, dtVarBytes, dtExtVarBytes: begin
        StrValue := FieldValue;
        TextOffset := 0;
        BuffOffset := 0;
        SetLength(Buffer, Length(FieldValue) div 2);
        for i := 1 to Length(FieldValue) do begin
          IsValidChar := not ((FieldValue[i] = #$D) or (FieldValue[i] = #$A) or (FieldValue[i] = #9));
          if ((not IsValidChar) or (i = Length(FieldValue))) then begin
            Count := i - TextOffset;
            if not IsValidChar then
              Dec(Count);
            if Count > 0 then begin
              HexToBin(PChar(PtrOffset(@StrValue[1], TextOffset)), PAnsiChar(PtrOffset(@Buffer[0], BuffOffset)), Count);
              Inc(BuffOffset, Count div 2);
            end;
            TextOffset := i;
          end;
        end;
        if Length(Buffer) > BuffOffset then
          SetLength(Buffer, BuffOffset);
      {$IFDEF USE_ASBYTES}
        if FieldDesc.DataType <> dtBlob then begin
      {$ENDIF}
          Field.{$IFDEF USE_ASANSISTRING}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.{$IFDEF USE_ASANSISTRING}GetAnsiString{$ELSE}GetString{$ENDIF}(Buffer);
      {$IFDEF USE_ASBYTES}
        end
        else
          Field.AsBytes := Buffer;
      {$ENDIF}
      end;
      else
        if Field is TWideStringField then
          TWideStringField(Field).Value := WideString(FieldValue)
        else
          Field.AsString := FieldValue;
    end;
  end;

  procedure ProcessXMLData;
  var
    AttributeCount: integer;
    p, i, j: integer;
    Field: TField;
    FieldList: TStringList;
    FieldIndex: integer;
    ActualName, Alias: string;
  begin
    FieldList := TStringListQ.Create;
    try
      FieldList.CaseSensitive := True;
      FieldList.Sorted := True;
      while Reader.Read do
        if (UpperCase(Reader.FullName) = 'Z:ROW') and (Reader.NodeType <> ntEndElement) then begin
          Append;
          try
            AttributeCount := Reader.AttributeCount;
            for i := 0 to AttributeCount - 1 do begin
              Reader.MoveToAttribute(i);
              FieldIndex := FieldList.IndexOf(Reader.FullName);
              if FieldIndex > -1 then
                Field := FieldList.Objects[FieldIndex] as TField
              else begin
                Field := FindField(Reader.FullName);
                if Field = nil then
                  for j := 0 to FieldAliases.Count - 1 do begin
                    p := Pos('=', FieldAliases[j]);
                    if p > 0 then begin
                      ActualName := AnsiLowerCase(Copy(FieldAliases[j], 0, p - 1));
                      Alias := Copy(FieldAliases[j], p + 1, Length(FieldAliases[j]) - p);
                      if Reader.FullName = Alias then begin
                        Field := FindField(ActualName);
                        if Field <> nil then
                          break;
                      end;
                    end;
                  end;
                if Field <> nil then
                  FieldList.AddObject(Reader.FullName, Field);
              end;
              if Field = nil then
                raise Exception.Create(SInvalidXML);
              SetXMLValueToField(Field, Reader.Value);
            end;
            Post;
          except
            Cancel;
            raise;
          end;
        end;
    finally
      FieldList.Free;
    end;
  end;

var
  D2: word;
  D4: cardinal;
  FieldName: TBytes;
  FieldType: word;
  FieldSize: word;
  FieldKind: word;
  RecordCount: integer;
  i, j: integer;
  OldActive: boolean;
  OldAutoCalcFields: boolean;
  St: TBytes;
  WSt: TBytes;
  FieldClass: TFieldClass;
  Field: TField;
  FieldArr: array of TField;
{$IFNDEF VER17P}
  Handle: IntPtr;
{$ENDIF}
  FileFormat: TCRFileFormat;
{$IFDEF FPC}
  Ts: TTimeStamp;
  Time: TDateTime;
{$ENDIF}
{$IFNDEF FPC}
  AutoGenerateValue: TAutoRefreshFlag;
{$ENDIF}  
  CurrentValue, InitialValue, Increment: integer;
begin
  Inc(FAvoidReload);
  FLoadingData := True;
  try
    OldActive := Active or FStreamedActive;

    Close;
    Clear;
    if LoadFields then begin
      LocFieldDefs := FieldDefs;
      DeleteFields;
    end
    else
      LocFieldDefs := TFieldDefs.Create(Self);

    Stream.Seek(LongInt(0), soFromBeginning);
    FileFormat := DetectFileFormat;

    Stream.Seek(LongInt(0), soFromBeginning);
    Stream.Read(Version, 2);  // Version

    Reader := nil;

    FieldAliases := nil;
    try
      // FieldDefs
      FAvoidRefreshData := True;
      try
        case FileFormat of
          ffVTD:
            ReadFieldDefsVTD;
          ffXML: begin
            FieldAliases := TStringList.Create;
            Stream.Position := 0;
            Reader := XMLTextReader.Create(Stream, DecodeHTMLEntities);
            ReadFieldDefsXML;
          end;
        end;
      finally
        FAvoidRefreshData := False;
      end;

      with Stream do begin
        if (FileFormat = ffVTD) and (Version >= 1) then begin
          // Fields
          Read(D2, 2);
          for i := 0 to D2 - 1 do begin
            Read(D2, 2);
            SetLength(FieldName, D2);
            Stream.ReadBuffer(FieldName[0], D2);
            Read(FieldKind, 2);
            Read(FieldType, 2);
          {$IFNDEF FPC}
            AutoGenerateValue := arNone;
          {$ENDIF}  
            InitialValue := -1;
            CurrentValue := 1;
            Increment := 1;
            if (Version >= 4) and (FieldType = ftVirtualAutoInc) then begin
              Read(D2, 2);
            {$IFNDEF FPC}
              AutoGenerateValue := TAutoRefreshFlag(D2);
            {$ENDIF}  
              Read(D4, 4);
              CurrentValue := D4;
              Read(D4, 4);
              InitialValue := D4;
              Read(D4, 4);
              Increment := D4;
            end;
            Read(FieldSize, 2);

            if LoadFields then begin
              if TFieldKind(FieldKind) = fkLookup then
                Continue;
              if (Version >= 4) and (FieldType = ftVirtualAutoInc) then
                FieldClass := TVirtualAutoIncField
              else if FieldType = Word(ftAutoInc) then
                FieldClass := TAutoIncField
              else
                FieldClass := GetFieldClass(TFieldType(FieldType));

              Field := FieldClass.Create(Self.Owner);//  Self);
              try
                Field.FieldName := Encoding.Default.GetString(FieldName);
                Field.FieldKind := TFieldKind(FieldKind);
                case TFieldType(FieldType) of
                  ftString, ftBytes, ftVarBytes:
                    Field.Size := FieldSize;
                  ftWideString:
                    Field.Size := FieldSize * sizeof(WideChar);
                end;
                if (Version >= 4) and (FieldType = ftVirtualAutoInc) then begin
                {$IFNDEF FPC}
                  Field.AutoGenerateValue := AutoGenerateValue;
                {$ENDIF}  
                  TVirtualAutoIncField(Field).InitialValue := InitialValue;
                  TVirtualAutoIncField(Field).FCurrentValue := CurrentValue;
                  TVirtualAutoIncField(Field).Increment := Increment;
                end;
                Field.DataSet := Self;
              except
                Field.Free;
                raise;
              end;
            end;
          end;
        end;
        RecordCount := 0;
        if FileFormat = ffVTD then
          Read(RecordCount, 4);

        if (FileFormat = ffXML) or ((FileFormat = ffVTD) and (RecordCount > 0)) then begin
          DisableControls;
          if not (csReading in ComponentState) then
            Open
          else begin
            DoBeforeOpen;
            try
              OpenCursor(False);
              SetState(dsBrowse);
            except
              SetState(dsInactive);
              CloseCursor;
              raise;
            end;
            DoAfterOpen;
            DoAfterScroll;
          end;

          OldAutoCalcFields := AutoCalcFields;
          AutoCalcFields := False;
          try
            case FileFormat of
              ffXML:
                ProcessXMLData;
              ffVTD: begin
                SetLength(FieldArr, LocFieldDefs.Count);
                for i := 0 to LocFieldDefs.Count - 1 do
                  FieldArr[i] := FindField(LocFieldDefs[i].Name);

                DoVirtualTableProgress(0);
                for j := 0 to RecordCount - 1 do begin
                  Append;
                  try
                    for i := 0 to LocFieldDefs.Count - 1 do begin
                      Field := FieldArr[i];

                      if Version < 2 then begin
                        Read(D2, 2);
                        D4 := D2;
                      end
                      else
                        Read(D4, 4);

                      if D4 > 0 then begin
                        if (Field <> nil) and (Field.DataType = ftWideString) then begin
                          SetLength(WSt, D4);
                          Stream.ReadBuffer(WSt[0], D4);
                          TWideStringField(Field).Value :=
                            Encoding.Unicode.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(WSt);
                        end
                        else begin
                          if (Field <> nil) and (Field.DataType = ftVarBytes) then begin
                            SetLength(St, D4 + 2);
                            D2 := D4;
                            St[0] := Lo(D2);
                            St[1] := Hi(D2);
                            Stream.ReadBuffer(St[2], D4);
                          end
                          else begin
                            SetLength(St, D4);
                            Stream.ReadBuffer(St[0], D4);
                          end;

                          if Field <> nil then
                            case Field.DataType of
                            {$IFNDEF USE_ASBYTES}
                              ftBlob,
                            {$ENDIF}
                              ftString, ftMemo:
                                Field.{$IFDEF USE_ASANSISTRING}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.{$IFDEF USE_ASANSISTRING}GetAnsiString{$ELSE}GetString{$ENDIF}(St);
                            {$IFDEF USE_ASBYTES}
                              ftBlob:
                                TBlobField(Field).AsBytes := St;
                            {$ENDIF}
                            {$IFDEF VER10P}
                              ftWideMemo:
                                TWideMemoField(Field).Value :=
                                  Encoding.Unicode.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(St);
                            {$ENDIF}
                            {$IFDEF FPC}
                              ftWideMemo:
                                TWideMemoField(Field).Value :=
                                  Encoding.Unicode.{$IFDEF IS_UNICODE}GetString{$ELSE}GetWideString{$ENDIF}(St);
                            {$ENDIF}
                              ftVariant: begin
                                Read(D2, 2);
                                case D2 of
                                  varBoolean: begin
                                    Field.AsBoolean := WordBool(Marshal.ReadInt16(St));
                                  end;
                                  varShortInt, varByte:
                                    Field.AsInteger := Marshal.ReadByte(St);
                                  varSmallInt, varWord:
                                    Field.AsInteger := Marshal.ReadInt16(St);
                                  varInteger:
                                    Field.AsInteger := Marshal.ReadInt32(St);
                                  varLongWord, varInt64:
                                    Field.Value := Marshal.ReadInt64(St);
                                  varDouble:
                                    Field.AsFloat := Double(Pointer(@St[0])^);
                                  varCurrency:
                                    Field.AsCurrency := Currency(Pointer(@St[0])^);
                                  varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
                                    SetLength(St, D4 - 2{#0});
                                    Field.Value := Encoding.Unicode.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(St);
                                  end;
                                  varString: begin
                                    SetLength(St, D4 - 1{#0});
                                    Field.{$IFDEF USE_ASANSISTRING}AsAnsiString{$ELSE}AsString{$ENDIF} := Encoding.Default.{$IFDEF USE_ASANSISTRING}GetAnsiString{$ELSE}GetString{$ENDIF}(St)
                                  end;
                                  else
                                    raise Exception.Create(Format(SVarSubtypeNotSupported, [D2]));
                                end;
                              end;
                            else
                            {$IFDEF VER17P}
                              Field.SetData(St);
                            {$ELSE}
                              Handle := AllocGCHandle(St, True);
                              try
                              {$IFNDEF FPC}
                                Field.SetData(GetAddrOfPinnedObject(Handle));
                              {$ELSE}
                                if Field.DataType = ftTime then begin
                                  Ts.Date := DateDelta;
                                  Ts.Time := Marshal.ReadInt32(Handle);
                                  Time := MemUtils.TimeStampToDateTime(Ts);
                                  Field.SetData(@Time);
                                end
                                else
                                  Field.SetData(Handle);
                              {$ENDIF}
                              finally
                                FreeGCHandle(Handle);
                              end;
                            {$ENDIF}
                            end;
                        end;
                      end;
                    end;
                  finally
                    Post;
                  end;
                  DoVirtualTableProgress(Round((j * 100) / RecordCount));
                end;
              end;
              else
                Assert(False);
            end;
          finally
            AutoCalcFields := OldAutoCalcFields;
            First;
            EnableControls;
          end;

          if not OldActive and (voPersistentData in FOptions) then
            if csReading in ComponentState then begin
              SetState(dsInactive);
              CloseCursor;
            end
            else
              Close;
        end
        else
          Active := OldActive;
      end;

    finally
      if LocFieldDefs <> FieldDefs then
        LocFieldDefs.Free;
      FieldAliases.Free;
      Reader.Free;
    end;
  finally
    Dec(FAvoidReload);
    FLoadingData := False;
  end;
end;

procedure TVirtualTable.InternalSaveToStream(Stream: TStream; StoreFields: boolean; StoreAllData: boolean);

  procedure AssignFields(Dest: TFields; Source: TFields);
  var
    Field: TField;
  begin
    Dest.Clear;
    while Source.Count > 0 do begin
      Field := Source[0];
      Source.Remove(Field);
      Dest.Add(Field);
    end;
  end;

var
  D2: word;
  D4: cardinal;
  St: AnsiString;
  i: integer;
  OldRecNo: integer;
  OldActive: boolean;
  OldMasterSource: TDataSource;
  OldFiltered: boolean;
  TempFields: TFields;
  Field: TField;
  FieldDesc: TFieldDesc;
  FieldArr: array of TField;
  FieldDescArr: array of TFieldDesc;
  Buffer: TBytes;
  pBuffer: IntPtr;
  BufferLen: Word;
  RecBuf: TRecordBuffer;
  IsNull: boolean;
  Piece: PPieceHeader;
  BufLen: integer;
  Blob: TBlob;
  BlobBuffer: TBytes;
  Handle: IntPtr;
  p: IntPtr;
  Offset: integer;
  v: variant;
begin
  p := nil;
  Inc(FAvoidReload);
  try
    OldActive := Active;
    OldMasterSource := MasterSource;
    OldFiltered := Filtered;

    with Stream do begin
      D2 := 4;
      Write(D2, 2); // Version 0 - 2.00 1 - 2.10

      // FieldDefs
      D2 := FieldDefs.Count;
      Write(D2, 2);
      for i := 0 to FieldDefs.Count - 1 do begin
        D2 := Length(FieldDefs[i].Name);
        Write(D2, 2);
        St := AnsiString(FieldDefs[i].Name);
        Write(PAnsiChar(St)^, LengthA(St));
        D2 := Word(FieldDefs[i].DataType);
        Write(D2, 2);
        D2 := FieldDefs[i].Size;
        Write(D2, 2);
        D4 := FieldDefs[i].Precision;
        Write(D4, 4);
      end;

    // Fields
    {$IFDEF VER20P}
      if not (lcPersistent in Fields.LifeCycles) or
    {$ELSE}
      if DefaultFields or
    {$ENDIF}
         not StoreFields
      then begin
        D2 := 0;
        Write(D2, 2);
      end
      else begin
        D2 := FieldCount;
        for i := 0 to FieldCount - 1 do
          if Fields[i].FieldKind = fkLookup then Dec(D2);
        Write(D2, 2);
        for i := 0 to FieldCount - 1 do begin
          if Fields[i].FieldKind = fkLookup then
            Continue;
          D2 := Length(Fields[i].FieldName);
          Write(D2, 2);
          St := AnsiString(Fields[i].FieldName);
          Write(PAnsiChar(St)^, LengthA(St));
          D2 := Word(Fields[i].FieldKind);  // for ver 1
          Write(D2, 2);
          D2 := Word(Fields[i].DataType);
          if (D2 = Word(ftAutoInc)) and (Fields[i] is TVirtualAutoIncField) then begin
            D2 := Word(ftVirtualAutoInc);
            Write(D2, 2);
            D2 := {$IFNDEF FPC}Word(Fields[i].AutoGenerateValue){$ELSE}1{$ENDIF};
            Write(D2, 2);
            D4 := TVirtualAutoIncField(Fields[i]).FCurrentValue;
            Write(D4, 4);
            D4 := TVirtualAutoIncField(Fields[i]).InitialValue;
            Write(D4, 4);
            D4 := TVirtualAutoIncField(Fields[i]).Increment;
            Write(D4, 4);
          end
          else
            Write(D2, 2);
          D2 := Fields[i].Size;
          Write(D2, 2);
        end;
      end;

      if FieldDefs.Count = 0 then begin
        D4 := 0;
        Write(D4, 4);
      end
      else begin
        DisableControls;
        if Active then
          OldRecNo := RecNo
        else
          OldRecNo := -1;

        if not Active and {$IFDEF VER20P}(lcPersistent in Fields.LifeCycles){$ELSE}not DefaultFields{$ENDIF} then begin
          TempFields := TFields.Create(nil);
          AssignFields(TempFields, Fields);
          Fields.Clear;
        end
        else
          TempFields := nil;

        Open;
        if StoreAllData then begin
          MasterSource := nil;
          Filtered := False;
        end;
        First;

        BufLen := 0;
        SetLength(FieldArr, FieldDefs.Count);
        SetLength(FieldDescArr, FieldDefs.Count);
        for i := 0 to FieldDefs.Count - 1 do begin
          FieldArr[i] := FindField(FieldDefs[i].Name);
          FieldDescArr[i] := Data.FindField(FieldDefs[i].Name);
          if (FieldDescArr[i] <> nil) and (FieldDescArr[i].Size > BufLen) then
            BufLen := FieldDescArr[i].Size;
        end;

        SetLength(Buffer, BufLen);
        Handle := AllocGCHandle(Buffer, True);
        pBuffer := GetAddrOfPinnedObject(Handle);

        DoVirtualTableProgress(0);
        try
          D4 := RecordCount;
          Write(D4, 4);
          while not EOF do begin
            for i := 0 to FieldDefs.Count - 1 do begin
              Field := FieldArr[i];
              FieldDesc := FieldDescArr[i];

              // get field desc and data from record buffer
              if FieldDesc <> nil then begin
                GetActiveRecBuf(RecBuf);
                if FieldDesc.DataType = dtVariant then
                  Data.GetField(FieldDesc, RecBuf, @v, BufferLen, False, IsNull)
                else
                  Data.GetField(FieldDesc, RecBuf, pBuffer, BufferLen, False, IsNull);
                D4 := BufferLen;
              end
              else
                IsNull := True;

              Blob := nil;
              Offset := 0;
              if (Field = nil) or (FieldDesc = nil) or IsNull then
                D4 := 0
              else begin
                // to write field data there must be Field and FieldDesc
                case FieldDesc.DataType of
                  dtString: begin
                    if D4 = 0 then
                      D4 := 1;
                  end;
                  dtWideString: begin
                    D4 := D4 * sizeof(WideChar);
                    if D4 = 0 then
                      D4 := 2;
                  end;
                  dtVarBytes:
                    Offset := 2;
                  dtInt8, dtInt16, dtInt32, dtInt64, dtUInt8, dtUInt16, dtUInt32,
                  dtBoolean, dtCurrency, dtSingle, dtFloat{$IFDEF VER14P}, dtExtended{$ENDIF}, dtGuid, dtBytes:
                    D4 := FieldDesc.Size;
                  dtDateTime, dtDate, dtTime:
                    D4 := SizeOf(TDateTime);
                {$IFNDEF FPC}
                  dtSQLTimeStamp:
                    D4 := SizeOf(TSQLTimeStamp);
                {$ENDIF}
                  dtBlob, dtMemo, dtWideMemo: begin
                    Blob := TBlob(GetGCHandleTarget(Marshal.ReadIntPtr(pBuffer)));
                    D4 := Blob.Size;
                  end;
                  dtVariant: begin
                    D2 := TVarData(v).VType;
                    case TVarData(v).VType of
                      varBoolean: begin
                        D4 := 2;
                        p := IntPtr(@TVarData(v).VBoolean);
                      end;
                      varSmallInt: begin
                        D4 := 2;
                        p := PSmallInt(@TVarData(v).VSmallInt);
                      end;
                      varInteger: begin
                        D4 := 4;
                        p := PInteger(@TVarData(v).VInteger);
                      end;
                      varShortInt: begin
                        D4 := 1;
                        p := PShortInt(@TVarData(v).VShortInt);
                      end;
                      varByte: begin
                        D4 := 1;
                        p := PByte(@TVarData(v).VByte);
                      end;
                      varWord: begin
                        D4 := 2;
                        p := PWord(@TVarData(v).VWord);
                      end;
                      varLongWord: begin
                        D4 := 4;
                        p := PCardinal(@TVarData(v).VLongWord);
                      end;
                      varInt64: begin
                        D4 := 8;
                        p := PInt64(@TVarData(v).VInt64);
                      end;
                      varCurrency: begin
                        D4 := 8;
                        p := PCurrency(@TVarData(v).VCurrency);
                      end;
                      varDouble: begin
                        D4 := 8;
                        p := PDouble(@TVarData(v).VDouble);
                      end;
                      varOleStr{$IFDEF VER12P}, varUString{$ENDIF}: begin
                        if TVarData(v).VOleStr = nil then begin
                          D4 := sizeof(WideChar);
                          p := PWideChar(EmptyWString);
                        end
                        else begin
                          D4 := PInteger(PAnsiChar(TVarData(v).VOleStr) - 4)^ * sizeof(WideChar) + sizeof(WideChar);
                          p := TVarData(v).VOleStr;
                        end;
                      end;
                      varString: begin
                        if TVarData(v).VString = nil then begin
                          D4 := 1;
                          p := PAnsiChar(EmptyString);
                        end
                        else begin
                          D4 := PInteger(PAnsiChar(TVarData(v).VString) - 4)^ + 1{#0};
                          p := TVarData(v).VString;
                        end;
                      end;
                    else
                      raise Exception.Create(Format(SVarSubtypeNotSupported, [TVarData(v).VType]));
                    end;
                  end;
                  dtFMTBCD:
                    D4 := SizeOfTBcd;
                  dtBCD:
                    D4 := SizeOf(Double);
                else
                  Assert(False, SUnknownDataType + ' FieldDesc.DataType=' + IntToStr(Integer(FieldDesc.DataType)));
                end;
              end;
              Write(D4, 4);
              if D4 > 0 then begin
                if FieldDesc.DataType in [dtBlob, dtMemo, dtWideMemo] then begin
                  // save blob to stream
                  Piece := Blob.FirstPiece;

                  while IntPtr(Piece) <> nil do begin
                    BufLen := Piece.Used;
                    SetLength(BlobBuffer, BufLen);
                    Marshal.Copy(PtrOffset(Piece, Sizeof(TPieceHeader)), BlobBuffer, 0, BufLen);
                    Stream.WriteBuffer(BlobBuffer[0], BufLen);
                    Piece := Piece.Next;
                  end;
                end
                else
                if FieldDesc.DataType = dtVariant then begin
                  WriteBuffer(Integer(p^), D4);
                  WriteBuffer(TVarData(v).VType, 2);
                end
                else
                  Stream.WriteBuffer(Buffer[Offset], D4);
              end;
            end;
            DoVirtualTableProgress(round((RecNo * 100) / RecordCount));
            Next;
          end;
        finally
          FreeGCHandle(Handle);
          if TempFields <> nil then begin
            Close;
            AssignFields(Fields, TempFields);
            TempFields.Free;
          end;

          MasterSource := OldMasterSource;
          Filtered := OldFiltered;
          Active := OldActive;
          if OldActive and (RecordCount > 0) then
            RecNo := OldRecNo;
          EnableControls;
        end;
      end;
    end;
  finally
    Dec(FAvoidReload);
  end;
end;

procedure TVirtualTable.InitAutoIncValues(RecBuf: IntPtr);
var
  i: integer;
  Field: TField;
  FieldDesc: TFieldDesc;
  Value: Variant;
begin
  for i := 0 to Fields.Count - 1 do begin
    Field := Fields[i];

    if (Field is TVirtualAutoIncField){$IFNDEF FPC} and (Field.AutoGenerateValue = arAutoInc){$ENDIF} then begin
      Value := TVirtualAutoIncField(Field).FCurrentValue;
      FieldDesc := TMemData(Data).FindField(Field.FieldName);
      Data.PutFieldAsVariant(FieldDesc, RecBuf, Value);
      TVirtualAutoIncField(Field).FCurrentValue := TVirtualAutoIncField(Field).FCurrentValue + TVirtualAutoIncField(Field).Increment;
    end;
  end;
end;

procedure TVirtualTable.SaveToStream(Stream: TStream; StoreFields: boolean = True; StoreAllData: boolean = false);
begin
  InternalSaveToStream(Stream, StoreFields, StoreAllData);
end;

function TVirtualTable.IsFieldDefsStored: boolean;
begin
  Result := FieldDefs.Count > 0;
end;

procedure TVirtualTable.LoadFromFile(const FileName: string; LoadFields: boolean = True; DecodeHTMLEntities: boolean = True);
var
  Stream: TStream;
begin
  Stream := TBufferedFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream, LoadFields, DecodeHTMLEntities);
  finally
    Stream.Free;
  end;
end;

procedure TVirtualTable.SaveToFile(const FileName: string; StoreFields: boolean = True; StoreAllData: boolean = false);
var
  Stream: TStream;
begin
  Stream := TBufferedFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, StoreFields, StoreAllData);
  finally
    Stream.Free;
  end;
end;

procedure TVirtualTable.SetActive(Value: boolean);
begin
  if csReading in ComponentState then begin
    if Value then
      FStreamedActive := True;
  end
  else
    inherited;
end;

function TVirtualTable.GetFieldDefs: TFieldDefs;
begin
  Result := inherited FieldDefs;
end;

procedure TVirtualTable.SetFieldDefs(Value: TFieldDefs);
begin
  inherited FieldDefs := Value;
end;

procedure TVirtualTable.SetDefaultSortType(const Value: TSortType);
begin
  FDefaultSortType := Value;
end;

procedure TVirtualTable.SetOptions(Value: TVirtualTableOptions);
begin
  FOptions := Value;
  Data.SetEmptyStrToNull := voSetEmptyStrToNull in FOptions;
end;

procedure TVirtualTable.DoVirtualTableProgress(Percent: integer);
begin
  if Assigned(FOnVirtualTableProgress) then
    FOnVirtualTableProgress(Self, Percent);
end;

function TVirtualTable.GetCanModify: boolean;
begin
  Result := Active and not FReadOnly;
end;

function TVirtualTable.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  if FieldType = ftAutoInc then
    Result := TVirtualAutoIncField
  else
    Result := inherited GetFieldClass(FieldType);
end;

function TVirtualTable.GetFieldClass(FieldType: TFieldType; DataType: Word): TFieldClass;
begin
  if FieldType = ftAutoInc then
    Result := TVirtualAutoIncField
  else
    Result := inherited GetFieldClass(FieldType, DataType);
end;

{$IFDEF VER12P}
function TVirtualTable.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  if FieldDef.DataType = ftAutoInc then
    Result := TVirtualAutoIncField
  else
    Result := inherited GetFieldClass(FieldDef);
end;
{$ENDIF}

procedure TVirtualTable.InternalInsert;
var
  Buffer: TRecordBuffer;
begin
  inherited;

  if not FLoadingData then begin
    Buffer := TRecordBuffer(ActiveBuffer);
    InitAutoIncValues(Buffer);
  end;  
end;

procedure TVirtualTable.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  Fld: TVirtualAutoIncField;
  FieldDesc: TFieldDesc;
  RecBuf: TRecordBuffer;
  Value: Variant;
begin
  inherited;

  if Field is TVirtualAutoIncField then begin
    Fld := TVirtualAutoIncField(Field);
    if Fld.FInitialValue = -1 then begin
      FieldDesc := TMemData(Data).FindField(Field.FieldName);
      RecBuf := TRecordBuffer(ActiveBuffer);
      Data.GetFieldAsVariant(FieldDesc, RecBuf, Value);
      while Value >= Fld.FCurrentValue do
        Fld.FCurrentValue := Fld.FCurrentValue + Fld.Increment;
    end;
  end;
end;

initialization
  VTOldBehavior := False;

end.
