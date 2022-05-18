//////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Virtual DataSet
//  Created:            20.02.98
//////////////////////////////////////////////////

unit VirtualDataSet;

  {$I Dac.inc}

interface

uses
  Classes, SysUtils, Variants, DB,
  CRTypes, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF};

const
  SNotSupportFieldType = 'Field type is not supported by TVirtualDataSet. '#13 +
    'Valid types is String, WideString, Smallint, Integer, Word, Boolean, Largeint, Float,' + {$IFDEF VER14P}' ftSingle,' +{$ENDIF}{$IFDEF VER12P}' ftExtended,' +{$ENDIF} ' Currency, Date, Time, DateTime,' + {$IFNDEF FPC}' SQLTimeStamp,' + {$ENDIF} ' Blob, Memo, Guid, Bcd, FmtBcd, Bytes, VarBytes, Variant';

type
  TCustomVirtualDataSet = class;

  TOnGetRecordCountEvent = procedure(Sender: TObject; out Count: Integer) of object;
  TOnGetFieldValueEvent = procedure(Sender: TObject; Field: TField; RecNo: Integer; out Value: Variant) of object;
  TOnModifyRecordEvent = procedure(Sender: TObject; var RecNo: Integer) of object;
  TOnDeleteRecordEvent = procedure(Sender: TObject; RecNo: Integer) of object;

  TCustomVirtualDataSetData = class (TData)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TCustomVirtualDataSet;
    FRecordNo: Integer;
  protected
    procedure InitData; override;
    procedure InternalOpen(DisableInitFields: boolean = False); override;
    procedure CreateFieldDescs; override;

    function GetItemCount: Integer; virtual;
    function GetItemNo: Integer; virtual;
    procedure SetItemNo(Value: Integer); virtual;
    function GetRecordCount: Integer; override;
    function GetRecordNo: Integer; override;
    procedure SetRecordNo(Value: Integer); override;

    procedure GetRecordBufer(Index: Integer; RecBuf: IntPtr);
    procedure GetRecordField(Index: Integer; RecBuf: IntPtr; Field: TFieldDesc);
  public
    constructor Create;

    procedure GetRecord(RecBuf: IntPtr); override;
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;
    procedure PutRecord(RecBuf: IntPtr); override;

    procedure SetToBegin; override;
    procedure SetToEnd; override;

    procedure AppendRecord(RecBuf: IntPtr); override;
    procedure InsertRecord(RecBuf: IntPtr); override;
    procedure UpdateRecord(RecBuf: IntPtr); override;
    procedure DeleteRecord; override;

    procedure EditRecord(RecBuf: IntPtr); override;
    procedure PostRecord(RecBuf: IntPtr); override;
    procedure CancelRecord(RecBuf: IntPtr); override;

    property Owner: TCustomVirtualDataSet read FOwner;
    property ItemNo: Integer read GetItemNo write SetItemNo;
    property RecordNo: Integer read GetRecordNo write SetRecordNo;
  end;

  TVirtualDataSetData = class (TCustomVirtualDataSetData)
  private
    FItemNo: Integer;
  protected
    procedure InitData; override;
    procedure InternalClose; override;

    function GetItemNo: integer; override;
    procedure SetItemNo(Value: Integer); override;
    function GetRecordCount: Integer; override;
    procedure SetRecordNo(Value: integer); override;

    function IsFiltered: boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function OmitRecord(ItemNo: Integer): boolean;
  public
    procedure GetNextRecord(RecBuf: IntPtr); override;
    procedure GetPriorRecord(RecBuf: IntPtr); override;

    procedure SetToBegin; override;
    procedure SetToEnd; override;

    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;

    procedure DataUpdated;
    procedure FilterUpdated; override;
  end;

  TVirtualDataSetUpdater = class (TDataSetUpdater)
  protected
    function DataSet: TCustomVirtualDataSet;

    function PerformAppend: boolean; override;
    function PerformUpdate: boolean; override;
    function PerformDelete: boolean; override;
    function PerformCancel: boolean;
  end;

  TVirtualDataSetService = class (TDataSetService)
  protected
    function Updater: TVirtualDataSetUpdater;

    procedure CreateDataSetUpdater; override;
  end;

  TCustomVirtualDataSet = class(TMemDataSet)
  private
    FIsCursorOpen: boolean;
    FReadOnly: Boolean;
  protected
    function DataSetService: TVirtualDataSetService;

    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;

    function GetDataSetServiceClass: TDataSetServiceClass; override;
    procedure CreateIRecordSet; override;
    procedure SetIRecordSet(Value: TData); override;

    function GetCanModify: boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;

    procedure InternalCancel; override;

    function GetField(FieldDesc: TFieldDesc): TField;

    procedure PerformInsertRecord;
    procedure PerformUpdateRecord;
    procedure PerformDeleteRecord;
    procedure PerformCancelRecord;

    procedure DoGetRecordCount(var Count: Integer); virtual; abstract;
    procedure DoGetFieldValue(Field: TFieldDesc; Index: Integer; var Value: Variant); virtual; abstract;
    procedure DoInsertRecord(var RecordNo: Integer); virtual; abstract;
    procedure DoUpdateRecord(var RecordNo: Integer); virtual; abstract;
    procedure DoDeleteRecord(RecordNo: Integer); virtual; abstract;
    procedure DoCancelRecord(RecordNo: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TVirtualDataSet = class (TCustomVirtualDataSet)
  private
    FOnGetRecordCount: TOnGetRecordCountEvent;
    FOnGetFieldValue: TOnGetFieldValueEvent;
    FOnInsertRecord: TOnModifyRecordEvent;
    FOnModifyRecord: TOnModifyRecordEvent;
    FOnDeleteRecord: TOnDeleteRecordEvent;
  protected
    procedure CreateIRecordSet; override;

    procedure DoGetRecordCount(var Count: Integer); override;
    procedure DoGetFieldValue(Field: TFieldDesc; RecNo: Integer; var Value: Variant); override;
    procedure DoInsertRecord(var RecordNo: Integer); override;
    procedure DoUpdateRecord(var RecordNo: Integer); override;
    procedure DoDeleteRecord(RecordNo: Integer); override;
    procedure DoCancelRecord(RecordNo: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoCalcFields;
    property Constraints stored IsConstraintsStored;
    property Filtered;
    property Filter;
    property FilterOptions;
    property IndexFieldNames;
    property MasterSource;
    property MasterFields;
    property DetailFields;

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
    property AfterRefresh;
    property BeforeRefresh;
    property BeforeScroll;
    property AfterScroll;

    property OnCalcFields;

    property OnGetRecordCount: TOnGetRecordCountEvent read FOnGetRecordCount write FOnGetRecordCount;
    property OnGetFieldValue: TOnGetFieldValueEvent read FOnGetFieldValue write FOnGetFieldValue;
    property OnInsertRecord: TOnModifyRecordEvent read FOnInsertRecord write FOnInsertRecord;
    property OnModifyRecord: TOnModifyRecordEvent read FOnModifyRecord write FOnModifyRecord;
    property OnDeleteRecord: TOnDeleteRecordEvent read FOnDeleteRecord write FOnDeleteRecord;
  end;

implementation

uses
{$IFNDEF FPC}
  SqlTimSt,
{$ENDIF}
  CRFunctions, CLRClasses, MemUtils;

{ TCustomVirtualDataSetData }

constructor TCustomVirtualDataSetData.Create;
begin
  inherited;

  FOwner := nil;
end;

procedure TCustomVirtualDataSetData.InitData;
begin
  inherited;

  FRecordNo := 0;
end;

procedure TCustomVirtualDataSetData.InternalOpen(DisableInitFields: boolean);
begin
  InitFields;

  inherited;

  FEOF := RecordCount = 0;
end;

procedure TCustomVirtualDataSetData.CreateFieldDescs;

  procedure AddFieldDesc(const FieldName: string; const FieldType: TFieldType;
    const FieldSize: integer; const Precision: integer; const Scale: integer;
    const Required: boolean; const Fixed: boolean);
  var
    Field: TFieldDesc;
  begin
    Field := CreateFieldDesc;
    try
      Field.FieldNo := FFields.Count + 1;
      Field.Name := FieldName;
      Field.DataType := TFieldTypeMap.GetDataType(FieldType);

      case FieldType of
        ftString: begin
          Field.Size := FieldSize + 1;
          Field.Length := FieldSize;
        end;
        ftWideString: begin
          Field.Size := (FieldSize + 1) * sizeof(WideChar);
          Field.Length := FieldSize;
        end;
        ftSmallint{$IFDEF VER12P}, ftShortInt{$ENDIF}:
          if (Precision <= 4) and (Precision <> 0) then begin
            Field.DataType := dtInt8;
            Field.Size := sizeof(SmallInt);
            Field.Length := Precision;
          end
          else
            Field.Size := sizeof(SmallInt);
      {$IFDEF VER12P}
        ftByte:
          Field.Size := sizeof(SmallInt);
      {$ENDIF}
        ftInteger, ftAutoInc:
          Field.Size := sizeof(Integer);
        ftWord:
          Field.Size := sizeof(word);
        ftBoolean:
          Field.Size := sizeof(Wordbool);
        ftLargeint{$IFDEF VER12P}, ftLongWord{$ENDIF}:
          if (Precision <= 10) and (Precision <> 0) then begin
            Field.DataType := dtUInt32;
            Field.Size := sizeof(Integer);
            Field.Length := Precision;
          end
          else
            Field.Size := sizeof(Largeint);
        ftFloat: begin
          Field.Size := sizeof(Double);
          Field.Length := Precision;
        end;
      {$IFDEF VER14P}
        ftSingle: begin
          Field.Size := sizeof(Single);
          Field.Length := Precision;
        end;
      {$ENDIF}
      {$IFDEF VER12P}
        ftExtended: begin
          Field.Size := sizeof(Extended);
          Field.Length := Precision;
        end;
      {$ENDIF}
        ftCurrency:
          Field.Size := sizeof(Double);
        ftDate, ftTime, ftDateTime: begin
          Field.Size := sizeof(TDateTime);
          Field.Length := Precision;
        end;
      {$IFNDEF FPC}
        ftTimeStamp: begin
          Field.Size := sizeof(TSQLTimeStamp);
          Field.Length := Precision;
        end;
      {$IFDEF VER14P}
        ftTimeStampOffset: begin
          Field.Size := sizeof(TSQLTimeStampOffset);
          Field.Length := Precision;
        end;
      {$ENDIF}
      {$ENDIF}
        ftBlob, ftMemo{$IFDEF VER10P}, ftWideMemo{$ENDIF}{$IFDEF FPC}, ftWideMemo{$ENDIF}:
          Field.Size := sizeof(Pointer);
        ftGuid: begin
          Field.Size := FieldSize + 1;
          Field.Length := FieldSize;
        end;
        ftBCD: begin
          Field.Size := sizeof(Currency);
          Field.Scale := Scale;
          Field.Length := Precision;
        end;
        ftFmtBcd: begin
          if Precision < SizeOfTBcd then
            Field.Size := SizeOfTBcd
          else
            Field.Size := Precision + 1{'.'} + 1 {#0}; // To right notation of large NUMERIC values
          Field.Scale := Scale;
          Field.Length := Precision;
        end;
        ftBytes: begin
          Field.Size := FieldSize;
          Field.Length := FieldSize;
        end;
        ftVarbytes: begin
          Field.Size := sizeof(word) + FieldSize;
          Field.Length := FieldSize;
        end;
        ftVariant: begin
          Field.Size := sizeof(TVariantObject);
        end;
{    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftVariant
 }
      else
      {$IFNDEF FPC}
      {$IFDEF VER14P}
        if FieldType = TFieldType(ftDATimeStampOffset) then begin
          Field.Size := sizeof(TSQLTimeStampOffset);
          Field.Length := Precision;
        end
        else
      {$ENDIF}
      {$ENDIF}
        DatabaseError(SNotSupportFieldType);
      end;
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
begin
  DataFieldCount := 0;
  for i := 0 to FOwner.FieldCount - 1 do
    if FOwner.Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

{$IFDEF VER20P}
  if (lcPersistent in  FOwner.Fields.LifeCycles) and
{$ELSE}
  if not FOwner.DefaultFields and
{$ENDIF}
     ((DataFieldCount > FOwner.FieldDefs.Count) {or (TVirtualDataSet(FOwner).FFieldDefsByField and not VTOldBehavior)})
  then
  // From fields
    for i := 0 to FOwner.FieldCount - 1 do begin
      if FOwner.Fields[i].FieldKind = fkData then begin
        OwnerField := FOwner.Fields[i];
        AddFieldDesc(OwnerField.FieldName, OwnerField.DataType, OwnerField.Size, 0, 0, False, False);
      end
    end
  else
  // From FieldDefs
    for i := 0 to FOwner.FieldDefs.Count - 1 do begin
      OwnerFieldDef := FOwner.FieldDefs[i];

      AddFieldDesc(OwnerFieldDef.Name, OwnerFieldDef.DataType, OwnerFieldDef.Size, OwnerFieldDef.Precision,
        OwnerFieldDef.Size, faRequired in OwnerFieldDef.Attributes, faFixed in OwnerFieldDef.Attributes);
    end
end;

function TCustomVirtualDataSetData.GetItemCount: Integer;
begin
  FOwner.DoGetRecordCount(Result);
end;

function TCustomVirtualDataSetData.GetItemNo: Integer;
begin
  Result := FRecordNo;
end;

procedure TCustomVirtualDataSetData.SetItemNo(Value: Integer);
begin
  SetRecordNo(Value);
end;

function TCustomVirtualDataSetData.GetRecordCount: Integer;
begin
  FOwner.DoGetRecordCount(Result);
end;

function TCustomVirtualDataSetData.GetRecordNo: Integer;
begin
  Result := FRecordNo;
end;

procedure TCustomVirtualDataSetData.SetRecordNo(Value: Integer);
begin
  FRecordNo := Value;
  if FRecordNo > 0 then
    FBOF := False
  else
    FBOF := True;
  if FRecordNo <= GetRecordCount then
    FEOF := False
  else
    FEOF := True;
end;

procedure TCustomVirtualDataSetData.GetRecordBufer(Index: Integer; RecBuf: IntPtr);
var
  i: Integer;
  Field: TFieldDesc;
begin
  if (not BOF ) and (not EOF) and (Index > 0) and (Index <= GetItemCount) then
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if Field.FieldDescKind <> fdkCalculated then
        GetRecordField(Index, RecBuf, Field);
    end;
end;

procedure TCustomVirtualDataSetData.GetRecordField(Index: Integer; RecBuf: IntPtr; Field: TFieldDesc);
var
  Value: Variant;
  FieldBuf: IntPtr;
begin
  FOwner.DoGetFieldValue(Field, Index, Value);

  if Field.IsComplex then begin
    FieldBuf := PtrOffset(RecBuf, Field.DataOffset);
    if InternalGetObject(FieldBuf) = nil then
      CreateComplexField(RecBuf, Field);
  end;

  PutFieldAsVariant(Field, RecBuf, Value, True);
end;

procedure TCustomVirtualDataSetData.GetRecord(RecBuf: IntPtr);
begin
  GetRecordBufer(ItemNo, RecBuf);
end;

procedure TCustomVirtualDataSetData.GetNextRecord(RecBuf: IntPtr);
begin
  if not EOF then begin
    if BOF then
      FBOF := False;

    if RecordNo > 0 then
      RecordNo := RecordNo + 1
    else
      RecordNo := 1;

    if RecordNo > GetRecordCount then begin
      FEOF := True;
      RecordNo := GetRecordCount + 1;
    end
    else if RecBuf <> nil then
      GetRecord(RecBuf);
  end;
end;

procedure TCustomVirtualDataSetData.GetPriorRecord(RecBuf: IntPtr);
begin
  if not BOF then begin
    if EOF then
      FEOF := False;

    if (RecordNo > 0) and (RecordNo < GetRecordCount + 1) then
      RecordNo := RecordNo - 1
    else
      RecordNo := GetRecordCount;

    if RecordNo < 1 then begin
      FBOF := True;
      RecordNo := 0;
    end
    else if RecBuf <> nil then
      GetRecord(RecBuf);
  end;
end;

procedure TCustomVirtualDataSetData.PutRecord(RecBuf: IntPtr);
begin
  // Empty
end;

procedure TCustomVirtualDataSetData.SetToBegin;
begin
  FRecordNo := 0; //FirstItem;
  FBOF := True;
  if GetRecordCount > 1 then
    FEOF := False;
end;

procedure TCustomVirtualDataSetData.SetToEnd;
begin
  FRecordNo := 0; //LastItem;
  FEOF := True;
  if GetRecordCount > 0 then
    FBOF := False;
end;

procedure TCustomVirtualDataSetData.AppendRecord(RecBuf: IntPtr);
begin
  SetToEnd;
  InternalAppend(RecBuf);
end;

procedure TCustomVirtualDataSetData.InsertRecord(RecBuf: IntPtr);
begin
  InternalAppend(RecBuf);
end;

procedure TCustomVirtualDataSetData.UpdateRecord(RecBuf: IntPtr);
begin
  Assert((RecordNo > 0) and (RecordNo <= GetRecordCount));

  InternalUpdate(RecBuf)
end;

procedure TCustomVirtualDataSetData.DeleteRecord;
begin
  Assert((RecordNo > 0) and (RecordNo <= GetRecordCount));

  InternalDelete;

  if RecordNo > GetRecordCount then
    RecordNo := GetRecordCount;
end;

procedure TCustomVirtualDataSetData.EditRecord(RecBuf: IntPtr);
begin
  // Empty
end;

procedure TCustomVirtualDataSetData.PostRecord(RecBuf: IntPtr);
begin
  UpdateRecord(RecBuf);
end;

procedure TCustomVirtualDataSetData.CancelRecord(RecBuf: IntPtr);
begin
  // Empty
end;

{ TVirtualDataSetData }

procedure TVirtualDataSetData.InitData;
begin
  inherited;

  FItemNo := 0;
  FRecordCount := -1;
end;

procedure TVirtualDataSetData.InternalClose;
begin
  if FilterRecBuf <> nil then begin
    FreeMem(FilterRecBuf);
    FilterRecBuf := nil;
  end;
end;

function TVirtualDataSetData.GetItemNo: integer;
begin
  Result := FItemNo;
end;

procedure TVirtualDataSetData.SetItemNo(Value: Integer);
var
  OldBOF: boolean;
  OldEOF: boolean;
  OldRecordNo: Integer;
  OldItemNo: Integer;
begin
  if not IsFiltered or (Value = 0) then begin
    inherited SetItemNo(Value);
    FItemNo := FRecordNo;
  end
  else begin
    OldBOF := FBOF;
    OldEOF := FEOF;
    OldRecordNo := FRecordNo;
    OldItemNo := FItemNo;

    if EOF or (Value < FRecordNo) then begin
      repeat
        GetPriorRecord(nil);
        if BOF or (Value > FItemNo) then begin
          FBOF := OldBOF;
          FEOF := OldEOF;
          FRecordNo := OldRecordNo;
          FItemNo := OldItemNo;
          Exit;
        end;
      until Value = FItemNo;
    end
    else if BOF or (Value > FItemNo) then begin
      repeat
        GetNextRecord(nil);
        if EOF or (Value < FItemNo) then begin
          FBOF := OldBOF;
          FEOF := OldEOF;
          FRecordNo := OldRecordNo;
          FItemNo := OldItemNo;
          Exit;
        end;
      until Value = FItemNo;
    end;
  end;
end;

function TVirtualDataSetData.GetRecordCount: Integer;
var
  i: Integer;
begin
  if not IsFiltered then
    Result := inherited GetRecordCount
  else begin
    if FRecordCount < 0 then begin
      FRecordCount := 0;
      for i := 1 to GetItemCount do
        if not OmitRecord(i) then
          Inc(FRecordCount);
    end;
    Result := FRecordCount;
  end;
end;

procedure TVirtualDataSetData.SetRecordNo(Value: integer);
var
  OldBOF: boolean;
  OldEOF: boolean;
  OldRecordNo: Integer;
  OldItemNo: Integer;
begin
  if not IsFiltered or (Value = 0) then begin
    inherited SetRecordNo(Value);
    FItemNo := FRecordNo;
  end
  else begin
    OldBOF := FBOF;
    OldEOF := FEOF;
    OldRecordNo := FRecordNo;
    OldItemNo := FItemNo;

    while Value > FRecordNo do begin
      GetNextRecord(nil);
      if EOF then begin
        FBOF := OldBOF;
        FEOF := OldEOF;
        FRecordNo := OldRecordNo;
        FItemNo := OldItemNo;
        Exit;
      end;
    end;

    while Value < FRecordNo do begin
      GetPriorRecord(nil);
      if BOF then begin
        FBOF := OldBOF;
        FEOF := OldEOF;
        FRecordNo := OldRecordNo;
        FItemNo := OldItemNo;
        Exit;
      end;
    end;
  end;
end;

function TVirtualDataSetData.IsFiltered: boolean;
begin
  Result := Assigned(FilterFunc) or Assigned(FilterMDFunc) or Assigned(FilterExpression) or Assigned(FilterRangeFunc);
end;

function TVirtualDataSetData.OmitRecord(ItemNo: Integer): boolean;
begin
  if ItemNo <= GetItemCount then begin
    if FilterRecBuf = nil then begin
      GetMem(FilterRecBuf, RecordSize);
      CreateComplexFields(FilterRecBuf, True);
    end;

    GetRecordBufer(ItemNo, FilterRecBuf);

    Result := Assigned(FilterFunc) and not FilterFunc(FilterRecBuf) or
              Assigned(FilterMDFunc) and not FilterMDFunc(FilterRecBuf) or
              Assigned(FilterExpression) and not Eval(FilterExpression) or
              Assigned(FilterRangeFunc) and not FilterRangeFunc(FilterRecBuf);
  end
  else
    Result := False;
end;

procedure TVirtualDataSetData.GetNextRecord(RecBuf: IntPtr);
begin
  if not IsFiltered then begin
    inherited GetNextRecord(RecBuf);
    FItemNo := FRecordNo;
  end
  else
    if not EOF then begin
      if BOF then
        FBOF := False;

      if FRecordNo > 0 then
        FRecordNo := FRecordNo + 1
      else
        FRecordNo := 1;

      repeat
        if FItemNo > 0 then
          FItemNo := FItemNo + 1
        else
          FItemNo := 1;

        if FItemNo > GetItemCount then begin
          FItemNo := GetItemCount + 1;
          FRecordCount := FRecordNo - 1;
          FEOF := True;
          Exit;
        end
      until not OmitRecord(FItemNo);

      if FRecordNo > RecordCount then
        FRecordCount := FRecordNo;

      if RecBuf <> nil then
        GetRecord(RecBuf);
    end;
end;

procedure TVirtualDataSetData.GetPriorRecord(RecBuf: IntPtr);
begin
  if not IsFiltered then begin
    inherited GetPriorRecord(RecBuf);
    FItemNo := FRecordNo;
  end
  else
    if not BOF then begin
      if EOF then
        FEOF := False;

      if (FRecordNo > 0) and (FRecordNo < RecordCount + 1) then
        FRecordNo := FRecordNo - 1
      else
        FRecordNo := FRecordCount;

      repeat
        if (FItemNo > 0) and (FItemNo < GetItemCount + 1) then
          FItemNo := FItemNo - 1
        else
          FItemNo := GetItemCount;

        if FItemNo < 1 then begin
          FItemNo := 0;
          FRecordNo := 0;
          FBOF := True;
          Exit;
        end
      until not OmitRecord(FItemNo);

      if FRecordNo < 1 then begin
        FRecordNo := 1;
        FRecordCount := RecordCount + 1;
      end;

      if RecBuf <> nil then
        GetRecord(RecBuf);
    end;
end;

procedure TVirtualDataSetData.SetToBegin;
begin
  inherited SetToBegin;
  FItemNo := 0;
end;

procedure TVirtualDataSetData.SetToEnd;
begin
  inherited SetToEnd;
  FItemNo := 0;
end;

procedure TVirtualDataSetData.GetBookmark(Bookmark: PRecBookmark);
begin
  Bookmark.Order := ItemNo;
end;

procedure TVirtualDataSetData.SetToBookmark(Bookmark: PRecBookmark);
begin
  if (IntPtr(Bookmark) <> nil) and (Bookmark.Order <> -1) then
    SetItemNo(Bookmark.Order);
end;

procedure TVirtualDataSetData.DataUpdated;
begin
  FRecordCount := -1;
end;

procedure TVirtualDataSetData.FilterUpdated;
begin
  FRecordCount := -1;
end;

{ TVirtualDataSetUpdater }

function TVirtualDataSetUpdater.DataSet: TCustomVirtualDataSet;
begin
  Result := TCustomVirtualDataSet(FDataSet);
end;

function TVirtualDataSetUpdater.PerformAppend: boolean;
begin
  DataSet.PerformInsertRecord;
  Result := True;
end;

function TVirtualDataSetUpdater.PerformUpdate: boolean;
begin
  DataSet.PerformUpdateRecord;
  Result := True;
end;

function TVirtualDataSetUpdater.PerformDelete: boolean;
begin
  DataSet.PerformDeleteRecord;
  Result := True;
end;

function TVirtualDataSetUpdater.PerformCancel: boolean;
begin
  DataSet.PerformCancelRecord;
  Result := True;
end;

{ TVirtualDataSetService }

function TVirtualDataSetService.Updater: TVirtualDataSetUpdater;
begin
  Result := TVirtualDataSetUpdater(FUpdater);
end;

procedure TVirtualDataSetService.CreateDataSetUpdater;
begin
  SetDataSetUpdater(TVirtualDataSetUpdater.Create(Self));
end;

{ TCustomVirtualDataSet }

constructor TCustomVirtualDataSet.Create(AOwner: TComponent);
begin
  inherited;

  CreateIRecordSet;
end;

function TCustomVirtualDataSet.DataSetService: TVirtualDataSetService;
begin
  Result := TVirtualDataSetService(FDataSetService);
end;

function TCustomVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := inherited AllocRecordBuffer;

  FillChar(Result, FRecBufSize, 0);
end;

procedure TCustomVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  TCustomVirtualDataSetData(Data).FreeComplexFields(Buffer, True);

  inherited;
end;

function TCustomVirtualDataSet.GetDataSetServiceClass: TDataSetServiceClass;
begin
  Result := TVirtualDataSetService;
end;

procedure TCustomVirtualDataSet.CreateIRecordSet;
begin
  SetIRecordSet(TCustomVirtualDataSetData.Create);
end;

procedure TCustomVirtualDataSet.SetIRecordSet(Value: TData);
begin
  inherited;

  if Data <> nil then
    (Data as TCustomVirtualDataSetData).FOwner := Self;
end;

function TCustomVirtualDataSet.GetCanModify: boolean;
begin
  Result := Active and not FReadOnly;
end;

procedure TCustomVirtualDataSet.InternalOpen;
begin
  inherited;

  FIsCursorOpen := True;
end;

procedure TCustomVirtualDataSet.InternalClose;
begin
  inherited;
  FIsCursorOpen := False;
end;

function TCustomVirtualDataSet.IsCursorOpen: boolean;
begin
  Result := FIsCursorOpen;
end;

procedure TCustomVirtualDataSet.InternalCancel;
begin
  inherited;

  DataSetService.Updater.PerformCancel;
end;

function TCustomVirtualDataSet.GetField(FieldDesc: TFieldDesc): TField;
var
  i: integer;
begin
  Assert(FieldDesc <> nil);
  Result := nil;
  for i := 0 to Fields.Count - 1 do
    if Fields[i].FieldNo = FieldDesc.FieldNo then begin
      Result := Fields[i];
      Break;
    end;
end;

procedure TCustomVirtualDataSet.PerformInsertRecord;
var
  ItemNo: Integer;
begin
  if TCustomVirtualDataSetData(Data).RecordNo > 0 then
    ItemNo := TCustomVirtualDataSetData(Data).ItemNo
  else
    ItemNo := TCustomVirtualDataSetData(Data).GetItemCount + 1;
  DoInsertRecord(ItemNo);
  TCustomVirtualDataSetData(Data).ItemNo := ItemNo;
end;

procedure TCustomVirtualDataSet.PerformUpdateRecord;
var
  ItemNo: Integer;
begin
  ItemNo := TCustomVirtualDataSetData(Data).ItemNo;
  DoUpdateRecord(ItemNo);
  TCustomVirtualDataSetData(Data).ItemNo := ItemNo;
end;

procedure TCustomVirtualDataSet.PerformDeleteRecord;
begin
  DoDeleteRecord(TCustomVirtualDataSetData(Data).ItemNo);
end;

procedure TCustomVirtualDataSet.PerformCancelRecord;
begin
  DoCancelRecord(TCustomVirtualDataSetData(Data).ItemNo);
end;


{ TVirtualDataSet }

constructor TVirtualDataSet.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TVirtualDataSet.CreateIRecordSet;
begin
  SetIRecordSet(TVirtualDataSetData.Create);
end;

procedure TVirtualDataSet.DoGetRecordCount(var Count: Integer);
begin
  if Assigned(FOnGetRecordCount) then
    FOnGetRecordCount(self, Count)
  else
    raise Exception.Create('The OnGetRecordCount event handler is not specified');
end;

procedure TVirtualDataSet.DoGetFieldValue(Field: TFieldDesc; RecNo: Integer; var Value: Variant);
begin
  if Assigned(FOnGetFieldValue) then begin
    Value := Unassigned;
    FOnGetFieldValue(Self, GetField(Field), RecNo, Value);
  end
  else
    raise Exception.Create('The OnGetFieldValue event handler is not specified');
end;

procedure TVirtualDataSet.DoInsertRecord(var RecordNo: Integer);
begin
  if Assigned(FOnInsertRecord) then begin
    FOnInsertRecord(Self, RecordNo);
    TVirtualDataSetData(Data).DataUpdated;
  end
  else
    raise Exception.Create('The OnInsertRecord event handler is not specified');
end;

procedure TVirtualDataSet.DoUpdateRecord(var RecordNo: Integer);
begin
  if Assigned(FOnModifyRecord) then begin
    FOnModifyRecord(Self, RecordNo);
    TVirtualDataSetData(Data).DataUpdated;
  end
  else
    raise Exception.Create('The OnModifyRecord event handler is not specified');
end;

procedure TVirtualDataSet.DoDeleteRecord(RecordNo: Integer);
begin
  if Assigned(FOnDeleteRecord) then begin
    FOnDeleteRecord(Self, RecordNo);
    TVirtualDataSetData(Data).DataUpdated;
  end
  else
    raise Exception.Create('The OnDeleteRecord event handler is not specified');
end;

procedure TVirtualDataSet.DoCancelRecord(RecordNo: Integer);
begin
  // Empty
end;


end.
