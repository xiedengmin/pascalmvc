
//////////////////////////////////////////////////
//  MongoDB Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I MongoDac.inc}
unit MongoObjectsUni;

interface

uses
  SysUtils, Classes, Variants,
  MemData, CRTypes, CRJson, CRFunctions,
{$IFNDEF UNIDACPRO}
  MongoCall;
{$ELSE}
  MongoCallUni;
{$ENDIF}

type
  THandleType = (htNone, htNew, htInternal, htOuter);
  TDocumentKind = (dkExpression, dkField);
  TFieldAction = (faGet, faSet, faUnset);

  TMongoDocument = class;

  TReadOnlyStream = class(TCustomMemoryStream)
  public
    constructor Create(const Buffer: IntPtr; const Len: integer);

    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TMongoAttribute = class(TAttribute)
  private
    FActualName: string;
    FDBType: Word;
    FIndex: integer;
  protected
    function GetActualName: string; override;
    procedure SetActualName(const Value: string); override;

    property DBType: Word read FDBType write FDBType;
    property AttributeIndex: integer read FIndex;
  end;

  TMongoObjectType = class(TObjectType)
  private
    FAttributeList: TStringList;

    procedure GetAttributeDataType(const Tag: TJSONTag; var DataType, SubDataType, Len: Word; const Unicode: boolean);
    function CreateAttributeChain(const Name: string; const DataType, SubDataType, DBType, Len: Word): TAttributeChain;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    property AttributeList: TStringList read FAttributeList;

    function FindAttributeByName(const Name: string): TMongoAttribute;
    function FindAttributeChain(const Name: string): TAttributeChain;

    function AddAttribute: TMongoAttribute; overload;
    function AddAttribute(const Field: TFieldDesc): TMongoAttribute; overload;
    function AddAttribute(const Owner, ObjectType: TObjectType; const Name, ActualName: string; const DataType, SubDataType, DBType, Length: Word): TMongoAttribute; overload;
  end;

  IDocumentExpression = interface
  ['{41212319-DB81-42DB-B88F-C881C8C37BDF}']
    function GetKind: TDocumentKind;
    function GetData: TJSONValue;
    function GetName: string;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetText: string;
    function GetFieldCount: integer;
    function GetField(const Index: integer): IDocumentExpression;
    function GetFieldByName(const Name: string): IDocumentExpression;

    function SetOid(const Name: string; const Value: TJSONOid): IDocumentExpression;
    function SetString(const Name: string; const Value: string): IDocumentExpression;
    function SetInteger(const Name: string; const Value: integer): IDocumentExpression;
    function SetInt64(const Name: string; const Value: Int64): IDocumentExpression;
    function SetDouble(const Name: string; const Value: double): IDocumentExpression;
    function SetBoolean(const Name: string; const Value: boolean): IDocumentExpression;
    function SetDateTime(const Name: string; const Value: TDateTime): IDocumentExpression;
    function SetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal): IDocumentExpression;
    function SetBinary(const Name: string; const Binary: TBytes; const SubType: integer): IDocumentExpression;
    function SetJavaCode(const Name: string; const Code: string): IDocumentExpression;
    function SetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant): IDocumentExpression;
    function SetRegex(const Name: string; const Pattern, Options: string): IDocumentExpression;
    function SetNull(const Name: string): IDocumentExpression;
    function SetMinKey(const Name: string): IDocumentExpression;
    function SetMaxKey(const Name: string): IDocumentExpression;
    function SetObject(const Name: string): IDocumentExpression;
    function SetArray(const Name: string): IDocumentExpression;
    function SetEnd: IDocumentExpression;

    function FindField(const Name: string): IDocumentExpression;

    property Name: string read GetName;
    property Value: Variant read GetValue write SetValue;

    property Text: string read GetText;

    property FieldCount: integer read GetFieldCount;
    property Fields[const Index: integer]: IDocumentExpression read GetField;
    property FieldByName[const Name: string]: IDocumentExpression read GetFieldByName; default;
  end;

  TDocumentExpression = class(TInterfacedObject, IDocumentExpression)
  private
    FDocument: TMongoDocument;
    FKind: TDocumentKind;
    FParent,
    FData: TJSONValue;
    FName: string;

    function GetKind: TDocumentKind;
    function GetData: TJSONValue;
    function GetName: string;
    function GetValue: Variant;
    procedure SetValue(const AValue: Variant);
    function GetText: string;
    function GetFieldCount: integer;
    function GetField(const Index: integer): IDocumentExpression;
    function GetFieldByName(const Name: string): IDocumentExpression;

  {$IFNDEF FPC}
    constructor Create(const Action: TFieldAction; const Document: TMongoDocument; const Parent: TJSONValue; const Tag: TJSONTag; const Name: string; const Kind: TDocumentKind = dkExpression);
  {$ENDIF}
  public
  {$IFDEF FPC}
    constructor Create(const Action: TFieldAction; const Document: TMongoDocument; const Parent: TJSONValue; const Tag: TJSONTag; const Name: string; const Kind: TDocumentKind = dkExpression);
  {$ENDIF}

    function SetOid(const Name: string; const Value: TJSONOid): IDocumentExpression;
    function SetString(const Name: string; const Value: string): IDocumentExpression;
    function SetInteger(const Name: string; const Value: integer): IDocumentExpression;
    function SetInt64(const Name: string; const Value: Int64): IDocumentExpression;
    function SetDouble(const Name: string; const Value: double): IDocumentExpression;
    function SetBoolean(const Name: string; const Value: boolean): IDocumentExpression;
    function SetDateTime(const Name: string; const Value: TDateTime): IDocumentExpression;
    function SetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal): IDocumentExpression;
    function SetBinary(const Name: string; const Binary: TBytes; const SubType: integer): IDocumentExpression;
    function SetJavaCode(const Name: string; const Code: string): IDocumentExpression;
    function SetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant): IDocumentExpression;
    function SetRegex(const Name: string; const Pattern, Options: string): IDocumentExpression;
    function SetNull(const Name: string): IDocumentExpression;
    function SetMinKey(const Name: string): IDocumentExpression;
    function SetMaxKey(const Name: string): IDocumentExpression;
    function SetObject(const Name: string): IDocumentExpression;
    function SetArray(const Name: string): IDocumentExpression;
    function SetEnd: IDocumentExpression;

    function FindField(const Name: string): IDocumentExpression;
  end;

  TMongoDocumentState = (dsNew, dsStored);

  TMongoDocument = class(TDBObject)
  private
    FAPI: TMongoAPI;
    FHandle: pbson_t;
    FHandleType: THandleType;
    FData: TJSONValue;
    FInputStream: TReadOnlyStream;
    FOutputStream: TMemoryStream;
    FFieldMap: TStringList;
    FUnicode,
    FImplicitAddFields,
    FImplicitChangeType: boolean;
    FPrepared: boolean;
    FSerializer: TJSONSerializer;
    FSerializerOwned: boolean;
    FDeserializer: TJSONDeserializer;
    FDeserializerOwned: boolean;
    FState: TMongoDocumentState;

    procedure CheckSerializer;
    procedure CheckDeserializer;

    function GetFieldCount: integer;
    function InternalGetField(const Index: integer; const Parent: TJSONValue): IDocumentExpression;
    function InternalGetFieldByName(const Name: string; const Parent: TJSONValue): IDocumentExpression;

    function GetField(const Index: integer): IDocumentExpression;
    function GetFieldByName(const Name: string): IDocumentExpression;

    function InternalCreateValue(const ValueParent: TJSONValue; const DataType: Word; const Size: integer): TJSONValue;
    function InternalGetValue(const Chain: TAttributeChain): TJSONValue;
    procedure InternalDeleteValue(const Chain: TAttributeChain);

    function VarToJson(const VarValue: Variant): TJSONValue;

    function GetText: string;
    procedure SetText(const Value: string);

    function InternalSetOid(const Name: string; const Value: TJSONOid; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetString(const Name: string; const Value: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetInteger(const Name: string; const Value: integer; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetInt64(const Name: string; const Value: Int64; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetDouble(const Name: string; const Value: double; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetBoolean(const Name: string; const Value: boolean; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetDateTime(const Name: string; const Value: TDateTime; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetBinary(const Name: string; const Binary: TBytes; const SubType: integer; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetJavaCode(const Name: string; const Code: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetRegex(const Name: string; const Pattern, Options: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetNull(const Name: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetMinKey(const Name: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetMaxKey(const Name: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetObject(const Name: string; const Parent: TJSONValue): IDocumentExpression;
    function InternalSetArray(const Name: string; const Parent: TJSONValue): IDocumentExpression;
  protected
    function GetIsNull: boolean; override;

    procedure GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean); override;
    procedure SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word); override;
    function GetAttrIsNull(const Field: TFieldDesc): boolean; reintroduce;

    procedure AllocHandle(const HandleType: THandleType; const AHandle: pbson_t = nil); overload;
    procedure FreeHandle;

    procedure Prepare;
    procedure Unprepare;

    procedure SetData(const AData: TJSONValue);
    procedure FreeData;

  {$IFNDEF FPC}
    constructor Create(const API: TMongoAPI); overload;
    constructor Create(const API: TMongoAPI; const Unicode: boolean); overload;
    constructor Create(const API: TMongoAPI; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean); overload;
    constructor Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode: boolean); overload;
    constructor Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean); overload;
  {$ENDIF}

    property API: TMongoAPI read FAPI;
    property Handle: pbson_t read FHandle;
    property Data: TJSONValue read FData;
    property State: TMongoDocumentState read FState write FState;
  public
    constructor Create(const Text: string); overload;
  {$IFDEF FPC}
    constructor Create(const API: TMongoAPI); overload;
    constructor Create(const API: TMongoAPI; const Unicode: boolean); overload;
    constructor Create(const API: TMongoAPI; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean); overload;
    constructor Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode: boolean); overload;
    constructor Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean); overload;
  {$ENDIF}
    destructor Destroy; override;

    procedure Assign(const Source: TMongoDocument);
    procedure Copy(const Source: TMongoDocument);

    function SetOid(const Name: string; const Value: TJSONOid): IDocumentExpression;
    function SetString(const Name: string; const Value: string): IDocumentExpression;
    function SetInteger(const Name: string; const Value: integer): IDocumentExpression;
    function SetInt64(const Name: string; const Value: Int64): IDocumentExpression;
    function SetDouble(const Name: string; const Value: double): IDocumentExpression;
    function SetBoolean(const Name: string; const Value: boolean): IDocumentExpression;
    function SetDateTime(const Name: string; const Value: TDateTime): IDocumentExpression;
    function SetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal): IDocumentExpression;
    function SetBinary(const Name: string; const Binary: TBytes; const SubType: integer): IDocumentExpression;
    function SetJavaCode(const Name: string; const Code: string): IDocumentExpression;
    function SetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant): IDocumentExpression;
    function SetRegex(const Name: string; const Pattern, Options: string): IDocumentExpression;
    function SetNull(const Name: string): IDocumentExpression;
    function SetMinKey(const Name: string): IDocumentExpression;
    function SetMaxKey(const Name: string): IDocumentExpression;
    function SetObject(const Name: string): IDocumentExpression;
    function SetArray(const Name: string): IDocumentExpression;

    function Unset(const Name: string): IDocumentExpression;

    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

    function FindField(const Name: string): IDocumentExpression;

    property Text: string read GetText write SetText;

    property FieldCount: integer read GetFieldCount;
    property Fields[const Index: integer]: IDocumentExpression read GetField;
    property FieldByName[const Name: string]: IDocumentExpression read GetFieldByName; default;
    property FieldMap: TStringList read FFieldMap;

    property Prepared: boolean read FPrepared;
  end;

implementation

uses
  MemUtils, CLRClasses,
{$IFNDEF UNIDACPRO}
  MongoConsts, MongoClasses, MongoDataTypeMap;
{$ELSE}
  MongoConstsUni, MongoClassesUni, MongoDataTypeMapUni;
{$ENDIF}

type
  TInternalJSONValue = class(TJSONValue);
  TInternalJSONString = class(TJSONString);

{ TReadOnlyStream }

constructor TReadOnlyStream.Create(const Buffer: IntPtr; const Len: integer);
begin
  inherited Create;

  SetPointer(Buffer, Len);
end;

function TReadOnlyStream.Write(const Buffer; Count: Longint): Longint;
begin
  // do nothing
  Result := 0;
end;

{ TMongoAttribute }

function TMongoAttribute.GetActualName: string;
begin
  Result := FActualName;
end;

procedure TMongoAttribute.SetActualName(const Value: string);
begin
  FActualName := Value;
end;

{ TMongoObjectType }

constructor TMongoObjectType.Create(const AName: string);
begin
  inherited Create;

  FAttributeList := TStringList.Create;
  FAttributeList.Sorted := True;

  FName := AName;
  FDataType := dtObject;
end;

destructor TMongoObjectType.Destroy;
begin
  FAttributeList.Free;

  inherited;
end;

procedure TMongoObjectType.GetAttributeDataType(const Tag: TJSONTag; var DataType, SubDataType, Len: Word; const Unicode: boolean);
begin
  DataType := TMongoConverterManager.GetDBType(Tag);
  DataType := TMongoConverterManager.GetDataType(DataType, SubDataType);
  if (DataType = dtString) and Unicode then
    DataType := dtWideString;
  Len := 0;
  if (DataType = dtBytes) then
    Len := 1
  else if (DataType in [dtString, dtWideString]) then
    Len := MaxStringSize;
end;

function TMongoObjectType.CreateAttributeChain(const Name: string; const DataType, SubDataType, DBType, Len: Word): TAttributeChain;
var
  St, AttrName, ActualName, NextName: string;
  iPos, ii, Index: integer;
  OType: TMongoObjectType;
  Attribute: TMongoAttribute;
  ParentDataType: Word;
begin
  Result := nil;
  OType := Self;
  ParentDataType := mongoObject;

  ActualName := '';
  AttrName := TrimLeft(Name);
  repeat
    iPos := Pos('.', AttrName);
    if iPos > 0 then begin
      St := Copy(AttrName, 1, iPos - 1);
      AttrName := Copy(AttrName, iPos + 1, Length(AttrName));

      ii := Pos('.', AttrName);
      if ii > 0 then
        NextName := Copy(AttrName, 1, ii - 1)
      else
        NextName := AttrName;
    end
    else begin
      St := AttrName;
      AttrName := '';
    end;

    Attribute := OType.FindAttributeByName(St);
    if Attribute <> nil then begin
      Index := Attribute.AttributeIndex;
      if ParentDataType = mongoArray then
        if not TryStrToInt(St, Index) then begin
          if Result <> nil then
            Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
          raise Exception.Create('Invalid array index: ' + St);
        end;
      Result := TAttributeChain.Create(Attribute, Result, Index);
    end;

    if ActualName = '' then
      ActualName := St
    else
      ActualName := ActualName + '.' + St;

    if Attribute = nil then begin
      if AttrName = '' then begin
        Attribute := OType.AddAttribute(OType, nil, St, ActualName, DataType, SubDataType, DBType, Len);
        OType.AttributeList.AddObject(Attribute.Name, Attribute);
      end
      else begin
        if not TryStrToInt(NextName, Index) then begin
          Attribute := OType.AddAttribute(OType, nil, St, ActualName, dtObject, mongoObject, mongoObject, 0);
          OType.AttributeList.AddObject(Attribute.Name, Attribute);
        end
        else begin
          Attribute := OType.AddAttribute(OType, nil, St, ActualName, dtObject, mongoArray, mongoArray, 0);
          OType.AttributeList.AddObject(Attribute.Name, Attribute);
        end;
      end;
      if (Attribute.DataType = dtObject) and
         (SubDataType <> mongoNull) and
         (SubDataType <> mongoUndefined) and
         (SubDataType <> mongoMinKey) and
         (SubDataType <> mongoMaxKey)
      then begin
        Attribute.ObjectType := TMongoObjectType.Create(ActualName);
        Attribute.ObjectType.Release;
      end;

      if not TryStrToInt(St, Index) then
        Index := -1;
      if (ParentDataType = mongoArray) and (Index = -1) then begin
        if Result <> nil then
          Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
        raise Exception.Create('Invalid array index: ' + St);
      end;
      Result := TAttributeChain.Create(Attribute, Result, Index);
    end
    else
    if (Attribute.SubDataType <> mongoObject) and
       (Attribute.SubDataType <> mongoArray) and
       (Attribute.SubDataType <> mongoGeneric) and
       (Attribute.SubDataType <> mongoJavaCode) and
       (Attribute.SubDataType <> mongoJavaScopeCode) and
       (Attribute.SubDataType <> mongoRegex) and
       (Attribute.SubDataType <> mongoTimestamp) and
       (Attribute.SubDataType <> mongoBinary) and
       (Attribute.SubDataType <> mongoDBPointer)
    then begin
      if AttrName <> '' then begin
        if Result <> nil then
          Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
        Result := nil;
      end;
      Exit;
    end;

    OType := TMongoObjectType(Attribute.ObjectType);
    ParentDataType := Attribute.SubDataType;
  until (iPos = 0) and (AttrName = '');
end;

function TMongoObjectType.FindAttributeByName(const Name: string): TMongoAttribute;
var
  n: integer;
begin
  Result := nil;

  n := FAttributeList.IndexOf(Name);
  if n >= 0 then
    Result := TMongoAttribute(FAttributeList.Objects[n]);
end;

function TMongoObjectType.FindAttributeChain(const Name: string): TAttributeChain;
var
  St, AttrName: string;
  iPos, Index: integer;
  OType: TMongoObjectType;
  Attribute: TMongoAttribute;
  ParentDataType: Word;
begin
  Result := nil;
  OType := Self;
  ParentDataType := mongoObject;

  AttrName := TrimLeft(Name);
  repeat
    iPos := Pos('.', AttrName);
    if iPos > 0 then begin
      St := Copy(AttrName, 1, iPos - 1);
      AttrName := Copy(AttrName, iPos + 1, Length(AttrName));
    end
    else begin
      St := AttrName;
      AttrName := '';
    end;

    Attribute := OType.FindAttributeByName(St);

    if Attribute <> nil then begin
      Index := Attribute.AttributeIndex;
      if ParentDataType = mongoArray then
        if not TryStrToInt(St, Index) then begin
          if Result <> nil then
            Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
          raise Exception.Create('Invalid array index: ' + St);
        end;
      Result := TAttributeChain.Create(Attribute, Result, Index);
    end;

    if Attribute = nil then begin
      if Result <> nil then
        Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
      Result := nil;
      Exit;
    end
    else
    if (Attribute.SubDataType <> mongoObject) and
       (Attribute.SubDataType <> mongoArray) and
       (Attribute.SubDataType <> mongoGeneric) and
       (Attribute.SubDataType <> mongoJavaCode) and
       (Attribute.SubDataType <> mongoJavaScopeCode) and
       (Attribute.SubDataType <> mongoRegex) and
       (Attribute.SubDataType <> mongoTimestamp) and
       (Attribute.SubDataType <> mongoBinary) and
       (Attribute.SubDataType <> mongoDBPointer)
    then begin
      if AttrName <> '' then begin
        if Result <> nil then
          Result.First{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
        Result := nil;
      end;
      Exit;
    end;

    OType := TMongoObjectType(Attribute.ObjectType);
    ParentDataType := Attribute.SubDataType;
  until (iPos = 0) and (AttrName = '');
end;

function TMongoObjectType.AddAttribute: TMongoAttribute;
begin
  Result := TMongoAttribute.Create;
  Result.Name := Name;
  Result.FIndex := FAttributes.Add(Result);
end;

function TMongoObjectType.AddAttribute(const Field: TFieldDesc): TMongoAttribute;
begin
  Result := AddAttribute(Field.ParentField.ObjectType, Field.ObjectType, Field.Name, TMongoFieldDesc(Field).FullName, Field.DataType, Field.SubDataType, TMongoFieldDesc(Field).DBType, Field.Length);
end;

function TMongoObjectType.AddAttribute(const Owner, ObjectType: TObjectType; const Name, ActualName: string; const DataType, SubDataType, DBType, Length: Word): TMongoAttribute;
begin
  Result := AddAttribute;

  Result.Owner := Owner;
  Result.ObjectType := ObjectType;
  Result.Name := Name;
  Result.ActualName := ActualName;
  Result.DataType := DataType;
  Result.SubDataType := SubDataType;
  Result.DBType := DBType;
  Result.Length := Length;
end;

{ TDocumentExpression }

constructor TDocumentExpression.Create(const Action: TFieldAction; const Document: TMongoDocument; const Parent: TJSONValue; const Tag: TJSONTag; const Name: string; const Kind: TDocumentKind = dkExpression);
var
  ActualName: string;
  Chain: TAttributeChain;
  DataType,
  SubDataType,
  Len: Word;
  i: integer;
begin
  inherited Create;

  FDocument := Document;
  FKind := Kind;
  FParent := Parent;
  FName := Name;

  if Tag in [jtObjectEnd, jtArrayEnd] then begin
    FParent := FParent.Parent;
    while (FParent <> nil) and not (FParent.Tag in [jtObject, jtArray]) do
      FParent := FParent.Parent;
    Exit;
  end;

  if FParent = nil then begin
    Document.FData := TJSONObject.Create(nil);
    TInternalJSONValue(Document.FData).FData := Document;
    FParent := Document.FData;
    ActualName := Name;
  end
  else if FParent.ActualName <> '' then
    ActualName := FParent.ActualName + '.' + Name
  else
    ActualName := Name;

  if Action = faGet then begin
    if not (FParent.Tag in JSONComplexTags) then
      raise Exception.Create('Field can not have children');
  end
  else
    if not (FParent.Tag in [jtObject, jtArray]) then
      raise Exception.Create('Field is not an object or array');

  case Action of
    faGet: begin
      i := -1;
      if Document.FieldMap.Count > 0 then begin
        i := Document.FieldMap.IndexOf(ActualName);
        if i >= 0 then
          FData := TJSONValue(Document.FieldMap.Objects[i]);
      end;
      if i = -1 then
        FData := TJSONObject(Document.Data)[ActualName];
    end;
    faSet: begin
      Chain := TMongoObjectType(Document.ObjectType).FindAttributeChain(ActualName);
      try
        if Chain = nil then begin
          if Document.FImplicitAddFields or (Document.State = dsNew) then begin
            TMongoObjectType(Document.ObjectType).GetAttributeDataType(Tag, DataType, SubDataType, Len, Document.FUnicode);
            Chain := TMongoObjectType(Document.ObjectType).CreateAttributeChain(ActualName, DataType, SubDataType, SubDataType, Len);
          end
          else
            raise Exception.CreateFmt(SFieldNotFound, [Name]);
        end
        else
          TMongoObjectType(Document.ObjectType).GetAttributeDataType(Tag, DataType, SubDataType, Len, Document.FUnicode);

        if Chain = nil then
          raise Exception.Create('Can not set field value');

        if Chain.Attribute.SubDataType <> SubDataType then begin
          if Document.FImplicitChangeType then begin
            Chain.Attribute.DataType := DataType;
            Chain.Attribute.SubDataType := SubDataType;
            Chain.Attribute.Length := Len;

            if (DataType = dtObject) and not (Tag in JSONConstantTags) then begin
              Chain.Attribute.ObjectType := TMongoObjectType.Create(ActualName);
              Chain.Attribute.ObjectType.Release;
            end;
          end
          else
            raise Exception.CreateFmt(SIncompatibleType, [Name]);
        end;

        case Tag of
          jtObject,
          jtArray: begin
            FData := Document.InternalGetValue(Chain);
            FParent := FData;
          end;
          jtString,
          jtBoolean,
          jtObjectId,
          jtInt32,
          jtInt64,
          jtDouble,
          jtDecimal128,
          jtDateTime,
          jtTimeStamp,
          jtNull,
          jtMinKey,
          jtMaxKey,
          jtUndefined,
          jtBinary,
          jtJavaCode,
          jtJavaScopeCode,
          jtRegex: FData := Document.InternalGetValue(Chain);
        else
          Assert(False);
        end;
      finally
        Chain.Free;
      end;
    end;
    faUnset: begin
      Chain := TMongoObjectType(Document.ObjectType).FindAttributeChain(ActualName);
      try
        Document.InternalDeleteValue(Chain);
      finally
        Chain.Free;
      end;
    end;
  end;
end;

function TDocumentExpression.GetKind: TDocumentKind;
begin
  Result := FKind;
end;

function TDocumentExpression.GetData: TJSONValue;
begin
  Result := FData;
end;

function TDocumentExpression.GetName: string;
begin
  Result := FName;
end;

function TDocumentExpression.GetValue: Variant;
begin
  Result := null;

  if FData <> nil then
    case FData.Tag of
      jtString: Result := TJSONString(FData).Value;
      jtNumber: Result := TJSONNumber(FData).Value;
      jtBoolean: Result := TJSONBoolean(FData).Value;
      jtObjectId: Result := TJSONObjectId(FData).AsString;
      jtInt32: Result := TJSONInt32(FData).Value;
      jtInt64: Result := TJSONInt64(FData).Value;
      jtDateTime: Result := TJSONDateTime(FData).Value;
      jtDouble: Result := TJSONDouble(FData).Value;
    else
      if (FData.Tag in JSONComplexTags) or (FData.Tag in JSONConstantTags) then
        Result := GetText
      else
        raise Exception.Create('Invalid value');
    end;
end;

procedure TDocumentExpression.SetValue(const AValue: Variant);
begin
  case TVarData(AValue).VType of
    varInteger,
    varSmallInt,
    varShortInt,
    varWord,
    varLongWord,
    varByte: FDocument.InternalSetInteger(FName, AValue, FParent);
  {$IFDEF VER12P}
    varUInt64,
  {$ENDIF}
    varInt64: FDocument.InternalSetInt64(FName, AValue, FParent);
    varBoolean: FDocument.InternalSetBoolean(FName, AValue, FParent);
  {$IFDEF MSWINDOWS}
    varCurrency,
  {$ENDIF}
    varSingle,
    varDouble: FDocument.InternalSetDouble(FName, AValue, FParent);
  {$IFDEF VER12P}
    varUString,
  {$ENDIF}
    varString,
    varOleStr: FDocument.InternalSetString(FName, AValue, FParent);
  else
    raise Exception.Create('Invalid value');
  end;
end;

function TDocumentExpression.GetText: string;
begin
  FDocument.FAPI.Serializer.ToText(FData, Result);
end;

function TDocumentExpression.GetFieldCount: integer;
begin
  if (FData = nil) and (FData.Tag <> jtObject) then
    raise Exception.Create(SMethodNotApplicable);

  if FData <> nil then
    Result := TJSONObject(FData).Pairs.Count
  else
    Result := 0;
end;

function TDocumentExpression.GetField(const Index: integer): IDocumentExpression;
begin
  Result := FDocument.InternalGetField(Index, FData);
  if Result = nil then
    raise Exception.CreateFmt(SFieldNotFound, [IntToStr(Index)]);
end;

function TDocumentExpression.GetFieldByName(const Name: string): IDocumentExpression;
begin
  Result := FDocument.InternalGetFieldByName(Name, FData);
  if Result = nil then
    raise Exception.CreateFmt(SFieldNotFound, [Name]);
end;

function TDocumentExpression.SetOid(const Name: string; const Value: TJSONOid): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetOid(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetOid(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetString(const Name: string; const Value: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetString(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetString(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetInteger(const Name: string; const Value: integer): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetInteger(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetInteger(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetInt64(const Name: string; const Value: Int64): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetInt64(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetInt64(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetDouble(const Name: string; const Value: double): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetDouble(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetDouble(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetBoolean(const Name: string; const Value: boolean): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetBoolean(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetBoolean(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetDateTime(const Name: string; const Value: TDateTime): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetDateTime(Name, Value, FParent);
    dkField: Result := FDocument.InternalSetDateTime(Name, Value, FData);
  end;
end;

function TDocumentExpression.SetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetTimestamp(Name, Timestamp, Increment, FParent);
    dkField: Result := FDocument.InternalSetTimestamp(Name, Timestamp, Increment, FData);
  end;
end;

function TDocumentExpression.SetBinary(const Name: string; const Binary: TBytes; const SubType: integer): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetBinary(Name, Binary, SubType, FParent);
    dkField: Result := FDocument.InternalSetBinary(Name, Binary, SubType, FData);
  end;
end;

function TDocumentExpression.SetJavaCode(const Name: string; const Code: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetJavaCode(Name, Code, FParent);
    dkField: Result := FDocument.InternalSetJavaCode(Name, Code, FData);
  end;
end;

function TDocumentExpression.SetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetJavaScopeCode(Name, Code, Scope, FParent);
    dkField: Result := FDocument.InternalSetJavaScopeCode(Name, Code, Scope, FData);
  end;
end;

function TDocumentExpression.SetRegex(const Name: string; const Pattern, Options: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetRegex(Name, Pattern, Options, FParent);
    dkField: Result := FDocument.InternalSetRegex(Name, Pattern, Options, FData);
  end;
end;

function TDocumentExpression.SetNull(const Name: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetNull(Name, FParent);
    dkField: Result := FDocument.InternalSetNull(Name, FData);
  end;
end;

function TDocumentExpression.SetMinKey(const Name: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetMinKey(Name, FParent);
    dkField: Result := FDocument.InternalSetMinKey(Name, FData);
  end;
end;

function TDocumentExpression.SetMaxKey(const Name: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetMaxKey(Name, FParent);
    dkField: Result := FDocument.InternalSetMaxKey(Name, FData);
  end;
end;

function TDocumentExpression.SetObject(const Name: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetObject(Name, FParent);
    dkField: Result := FDocument.InternalSetObject(Name, FData);
  end;
end;

function TDocumentExpression.SetArray(const Name: string): IDocumentExpression;
begin
  case FKind of
    dkExpression: Result := FDocument.InternalSetArray(Name, FParent);
    dkField: Result := FDocument.InternalSetArray(Name, FData);
  end;
end;

function TDocumentExpression.SetEnd: IDocumentExpression;
begin
  if (FParent = nil) or not (FParent.Tag in [jtObject, jtArray]) then
    raise Exception.Create(SMethodNotApplicable);

  Result := TDocumentExpression.Create(faGet, FDocument, FParent, jtObjectEnd, FParent.ActualName);
end;

function TDocumentExpression.FindField(const Name: string): IDocumentExpression;
begin
  Result := FDocument.InternalGetFieldByName(Name, FData);
end;

{ TMongoDocument }

constructor TMongoDocument.Create(const API: TMongoAPI);
begin
  Create(API, nil, False, False, False)
end;

constructor TMongoDocument.Create(const API: TMongoAPI; const Unicode: boolean);
begin
  Create(API, nil, Unicode, False, False);
end;

constructor TMongoDocument.Create(const API: TMongoAPI; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean);
begin
  Create(API, nil, Unicode, ImplicitAddFields, ImplicitChangeType);
end;

constructor TMongoDocument.Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode: boolean);
begin
  Create(API, Handle, Unicode, False, False);
end;

constructor TMongoDocument.Create(const API: TMongoAPI; const Handle: pbson_t; const Unicode, ImplicitAddFields, ImplicitChangeType: boolean);
begin
  inherited Create;

  FState := dsNew;
  SetObjectType(nil);

  FAPI := API;
  FInputStream := nil;
  FOutputStream := nil;
  FData := nil;
  FUnicode := Unicode;
  FImplicitAddFields := ImplicitAddFields;
  FImplicitChangeType := ImplicitChangeType;
  FSerializer := nil;
  FDeserializer := nil;

  FFieldMap := TStringList.Create;

  if Handle <> nil then
    AllocHandle(htOuter, Handle)
  else
    AllocHandle(htNone);
end;

constructor TMongoDocument.Create(const Text: string);
begin
  Create(nil);

  SetText(Text);
end;

destructor TMongoDocument.Destroy;
begin
  SetObjectType(nil);

  FreeHandle;
  FreeData;
  FInputStream.Free;
  FOutputStream.Free;
  FFieldMap.Free;

  inherited;
end;

procedure TMongoDocument.CheckSerializer;
begin
  if FSerializer = nil then
    if FAPI <> nil then
      FSerializer := FAPI.Serializer
    else begin
      FSerializer := TJSONSerializer.Create;
      FSerializerOwned := True;
    end;
end;

procedure TMongoDocument.CheckDeserializer;
begin
  if FDeserializer = nil then
    if FAPI <> nil then
      FDeserializer := FAPI.Deserializer
    else begin
      FDeserializer := TJSONDeserializer.Create;
      FDeserializerOwned := True;
    end;
end;

function TMongoDocument.GetFieldCount: integer;
begin
  if (FData <> nil) and (FData is TJSONObject) then
    Result := TJSONObject(FData).Pairs.Count
  else
    Result := 0;
end;

function TMongoDocument.InternalGetField(const Index: integer; const Parent: TJSONValue): IDocumentExpression;
begin
  if Parent <> nil then begin
    if Parent.Tag = jtObject then
      Result := TDocumentExpression.Create(faGet, Self, Parent, Parent.Tag, TJSONObject(Parent).Pairs[Index].Name.AsString, dkField)
    else if Parent.Tag = jtArray then
      Result := TDocumentExpression.Create(faGet, Self, Parent, Parent.Tag, IntToStr(Index), dkField)
    else
      raise Exception.CreateFmt(SFieldIsNotArray, [Parent.ActualName]);
  end
  else
    Result := nil;

  if (Result <> nil) and (Result.GetData = nil) then
    Result := nil;
end;

function TMongoDocument.InternalGetFieldByName(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  if Parent <> nil then begin
    if Parent.Tag in JSONComplexTags then
      Result := TDocumentExpression.Create(faGet, Self, Parent, Parent.Tag, Name, dkField)
    else
      raise Exception.CreateFmt(SFieldNotHaveChildren, [Parent.ActualName]);
  end
  else
    Result := nil;

  if (Result <> nil) and (Result.GetData = nil) then
    Result := nil;
end;

function TMongoDocument.GetField(const Index: integer): IDocumentExpression;
begin
  Result := InternalGetField(Index, FData);
  if Result = nil then
    raise Exception.CreateFmt(SFieldNotFound, [IntToStr(Index)]);
end;

function TMongoDocument.GetFieldByName(const Name: string): IDocumentExpression;
begin
  Result := InternalGetFieldByName(Name, FData);
  if Result = nil then
    raise Exception.CreateFmt(SFieldNotFound, [Name]);
end;

function TMongoDocument.GetIsNull: boolean;
begin
  Result := False;
end;

function TMongoDocument.InternalCreateValue(const ValueParent: TJSONValue; const DataType: Word; const Size: integer): TJSONValue;
begin
  Result := nil;

  case DataType of
    mongoObject: Result := TJSONObject.Create(ValueParent);
    mongoArray: Result := TJSONArray.Create(ValueParent);
    mongoObjectId: Result := TJSONObjectId.Create(ValueParent);
    mongoString: begin
      if FUnicode then
        Result := TJSONWideString.Create(ValueParent)
      else
        Result := TJSONAnsiString.Create(ValueParent);

      TInternalJSONString(Result).InitValue(Size);
    end;
    mongoNumber: Result := TJSONNumber.Create(ValueParent);
    mongoBoolean: Result := TJSONBoolean.Create(ValueParent);
    mongoNull: Result := TJSONNull.Create(ValueParent);
    mongoInt32: Result := TJSONInt32.Create(ValueParent);
    mongoInt64: Result := TJSONInt64.Create(ValueParent);
    mongoDateTime: Result := TJSONDateTime.Create(ValueParent);
    mongoJavaCode: begin
      Result := TJSONJavaCode.Create(ValueParent);

      if FUnicode then
        TJSONJavaCode(Result).Code := TJSONWideString.Create(Result)
      else
        TJSONJavaCode(Result).Code := TJSONAnsiString.Create(Result);
    end;
    mongoUndefined: Result := TJSONUndefined.Create(ValueParent);
    mongoJavaScopeCode: begin
      Result := TJSONJavaScopeCode.Create(ValueParent);

      if FUnicode then
        TJSONJavaScopeCode(Result).Code := TJSONWideString.Create(Result)
      else
        TJSONJavaScopeCode(Result).Code := TJSONAnsiString.Create(Result);

      TJSONJavaScopeCode(Result).Scope := TJSONObject.Create(Result);
    end;
    mongoRegex: begin
      Result := TJSONRegex.Create(ValueParent);

      if FUnicode then begin
        TJSONRegex(Result).Pattern := TJSONWideString.Create(Result);
        TJSONRegex(Result).Options := TJSONWideString.Create(Result);
      end
      else begin
        TJSONRegex(Result).Pattern := TJSONAnsiString.Create(Result);
        TJSONRegex(Result).Options := TJSONAnsiString.Create(Result);
      end;
    end;
    mongoTimestamp: begin
      Result := TJSONTimestamp.Create(ValueParent);

      TJSONTimestamp(Result).Timestamp := TJSONInt32.Create(Result);
      TJSONTimestamp(Result).Increment := TJSONInt32.Create(Result);
    end;
    mongoBinary: begin
      Result := TJSONBinary.Create(ValueParent);

      TJSONBinary(Result).Binary := TJSONBytes.Create(Result);
      TJSONBinary(Result).Subtype := TJSONInt32.Create(Result);
    end;
    mongoDouble: Result := TJSONDouble.Create(ValueParent);
    mongoBytes: Result := TJSONBytes.Create(ValueParent);
    mongoMinKey: Result := TJSONMinKey.Create(ValueParent);
    mongoMaxKey: Result := TJSONMaxKey.Create(ValueParent);
    mongoDBPointer: begin
      Result := TJSONDBPointer.Create(ValueParent);

      if FUnicode then
        TJSONDBPointer(Result).Name := TJSONWideString.Create(Result)
      else
        TJSONDBPointer(Result).Name := TJSONAnsiString.Create(Result);

    {$IFDEF VER12P}
      TJSONDBPointer(Result).Value := TJSONObjectId.Create(Result);
    {$ELSE}
      TJSONDBPointer(Result).SetValue(TJSONObjectId.Create(Result));
    {$ENDIF}
    end;
  else
    Assert(False);
  end;
end;

function TMongoDocument.InternalGetValue(const Chain: TAttributeChain): TJSONValue;
var
  Attribute: TMongoAttribute;
  Pair: TJSONPair;
  Parent,
  Value: TJSONValue;
  CurChain: TAttributeChain;
begin
  CurChain := Chain;
  while CurChain.Prev <> nil do
    CurChain := CurChain.Prev;
  Parent := FData;
  if Parent = nil then begin
    FData := TJSONObject.Create(nil);
    TInternalJSONValue(FData).FData := Self;
    Parent := FData;
  end;
  Value := nil;

  while CurChain <> nil do begin
    Attribute := TMongoAttribute(CurChain.Attribute);
    if (CurChain.Next = nil) and (Parent.Tag in [jtRegex, jtJavaCode, jtJavaScopeCode, jtBinary, jtDBPointer, jtTimestamp]) then begin
      Value := TJSONObject(FData)[Attribute.ActualName];
      Break;
    end;

    case Parent.Tag of
      jtObject: begin
        Value := TJSONObject(Parent)[Attribute.ActualName];
        if Value = nil then begin
          if FImplicitAddFields or (FState = dsNew) then begin
            Pair := TJSONPair.Create(Parent, Attribute.Name, FUnicode);
            if Attribute.SubDataType <> mongoGeneric then
              Value := InternalCreateValue(Pair, Attribute.SubDataType, Attribute.Length)
            else
              Value := InternalCreateValue(Pair, Attribute.DBType, Attribute.Length);
            Pair.Value := Value;
            TJSONObject(Parent).AddPair(Pair);
          end
          else
            raise Exception.CreateFmt(SFieldNotFound, [Attribute.Name]);
        end
        else if (TMongoConverterManager.GetDBType(Value.Tag) <> Attribute.SubDataType) and (Attribute.SubDataType <> mongoGeneric) then begin
          Pair := TJSONObject(Parent).GetPair(Attribute.Name);
          Pair.Value{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
          Value := InternalCreateValue(Pair, Attribute.SubDataType, Attribute.Length);
          Value.ActualName := Attribute.ActualName;
          Pair.Value := Value;
        end;
      end;
      jtArray: begin
        if (CurChain.Index >= TJSONArray(Parent).Elements.Count) or (CurChain.Index = -1) then begin
          while CurChain.Index > TJSONArray(Parent).Elements.Count do begin
            Value := TJSONNull.Create(Parent);
            TJSONArray(Parent).AddElement(Value);
          end;
          if Attribute.SubDataType <> mongoGeneric then
            Value := InternalCreateValue(Parent, Attribute.SubDataType, Attribute.Length)
          else
            Value := InternalCreateValue(Parent, Attribute.DBType, Attribute.Length);
          TJSONArray(Parent).AddElement(Value);
        end
        else begin
          Value := TJSONArray(Parent).Elements[CurChain.Index];
          if (TMongoConverterManager.GetDBType(Value.Tag) <> Attribute.SubDataType) and (Attribute.SubDataType <> mongoGeneric) then begin
            TJSONArray(Parent).Elements[CurChain.Index]{$IFNDEF NEXTGEN}.Free{$ELSE}.DisposeOf{$ENDIF};
            Value := InternalCreateValue(Parent, Attribute.SubDataType, Attribute.Length);
            Value.ActualName := Attribute.ActualName;
            TJSONArray(Parent).Elements[CurChain.Index] := Value;
          end;
        end;
      end;
      jtJavaScopeCode: Value := TJSONJavaScopeCode(Parent).Scope;
    else
      Value.Free;
      Assert(False);
    end;

    CurChain := CurChain.Next;
    Parent := Value;
  end;

  Result := Value;
end;

procedure TMongoDocument.InternalDeleteValue(const Chain: TAttributeChain);
var
  Attribute: TMongoAttribute;
  Pair: TJSONPair;
  Parent,
  Value: TJSONValue;
  CurChain: TAttributeChain;
begin
  if Chain = nil then
    Exit;

  CurChain := Chain;
  while CurChain.Prev <> nil do
    CurChain := CurChain.Prev;
  Parent := FData;
  Value := nil;

  while CurChain <> nil do begin
    Attribute := TMongoAttribute(CurChain.Attribute);

    case Parent.Tag of
      jtObject: begin
        Pair := TJSONObject(Parent).GetPair(Attribute.Name);
        Assert(Pair <> nil);
        if CurChain.Next = nil then begin
          TJSONObject(Parent).DeletePair(Pair);
          Exit;
        end
        else
          Value := Pair.Value;
      end;
      jtArray: begin
        Assert(Chain.Index <= TJSONArray(Parent).Elements.Count);

        Value := TJSONArray(Parent).Elements[Chain.Index];
        if CurChain.Next = nil then begin
          TJSONArray(Parent).DeleteElement(Value);
          Exit;
        end;
      end;
    else
      Assert(False);
    end;

    CurChain := CurChain.Next;
    Parent := Value;
  end;
end;

function TMongoDocument.VarToJson(const VarValue: Variant): TJSONValue;
begin
  case TVarData(VarValue).VType of
    varInteger,
    varSmallInt,
    varShortInt,
    varWord,
    varLongWord,
    varByte: begin
      Result := InternalCreateValue(nil, mongoInt32, 0);
      TJSONInt32(Result).Value := VarValue;
    end;
  {$IFDEF VER12P}
    varUInt64,
  {$ENDIF}
    varInt64: begin
      Result := InternalCreateValue(nil, mongoInt64, 0);
      TJSONInt64(Result).Value := VarValue;
    end;
    varBoolean: begin
      Result := InternalCreateValue(nil, mongoBoolean, 0);
      TJSONBoolean(Result).Value := VarValue;
    end;
  {$IFDEF MSWINDOWS}
    varCurrency,
  {$ENDIF}
    varSingle,
    varDouble: begin
      Result := InternalCreateValue(nil, mongoDouble, 0);
      TJSONDouble(Result).Value := VarValue;
    end;
  {$IFDEF VER12P}
    varUString,
  {$ENDIF}
    varString,
    varOleStr: begin
      Result := InternalCreateValue(nil, mongoString, 0);
      TJSONString(Result).Value := VarValue;
    end;
  else
    raise Exception.Create('Invalid value');
  end;
end;

function TMongoDocument.GetText: string;
begin
  if FData <> nil then begin
    CheckSerializer;
    FSerializer.ToText(FData, Result);
  end
  else
    Result := '';
end;

procedure TMongoDocument.SetText(const Value: string);
begin
  if FData <> nil then
    FData.Free;

  CheckDeserializer;
  FData := FDeserializer.FromText(Value, nil, FUnicode);
end;

function TMongoDocument.InternalSetOid(const Name: string; const Value: TJSONOid; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtObjectId, Name);
{$IFDEF VER12P}
  TJSONObjectId(Result.GetData).Value := Value;
{$ELSE}
  TJSONObjectId(Result.GetData).SetValue(Value);
{$ENDIF}
end;

function TMongoDocument.InternalSetString(const Name: string; const Value: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtString, Name);
  TJSONString(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetInteger(const Name: string; const Value: integer; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtInt32, Name);
  TJSONInt32(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetInt64(const Name: string; const Value: Int64; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtInt64, Name);
  TJSONInt64(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetDouble(const Name: string; const Value: double; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtDouble, Name);
  TJSONDouble(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetBoolean(const Name: string; const Value: boolean; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtBoolean, Name);
  TJSONBoolean(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetDateTime(const Name: string; const Value: TDateTime; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtDateTime, Name);
  TJSONDateTime(Result.GetData).Value := Value;
end;

function TMongoDocument.InternalSetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtTimestamp, Name);
  TJSONTimestamp(Result.GetData).Timestamp.Value := Timestamp;
  TJSONTimestamp(Result.GetData).Increment.Value := Increment;
end;

function TMongoDocument.InternalSetBinary(const Name: string; const Binary: TBytes; const SubType: integer; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtBinary, Name);
{$IFDEF VER12P}
  TJSONBinary(Result.GetData).Binary.Value := Binary;
{$ELSE}
  TJSONBinary(Result.GetData).Binary.SetValue(Binary);
{$ENDIF}
  TJSONBinary(Result.GetData).Subtype.Value := SubType;
end;

function TMongoDocument.InternalSetJavaCode(const Name: string; const Code: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtJavaCode, Name);
  TJSONJavaCode(Result.GetData).Code.Value := Code;
end;

function TMongoDocument.InternalSetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant; const Parent: TJSONValue): IDocumentExpression;
var
  i: integer;
  TmpValue: TJSONJavaScopeCode;
  TmpName: string;
  TmpPair: TJSONPair;
begin
  if Length(Scope) mod 2 <> 0 then
    raise Exception.Create('Invalid Scope');

  Result := TDocumentExpression.Create(faSet, Self, Parent, jtJavaScopeCode, Name);
  TmpValue := TJSONJavaScopeCode(Result.GetData);
  TmpValue.Code.Value := Code;

  TmpValue.Scope.Pairs.Clear;
  for i := 0 to Length(Scope) div 2 - 1 do begin
    TmpName := VarToStr(Scope[i * 2]);
    TmpPair := TJSONPair.Create(TmpValue.Scope, TmpName, FUnicode);
    TmpPair.Value := VarToJson(Scope[i * 2 + 1]);
    TmpValue.Scope.AddPair(TmpPair);
  end;
end;

function TMongoDocument.InternalSetRegex(const Name: string; const Pattern, Options: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtRegex, Name);
  TJSONRegex(Result.GetData).Pattern.Value := Pattern;
  TJSONRegex(Result.GetData).Options.Value := Options;
end;

function TMongoDocument.InternalSetNull(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtNull, Name);
end;

function TMongoDocument.InternalSetMinKey(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtMinKey, Name);
end;

function TMongoDocument.InternalSetMaxKey(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtMaxKey, Name);
end;

function TMongoDocument.InternalSetObject(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtObject, Name);
end;

function TMongoDocument.InternalSetArray(const Name: string; const Parent: TJSONValue): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faSet, Self, Parent, jtArray, Name);
end;

procedure TMongoDocument.GetAttributeValue(const Name: string; out AttrBuf: IntPtr; out AttrLen: Word; out IsBlank, NativeBuffer: boolean);
var
  Field: IDocumentExpression;
  Chain: TAttributeChain;
  Value: TJSONValue;
begin
  inherited;


  IsBlank := True;
  NativeBuffer := True;
  AttrBuf := nil;

  Field := FindField(Name);
  if Field <> nil then begin
    IsBlank := False;
    AttrBuf := Field.GetData;
    AttrLen := Field.GetData.Size;
  end
  else if FImplicitAddFields or (State = dsNew) then begin
    Chain := TMongoObjectType(ObjectType).FindAttributeChain(Name);
    if Chain <> nil then
      try
        IsBlank := False;
        Value := InternalGetValue(Chain);
        AttrBuf := Value;
        AttrLen := Value.Size;
      finally
        Chain.Free;
      end
    else
      raise Exception.CreateFmt(SFieldNotFound, [Name]);
  end
  else
    raise Exception.CreateFmt(SFieldNotFound, [Name]);
end;

procedure TMongoDocument.SetAttributeValue(const Name: string; ValuePtr: IntPtr; ValueLen: Word);
begin
  // do nothing
end;

function TMongoDocument.GetAttrIsNull(const Field: TFieldDesc): boolean;
var
  FieldExpression: IDocumentExpression;
begin
  FieldExpression := FindField(TMongoFieldDesc(Field).FullName);

  if FieldExpression = nil then
    Result := True
  else if (FieldExpression.GetData.Tag in [jtObjectId, jtBytes]) and (FieldExpression.GetData.Size = 0) then
    Result := True
  else if (FieldExpression.GetData.Tag = jtNull) and not (Field.DataType in [dtString, dtWideString, dtExtString, dtExtWideString]) then
    Result := True
  else if not ((FieldExpression.GetData.Tag in JSONComplexTags) or (FieldExpression.GetData.Tag in JSONConstantTags)) then
    Result := FieldExpression.GetData.ValuePtr = nil
  else
    Result := False;
end;

procedure TMongoDocument.AllocHandle(const HandleType: THandleType; const AHandle: pbson_t = nil);
begin
  FreeHandle;
  FHandleType := HandleType;

  case FHandleType of
    htNew: begin
      FreeData;
      FHandle := FAPI.bson_new;
      FAPI.bson_init(FHandle);
    end;
    htOuter: begin
      FreeData;
      FHandle := AHandle;
      if FInputStream <> nil then
        FInputStream.Free;
      FInputStream := TReadOnlyStream.Create(FAPI.bson_get_data(FHandle), FHandle^.len);
      FData := FAPI.Deserializer.FromBinary(FInputStream, Self, FUnicode) as TJSONObject;
    end;
    htInternal:
      if FData <> nil then begin
        FHandle := FAPI.bson_new;
        if FOutputStream = nil then
          FOutputStream := TMemoryStream.Create
        else
          FOutputStream.Size := 0;
        FAPI.Serializer.ToBinary(FData, FOutputStream);
        FAPI.bson_init_static(FHandle, FOutputStream.Memory, FOutputStream.Size);
      end
      else
        raise Exception.Create('Invalid JSON statement');
  end;
end;

procedure TMongoDocument.FreeHandle;
begin
  if (FHandleType in [htNew, htInternal]) and (FHandle <> nil) then begin
    FAPI.bson_destroy(FHandle);
    FHandle := nil;
    FHandleType := htNone;
  end;
end;

procedure TMongoDocument.Prepare;
begin
  AllocHandle(htInternal);
  FPrepared := True;
end;

procedure TMongoDocument.Unprepare;
begin
  FreeHandle;
  FPrepared := False;
end;

procedure TMongoDocument.SetData(const AData: TJSONValue);
begin
  FreeData;
  FreeHandle;

  FData := AData;
end;

procedure TMongoDocument.FreeData;
begin
  if FData <> nil then
    FreeAndNil(FData);
end;

procedure TMongoDocument.Assign(const Source: TMongoDocument);
begin
  if Source = nil then
    raise Exception.Create(SInvalidDocument);

  if Source.FAPI <> nil then
    FAPI := Source.FAPI;
  FUnicode := Source.FUnicode;
  FImplicitAddFields := Source.FImplicitAddFields;
  SetObjectType(Source.ObjectType);
  if FData <> nil then
    FData.Free;

  FData := Source.FData.Clone(nil, IntPtr(Self)) as TJSONObject;
end;

procedure TMongoDocument.Copy(const Source: TMongoDocument);
begin
  if Source = nil then
    raise Exception.Create(SInvalidDocument);

  if FData <> nil then
    FData.Copy(Source.FData)
  else
    FData := Source.FData.Clone(nil, IntPtr(Self)) as TJSONObject;
end;

function TMongoDocument.SetOid(const Name: string; const Value: TJSONOid): IDocumentExpression;
begin
  Result := InternalSetOid(Name, Value, FData);
end;

function TMongoDocument.SetString(const Name: string; const Value: string): IDocumentExpression;
begin
  Result := InternalSetString(Name, Value, FData);
end;

function TMongoDocument.SetInteger(const Name: string; const Value: integer): IDocumentExpression;
begin
  Result := InternalSetInteger(Name, Value, FData);
end;

function TMongoDocument.SetInt64(const Name: string; const Value: Int64): IDocumentExpression;
begin
  Result := InternalSetInt64(Name, Value, FData);
end;

function TMongoDocument.SetDouble(const Name: string; const Value: double): IDocumentExpression;
begin
  Result := InternalSetDouble(Name, Value, FData);
end;

function TMongoDocument.SetBoolean(const Name: string; const Value: boolean): IDocumentExpression;
begin
  Result := InternalSetBoolean(Name, Value, FData);
end;

function TMongoDocument.SetDateTime(const Name: string; const Value: TDateTime): IDocumentExpression;
begin
  Result := InternalSetDateTime(Name, Value, FData);
end;

function TMongoDocument.SetTimestamp(const Name: string; const Timestamp: integer; Increment: Cardinal): IDocumentExpression;
begin
  Result := InternalSetTimestamp(Name, Timestamp, Increment, FData);
end;

function TMongoDocument.SetBinary(const Name: string; const Binary: TBytes; const SubType: integer): IDocumentExpression;
begin
  Result := InternalSetBinary(Name, Binary, SubType, FData);
end;

function TMongoDocument.SetJavaCode(const Name: string; const Code: string): IDocumentExpression;
begin
  Result := InternalSetJavaCode(Name, Code, FData);
end;

function TMongoDocument.SetJavaScopeCode(const Name: string; const Code: string; const Scope: array of Variant): IDocumentExpression;
begin
  Result := InternalSetJavaScopeCode(Name, Code, Scope, FData);
end;

function TMongoDocument.SetRegex(const Name: string; const Pattern, Options: string): IDocumentExpression;
begin
  Result := InternalSetRegex(Name, Pattern, Options, FData);
end;

function TMongoDocument.SetNull(const Name: string): IDocumentExpression;
begin
  Result := InternalSetNull(Name, FData);
end;

function TMongoDocument.SetMinKey(const Name: string): IDocumentExpression;
begin
  Result := InternalSetMinKey(Name, FData);
end;

function TMongoDocument.SetMaxKey(const Name: string): IDocumentExpression;
begin
  Result := InternalSetMaxKey(Name, FData);
end;

function TMongoDocument.SetObject(const Name: string): IDocumentExpression;
begin
  Result := InternalSetObject(Name, FData);
end;

function TMongoDocument.SetArray(const Name: string): IDocumentExpression;
begin
  Result := InternalSetArray(Name, FData);
end;

function TMongoDocument.Unset(const Name: string): IDocumentExpression;
begin
  Result := TDocumentExpression.Create(faUnset, Self, FData, jtNull, Name);
end;

procedure TMongoDocument.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMongoDocument.LoadFromStream(Stream: TStream);
begin
  if FData <> nil then
    FData.Free;

  CheckDeserializer;
  FData := FDeserializer.FromText(Stream, nil, FUnicode);
end;

procedure TMongoDocument.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Size := 0;
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMongoDocument.SaveToStream(Stream: TStream);
begin
  if FData <> nil then begin
    CheckSerializer;
    FSerializer.ToText(FData, Stream);
  end
  else
    Stream.Size := 0;
end;

function TMongoDocument.FindField(const Name: string): IDocumentExpression;
begin
  Result := InternalGetFieldByName(Name, FData);
end;

end.
