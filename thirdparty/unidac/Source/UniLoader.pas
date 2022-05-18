
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniLoader;
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DB,
  CRTypes, CRAccess, MemUtils, MemData,
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, DBAccess, DALoader,
  Uni, UniProvider;

type
  TUniLoaderColumn = class (TDAColumn)
  private
    FSize: integer;
    FPrecision: integer;
    FScale: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Size: integer read FSize write FSize default 0;
    property Precision: integer read FPrecision write FPrecision default 0;
    property Scale: integer read FScale write FScale default 0;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniLoader = class (TDALoader)
  private
    FSpecificOptions: TSpecificOptionsHolder;
    FUseBlankValues: boolean;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);
    procedure SetUseBlankValues(const Value: boolean);

  protected
    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    procedure AssignTo(Dest: TPersistent); override;
    function GetInternalLoaderClass: TCRLoaderClass; override;
    procedure SetInternalLoader(Value: TCRLoader); override;
    class function GetColumnClass: TDAColumnClass; override;
    function GetFieldTypeMapClass: TFieldTypeMapClass; override;
    function  NeedRecreateColumns: boolean; override;
    procedure ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); override;
    procedure WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn); override;
    procedure BeginConnection; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;
    property UseBlankValues: boolean read FUseBlankValues write SetUseBlankValues default True;
    property TableName;
    property Columns;

    property OnPutData;
    property OnGetColumnData;
    property OnProgress;
  end;


implementation

uses
  DAConsts;

{ TUniLoaderColumn }

procedure TUniLoaderColumn.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniLoaderColumn then begin
    TUniLoaderColumn(Dest).Size := Size;
    TUniLoaderColumn(Dest).Precision := Precision;
    TUniLoaderColumn(Dest).Scale := Scale;
  end;
end;

{ TUniLoader }

constructor TUniLoader.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSpecificOptions := TSpecificOptionsHolder.Create(Self, GetProvider);
  FUseBlankValues := True;
end;

destructor TUniLoader.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

function TUniLoader.NeedRecreateColumns: boolean;
begin
  Result := True;
end;

procedure TUniLoader.ReadColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  inherited;

  with TUniLoaderColumn(Column) do begin
    Size := CRColumn.Size;
    Precision := CRColumn.Precision;
    Scale := CRColumn.Scale;
  end;
end;

procedure TUniLoader.WriteColumn(Column: TDAColumn; CRColumn: TCRLoaderColumn);
begin
  inherited;

  with TUniLoaderColumn(Column) do begin
    CRColumn.Size := Size;
    CRColumn.Precision := Precision;
    CRColumn.Scale := Scale;
  end;
end;

procedure TUniLoader.BeginConnection;
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FILoader, FSpecificOptions.Values);
    GetProvider.SetObjectProps(Self, FSpecificOptions.Values);
    FSpecificOptions.IsModified := False;
  end;
end;

function TUniLoader.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniLoader.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniLoader.GetUniTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniLoader.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

procedure TUniLoader.SetUseBlankValues(const Value: boolean);
begin
  if FUseBlankValues <> Value then begin
    FUseBlankValues := Value;
    Options.UseBlankValues := Value;
  end;
end;

function TUniLoader.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniLoader.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniLoader.CanGetProvider: boolean;
begin
  Result := TUniUtils.CanGetProvider(TUniConnection(FConnection));
end;

function TUniLoader.GetProvider: TUniProvider;
begin
  if FConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniUtils.GetProvider(TUniConnection(FConnection));
end;

procedure TUniLoader.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniLoader then begin
    TUniLoader(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniLoader.GetInternalLoaderClass: TCRLoaderClass;
var
  Connection: TCustomDAConnection;
begin
  Connection := UsedConnection;
  if Connection <> nil then
    Result := TDBAccessUtils.GetIConnection(Connection).GetLoaderClass
  else
    Result := TCRLoader;
end;

procedure TUniLoader.SetInternalLoader(Value: TCRLoader);
begin
  inherited;

  if FILoader <> nil then begin
    GetProvider.SetObjectProps(FILoader, FSpecificOptions.Values);
    GetProvider.SetObjectProps(Self, FSpecificOptions.Values);
  end;
end;

class function TUniLoader.GetColumnClass: TDAColumnClass;
begin
  Result := TUniLoaderColumn;
end;

function TUniLoader.GetFieldTypeMapClass: TFieldTypeMapClass;
begin
  Result := GetProvider.GetFieldTypeMapClass;
end;

end.
