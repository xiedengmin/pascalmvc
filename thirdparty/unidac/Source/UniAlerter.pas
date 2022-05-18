
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniAlerter;
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DB,
  CRTypes, CRAccess, MemUtils, MemData,
  {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, DBAccess, DAAlerter,
  Uni, UniProvider;

type

{ TUniAlerter }

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniAlerter = class (TDAAlerter)
  private
    FSpecificOptions: TSpecificOptionsHolder;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetUniTransaction: TUniTransaction;
    procedure SetUniTransaction(Value: TUniTransaction);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);

  protected
    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    procedure AssignTo(Dest: TPersistent); override;
    function GetInternalAlerterClass: TCRAlerterClass; override;
    procedure SetInternalAlerter(Value: TCRAlerter); override;
    procedure BeginConnection(WithTransaction: boolean = True); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetUniTransaction write SetUniTransaction stored IsTransactionStored;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;
    property Events;
    property Active;
    property AutoRegister;

    property OnEvent;
    property OnError;
  end;

implementation

uses
  DAConsts;

{ TUniAlerter }

constructor TUniAlerter.Create(Owner: TComponent);
begin
  inherited Create(Owner);

  FSpecificOptions := TSpecificOptionsHolder.Create(Self, GetProvider);
end;

destructor TUniAlerter.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

function TUniAlerter.CanGetProvider: boolean;
begin
  Result := TUniUtils.CanGetProvider(TUniConnection(FConnection));
end;

function TUniAlerter.GetProvider: TUniProvider;
begin
  if FConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniUtils.GetProvider(TUniConnection(FConnection));
end;

procedure TUniAlerter.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniAlerter then begin
    TUniAlerter(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniAlerter.GetInternalAlerterClass: TCRAlerterClass;
begin
  Result := GetProvider.GetAlerterClass;
end;

procedure TUniAlerter.SetInternalAlerter(Value: TCRAlerter);
begin
  inherited;

  if FIAlerter <> nil then
    GetProvider.SetObjectProps(FIAlerter, FSpecificOptions.Values);
end;

procedure TUniAlerter.BeginConnection(WithTransaction: boolean = True);
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FIAlerter, FSpecificOptions.Values);
    FSpecificOptions.IsModified := False;
  end;
end;

function TUniAlerter.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniAlerter.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniAlerter.GetUniTransaction: TUniTransaction;
var
  vConnection: TCustomDAConnection;
begin
  Result := TUniTransaction(inherited Transaction);

  if Result = nil then begin
    vConnection := UsedConnection;
    if (vConnection <> nil) and (TUniConnection(vConnection).DefaultTransaction <> nil) then
      Result := TUniConnection(vConnection).DefaultTransaction
  end;
end;

procedure TUniAlerter.SetUniTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TUniAlerter.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniAlerter.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

end.
 
