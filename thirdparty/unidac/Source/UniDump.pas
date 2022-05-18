
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}

unit UniDump;
{$ENDIF}

interface

uses
  Classes, SysUtils, Variants, DB,
  CRTypes, MemData, {$IFDEF FPC}MemDataSet{$ELSE}MemDS{$ENDIF}, 
  CRAccess, DBAccess, DADump, DAScript, Uni, UniProvider;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniDump = class(TDADump)
  private
    FSpecificOptions: TSpecificOptionsHolder;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetSpecificOptions: TSpecificOptionsList;
    procedure SetSpecificOptions(Value: TSpecificOptionsList);

  protected
    function CanGetProvider: boolean;
    function GetProvider: TUniProvider;

    procedure AssignTo(Dest: TPersistent); override;
    function GetProcessorClass: TDADumpProcessorClass; override;
    procedure SetProcessor(Value: TDADumpProcessor); override;
    function GetTableNames: string; override;
    procedure SetTableNames(Value: string); override;
    function CreateScript: TDAScript; override;
    function GenerateHeader: string; override;
    procedure BeginConnection; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property Connection: TUniConnection read GetConnection write SetConnection;
    property SpecificOptions: TSpecificOptionsList read GetSpecificOptions write SetSpecificOptions;

    property Options;
  end;

implementation

uses
  CRFunctions, DAConsts, UniScript;

constructor TUniDump.Create(Owner: TComponent);
begin
  inherited;

  FSpecificOptions := TSpecificOptionsHolder.Create(Self, GetProvider);
end;

destructor TUniDump.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

function TUniDump.CanGetProvider: boolean;
begin
  Result := TUniUtils.CanGetProvider(TUniConnection(FConnection));
end;

function TUniDump.GetProvider: TUniProvider;
begin
  if FConnection = nil then
    DatabaseError(SConnectionNotDefined);

  Result := TUniUtils.GetProvider(TUniConnection(FConnection));
end;

procedure TUniDump.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniDump then begin
    TUniDump(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniDump.GetProcessorClass: TDADumpProcessorClass;
begin
  Result := GetProvider.GetDumpProcessorClass;
end;

procedure TUniDump.SetProcessor(Value: TDADumpProcessor);
begin
  inherited;

  if FProcessor <> nil then
    GetProvider.SetObjectProps(FProcessor, FSpecificOptions.Values);
end;

function TUniDump.GenerateHeader: string;
var
  ProviderName: string;
begin
  ProviderName := GetProvider.GetProviderName;
  Result := Format(SBHCaption, ['UniDAC', UniDacVersion,
    ProviderName, Connection.ServerVersion, ProviderName, Connection.ClientVersion,
    DateTimeToStr(Now), Connection.Server, Connection.Database]);
end;

procedure TUniDump.BeginConnection;
begin
  inherited;

  if FSpecificOptions.IsModified then begin
    GetProvider.SetObjectProps(FProcessor, FSpecificOptions.Values);
    FSpecificOptions.IsModified := False;
  end;
end;

function TUniDump.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniDump.SetConnection(Value: TUniConnection);
begin
  inherited Connection := Value;
end;

function TUniDump.GetSpecificOptions: TSpecificOptionsList;
begin
  Result := FSpecificOptions.Values;
end;

procedure TUniDump.SetSpecificOptions(Value: TSpecificOptionsList);
begin
  FSpecificOptions.Values.Assign(Value);
end;

function TUniDump.GetTableNames: string;
begin
  Result := DefaultSQLInfo.NamesFromList(FTables);
end;

procedure TUniDump.SetTableNames(Value: string);
begin
  DefaultSQLInfo.NamesToList(Value, FTables);
end;

function TUniDump.CreateScript: TDAScript;
begin
  Result := TUniScript.Create(nil);
end;

end.
 
