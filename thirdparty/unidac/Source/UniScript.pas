
//////////////////////////////////////////////////
//  Universal Data Access Components
//  Copyright � 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF CLR}
{$I UniDac.inc}
unit UniScript;
{$ENDIF}

interface

uses
  Classes, SysUtils, 
  CRTypes, CRParser, CRAccess, DAScript, DBAccess, Uni;

type

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TUniScript = class(TDAScript)
  private
    FSpecificOptions: TStrings;

    function GetConnection: TUniConnection;
    procedure SetConnection(Value: TUniConnection);
    function GetTransaction: TUniTransaction;
    procedure SetTransaction(Value: TUniTransaction);
    //procedure AssignTo(Dest: TPersistent); override;
    function GetDataSet: TCustomUniDataSet;
    procedure SetDataSet(Value: TCustomUniDataSet);
    procedure SetSpecificOptions(Value: TStrings);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetProcessorClass: TDAScriptProcessorClass; override;
    procedure SetProcessor(Value: TDAScriptProcessor); override;
    function CreateCommand: TCustomDASQL; override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

  published
    property AutoCommit;
    property ScanParams;

    property Connection: TUniConnection read GetConnection write SetConnection;
    property Transaction: TUniTransaction read GetTransaction write SetTransaction stored IsTransactionStored;
    property DataSet: TCustomUniDataSet read GetDataSet write SetDataSet;
    property SpecificOptions: TStrings read FSpecificOptions write SetSpecificOptions;
  end;

implementation

uses
  DAConsts;

{ TUniScript }

constructor TUniScript.Create(Owner: TComponent);
begin
  inherited;

  FSpecificOptions := TStringList.Create;
end;

destructor TUniScript.Destroy;
begin
  FSpecificOptions.Free;

  inherited;
end;

function TUniScript.GetConnection: TUniConnection;
begin
  Result := TUniConnection(inherited Connection);
end;

procedure TUniScript.SetConnection(Value: TUniConnection);
begin
  inherited SetConnection(Value);
end;

function TUniScript.GetTransaction: TUniTransaction;
begin
  Result := TUniTransaction(inherited Transaction);
end;

procedure TUniScript.SetTransaction(Value: TUniTransaction);
begin
  inherited Transaction := Value;
end;

function TUniScript.GetDataSet: TCustomUniDataSet;
begin
  Result := TCustomUniDataSet(inherited DataSet);
end;

procedure TUniScript.SetDataSet(Value: TCustomUniDataSet);
begin
  inherited DataSet := Value;
end;

procedure TUniScript.SetSpecificOptions(Value: TStrings);
begin
  FSpecificOptions.Assign(Value);
end;

procedure TUniScript.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TUniScript then begin
    TUniScript(Dest).SpecificOptions.Assign(SpecificOptions);
  end;
end;

function TUniScript.CreateCommand: TCustomDASQL;
begin
  Result := TUniSQL.Create(nil);
end;

function TUniScript.GetProcessorClass: TDAScriptProcessorClass;
begin
  if UsedConnection = nil then
    raise Exception.Create(SConnectionNotDefined);
  Result := TUniUtils.GetProvider(TUniConnection(UsedConnection)).GetScriptProcessorClass;
end;

procedure TUniScript.SetProcessor(Value: TDAScriptProcessor);
begin
  inherited;

  if FProcessor <> nil then begin
    TUniUtils.GetProvider(TUniConnection(UsedConnection)).SetObjectProps(
      FProcessor, FSpecificOptions);
  end;
end;

end.