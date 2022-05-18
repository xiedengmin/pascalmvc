unit RandomForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ScTypes;

type
  TfmRandom = class(TForm)
    ProgressBar: TProgressBar;
    lbInform: TLabel;
    btClose: TButton;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FData: TBytes;
    FCount: Integer;
  public
    property Data: TBytes read FData;
  end;

var
  fmRandom: TfmRandom;

implementation

{$IFNDEF FPC}
{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

{$IFDEF XPMAN}
  {$R WindowsXP.res}
{$ENDIF}

const DATASIZE = 512;

function PerfCounter: Int64;
begin
  if not QueryPerformanceCounter(Result) then
    Result := GetTickCount;
end;

procedure TfmRandom.FormCreate(Sender: TObject);
begin
  SetLength(FData, DATASIZE);
  ProgressBar.Max := DATASIZE;
end;

procedure TfmRandom.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  data: Int64;
begin
  if FCount >= DATASIZE then
    Exit;

  data := X xor Y xor PerfCounter;
  FData[FCount] := byte(data);
  Inc(FCount);
  data := data shr 8;
  FData[FCount] := byte(data);
  Inc(FCount);
  ProgressBar.StepIt;

  if FCount >= DATASIZE then begin
    lbInform.Caption := 'The data for the random generator has been generated!';
    btClose.ModalResult := mrOk;
    btClose.Caption := '&OK';
  end;
end;

end.
