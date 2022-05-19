(*   _                     _
 *  | |__  _ __ ___   ___ | | __
 *  | '_ \| '__/ _ \ / _ \| |/ /
 *  | |_) | | | (_) | (_) |   <
 *  |_.__/|_|  \___/ \___/|_|\_\
 *
 * Microframework which helps to develop web Pascal applications.
 *
 * Copyright (c) 2012-2020 Silvio Clecio <silvioprog@gmail.com>
 *
 * Brook framework is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Brook framework is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Brook framework; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 *)

unit String_frMain;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Dialogs,
  FMX.Forms,
  BrookHandledClasses,
  BrookString;

type
  TfrMain = class(TForm)
    lbDesc: TLabel;
    btAddNow: TButton;
    btShowContent: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btAddNowClick(Sender: TObject);
    procedure btShowContentClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure BrookLibraryLoader1Load(Sender: TObject);
  private
    FString: TBrookString;
  protected
    procedure UpdateButtons;
  end;

var
  frMain: TfrMain;

implementation

{$R *.fmx}

procedure TfrMain.FormCreate(Sender: TObject);
begin
  FString := TBrookString.Create(nil);
end;

procedure TfrMain.FormDestroy(Sender: TObject);
begin
  FString.Free;
end;

procedure TfrMain.UpdateButtons;
begin
  btShowContent.Enabled := FString.Length > 0;
  btClear.Enabled := btShowContent.Enabled;
end;

procedure TfrMain.BrookLibraryLoader1Load(Sender: TObject);
begin
  btAddNow.Enabled := True;
end;

procedure TfrMain.btAddNowClick(Sender: TObject);
begin
  FString.Write(Format('%s%s',
    [FormatDateTime('hh:nn:ss.zzz', Now), sLineBreak]));
  UpdateButtons;
end;

procedure TfrMain.btShowContentClick(Sender: TObject);
begin
  ShowMessageFmt('All clicks:%s%s%s', [sLineBreak, sLineBreak, FString.Text]);
end;

procedure TfrMain.btClearClick(Sender: TObject);
begin
  FString.Clear;
  UpdateButtons;
end;

end.
