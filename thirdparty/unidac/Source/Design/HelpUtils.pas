
//////////////////////////////////////////////////
//  Copyright © 1998-2021 Devart. All right reserved.
//  Help Utils
//////////////////////////////////////////////////

{$I Dac.inc}
unit HelpUtils;

interface

uses
  {$IFDEF FPC}
  lclintf,
  {$ENDIF}
{$IFDEF MSWINDOWS}
  SysUtils,
{$IFDEF VER16P}
  UITypes,
{$ENDIF}
{$IFDEF VER9P}
  Dialogs;
{$ELSE}
  Forms;
{$ENDIF}
{$ELSE}
  SysUtils;
{$ENDIF}

  procedure ShowHelp(HelpFile, JumpID: string);
  procedure OpenUrl(Url: string);
  procedure MailTo(Address: string);

implementation

{$IFDEF MSWINDOWS}
uses
  Registry, ShellAPI, ShlObj, Windows,
{$IFDEF VER9P}
{$IFNDEF VER22P}
  HelpIntfs,
{$ENDIF}
{$ENDIF}
  CRParser;
{$ENDIF}

{$IFDEF VER9P}
{$IFNDEF VER22P}
type
  IApiHelpSystem = IHelpSystem;
{$ENDIF}
{$ENDIF}

procedure ShowHelp(HelpFile, JumpID: string);
{$IFDEF MSWINDOWS}
{$IFNDEF VER22P}
var
{$IFDEF VER9P}
  HelpSystem: IApiHelpSystem;
{$ELSE}
  OldFile: string;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
{$IFDEF VER22P}
  if ExtractFileExt(HelpFile) = '.chm' then begin
    ShellExecute(0, 'open', PChar(HelpFile), '', '', SW_SHOW);
  end
  else
    Assert(False);
{$ELSE}
{$IFDEF VER9P}
  GetHelpSystem(HelpSystem);
  if Assigned(HelpSystem) then
    HelpSystem.ShowTopicHelp(JumpID, HelpFile)
  else
    MessageDlg('Failed to use IDE HelpSystem', mtError, [mbOk], 0);
{$ELSE}
  if ExtractFileExt(HelpFile) = '.hlp' then begin
    OldFile := Application.HelpFile;
    try
      Application.HelpFile := HelpFile;
    {$IFNDEF FPC}
      Application.HelpJump(JumpID);
    {$ENDIF}
    finally
      Application.HelpFile := OldFile;
    end;
  end
  else
  if ExtractFileExt(HelpFile) = '.chm' then begin
    ShellExecute(0, 'open', PChar(HelpFile), '', '', SW_SHOW);
  end
  else
    Assert(False);
{$ENDIF}
{$ENDIF}
{$ENDIF MSWINDOWS}
end;

procedure OpenUrl(Url: string);
{$IFDEF MSWINDOWS}
var
  htmlDescription: string;
  shellcommand: string;
  Path: array[0..MAX_PATH] of char;
  Filename: string;
  Parser: TParser;
{$ENDIF}
begin
  if (LowerCase(Copy(Trim(Url), 1, 4)) <> 'http') then
    Url := 'http://' + Url;
{$IFDEF MSWINDOWS}
  if ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOW) > 32 then
    Exit;

  Filename := '';
  with TRegistry.Create(KEY_READ OR KEY_WRITE) do begin
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly('.htm') and KeyExists('') then begin
      htmlDescription := ReadString('');
      CloseKey;
      if OpenKeyReadOnly(htmlDescription + '\shell\open\Command') and KeyExists('') then begin
        shellcommand := Trim(ReadString(''));
        Parser := TParser.Create(shellcommand);
        try
          parser.GetNext(Filename);
          if not FileExists(Filename) then
            Filename := '';
        finally
          Parser.Free;
        end;
      end;
    end;
  end;

  if Filename = '' then begin
  {$IFDEF FPC}
    Path := '';
  {$ELSE}
    SHGetSpecialFolderPath(0, Path, $26{CSIDL_PROGRAM_FILES}, False);
  {$ENDIF}
    FileName := Path + '\Internet Explorer\iexplore.exe';
  end;
  ShellExecute(0, 'open', PChar(Filename), PChar(Url), '', SW_SHOW);
{$ELSE}
  {$IFDEF FPC}
  lclintf.OpenURL(Url);
  {$ENDIF}
{$ENDIF}
end;

procedure MailTo(Address: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar('mailto:' + Address), '', '', SW_SHOW);
{$ELSE}
  {$IFDEF FPC}
  lclintf.OpenURL(PChar('mailto:' + Address));
  {$ENDIF}
{$ENDIF}
end;

end.
