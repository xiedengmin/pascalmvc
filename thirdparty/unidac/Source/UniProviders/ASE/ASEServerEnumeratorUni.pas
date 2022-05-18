
//////////////////////////////////////////////////
//  ASE Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I ASEDac.inc}
unit ASEServerEnumeratorUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry, IniFiles,
{$ENDIF}
  Classes, Types, SysUtils, StrUtils,
  CRServerEnumerator;

type
  TASEServerEnumerator = class (TCRServerEnumerator)
  public
    procedure GetServerList(List: TStrings); override;
  end;

implementation

uses
  CRFunctions;

{ TASEServerEnumerator }

procedure TASEServerEnumerator.GetServerList(List: TStrings);
{$IFDEF MSWINDOWS}
var
  List1: TStringList;
  Reg: TRegistry;
  RootPath: string;
  ASEIni: TIniFile;
  IniSection, IniParams: TStringList;
  i: integer;
  ParamName: string;
{$ENDIF}
begin
  List.Clear;
{$IFDEF MSWINDOWS}
  List1 := TStringList.create;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\SYBASE\Setup') then begin
      RootPath := Reg.ReadString('SYBASE');
      if FileExists(RootPath + '\ini\sql.ini') then begin
        ASEIni := TIniFile.Create(RootPath + '\ini\sql.ini');
        IniSection := TStringList.Create;
        try
          ASEIni.ReadSections(IniSection);
          for i := 0 to IniSection.Count - 1 do begin
            if ASEIni.ValueExists(IniSection[i], 'master') then
              ParamName := 'master'
            else if ASEIni.ValueExists(IniSection[i], 'query') then
              ParamName := 'query'
            else
              exit;
            IniParams := TStringList.Create;
            try
              IniParams.Delimiter := ',';
              IniParams.DelimitedText := ASEIni.ReadString(IniSection[i], ParamName, '');
              List1.AddObject(IniParams[1], TObject(StrToInt(IniParams[2])));
            finally
              IniParams.Free;
            end;
          end;
          AssignStrings(List1, List);
        finally
          IniSection.Free;
          ASEIni.Free;
        end;
      end;
    end;
  finally
    Reg.Free;
    List1.Free;
  end;
{$ENDIF}
end;

end.
