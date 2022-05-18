
//////////////////////////////////////////////////
//  Oracle Data Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  Oracle Server Enumerator
//////////////////////////////////////////////////

{$I Odac.inc}
unit OraServerEnumeratorUni;

interface

uses
  Classes, SysUtils, CRServerEnumerator, 
{$IFNDEF UNIDACPRO}
  OraCall;
{$ELSE}
  OraCallUni;
{$ENDIF}

type
  TOraServerEnumerator = class (TCRServerEnumerator)
  private
    FDirect: boolean;
    FHomeName: string;

  public
    function SetProp(Prop: integer; const Value: variant): boolean; override;
    function GetProp(Prop: integer; var Value: variant): boolean; override;

    function GetTNSFileName(Home: TOracleHome): string;
    procedure GetServerList(List: TStrings); override;
  end;

implementation

uses
  CRProps, CRFunctions, CRParser,
{$IFNDEF UNIDACPRO}
  OraProps, OraParser;
{$ELSE}
  OraPropsUni, OraParserUni;
{$ENDIF}

{ TOraServerEnumerator }

function TOraServerEnumerator.GetTNSFileName(Home: TOracleHome): string;
const
  sTnsAdmin = 'TNS_ADMIN';
  sTnsNames = 'tnsnames.ora';
{$IFDEF MSWINDOWS}
  sNetwork = '\network\admin\';
  sNet80 = '\net80\admin\';
{$ELSE}
  sNetwork = '/network/admin/';
{$ENDIF}
var
{$IFDEF MSWINDOWS}
{$ENDIF}
  FileName: string;
  TNSFound: boolean;

  function AddPath(Path, FileName: string): string;
  begin
    if (Path <> '') and (Path[Length(Path)] <> '\') then
      Result := Path + '\' + FileName
    else
      Result := Path + FileName;
  end;

begin
  TNSFound := False;

  if Home = nil then
    Exit;

  if not TNSFound and (Home.TNSPath <> '') then begin
    FileName := AddPath(Home.TNSPath, sTnsNames);
    if FileExists(FileName) then
      TNSFound := True;
  end;

  if not TNSFound then begin
    if Home.Path <> '' then begin
      FileName := Home.Path + sNetwork + sTnsNames;
      if FileExists(FileName) then
        TNSFound := True
      else begin
      {$IFDEF MSWINDOWS}
        FileName := Home.Path + sNet80 + sTnsNames;
        if FileExists(FileName) then
          TNSFound := True
      {$ENDIF}
      end;
    end;
  end;

  if TNSFound then
    Result := FileName;
end;

procedure TOraServerEnumerator.GetServerList(List: TStrings);
var
  i, j, p: integer;
  Home: TOracleHome;
  IFile, F, LoadedFiles: TStrings;
  Parser: TTNSParser;
  Code: integer;
  FileName, CurrentDir, St, Alias: string;
  Bracket: integer;
begin
  inherited;

  if not FDirect then
    if FHomeName <> '' then
      Home := OracleHomes.FindHome(FHomeName)
    else
      Home := OracleHomes.Default
  else
    Home := nil;

  if Home = nil then
    exit;

  FileName := GetTNSFileName(Home);
  if FileName = '' then
    exit;

  F := TStringList.Create;
  try
    F.LoadFromFile(FileName);

    IFile := TStringList.Create;
    i := 0;
    CurrentDir := GetCurrentDir;
    SetCurrentDir(ExtractFileDir(FileName));
    LoadedFiles := TStringList.Create;
    try
      while i < F.Count do begin
        p := Pos('IFILE', UpperCase(F[i]));
        if p > 0 then begin
          LoadedFiles.Add(FileName);

          for j := p - 1 downto 1 do
            if F[i][j] = '#' then begin
              p := 0;
              Break;
            end;

          if p = 0 then begin
            Inc(i);
            continue;
          end;

          FileName := Trim(Copy(F[i], Pos('=',F[i]) + 1, MaxInt));

          F.Delete(i);

          if LoadedFiles.IndexOf(FileName) < 0  then //skip duplicate file name
            LoadedFiles.Add(FileName)
          else
            break;

          if FileExists(FileName) then begin
            IFile.LoadFromFile(FileName);
            for j := IFile.Count - 1 downto 0 do
              F.Insert(i, IFile[j]);
          end;
        end;
        Inc(i);
      end;
    finally
      LoadedFiles.Free;
      SetCurrentDir(CurrentDir);
      IFile.Free;
    end;

    Parser := TTNSParser.Create('');
    try
      Parser.OmitBlank := True;
      Parser.OmitComment := True;
      Parser.SetText(F.Text);
      Code := 0;
      St := '';
      Alias := '';
      Bracket := 0;
      List.Clear;
      repeat
        if (Bracket = 0) and ((Code = lcIdent) or (Code = lcNumber) or (St = '-') or (St = '.')) then
          Alias := Alias + St;

        Code := Parser.GetNext(St);
        if Code = lxLeftBracket then
          Inc(Bracket)
        else if Code = lxRightBracket then
          Dec(Bracket)
        else if ((Code = lxEqual) or (Code = lxComma)) and (Bracket = 0) then begin
          List.Add(Alias);
          Alias := '';
        end;
      until Code = lcEnd;
    finally
      Parser.Free;
    end;
  finally
    F.Free;
  end;
end;

function TOraServerEnumerator.GetProp(Prop: integer; var Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prHomeName:
      Value := FHomeName;
  else
    Result := inherited GetProp(Prop, Value);
  end;
end;

function TOraServerEnumerator.SetProp(Prop: integer; const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDirect:
      FDirect := Value;
    prHomeName:
      FHomeName := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

end.
