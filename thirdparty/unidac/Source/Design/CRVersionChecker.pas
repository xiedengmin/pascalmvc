
{$I Dac.inc}
unit CRVersionChecker;

interface

uses
  Classes, SysUtils, Windows, WinInet, Forms, StdCtrls, Controls,
  ExtCtrls, Registry, Graphics,
{$IFDEF VER6P}
  StrUtils,
{$ENDIF}
  CRTypes, CRFunctions, CRHttp, CLRClasses, HelpUtils;

type
  TResultCode = (rcSuccess, rcFileNotFound, rcIncorrectSecurity, rcProductNotFound, rcInvalidUrl, rcNoPermissions);

  TFileVersion = record
    Major,
    Minor,
    Release,
    Build: integer;
    Edition: string;
  end;

  TDACInfo = record
    Name,
    Version,
    Edition: string;
  end;

  TDACVersion = record
    Name,
    CurrentVersion,
    NewVersion,
    Edition: string;
  end;

const
  ResultErrorMessages: array[TResultCode] of string = (
    'Operation completed successfully',
    'Cannot find a new installation file.',
    'Invalid login or password.',
    'The product is not found on the site.',
    'Invalid URL was generated.',
    'There is no access to the site or invalid login or password.'
  );

  DACUpdatesRegistryKey = 'Software\Devart\DAC\CheckUpdates';

type
  TCustomVersionChecker = class
  protected
    class procedure ShowDACUpdates;
    class function NeedCheck: boolean;
    class function GetUrlContent(const Url: string): string;
    class function ExtractTextVersion(const ResultString: string): string;
    class function ExtractJsonVersion(const ResultString: string): string;
  public
    class function VersionToInt(Version: string): Integer;
    class function GetFileVersion(FileName: string): TFileVersion;
    class function GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string; virtual;
  end;

  TVersionChecker = class (TCustomVersionChecker)
  public
    class function GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string; override;
  end;

  TVersionCheckerGet = class (TCustomVersionChecker)
  public
    class function GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string; override;
  end;

  TVersionCheckerPost = class (TCustomVersionChecker)
  public
    class function GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string; override;
  end;

  TCheckUpdatesForm = class (TForm)
  private
    pCaption: TPanel;
    pDACs: TPanel;
    pControls: TPanel;
    img: TImage;
    lbl: TLabel;
    bt: TButton;
    cb: TCheckBox;
    bevTop: TBevel;
    bevBottom: TBevel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonClick(Sender: TObject);
    procedure LabelClick(Sender: TObject);
    procedure LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;

implementation

{$I IdeConsts.inc}


type
  TVersionCheckerThread = class (TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

var
  DACVersionArr: array of TDACVersion;
  DoNotShowDACList: TStringList;

procedure LoadDoNotShowDACList;
var
  Reg: TRegistry;
  TempList: TStringList;
  ii: integer;
begin
  DoNotShowDACList.Clear;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(DACUpdatesRegistryKey, False) then begin
      TempList := TStringList.Create;
      try
        Reg.GetValueNames(TempList);
        for ii := 0 to TempList.Count - 1 do
          DoNotShowDACList.Add(TempList[ii] + '=' + Reg.ReadString(TempList[ii]));
      finally
        TempList.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure SaveDoNotShowDACList;
var
  Reg: TRegistry;
  ii: integer;
begin
  Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(DACUpdatesRegistryKey, True) then begin
      for ii := 0 to DoNotShowDACList.Count - 1 do
        Reg.WriteString(DoNotShowDACList.Names[ii], DoNotShowDACList.Values[DoNotShowDACList.Names[ii]]);
    end;
  finally
    Reg.Free;
  end;
end;

function IsAppMethod(const IDEKey: string): boolean;
var
  Reg: TRegistry;
  str: string;
begin
  Result := False;

  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if Reg.OpenKey(IDEKey + '\Maintenance', False) then begin
      str := Reg.ReadString('ProductVersion');
      if UpperCase(str) = 'APPMETHOD' then
        Result := True;
      Reg.CloseKey;
    end;

    if Reg.OpenKey(IDEKey + '\Personalities', False) then begin
      str := Reg.ReadString('');
      if UpperCase(str) = 'APPMETHOD' then
        Result := True;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

{ TVersionCheckerThread }

procedure TVersionCheckerThread.Execute;
const
  Products:
  array [0..8, 0..1] of string = (
    ('IBDAC', 'dclibdac'),
    ('LiteDAC', 'dcllitedac'),
    ('MyDAC', 'dclmydac'),
    ('ODAC', 'dclodac'),
    ('PgDAC', 'dclpgdac'),
    ('SDAC', 'dclsdac'),
    ('UniDAC', 'dclunidac'),
    ('VirtualDAC', 'dclvquery'),
    ('VirtualDAC', 'dclvtable')
  );

  procedure GetDACUpdateList(const KnownPackagesKey: string);
  var
    Reg: TRegistry;
    PackageList: TStringList;
    BplName, PackagePath, EnvVarName, LatestVersion: string;
    i, j, k, DACVersionsNumber: integer;
    FileVersion: TFileVersion;
    DACInfoArr: array of TDACInfo;
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      PackageList := TStringList.Create;
      try
        SetLength(DACInfoArr, 0);
        Reg.RootKey := HKEY_CURRENT_USER;
        if Reg.OpenKey(IDEInfos[IDEVer].RegistryKey + KnownPackagesKey, False) then begin
          Reg.GetValueNames(PackageList);
          SetLength(DACInfoArr, PackageList.Count);
          k := 0;
          for i := 0 to PackageList.Count - 1 do
            for j := Low(Products) to High(Products) do begin
              BplName := Products[j][1] + IDEInfos[IDEVer].PackageSuffix + '.bpl';
              PackagePath := PackageList[i];
              if SameText(ExtractFileName(PackagePath), BplName) then begin
                if Pos('$(', PackagePath) = 1 then begin
                  EnvVarName := Copy(PackagePath, 3, Pos(')\', PackagePath) - 3);
                  PackagePath := StringReplace(PackagePath, '$(' + EnvVarName + ')', GetEnvironmentVariable(EnvVarName), []);
                end;
                if FileExists(PackagePath) then begin
                  FileVersion := TVersionChecker.GetFileVersion(PackagePath);
                  if FileVersion.Major <> -1 then begin
                    DACInfoArr[k].Name := Products[j][0];
                    DACInfoArr[k].Version := IntToStr(FileVersion.Major) + '.' + IntToStr(FileVersion.Minor) + '.' + IntToStr(FileVersion.Release);
                    DACInfoArr[k].Edition := FileVersion.Edition;
                    inc(k);
                  end;
                end;
                Break;
              end;
            end;
          SetLength(DACInfoArr, k);
          if k > 0 then begin
            DACVersionsNumber := 0;
            SetLength(DACVersionArr, High(DACInfoArr) + 2);
            for i := 0 to High(DACInfoArr) do begin
              LatestVersion := TVersionChecker.GetLatestVersion(DACInfoArr[i].Name, DACInfoArr[i].Edition, DACInfoArr[i].Version);
              if (DoNotShowDACList.Count > 0) and (DoNotShowDACList.Values[DACInfoArr[i].Name] <> '') and
                 (TVersionChecker.VersionToInt(LatestVersion) <= TVersionChecker.VersionToInt(DoNotShowDACList.Values[DACInfoArr[i].Name])) then
                Continue;
              if TVersionChecker.VersionToInt(LatestVersion) > TVersionChecker.VersionToInt(DACInfoArr[i].Version) then begin
                if DACVersionsNumber = 0 then begin
                  DACVersionArr[DACVersionsNumber].Name := '';
                  DACVersionArr[DACVersionsNumber].CurrentVersion := 'Current version';
                  DACVersionArr[DACVersionsNumber].NewVersion := 'New version';
                  DACVersionArr[DACVersionsNumber].Edition := 'Edition';
                  Inc(DACVersionsNumber);
                end;
                DACVersionArr[DACVersionsNumber].Name := DACInfoArr[i].Name;
                DACVersionArr[DACVersionsNumber].CurrentVersion := DACInfoArr[i].Version;
                DACVersionArr[DACVersionsNumber].NewVersion := LatestVersion;
                DACVersionArr[DACVersionsNumber].Edition := DACInfoArr[i].Edition;
                Inc(DACVersionsNumber);
              end;
            end;
            SetLength(DACVersionArr, DACVersionsNumber);
          end;
        end;
      finally
        PackageList.Free;
      end;
    finally
      Reg.Free;
    end;
  end;

begin
  LoadDoNotShowDACList;

  if IsAppMethod(IDEInfos[IDEVer].RegistryKey) then
    GetDACUpdateList('\Known Appmethod Packages')
  else
    GetDACUpdateList('\Known Packages');

  if (Length(DACVersionArr) > 0) then
    Synchronize(TVersionChecker.ShowDACUpdates);
end;

constructor TVersionCheckerThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;
  Priority := tpLower;
  Resume;
end;

{ TCustomVersionChecker }

class procedure TCustomVersionChecker.ShowDACUpdates;
const
  BorderSize = 12;
  DACPanelSize = 20;
var
  frm: TCheckUpdatesForm;
  pDACInfo: array of TPanel;
  lblDACNames, lblDACEditions, lblDACCurVer, lblDACNewVer: array of TLabel;
  i, DACVersionsNumber: integer;
begin
  DACVersionsNumber := Length(DACVersionArr);
  if DACVersionsNumber <= 0 then Exit;
  frm := nil;
  try
    frm := TCheckUpdatesForm.CreateNew(nil);
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsSingle;
    frm.BorderIcons := [biSystemMenu];
    frm.FormStyle := fsStayOnTop;
    frm.Caption := 'Check Updates';
    frm.Width := 370;
    frm.OnClose := frm.FormClose;

    frm.pCaption := TPanel.Create(frm);
    frm.pCaption.Parent := frm;
    frm.pCaption.Width := frm.ClientWidth;
    frm.pCaption.Top := 0;
    frm.pCaption.Left := 0;
    frm.pCaption.Caption := '';
    frm.pCaption.BevelInner := bvNone;
    frm.pCaption.BevelOuter := bvNone;
    frm.pCaption.OnMouseMove := frm.PanelMouseMove;
    frm.img := TImage.Create(frm.pCaption);
    frm.img.Parent := frm.pCaption;
    frm.img.AutoSize := True;
    frm.img.Transparent := True;
    frm.img.Picture.Bitmap.LoadFromResourceName(HInstance, 'CHECKUPDATES');
    frm.img.Left := BorderSize;
    frm.img.Top := BorderSize;
    frm.pCaption.Height := frm.img.Height + BorderSize * 2;
    frm.lbl := TLabel.Create(frm.pCaption);
    frm.lbl.Parent := frm.pCaption;
    frm.lbl.AutoSize := False;
    frm.lbl.Width := frm.pCaption.Width - frm.img.Width - BorderSize * 2 - 12;
    frm.lbl.Height := frm.pCaption.Height - BorderSize * 2;
    frm.lbl.Top := BorderSize;
    frm.lbl.Left := frm.img.Width + BorderSize + 12;
    if DACVersionsNumber > 2 then
      frm.lbl.Caption := 'The new versions are available!'
    else
      frm.lbl.Caption := 'The new version is available!';
    frm.lbl.Font.Size := 14;
    frm.lbl.Alignment := taLeftJustify;
    frm.lbl.Layout := tlCenter;

    frm.pDACs := TPanel.Create(frm);
    frm.pDACs.Parent := frm;
    frm.pDACs.Width := frm.ClientWidth;
    frm.pDACs.Height := DACVersionsNumber * DACPanelSize + BorderSize * 2;
    frm.pDACs.Top := frm.pCaption.Top + frm.pCaption.Height;
    frm.pDACs.Left := 0;
    frm.pDACs.Caption := '';
    frm.pDACs.BevelInner := bvNone;
    frm.pDACs.BevelOuter := bvNone;
    frm.pDACs.OnMouseMove := frm.PanelMouseMove;
    frm.bevTop := TBevel.Create(frm.pDACs);
    frm.bevTop.Parent := frm.pDACs;
    frm.bevTop.Width := frm.pDACs.Width - BorderSize * 2;
    frm.bevTop.Height := 1;
    frm.bevTop.Top := 1;
    frm.bevTop.Left := BorderSize;
    frm.bevTop.Shape := bsTopLine;
    frm.bevBottom := TBevel.Create(frm.pDACs);
    frm.bevBottom.Parent := frm.pDACs;
    frm.bevBottom.Width := frm.pDACs.Width - BorderSize * 2;
    frm.bevBottom.Height := 1;
    frm.bevBottom.Top := frm.pDACs.Height - 1;
    frm.bevBottom.Left := BorderSize;
    frm.bevBottom.Shape := bsTopLine;

    SetLength(pDACInfo, DACVersionsNumber);
    SetLength(lblDACNames, DACVersionsNumber);
    SetLength(lblDACEditions, DACVersionsNumber);
    SetLength(lblDACCurVer, DACVersionsNumber);
    SetLength(lblDACNewVer, DACVersionsNumber);
    for i := 0 to DACVersionsNumber - 1 do begin
      pDACInfo[i] := TPanel.Create(frm.pDACs);
      pDACInfo[i].Parent := frm.pDACs;
      pDACInfo[i].Width := frm.pDACs.Width;
      pDACInfo[i].Height := DACPanelSize;
      pDACInfo[i].Left := BorderSize;
      pDACInfo[i].Top := BorderSize + i * DACPanelSize;
      pDACInfo[i].Caption := '';
      pDACInfo[i].BevelInner := bvNone;
      pDACInfo[i].BevelOuter := bvNone;
      pDACInfo[i].OnMouseMove := frm.PanelMouseMove;

      lblDACNames[i] := TLabel.Create(pDACInfo[i]);
      lblDACNames[i].Parent := pDACInfo[i];
      lblDACNames[i].Caption := DACVersionArr[i].Name;
      lblDACNames[i].Font.Style := [fsBold];
      lblDACNames[i].Left := BorderSize;
      lblDACNames[i].Alignment := taCenter;
      lblDACNames[i].Layout := tlCenter;

      lblDACEditions[i] := TLabel.Create(pDACInfo[i]);
      lblDACEditions[i].Parent := pDACInfo[i];
      lblDACEditions[i].Caption := DACVersionArr[i].Edition;
      if i = 0 then
        lblDACEditions[i].Left := 90
      else
        lblDACEditions[i].Left := 90 + lblDACEditions[0].Width div 2 - lblDACEditions[i].Width div 2;
      lblDACEditions[i].Alignment := taCenter;
      lblDACEditions[i].Layout := tlCenter;

      lblDACCurVer[i] := TLabel.Create(pDACInfo[i]);
      lblDACCurVer[i].Parent := pDACInfo[i];
      lblDACCurVer[i].Caption := DACVersionArr[i].CurrentVersion;
      if i = 0 then
        lblDACCurVer[i].Left := 150
      else
        lblDACCurVer[i].Left := 150 + lblDACCurVer[0].Width div 2 - lblDACCurVer[i].Width div 2;
      lblDACCurVer[i].Alignment := taCenter;
      lblDACCurVer[i].Layout := tlCenter;

      lblDACNewVer[i] := TLabel.Create(pDACInfo[i]);
      lblDACNewVer[i].Parent := pDACInfo[i];
      lblDACNewVer[i].Caption := DACVersionArr[i].NewVersion;
      if i = 0 then
        lblDACNewVer[i].Left := 250
      else begin
        lblDACNewVer[i].Left := 250 + lblDACNewVer[0].Width div 2 - lblDACNewVer[i].Width div 2;
        lblDACNewVer[i].Font.Color := clBlue;
        lblDACNewVer[i].Font.Style := [fsUnderline];
        lblDACNewVer[i].Tag := i;
        lblDACNewVer[i].OnClick := frm.LabelClick;
        lblDACNewVer[i].OnMouseMove := frm.LabelMouseMove;
      end;
      lblDACNewVer[i].Alignment := taCenter;
      lblDACNewVer[i].Layout := tlCenter;
    end;

    frm.pControls := TPanel.Create(frm);
    frm.pControls.Parent := frm;
    frm.pControls.Width := frm.ClientWidth;
    frm.pControls.Height := 30 + BorderSize * 2;
    frm.pControls.Top := frm.pDACs.Top + frm.pDACs.Height;
    frm.pControls.Left := 0;
    frm.pControls.Caption := '';
    frm.pControls.BevelInner := bvNone;
    frm.pControls.BevelOuter := bvNone;
    frm.pControls.OnMouseMove := frm.PanelMouseMove;
    frm.cb := TCheckBox.Create(frm.pControls);
    frm.cb.Parent := frm.pControls;
    frm.cb.Width := 120;
    frm.cb.Caption := 'Don''t show again';
    frm.cb.Left := BorderSize;
    frm.cb.Top := frm.pControls.Height div 2 - frm.cb.Height div 2;
    frm.bt := TButton.Create(frm.pControls);
    frm.bt.Parent := frm.pControls;
    frm.bt.ModalResult := mrOk;
    frm.bt.Caption := 'OK';
    frm.bt.Height := 30;
    frm.bt.Width := 100;
    frm.bt.Left := frm.pControls.Width - frm.bt.Width - BorderSize;
    frm.bt.Top := frm.pControls.Height div 2 - frm.bt.Height div 2;
    frm.bt.OnClick := frm.ButtonClick;
    frm.ClientHeight := frm.pCaption.Height + frm.pDACs.Height + frm.pControls.Height;
    frm.ActiveControl := frm.bt;
    frm.Show;
  except
    if Assigned(frm) then
      FreeAndNil(frm);
  end;
end;


class function TCustomVersionChecker.NeedCheck: boolean;
const
  VersionCheckKey = 'Software\Devart';
var
  Registry: TRegistry;
begin
  Result := True;

  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    try
      if Registry.OpenKeyReadOnly(VersionCheckKey) and 
         Registry.ValueExists('CheckVersion') 
      then
        Result := Registry.ReadBool('CheckVersion');
    except
      // if any error then left Result value is True
    end;
  finally
    Registry.Free;
  end;
end;

class function TCustomVersionChecker.GetUrlContent(const Url: string): string;
const
  BLOCK_SIZE = 512;
var
  InetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: TBytes;
  BytesRead: DWORD;
  i: integer;
begin
  Result := '';
  InetHandle := InternetOpen(PChar(Url), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(InetHandle) then
    RaiseLastOSError;
  try
    UrlHandle := InternetOpenUrl(InetHandle, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);
    if not Assigned(UrlHandle) then
      RaiseLastOSError;
    try
      SetLength(Buffer, BLOCK_SIZE);
      repeat
        InternetReadFile(UrlHandle, Buffer, BLOCK_SIZE, BytesRead);
        for i := 0 to BytesRead - 1 do
          Result := Result + Chr(Buffer[i]);
      until BytesRead = 0;
    finally
      InternetCloseHandle(UrlHandle);
    end;
  finally
    InternetCloseHandle(InetHandle);
  end;
end;

class function TCustomVersionChecker.ExtractTextVersion(const ResultString: string): string;
var
  StrList: TStringList;
  ResultCode: TResultCode;
begin
  StrList := TStringList.Create;
  try
    ExtractStrings([';'], [], PChar(ResultString), StrList);
    ResultCode := rcInvalidUrl;
    try
      ResultCode := TResultCode(StrToInt(Copy(StrList[0], 3, Length(StrList[0]) - 2)));
    except
    end;
    if ResultCode <> rcSuccess then
      raise Exception.Create(ResultErrorMessages[ResultCode]);
    Result := Trim(Copy(StrList[1], 3, Length(StrList[1]) - 2));
  finally
    StrList.Free;
  end;
end;

class function TCustomVersionChecker.ExtractJsonVersion(const ResultString: string): string;

  function ExtracValue(const ResultString, Name: string): string;
  var
    i, j: integer;
  begin
    Result := '';

    i := Pos(Name, ResultString);
    if i >= 1 then
      i := i + Length(Name)
    else
      Exit;

    while i <= Length(ResultString) do
      if ResultString[i] = ':' then
        Break
      else
        Inc(i);

    if i < Length(ResultString) then
      Inc(i)
    else
      Exit;

    j := i;
    while j <= Length(ResultString) do
      if (ResultString[j] = ',') or (ResultString[j] = '}') then
        Break
      else
        Inc(j);

    Result := Trim(Copy(ResultString, i, j - i));

    if Result = '-1' then
      Result := '';
  end;

var
  v1: string;
  v2: string;
  v3: string;
  v4: string;
begin
  v1 := ExtracValue(ResultString, '"major"');
  v2 := ExtracValue(ResultString, '"minor"');
  v3 := ExtracValue(ResultString, '"build"');
  v4 := ExtracValue(ResultString, '"revision"');

  Result := '';
  if v1 <> '' then begin
    Result := v1;
    if v1 <> '' then begin
      Result := Result + '.' + v2;
      if v3 <> '' then begin
        Result := Result + '.' + v3;
        if v4 <> '' then
          Result := Result + '.' + v4;
      end;
    end;
  end;
end;

class function TCustomVersionChecker.VersionToInt(Version: string): Integer;
var
  i, j, k: integer;
  str_arr: TStringArray;
begin
  Result := 0;
  str_arr := SplitString(Version, '.');
  for i := 0 to 3 do begin
    if i < Length(str_arr) then
      TryStrToInt(str_arr[i], k)
    else
      k := 0;
    for j := 0 to 2 - i do
      k := k * 100;
    Result := Result + k;
  end;
end;

class function TCustomVersionChecker.GetFileVersion(FileName: string): TFileVersion;
var
  VerBuf: Pointer;
  InfoSize, Wnd, VerSize, EditionSize: DWORD;
  FI: PVSFixedFileInfo;
  EditionBuf: Pointer;
begin
  Result.Major := -1;
  Result.Edition := '';
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize > 0 then begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then begin
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then begin
          Result.Major :=   FI.dwFileVersionMS shr 16;
          Result.Minor :=   FI.dwFileVersionMS and $FFFF;
          Result.Release := FI.dwFileVersionLS shr 16;
          Result.Build :=   FI.dwFileVersionLS and $FFFF;
        end;
        if VerQueryValue(VerBuf, PChar('\VarFileInfo\Translation'), Pointer(FI), VerSize) then begin
          if VerQueryValue(VerBuf, PChar('\StringFileInfo\' + IntToHex(MakeLong(HiWord(FI.dwSignature), LoWord(FI.dwSignature)), 8) + '\ProductEdition'), EditionBuf, EditionSize) then
            Result.Edition := StrPas(PChar(EditionBuf));
        end;
      end;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

class function TCustomVersionChecker.GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string;
begin
  Assert(False, 'Must be overrided');
end;

{ TVersionChecker }

class function TVersionChecker.GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string;
var
  Url: string;
  ResultString: string;
begin
  Url := 'http://devart.com/FileUpdateHandler.axd' +
         '?op=0' +
         '&product=' + ProductName +
         '&edition=' + ProductEdition +
         '&userversion=' + ProductVersion +
         '&cdata=' + IDEInfos[IDEVer].Name;
  ResultString := GetUrlContent(Url);
  Result := ExtractTextVersion(ResultString);
end;

{ TVersionCheckerGet }

class function TVersionCheckerGet.GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string;
var
  Url: string;
  ResultString: string;
begin
  Url := 'http://staginglicensing.devart.com/api/v1/checkupdate' +
         '?product=' + ProductName +
         '&edition=' + ProductEdition +
         '&userversion=' + ProductVersion +
         '&cdata=' + IDEInfos[IDEVer].Name +
         '&ide=' + IDEInfos[IDEVer].Name;
  ResultString := GetUrlContent(Url);
  Result := ExtractJsonVersion(ResultString);
end;

{ TVersionCheckerPost }

class function TVersionCheckerPost.GetLatestVersion(const ProductName, ProductEdition, ProductVersion: string): string;
var
  Request: TCRHttpWebRequest;
  Response: TCRHttpWebResponse;
  RequestStr: string;
  RequestData: TBytes;
  ResponseStr: string;
  ResponseData: TBytes;
begin
  RequestStr :=
    '{' +
    '  "product": "' + ProductName + '",' +
    '  "edition": "' + ProductEdition + '",' +
    '  "userVersion": "' + ProductVersion + '",' +
    '  "ide": "' + IDEInfos[IDEVer].Name + '"' +
    '}';
  RequestData := Encoding.UTF8.GetBytes(RequestStr);

  Request := TCRHttpWebRequest.Create('http://staginglicensing.devart.com/api/v1/checkupdate'); { TODO : Update URL }
  try
    Request.Method := rmPOST;
    Request.ContentType := 'application/json';
    Request.WriteBuffer(RequestData);
    Request.ContentLength := Length(RequestData);
    Response := Request.GetResponse;
    try
      ResponseData := Response.ReadAsBytes;
      ResponseStr := Encoding.UTF8.GetString(ResponseData);
      Result := ExtractJsonVersion(ResponseStr);
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;


{ TCheckUpdatesForm }

procedure TCheckUpdatesForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  if cb.Checked then begin
    DoNotShowDACList.Clear;
    for i := Low(DACVersionArr) + 1 to High(DACVersionArr) do
      DoNotShowDACList.Add(DACVersionArr[i].Name + '=' + DACVersionArr[i].NewVersion);
    SaveDoNotShowDACList;
  end;
  Action := caFree;
end;

procedure TCheckUpdatesForm.ButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCheckUpdatesForm.LabelClick(Sender: TObject);
var
  UpdatesUrl: string;
begin
  if SameText(DACVersionArr[Integer(TLabel(Sender).Tag)].Edition, 'TRIAL') then
    UpdatesUrl := Format('https://www.devart.com/%s/download.html', [LowerCase(DACVersionArr[Integer(TLabel(Sender).Tag)].Name)])
  else
    UpdatesUrl := 'https://secure.devart.com';
  OpenUrl(UpdatesUrl);
end;

procedure TCheckUpdatesForm.LabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.Color := clOlive;
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TCheckUpdatesForm.PanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: integer;

  procedure ProcessComponent(Component: TComponent);
  var
    j: integer;
  begin
    if (Component is TLabel) and (TLabel(Component).Tag > 0) then begin
      TLabel(Component).Font.Color := clBlue;
      TLabel(Component).Cursor := crDefault;
    end;
    for j := 0 to Component.ComponentCount - 1 do
      ProcessComponent(Component.Components[j]);
  end;
begin
  for i := 0 to Self.ComponentCount - 1 do
    ProcessComponent(Self.Components[i]);
end;

initialization
  DoNotShowDACList := TStringList.Create;
  if TVersionChecker.NeedCheck then
    TVersionCheckerThread.Create;

finalization
  DoNotShowDACList.Free;


end.
