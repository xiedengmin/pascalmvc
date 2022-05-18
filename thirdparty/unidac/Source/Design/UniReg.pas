{$IFNDEF CLR}

{$I UniDac.inc}

unit UniReg;
{$ENDIF}
interface

uses
  SysUtils, Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Dialogs;

{$IFNDEF FPC}
type
  TUniProvidersLoader = class (TList)
  public
    destructor Destroy; override;

    procedure LoadProviders;
    procedure FreeProviders;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFNDEF FPC}
{$IFNDEF CLR}
  {$IFDEF VER9}
    {$R UniDesign9.res}
  {$ELSE}
    {$R UniDesign.res}
  {$ENDIF}
  {$IFDEF VER10P}
    {$R UniDesign10p.res}
  {$ENDIF}
{$ENDIF}
{$ENDIF}

uses
{$IFDEF CLR}
  System.Reflection,
{$ENDIF}
  DB, DBAccess, MemUtils,
  UniDacVcl, UniProvider, Uni, UniScript, UniSQLMonitor,
{$IFNDEF STD}
  UniDump, UniLoader, UniAlerter,
{$ENDIF}
  DacReg, CRTypes;

{$IFNDEF FPC}
var
  ProvidersLoader: TUniProvidersLoader;

destructor TUniProvidersLoader.Destroy;
begin
  FreeProviders;

  inherited;
end;

procedure TUniProvidersLoader.LoadProviders;
{$I IdeConsts.inc}
var
  i: integer;
  List: TList;
{$IFDEF CLR}
  AssemblyInfo: string;
{$ENDIF}
  function LowerCaseEx(Value: string): string;
  begin
    Result := UpperCase(Copy(Value, 1, 1)) + LowerCase(Copy(Value, 2, Length(Value))); 
  end;

  procedure LoadProvider(PackageName, SiblingPackageName, ProviderUnitName: string);
  var
  {$IFNDEF CLR}
    ProviderHandle: HMODULE;
    RegisterProc: TProcedure;
  {$ELSE}
    AAssembly: Assembly;
    AType: System.Type;
  {$ENDIF}
  begin
  {$IFDEF CLR}
    try
      AAssembly := Assembly.Load(PackageName);
      AType := AAssembly.GetType(ProviderUnitName);
      AType.InvokeMember('RegisterComponent',
        BindingFlags.InvokeMethod or BindingFlags.Public or BindingFlags.Static, nil, nil, nil);
    except
    end;
  {$ELSE}
  {$IFDEF EXPRESS}
    try
      ProviderHandle := LoadPackage(SiblingPackageName);
      UnLoadPackage(ProviderHandle);
    except
      ProviderHandle := 0;
    end;
    if ProviderHandle = 0 then
      Exit; 
  {$ENDIF}
    try
      ProviderHandle := LoadPackage(PackageName);
    except
      ProviderHandle := 0;
    end;
    if ProviderHandle <> 0 then begin
      RegisterProc := GetProcAddress(ProviderHandle, PChar('@' + LowerCaseEx(ProviderUnitName) + '@' + 'RegisterComponent$qqrv'));
      if Assigned(RegisterProc) then begin
        RegisterProc;
        Add(Pointer(ProviderHandle));
      end
      else // old version of odac.bpl, ...
        UnloadPackage(ProviderHandle);
    end;
  {$ENDIF}
  end;

begin
  FreeProviders;

{$IFDEF CLR}
  AssemblyInfo := Assembly.GetExecutingAssembly.GetName.ToString;
  i := Pos(',', AssemblyInfo);
  if i > 0 then
    AssemblyInfo := AssemblyInfo.Substring(i - 1);
{$ENDIF}

  List := UniProviders.LockList;
  try
    for i := 0 to List.Count - 1 do
      with TUniProviderDesc(List[i]) do
      {$IFNDEF CLR}
        LoadProvider(PackageName + IDEInfos[IDEVer].PackageSuffix + '.bpl',
          LowerCase(SiblingProduct) + IDEInfos[IDEVer].PackageSuffix + '.bpl',   
          ProviderUnitName);
      {$ELSE}
        LoadProvider(AssemblyName + AssemblyInfo, '', AssemblyName + '.Units.' + ProviderUnitName);
      {$ENDIF}
  finally
    UniProviders.UnlockList;
  end;
end;

procedure TUniProvidersLoader.FreeProviders;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do begin
  {$IFDEF CLR}
  {$ELSE}
    UnloadPackage(Cardinal(Items[i]));
  {$ENDIF}
    Delete(i);
  end;
end;
{$ENDIF}

procedure Register;
begin
{$IFNDEF FPC}
{$IFNDEF CLR}
{$IFDEF VER9P}
  RegisterSplashScreen('Devart Universal Data Access Components',
                       UniDacVersion,
                       LoadBitmap(HInstance, {$IFDEF VER9}'SPLASHGR'{$ENDIF}
                                             {$IFDEF VER10}'SPLASHBL'{$ENDIF}
                                             {$IFDEF VER11}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER12}'SPLASHWH'{$ENDIF}
                                             {$IFDEF VER14P}'SPLASHBL'{$ENDIF}),
                     {$IFDEF BETA}
                       true, 'Beta'
                     {$ELSE}
                     {$IFDEF RC}
                       false, 'Release Candidate'
                     {$ELSE}
                       false, 'Licensed'
                     {$ENDIF}
                     {$ENDIF}
                      );
  RegisterAboutBox('Devart Universal Data Access Components',
                   UniDacVersion,
                   'http://www.devart.com/unidac/',
                   'Devart Universal Data Access Components' + #13#10 +
                   'Copyright 1998-2021 Devart. All rights reserved.' + #13#10 +
                   'Web: www.devart.com/unidac/' + #13#10 +
                   'Support: www.devart.com/unidac/support.html',
                   LoadBitmap(HInstance, 'ABOUT'),
                 {$IFDEF BETA}
                   true, 'Beta'
                 {$ELSE}
                 {$IFDEF RC}
                   false, 'Release Candidate'
                 {$ELSE}
                   false, 'Licensed'
                 {$ENDIF}
                 {$ENDIF}
                   ,
                 {$IFDEF EXPRESS}
                   'Express edition'
                 {$ELSE}
                 {$IFDEF STD}
                   'Standard edition'
                 {$ELSE}
                   'Professional edition'
                 {$ENDIF}
                 {$ENDIF}
                  );
{$ENDIF}
{$ENDIF}
{$ENDIF}

{$IFNDEF STD}
{$IFNDEF EXPRESS}
  RegisterCRBatchMove;
{$ENDIF}
{$ENDIF}

  RegisterComponents('UniDAC', [TUniConnection]);
  RegisterComponents('UniDAC', [TUniQuery]);
  RegisterComponents('UniDAC', [TUniTable]);
  RegisterComponents('UniDAC', [TUniStoredProc]);
  RegisterComponents('UniDAC', [TUniSQL]);
  RegisterComponents('UniDAC', [TUniScript]);
  RegisterComponents('UniDAC', [TUniUpdateSQL]);
  RegisterComponents('UniDAC', [TUniTransaction]);
  RegisterComponents('UniDAC', [TUniDataSource]);
{$IFNDEF STD}
  RegisterComponents('UniDAC', [TUniLoader]);
  RegisterComponents('UniDAC', [TUniDump]);
  RegisterComponents('UniDAC', [TUniAlerter]);
  RegisterComponents('UniDAC', [TUniMetaData]);
{$ENDIF}
  RegisterComponents('UniDAC', [TUniSQLMonitor]);
  RegisterComponents('UniDAC', [TUniConnectDialog]);
{$IFNDEF STD}
  RegisterComponents('UniDAC', [TUniEncryptor]);
{$ENDIF}

{$IFNDEF FPC}
  if ProvidersLoader = nil then
    ProvidersLoader := TUniProvidersLoader.Create;
  ProvidersLoader.LoadProviders;
{$ENDIF}
end;

initialization
{$IFDEF FPC}
{$I UniDesign.lrs}
{$ENDIF}

{$IFNDEF FPC}
  ProvidersLoader := nil;
{$ENDIF}

finalization
{$IFNDEF FPC}
  ProvidersLoader.Free;
{$ENDIF}
end.
