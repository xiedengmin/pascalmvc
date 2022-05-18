unit CRSsoStorage;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Registry,
{$ENDIF}
  SysUtils, Classes,
  CRTypes, CRFunctions,
  ScBridge;

{$I SecureBridgeVer.inc}

type
  TCRSsoStorage = class(TScStorage)
  protected
    FPKCS12Processor: TScPKCS12Processor;

    procedure InternalLoad(Item: TScStorageItem); override;
    procedure InternalSave(Item: TScStorageItem); override;
    procedure InternalDelete(Item: TScStorageItem); override;
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); override;
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); override;
    procedure InternalDeleteStorage; override;

    procedure LoadSSOWallet; virtual; abstract;
  public
    destructor Destroy; override;
  end;

  TCRSsoFileStorage = class(TCRSsoStorage)
  private
    FPath: string;
    FFileName: string;

    procedure SetPath(const Value: string);
    procedure SetFileName(const Value: string);
    function GetFullPath: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPath(Reader: TReader);
    procedure WritePath(Writer: TWriter);
    procedure ReadFileName(Reader: TReader);
    procedure WriteFileName(Writer: TWriter);
    procedure LoadSSOWallet; override;
  public
    constructor Create(AOwner: TComponent); override;

  {$IFDEF ODBC_DRIVER}
    procedure SetFullWalletPath(const Value: string);
  {$ENDIF}
  published
    property Path: string read FPath write SetPath stored False;
    property FileName: string read FFileName write SetFileName stored False;
  end;

{$IFDEF MSWINDOWS}

  TCRSsoRegStorage = class(TCRSsoStorage)
  private
    FKeyPath: string;
    FValueName: string;
    FRegistry: TRegistry;

    procedure SetKeyPath(const Value: string);
    procedure SetValueName(const Value: string);
    procedure SetRootKey(const Value: NativeUint);
    function GetRootKey: NativeUint;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadKeyPath(Reader: TReader);
    procedure WriteKeyPath(Writer: TWriter);
    procedure ReadValueName(Reader: TReader);
    procedure WriteValueName(Writer: TWriter);
    procedure LoadSSOWallet; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  {$IFDEF ODBC_DRIVER}
    procedure SetFullWalletRegPath(const Value: string);
  {$ENDIF}
  published
  {$HPPEMIT '#ifdef KeyPath'}
  {$HPPEMIT '#undef KeyPath'}
  {$HPPEMIT '#endif'}
    property KeyPath: string read FKeyPath write SetKeyPath stored False;
    property ValueName: string read FValueName write SetValueName stored False;
    property RootKey: NativeUint read GetRootKey write SetRootKey default HKEY_CURRENT_USER;
  end;

{$ENDIF}

implementation

const
  DefaultWalletPath = '.';
  DefaultWalletFileName = 'cwallet.sso';
  DefaultWalletKeyPath = '\Software\ORACLE\WALLETS';
  DefaultWalletKeyName = 'DEFAULT';
  DefaultWalletValueName = 'cwallet.sso';

{ TCRSsoStorage }

destructor TCRSsoStorage.Destroy;
begin
  FPKCS12Processor.Free;

  inherited;
end;

procedure TCRSsoStorage.InternalLoad(Item: TScStorageItem);
begin
end;

procedure TCRSsoStorage.InternalSave(Item: TScStorageItem);
begin
end;

procedure TCRSsoStorage.InternalDelete(Item: TScStorageItem);
begin
end;

procedure TCRSsoStorage.InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean);
begin
end;

procedure TCRSsoStorage.InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings);
begin
  if FPKCS12Processor = nil then
    LoadSSOWallet;
end;

procedure TCRSsoStorage.InternalDeleteStorage;
begin
  Keys.Clear;
  Users.Clear;
  Certificates.Clear;
  CRLs.Clear;

  FreeAndNil(FPKCS12Processor);
end;

{ TCRSsoFileStorage }

constructor TCRSsoFileStorage.Create(AOwner: TComponent);
begin
  inherited;

  FPath := DefaultWalletPath;
  FFileName := DefaultWalletFileName;
end;

procedure TCRSsoFileStorage.SetPath(const Value: string);
begin
  if Value <> FPath then begin
    FPath := Trim(Value);
    Invalidate;
  end;
end;

procedure TCRSsoFileStorage.SetFileName(const Value: string);
begin
  if Value <> FFileName then begin
    FFileName := Trim(Value);
    Invalidate;
  end;
end;

function TCRSsoFileStorage.GetFullPath: string;
begin
  Result := FPath;
  if (Result <> '') and (Result[1] = '.') and  // Path = '.'; '..'; '.\'; '..\'
     (CurrentProjectOutputDir <> '')
  then
    Result := IncludeTrailingPathDelimiter(CurrentProjectOutputDir) + Result;
end;

procedure TCRSsoFileStorage.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TCRSsoFileStorage) then begin
    TCRSsoFileStorage(Dest).Path := Path;
    TCRSsoFileStorage(Dest).FileName := FileName;
  end
  else
    inherited;
end;

procedure TCRSsoFileStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Path', ReadPath, WritePath, FPath <> DefaultWalletPath);
  Filer.DefineProperty('FileName', ReadFileName, WriteFileName, FFileName <> DefaultWalletFileName);
end;

procedure TCRSsoFileStorage.ReadPath(Reader: TReader);
begin
  FPath := Reader.ReadString;
end;

procedure TCRSsoFileStorage.WritePath(Writer: TWriter);
begin
  Writer.WriteString(FPath);
end;

procedure TCRSsoFileStorage.ReadFileName(Reader: TReader);
begin
  FFileName := Reader.ReadString;
end;

procedure TCRSsoFileStorage.WriteFileName(Writer: TWriter);
begin
  Writer.WriteString(FFileName);
end;

procedure TCRSsoFileStorage.LoadSSOWallet;
var
  FullFileName: string;
  Stream: TFileStream;
  Password: string;
begin
  FullFileName := GetFullPath;
  if FullFileName <> '' then
    FullFileName := IncludeTrailingPathDelimiter(FullFileName);
  FullFileName := FullFileName + FFileName;

  Stream := TFileStream.Create(FullFileName, fmOpenRead);
  try
  {$IFDEF SB_DEMO_VER2}
    Password := TScPKCS12Processor.ImportPasswordFromSSO(Stream);
  {$ELSE}
    Password := TScPKCS12Processor.ImportFromSSO(Stream);
  {$ENDIF}
    FPKCS12Processor := TScPKCS12Processor.Create;
    try
      FPKCS12Processor.ImportFrom(Stream, Password);
      ImportFromPKCS12(FPKCS12Processor);
    except
      FreeAndNil(FPKCS12Processor);
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

{$IFDEF ODBC_DRIVER}
procedure TCRSsoFileStorage.SetFullWalletPath(const Value: string);
var
  FileName: string;
  FileExt: string;
begin
  FileName := ExtractFileName(Value);
  if FileName = '' then begin
    FPath := Value;
    FFileName := DefaultWalletFileName;
  end
  else begin
    FileExt := ExtractFileExt(Value);
    if (FileExt = '') or (FileExt = '.') then begin
      FPath := Value;
      FFileName := DefaultWalletFileName;
    end
    else begin
      FPath := ExtractFilePath(Value);
      FFileName := FileName;
    end;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

{ TCRSsoRegStorage }

constructor TCRSsoRegStorage.Create(AOwner: TComponent);
begin
  inherited;

  FRegistry := TRegistry.Create(KEY_READ);

  FKeyPath := DefaultWalletKeyPath + '\' + DefaultWalletKeyName;
  FValueName := DefaultWalletValueName;
end;

destructor TCRSsoRegStorage.Destroy;
begin
  FRegistry.Free;

  inherited;
end;

procedure TCRSsoRegStorage.SetKeyPath(const Value: string);
begin
  if Value <> FKeyPath then begin
    FKeyPath := Trim(Value);
    Invalidate;
  end;
end;

procedure TCRSsoRegStorage.SetValueName(const Value: string);
begin
  if Value <> FValueName then begin
    FValueName := Trim(Value);
    Invalidate;
  end;
end;

procedure TCRSsoRegStorage.SetRootKey(const Value: NativeUint);
begin
  if Value <> FRegistry.RootKey then begin
    FRegistry.RootKey := Value;
    Invalidate;
  end;
end;

function TCRSsoRegStorage.GetRootKey: NativeUint;
begin
  Result := FRegistry.RootKey;
end;

procedure TCRSsoRegStorage.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TCRSsoRegStorage) then begin
    TCRSsoRegStorage(Dest).RootKey := RootKey;
    TCRSsoRegStorage(Dest).ValueName := ValueName;
    TCRSsoRegStorage(Dest).KeyPath := KeyPath;
  end
  else
    inherited;
end;

procedure TCRSsoRegStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('KeyPath', ReadKeyPath, WriteKeyPath, KeyPath <> DefaultWalletKeyPath + '\' + DefaultWalletKeyName);
  Filer.DefineProperty('ValueName', ReadValueName, WriteValueName, KeyPath <> DefaultWalletValueName);
end;

procedure TCRSsoRegStorage.ReadKeyPath(Reader: TReader);
begin
  FKeyPath := Reader.ReadString;
end;

procedure TCRSsoRegStorage.WriteKeyPath(Writer: TWriter);
begin
  Writer.WriteString(FKeyPath);
end;

procedure TCRSsoRegStorage.ReadValueName(Reader: TReader);
begin
  FValueName := Reader.ReadString;
end;

procedure TCRSsoRegStorage.WriteValueName(Writer: TWriter);
begin
  Writer.WriteString(FValueName);
end;

procedure TCRSsoRegStorage.LoadSSOWallet;
var
  DataSize: Integer;
  CertificateData: TBytes;
  Stream: TMemoryStream;
  Password: string;
begin
  if FRegistry.OpenKeyReadOnly(FKeyPath) then begin
    DataSize := FRegistry.GetDataSize(ValueName);
    if DataSize > 0 then begin
      SetLength(CertificateData, DataSize);
      FRegistry.ReadBinaryData(ValueName, CertificateData[0], DataSize);
    end
    else
      SetLength(CertificateData, 0);

    Stream := TMemoryStream.Create;
    try
      Stream.Write(CertificateData, Length(CertificateData));
      Stream.Seek(0, soBeginning);

    {$IFDEF SB_DEMO_VER2}
      Password := TScPKCS12Processor.ImportPasswordFromSSO(Stream);
    {$ELSE}
      Password := TScPKCS12Processor.ImportFromSSO(Stream);
    {$ENDIF}
      FPKCS12Processor := TScPKCS12Processor.Create;
      try
        FPKCS12Processor.ImportFrom(Stream, Password);
        ImportFromPKCS12(FPKCS12Processor);
      except
        FreeAndNil(FPKCS12Processor);
        raise;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

 {$IFDEF ODBC_DRIVER}
procedure TCRSsoRegStorage.SetFullWalletRegPath(const Value: string);

  function DecodeRegPath(out KeyPath, ValueName: string): string;
  var
    i: Integer;
  begin
    i := Length(Value);
    while (i >= 1) and (Value[i] <> '\') do
      Dec(i);
    KeyPath := Copy(Value, 1, i - 1);
    ValueName := Copy(Value, i + 1, Length(Value) - i);
  end;

var
  KeyPath: string;
  ValueName: string;
begin
  DecodeRegPath(KeyPath, ValueName);

  if KeyPath = '' then begin
    FKeyPath := DefaultWalletKeyPath + '\' + ValueName;
    FValueName := DefaultWalletValueName;
  end
  else if Pos('\', KeyPath) = 0 then begin
    FKeyPath := DefaultWalletKeyPath + '\' + KeyPath;
    if ValueName <> '' then
      FValueName := ValueName
    else
      FValueName := DefaultWalletValueName;
  end
  else if FRegistry.KeyExists(KeyPath + '\' + ValueName) then begin
    FKeyPath := KeyPath + '\' + ValueName;
    FValueName := DefaultWalletValueName;
  end
  else begin
    FKeyPath := KeyPath;
    if ValueName <> '' then
      FValueName := ValueName
    else
      FValueName := DefaultWalletValueName;
  end;
end;
{$ENDIF}

{$ENDIF}

end.