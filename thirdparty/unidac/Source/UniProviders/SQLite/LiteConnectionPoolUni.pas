
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I LiteDac.inc}
unit LiteConnectionPoolUni;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SyncObjs, Variants,
  CRTypes, CRAccess, CRConnectionPool, CREncryption,
{$IFNDEF UNIDACPRO}
  LiteConsts, LiteClasses, LiteCall;
{$ELSE}
  LiteConstsUni, LiteClassesUni, LiteCallUni;
{$ENDIF}

type
  TLiteConnectionParameters = class(TCRConnectionParameters)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ConnectParamsToString: string; override;

  public
    Database: string;
    ClientLibrary: string;
    UseUnicode: boolean;
    Direct: boolean;
    ASCIIDataBase: boolean;
    EncryptionKey: string;
    DateFormat: string;
    TimeFormat: string;
    NativeDate: boolean;
    EnableSharedCache: boolean;
    EnableLoadExtension: boolean;
    BusyTimeout: integer;
    ReadUncommitted: boolean;
    DefaultCollations: boolean;
    ForeignKeys : boolean;
    ForceCreateDatabase: boolean;
    EncryptionAlgorithm: TLiteEncryptionAlgorithm;
    CipherLicense: string;
    ConnectMode: TConnectMode;
    LockingMode: TLockingMode;
    Synchronous: TSynchronous;
    JournalMode: TJournalMode;

    function Equals(Obj: TCRConnectionParameters): boolean; override;
    function SetProp(Prop: integer; const Value: variant): boolean; override;
  end;

  TLiteLocalConnectionPool = class(TCRLocalConnectionPool)
  protected
    class function GetConnectorClass: TCRConnectionClass; override;
    procedure InitConnectorParams(Connector: TCRConnection); override;
  end;

  TLiteConnectionPoolManager = class(TCRConnectionPoolManager)
  protected
    class function GetPoolManagerIndex: Integer; override;
    class procedure SetPoolManagerIndex(Value: Integer); override;
    function CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool; override;
  end;

implementation

uses
  MemData, CRProps,
{$IFNDEF UNIDACPRO}
  LiteProps;
{$ELSE}
  LitePropsUni;
{$ENDIF}

var
  PoolManagerIndex: Integer;

{ TLiteConnectionParameters }

procedure TLiteConnectionParameters.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TLiteConnectionParameters then begin
    TLiteConnectionParameters(Dest).Database := Database;
    TLiteConnectionParameters(Dest).ClientLibrary := ClientLibrary;
{$IFNDEF NOSTATIC}
    TLiteConnectionParameters(Dest).Direct := Direct;
{$ELSE}
    TLiteConnectionParameters(Dest).Direct := False;
{$ENDIF}    
    TLiteConnectionParameters(Dest).UseUnicode := UseUnicode;
    TLiteConnectionParameters(Dest).ASCIIDataBase := ASCIIDataBase;
    TLiteConnectionParameters(Dest).EncryptionKey := EncryptionKey;
    TLiteConnectionParameters(Dest).DateFormat := DateFormat;
    TLiteConnectionParameters(Dest).TimeFormat := TimeFormat;
    TLiteConnectionParameters(Dest).NativeDate := NativeDate;
    TLiteConnectionParameters(Dest).EnableSharedCache := EnableSharedCache;
    TLiteConnectionParameters(Dest).EnableLoadExtension := EnableLoadExtension;
    TLiteConnectionParameters(Dest).BusyTimeout := BusyTimeout;
    TLiteConnectionParameters(Dest).ReadUncommitted := ReadUncommitted;
    TLiteConnectionParameters(Dest).DefaultCollations := DefaultCollations;
    TLiteConnectionParameters(Dest).ForeignKeys := ForeignKeys;
    TLiteConnectionParameters(Dest).ForceCreateDatabase := ForceCreateDatabase;
    TLiteConnectionParameters(Dest).EncryptionAlgorithm := EncryptionAlgorithm;
    TLiteConnectionParameters(Dest).CipherLicense := CipherLicense;
    TLiteConnectionParameters(Dest).ConnectMode := ConnectMode;
    TLiteConnectionParameters(Dest).LockingMode := LockingMode;
    TLiteConnectionParameters(Dest).Synchronous := Synchronous;
    TLiteConnectionParameters(Dest).JournalMode := JournalMode;
  end;
end;

function TLiteConnectionParameters.ConnectParamsToString: string;
begin
  Result := inherited ConnectParamsToString;
  { TODO -oZEuS : not implemented yet }
end;

function TLiteConnectionParameters.Equals(
  Obj: TCRConnectionParameters): boolean;
var
  LiteObj: TLiteConnectionParameters;
begin
  Result := inherited Equals(Obj);
  if Result and (Obj is TLiteConnectionParameters) then begin
    LiteObj := TLiteConnectionParameters(Obj);
    Result :=
      SameText(Database, LiteObj.Database) and
      SameText(ClientLibrary, LiteObj.ClientLibrary) and
      (UseUnicode = LiteObj.UseUnicode) and
{$IFNDEF NOSTATIC}
      (Direct = LiteObj.Direct) and
{$ENDIF}      
      (EncryptionAlgorithm = LiteObj.EncryptionAlgorithm) and
      (ASCIIDataBase = LiteObj.ASCIIDataBase) and
      SameText(EncryptionKey, LiteObj.EncryptionKey) and
      SameText(DateFormat, LiteObj.DateFormat) and
      SameText(TimeFormat, LiteObj.TimeFormat) and
      (NativeDate = LiteObj.NativeDate) and
      (EnableSharedCache = LiteObj.EnableSharedCache) and
      (EnableLoadExtension = LiteObj.EnableLoadExtension) and
      (BusyTimeout = LiteObj.BusyTimeout) and
      (ReadUncommitted = LiteObj.ReadUncommitted) and
      (DefaultCollations = LiteObj.DefaultCollations) and
      (ForeignKeys = LiteObj.ForeignKeys) and
      (ForceCreateDatabase = LiteObj.ForceCreateDatabase) and
      (CipherLicense = LiteObj.CipherLicense) and
      (ConnectMode = LiteObj.ConnectMode) and
      (LockingMode = LiteObj.LockingMode) and
      (Synchronous = LiteObj.Synchronous) and
      (JournalMode = LiteObj.JournalMode);
  end;
end;

function TLiteConnectionParameters.SetProp(Prop: integer;
  const Value: variant): boolean;
begin
  Result := True;
  case Prop of
    prDatabase:
      Database := Value;
    prClientLibrary:
      ClientLibrary := Value;
    prUseUnicode:
      UseUnicode := Value;
    prASCIIDataBase:
      ASCIIDataBase := Value;
    prEncryptionKey:
      EncryptionKey := Value;
    prEnableSharedCache:
      EnableSharedCache:= Value;
    prEnableLoadExtension:
      EnableLoadExtension := Value;
    prBusyTimeout:
      BusyTimeout := Value;
    prReadUncommitted:
      ReadUncommitted := Value;
    prDefaultCollations:
      DefaultCollations := Value;
    prDateFormat:
      DateFormat := Value;
    prTimeFormat:
      TimeFormat := Value;
    prNativeDate:
      NativeDate := Value;
    prForeignKeys:
      ForeignKeys := Value;
    prStaticLibrary:
      Direct := Value;
    prForceCreateDatabase:
      ForceCreateDatabase := Value;
    prEncryptionAlgorithm:
{$IFDEF CODEC}
      EncryptionAlgorithm := Value;
{$ELSE}
      EncryptionAlgorithm := DefaultEncryptionAlgorithm;
{$ENDIF}
    prCipherLicense:
      CipherLicense := Value;
    prConnectMode:
      ConnectMode := Value;
    prLockingMode:
      LockingMode := Value;
    prSynchronous:
      Synchronous := Value;
    prJournalMode:
      JournalMode := Value;
  else
    Result := inherited SetProp(Prop, Value);
  end;
end;

{ TLiteLocalConnectionPool }

class function TLiteLocalConnectionPool.GetConnectorClass: TCRConnectionClass;
begin
  Result := TSQLiteConnection;
end;

procedure TLiteLocalConnectionPool.InitConnectorParams(Connector: TCRConnection);
begin
  Connector.SetProp(prDatabase, TLiteConnectionParameters(ConnectionParameters).Database);
  Connector.SetProp(prClientLibrary, TLiteConnectionParameters(ConnectionParameters).ClientLibrary);
{$IFNDEF NOSTATIC}
  Connector.SetProp(prStaticLibrary, TLiteConnectionParameters(ConnectionParameters).Direct);
{$ELSE}
  Connector.SetProp(prStaticLibrary, False);
{$ENDIF}
  Connector.SetProp(prUseUnicode, TLiteConnectionParameters(ConnectionParameters).UseUnicode);
  Connector.SetProp(prASCIIDataBase, TLiteConnectionParameters(ConnectionParameters).ASCIIDataBase);
  Connector.SetProp(prEncryptionKey, TLiteConnectionParameters(ConnectionParameters).EncryptionKey);
  Connector.SetProp(prDateFormat, TLiteConnectionParameters(ConnectionParameters).DateFormat);
  Connector.SetProp(prTimeFormat, TLiteConnectionParameters(ConnectionParameters).TimeFormat);
  Connector.SetProp(prNativeDate, TLiteConnectionParameters(ConnectionParameters).NativeDate);
  Connector.SetProp(prEnableSharedCache, TLiteConnectionParameters(ConnectionParameters).EnableSharedCache);
  Connector.SetProp(prEnableLoadExtension, TLiteConnectionParameters(ConnectionParameters).EnableLoadExtension);
  Connector.SetProp(prBusyTimeout, TLiteConnectionParameters(ConnectionParameters).BusyTimeout);
  Connector.SetProp(prReadUncommitted, TLiteConnectionParameters(ConnectionParameters).ReadUncommitted);
  Connector.SetProp(prDefaultCollations, TLiteConnectionParameters(ConnectionParameters).DefaultCollations);
  Connector.SetProp(prForeignKeys, TLiteConnectionParameters(ConnectionParameters).ForeignKeys);
  Connector.SetProp(prForceCreateDatabase, TLiteConnectionParameters(ConnectionParameters).ForceCreateDatabase);
{$IFDEF CODEC}
  Connector.SetProp(prEncryptionAlgorithm, TLiteConnectionParameters(ConnectionParameters).EncryptionAlgorithm);
{$ENDIF}
  Connector.SetProp(prCipherLicense, TLiteConnectionParameters(ConnectionParameters).CipherLicense);
  Connector.SetProp(prConnectMode, TLiteConnectionParameters(ConnectionParameters).ConnectMode);
  Connector.SetProp(prLockingMode, TLiteConnectionParameters(ConnectionParameters).LockingMode);
  Connector.SetProp(prSynchronous, TLiteConnectionParameters(ConnectionParameters).Synchronous);
  Connector.SetProp(prJournalMode, TLiteConnectionParameters(ConnectionParameters).JournalMode);
end;

{ TLiteConnectionPoolManager }

class function TLiteConnectionPoolManager.GetPoolManagerIndex: Integer;
begin
  Result := PoolManagerIndex;
end;

class procedure TLiteConnectionPoolManager.SetPoolManagerIndex(Value: Integer);
begin
  PoolManagerIndex := Value;
end;

function TLiteConnectionPoolManager.CreateConnectionPool(ConnectionParameters: TCRConnectionParameters): TCRConnectionPool;
begin
  Result := TLiteLocalConnectionPool.Create(Self, ConnectionParameters);
end;

initialization
  PoolManagerIndex := -1;

end.
