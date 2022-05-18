
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQLite ConnectionString
//////////////////////////////////////////////////

{$I LiteDac.inc}
unit LiteConnectionStringUni;

interface

uses
  SysUtils,
  CRConnectionString;

type
  TLiteConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
    function IgnoreParam(Code: Integer): boolean; override;

    function ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string; override;
    function ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant; override;
  end;

implementation

uses
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRProps,
  CRAccess,
  DAConsts,
{$IFNDEF UNIDACPRO}
  LiteProps, LiteConsts;
{$ELSE}
  LitePropsUni, LiteConstsUni;
{$ENDIF}

function AlgorithmToStr(const Algorithm: TLiteEncryptionAlgorithm): string;
begin
  Result := LiteEncryptionAlgorithm[Algorithm];
end;

function StrToAlgorithm(const AlgorithmStr: string): TLiteEncryptionAlgorithm;
var
  i: TLiteEncryptionAlgorithm;
  AlgorithmUpper: string;
begin
  AlgorithmUpper := UpperCase(AlgorithmStr);
  Result := leDefault;

  for i := Low(LiteEncryptionAlgorithm) to High(LiteEncryptionAlgorithm) do
    if UpperCase(LiteEncryptionAlgorithm[i]) = AlgorithmUpper then begin
      Result := i;
      Break;
    end;
end;

{ TLiteConnectionStringBuilder }

procedure TLiteConnectionStringBuilder.InitParams;
begin
  inherited;

{$IFNDEF NOSTATIC}
  AddParam(ppHighest, 'Direct', [], prStaticLibrary, varBoolean, DefValDirect);
{$ENDIF}
  AddParam(ppNormal, 'Database', ['Data Source'], prDatabase, varString, '');
  AddParam(ppNormal, 'Encryption Key', ['EncryptionKey', 'Password', 'PWD'], prEncryptionKey, varString, '');
{$IFNDEF NOSTATIC}
  AddParam(ppNormal, 'Encryption Algorithm', ['EncryptionAlgorithm'], prEncryptionAlgorithm, varEnum, Variant(DefaultEncryptionAlgorithm), TypeInfo(TLiteEncryptionAlgorithm), 'le');
{$ENDIF}
  AddParam(ppNormal, 'Force Create Database', ['ForceCreateDatabase'], prForceCreateDatabase, varBoolean, DefValForceCreateDatabase);
  AddParam(ppNormal, 'Client Library', ['ClientLibrary'], prClientLibrary, varString, '');
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, Null); // dummy
  AddParam(ppNormal, 'Cipher License', ['CipherLicense', 'cipher_license'], prCipherLicense, varString, '');
  AddParam(ppNormal, 'Enable Shared Cache', ['EnableSharedCache'], prEnableSharedCache, varBoolean, DefValEnableSharedCache);
  AddParam(ppNormal, 'Connect Mode', ['ConnectMode'], prConnectMode, varEnum, Variant(DefValConnectMode), TypeInfo(TConnectMode), 'cm');
  AddParam(ppNormal, 'Journal Mode', ['JournalMode'], prJournalMode, varEnum, Variant(DefValJournalMode), TypeInfo(TJournalMode), 'jm');
  AddParam(ppNormal, 'Locking Mode', ['LockingMode'], prLockingMode, varEnum, Variant(DefValLockingMode), TypeInfo(TLockingMode), 'lm');
  AddParam(ppNormal, 'Synchronous', [], prSynchronous, varEnum, Variant(DefValSynchronous), TypeInfo(TSynchronous), 'sm');
end;

function TLiteConnectionStringBuilder.IgnoreParam(Code: Integer): boolean;
begin
  if Code = prUsername then
    Result := True
  else
    Result := inherited IgnoreParam(Code);
end;

function TLiteConnectionStringBuilder.ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string;
begin
  case Param.Code of
    prEncryptionAlgorithm:
      Result := AlgorithmToStr(Value);
    else
      Result := inherited ConvertVarToStr(Param, Value);
  end;
end;

function TLiteConnectionStringBuilder.ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant;
begin
  case Param.Code of
    prEncryptionAlgorithm:
      Result := StrToAlgorithm(Value);
    else
      Result := inherited ConvertStrToVar(Param, Value);
  end;
end;

end.
