
//////////////////////////////////////////////////
//  SQLite Data Access Components
//  Copyright © 2008-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I VirtualQuery.inc}
{$I LiteDac.inc}
unit LiteCollationVirtual;

interface

uses
{$IFDEF VER17P}
  System.Types,
{$ENDIF}  
  SysUtils, Classes, Variants,
  CLRClasses, CRTypes, CRAccess, MemUtils,
{$IFDEF VIRTUAL_QUERY}
  LiteCallVirtual, LiteErrorVirtual;
{$ELSE}
{$IFNDEF UNIDACPRO}
  LiteCall, LiteError;
{$ELSE}
  LiteCallUni, LiteErrorUni;
{$ENDIF}
{$ENDIF}

type
  TLiteCollation = function(const Str1, Str2: string): Integer;
  TLiteCollationMethod = function(const Str1, Str2: string): Integer of object;

  TLiteAnsiCollation = function(const Str1, Str2: AnsiString): Integer;
  TLiteAnsiCollationMethod = function(const Str1, Str2: AnsiString): Integer of object;

  TLiteWideCollation = function(const Str1, Str2: WideString): Integer;
  TLiteWideCollationMethod = function(const Str1, Str2: WideString): Integer of object;

  TCustomLiteCollationDesc = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRConnection;
    FName: string;
    FTextRepresentation: Integer;
  protected
    procedure RegisterCollation;
    procedure UnregisterCollation;

    function GetAnsiStr(StrSize: Integer; const pStr: IntPtr): AnsiString;
    function GetWideStr(StrSize: Integer; const pStr: IntPtr): WideString;

    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; virtual; abstract;
  public
    constructor Create(Connection: TCRConnection; const Name: string); overload;
    destructor Destroy; override;

    property Name: string read FName;
    property TextRepresentation: Integer read FTextRepresentation;
  end;

  TLiteCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteCollation: TLiteCollation;
    FLiteCollationMethod: TLiteCollationMethod;
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; const Name: string; LiteCollation: TLiteCollation); overload;
    constructor Create(Connection: TCRConnection; const Name: string; LiteCollation: TLiteCollationMethod); overload;

    property LiteCollation: TLiteCollation read FLiteCollation write FLiteCollation;
  end;

  TLiteAnsiCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteAnsiCollation: TLiteAnsiCollation;
    FLiteAnsiCollationMethod: TLiteAnsiCollationMethod;
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; const Name: string; LiteAnsiCollation: TLiteAnsiCollation); overload;
    constructor Create(Connection: TCRConnection; const Name: string; LiteAnsiCollation: TLiteAnsiCollationMethod); overload;

    property LiteAnsiCollation: TLiteAnsiCollation read FLiteAnsiCollation write FLiteAnsiCollation;
  end;

  TLiteWideCollationDesc = class(TCustomLiteCollationDesc)
  private
    FLiteWideCollation: TLiteWideCollation;
    FLiteWideCollationMethod: TLiteWideCollationMethod;    
  protected
    function DoCollate(StrSize1: Integer; const pStr1: IntPtr; StrSize2: Integer; const pStr2: IntPtr): Integer; override;
  public
    constructor Create(Connection: TCRConnection; const Name: string; LiteWideCollation: TLiteWideCollation); overload;
    constructor Create(Connection: TCRConnection; const Name: string; LiteWideCollation: TLiteWideCollationMethod); overload;

    property LiteWideCollation: TLiteWideCollation read FLiteWideCollation write FLiteWideCollation;
  end;

  TSQLiteCollationManager = class
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FConnection: TCRConnection;
    FCollationList: TCRObjectList;
  protected
    procedure InternalAddCollation(LiteCollationDesc: TCustomLiteCollationDesc);
    procedure InternalRemoveCollation(LiteCollationDesc: TCustomLiteCollationDesc);

    function FindCollation(Name: string): TCustomLiteCollationDesc;
  public
    constructor Create(Connection: TCRConnection);
    destructor Destroy; override;

    procedure RegisterCollation(const Name: string; LiteCollation: TLiteCollation); overload;
    procedure RegisterCollation(const Name: string; LiteCollation: TLiteCollationMethod); overload;
    procedure UnRegisterCollation(const Name: string);

    procedure RegisterAnsiCollation(const Name: string; LiteAnsiCollation: TLiteAnsiCollation); overload;
    procedure RegisterAnsiCollation(const Name: string; LiteAnsiCollation: TLiteAnsiCollationMethod); overload;
    procedure UnRegisterAnsiCollation(const Name: string);

    procedure RegisterWideCollation(const Name: string; LiteWideCollation: TLiteWideCollation); overload;
    procedure RegisterWideCollation(const Name: string; LiteWideCollation: TLiteWideCollationMethod); overload;    
    procedure UnRegisterWideCollation(const Name: string);

    procedure RegisterDefaultCollations;
    procedure UnRegisterDefaultCollations;

    procedure UnRegistrAllCollations;
  end;

implementation

uses
  CRFunctions,
{$IFDEF VIRTUAL_QUERY}
  LiteClassesVirtual;
{$ELSE}
{$IFDEF UNIDACPRO}
  LiteClassesUni;
{$ELSE}
  LiteClasses;
{$ENDIF}
{$ENDIF}

var
  CallBackLiteCollationPtr: IntPtr;

{ Function }

function CallBackLiteCollation(pUserData: IntPtr;
                               StrSize1: Integer; const pStr1: IntPtr;
                               StrSize2: Integer; const pStr2: IntPtr
                              ): Integer; cdecl;
var
  collationDesc: TCustomLiteCollationDesc;
begin
  collationDesc := TCustomLiteCollationDesc(pUserData);
  Result := collationDesc.DoCollate(StrSize1, pStr1, StrSize2, pStr2);
end;

function DefaultUnicodeNoCase(const WStr1, WStr2: WideString): Integer;
var
  upperStr1: WideString;
  upperStr2: WideString;
begin
  upperStr1 := {$IFNDEF NEXTGEN}WideUpperCase{$ELSE}UpperCase{$ENDIF}(WStr1);
  upperStr2 := {$IFNDEF NEXTGEN}WideUpperCase{$ELSE}UpperCase{$ENDIF}(WStr2);

  if upperStr1 > upperStr2 then
    Result := 1
  else if upperStr1 < upperStr2 then
    Result := -1
  else
    Result :=  0;
end;

{ TCustomLiteCollationDesc }

constructor TCustomLiteCollationDesc.Create(Connection: TCRConnection; const Name: string);
begin
  inherited Create;

  FConnection := Connection;
  FName := Name;

  if TSQLiteConnection(FConnection).IsUnicodeDataBase then
    FTextRepresentation := SQLITE_UTF16
  else
    FTextRepresentation := SQLITE_UTF8;
end;

destructor TCustomLiteCollationDesc.Destroy;
begin
  inherited;
end;

procedure TCustomLiteCollationDesc.RegisterCollation;
var
  pName: PAnsiChar;
  pSelf: IntPtr;
  sa: AnsiString;
begin
  pSelf := Self;
  sa := AnsiString(Name);
  pName := Marshal.StringToHGlobalAnsi(sa);
  try
    TSQLiteConnection(FConnection).API.sqlite3_create_collation(TSQLiteConnection(FConnection).API.SQLite, pName, FTextRepresentation, pSelf, CallBackLiteCollationPtr);
  finally
    Marshal.FreeCoTaskMem(pName);
  end;
end;

procedure TCustomLiteCollationDesc.UnregisterCollation;
var
  pName: PAnsiChar;
  sa: AnsiString;
begin
  if FConnection.GetConnected then begin
    sa := AnsiString(Name);
    pName := Marshal.StringToHGlobalAnsi(sa);
    try
      TSQLiteConnection(FConnection).API.sqlite3_create_collation(TSQLiteConnection(FConnection).API.SQLite, pName, FTextRepresentation, nil, nil);
    finally
      Marshal.FreeCoTaskMem(pName);
    end;
  end;
end;

function TCustomLiteCollationDesc.GetAnsiStr(StrSize: Integer; const pStr: IntPtr): AnsiString;
begin
  Result := Marshal.PtrToStringAnsi(pStr, StrSize);
end;

function TCustomLiteCollationDesc.GetWideStr(StrSize: Integer; const pStr: IntPtr): WideString;
begin
  Result := Marshal.PtrToStringUni(pStr, StrSize shr 1);
end;

{ TLiteCollationDesc }

constructor TLiteCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteCollation: TLiteCollation);
begin
  inherited Create(Connection, Name);

  FLiteCollation := LiteCollation;
  FLiteCollationMethod := nil;
end;

constructor TLiteCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteCollation: TLiteCollationMethod);
begin
  inherited Create(Connection, Name);

  FLiteCollation := nil;
  FLiteCollationMethod := LiteCollation;
end;

function TLiteCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                      StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then begin
    if Assigned(FLiteCollationMethod) then
      Result := FLiteCollationMethod(string(GetWideStr(StrSize1, pStr1)), string(GetWideStr(StrSize2, pStr2)))
    else
      Result := FLiteCollation(string(GetWideStr(StrSize1, pStr1)), string(GetWideStr(StrSize2, pStr2)));
  end
  else begin
    if Assigned(FLiteCollationMethod) then
      Result := FLiteCollationMethod(string(GetAnsiStr(StrSize1, pStr1)), string(GetAnsiStr(StrSize2, pStr2)))
    else
      Result := FLiteCollation(string(GetAnsiStr(StrSize1, pStr1)), string(GetAnsiStr(StrSize2, pStr2)));
  end;
end;

{ TLiteAnsiCollationDesc }

constructor TLiteAnsiCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteAnsiCollation: TLiteAnsiCollation);
begin
  inherited Create(Connection, Name);

  FLiteAnsiCollation := LiteAnsiCollation;
  FLiteAnsiCollationMethod := nil;
end;

constructor TLiteAnsiCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteAnsiCollation: TLiteAnsiCollationMethod);
begin
  inherited Create(Connection, Name);

  FLiteAnsiCollation := nil; 
  FLiteAnsiCollationMethod := LiteAnsiCollation;
end;

function TLiteAnsiCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                          StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then begin
    if Assigned(FLiteAnsiCollationMethod) then
      Result := FLiteAnsiCollationMethod(AnsiString(GetWideStr(StrSize1, pStr1)), AnsiString(GetWideStr(StrSize2, pStr2)))
    else
      Result := FLiteAnsiCollation(AnsiString(GetWideStr(StrSize1, pStr1)), AnsiString(GetWideStr(StrSize2, pStr2)));
  end
  else begin
    if Assigned(FLiteAnsiCollationMethod) then
      Result := FLiteAnsiCollationMethod(GetAnsiStr(StrSize1, pStr1), GetAnsiStr(StrSize2, pStr2))
    else
      Result := FLiteAnsiCollation(GetAnsiStr(StrSize1, pStr1), GetAnsiStr(StrSize2, pStr2));
  end;
end;

{ TLiteWideCollationDesc }

constructor TLiteWideCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteWideCollation: TLiteWideCollation);
begin
  inherited Create(Connection, Name);

  FLiteWideCollation := LiteWideCollation;
  FLiteWideCollationMethod := nil;
end;

constructor TLiteWideCollationDesc.Create(Connection: TCRConnection; const Name: string; LiteWideCollation: TLiteWideCollationMethod);
begin
  inherited Create(Connection, Name);

  FLiteWideCollation := nil;
  FLiteWideCollationMethod := LiteWideCollation;
end;

function TLiteWideCollationDesc.DoCollate(StrSize1: Integer; const pStr1: IntPtr;
                                          StrSize2: Integer; const pStr2: IntPtr): Integer;
begin
  if TSQLiteConnection(FConnection).IsUnicodeDataBase then begin
    if Assigned(FLiteWideCollationMethod) then
      Result := FLiteWideCollationMethod(GetWideStr(StrSize1, pStr1), GetWideStr(StrSize2, pStr2))
    else
      Result := FLiteWideCollation(GetWideStr(StrSize1, pStr1), GetWideStr(StrSize2, pStr2))
  end
  else begin
    if Assigned(FLiteWideCollationMethod) then
      Result := FLiteWideCollationMethod(WideString(GetAnsiStr(StrSize1, pStr1)), WideString(GetAnsiStr(StrSize2, pStr2)))
    else
      Result := FLiteWideCollation(WideString(GetAnsiStr(StrSize1, pStr1)), WideString(GetAnsiStr(StrSize2, pStr2)));
  end;  
end;

{ TSQLiteCollationManager }

constructor TSQLiteCollationManager.Create(Connection: TCRConnection);
begin
  inherited Create;

  FConnection := Connection;
  FCollationList := TCRObjectList.Create;
end;

destructor TSQLiteCollationManager.Destroy;
begin
  UnRegistrAllCollations;
  FCollationList.Free;

  inherited;
end;

procedure TSQLiteCollationManager.InternalAddCollation(LiteCollationDesc: TCustomLiteCollationDesc);
var
  ExistLiteCollationDesc: TCustomLiteCollationDesc;
begin
  ExistLiteCollationDesc := FindCollation(LiteCollationDesc.Name);
  if ExistLiteCollationDesc <> nil then
    InternalRemoveCollation(ExistLiteCollationDesc);

  LiteCollationDesc.RegisterCollation;
  FCollationList.Add(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.InternalRemoveCollation(LiteCollationDesc: TCustomLiteCollationDesc);
begin
  LiteCollationDesc.UnregisterCollation;
  FCollationList.Remove(LiteCollationDesc);
end;

function TSQLiteCollationManager.FindCollation(Name: string): TCustomLiteCollationDesc;
var
  i: integer;
begin
  for i := 0 to FCollationList.Count - 1 do
  begin
    Result := TCustomLiteCollationDesc(FCollationList[i]);
    if Result.Name = Name then
      exit;
  end;

  // if not found
  Result := nil;
end;

procedure TSQLiteCollationManager.RegisterCollation(const Name: string; LiteCollation: TLiteCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteCollationDesc.Create(FConnection, Name, LiteCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterCollation(const Name: string; LiteCollation: TLiteCollationMethod);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteCollationDesc.Create(FConnection, Name, LiteCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterCollation(const Name: string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterAnsiCollation(const Name: string; LiteAnsiCollation: TLiteAnsiCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteAnsiCollationDesc.Create(FConnection, Name, LiteAnsiCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterAnsiCollation(const Name: string; LiteAnsiCollation: TLiteAnsiCollationMethod);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteAnsiCollationDesc.Create(FConnection, Name, LiteAnsiCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterAnsiCollation(const Name: string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterWideCollation(const Name: string; LiteWideCollation: TLiteWideCollation);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteWideCollationDesc.Create(FConnection, Name, LiteWideCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterWideCollation(const Name: string; LiteWideCollation: TLiteWideCollationMethod);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := TLiteWideCollationDesc.Create(FConnection, Name, LiteWideCollation);
  InternalAddCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.UnRegisterWideCollation(const Name: string);
var
  LiteCollationDesc: TCustomLiteCollationDesc;
begin
  LiteCollationDesc := FindCollation(Name);
  if LiteCollationDesc <> nil then
    InternalRemoveCollation(LiteCollationDesc);
end;

procedure TSQLiteCollationManager.RegisterDefaultCollations;
begin
  RegisterWideCollation('UniNoCase', DefaultUnicodeNoCase);
end;

procedure TSQLiteCollationManager.UnRegisterDefaultCollations;
begin
  UnRegisterWideCollation('UniNoCase');
end;

procedure TSQLiteCollationManager.UnRegistrAllCollations;
var
  i: integer;
begin
  // unregister collations
  for i := 0 to FCollationList.Count - 1 do
    TCustomLiteCollationDesc(FCollationList[i]).UnregisterCollation;
  FCollationList.Clear;
end;

initialization
  CallBackLiteCollationPtr := @CallBackLiteCollation;

finalization
  CallBackLiteCollationPtr := nil;

end.
