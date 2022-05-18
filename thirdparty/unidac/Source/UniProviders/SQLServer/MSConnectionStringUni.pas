
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  SQL Server ConnectionString
//////////////////////////////////////////////////

{$I Sdac.inc}
unit MSConnectionStringUni;

interface

uses
  SysUtils, Classes,
{$IFDEF VER6P}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  CRAccess, CRVio, CRConnectionString, DAConsts,
  DBAccess,
{$IFNDEF UNIDACPRO}
  MSConsts, MSClasses{$IFDEF MSWINDOWS}, OLEDBAccess{$ENDIF};
{$ELSE}
  MSConstsUni, MSClassesUni{$IFDEF MSWINDOWS}, OLEDBAccessUni{$ENDIF};
{$ENDIF}

const
  cpUseProcedureforPrepare = -201;
  cpAsynchronousConnection = -202;
  cpClientFailover         = -203;
  cpTagWithColumnCollation = -204;

type
  TCustomMSConnectionStringBuilder = class(TCRConnectionStringBuilder)
  private
    FProviderName: string;
  protected
    procedure InitParams; override;
    function GetParamValue(Param: TConnectionStringParam): Variant; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;
    function ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string; override;
    function ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant; override;
  end;

  TMSConnectionStringBuilder = class(TCustomMSConnectionStringBuilder)
  protected
    procedure CheckParamValue(const Name, Value: string; IsValueQuoted: boolean = false); override;
  protected
    procedure InitParams; override;
    function ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant; override;
  end;

  TMSCompactConnectionStringBuilder = class(TCustomMSConnectionStringBuilder)
  private
    function InitModeToStr(Mode: TMSInitMode): string;
    function StrToInitMode(const Value: string; var Mode: TMSInitMode): boolean;
  public
    procedure InitParams; override;
    function ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string; override;
    function ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant; override;
  end;

{$IFDEF MSWINDOWS}
  TOLEDBConnectionStringBuilder = class(TMSConnectionStringBuilder)
  private
    FUnknownParams: String;
    FWithUnknownParams: Boolean;
    FNeedRevertChanges: Boolean;

    procedure ParseServerString(var Instance: string; var Port: integer);
  protected
    procedure InitParams; override;    
  public
    constructor Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod); override;
    class function GetInstance(const Connection: TCustomDAConnection): TOLEDBConnectionStringBuilder;

    function ReadUnknownParam(const Name, Value: string): boolean; override;
    procedure WriteParam(var ConnectionString: string; Param: TConnectionStringParam); override;
    procedure WriteUnknownParams(var ConnectionString: string); override;

    procedure Convert();
    procedure Revert();

    property WithUnknownParams: Boolean read FWithUnknownParams write FWithUnknownParams;
  end;
{$ENDIF}

implementation

uses
  TypInfo,
  CRProps,
  CRFunctions,
{$IFNDEF UNIDACPRO}
  MSProps;
{$ELSE}
  MSPropsUni;
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TInternalMSConnection = class(TCustomDAConnection)
  end;
{$ENDIF}

{ TCustomMSConnectionStringBuilder }

procedure TCustomMSConnectionStringBuilder.InitParams;
begin
  inherited;

  /// list of supported parameters must be syncronized with SetConnectString (ProcessParam and set param to default)
  AddParam(ppNormal, 'Data Source', ['Host', 'Server', 'Address', 'Addr', 'Network Address'], prServer, varString, '');
  AddParam(ppNormal, 'Initial Catalog', ['Database'], prDatabase, varString, '');
  AddParam(ppNormal, 'Force Create Database', ['ForceCreateDatabase'], prForceCreateDatabase, varBoolean, DefValForceCreateDatabase);

  AddParam(ppNormal, 'Use Procedure for Prepare', [], cpUseProcedureforPrepare, varString, Null); // dummy
  AddParam(ppNormal, 'Asynchronous Connection', [], cpAsynchronousConnection, varString, Null); // dummy
  AddParam(ppNormal, 'Client Failover', [], cpClientFailover, varString, Null); // dummy
  AddParam(ppNormal, 'Tag with column collation when possible', [], cpTagWithColumnCollation, varString, Null); // dummy
end;

function TCustomMSConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
begin
  case Param.Code of
    cpUseProcedureforPrepare,
    cpAsynchronousConnection,
    cpClientFailover,
    cpTagWithColumnCollation:
      Result := Null;
    prServer:
      if TMSProvider(GetProp(prProvider)) <> prCompact then
        Result := GetProp(prServer)
      else
        Result := GetProp(prDatabase);
    prDatabase:
      if TMSProvider(GetProp(prProvider)) <> prCompact then
        Result := GetProp(prDatabase)
      else
        Result := Null;
    else
      Result := inherited GetParamValue(Param);
  end;
end;

procedure TCustomMSConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
begin
  case Param.Code of
    cpUseProcedureforPrepare,
    cpAsynchronousConnection,
    cpClientFailover,
    cpTagWithColumnCollation: begin
      // dummy
    end;
    prProvider: begin
      SetProp(prProvider, Value);
      if TMSProvider(GetProp(prProvider)) = prNativeClient then
        SetProp(prNativeClientVersion, Variant(GetNativeClientVersion(FProviderName, TNativeClientVersion(GetProp(prNativeClientVersion)))))
      else if TMSProvider(GetProp(prProvider)) = prCompact then
        SetProp(prCompactVersion, Variant(GetCompactVersion(GetProviderName(Value))));
    end;
    prServer:
      if TMSProvider(GetProp(prProvider)) <> prCompact then
        SetProp(prServer, Value)
      else
        SetProp(prDatabase, Value);
    else
      inherited SetParamValue(Param, Value);
  end;
end;

function TCustomMSConnectionStringBuilder.ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string;
begin
  case Param.Code of
    prProvider: begin
      case TMSProvider(Value) of
        prAuto:
          Result := '';
        prNativeClient:
          Result := GetProviderName(TMSProvider(Value), cvAuto, TNativeClientVersion(GetProp(prNativeClientVersion)));
        else
          Result := GetProviderName(TMSProvider(Value));
      end;
    end;
    prServer:
      if TMSProvider(GetProp(prProvider)) <> prCompact then
        Result := Value
      else
        Result := '"' + Value + '"';
    else
      Result := inherited ConvertVarToStr(Param, Value);
  end;
end;

function TCustomMSConnectionStringBuilder.ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant;
begin
  case Param.Code of
    prProvider: begin
      FProviderName := UpperCase(Value);
      if (FProviderName <> SProviderTDS) and
         (FProviderName <> SProviderSQLOLEDB) and
         (FProviderName <> SProviderMSOLEDBSQL) and
         (FProviderName <> SProviderNativeClient) and
         (FProviderName <> SProviderNativeClient10) and
         (FProviderName <> SProviderNativeClient11) and
         (FProviderName <> SProviderCompact) and
         (FProviderName <> SProviderCompact35) and
         (FProviderName <> SProviderCompact40)
      then
        raise EConnectionStringError.CreateFmt(SInvalidConnectParamValue, [Param.Name, Value]);

      Result := GetProvider(FProviderName);
    end;
    else
      Result := inherited ConvertStrToVar(Param, Value);
  end;
end;

{ TMSConnectionionStringBuilder }

procedure TMSConnectionStringBuilder.CheckParamValue(const Name, Value: string; IsValueQuoted: boolean = false);
var
  i: integer;
begin
  if not (CheckParamName(['Password', 'PWD'], Name)) and not IsValueQuoted then
    for i := 1 to Length(Value) do
      case Value[i] of
        '!'..'/', '\', '_', ' ', '@', '|', ':', '0'..'9', 'a'..'z', 'A'..'Z':;
      else
        raise Exception.CreateFmt(SConnectParamInvalidChar, [Value[i]]);
      end;
end;

procedure TMSConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(cpPersistSecurityInfo);

  AddParam(ppHighest, 'Provider', ['OLEDBProvider'], prProvider, varEnum, DefValProvider, TypeInfo(TMSProvider));

  AddParam(ppNormal, 'Port', [], prPort, varInteger, DefaultSDACPort, [0]);
  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVersion, TypeInfo(TIPVersion));

{$IFDEF MSWINDOWS}
{$IFNDEF SQL_AZURE}
  AddParam(ppNormal, 'Authentication', ['Integrated Security', 'Trusted_Connection'], prAuthentication, varEnum, DefValAuthentication, TypeInfo(TMSAuthentication), 'au');
{$ENDIF}
{$ENDIF}

  AddParam(ppNormal, 'Application Intent', ['ApplicationIntent'], prApplicationIntent, varEnum, DefValApplicationIntent, TypeInfo(TApplicationIntent));
  AddParam(ppNormal, 'Auto Translate', ['AutoTranslate'], prAutoTranslate, varBoolean, DefValAutoTranslate);
  AddParam(ppNormal, 'MultipleConnections', ['Multiple Connections'], prMultipleConnections, varBoolean, DefValMultipleConnections);
  AddParam(ppNormal, 'Application Name', ['ApplicationName', 'AppName', 'App'], prApplicationName, varString, '');
  AddParam(ppNormal, 'Packet Size', ['PacketSize'], prPacketSize, varInteger, DefaultPacketSize);
  AddParam(ppNormal, 'MultipleActiveResultSets', ['Multiple Active Result Sets', 'MARS Connectionion', 'MARS'], prMARS, varBoolean, DefValMultipleActiveResultSets);

  AddParam(ppNormal, 'Current Language', ['Language'], prLanguage, varString, '');
  AddParam(ppNormal, 'Persist Security Info', ['PersistSecurityInfo'], prPersistSecurityInfo, varBoolean, DefValPersistSecurityInfo);
  AddParam(ppNormal, 'Network Library', ['Network', 'NetworkLibrary', 'NetLibrary', 'Net'], prNetworkLibrary, varString, '');
  AddParam(ppNormal, 'Workstation ID', ['WorkstationID', 'WSID'], prWorkstationID, varString, '');
  AddParam(ppNormal, 'AttachDBFileName', ['InitialFileName', 'InitFileName', 'Initial File Name'], prInitialFileName, varString, '');
  AddParam(ppNormal, 'Failover Partner', ['FailoverPartner', 'Failover_Partner'], prFailoverPartner, varString, '');
  AddParam(ppNormal, 'MultiSubnetFailover', [], prMultiSubnetFailover, varBoolean, DefValMultiSubnetFailover);
  AddParam(ppNormal, 'Trust Server Certificate', ['TrustServerCertificate'], prTrustServerCertificate, varBoolean, DefValTrustServerCertificate);
  AddParam(ppNormal, 'Use Encryption for Data', ['Encryption', 'Encrypt'], prEncrypt, varBoolean, DefValEncrypt);
end;

function TMSConnectionStringBuilder.ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant;
var
  b: Boolean;
begin
  case Param.Code of
    prAuthentication: begin
      if Value = 'SSPI' then begin
        Result := auWindows;
        Exit;
      end;

      if TryStrToBool(Value, b) then begin
        if b then
          Result := auWindows
        else
          Result := auServer;
      end
      else
        Result := inherited ConvertStrToVar(Param, Value);
    end
    else
      Result := inherited ConvertStrToVar(Param, Value);
  end;
end;

{ TMSCompactConnectionStringBuilder }

function TMSCompactConnectionStringBuilder.InitModeToStr(Mode: TMSInitMode): string;
begin
  Result := '';
  case Mode of
    imReadOnly:
      Result := 'Read Only';
    imReadWrite:
      Result := 'Read Write';
    imExclusive:
      Result := 'Exclusive';
    imShareRead:
      Result := 'Shared Read';
    else
      Assert(False);
  end;
end;

function TMSCompactConnectionStringBuilder.StrToInitMode(const Value: string; var Mode: TMSInitMode): boolean;
var
  LowValue: string;
begin
  Result := True;
  LowValue := LowerCase(Value);
  if LowValue = 'read only' then
    Mode := imReadOnly
  else
  if LowValue = 'read write' then
    Mode := imReadWrite
  else
  if LowValue = 'exclusive' then
    Mode := imExclusive
  else
  if LowValue = 'shared read' then
    Mode := imShareRead
  else
    Result := False;
end;

procedure TMSCompactConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppHighest, 'Provider', ['OLEDBProvider'], prProvider, varString, DefValProviderCompact, TypeInfo(TMSProvider));

  AddParam(ppNormal, 'ssce: database password', ['Password', 'PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'ssce: encrypt database', ['Use Encryption for Data', 'Encryption', 'Encrypt'], prEncrypt, varBoolean, DefValEncrypt);
  AddParam(ppNormal, 'ssce: max buffer size', [], prMaxBufferSize, varInteger, DefaultMaxBufferSize);
  AddParam(ppNormal, 'ssce: max database size', [], prMaxDatabaseSize, varInteger, DefaultMaxDatabaseSize);
  AddParam(ppNormal, 'ssce: mode', [], prInitMode, varEnum, DefaultInitMode, TypeInfo(TMSInitMode));
  AddParam(ppNormal, 'ssce: locale identifier', [], prLocaleIdentifier, varString, {$IFDEF MSWINDOWS}LocaleIdentifierToStr(GetSystemDefaultLCID){$ELSE}''{$ENDIF});
  AddParam(ppNormal, 'ssce: default lock timeout', [], prDefaultLockTimeout, varInteger, DefaultDefaultLockTimeout);
  AddParam(ppNormal, 'ssce: default lock escalation', [], prDefaultLockEscalation, varInteger, DefaultDefaultLockEscalation);
  AddParam(ppNormal, 'ssce: flush interval', [], prFlushInterval, varInteger, DefaultFlushInterval);
  AddParam(ppNormal, 'ssce: autoshrink threshold', [], prAutoShrinkThreshold, varInteger, DefaultAutoShrinkThreshold);
  AddParam(ppNormal, 'ssce: temp file directory', [], prTempFileDirectory, varString, '');
  AddParam(ppNormal, 'ssce: temp file max size', [], prTempFileMaxSize, varInteger, DefaultTempFileMaxSize);
end;

function TMSCompactConnectionStringBuilder.ConvertVarToStr(Param: TConnectionStringParam; const Value: Variant): string;
begin
  case Param.Code of
    prInitMode:
      Result := InitModeToStr(Value);
    else
      Result := inherited ConvertVarToStr(Param, Value);
  end;
end;

function TMSCompactConnectionStringBuilder.ConvertStrToVar(Param: TConnectionStringParam; const Value: string): Variant;
var
  Mode: TMSInitMode;
begin
  case Param.Code of
    prInitMode: begin
      if not StrToInitMode(Value, Mode) then
        raise EConnectionStringError.CreateFmt(SInvalidConnectParamValue, [Param.Name, Value]);
      Result := Mode
    end;
    else
      Result := inherited ConvertStrToVar(Param, Value);
  end;
end;

{ TOLEDBConnectionStringBuilder }

{$IFDEF MSWINDOWS}
constructor TOLEDBConnectionStringBuilder.Create(GetPropMethod: TGetConnectionStringParamMethod;
  SetPropMethod: TSetConnectionStringParamMethod);
begin
  inherited;

  FUnknownParams := '';
  FWithUnknownParams := False;
  FNeedRevertChanges := False;
end;

class function TOLEDBConnectionStringBuilder.GetInstance(
  const Connection: TCustomDAConnection): TOLEDBConnectionStringBuilder;
begin
  Result := TOLEDBConnectionStringBuilder.Create(
    TInternalMSConnection(Connection).GetConnectionStringParam,
    TInternalMSConnection(Connection).SetConnectionStringParam);
end;

procedure TOLEDBConnectionStringBuilder.ParseServerString(var Instance: string; var Port: integer);
var
  CommaPos: Integer;
  Str: string;
begin
  CommaPos := Pos(',', Instance);
  if CommaPos = 0 then
    Exit;

  Str := Copy(Instance, CommaPos + 1, Length(Instance) - CommaPos);
  Instance := Copy(Instance, 1, CommaPos - 1);

  if not TryStrToInt(Str, Port) then
    Port := 0;
end;

procedure TOLEDBConnectionStringBuilder.InitParams;
begin
  inherited;

  DeleteParam(cpLoginPrompt);
  DeleteParam(cpPooling);
  DeleteParam(cpConnectionLifetime);
  DeleteParam(cpMaxPoolSize);
  DeleteParam(cpMinPoolSize);
  DeleteParam(cpValidateConnection);
  DeleteParam(prApplicationIntent);
  DeleteParam(prFailoverPartner);
  DeleteParam(prIPVersion);
  DeleteParam(prMARS);
  DeleteParam(prMultipleConnections);
  DeleteParam(prProvider);
  DeleteParam(prTrustServerCertificate);
  DeleteParam(prMultiSubnetFailover);

{$IFNDEF SQL_AZURE}
  AddParam(ppNormal, 'Integrated Security', ['Authentication', 'Trusted_Connection'], prAuthentication, varEnum, DefValAuthentication, TypeInfo(TMSAuthentication), 'au');
{$ENDIF}

  AddParam(ppNormal, 'Connect Timeout', ['ConnectionTimeout', 'Connection Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'Initial File Name', ['InitialFileName', 'InitFileName', 'AttachDBFileName'], prInitialFileName, varString, '');
end;

function TOLEDBConnectionStringBuilder.ReadUnknownParam(const Name, Value: string): boolean;
begin
  if (CompareText('Replication server name connect option', Name) <> 0) and
     (CompareText('Extended Properties', Name) <> 0)
  then
    if Pos(Name, FUnknownParams) = 0 then
      AppendParamStr(FUnknownParams, Name + '=' + Value);

  Result := True;
end;

procedure TOLEDBConnectionStringBuilder.WriteParam(var ConnectionString: string; Param: TConnectionStringParam);
var
  Value: Variant;
  Authentication: Integer;
begin
  case Param.Code of
    prAuthentication: begin
      Value := GetParamValue(Param);
      if (Value <> Null) and TryStrToInt(VarToStr(Value), Authentication) and
        (Authentication = Ord(auWindows))
      then
        AppendParamStr(ConnectionString, Param.Name + '=' + 'SSPI');
    end;
    prPort:
      if FWithUnknownParams then
        inherited;
    else
      inherited;
  end;
end;

procedure TOLEDBConnectionStringBuilder.WriteUnknownParams(var ConnectionString: string);
begin
  inherited;

  if FWithUnknownParams and (FUnknownParams <> '') then
    AppendParamStr(ConnectionString, FUnknownParams);
end;

procedure TOLEDBConnectionStringBuilder.Convert();
var
  PortParam: TConnectionStringParam;
  Port: Variant;
  ServerParam: TConnectionStringParam;
  ServerName: string;
  CommaPos: Integer;
begin
  FNeedRevertChanges := False;
  PortParam := FindParamByCode(prPort);
  ServerParam := FindParamByCode(prServer);
  if (PortParam = nil) or (ServerParam = nil) then
    Exit;

  Port := GetParamValue(PortParam);
  if (Port = Null) or (Port = DefaultSDACPort) then
    Exit;

  ServerName := VarToStr(GetParamValue(ServerParam));
  if Trim(ServerName) = '' then
    Exit;

  CommaPos := Pos(',', ServerName);
  if CommaPos > 0 then begin
    AppendParamStr(FUnknownParams, PortParam.Name + '=' + VarToStr(Port));
    Exit;
  end;

  ServerName := ServerName + ',' + VarToStr(Port);
  SetParamValue(ServerParam, ServerName);

  FNeedRevertChanges := True;
end;

procedure TOLEDBConnectionStringBuilder.Revert;
var
  PortParam: TConnectionStringParam;
  ServerParam: TConnectionStringParam;
  CommaPos: Integer;
  ServerName: string;
  Port: Integer;
begin
  if not FNeedRevertChanges then
    Exit;

  PortParam := FindParamByCode(prPort);
  ServerParam := FindParamByCode(prServer);
  if (PortParam = nil) or (ServerParam = nil) then
    Exit;

  ServerName := VarToStr(GetParamValue(ServerParam));
  if Trim(ServerName) = '' then
    Exit;

  Port := 0;
  try
    CommaPos := Pos(',', ServerName);
    if CommaPos = 0 then
      Exit;

    ParseServerString(ServerName, Port);
    SetParamValue(ServerParam, ServerName);
  finally
    SetParamValue(PortParam, Port);
  end;
end;
{$ENDIF}

end.

