
/////////////////////////////////////////////////
//  DB Access Components
//  Copyright © 1998-2021 Devart. All right reserved.
//  MySQL ConnectionString
//////////////////////////////////////////////////

{$I MyDac.inc}
unit MyConnectionStringUni;

interface

uses
  SysUtils, Classes, 
{$IFDEF VER6P}
  Variants,
{$ENDIF}
  CRTypes, CRParser, CRAccess, CRVio, CRConnectionString, DAConsts;

const
  cpServerParameters = -201;

type
  TEmbParamsMethod = function: TStrings of object;

  TCustomMyConnectionStringBuilder = class(TCRConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

  TMyConnectionStringBuilder = class(TCustomMyConnectionStringBuilder)
  protected
    procedure InitParams; override;
  end;

  TMyEmbConnectionStringBuilder = class(TCustomMyConnectionStringBuilder)
  private
    FInProcessServerParameters: boolean;
    FEmbParamsMethod: TEmbParamsMethod;
  protected
    procedure InitParams; override;
    function GetParamValue(Param: TConnectionStringParam): Variant; override;
    procedure SetParamValue(Param: TConnectionStringParam; const Value: Variant); override;
    procedure ResetParams; override;
  public
    constructor Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod); override;

    property EmbParams: TEmbParamsMethod read FEmbParamsMethod write FEmbParamsMethod;
  end;

implementation

uses
  TypInfo, 
  CRProps,
  CRFunctions,
{$IFNDEF UNIDACPRO}
  MyConsts, MyProps, MyCall, MyClasses;
{$ELSE}
  UniConsts,
  MyConstsUni, MyPropsUni, MyCallUni, MyClassesUni;
{$ENDIF}

{ TCustomMyConnectionStringBuilder }

procedure TCustomMyConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'User ID', ['User', 'UID', 'User Name', 'UserName'], prUsername, varString, '');
  AddParam(ppNormal, 'Password', ['PWD'], prPassword, varString, '');
  AddParam(ppNormal, 'Connection Timeout', ['ConnectionTimeout', 'Connect Timeout', 'ConnectTimeout'], prConnectionTimeout, varInteger, DefValConnectionTimeout);
  AddParam(ppNormal, 'Use Unicode', ['UseUnicode'], prUseUnicode, varBoolean, DefValUseUnicode);
  AddParam(ppNormal, 'Character Set', ['CharacterSet', 'Charset'], prCharset, varString, '');
end;

{ TMyConnectionStringBuilder }

procedure TMyConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Host', 'Server'], prServer, varString, '');
  AddParam(ppNormal, 'Database', [], prDatabase, varString, '');
  AddParam(ppNormal, 'Port', [], prPort, varInteger, {$IFNDEF UNIDACPRO}MYSQL_PORT{$ELSE}DefValPort{$ENDIF});
  AddParam(ppNormal, 'Compress', [], prCompress, varBoolean, DefValCompress);
  AddParam(ppNormal, 'Protocol', [], prProtocol, varEnum, DefValProtocol, TypeInfo(TMyProtocol));
  AddParam(ppNormal, 'Embedded', [], prEmbedded, varBoolean, DefValEmbedded);
{$IFDEF HAVE_DIRECT}
  AddParam(ppNormal, 'IP Version', ['IPVersion'], prIPVersion, varEnum, DefValIPVersion, TypeInfo(TIPVersion));
{$ENDIF}
  AddParam(ppNormal, 'Interactive', [], prInteractive, varBoolean, DefValInteractive);

  AddParam(ppNormal, 'SSL CA Cert', ['SSL CACert'], prSSLCA, varString, '');
  AddParam(ppNormal, 'SSL Cert', [], prSSLCert, varString, '');
  AddParam(ppNormal, 'SSL Key', [], prSSLKey, varString, '');
  AddParam(ppNormal, 'SSL Cipher List', ['SSL Cipher', 'SSL Chipher List', 'SSL CipherList', 'SSL ChipherList'], prSSLCipher, varString, '');

  AddParam(ppNormal, 'Http Url', [], prHttpUrl, varString, '');
  AddParam(ppNormal, 'Http User Name', ['Http Username'], prHttpUsername, varString, '');
  AddParam(ppNormal, 'Http Password', [], prHttpPassword, varString, '');
  AddParam(ppNormal, 'Http Trust Server Certificate', ['HttpTrustServerCertificate'], prHttpTrustServerCertificate, varBoolean, False);

  AddParam(ppNormal, 'Proxy Host Name', ['Proxy Hostname'], prProxyHostname, varString, '');
  AddParam(ppNormal, 'Proxy Port', [], prProxyPort, varInteger, DefValProxyPort);
  AddParam(ppNormal, 'Proxy User Name', ['Proxy Username'], prProxyUsername, varString, '');
  AddParam(ppNormal, 'Proxy Password', [], prProxyPassword, varString, '');



end;

{ TMyEmbConnectionStringBuilder}

constructor TMyEmbConnectionStringBuilder.Create(GetPropMethod: TGetConnectionStringParamMethod; SetPropMethod: TSetConnectionStringParamMethod);
begin
  Inherited Create(GetPropMethod, SetPropMethod);

  FInProcessServerParameters := False;
end;

procedure TMyEmbConnectionStringBuilder.InitParams;
begin
  inherited;

  AddParam(ppNormal, 'Data Source', ['Database'], prDatabase, varString, '');
  AddParam(ppNormal, 'Server Parameters', ['Params'], cpServerParameters, varString, '');
end;

function TMyEmbConnectionStringBuilder.GetParamValue(Param: TConnectionStringParam): Variant;
var
  i: integer;
  ts: string;
begin
  case Param.Code of
    cpServerParameters: begin
      ts := '';
      for i := 0 to EmbParams.Count - 1 do begin
        if ts <> '' then
          ts := ts + ';';
        ts := ts + EmbParams.Strings[i];
      end;

      if ts <> '--basedir=' + DefValBaseDir + ';--datadir=' + DefValDataDir then
        Result := '"' + ts + '"'
      else
        Result := '';
    end;
    else
      Result := inherited GetParamValue(Param);
  end;
end;

procedure TMyEmbConnectionStringBuilder.SetParamValue(Param: TConnectionStringParam; const Value: Variant);
var
  i: Integer;
  ServerParams: TStrValueStringList;
begin
  case Param.Code of
    cpServerParameters: begin
      EmbParams.Clear;

      ServerParams := Parse(Value);
      try
        for i := 0 to ServerParams.Count - 1 do begin
          EmbParams.Add(ServerParams.Keys[i] + '=' + ServerParams.Values[i]);
          if CheckParamName(['--basedir', '--datadir', '--character-sets-dir', '--tmpdir', '--log-bin', '--log-bin-index'], ServerParams.Keys[i]) then
            CheckDirParam(EmbParams, ServerParams.Keys[i]);
        end;
      finally
        ServerParams.Free;
      end;
    end;
    else
      inherited SetParamValue(Param, Value);
  end;
end;

procedure TMyEmbConnectionStringBuilder.ResetParams;
begin
  inherited;

  EmbParams.Clear;

  SetProp(prBaseDir, DefValBaseDir);
  SetProp(prDataDir, DefValDataDir);
end;

end.
